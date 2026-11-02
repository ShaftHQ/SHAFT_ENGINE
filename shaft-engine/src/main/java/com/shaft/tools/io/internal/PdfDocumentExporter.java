package com.shaft.tools.io.internal;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrTextBlock;
import com.shaft.tools.io.pdf.PdfDocumentOptions;
import com.shaft.tools.io.pdf.PdfDocumentResult;
import com.shaft.tools.io.pdf.PdfExportFormat;
import com.shaft.tools.io.pdf.PdfExportRequest;
import com.shaft.tools.io.pdf.PdfExportResult;
import com.shaft.tools.io.pdf.PdfPageResult;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.io.IOUtils;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.font.PDType1Font;
import org.apache.pdfbox.pdmodel.font.Standard14Fonts;
import org.apache.pdfbox.pdmodel.graphics.state.RenderingMode;
import org.apache.pdfbox.util.Matrix;
import org.apache.pdfbox.pdmodel.documentinterchange.markedcontent.PDPropertyList;
import org.apache.pdfbox.cos.COSDictionary;
import org.apache.pdfbox.cos.COSName;
import org.apache.pdfbox.cos.COSString;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

final class PdfDocumentExporter {
    private static final ObjectMapper JSON = new ObjectMapper();

    private PdfDocumentExporter() {
        throw new IllegalStateException("Utility class");
    }

    static void preflight(Path source, List<PdfExportRequest> requests) {
        Map<Path, PdfExportRequest> outputs = new HashMap<>();
        for (PdfExportRequest request : requests) {
            Path output = request.output();
            if (output.equals(source)) {
                throw new IllegalArgumentException("PDF export output must not overwrite its input: " + source);
            }
            if (outputs.put(output, request) != null) {
                throw new IllegalArgumentException("Duplicate PDF export output: " + output);
            }
            Path parent = output.getParent();
            if (parent == null || !Files.isDirectory(parent) || !Files.isWritable(parent)) {
                throw new IllegalArgumentException("PDF export parent is not a writable directory: " + parent);
            }
            if (Files.exists(output) && !request.replaceExisting()) {
                throw new IllegalArgumentException("PDF export output already exists: " + output);
            }
        }
    }

    static List<PdfExportResult> export(PdfDocumentResult result, PdfDocumentOptions options,
                                        List<PdfExportRequest> requests) {
        if (requests.isEmpty()) {
            return List.of();
        }
        boolean signed = isSigned(result.source());
        if (signed && requests.stream().anyMatch(request -> request.format() == PdfExportFormat.SEARCHABLE_PDF
                && !request.allowSignatureInvalidation())) {
            throw new IllegalArgumentException("Searchable PDF export would invalidate existing signatures. "
                    + "Opt in with allowingSignatureInvalidation().");
        }
        List<PdfExportResult> completed = new ArrayList<>();
        for (PdfExportRequest request : requests) {
            byte[] bytes = switch (request.format()) {
                case SEARCHABLE_PDF -> searchablePdf(result, options);
                case HOCR -> hocr(result).getBytes(StandardCharsets.UTF_8);
                case TSV -> tsv(result).getBytes(StandardCharsets.UTF_8);
                case JSON -> json(result);
            };
            writeAtomically(request.output(), bytes, request.replaceExisting());
            completed.add(new PdfExportResult(request.format(), request.output(), bytes.length, sha256(bytes)));
        }
        return List.copyOf(completed);
    }

    private static boolean isSigned(Path source) {
        try (PDDocument document = Loader.loadPDF(source.toFile())) {
            return !document.getSignatureDictionaries().isEmpty();
        } catch (IOException exception) {
            throw new IllegalStateException("Could not inspect PDF signatures for '" + source + "'.", exception);
        }
    }

    private static byte[] searchablePdf(PdfDocumentResult result, PdfDocumentOptions options) {
        try (PDDocument document = Loader.loadPDF(result.source().toFile(), IOUtils.createTempFileOnlyStreamCache());
             var output = new java.io.ByteArrayOutputStream()) {
            PDType1Font font = new PDType1Font(Standard14Fonts.FontName.HELVETICA);
            for (PdfPageResult pageResult : result.pages()) {
                if (pageResult.source() == com.shaft.tools.io.pdf.PdfTextSource.NATIVE) {
                    continue;
                }
                PDPage page = document.getPage(pageResult.pageNumber() - 1);
                try (PDPageContentStream content = new PDPageContentStream(document, page,
                        PDPageContentStream.AppendMode.APPEND, true, true)) {
                    content.setRenderingMode(RenderingMode.NEITHER);
                    for (OcrTextBlock word : pageResult.searchableOverlay()) {
                        String searchable = winAnsi(word.text());
                        if (searchable.isBlank()) {
                            continue;
                        }
                        float scale = 72f / options.renderDpi();
                        float fontSize = Math.max(1, word.bounds().height() * scale);
                        float x = word.bounds().x() * scale;
                        float y = page.getCropBox().getHeight()
                                - (word.bounds().y() + word.bounds().height()) * scale;
                        content.beginText();
                        content.setFont(font, fontSize);
                        double angle = Math.toRadians(-pageResult.rotationDegrees() + pageResult.deskewDegrees());
                        content.setTextMatrix(new Matrix((float) Math.cos(angle), (float) Math.sin(angle),
                                (float) -Math.sin(angle), (float) Math.cos(angle), x, Math.max(0, y)));
                        COSDictionary properties = new COSDictionary();
                        properties.setItem(COSName.ACTUAL_TEXT, new COSString(word.text()));
                        content.beginMarkedContent(COSName.getPDFName("Span"), PDPropertyList.create(properties));
                        content.showText(searchable);
                        content.endMarkedContent();
                        content.endText();
                    }
                }
            }
            document.save(output);
            return output.toByteArray();
        } catch (IOException exception) {
            throw new IllegalStateException("Could not create searchable PDF export.", exception);
        }
    }

    private static String hocr(PdfDocumentResult result) {
        StringBuilder output = new StringBuilder("<!doctype html><html><body>");
        for (PdfPageResult page : result.pages()) {
            output.append("<div class=\"ocr_page\" id=\"page_").append(page.pageNumber()).append("\">");
            int wordNumber = 0;
            for (OcrTextBlock word : words(page)) {
                output.append("<span class=\"ocrx_word\" id=\"word_").append(page.pageNumber()).append('_')
                        .append(++wordNumber).append("\" title=\"bbox ").append(word.bounds().x()).append(' ')
                        .append(word.bounds().y()).append(' ').append(word.bounds().x() + word.bounds().width())
                        .append(' ').append(word.bounds().y() + word.bounds().height()).append("; x_wconf ")
                        .append(Math.round(word.confidence() * 100)).append("\">").append(xml(word.text()))
                        .append("</span> ");
            }
            output.append("</div>");
        }
        return output.append("</body></html>").toString();
    }

    private static String tsv(PdfDocumentResult result) {
        StringBuilder output = new StringBuilder("level\tpage_num\tblock_num\tpar_num\tline_num\tword_num\tleft\ttop\twidth\theight\tconf\ttext\n");
        for (PdfPageResult page : result.pages()) {
            int wordNumber = 0;
            for (OcrTextBlock word : words(page)) {
                output.append("5\t").append(page.pageNumber()).append("\t1\t1\t1\t").append(++wordNumber)
                        .append('\t').append(word.bounds().x()).append('\t').append(word.bounds().y()).append('\t')
                        .append(word.bounds().width()).append('\t').append(word.bounds().height()).append('\t')
                        .append(Math.round(word.confidence() * 100)).append('\t')
                        .append(word.text().replace('\t', ' ').replace('\n', ' ')).append('\n');
            }
        }
        return output.toString();
    }

    private static byte[] json(PdfDocumentResult result) {
        Map<String, Object> document = new LinkedHashMap<>();
        document.put("source", result.source().toString());
        document.put("text", result.fullText());
        document.put("pages", result.pages().stream().map(page -> {
            Map<String, Object> value = new LinkedHashMap<>();
            value.put("pageNumber", page.pageNumber());
            value.put("source", page.source().name());
            value.put("confidence", page.confidence());
            value.put("rotationDegrees", page.rotationDegrees());
            value.put("deskewDegrees", page.deskewDegrees());
            value.put("text", page.recognition().fullText());
            value.put("blocks", page.recognition().blocks());
            value.put("tables", page.tables());
            value.put("warnings", page.warnings());
            return value;
        }).toList());
        document.put("warnings", result.warnings());
        try {
            return JSON.writerWithDefaultPrettyPrinter().writeValueAsBytes(document);
        } catch (JsonProcessingException exception) {
            throw new IllegalStateException("Could not serialize PDF OCR JSON export.", exception);
        }
    }

    private static List<OcrTextBlock> words(PdfPageResult page) {
        return page.recognition().blocks().stream().filter(block -> block.level() == OcrBlockLevel.WORD).toList();
    }

    private static void writeAtomically(Path output, byte[] bytes, boolean replaceExisting) {
        Path temporary = null;
        try {
            temporary = Files.createTempFile(output.getParent(), "." + output.getFileName() + ".", ".tmp");
            Files.write(temporary, bytes);
            try (var channel = java.nio.channels.FileChannel.open(temporary, java.nio.file.StandardOpenOption.WRITE)) {
                channel.force(true);
            }
            var options = replaceExisting
                    ? new StandardCopyOption[]{StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING}
                    : new StandardCopyOption[]{StandardCopyOption.ATOMIC_MOVE};
            try {
                Files.move(temporary, output, options);
            } catch (AtomicMoveNotSupportedException exception) {
                if (replaceExisting) {
                    Files.move(temporary, output, StandardCopyOption.REPLACE_EXISTING);
                } else {
                    Files.move(temporary, output);
                }
            }
            temporary = null;
        } catch (IOException exception) {
            throw new IllegalStateException("Could not atomically write PDF OCR export '" + output + "'.", exception);
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort cleanup after retaining the original output.
                }
            }
        }
    }

    private static String sha256(byte[] bytes) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is unavailable.", exception);
        }
    }

    private static String winAnsi(String value) {
        return value.codePoints().map(codePoint -> codePoint >= 32 && codePoint <= 255 ? codePoint : '?')
                .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append).toString();
    }

    private static String xml(String value) {
        return value.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
                .replace("\"", "&quot;").replace("'", "&#39;");
    }
}
