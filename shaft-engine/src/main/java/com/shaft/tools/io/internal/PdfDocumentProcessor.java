package com.shaft.tools.io.internal;

import com.shaft.gui.internal.ocr.OcrDocumentPageAnalysis;
import com.shaft.gui.internal.ocr.OcrProcessingActions;
import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTextBlock;
import com.shaft.tools.io.pdf.PdfDocumentOptions;
import com.shaft.tools.io.pdf.PdfDocumentResult;
import com.shaft.tools.io.pdf.PdfExportRequest;
import com.shaft.tools.io.pdf.PdfExportResult;
import com.shaft.tools.io.pdf.PdfPageResult;
import com.shaft.tools.io.pdf.PdfTextSource;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.io.IOUtils;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDResources;
import org.apache.pdfbox.pdmodel.graphics.PDXObject;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;
import org.apache.pdfbox.rendering.ImageType;
import org.apache.pdfbox.rendering.PDFRenderer;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/** Internal owner of hybrid PDF text and OCR processing. */
public final class PdfDocumentProcessor {
    private PdfDocumentProcessor() {
        throw new IllegalStateException("Utility class");
    }

    public static PdfDocumentResult process(Path source, PdfDocumentOptions options,
                                            List<PdfExportRequest> exportRequests) {
        return process(source, options, exportRequests,
                new PdfRasterBudget(Math.multiplyExact(options.maximumPixelsPerPage(), 4)));
    }

    static PdfDocumentResult process(Path source, PdfDocumentOptions options,
                                     List<PdfExportRequest> exportRequests, PdfRasterBudget rasterBudget) {
        Path input = validateInput(source, options);
        PdfDocumentExporter.preflight(input, exportRequests);
        List<PdfPageResult> pages = new ArrayList<>();
        ExecutorService ocrExecutor = Executors.newSingleThreadExecutor(Thread.ofVirtual()
                .name("shaft-pdf-ocr-", 0).factory());
        try (PDDocument document = Loader.loadPDF(input.toFile(), IOUtils.createTempFileOnlyStreamCache())) {
            if (document.isEncrypted()) {
                throw new IllegalArgumentException("Encrypted PDF documents are not supported: " + input);
            }
            if (document.getNumberOfPages() > options.maximumPages()) {
                throw new IllegalArgumentException("PDF page count " + document.getNumberOfPages()
                        + " exceeds the configured maximum " + options.maximumPages() + ".");
            }
            PDFRenderer renderer = new PDFRenderer(document);
            renderer.setSubsamplingAllowed(true);
            for (int index = 0; index < document.getNumberOfPages(); index++) {
                pages.add(processPage(document, renderer, ocrExecutor, index, options, rasterBudget));
            }
        } catch (IOException exception) {
            throw new IllegalStateException("Could not process PDF document '" + input + "'.", exception);
        } finally {
            ocrExecutor.shutdownNow();
        }
        PdfDocumentResult unexported = new PdfDocumentResult(input, pages, List.of(), List.of());
        List<PdfExportResult> exports = PdfDocumentExporter.export(unexported, options, exportRequests);
        PdfDocumentResult result = new PdfDocumentResult(input, pages, exports, List.of());
        if (options.attachAllureEvidence()) {
            PdfDocumentReporter.attach(result, options.maximumAllureArtifactBytes());
        }
        return result;
    }

    private static PdfPageResult processPage(PDDocument document, PDFRenderer renderer, ExecutorService executor,
                                             int pageIndex, PdfDocumentOptions options,
                                             PdfRasterBudget rasterBudget) throws IOException {
        PDPage page = document.getPage(pageIndex);
        int width = Math.max(1, Math.round(page.getCropBox().getWidth() * options.renderDpi() / 72f));
        int height = Math.max(1, Math.round(page.getCropBox().getHeight() * options.renderDpi() / 72f));
        long pixels = (long) width * height;
        if (pixels > options.maximumPixelsPerPage()) {
            throw new IllegalArgumentException("PDF page " + (pageIndex + 1) + " has " + pixels
                    + " rendered pixels, above the configured maximum " + options.maximumPixelsPerPage() + ".");
        }
        OcrResult nativeResult = new PdfNativeTextExtractor(pageIndex + 1, options.renderDpi())
                .extract(document, width, height);
        int nativeCharacters = nonWhitespace(nativeResult.fullText());
        double imageCoverage = PdfImageCoverageExtractor.coverage(page);
        boolean needsOcr = nativeCharacters < options.nativeTextMinimumCharacters()
                || imageCoverage >= options.imageCoverageThreshold();
        OcrDocumentPageAnalysis ocr = null;
        if (needsOcr) {
            PdfRasterBudget.Lease lease = rasterBudget.acquire(Math.multiplyExact(pixels, 4));
            byte[] rendered;
            try {
                rendered = render(renderer, pageIndex, options.renderDpi());
            } catch (RuntimeException | IOException exception) {
                lease.close();
                throw exception;
            }
            var analysis = OcrProcessingActions.documentPageAnalysisTask(rendered, options.ocrOptions(),
                    options.detectOrientation(), options.deskew());
            Future<OcrDocumentPageAnalysis> task = executor.submit(() -> {
                try (lease) {
                    return analysis.get();
                }
            });
            try {
                ocr = task.get(options.pageTimeout().toMillis(), TimeUnit.MILLISECONDS);
            } catch (TimeoutException exception) {
                task.cancel(true);
                lease.close();
                throw new IllegalStateException("OCR timed out for PDF page " + (pageIndex + 1) + " after "
                        + options.pageTimeout() + ".", exception);
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException("PDF OCR was interrupted for page " + (pageIndex + 1) + ".", exception);
            } catch (ExecutionException exception) {
                Throwable cause = exception.getCause();
                if (cause instanceof RuntimeException runtimeException) {
                    throw runtimeException;
                }
                throw new IllegalStateException("PDF OCR failed for page " + (pageIndex + 1) + ".", cause);
            }
        }
        PdfTextSource source;
        OcrResult recognition;
        List<String> warnings = new ArrayList<>();
        int rotation = 0;
        double deskew = 0;
        if (ocr == null) {
            source = PdfTextSource.NATIVE;
            recognition = nativeResult;
        } else if (nativeCharacters == 0) {
            source = PdfTextSource.OCR;
            recognition = ocr.result();
            warnings.addAll(ocr.warnings());
            rotation = ocr.rotationDegrees();
            deskew = ocr.deskewDegrees();
        } else {
            source = PdfTextSource.HYBRID;
            recognition = merge(nativeResult, ocr.result());
            warnings.addAll(ocr.warnings());
            rotation = ocr.rotationDegrees();
            deskew = ocr.deskewDegrees();
        }
        PdfTableExtractor.Extraction tables = PdfTableExtractor.extract(recognition,
                options.tableConfidenceThreshold());
        warnings.addAll(tables.warnings());
        double confidence = recognition.blocks().stream().filter(block -> block.level() == OcrBlockLevel.WORD)
                .mapToDouble(OcrTextBlock::confidence).average().orElse(recognition.fullText().isBlank() ? 0 : 1);
        List<OcrTextBlock> searchableOverlay = ocr == null ? List.of() : ocr.result().blocks().stream()
                .filter(block -> block.level() == OcrBlockLevel.WORD)
                .filter(block -> source != PdfTextSource.HYBRID || !overlapsMatchingNativeWord(block, nativeResult))
                .toList();
        return new PdfPageResult(pageIndex + 1, recognition, source, confidence, rotation, deskew,
                searchableOverlay, tables.tables(), warnings);
    }

    private static Path validateInput(Path source, PdfDocumentOptions options) {
        Path input = source.toAbsolutePath().normalize();
        try {
            if (!Files.isRegularFile(input)) {
                throw new IllegalArgumentException("PDF input does not exist or is not a regular file: " + input);
            }
            long size = Files.size(input);
            if (size > options.maximumInputBytes()) {
                throw new IllegalArgumentException("PDF input size " + size + " exceeds the configured maximum "
                        + options.maximumInputBytes() + ".");
            }
            return input;
        } catch (IOException exception) {
            throw new IllegalStateException("Could not inspect PDF input '" + input + "'.", exception);
        }
    }

    private static byte[] render(PDFRenderer renderer, int pageIndex, int dpi) throws IOException {
        BufferedImage image = renderer.renderImageWithDPI(pageIndex, dpi, ImageType.RGB);
        try {
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            if (!ImageIO.write(image, "png", output)) {
                throw new IOException("No PNG writer is available for rendered PDF pages.");
            }
            return output.toByteArray();
        } finally {
            image.flush();
        }
    }

    private static double imageCoverage(PDPage page, int pageWidth, int pageHeight) throws IOException {
        PDResources resources = page.getResources();
        if (resources == null) {
            return 0;
        }
        long imagePixels = 0;
        for (var name : resources.getXObjectNames()) {
            PDXObject object = resources.getXObject(name);
            if (object instanceof PDImageXObject image) {
                imagePixels += (long) image.getWidth() * image.getHeight();
            }
        }
        return Math.min(1, imagePixels / (double) Math.max(1L, (long) pageWidth * pageHeight));
    }

    private static OcrResult merge(OcrResult nativeResult, OcrResult ocrResult) {
        List<OcrTextBlock> merged = new ArrayList<>(nativeResult.blocks());
        List<String> uniqueOcrWords = new ArrayList<>();
        for (OcrTextBlock block : ocrResult.blocks()) {
            if (block.level() != OcrBlockLevel.WORD || !overlapsMatchingNativeWord(block, nativeResult)) {
                merged.add(block);
                if (block.level() == OcrBlockLevel.WORD) {
                    uniqueOcrWords.add(block.text());
                }
            }
        }
        merged.sort(java.util.Comparator.comparingInt((OcrTextBlock block) -> block.bounds().y())
                .thenComparingInt(block -> block.bounds().x()).thenComparing(block -> block.level().ordinal()));
        String text = nativeResult.fullText();
        if (!uniqueOcrWords.isEmpty()) {
            text = text + System.lineSeparator() + String.join(" ", uniqueOcrWords);
        }
        return new OcrResult(text.strip(), merged);
    }

    private static int nonWhitespace(String value) {
        return (int) value.codePoints().filter(codePoint -> !Character.isWhitespace(codePoint)).count();
    }

    private static String normalize(String value) {
        return value.strip().replaceAll("\\s+", " ").toLowerCase(Locale.ROOT);
    }

    private static boolean overlapsMatchingNativeWord(OcrTextBlock candidate, OcrResult nativeResult) {
        return nativeResult.blocks().stream().filter(block -> block.level() == OcrBlockLevel.WORD)
                .anyMatch(block -> normalize(block.text()).equals(normalize(candidate.text()))
                        && overlap(block, candidate) >= 0.8);
    }

    private static double overlap(OcrTextBlock first, OcrTextBlock second) {
        int left = Math.max(first.bounds().x(), second.bounds().x());
        int top = Math.max(first.bounds().y(), second.bounds().y());
        int right = Math.min(first.bounds().right(), second.bounds().right());
        int bottom = Math.min(first.bounds().bottom(), second.bounds().bottom());
        long intersection = (long) Math.max(0, right - left) * Math.max(0, bottom - top);
        long smaller = Math.min((long) first.bounds().width() * first.bounds().height(),
                (long) second.bounds().width() * second.bounds().height());
        return smaller == 0 ? 0 : intersection / (double) smaller;
    }
}
