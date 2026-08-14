package com.shaft.gui.internal.ocr;

import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrRectangle;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrTextBlock;
import com.shaft.tools.io.PdfFileManager;
import com.shaft.tools.io.pdf.PdfBatchOptions;
import com.shaft.tools.io.pdf.PdfDocumentOptions;
import com.shaft.tools.io.pdf.PdfDocumentRequest;
import com.shaft.tools.io.pdf.PdfExportFormat;
import com.shaft.tools.io.pdf.PdfExportRequest;
import com.shaft.tools.io.pdf.PdfTextSource;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.font.PDType1Font;
import org.apache.pdfbox.pdmodel.font.Standard14Fonts;
import org.apache.pdfbox.pdmodel.graphics.image.LosslessFactory;
import org.apache.pdfbox.text.PDFTextStripper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class PdfFileManagerDocumentContractTest {
    private final List<Path> temporaryFiles = new ArrayList<>();

    @AfterMethod(alwaysRun = true)
    public void reset() {
        OcrProcessingProviderRegistry.clearProviderForTesting();
        temporaryFiles.forEach(path -> {
            try {
                Files.deleteIfExists(path);
            } catch (IOException ignored) {
                // Best-effort test cleanup.
            }
        });
        temporaryFiles.clear();
    }

    @Test
    public void nativeScannedAndMixedPagesUseExpectedSourceWithoutDuplicatingText() throws IOException {
        Path pdf = createMixedPdf();
        OcrProcessingProviderRegistry.setProvidersForTesting(List.of(provider(ocrResult("Native PixelOnly"))));

        var result = new PdfFileManager(pdf.toString()).process(options());

        Assert.assertEquals(result.pages().size(), 2);
        Assert.assertEquals(result.pages().get(0).source(), PdfTextSource.NATIVE);
        Assert.assertEquals(result.pages().get(1).source(), PdfTextSource.HYBRID);
        Assert.assertTrue(result.pages().get(0).recognition().fullText().contains("Native first page content"));
        Assert.assertEquals(count(result.pages().get(1).recognition().fullText(), "Native"), 1,
                "Hybrid text must not duplicate a native word recognized from pixels: "
                        + result.pages().get(1).recognition().blocks());
        Assert.assertTrue(result.pages().get(1).recognition().fullText().contains("PixelOnly"));
    }

    @Test
    public void exportsAreExplicitParseableAndAtomicallyReceipted() throws IOException {
        Path pdf = createScannedPdf();
        OcrProcessingProviderRegistry.setProvidersForTesting(List.of(provider(ocrResult("PixelOnly"))));
        Path searchable = temporary("searchable", ".pdf");
        Path hocr = temporary("result", ".hocr");
        Path tsv = temporary("result", ".tsv");
        Path json = temporary("result", ".json");
        Files.deleteIfExists(searchable);
        Files.deleteIfExists(hocr);
        Files.deleteIfExists(tsv);
        Files.deleteIfExists(json);

        var result = new PdfFileManager(pdf.toString()).process(options(),
                PdfExportRequest.to(PdfExportFormat.SEARCHABLE_PDF, searchable),
                PdfExportRequest.to(PdfExportFormat.HOCR, hocr),
                PdfExportRequest.to(PdfExportFormat.TSV, tsv),
                PdfExportRequest.to(PdfExportFormat.JSON, json));

        Assert.assertEquals(result.exports().size(), 4);
        Assert.assertTrue(Files.readString(hocr).contains("ocrx_word"));
        Assert.assertTrue(Files.readString(tsv).startsWith("level\tpage_num"));
        Assert.assertTrue(Files.readString(json).contains("\"source\""));
        try (PDDocument document = Loader.loadPDF(searchable.toFile())) {
            Assert.assertTrue(new PDFTextStripper().getText(document).contains("PixelOnly"));
        }
        Assert.assertTrue(result.exports().stream().allMatch(export -> export.sha256().length() == 64));
    }

    @Test
    public void alignedWordsProduceTableWhileProseDoesNot() throws IOException {
        Path pdf = createScannedPdf();
        OcrResult table = new OcrResult("Name Amount\nTea 10", List.of(
                word("Name", 10, 10), word("Amount", 180, 10), word("Tea", 10, 60), word("10", 180, 60)));
        OcrProcessingProviderRegistry.setProvidersForTesting(List.of(provider(table)));

        var result = new PdfFileManager(pdf.toString()).process(options());

        Assert.assertEquals(result.pages().getFirst().tables().size(), 1);
        Assert.assertEquals(result.pages().getFirst().tables().getFirst().rows().size(), 2);
        Assert.assertEquals(result.pages().getFirst().tables().getFirst().rows().getFirst().cells().size(), 2);

        OcrResult prose = new OcrResult("This ordinary sentence\nHas no table", List.of(
                word("This", 10, 10), word("ordinary", 65, 10), word("sentence", 155, 10),
                word("Has", 10, 60), word("no", 60, 60), word("table", 95, 60)));
        OcrProcessingProviderRegistry.setProvidersForTesting(List.of(provider(prose)));

        Assert.assertTrue(new PdfFileManager(pdf.toString()).process(options()).pages().getFirst().tables().isEmpty());
    }

    @Test
    public void batchPreflightRejectsDuplicateOutputsBeforeProcessing() throws IOException {
        Path first = createNativePdf("First");
        Path second = createNativePdf("Second");
        Path output = temporary("duplicate", ".json");
        Files.deleteIfExists(output);
        List<PdfDocumentRequest> requests = List.of(
                new PdfDocumentRequest(first, options(), List.of(PdfExportRequest.to(PdfExportFormat.JSON, output))),
                new PdfDocumentRequest(second, options(), List.of(PdfExportRequest.to(PdfExportFormat.JSON, output))));

        Assert.expectThrows(IllegalArgumentException.class,
                () -> PdfFileManager.processAll(requests, new PdfBatchOptions(2, 32 * 1024 * 1024, false)));
        Assert.assertFalse(Files.exists(output));
    }

    private static OcrProcessingProvider provider(OcrResult result) {
        return new OcrProcessingProvider() {
            @Override
            public OcrResult recognize(byte[] image, OcrOptions options) {
                return result;
            }
        };
    }

    private static PdfDocumentOptions options() {
        return PdfDocumentOptions.defaults().withRenderDpi(72).withAllureEvidence(false)
                .withResourceLimits(10 * 1024 * 1024, 10, 2_000_000);
    }

    private Path createMixedPdf() throws IOException {
        Path path = temporary("mixed", ".pdf");
        try (PDDocument document = new PDDocument()) {
            addNativePage(document, "Native first page content");
            PDPage page = addNativePage(document, "Native");
            addImage(document, page, "PixelOnly");
            document.save(path.toFile());
        }
        return path;
    }

    private Path createScannedPdf() throws IOException {
        Path path = temporary("scanned", ".pdf");
        try (PDDocument document = new PDDocument()) {
            PDPage page = new PDPage(PDRectangle.LETTER);
            document.addPage(page);
            addImage(document, page, "PixelOnly");
            document.save(path.toFile());
        }
        return path;
    }

    private Path createNativePdf(String text) throws IOException {
        Path path = temporary("native", ".pdf");
        try (PDDocument document = new PDDocument()) {
            addNativePage(document, text);
            document.save(path.toFile());
        }
        return path;
    }

    private static PDPage addNativePage(PDDocument document, String text) throws IOException {
        PDPage page = new PDPage(PDRectangle.LETTER);
        document.addPage(page);
        try (PDPageContentStream content = new PDPageContentStream(document, page)) {
            content.beginText();
            content.setFont(new PDType1Font(Standard14Fonts.FontName.HELVETICA), 18);
            content.newLineAtOffset(72, 700);
            content.showText(text);
            content.endText();
        }
        return page;
    }

    private static void addImage(PDDocument document, PDPage page, String text) throws IOException {
        BufferedImage image = new BufferedImage(500, 300, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        try {
            graphics.setColor(Color.WHITE);
            graphics.fillRect(0, 0, image.getWidth(), image.getHeight());
            graphics.setColor(Color.BLACK);
            graphics.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 42));
            graphics.drawString(text, 35, 160);
        } finally {
            graphics.dispose();
        }
        try (PDPageContentStream content = new PDPageContentStream(document, page,
                PDPageContentStream.AppendMode.APPEND, true, true)) {
            content.drawImage(LosslessFactory.createFromImage(document, image), 50, 300, 500, 300);
        }
    }

    private Path temporary(String prefix, String suffix) throws IOException {
        Path path = Files.createTempFile("shaft-" + prefix + '-', suffix);
        temporaryFiles.add(path);
        return path;
    }

    private static OcrResult ocrResult(String text) {
        return new OcrResult(text, List.of(word("Native", 72, 75), word("PixelOnly", 150, 20)));
    }

    private static OcrTextBlock word(String text, int x, int y) {
        return new OcrTextBlock(text, new OcrRectangle(x, y, Math.max(20, text.length() * 12), 30),
                0.95, OcrBlockLevel.WORD);
    }

    private static int count(String value, String token) {
        return (value.length() - value.replace(token, "").length()) / token.length();
    }
}
