package com.shaft.ocr.internal;

import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrOptions;
import org.testng.Assert;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Set;
import java.util.stream.Collectors;

public class PdfDocumentOcrTest {
    @Test
    public void rotatedDocumentPageReportsCorrectionAndCompleteLayoutLevels() throws IOException {
        byte[] page = rotatedPage();

        var analysis = new TesseractOcrProvider().analyzeDocumentPage(page,
                OcrOptions.defaults().withLanguages("English").withMinimumConfidence(0), true, true);

        Assert.assertTrue(Set.of(90, 270).contains(analysis.rotationDegrees()),
                "A quarter-turn document page must report its applied cardinal correction: " + analysis);
        Set<OcrBlockLevel> levels = analysis.result().blocks().stream().map(block -> block.level())
                .collect(Collectors.toSet());
        Assert.assertTrue(levels.containsAll(Set.of(OcrBlockLevel.PAGE, OcrBlockLevel.BLOCK,
                OcrBlockLevel.PARAGRAPH, OcrBlockLevel.LINE, OcrBlockLevel.WORD)),
                "Document OCR must expose every public layout level.");
        Assert.assertTrue(analysis.result().fullText().contains("SHAFT"));
    }

    private static byte[] rotatedPage() throws IOException {
        BufferedImage upright = new BufferedImage(1_200, 800, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = upright.createGraphics();
        try {
            graphics.setColor(Color.WHITE);
            graphics.fillRect(0, 0, upright.getWidth(), upright.getHeight());
            graphics.setColor(Color.BLACK);
            graphics.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 46));
            for (int line = 0; line < 7; line++) {
                graphics.drawString("SHAFT DOCUMENT OCR SAMPLE LINE " + line, 70, 100 + line * 90);
            }
        } finally {
            graphics.dispose();
        }
        BufferedImage rotated = new BufferedImage(upright.getHeight(), upright.getWidth(), BufferedImage.TYPE_INT_RGB);
        Graphics2D rotation = rotated.createGraphics();
        try {
            rotation.translate(rotated.getWidth(), 0);
            rotation.rotate(Math.PI / 2);
            rotation.drawImage(upright, 0, 0, null);
        } finally {
            rotation.dispose();
            upright.flush();
        }
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ImageIO.write(rotated, "png", output);
        rotated.flush();
        return output.toByteArray();
    }
}
