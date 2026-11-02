package com.shaft.ocr.internal;

import com.shaft.gui.ocr.OcrBlockLevel;
import com.shaft.gui.ocr.OcrOptions;
import com.shaft.gui.ocr.OcrResult;
import com.shaft.gui.ocr.OcrRectangle;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;

public class TesseractRealEngineTest {
    @DataProvider
    public Object[][] languages() {
        return new Object[][]{
                {"English", "SHAFT OCR READY", "SHAFT"},
                {"Arabic", "مرحبا بالعالم", "مرحبا"}
        };
    }

    @Test(dataProvider = "languages")
    public void recognizesRealTextAndBoundingBoxesWithoutSystemTesseract(String language, String text,
                                                                         String expectedFragment) throws Exception {
        OcrResult result = new TesseractOcrProvider().recognize(render(text),
                OcrOptions.defaults().withLanguages(language).withMinimumConfidence(0));

        Assert.assertTrue(result.fullText().contains(expectedFragment), result.fullText());
        Assert.assertTrue(result.blocks().stream().anyMatch(block -> block.level() == OcrBlockLevel.WORD));
        Assert.assertTrue(result.blocks().stream().allMatch(block -> block.bounds().width() > 0));
    }

    @Test
    public void rejectsRecognitionRegionsOutsideDecodedImageBounds() throws Exception {
        IllegalArgumentException exception = Assert.expectThrows(IllegalArgumentException.class,
                () -> new TesseractOcrProvider().recognize(render("bounds"),
                        OcrOptions.defaults().withLanguages("English")
                                .within(new OcrRectangle(1100, 200, 200, 100))));
        Assert.assertTrue(exception.getMessage().contains("exceeds the decoded image bounds"));
    }

    private static byte[] render(String text) throws Exception {
        BufferedImage image = new BufferedImage(1200, 240, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        try {
            graphics.setColor(Color.WHITE);
            graphics.fillRect(0, 0, image.getWidth(), image.getHeight());
            graphics.setColor(Color.BLACK);
            graphics.setFont(new Font("Arial", Font.PLAIN, 76));
            graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
            graphics.drawString(text, 40, 145);
        } finally {
            graphics.dispose();
        }
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ImageIO.write(image, "png", output);
        return output.toByteArray();
    }
}
