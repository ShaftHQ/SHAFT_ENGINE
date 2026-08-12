package com.shaft.ocr.internal;

import com.shaft.gui.ocr.OcrPreprocessingMode;
import org.testng.Assert;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

public class OcrImagePreprocessorTest {
    @Test
    public void convertsColorImagesToGrayscale() throws Exception {
        BufferedImage result = decode(OcrImagePreprocessor.apply(pixel(Color.RED), OcrPreprocessingMode.GRAYSCALE));

        Color color = new Color(result.getRGB(0, 0));
        Assert.assertEquals(color.getRed(), color.getGreen());
        Assert.assertEquals(color.getGreen(), color.getBlue());
    }

    @Test
    public void convertsImagesToBinaryAndCanInvertThem() throws Exception {
        Color binary = new Color(decode(OcrImagePreprocessor.apply(pixel(new Color(220, 220, 220)),
                OcrPreprocessingMode.BINARY)).getRGB(0, 0));
        Color inverted = new Color(decode(OcrImagePreprocessor.apply(pixel(Color.BLACK),
                OcrPreprocessingMode.INVERT)).getRGB(0, 0));

        Assert.assertEquals(binary, Color.WHITE);
        Assert.assertEquals(inverted, Color.WHITE);
    }

    @Test
    public void leavesPngBytesUnchangedWhenPreprocessingIsDisabled() {
        byte[] image = pixel(Color.BLUE);
        Assert.assertSame(OcrImagePreprocessor.apply(image, OcrPreprocessingMode.NONE), image);
    }

    private static byte[] pixel(Color color) {
        try {
            BufferedImage image = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
            image.setRGB(0, 0, color.getRGB());
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            ImageIO.write(image, "png", output);
            return output.toByteArray();
        } catch (Exception exception) {
            throw new IllegalStateException(exception);
        }
    }

    private static BufferedImage decode(byte[] image) throws Exception {
        return ImageIO.read(new ByteArrayInputStream(image));
    }
}
