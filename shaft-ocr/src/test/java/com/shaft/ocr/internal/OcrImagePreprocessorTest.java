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

    @Test
    public void autoUsesImageAdaptiveThresholdForLowContrastText() throws Exception {
        BufferedImage source = new BufferedImage(2, 1, BufferedImage.TYPE_INT_RGB);
        source.setRGB(0, 0, new Color(110, 110, 110).getRGB());
        source.setRGB(1, 0, new Color(120, 120, 120).getRGB());
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ImageIO.write(source, "png", output);

        BufferedImage result = decode(OcrImagePreprocessor.apply(output.toByteArray(), OcrPreprocessingMode.AUTO));

        Assert.assertNotEquals(result.getRaster().getSample(0, 0, 0), result.getRaster().getSample(1, 0, 0));
    }

    @Test
    public void autoIgnoresHiddenRgbInTransparentPadding() throws Exception {
        BufferedImage source = new BufferedImage(6, 1, BufferedImage.TYPE_INT_ARGB);
        for (int x = 0; x < 4; x++) source.setRGB(x, 0, 0x00000000);
        source.setRGB(4, 0, new Color(110, 110, 110).getRGB());
        source.setRGB(5, 0, new Color(120, 120, 120).getRGB());
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ImageIO.write(source, "png", output);

        BufferedImage result = decode(OcrImagePreprocessor.apply(output.toByteArray(), OcrPreprocessingMode.AUTO));

        Assert.assertNotEquals(result.getRaster().getSample(4, 0, 0), result.getRaster().getSample(5, 0, 0));
        Assert.assertEquals(result.getRaster().getSample(0, 0, 0), 255);
    }

    @Test
    public void autoHandlesUniformImagesDeterministically() throws Exception {
        BufferedImage result = decode(OcrImagePreprocessor.apply(pixel(new Color(117, 117, 117)),
                OcrPreprocessingMode.AUTO));

        Assert.assertEquals(result.getRaster().getSample(0, 0, 0), 0);
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
