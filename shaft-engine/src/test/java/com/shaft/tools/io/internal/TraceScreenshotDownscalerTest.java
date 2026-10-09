package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Random;

public class TraceScreenshotDownscalerTest {
    static byte[] noisyPng(int width, int height) throws IOException {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Random random = new Random(42);
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                image.setRGB(x, y, random.nextInt(0xFFFFFF));
            }
        }
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ImageIO.write(image, "png", output);
        return output.toByteArray();
    }

    @Test
    public void oversizedPngShouldBeDownscaledToFitWhileKeepingAspectRatio() throws IOException {
        byte[] png = noisyPng(1000, 500);
        long budget = 1024L * 1024L;
        Assert.assertTrue(png.length > budget, "Fixture must exceed the 1 MiB budget: " + png.length);

        byte[] fitted = TraceScreenshotDownscaler.fit(png, budget);

        Assert.assertNotNull(fitted);
        Assert.assertTrue(fitted.length <= budget, String.valueOf(fitted.length));
        BufferedImage result = ImageIO.read(new ByteArrayInputStream(fitted));
        Assert.assertTrue(result.getWidth() < 1000);
        Assert.assertEquals(result.getWidth() / (double) result.getHeight(), 2.0, 0.02);
    }

    @Test
    public void inBudgetAndUndecodableInputsShouldNotBeRewritten() throws IOException {
        byte[] png = noisyPng(10, 10);
        Assert.assertSame(TraceScreenshotDownscaler.fit(png, 1024L * 1024L), png);
        Assert.assertNull(TraceScreenshotDownscaler.fit(new byte[2 * 1024 * 1024], 1024L * 1024L));
        Assert.assertNull(TraceScreenshotDownscaler.fit(null, 1024L));
    }
}
