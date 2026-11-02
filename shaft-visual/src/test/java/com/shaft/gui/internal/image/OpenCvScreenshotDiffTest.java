package com.shaft.gui.internal.image;

import org.testng.Assert;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.util.List;

public class OpenCvScreenshotDiffTest {
    private final OpenCvVisualProcessingProvider provider = new OpenCvVisualProcessingProvider();

    @BeforeClass(alwaysRun = true)
    public void loadOpenCvNativeLibrary() {
        // Engine startup loads the native lib asynchronously (see PropertiesHelper); force a
        // synchronous load here so this class is deterministic when run in isolation.
        provider.load();
    }

    @Test
    public void identicalImagesShouldMatchWithZeroDiffPixels() {
        byte[] baseline = png(20, 20, Color.BLUE, null);
        byte[] actual = png(20, 20, Color.BLUE, null);

        var result = provider.compareScreenshotAgainstBaseline(baseline, actual, null, null, null);

        Assert.assertTrue(result.matched());
        Assert.assertEquals(result.diffPixels(), 0);
        Assert.assertEquals(result.diffRatio(), 0.0, 0.0001);
        Assert.assertEquals(result.diffImage().length, 0);
    }

    @Test
    public void changedPixelsWithinMaxDiffPixelsBudgetShouldMatch() {
        byte[] baseline = png(10, 10, Color.WHITE, null);
        byte[] actual = png(10, 10, Color.WHITE, new int[]{0, 0, 2, 2}); // 4 changed pixels

        var withinBudget = provider.compareScreenshotAgainstBaseline(baseline, actual, null, 4, null);
        var overBudget = provider.compareScreenshotAgainstBaseline(baseline, actual, null, 3, null);

        Assert.assertTrue(withinBudget.matched());
        Assert.assertEquals(withinBudget.diffPixels(), 4);

        Assert.assertFalse(overBudget.matched());
        Assert.assertEquals(overBudget.diffPixels(), 4);
        Assert.assertTrue(overBudget.diffImage().length > 0);
    }

    @Test
    public void changedPixelsWithinMaxDiffPixelRatioBudgetShouldMatch() {
        byte[] baseline = png(10, 10, Color.WHITE, null); // 100 total pixels
        byte[] actual = png(10, 10, Color.WHITE, new int[]{0, 0, 2, 2}); // 4 changed pixels -> ratio 0.04

        var withinBudget = provider.compareScreenshotAgainstBaseline(baseline, actual, null, null, 0.04);
        var overBudget = provider.compareScreenshotAgainstBaseline(baseline, actual, null, null, 0.03);

        Assert.assertTrue(withinBudget.matched());
        Assert.assertEquals(withinBudget.diffRatio(), 0.04, 0.0001);

        Assert.assertFalse(overBudget.matched());
    }

    @Test
    public void maskedRegionShouldBeIgnoredDuringComparison() {
        byte[] baseline = png(10, 10, Color.WHITE, null);
        byte[] actual = png(10, 10, Color.WHITE, new int[]{0, 0, 4, 4}); // 16 changed pixels, all within the mask

        var result = provider.compareScreenshotAgainstBaseline(baseline, actual, List.of(new int[]{0, 0, 4, 4}), null, null);

        Assert.assertTrue(result.matched());
        Assert.assertEquals(result.diffPixels(), 0);
    }

    @Test
    public void sizeMismatchShouldFailWithFullFrameDiff() {
        byte[] baseline = png(10, 10, Color.WHITE, null);
        byte[] actual = png(12, 12, Color.WHITE, null);

        var result = provider.compareScreenshotAgainstBaseline(baseline, actual, null, null, null);

        Assert.assertFalse(result.matched());
        Assert.assertEquals(result.diffPixels(), 100);
        Assert.assertEquals(result.diffRatio(), 1.0, 0.0001);
        Assert.assertTrue(result.diffImage().length > 0);
    }

    @Test
    public void defaultZeroBudgetShouldFailOnAnyDifference() {
        byte[] baseline = png(10, 10, Color.WHITE, null);
        byte[] actual = png(10, 10, Color.WHITE, new int[]{5, 5, 1, 1}); // 1 changed pixel

        var result = provider.compareScreenshotAgainstBaseline(baseline, actual, null, null, null);

        Assert.assertFalse(result.matched());
        Assert.assertEquals(result.diffPixels(), 1);
    }

    private static byte[] png(int width, int height, Color background, int[] redRect) {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        graphics.setColor(background);
        graphics.fillRect(0, 0, width, height);
        if (redRect != null) {
            graphics.setColor(Color.RED);
            graphics.fillRect(redRect[0], redRect[1], redRect[2], redRect[3]);
        }
        graphics.dispose();
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try {
            ImageIO.write(image, "png", output);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
        return output.toByteArray();
    }
}
