package com.shaft.gui.internal.image;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.Platform;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.remote.Browser;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Offline image highlight and folder comparison. No browser, device, or network call.
 */
public class ImageProcessingActionsOfflineCoverageTest {

    @DataProvider
    public Object[][] highlightPlatforms() {
        return new Object[][]{
                {Platform.LINUX.name(), "", "", 1.0},
                {Platform.LINUX.name(), "", "", 2.0},
                {Platform.WINDOWS.name(), "", "", 1.25},
                {Platform.MAC.name(), "", "", 1.0},
                {Platform.IOS.name(), Browser.SAFARI.browserName(), "", 1.0},
                {Platform.ANDROID.name(), "", "com.android.chrome", 1.0}
        };
    }

    @Test(dataProvider = "highlightPlatforms")
    public void highlightDrawsARectangleForEachPlatformBranch(String platform, String browserName, String appPackage,
                                                              double scalingFactor) throws Exception {
        String originalPlatform = SHAFT.Properties.platform.targetPlatform();
        String originalBrowser = SHAFT.Properties.mobile.browserName();
        String originalPackage = SHAFT.Properties.mobile.appPackage();
        double originalScale = SHAFT.Properties.visuals.screenshotParamsScalingFactor();
        try {
            SHAFT.Properties.platform.set().targetPlatform(platform);
            SHAFT.Properties.mobile.set().browserName(browserName);
            SHAFT.Properties.mobile.set().appPackage(appPackage);
            SHAFT.Properties.visuals.set().screenshotParamsScalingFactor(scalingFactor);

            byte[] highlighted = ImageProcessingActions.highlightElementInScreenshot(
                    png(90, 90, Color.WHITE, false),
                    new Rectangle(20, 20, 12, 12),
                    Color.RED);
            Assert.assertTrue(highlighted.length > 0);
            Assert.assertNotNull(ImageIO.read(new ByteArrayInputStream(highlighted)));

            byte[] alphaHighlighted = ImageProcessingActions.highlightElementInScreenshot(
                    png(40, 30, new Color(0, 0, 255, 128), true),
                    new Rectangle(4, 4, 8, 8),
                    Color.GREEN);
            Assert.assertTrue(alphaHighlighted.length > 0);
            Assert.assertNotNull(ImageIO.read(new ByteArrayInputStream(alphaHighlighted)));
        } finally {
            SHAFT.Properties.platform.set().targetPlatform(originalPlatform);
            SHAFT.Properties.mobile.set().browserName(originalBrowser);
            SHAFT.Properties.mobile.set().appPackage(originalPackage);
            SHAFT.Properties.visuals.set().screenshotParamsScalingFactor(originalScale);
        }
    }

    @Test
    public void highlightReturnsTheOriginalBytesWhenTheImageCannotBeDecoded() {
        Assert.assertEquals(ImageProcessingActions.highlightElementInScreenshot(
                null, new Rectangle(0, 0, 1, 1), Color.RED).length, 0);
        byte[] junk = new byte[]{1, 2, 3};
        Assert.assertSame(ImageProcessingActions.highlightElementInScreenshot(
                junk, new Rectangle(0, 0, 1, 1), Color.RED), junk);
    }

    @Test
    public void compareImageFoldersPassesForIdenticalFilesAndFailsForMismatches() throws Exception {
        Path root = Files.createTempDirectory("shaft-image-folders");
        try {
            Path reference = root.resolve("reference");
            Path test = root.resolve("test");
            Files.createDirectories(reference);
            Files.createDirectories(test);
            byte[] image = png(12, 12, Color.BLUE, false);
            Files.write(reference.resolve("one.png"), image);
            Files.write(test.resolve("one.png"), image);
            ImageProcessingActions.compareImageFolders(reference.toString(), test.toString(), 100);

            Files.write(test.resolve("one.png"), png(12, 12, Color.RED, false));
            Assert.assertThrows(AssertionError.class,
                    () -> ImageProcessingActions.compareImageFolders(reference.toString(), test.toString(), 100));

            Files.write(test.resolve("one.png"), png(8, 20, Color.BLUE, false));
            Assert.assertThrows(AssertionError.class,
                    () -> ImageProcessingActions.compareImageFolders(reference.toString(), test.toString(), 100));

            Files.write(test.resolve("two.png"), image);
            Assert.assertThrows(RuntimeException.class,
                    () -> ImageProcessingActions.compareImageFolders(reference.toString(), test.toString(), 100));

            Assert.assertThrows(RuntimeException.class,
                    () -> ImageProcessingActions.compareImageFolders(root.resolve("missing").toString(), test.toString(), 100));
        } finally {
            delete(root);
        }
    }

    private static byte[] png(int width, int height, Color color, boolean alpha) throws IOException {
        BufferedImage image = new BufferedImage(width, height,
                alpha ? BufferedImage.TYPE_INT_ARGB : BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        try {
            graphics.setColor(color);
            graphics.fillRect(0, 0, width, height);
        } finally {
            graphics.dispose();
        }
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        Assert.assertTrue(ImageIO.write(image, "png", output));
        return output.toByteArray();
    }

    private static void delete(Path root) throws IOException {
        if (!Files.exists(root)) {
            return;
        }
        try (var walk = Files.walk(root)) {
            walk.sorted(java.util.Comparator.reverseOrder()).forEach(path -> {
                try {
                    Files.deleteIfExists(path);
                } catch (IOException exception) {
                    throw new java.io.UncheckedIOException(exception);
                }
            });
        }
    }
}
