package com.shaft.gui.internal.image;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.Arrays;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Coverage-focused unit tests for {@link ImageProcessingActions#compareScreenshotAgainstBaseline}, covering
 * the {@code matchesScreenshot()} baseline auto-create-on-first-run, {@code -Dshaft.updateSnapshots} lifecycle,
 * and the per-browser/OS baseline naming scheme with legacy fallback.
 */
public class ImageProcessingActionsScreenshotBaselineUnitTest {
    private static final FileActions FILE_ACTIONS = FileActions.getInstance(true);
    private static final Path TEMP_ROOT = Path.of("target", "temp", "imageProcessingActionsScreenshotBaselineUnitTest");

    private Path tempDir;

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod(Method method) throws Exception {
        tempDir = TEMP_ROOT.resolve(method.getName());
        FILE_ACTIONS.deleteFolder(tempDir.toString());
        FILE_ACTIONS.createFolder(tempDir.toString());
        setAiFolderPath(tempDir.toAbsolutePath() + File.separator);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws Exception {
        setAiFolderPath(null);
        VisualProcessingProviderRegistry.resetProviderForTesting();
        if (tempDir != null) {
            FILE_ACTIONS.deleteFolder(tempDir.toString());
        }
        Properties.clearForCurrentThread();
    }

    /**
     * Mirrors {@code ImageProcessingActions#browserPlatformSuffix()}: lowercase, non-alphanumerics stripped.
     */
    private static String browserPlatformSuffix() {
        String browser = SHAFT.Properties.web.targetBrowserName().toLowerCase().replaceAll("[^a-z0-9]", "");
        String platform = SHAFT.Properties.platform.targetPlatform().toLowerCase().replaceAll("[^a-z0-9]", "");
        return "_" + browser + "_" + platform;
    }

    @Test
    public void compareScreenshotAgainstBaselineByLocatorShouldCreateMissingBaselineAndPass() {
        By locator = By.id("new-screenshot-baseline");
        byte[] actual = png(Color.PINK);

        var result = ImageProcessingActions.compareScreenshotAgainstBaseline(locator, actual, null, null, null);

        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        String newSchemePath = tempDir.resolve(hashed + browserPlatformSuffix() + ".png").toString();
        Assert.assertTrue(result.matched());
        Assert.assertTrue(FILE_ACTIONS.doesFileExist(newSchemePath), "new baseline should be written under the new per-browser/OS naming scheme");
        Assert.assertEquals(FILE_ACTIONS.readFileAsByteArray(newSchemePath), actual);
        Assert.assertFalse(FILE_ACTIONS.doesFileExist(tempDir.resolve(hashed + ".png").toString()), "a fresh baseline must not be written under the legacy scheme");
    }

    @Test
    public void compareScreenshotAgainstBaselineByKeyShouldCreateMissingBaselineAndPass() {
        String baselineKey = "page_new-baseline";
        byte[] actual = png(Color.CYAN);

        var result = ImageProcessingActions.compareScreenshotAgainstBaseline(baselineKey, actual, null, null, null);

        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(baselineKey);
        Assert.assertTrue(result.matched());
        Assert.assertTrue(FILE_ACTIONS.doesFileExist(tempDir.resolve(hashed + browserPlatformSuffix() + ".png").toString()));
    }

    @Test
    public void updateSnapshotsShouldOverwriteExistingBaselineAndPass() {
        By locator = By.id("existing-baseline-to-update");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        String newSchemePath = tempDir.resolve(hashed + browserPlatformSuffix() + ".png").toString();
        byte[] oldBaseline = png(Color.GRAY);
        byte[] newScreenshot = png(Color.ORANGE);
        FILE_ACTIONS.writeToFile(newSchemePath, oldBaseline);
        SHAFT.Properties.visuals.set().updateSnapshots(true);

        var result = ImageProcessingActions.compareScreenshotAgainstBaseline(locator, newScreenshot, null, null, null);

        Assert.assertTrue(result.matched());
        Assert.assertEquals(FILE_ACTIONS.readFileAsByteArray(newSchemePath), newScreenshot);
    }

    @Test
    public void updateSnapshotsWithOnlyLegacyBaselineShouldWriteNewSchemeBaseline() {
        By locator = By.id("legacy-baseline-to-update");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        String legacyPath = tempDir.resolve(hashed + ".png").toString();
        String newSchemePath = tempDir.resolve(hashed + browserPlatformSuffix() + ".png").toString();
        byte[] legacyBaseline = png(Color.GRAY);
        byte[] newScreenshot = png(Color.ORANGE);
        FILE_ACTIONS.writeToFile(legacyPath, legacyBaseline);
        SHAFT.Properties.visuals.set().updateSnapshots(true);

        var result = ImageProcessingActions.compareScreenshotAgainstBaseline(locator, newScreenshot, null, null, null);

        Assert.assertTrue(result.matched());
        Assert.assertEquals(FILE_ACTIONS.readFileAsByteArray(newSchemePath), newScreenshot,
                "-Dshaft.updateSnapshots must always write the new per-browser/OS scheme, even when only a legacy baseline existed");
    }

    @Test
    public void existingBaselineShouldDelegateToProviderAndPersistDiffImageOnMismatch() {
        By locator = By.id("existing-baseline-mismatch");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        String newSchemePath = tempDir.resolve(hashed + browserPlatformSuffix() + ".png").toString();
        byte[] baseline = png(Color.GREEN);
        byte[] actual = png(Color.RED);
        byte[] diffImage = png(Color.MAGENTA);
        FILE_ACTIONS.writeToFile(newSchemePath, baseline);

        VisualProcessingProvider provider = mock(VisualProcessingProvider.class);
        when(provider.compareScreenshotAgainstBaseline(any(), any(), any(), any(), any()))
                .thenReturn(new VisualProcessingProvider.ScreenshotComparisonResult(false, diffImage, 42, 0.5));
        VisualProcessingProviderRegistry.setProviderForTesting(provider);

        var result = ImageProcessingActions.compareScreenshotAgainstBaseline(locator, actual, null, 10, null);

        Assert.assertFalse(result.matched());
        Assert.assertEquals(result.diffPixels(), 42);
        Assert.assertEquals(FILE_ACTIONS.readFileAsByteArray(tempDir.resolve(hashed + browserPlatformSuffix() + "_diff.png").toString()), diffImage);
    }

    @Test
    public void existingBaselineMatchShouldNotWriteDiffImage() {
        By locator = By.id("existing-baseline-match");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        String newSchemePath = tempDir.resolve(hashed + browserPlatformSuffix() + ".png").toString();
        byte[] baseline = png(Color.GREEN);
        byte[] actual = png(Color.GREEN);
        FILE_ACTIONS.writeToFile(newSchemePath, baseline);

        VisualProcessingProvider provider = mock(VisualProcessingProvider.class);
        when(provider.compareScreenshotAgainstBaseline(any(), any(), any(), any(), any()))
                .thenReturn(new VisualProcessingProvider.ScreenshotComparisonResult(true, new byte[0], 0, 0.0));
        VisualProcessingProviderRegistry.setProviderForTesting(provider);

        var result = ImageProcessingActions.compareScreenshotAgainstBaseline(locator, actual, null, null, null);

        Assert.assertTrue(result.matched());
        Assert.assertFalse(FILE_ACTIONS.doesFileExist(tempDir.resolve(hashed + browserPlatformSuffix() + "_diff.png").toString()));
    }

    @Test
    public void legacyBaselineOnlyShouldBeUsedAsFallbackAndNotBeUpgraded() {
        By locator = By.id("legacy-only-baseline");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        String legacyPath = tempDir.resolve(hashed + ".png").toString();
        String newSchemePath = tempDir.resolve(hashed + browserPlatformSuffix() + ".png").toString();
        byte[] legacyBaseline = png(Color.GREEN);
        byte[] actual = png(Color.GREEN);
        FILE_ACTIONS.writeToFile(legacyPath, legacyBaseline);

        VisualProcessingProvider provider = mock(VisualProcessingProvider.class);
        when(provider.compareScreenshotAgainstBaseline(any(), any(), any(), any(), any()))
                .thenReturn(new VisualProcessingProvider.ScreenshotComparisonResult(true, new byte[0], 0, 0.0));
        VisualProcessingProviderRegistry.setProviderForTesting(provider);

        var result = ImageProcessingActions.compareScreenshotAgainstBaseline(locator, actual, null, null, null);

        Assert.assertTrue(result.matched(), "an existing legacy baseline should be used instead of treating it as missing");
        Assert.assertFalse(FILE_ACTIONS.doesFileExist(newSchemePath), "a passing comparison against the legacy fallback must not silently create a new-scheme baseline");
    }

    @Test
    public void bothSchemesPresentShouldPreferNewSchemeBaseline() {
        By locator = By.id("both-schemes-present");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        String legacyPath = tempDir.resolve(hashed + ".png").toString();
        String newSchemePath = tempDir.resolve(hashed + browserPlatformSuffix() + ".png").toString();
        byte[] legacyBaseline = png(Color.RED);
        byte[] newSchemeBaseline = png(Color.GREEN);
        FILE_ACTIONS.writeToFile(legacyPath, legacyBaseline);
        FILE_ACTIONS.writeToFile(newSchemePath, newSchemeBaseline);

        VisualProcessingProvider provider = mock(VisualProcessingProvider.class);
        when(provider.compareScreenshotAgainstBaseline(any(byte[].class), any(), any(), any(), any()))
                .thenAnswer(invocation -> {
                    byte[] baselineArg = invocation.getArgument(0);
                    boolean matchesNewScheme = Arrays.equals(baselineArg, newSchemeBaseline);
                    return new VisualProcessingProvider.ScreenshotComparisonResult(matchesNewScheme, new byte[0], matchesNewScheme ? 0 : 99, matchesNewScheme ? 0.0 : 1.0);
                });
        VisualProcessingProviderRegistry.setProviderForTesting(provider);

        var result = ImageProcessingActions.compareScreenshotAgainstBaseline(locator, png(Color.GREEN), null, null, null);

        Assert.assertTrue(result.matched(), "when both schemes exist, the new per-browser/OS scheme must win");
    }

    private static byte[] png(Color color) {
        BufferedImage image = new BufferedImage(6, 6, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        graphics.setColor(color);
        graphics.fillRect(0, 0, 6, 6);
        graphics.dispose();
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try {
            javax.imageio.ImageIO.write(image, "png", output);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
        return output.toByteArray();
    }

    @SuppressWarnings("unchecked")
    private static void setAiFolderPath(String value) throws Exception {
        Field aiFolderPathField = ImageProcessingActions.class.getDeclaredField("aiFolderPath");
        aiFolderPathField.setAccessible(true);
        ThreadLocal<String> aiFolderPath = (ThreadLocal<String>) aiFolderPathField.get(null);
        if (value == null) {
            aiFolderPath.remove();
        } else {
            aiFolderPath.set(value);
        }
    }
}
