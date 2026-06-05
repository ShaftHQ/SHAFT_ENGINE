package com.shaft.gui.internal.image;

import com.applitools.eyes.AppEnvironment;
import com.applitools.eyes.BatchInfo;
import com.applitools.eyes.ImageMatchSettings;
import com.applitools.eyes.SessionStartInfo;
import com.applitools.eyes.TestResults;
import com.applitools.eyes.exceptions.DiffsFoundException;
import com.applitools.eyes.images.Eyes;
import com.assertthat.selenium_shutterbug.core.CaptureElement;
import com.assertthat.selenium_shutterbug.core.ElementSnapshot;
import com.assertthat.selenium_shutterbug.core.Shutterbug;
import com.assertthat.selenium_shutterbug.utils.image.UnableToCompareImagesException;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.properties.internal.Properties;
import nu.pattern.OpenCV;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.openqa.selenium.UnsupportedCommandException;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ImageProcessingActionsCoverageUnitTest {
    private static final FileActions FILE_ACTIONS = FileActions.getInstance(true);
    private static final Path TEMP_ROOT = Path.of("target", "temp", "imageProcessingActionsCoverageUnitTest");

    private Path tempDir;

    @BeforeMethod(alwaysRun = true)
    public void beforeMethod(Method method) throws Exception {
        tempDir = TEMP_ROOT.resolve(method.getName() + "-" + Thread.currentThread().threadId());
        FILE_ACTIONS.deleteFolder(tempDir.toString());
        FILE_ACTIONS.createFolder(tempDir.toString());
        setAiFolderPath(tempDir.toAbsolutePath() + File.separator);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws Exception {
        setAiFolderPath(null);
        if (tempDir != null) {
            FILE_ACTIONS.deleteFolder(tempDir.toString());
        }
        Properties.clearForCurrentThread();
    }

    @Test
    public void imageBytesShouldEncodeAndDecodeDeterministicBufferedImage() throws Exception {
        byte[] imageBytes = createPng(12, 9, Color.BLUE);

        BufferedImage decoded = ImageIO.read(new ByteArrayInputStream(imageBytes));
        ByteArrayOutputStream reEncodedOutput = new ByteArrayOutputStream();
        ImageIO.write(decoded, "png", reEncodedOutput);
        BufferedImage reDecoded = ImageIO.read(new ByteArrayInputStream(reEncodedOutput.toByteArray()));

        Assert.assertEquals(decoded.getWidth(), 12);
        Assert.assertEquals(decoded.getHeight(), 9);
        Assert.assertEquals(reDecoded.getWidth(), decoded.getWidth());
        Assert.assertEquals(reDecoded.getHeight(), decoded.getHeight());
        Assert.assertEquals(reDecoded.getRGB(1, 1), decoded.getRGB(1, 1));
    }

    @Test
    public void formatElementLocatorShouldReturnStableCachedHash() throws Exception {
        By locator = By.cssSelector("section[data-id='login'] input[name='email']");

        String firstHash = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        String secondHash = ImageProcessingActions.formatElementLocatorToImagePath(locator);

        Assert.assertEquals(secondHash, firstHash);
        Assert.assertEquals(firstHash.length(), 64);
        Assert.assertTrue(getLocatorHashCache().containsValue(firstHash));
    }

    @Test(dataProvider = "highlightPlatforms")
    public void highlightElementInScreenshotShouldReturnDecodableImageForPlatformBranches(String platform,
                                                                                         String browserName,
                                                                                         String appPackage,
                                                                                         double scalingFactor) throws Exception {
        SHAFT.Properties.platform.set().targetPlatform(platform);
        SHAFT.Properties.mobile.set().browserName(browserName);
        SHAFT.Properties.mobile.set().appPackage(appPackage);
        SHAFT.Properties.visuals.set().screenshotParamsScalingFactor(scalingFactor);

        byte[] highlighted = ImageProcessingActions.highlightElementInScreenshot(
                createPng(90, 90, Color.WHITE),
                new org.openqa.selenium.Rectangle(20, 20, 12, 12),
                Color.RED
        );

        Assert.assertTrue(highlighted.length > 0);
        Assert.assertNotNull(ImageIO.read(new ByteArrayInputStream(highlighted)));
    }

    @DataProvider
    public Object[][] highlightPlatforms() {
        return new Object[][]{
                {Platform.LINUX.name(), "", "", 1.0},
                {Platform.WINDOWS.name(), "", "", 1.25},
                {Platform.MAC.name(), "", "", 1.0},
                {Platform.IOS.name(), "Safari", "", 1.0},
                {Platform.ANDROID.name(), "", "com.android.chrome", 1.0}
        };
    }

    @Test
    public void highlightElementInScreenshotShouldThrowForUnsupportedImageBytes() {
        Assert.assertThrows(Throwable.class, () -> ImageProcessingActions.highlightElementInScreenshot(
                "not-an-image".getBytes(StandardCharsets.UTF_8),
                new org.openqa.selenium.Rectangle(1, 1, 2, 2),
                Color.GREEN
        ));
    }

    @Test
    public void findImageWithinCurrentPageShouldReturnCoordinatesWhenReferenceIsCroppedFromScreenshot() {
        byte[] screenshot = createPatternPng(64, 64, 16, 18, 18, 14, Color.ORANGE);
        byte[] croppedReference = createPng(18, 14, Color.ORANGE);
        Path referenceImage = tempDir.resolve("cropped-reference.png");
        FILE_ACTIONS.writeToFile(referenceImage.toString(), croppedReference);

        List<Integer> coordinates = ImageProcessingActions.findImageWithinCurrentPage(referenceImage.toString(), screenshot);

        Assert.assertFalse(coordinates.isEmpty());
        Assert.assertEquals(coordinates.size(), 2);
    }

    @Test
    public void findImageWithinCurrentPageShouldReturnEmptyForInvalidOrEmptyInputs() {
        Path invalidReference = tempDir.resolve("invalid-reference.png");
        FILE_ACTIONS.writeToFile(invalidReference.toString(), "invalid-image");

        Assert.assertTrue(ImageProcessingActions.findImageWithinCurrentPage(invalidReference.toString(), createPng(10, 10, Color.BLACK)).isEmpty());
        Assert.assertTrue(ImageProcessingActions.findImageWithinCurrentPage(invalidReference.toString(), new byte[0]).isEmpty());
        Assert.assertTrue(ImageProcessingActions.findImageWithinCurrentPage(invalidReference.toString(), null).isEmpty());
    }

    @Test
    public void findImageWithinCurrentPageShouldRespectThresholdForNegativeComparison() {
        double impossibleThreshold = 1.1;
        SHAFT.Properties.visuals.set().visualMatchingThreshold(impossibleThreshold);
        Path referenceImage = tempDir.resolve("threshold-reference.png");
        byte[] screenshot = createPatternPng(30, 30, 4, 4, 8, 8, Color.CYAN);
        FILE_ACTIONS.writeToFile(referenceImage.toString(), createPng(8, 8, Color.CYAN));

        Assert.assertTrue(ImageProcessingActions.findImageWithinCurrentPage(referenceImage.toString(), screenshot).isEmpty());
    }

    @Test
    public void referenceImageHelpersShouldReturnMissingAndExistingBaselineImagesFromTempDirectory() throws Exception {
        By locator = By.id("avatar");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        byte[] referenceBytes = createPng(8, 8, Color.YELLOW);
        byte[] diffBytes = createPng(8, 8, Color.RED);

        Assert.assertNull(ImageProcessingActions.getReferenceImage(locator));
        Assert.assertEquals(ImageProcessingActions.getShutterbugDifferencesImage(locator), new byte[0]);

        FILE_ACTIONS.writeToFile(tempDir.resolve(hashed + ".png").toString(), referenceBytes);
        FILE_ACTIONS.writeToFile(tempDir.resolve(hashed + "_shutterbug.png").toString(), diffBytes);

        Assert.assertEquals(ImageProcessingActions.getReferenceImage(locator), referenceBytes);
        Assert.assertEquals(ImageProcessingActions.getShutterbugDifferencesImage(locator), diffBytes);
    }

    @Test
    public void referenceImageHelperShouldResolveFolderPathWhenUnset() throws Exception {
        setAiFolderPath("");
        By locator = By.id("resolved-folder");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        byte[] referenceBytes = createPng(6, 6, Color.GREEN);
        FILE_ACTIONS.writeToFile(tempDir.resolve(hashed + ".png").toString(), referenceBytes);

        try (MockedStatic<ScreenshotHelper> screenshotHelperMocked = Mockito.mockStatic(ScreenshotHelper.class)) {
            screenshotHelperMocked.when(ScreenshotHelper::getAiAidedElementIdentificationFolderPath)
                    .thenReturn(tempDir.toAbsolutePath() + File.separator);

            Assert.assertEquals(ImageProcessingActions.getReferenceImage(locator), referenceBytes);
        }
    }

    @Test
    public void compareAgainstBaselineExactOpenCvShouldCreateMissingBaselineAndPass() {
        By locator = By.id("new-open-cv-baseline");
        byte[] screenshot = createPng(14, 14, Color.PINK);

        boolean result = ImageProcessingActions.compareAgainstBaseline(mock(WebDriver.class), locator, screenshot,
                ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV);

        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        Assert.assertTrue(result);
        Assert.assertTrue(FILE_ACTIONS.doesFileExist(tempDir.resolve(hashed + ".png").toString()));
    }

    @Test
    public void compareAgainstBaselineExactOpenCvShouldPassAndFailWhenBaselineAlreadyExists() {
        By matchingLocator = By.id("existing-open-cv-match");
        String matchingHash = ImageProcessingActions.formatElementLocatorToImagePath(matchingLocator);
        byte[] reference = createPng(20, 20, Color.GRAY);
        FILE_ACTIONS.writeToFile(tempDir.resolve(matchingHash + ".png").toString(), reference);

        Assert.assertTrue(ImageProcessingActions.compareAgainstBaseline(mock(WebDriver.class), matchingLocator, reference,
                ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV));

        By failingLocator = By.id("existing-open-cv-mismatch");
        String failingHash = ImageProcessingActions.formatElementLocatorToImagePath(failingLocator);
        FILE_ACTIONS.writeToFile(tempDir.resolve(failingHash + ".png").toString(), reference);

        Assert.assertFalse(ImageProcessingActions.compareAgainstBaseline(mock(WebDriver.class), failingLocator, new byte[0],
                ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV));
    }

    @Test
    public void compareAgainstBaselineExactShutterbugShouldCreateMissingReference() {
        By locator = By.id("new-shutterbug-baseline");
        byte[] screenshot = createPng(12, 12, Color.ORANGE);

        boolean result = ImageProcessingActions.compareAgainstBaseline(mock(WebDriver.class), locator, screenshot,
                ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG);

        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        Assert.assertTrue(result);
        Assert.assertTrue(FILE_ACTIONS.doesFileExist(tempDir.resolve(hashed + ".png").toString()));
    }

    @Test
    public void compareAgainstBaselineExactShutterbugShouldReturnExistingComparisonResultAndHandleIOException() throws Exception {
        By positiveLocator = By.id("existing-shutterbug-positive");
        String positiveHash = ImageProcessingActions.formatElementLocatorToImagePath(positiveLocator);
        byte[] screenshot = createPng(12, 12, Color.MAGENTA);
        FILE_ACTIONS.writeToFile(tempDir.resolve(positiveHash + ".png").toString(), screenshot);
        ElementSnapshot positiveSnapshot = mock(ElementSnapshot.class);
        when(positiveSnapshot.equalsWithDiff(anyString(), anyString(), anyDouble())).thenReturn(true);

        By negativeLocator = By.id("existing-shutterbug-io-negative");
        String negativeHash = ImageProcessingActions.formatElementLocatorToImagePath(negativeLocator);
        FILE_ACTIONS.writeToFile(tempDir.resolve(negativeHash + ".png").toString(), screenshot);
        ElementSnapshot negativeSnapshot = mock(ElementSnapshot.class);
        when(negativeSnapshot.equalsWithDiff(anyString(), anyString(), anyDouble())).thenThrow(new java.io.IOException("forced"));

        try (MockedStatic<Shutterbug> shutterbugMocked = Mockito.mockStatic(Shutterbug.class)) {
            shutterbugMocked.when(() -> Shutterbug.shootElement(any(WebDriver.class), Mockito.eq(positiveLocator), Mockito.eq(CaptureElement.VIEWPORT), Mockito.eq(true)))
                    .thenReturn(positiveSnapshot);
            shutterbugMocked.when(() -> Shutterbug.shootElement(any(WebDriver.class), Mockito.eq(negativeLocator), Mockito.eq(CaptureElement.VIEWPORT), Mockito.eq(true)))
                    .thenReturn(negativeSnapshot);

            Assert.assertTrue(ImageProcessingActions.compareAgainstBaseline(mock(WebDriver.class), positiveLocator, screenshot,
                    ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG));
            Assert.assertFalse(ImageProcessingActions.compareAgainstBaseline(mock(WebDriver.class), negativeLocator, screenshot,
                    ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG));
        }
    }

    @Test
    public void compareAgainstBaselineExactShutterbugShouldFallbackToOpenCvForUnsupportedComparison() throws Exception {
        By locator = By.id("fallback-shutterbug");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        byte[] screenshot = createPng(12, 12, Color.CYAN);
        FILE_ACTIONS.writeToFile(tempDir.resolve(hashed + ".png").toString(), screenshot);

        ElementSnapshot snapshot = mock(ElementSnapshot.class);
        when(snapshot.equalsWithDiff(anyString(), anyString(), anyDouble()))
                .thenThrow(new UnableToCompareImagesException("dimensions mismatch"));

        try (MockedStatic<Shutterbug> shutterbugMocked = Mockito.mockStatic(Shutterbug.class)) {
            shutterbugMocked.when(() -> Shutterbug.shootElement(any(WebDriver.class), Mockito.eq(locator), Mockito.eq(CaptureElement.VIEWPORT), Mockito.eq(true)))
                    .thenReturn(snapshot);

            Assert.assertTrue(ImageProcessingActions.compareAgainstBaseline(mock(WebDriver.class), locator, screenshot,
                    ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG));
        }
    }

    @Test
    public void compareAgainstBaselineExactShutterbugShouldFallbackToOpenCvForUnsupportedCommand() throws Exception {
        By locator = By.id("fallback-unsupported-command");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        byte[] screenshot = createPng(12, 12, Color.BLUE);
        FILE_ACTIONS.writeToFile(tempDir.resolve(hashed + ".png").toString(), screenshot);

        ElementSnapshot snapshot = mock(ElementSnapshot.class);
        when(snapshot.equalsWithDiff(anyString(), anyString(), anyDouble()))
                .thenThrow(new UnsupportedCommandException("unsupported"));

        try (MockedStatic<Shutterbug> shutterbugMocked = Mockito.mockStatic(Shutterbug.class)) {
            shutterbugMocked.when(() -> Shutterbug.shootElement(any(WebDriver.class), Mockito.eq(locator), Mockito.eq(CaptureElement.VIEWPORT), Mockito.eq(true)))
                    .thenReturn(snapshot);

            Assert.assertTrue(ImageProcessingActions.compareAgainstBaseline(mock(WebDriver.class), locator, screenshot,
                    ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG));
        }
    }

    @Test(dataProvider = "eyesEngines")
    public void compareAgainstBaselineEyesShouldPassForNewOrPassedResults(ImageProcessingActions.VisualValidationEngine engine,
                                                                          boolean newResult,
                                                                          boolean passedResult) {
        TestResults testResults = mock(TestResults.class);
        when(testResults.isNew()).thenReturn(newResult);
        when(testResults.isPassed()).thenReturn(passedResult);

        try (MockedConstruction<Eyes> ignored = Mockito.mockConstruction(Eyes.class, (mock, context) -> when(mock.close()).thenReturn(testResults))) {
            Assert.assertTrue(ImageProcessingActions.compareAgainstBaseline(mock(WebDriver.class), By.id("eyes-" + engine.name()),
                    createPng(10, 10, Color.LIGHT_GRAY), engine));
        }
    }

    @DataProvider
    public Object[][] eyesEngines() {
        return new Object[][]{
                {ImageProcessingActions.VisualValidationEngine.EXACT_EYES, false, true},
                {ImageProcessingActions.VisualValidationEngine.STRICT_EYES, true, false},
                {ImageProcessingActions.VisualValidationEngine.CONTENT_EYES, true, true},
                {ImageProcessingActions.VisualValidationEngine.LAYOUT_EYES, false, true}
        };
    }

    @Test
    public void compareAgainstBaselineEyesShouldReturnFalseForDiffsFoundAndUseMobileHostBranches() {
        SessionStartInfo sessionStartInfo = new SessionStartInfo("agent", null, "app", "version", "scenario", new BatchInfo("batch"),
                null, null, new AppEnvironment(), new ImageMatchSettings(), null, null, null, null, Collections.emptyList());
        DiffsFoundException diffsFoundException = new DiffsFoundException(mock(TestResults.class), sessionStartInfo);

        try (MockedStatic<DriverFactoryHelper> driverFactoryMocked = Mockito.mockStatic(DriverFactoryHelper.class);
             MockedConstruction<Eyes> ignored = Mockito.mockConstruction(Eyes.class, (mock, context) -> when(mock.close()).thenThrow(diffsFoundException))) {
            driverFactoryMocked.when(DriverFactoryHelper::isMobileNativeExecution).thenReturn(true);
            driverFactoryMocked.when(DriverFactoryHelper::isMobileWebExecution).thenReturn(false);

            Assert.assertFalse(ImageProcessingActions.compareAgainstBaseline(mock(WebDriver.class), By.id("eyes-diff"),
                    createPng(10, 10, Color.DARK_GRAY), ImageProcessingActions.VisualValidationEngine.STRICT_EYES));
        }

        TestResults testResults = mock(TestResults.class);
        when(testResults.isNew()).thenReturn(true);
        try (MockedStatic<DriverFactoryHelper> driverFactoryMocked = Mockito.mockStatic(DriverFactoryHelper.class);
             MockedConstruction<Eyes> ignored = Mockito.mockConstruction(Eyes.class, (mock, context) -> when(mock.close()).thenReturn(testResults))) {
            driverFactoryMocked.when(DriverFactoryHelper::isMobileNativeExecution).thenReturn(false);
            driverFactoryMocked.when(DriverFactoryHelper::isMobileWebExecution).thenReturn(true);

            Assert.assertTrue(ImageProcessingActions.compareAgainstBaseline(mock(WebDriver.class), By.id("eyes-mobile-web"),
                    createPng(10, 10, Color.DARK_GRAY), ImageProcessingActions.VisualValidationEngine.LAYOUT_EYES));
        }
    }

    @Test
    public void compareImageFoldersShouldHandleMatchingMismatchingAndInvalidFolders() {
        Path matchingReference = tempDir.resolve("matching-reference");
        Path matchingTest = tempDir.resolve("matching-test");
        FILE_ACTIONS.createFolder(matchingReference.toString());
        FILE_ACTIONS.createFolder(matchingTest.toString());
        byte[] image = createPng(16, 16, Color.BLUE);
        FILE_ACTIONS.writeToFile(matchingReference.resolve("one.png").toString(), image);
        FILE_ACTIONS.writeToFile(matchingTest.resolve("one.png").toString(), image);

        ImageProcessingActions.compareImageFolders(matchingReference.toString(), matchingTest.toString(), 100);
        Assert.assertFalse(FILE_ACTIONS.doesFileExist(matchingTest.resolve("failedImagesDirectory").toString()));

        Path resizedReference = tempDir.resolve("resized-reference");
        Path resizedTest = tempDir.resolve("resized-test");
        FILE_ACTIONS.createFolder(resizedReference.toString());
        FILE_ACTIONS.createFolder(resizedTest.toString());
        FILE_ACTIONS.writeToFile(resizedReference.resolve("one.png").toString(), createPng(16, 16, Color.BLUE));
        FILE_ACTIONS.writeToFile(resizedTest.resolve("one.png").toString(), createPng(18, 18, Color.BLUE));

        ImageProcessingActions.compareImageFolders(resizedReference.toString(), resizedTest.toString(), 0);
        Assert.assertFalse(FILE_ACTIONS.doesFileExist(resizedTest.resolve("failedImagesDirectory").toString()));
    }

    @Test
    public void loadOpenCvShouldSwitchHighlightingToJavaScriptWhenLoadingFails() {
        try (MockedStatic<OpenCV> openCvMocked = Mockito.mockStatic(OpenCV.class)) {
            openCvMocked.when(OpenCV::loadLocally).thenThrow(new RuntimeException("forced"));

            ImageProcessingActions.loadOpenCV();

            Assert.assertEquals(SHAFT.Properties.visuals.screenshotParamsHighlightMethod(), "JavaScript");
        }
    }

    @Test
    public void privateConstructorShouldThrowUtilityClassException() throws Exception {
        Constructor<ImageProcessingActions> constructor = ImageProcessingActions.class.getDeclaredConstructor();
        constructor.setAccessible(true);

        InvocationTargetException exception = Assert.expectThrows(InvocationTargetException.class, constructor::newInstance);

        Assert.assertTrue(exception.getTargetException() instanceof IllegalStateException);
        Assert.assertEquals(exception.getTargetException().getMessage(), "Utility class");
    }

    @SuppressWarnings("unchecked")
    private static Map<String, String> getLocatorHashCache() throws Exception {
        Field mapField = ImageProcessingActions.class.getDeclaredField("locatorHashMapping");
        mapField.setAccessible(true);
        return (Map<String, String>) mapField.get(null);
    }

    private static void setAiFolderPath(String value) throws Exception {
        Field aiFolderPathField = ImageProcessingActions.class.getDeclaredField("aiFolderPath");
        aiFolderPathField.setAccessible(true);
        @SuppressWarnings("unchecked")
        ThreadLocal<String> aiFolderPath = (ThreadLocal<String>) aiFolderPathField.get(null);
        if (value == null) {
            aiFolderPath.remove();
        } else {
            aiFolderPath.set(value);
        }
    }

    private static byte[] createPng(int width, int height, Color color) {
        return encodePng(createImage(width, height, color));
    }

    private static byte[] createPatternPng(int width, int height, int x, int y, int cropWidth, int cropHeight, Color cropColor) {
        BufferedImage image = createImage(width, height, Color.WHITE);
        Graphics2D graphics = image.createGraphics();
        graphics.setColor(Color.BLACK);
        graphics.drawRect(0, 0, width - 1, height - 1);
        graphics.setColor(cropColor);
        graphics.fillRect(x, y, cropWidth, cropHeight);
        graphics.dispose();
        return encodePng(image);
    }

    private static BufferedImage createImage(int width, int height, Color color) {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        graphics.setColor(color);
        graphics.fillRect(0, 0, width, height);
        graphics.dispose();
        return image;
    }

    private static byte[] encodePng(BufferedImage image) {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try {
            ImageIO.write(image, "png", output);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
        return output.toByteArray();
    }
}
