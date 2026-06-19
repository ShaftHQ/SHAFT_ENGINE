package testPackage.unitTests;

import com.applitools.eyes.TestResults;
import com.applitools.eyes.images.Eyes;
import com.assertthat.selenium_shutterbug.core.CaptureElement;
import com.assertthat.selenium_shutterbug.core.ElementSnapshot;
import com.assertthat.selenium_shutterbug.core.Shutterbug;
import com.assertthat.selenium_shutterbug.utils.image.UnableToCompareImagesException;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotHelper;
import com.shaft.properties.internal.Properties;
import nu.pattern.OpenCV;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Map;

public class ImageProcessingActionsUnitTest {
    private static final FileActions testFileActions = FileActions.getInstance(true);
    private static final Path TEMP_ROOT = Path.of("target", "temp", "imageProcessingUnitTests");
    private final ThreadLocal<Path> tempDir = new ThreadLocal<>();

    @BeforeMethod(alwaysRun = true)
    public void setup(Method method) throws Exception {
        Path methodTempDir = TEMP_ROOT.resolve(method.getName() + "-" + Thread.currentThread().threadId());
        tempDir.set(methodTempDir);
        testFileActions.deleteFolder(methodTempDir.toString());
        testFileActions.createFolder(methodTempDir.toString());
        setAiFolderPath(methodTempDir.toAbsolutePath().toString() + File.separator);
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws Exception {
        Path methodTempDir = tempDir.get();
        if (methodTempDir != null) {
            testFileActions.deleteFolder(methodTempDir.toString());
            tempDir.remove();
        }
        setAiFolderPath(null);
        Properties.clearForCurrentThread();
    }

    @Test
    public void formatElementLocatorShouldBeDeterministicAndCached() throws Exception {
        By locator = By.id("username");
        String first = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        String second = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        Assert.assertEquals(first, second);

        Map<?, ?> cache = getLocatorHashCache();
        Assert.assertTrue(cache.containsValue(first));
    }

    @Test
    public void referenceImageMethodsShouldHandleExistingAndMissingFiles() {
        By locator = By.id("avatar");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        byte[] imageBytes = "img".getBytes(StandardCharsets.UTF_8);
        testFileActions.writeToFile(tempDir().resolve(hashed + ".png").toString(), imageBytes);

        Assert.assertEquals(ImageProcessingActions.getReferenceImage(locator), imageBytes);
        Assert.assertEquals(ImageProcessingActions.getShutterbugDifferencesImage(locator), new byte[0]);
        Assert.assertNull(ImageProcessingActions.getReferenceImage(By.id("missing")));
    }

    @Test
    public void referenceImageMethodsShouldLoadAiFolderPathWhenUnset() throws Exception {
        setAiFolderPath("");
        By locator = By.id("avatarShutterbug");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        byte[] imageBytes = "img".getBytes(StandardCharsets.UTF_8);
        byte[] shutterbugBytes = "diff".getBytes(StandardCharsets.UTF_8);
        testFileActions.writeToFile(tempDir().resolve(hashed + ".png").toString(), imageBytes);
        testFileActions.writeToFile(tempDir().resolve(hashed + "_shutterbug.png").toString(), shutterbugBytes);

        try (MockedStatic<ScreenshotHelper> screenshotHelperMock = org.mockito.Mockito.mockStatic(ScreenshotHelper.class)) {
            screenshotHelperMock.when(ScreenshotHelper::getAiAidedElementIdentificationFolderPath).thenReturn(tempDir().toAbsolutePath().toString() + File.separator);
            Assert.assertEquals(ImageProcessingActions.getReferenceImage(locator), imageBytes);
            Assert.assertEquals(ImageProcessingActions.getShutterbugDifferencesImage(locator), shutterbugBytes);
        }
    }

    @Test
    public void compareAgainstBaselineExactOpenCvShouldCreateReferenceOnFirstRun() {
        By locator = By.id("newElement");
        byte[] screenshot = "element-shot".getBytes(StandardCharsets.UTF_8);
        boolean result = ImageProcessingActions.compareAgainstBaseline(
                org.mockito.Mockito.mock(WebDriver.class),
                locator,
                screenshot,
                ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV
        );

        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        Assert.assertTrue(result);
        Assert.assertTrue(testFileActions.doesFileExist(tempDir().resolve(hashed + ".png").toString()));
    }

    @Test
    public void findImageWithinCurrentPageShouldReturnEmptyForNullScreenshot() {
        Assert.assertTrue(ImageProcessingActions.findImageWithinCurrentPage("missing-reference.png", null).isEmpty());
    }

    @Test
    public void compareImageFoldersShouldFailWhenFolderCountsMismatch() {
        Path reference = tempDir().resolve("reference");
        Path test = tempDir().resolve("test");
        testFileActions.createFolder(reference.toString());
        testFileActions.createFolder(test.toString());
        testFileActions.writeToFile(reference.resolve("one.txt").toString(), "1");
        Assert.assertThrows(Throwable.class, () -> ImageProcessingActions.compareImageFolders(reference.toString(), test.toString(), 90));
    }

    @Test
    public void compareImageFoldersShouldPassWhenImagesAreIdentical() {
        Path reference = tempDir().resolve("reference-pass");
        Path test = tempDir().resolve("test-pass");
        testFileActions.createFolder(reference.toString());
        testFileActions.createFolder(test.toString());

        byte[] image = createPng(20, 20, Color.BLUE);
        testFileActions.writeToFile(reference.resolve("one.png").toString(), image);
        testFileActions.writeToFile(test.resolve("one.png").toString(), image);

        ImageProcessingActions.compareImageFolders(reference.toString(), test.toString(), 100);
        Assert.assertFalse(testFileActions.doesFileExist(test.resolve("failedImagesDirectory").toString()));
        Assert.assertFalse(testFileActions.doesFileExist(reference.resolve("processingDirectory").toString()));
        Assert.assertFalse(testFileActions.doesFileExist(test.resolve("processingDirectory").toString()));
    }

    @Test
    public void compareImageFoldersShouldFailWhenImagesDiffer() {
        Path reference = tempDir().resolve("reference-fail");
        Path test = tempDir().resolve("test-fail");
        testFileActions.createFolder(reference.toString());
        testFileActions.createFolder(test.toString());
        testFileActions.writeToFile(reference.resolve("one.png").toString(), createPng(20, 20, Color.BLUE));
        testFileActions.writeToFile(test.resolve("one.png").toString(), createPng(20, 20, Color.RED));

        Assert.assertThrows(Throwable.class,
                () -> ImageProcessingActions.compareImageFolders(reference.toString(), test.toString(), 100));
        Assert.assertTrue(testFileActions.doesFileExist(test.resolve("failedImagesDirectory").toString()));
    }

    @Test
    public void findImageWithinCurrentPageShouldReturnCoordinatesWhenImageMatches() {
        Path referenceImage = tempDir().resolve("reference-image.png");
        byte[] screenshot = createPng(30, 30, Color.GREEN);
        testFileActions.writeToFile(referenceImage.toString(), screenshot);

        Assert.assertFalse(ImageProcessingActions.findImageWithinCurrentPage(referenceImage.toString(), screenshot).isEmpty());
    }

    @Test
    public void compareAgainstBaselineExactOpenCvShouldFailForDifferentImagesWhenReferenceExists() {
        By locator = By.id("differentElement");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        testFileActions.writeToFile(tempDir().resolve(hashed + ".png").toString(), createPng(30, 30, Color.YELLOW));

        boolean result = ImageProcessingActions.compareAgainstBaseline(
                org.mockito.Mockito.mock(WebDriver.class),
                locator,
                // empty screenshot forces OpenCV lookup to return no match against an existing baseline image
                new byte[0],
                ImageProcessingActions.VisualValidationEngine.EXACT_OPENCV
        );

        Assert.assertFalse(result);
    }

    @Test
    public void compareAgainstBaselineExactShutterbugShouldCreateReferenceWhenMissing() {
        By locator = By.id("newShutterbugElement");
        byte[] screenshot = createPng(20, 20, Color.ORANGE);

        boolean result = ImageProcessingActions.compareAgainstBaseline(
                org.mockito.Mockito.mock(WebDriver.class),
                locator,
                screenshot,
                ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG
        );

        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        Assert.assertTrue(result);
        Assert.assertTrue(testFileActions.doesFileExist(tempDir().resolve(hashed + ".png").toString()));
    }

    @Test
    public void highlightElementInScreenshotShouldReturnImageBytes() {
        byte[] highlighted = ImageProcessingActions.highlightElementInScreenshot(
                createPng(50, 50, Color.WHITE),
                new org.openqa.selenium.Rectangle(10, 10, 20, 20),
                Color.RED
        );

        Assert.assertTrue(highlighted.length > 0);
    }

    @Test
    public void highlightElementInScreenshotShouldHandleAlphaBearingScreenshot() {
        // Native mobile-app screenshots decode to an alpha-bearing image, and JPEG cannot encode
        // alpha (ImageIO.write returns false → empty byte[]), which previously dropped the
        // highlighted screenshot from the report. The highlight step must flatten alpha first so
        // it still returns usable, non-empty bytes. Regression for missing mobile screenshots.
        byte[] highlighted = ImageProcessingActions.highlightElementInScreenshot(
                createArgbPng(50, 50, Color.WHITE),
                new org.openqa.selenium.Rectangle(10, 10, 20, 20),
                Color.RED
        );

        Assert.assertTrue(highlighted.length > 0,
                "highlighting an alpha-bearing (mobile) screenshot must still produce image bytes");
    }

    @Test
    public void highlightElementInScreenshotShouldHandleWindowsScaling() {
        String originalTargetPlatform = SHAFT.Properties.platform.targetPlatform();
        double originalScalingFactor = SHAFT.Properties.visuals.screenshotParamsScalingFactor();
        try {
            SHAFT.Properties.platform.set().targetPlatform(Platform.WINDOWS.name());
            SHAFT.Properties.visuals.set().screenshotParamsScalingFactor(1.5);

            byte[] highlighted = ImageProcessingActions.highlightElementInScreenshot(
                    createPng(60, 60, Color.WHITE),
                    new org.openqa.selenium.Rectangle(10, 10, 20, 20),
                    Color.BLUE
            );
            Assert.assertTrue(highlighted.length > 0);
        } finally {
            SHAFT.Properties.platform.set().targetPlatform(originalTargetPlatform);
            SHAFT.Properties.visuals.set().screenshotParamsScalingFactor(originalScalingFactor);
        }
    }

    @Test
    public void findImageWithinCurrentPageShouldRetryAcrossOpenCvMatchMethods() {
        double originalThreshold = SHAFT.Properties.visuals.visualMatchingThreshold();
        try {
            SHAFT.Properties.visuals.set().visualMatchingThreshold(1.1);
            Path referenceImage = tempDir().resolve("retry-reference-image.png");
            byte[] screenshot = createPng(25, 25, Color.MAGENTA);
            testFileActions.writeToFile(referenceImage.toString(), screenshot);

            Assert.assertTrue(ImageProcessingActions.findImageWithinCurrentPage(referenceImage.toString(), screenshot).isEmpty());
        } finally {
            SHAFT.Properties.visuals.set().visualMatchingThreshold(originalThreshold);
        }
    }

    @Test
    public void compareAgainstBaselineExactShutterbugShouldReturnComparisonResultWhenReferenceExists() throws Exception {
        By locator = By.id("existingShutterbugElement");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        byte[] screenshot = createPng(20, 20, Color.CYAN);
        testFileActions.writeToFile(tempDir().resolve(hashed + ".png").toString(), screenshot);

        ElementSnapshot snapshot = org.mockito.Mockito.mock(ElementSnapshot.class);
        org.mockito.Mockito.when(snapshot.equalsWithDiff(org.mockito.Mockito.anyString(), org.mockito.Mockito.anyString(), org.mockito.Mockito.anyDouble())).thenReturn(true);

        try (MockedStatic<Shutterbug> shutterbugMock = org.mockito.Mockito.mockStatic(Shutterbug.class)) {
            shutterbugMock.when(() -> Shutterbug.shootElement(org.mockito.Mockito.any(WebDriver.class), org.mockito.Mockito.eq(locator), org.mockito.Mockito.eq(CaptureElement.VIEWPORT), org.mockito.Mockito.eq(true)))
                    .thenReturn(snapshot);

            Assert.assertTrue(ImageProcessingActions.compareAgainstBaseline(
                    org.mockito.Mockito.mock(WebDriver.class),
                    locator,
                    screenshot,
                    ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG
            ));
        }
    }

    @Test
    public void compareAgainstBaselineExactShutterbugShouldFallbackToOpenCv() throws Exception {
        By locator = By.id("fallbackShutterbugElement");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        byte[] screenshot = createPng(20, 20, Color.PINK);
        testFileActions.writeToFile(tempDir().resolve(hashed + ".png").toString(), screenshot);

        ElementSnapshot snapshot = org.mockito.Mockito.mock(ElementSnapshot.class);
        org.mockito.Mockito.when(snapshot.equalsWithDiff(org.mockito.Mockito.anyString(), org.mockito.Mockito.anyString(), org.mockito.Mockito.anyDouble()))
                .thenThrow(new UnableToCompareImagesException("dimensions mismatch"));

        try (MockedStatic<Shutterbug> shutterbugMock = org.mockito.Mockito.mockStatic(Shutterbug.class)) {
            shutterbugMock.when(() -> Shutterbug.shootElement(org.mockito.Mockito.any(WebDriver.class), org.mockito.Mockito.eq(locator), org.mockito.Mockito.eq(CaptureElement.VIEWPORT), org.mockito.Mockito.eq(true)))
                    .thenReturn(snapshot);

            Assert.assertTrue(ImageProcessingActions.compareAgainstBaseline(
                    org.mockito.Mockito.mock(WebDriver.class),
                    locator,
                    screenshot,
                    ImageProcessingActions.VisualValidationEngine.EXACT_SHUTTERBUG
            ));
        }
    }

    @Test
    public void compareAgainstBaselineEyesShouldReturnTrueForPassedResults() {
        TestResults testResults = org.mockito.Mockito.mock(TestResults.class);
        org.mockito.Mockito.when(testResults.isNew()).thenReturn(false);
        org.mockito.Mockito.when(testResults.isPassed()).thenReturn(true);

        try (MockedConstruction<Eyes> ignored = org.mockito.Mockito.mockConstruction(Eyes.class, (mock, context) -> {
            org.mockito.Mockito.when(mock.close()).thenReturn(testResults);
        })) {
            Assert.assertTrue(ImageProcessingActions.compareAgainstBaseline(
                    org.mockito.Mockito.mock(WebDriver.class),
                    By.id("eyesElement"),
                    createPng(20, 20, Color.GRAY),
                    ImageProcessingActions.VisualValidationEngine.STRICT_EYES
            ));
        }
    }

    @Test
    public void compareAgainstBaselineEyesShouldUseMobileNativeHostConfiguration() {
        TestResults testResults = org.mockito.Mockito.mock(TestResults.class);
        org.mockito.Mockito.when(testResults.isNew()).thenReturn(true);
        org.mockito.Mockito.when(testResults.isPassed()).thenReturn(false);

        try (MockedStatic<DriverFactoryHelper> driverFactoryMock = org.mockito.Mockito.mockStatic(DriverFactoryHelper.class);
             MockedConstruction<Eyes> ignored = org.mockito.Mockito.mockConstruction(Eyes.class, (mock, context) -> {
                 org.mockito.Mockito.when(mock.close()).thenReturn(testResults);
             })) {
            driverFactoryMock.when(DriverFactoryHelper::isMobileNativeExecution).thenReturn(true);
            driverFactoryMock.when(DriverFactoryHelper::isMobileWebExecution).thenReturn(false);

            Assert.assertTrue(ImageProcessingActions.compareAgainstBaseline(
                    org.mockito.Mockito.mock(WebDriver.class),
                    By.id("eyesNativeElement"),
                    createPng(20, 20, Color.GRAY),
                    ImageProcessingActions.VisualValidationEngine.EXACT_EYES
            ));
        }
    }

    @Test
    public void loadOpenCvShouldFallbackToJavaScriptWhenLoadingFails() {
        String originalMethod = SHAFT.Properties.visuals.screenshotParamsHighlightMethod();
        boolean originalDisableLogging = SHAFT.Properties.reporting.disableLogging();
        try (MockedStatic<OpenCV> openCvMock = org.mockito.Mockito.mockStatic(OpenCV.class)) {
            SHAFT.Properties.reporting.set().disableLogging(true);
            openCvMock.when(OpenCV::loadLocally).thenThrow(new RuntimeException("forced failure"));
            ImageProcessingActions.loadOpenCV();
            Assert.assertEquals(SHAFT.Properties.visuals.screenshotParamsHighlightMethod(), "JavaScript");
        } finally {
            SHAFT.Properties.visuals.set().screenshotParamsHighlightMethod(originalMethod);
            SHAFT.Properties.reporting.set().disableLogging(originalDisableLogging);
        }
    }

    @Test
    public void privateConstructorShouldThrowUtilityClassException() throws Exception {
        Constructor<ImageProcessingActions> constructor = ImageProcessingActions.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        InvocationTargetException exception = Assert.expectThrows(InvocationTargetException.class, constructor::newInstance);
        Assert.assertTrue(exception.getTargetException().getMessage().contains("Utility class"));
    }

    @SuppressWarnings("unchecked")
    private static Map<String, String> getLocatorHashCache() throws Exception {
        Field mapField = ImageProcessingActions.class.getDeclaredField("locatorHashMapping");
        mapField.setAccessible(true);
        return (Map<String, String>) mapField.get(null);
    }

    private static void clearLocatorHashCache() throws Exception {
        getLocatorHashCache().clear();
    }

    private Path tempDir() {
        Path methodTempDir = tempDir.get();
        if (methodTempDir == null) {
            throw new IllegalStateException("Test temp directory was not initialized for the current thread.");
        }
        return methodTempDir;
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
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = image.createGraphics();
        graphics.setColor(color);
        graphics.fillRect(0, 0, width, height);
        graphics.dispose();

        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try {
            ImageIO.write(image, "png", output);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        return output.toByteArray();
    }

    private static byte[] createArgbPng(int width, int height, Color color) {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
        Graphics2D graphics = image.createGraphics();
        graphics.setColor(color);
        graphics.fillRect(0, 0, width, height);
        graphics.dispose();

        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try {
            ImageIO.write(image, "png", output);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        return output.toByteArray();
    }

}
