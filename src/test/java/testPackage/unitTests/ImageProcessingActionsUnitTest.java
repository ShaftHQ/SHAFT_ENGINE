package testPackage.unitTests;

import com.shaft.cli.FileActions;
import com.shaft.gui.internal.image.ImageProcessingActions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Map;

public class ImageProcessingActionsUnitTest {
    private static final FileActions testFileActions = FileActions.getInstance(true);
    private static final Path TEMP_DIR = Path.of("target", "temp", "imageProcessingUnitTests");

    @BeforeMethod(alwaysRun = true)
    public void setup() throws Exception {
        testFileActions.deleteFolder(TEMP_DIR.toString());
        testFileActions.createFolder(TEMP_DIR.toString());
        setAiFolderPath(TEMP_DIR.toAbsolutePath() + File.separator);
        clearLocatorHashCache();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        testFileActions.deleteFolder(TEMP_DIR.toString());
    }

    @Test
    public void formatElementLocatorShouldBeDeterministicAndCached() throws Exception {
        By locator = By.id("username");
        String first = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        String second = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        Assert.assertEquals(first, second);

        Map<?, ?> cache = getLocatorHashCache();
        Assert.assertEquals(cache.size(), 1);
    }

    @Test
    public void referenceImageMethodsShouldHandleExistingAndMissingFiles() {
        By locator = By.id("avatar");
        String hashed = ImageProcessingActions.formatElementLocatorToImagePath(locator);
        byte[] imageBytes = "img".getBytes(StandardCharsets.UTF_8);
        testFileActions.writeToFile(TEMP_DIR.resolve(hashed + ".png").toString(), imageBytes);

        Assert.assertEquals(ImageProcessingActions.getReferenceImage(locator), imageBytes);
        Assert.assertEquals(ImageProcessingActions.getShutterbugDifferencesImage(locator), new byte[0]);
        Assert.assertNull(ImageProcessingActions.getReferenceImage(By.id("missing")));
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
        Assert.assertTrue(testFileActions.doesFileExist(TEMP_DIR.resolve(hashed + ".png").toString()));
    }

    @Test
    public void findImageWithinCurrentPageShouldReturnEmptyForNullScreenshot() {
        Assert.assertTrue(ImageProcessingActions.findImageWithinCurrentPage("missing-reference.png", null).isEmpty());
    }

    @Test
    public void compareImageFoldersShouldFailWhenFolderCountsMismatch() {
        Path reference = TEMP_DIR.resolve("reference");
        Path test = TEMP_DIR.resolve("test");
        testFileActions.createFolder(reference.toString());
        testFileActions.createFolder(test.toString());
        testFileActions.writeToFile(reference.resolve("one.txt").toString(), "1");
        Assert.assertThrows(Throwable.class, () -> ImageProcessingActions.compareImageFolders(reference.toString(), test.toString(), 90));
    }

    @Test(description = "ImageProcessingActions has no hard Shutterbug imports in field/method types")
    public void testNoHardShutterbugImport() {
        for (var field : ImageProcessingActions.class.getDeclaredFields()) {
            Assert.assertFalse(field.getType().getName().contains("selenium_shutterbug"),
                    "No field should reference selenium-shutterbug types");
        }
        for (var m : ImageProcessingActions.class.getDeclaredMethods()) {
            Assert.assertFalse(m.getReturnType().getName().contains("selenium_shutterbug"),
                    "No method return type should reference selenium-shutterbug");
            for (var p : m.getParameterTypes())
                Assert.assertFalse(p.getName().contains("selenium_shutterbug"),
                        "No parameter type should reference selenium-shutterbug");
        }
    }

    @Test(description = "ImageProcessingActions has no hard OpenCV imports in field/method types")
    public void testNoHardOpenCVImport() {
        for (var field : ImageProcessingActions.class.getDeclaredFields()) {
            Assert.assertFalse(field.getType().getName().startsWith("org.opencv"),
                    "No field should reference org.opencv types");
            Assert.assertFalse(field.getType().getName().startsWith("nu.pattern"),
                    "No field should reference nu.pattern (OpenCV loader) types");
        }
        for (var m : ImageProcessingActions.class.getDeclaredMethods()) {
            Assert.assertFalse(m.getReturnType().getName().startsWith("org.opencv"),
                    "No method return type should reference org.opencv");
            Assert.assertFalse(m.getReturnType().getName().startsWith("nu.pattern"),
                    "No method return type should reference nu.pattern");
            for (var p : m.getParameterTypes()) {
                Assert.assertFalse(p.getName().startsWith("org.opencv"),
                        "No parameter type should reference org.opencv");
                Assert.assertFalse(p.getName().startsWith("nu.pattern"),
                        "No parameter type should reference nu.pattern");
            }
        }
    }

    @Test(description = "ImageProcessingActions has no hard Applitools imports in field/method types")
    public void testNoHardApplitoolsImport() {
        for (var field : ImageProcessingActions.class.getDeclaredFields()) {
            Assert.assertFalse(field.getType().getName().startsWith("com.applitools"),
                    "No field should reference com.applitools types");
        }
        for (var m : ImageProcessingActions.class.getDeclaredMethods()) {
            Assert.assertFalse(m.getReturnType().getName().startsWith("com.applitools"),
                    "No return type should reference com.applitools");
        }
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

    private static void setAiFolderPath(String value) throws Exception {
        Field aiFolderPath = ImageProcessingActions.class.getDeclaredField("aiFolderPath");
        aiFolderPath.setAccessible(true);
        aiFolderPath.set(null, value);
    }
}
