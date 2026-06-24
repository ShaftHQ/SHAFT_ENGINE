package testPackage.unitTests;

import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertiesHelper;
import com.shaft.properties.internal.PropertyFileManager;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Files;
import java.util.List;

@Test(singleThreaded = true)
public class PropertiesHelperInitializationUnitTest {
    private static final String PROPERTIES_FOLDER_PATH = "src/main/resources/properties";
    private static final List<String> GENERATED_ROOT_PROPERTIES = List.of(
            "TestNG.properties",
            "cucumber.properties",
            "custom.properties",
            "customWebdriverCapabilities.properties",
            "junit-platform.properties",
            "log4j2.properties",
            "reportportal.properties"
    );

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        GENERATED_ROOT_PROPERTIES.forEach(this::deleteGeneratedPropertyFile);
        Properties.clearForCurrentThread();
    }

    @Test
    public void initializeDefaultPropertiesShouldOnlyGenerateCustomPropertiesInTargetFolder() throws Exception {
        GENERATED_ROOT_PROPERTIES.forEach(this::deleteGeneratedPropertyFile);

        PropertiesHelper.initialize();

        Assert.assertTrue(Files.isRegularFile(Path.of(PROPERTIES_FOLDER_PATH, "custom.properties")),
                "custom.properties should be generated in the target properties folder.");
        GENERATED_ROOT_PROPERTIES.stream()
                .filter(fileName -> !"custom.properties".equals(fileName))
                .forEach(fileName -> Assert.assertFalse(Files.isRegularFile(Path.of(PROPERTIES_FOLDER_PATH, fileName)),
                        fileName + " should not be generated in the target properties folder."));
    }

    @Test
    public void defaultCustomPropertiesTemplateShouldContainCommonDefaultsAndDocumentationLink() throws IOException {
        String customTemplateContent = Files.readString(Path.of(PROPERTIES_FOLDER_PATH, "default", "custom.properties"));

        Assert.assertTrue(customTemplateContent.contains("https://shafthq.github.io/"),
                "custom.properties template should include a user guide link.");
        List.of(
                "executionAddress=local",
                "targetOperatingSystem=Linux",
                "targetBrowserName=chrome",
                "headlessExecution=false",
                "retryMaximumNumberOfAttempts=0",
                "screenshotParamsWhenToTakeAScreenshot=ValidationPointsOnly"
        ).forEach(expectedDefault -> Assert.assertTrue(customTemplateContent.contains(expectedDefault),
                "custom.properties template should include default: " + expectedDefault));
    }

    @Test
    public void resolveClasspathResourceLocationShouldResolveClasspathDirectoryUsingUri() {
        URL resourceUrl = PropertyFileManager.class.getResource("/resources/properties/default/");
        Assert.assertNotNull(resourceUrl, "Default properties resource should be on the classpath.");
        String resolvedFolder = PropertyFileManager.resolveClasspathResourceLocation(resourceUrl);
        Assert.assertFalse(resolvedFolder.contains("%"),
                "Classpath folder path should not contain URL-encoded segments on Windows.");
        Assert.assertTrue(Path.of(resolvedFolder, "custom.properties").toFile().isFile(),
                "Resolved classpath folder should contain custom.properties.");
    }

    private void deleteGeneratedPropertyFile(String fileName) {
        var propertyFile = Path.of(PROPERTIES_FOLDER_PATH, fileName);
        for (int attempt = 0; attempt < 5 && Files.isRegularFile(propertyFile); attempt++) {
            try {
                Files.deleteIfExists(propertyFile);
            } catch (IOException e) {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException interruptedException) {
                    Thread.currentThread().interrupt();
                    Assert.fail("Interrupted while deleting generated property file: " + propertyFile, interruptedException);
                }
            }
        }
        Assert.assertFalse(Files.isRegularFile(propertyFile), "Failed to delete generated property file: " + propertyFile);
    }

}
