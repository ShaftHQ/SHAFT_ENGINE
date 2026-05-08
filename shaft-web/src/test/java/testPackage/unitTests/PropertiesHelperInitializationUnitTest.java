package testPackage.unitTests;

import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertiesHelper;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
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

        Assert.assertTrue(new File(PROPERTIES_FOLDER_PATH, "custom.properties").isFile(),
                "custom.properties should be generated in the target properties folder.");
        GENERATED_ROOT_PROPERTIES.stream()
                .filter(fileName -> !"custom.properties".equals(fileName))
                .forEach(fileName -> Assert.assertFalse(new File(PROPERTIES_FOLDER_PATH, fileName).isFile(),
                        fileName + " should not be generated in the target properties folder."));
    }

    @Test
    public void defaultCustomPropertiesTemplateShouldContainCommonDefaultsAndDocumentationLink() throws IOException {
        String customTemplateContent = Files.readString(Path.of(PROPERTIES_FOLDER_PATH, "default", "custom.properties"));

        Assert.assertTrue(customTemplateContent.contains("https://shafthq.github.io/"),
                "custom.properties template should include a user guide link.");
        Assert.assertTrue(customTemplateContent.contains("executionAddress=local"));
        Assert.assertTrue(customTemplateContent.contains("targetOperatingSystem=Linux"));
        Assert.assertTrue(customTemplateContent.contains("targetBrowserName=chrome"));
        Assert.assertTrue(customTemplateContent.contains("headlessExecution=false"));
        Assert.assertTrue(customTemplateContent.contains("setParallel=NONE"));
        Assert.assertTrue(customTemplateContent.contains("setThreadCount=1"));
        Assert.assertTrue(customTemplateContent.contains("retryMaximumNumberOfAttempts=0"));
        Assert.assertTrue(customTemplateContent.contains("maximumPerformanceMode=0"));
        Assert.assertTrue(customTemplateContent.contains("screenshotParamsWhenToTakeAScreenshot=ValidationPointsOnly"));
    }

    private void deleteGeneratedPropertyFile(String fileName) {
        var propertyFile = new File(PROPERTIES_FOLDER_PATH, fileName);
        if (propertyFile.isFile()) {
            Assert.assertTrue(propertyFile.delete(), "Failed to delete generated property file: " + propertyFile.getPath());
        }
    }

}
