package testPackage.properties;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.BrowserStackSdkHelper;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import org.yaml.snakeyaml.Yaml;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.Map;

/**
 * Tests for the BrowserStack SDK YAML generation utility.
 * Validates that SHAFT's BrowserStack properties are correctly mapped
 * to the {@code browserstack.yml} format expected by the BrowserStack SDK.
 */
public class BrowserStackSdkTests {

    private String generatedYamlPath;

    @BeforeMethod
    public void beforeMethod() {
        generatedYamlPath = null;
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (generatedYamlPath != null) {
            new File(generatedYamlPath).delete();
        }
        // Also clean up any browserstack.yml in project root
        var defaultPath = new File(System.getProperty("user.dir"), "browserstack.yml");
        if (defaultPath.exists()) {
            defaultPath.delete();
        }
    }

    @Test
    public void generateBrowserStackYmlContainsAuthenticationCredentials() {
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        SHAFT.Validations.assertThat().object(config.get("userName"))
                .isEqualTo(SHAFT.Properties.browserStack.userName()).perform();
        SHAFT.Validations.assertThat().object(config.get("accessKey"))
                .isEqualTo(SHAFT.Properties.browserStack.accessKey()).perform();
    }

    @Test
    public void generateBrowserStackYmlCreatesFileInProjectRoot() {
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var expectedPath = System.getProperty("user.dir") + File.separator + "browserstack.yml";

        SHAFT.Validations.assertThat().object(generatedYamlPath)
                .isEqualTo(expectedPath).perform();
        SHAFT.Validations.assertThat().object(new File(generatedYamlPath).exists())
                .isEqualTo(true).perform();
    }

    @Test
    public void generateBrowserStackYmlContainsBuildAndProjectNames() {
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        SHAFT.Validations.assertThat().object(config.get("buildName")).isNotNull().perform();
        SHAFT.Validations.assertThat().object(config.get("projectName")).isNotNull().perform();
        SHAFT.Validations.assertThat().object(config.get("buildIdentifier")).isNotNull().perform();
    }

    @Test
    public void generateBrowserStackYmlContainsDefaultSettings() {
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        SHAFT.Validations.assertThat().object(config.get("browserstackLocal"))
                .isEqualTo(SHAFT.Properties.browserStack.local()).perform();
        SHAFT.Validations.assertThat().object(config.get("debug"))
                .isEqualTo(SHAFT.Properties.browserStack.debug()).perform();
        SHAFT.Validations.assertThat().object(config.get("networkLogs"))
                .isEqualTo(SHAFT.Properties.browserStack.networkLogs()).perform();
        SHAFT.Validations.assertThat().object(config.get("acceptInsecureCerts"))
                .isEqualTo(SHAFT.Properties.browserStack.acceptInsecureCerts()).perform();
        SHAFT.Validations.assertThat().object(config.get("browserstackAutomation"))
                .isEqualTo(SHAFT.Properties.browserStack.browserstackAutomation()).perform();
    }

    @Test
    public void generateBrowserStackYmlWithCustomBuildName() {
        SHAFT.Properties.browserStack.set().buildName("custom-build-name");
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        SHAFT.Validations.assertThat().object(config.get("buildName"))
                .isEqualTo("custom-build-name").perform();
        // Reset
        SHAFT.Properties.browserStack.set().buildName("");
    }

    @Test
    public void generateBrowserStackYmlWithCustomProjectName() {
        SHAFT.Properties.browserStack.set().projectName("custom-project-name");
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        SHAFT.Validations.assertThat().object(config.get("projectName"))
                .isEqualTo("custom-project-name").perform();
        // Reset
        SHAFT.Properties.browserStack.set().projectName("");
    }

    @Test
    public void generateBrowserStackYmlContainsSeleniumVersion() {
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        SHAFT.Validations.assertThat().object(config.get("seleniumVersion"))
                .isEqualTo(SHAFT.Properties.browserStack.seleniumVersion()).perform();
    }

    @Test
    public void generateBrowserStackYmlContainsParallelsPerPlatform() {
        SHAFT.Properties.browserStack.set().parallelsPerPlatform(5);
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        SHAFT.Validations.assertThat().object(config.get("parallelsPerPlatform"))
                .isEqualTo(5).perform();
        // Reset
        SHAFT.Properties.browserStack.set().parallelsPerPlatform(1);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void generateBrowserStackYmlWithDesktopWebPlatform() {
        // Desktop web is the default when no mobile/app properties are set
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        SHAFT.Validations.assertThat().object(config.containsKey("platforms"))
                .isEqualTo(true).perform();
        var platforms = (List<Map<String, Object>>) config.get("platforms");
        SHAFT.Validations.assertThat().object(platforms).isNotNull().perform();
        SHAFT.Validations.assertThat().object(platforms.isEmpty())
                .isEqualTo(false).perform();
    }

    @Test
    public void generateBrowserStackYmlContainsRequiredFields() {
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        SHAFT.Validations.assertThat().object(config).isNotNull().perform();
        SHAFT.Validations.assertThat().object(config.containsKey("userName"))
                .isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(config.containsKey("accessKey"))
                .isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(config.containsKey("buildName"))
                .isEqualTo(true).perform();
        SHAFT.Validations.assertThat().object(config.containsKey("browserstackAutomation"))
                .isEqualTo(true).perform();
    }

    @Test
    public void generateBrowserStackYmlWithAppUrl() {
        SHAFT.Properties.browserStack.set().appUrl("bs://test-app-url");
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        SHAFT.Validations.assertThat().object(config.get("app"))
                .isEqualTo("bs://test-app-url").perform();
        // Reset
        SHAFT.Properties.browserStack.set().appUrl("");
    }

    @Test
    public void newBrowserStackPropertiesAreReadableAndSettable() {
        // Test buildName
        SHAFT.Properties.browserStack.set().buildName("test-build");
        SHAFT.Validations.assertThat().object(SHAFT.Properties.browserStack.buildName())
                .isEqualTo("test-build").perform();
        SHAFT.Properties.browserStack.set().buildName("");

        // Test projectName
        SHAFT.Properties.browserStack.set().projectName("test-project");
        SHAFT.Validations.assertThat().object(SHAFT.Properties.browserStack.projectName())
                .isEqualTo("test-project").perform();
        SHAFT.Properties.browserStack.set().projectName("");

        // Test parallelsPerPlatform
        SHAFT.Properties.browserStack.set().parallelsPerPlatform(3);
        SHAFT.Validations.assertThat().number(SHAFT.Properties.browserStack.parallelsPerPlatform())
                .isEqualTo(3).perform();
        SHAFT.Properties.browserStack.set().parallelsPerPlatform(1);

        // Test browserstackAutomation
        SHAFT.Properties.browserStack.set().browserstackAutomation(false);
        SHAFT.Validations.assertThat().object(SHAFT.Properties.browserStack.browserstackAutomation())
                .isEqualTo(false).perform();
        SHAFT.Properties.browserStack.set().browserstackAutomation(true);
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> loadYaml(String path) {
        try (var reader = new FileReader(path)) {
            return new Yaml().load(reader);
        } catch (IOException e) {
            throw new IllegalStateException("Failed to read generated browserstack.yml: " + e.getMessage(), e);
        }
    }
}
