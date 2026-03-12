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

import static org.testng.Assert.*;

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

        assertEquals(config.get("userName"), SHAFT.Properties.browserStack.userName());
        assertEquals(config.get("accessKey"), SHAFT.Properties.browserStack.accessKey());
    }

    @Test
    public void generateBrowserStackYmlCreatesFileInProjectRoot() {
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var expectedPath = System.getProperty("user.dir") + File.separator + "browserstack.yml";

        assertEquals(generatedYamlPath, expectedPath);
        assertTrue(new File(generatedYamlPath).exists());
    }

    @Test
    public void generateBrowserStackYmlContainsBuildAndProjectNames() {
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        assertNotNull(config.get("buildName"));
        assertNotNull(config.get("projectName"));
        assertNotNull(config.get("buildIdentifier"));
    }

    @Test
    public void generateBrowserStackYmlContainsDefaultSettings() {
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        assertEquals(config.get("browserstackLocal"), SHAFT.Properties.browserStack.local());
        assertEquals(config.get("debug"), SHAFT.Properties.browserStack.debug());
        assertEquals(config.get("networkLogs"), SHAFT.Properties.browserStack.networkLogs());
        assertEquals(config.get("acceptInsecureCerts"), SHAFT.Properties.browserStack.acceptInsecureCerts());
        assertEquals(config.get("browserstackAutomation"), SHAFT.Properties.browserStack.browserstackAutomation());
    }

    @Test
    public void generateBrowserStackYmlWithCustomBuildName() {
        SHAFT.Properties.browserStack.set().buildName("custom-build-name");
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        assertEquals(config.get("buildName"), "custom-build-name");
        // Reset
        SHAFT.Properties.browserStack.set().buildName("");
    }

    @Test
    public void generateBrowserStackYmlWithCustomProjectName() {
        SHAFT.Properties.browserStack.set().projectName("custom-project-name");
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        assertEquals(config.get("projectName"), "custom-project-name");
        // Reset
        SHAFT.Properties.browserStack.set().projectName("");
    }

    @Test
    public void generateBrowserStackYmlContainsSeleniumVersion() {
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        assertEquals(config.get("seleniumVersion"), SHAFT.Properties.browserStack.seleniumVersion());
    }

    @Test
    public void generateBrowserStackYmlContainsParallelsPerPlatform() {
        SHAFT.Properties.browserStack.set().parallelsPerPlatform(5);
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        assertEquals(config.get("parallelsPerPlatform"), 5);
        // Reset
        SHAFT.Properties.browserStack.set().parallelsPerPlatform(1);
    }

    @Test
    @SuppressWarnings("unchecked")
    public void generateBrowserStackYmlWithDesktopWebPlatform() {
        // Desktop web is the default when no mobile/app properties are set
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        assertTrue(config.containsKey("platforms"));
        var platforms = (List<Map<String, Object>>) config.get("platforms");
        assertNotNull(platforms);
        assertFalse(platforms.isEmpty());
    }

    @Test
    public void generateBrowserStackYmlContainsRequiredFields() {
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        assertNotNull(config);
        assertTrue(config.containsKey("userName"));
        assertTrue(config.containsKey("accessKey"));
        assertTrue(config.containsKey("buildName"));
        assertTrue(config.containsKey("browserstackAutomation"));
    }

    @Test
    public void generateBrowserStackYmlWithAppUrl() {
        SHAFT.Properties.browserStack.set().appUrl("bs://test-app-url");
        generatedYamlPath = BrowserStackSdkHelper.generateBrowserStackYml();
        var config = loadYaml(generatedYamlPath);

        assertEquals(config.get("app"), "bs://test-app-url");
        // Reset
        SHAFT.Properties.browserStack.set().appUrl("");
    }

    @Test
    public void newBrowserStackPropertiesAreReadableAndSettable() {
        // Test buildName
        SHAFT.Properties.browserStack.set().buildName("test-build");
        assertEquals(SHAFT.Properties.browserStack.buildName(), "test-build");
        SHAFT.Properties.browserStack.set().buildName("");

        // Test projectName
        SHAFT.Properties.browserStack.set().projectName("test-project");
        assertEquals(SHAFT.Properties.browserStack.projectName(), "test-project");
        SHAFT.Properties.browserStack.set().projectName("");

        // Test parallelsPerPlatform
        SHAFT.Properties.browserStack.set().parallelsPerPlatform(3);
        assertEquals(SHAFT.Properties.browserStack.parallelsPerPlatform(), 3);
        SHAFT.Properties.browserStack.set().parallelsPerPlatform(1);

        // Test browserstackAutomation
        SHAFT.Properties.browserStack.set().browserstackAutomation(false);
        assertFalse(SHAFT.Properties.browserStack.browserstackAutomation());
        SHAFT.Properties.browserStack.set().browserstackAutomation(true);
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> loadYaml(String path) {
        try (var reader = new FileReader(path)) {
            return new Yaml().load(reader);
        } catch (IOException e) {
            fail("Failed to read generated browserstack.yml: " + e.getMessage());
            return null;
        }
    }
}
