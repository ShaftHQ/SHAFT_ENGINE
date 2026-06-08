package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.BrowserStackSdkHelper;
import com.shaft.properties.internal.Properties;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;
import org.yaml.snakeyaml.Yaml;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

public class BrowserStackSdkHelperCoverageUnitTest {
    private static final Path PROJECT_ROOT = Path.of(System.getProperty("user.dir"));
    private static final Path GENERATED_YML = PROJECT_ROOT.resolve("browserstack.yml");
    private static final Path TEMP_DIR = PROJECT_ROOT.resolve("target").resolve("temp").resolve("browserstack-sdk-helper");

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws Exception {
        Properties.clearForCurrentThread();
        Files.deleteIfExists(GENERATED_YML);
        if (Files.exists(TEMP_DIR)) {
            Files.walk(TEMP_DIR)
                    .sorted((a, b) -> b.compareTo(a))
                    .forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (Exception ignored) {
                            // do nothing
                        }
                    });
        }
    }

    @Test
    public void constructorShouldThrowIllegalStateExceptionForUtilityClass() throws Exception {
        Constructor<BrowserStackSdkHelper> constructor = BrowserStackSdkHelper.class.getDeclaredConstructor();
        constructor.setAccessible(true);

        Throwable thrown = captureThrowable(() -> constructor.newInstance());

        assertTrue(thrown instanceof InvocationTargetException);
        assertTrue(thrown.getCause() instanceof IllegalStateException);
        assertEquals(thrown.getCause().getMessage(), "Utility class");
    }

    @Test
    public void generateBrowserStackYmlShouldCopyCustomRelativeFile() throws Exception {
        Files.createDirectories(TEMP_DIR);
        Path customYml = TEMP_DIR.resolve("custom-browserstack.yml");
        Files.writeString(customYml, "userName: copied-user\naccessKey: copied-key\n", StandardCharsets.UTF_8);

        SHAFT.Properties.browserStack.set().customBrowserStackYmlPath("target" + File.separator + "temp" + File.separator
                + "browserstack-sdk-helper" + File.separator + "custom-browserstack.yml");

        String ymlPath = BrowserStackSdkHelper.generateBrowserStackYml();

        assertEquals(ymlPath, GENERATED_YML.toString());
        assertTrue(Files.exists(GENERATED_YML));
        assertEquals(Files.readString(GENERATED_YML), Files.readString(customYml));
    }

    @Test
    public void generateBrowserStackYmlShouldUseRootCustomFileWhenAlreadyInPlace() throws Exception {
        Files.writeString(GENERATED_YML, "projectName: existing-root-file\n", StandardCharsets.UTF_8);
        SHAFT.Properties.browserStack.set().customBrowserStackYmlPath("browserstack.yml");

        String ymlPath = BrowserStackSdkHelper.generateBrowserStackYml();

        assertEquals(ymlPath, GENERATED_YML.toString());
        assertEquals(Files.readString(GENERATED_YML), "projectName: existing-root-file\n");
    }

    @Test
    public void generateBrowserStackYmlShouldThrowForMissingCustomFile() {
        SHAFT.Properties.browserStack.set().customBrowserStackYmlPath("target/temp/browserstack-sdk-helper/missing.yml");

        Throwable thrown = captureThrowable(BrowserStackSdkHelper::generateBrowserStackYml);

        assertTrue(thrown instanceof IllegalStateException);
        assertTrue(thrown.getMessage().contains("Custom browserstack.yml not found"));
    }

    @Test
    public void generateBrowserStackYmlShouldSupportPlatformsListAndAppUrl() throws Exception {
        SHAFT.Properties.browserStack.set()
                .customBrowserStackYmlPath("")
                .userName("user-list")
                .accessKey("key-list")
                .platformsList("[{\"deviceName\":\"Samsung Galaxy S23\",\"osVersion\":\"13.0\",\"platformName\":\"android\"}]")
                .buildName("custom-build")
                .projectName("custom-project")
                .browserstackAutomation(false)
                .parallelsPerPlatform(2)
                .local(true)
                .debug(true)
                .networkLogs(true)
                .acceptInsecureCerts(false)
                .seleniumVersion("4.26.0")
                .appiumVersion("2.17.1")
                .geoLocation("US")
                .appUrl("bs://existing-app")
                .appRelativeFilePath("")
                .customID("");

        Map<String, Object> config = loadGeneratedYml(BrowserStackSdkHelper.generateBrowserStackYml());
        List<Map<String, Object>> platforms = (List<Map<String, Object>>) config.get("platforms");

        assertEquals(config.get("userName"), "user-list");
        assertEquals(config.get("accessKey"), "key-list");
        assertEquals(config.get("buildName"), "custom-build");
        assertEquals(config.get("projectName"), "custom-project");
        assertEquals(config.get("browserstackAutomation"), false);
        assertEquals(config.get("parallelsPerPlatform"), 2);
        assertEquals(config.get("browserstackLocal"), true);
        assertEquals(config.get("debug"), true);
        assertEquals(config.get("networkLogs"), true);
        assertEquals(config.get("acceptInsecureCerts"), false);
        assertEquals(config.get("seleniumVersion"), "4.26.0");
        assertEquals(config.get("appiumVersion"), "2.17.1");
        assertEquals(config.get("geoLocation"), "US");
        assertEquals(config.get("app"), "bs://existing-app");
        assertEquals(config.get("buildIdentifier"), "${BUILD_NUMBER}");
        assertEquals(platforms.getFirst().get("deviceName"), "Samsung Galaxy S23");
        assertEquals(platforms.getFirst().get("osVersion"), "13.0");
        assertEquals(platforms.getFirst().get("platformName"), "android");
    }

    @Test
    public void generateBrowserStackYmlShouldFallbackToDesktopForInvalidPlatformsList() throws Exception {
        SHAFT.Properties.platform.set().targetPlatform("windows");
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.web.set().targetBrowserName("safari");
        SHAFT.Properties.browserStack.set()
                .customBrowserStackYmlPath("")
                .userName("desktop-user")
                .accessKey("desktop-key")
                .platformsList("[invalid-json")
                .buildName("")
                .projectName("")
                .osVersion("Sonoma")
                .browserVersion("17.0")
                .appUrl("")
                .appRelativeFilePath("");

        Map<String, Object> config = loadGeneratedYml(BrowserStackSdkHelper.generateBrowserStackYml());
        List<Map<String, Object>> platforms = (List<Map<String, Object>>) config.get("platforms");
        Map<String, Object> platform = platforms.getFirst();

        assertEquals(platform.get("os"), "Windows");
        assertEquals(platform.get("osVersion"), "Sonoma");
        assertEquals(platform.get("browserName"), "safari");
        assertEquals(platform.get("browserVersion"), "17.0");
        assertTrue(((String) config.get("buildName")).matches(".+_\\d{8}"));
        assertTrue(config.get("projectName") != null);
    }

    @Test
    public void generateBrowserStackYmlShouldSupportMobileWebConfiguration() throws Exception {
        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.mobile.set().browserName("chrome");
        SHAFT.Properties.web.set().targetBrowserName("chrome");
        SHAFT.Properties.browserStack.set()
                .customBrowserStackYmlPath("")
                .userName("mobile-user")
                .accessKey("mobile-key")
                .platformsList("")
                .deviceName("Google Pixel 7")
                .osVersion("13.0")
                .appUrl("")
                .appRelativeFilePath("");

        Map<String, Object> config = loadGeneratedYml(BrowserStackSdkHelper.generateBrowserStackYml());
        List<Map<String, Object>> platforms = (List<Map<String, Object>>) config.get("platforms");
        Map<String, Object> platform = platforms.getFirst();

        assertEquals(platform.get("deviceName"), "Google Pixel 7");
        assertEquals(platform.get("osVersion"), "13.0");
        assertEquals(platform.get("browserName"), "chrome");
    }

    @Test
    public void generateBrowserStackYmlShouldSupportNativeAppRelativePathAndCustomId() throws Exception {
        SHAFT.Properties.platform.set().targetPlatform("android");
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.browserStack.set()
                .customBrowserStackYmlPath("")
                .userName("native-user")
                .accessKey("native-key")
                .platformsList("")
                .deviceName("Google Pixel 8")
                .platformVersion("14.0")
                .appUrl("")
                .appRelativeFilePath("apps/demo.apk")
                .customID("my-custom-id");

        Map<String, Object> config = loadGeneratedYml(BrowserStackSdkHelper.generateBrowserStackYml());
        List<Map<String, Object>> platforms = (List<Map<String, Object>>) config.get("platforms");
        Map<String, Object> platform = platforms.getFirst();
        Map<String, Object> appStoreConfiguration = (Map<String, Object>) config.get("appStoreConfiguration");

        assertEquals(platform.get("deviceName"), "Google Pixel 8");
        assertEquals(platform.get("osVersion"), "14.0");
        assertEquals(config.get("app"), "apps/demo.apk");
        assertEquals(appStoreConfiguration.get("customId"), "my-custom-id");
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> loadGeneratedYml(String ymlPath) throws Exception {
        return new Yaml().load(Files.newInputStream(Path.of(ymlPath)));
    }

    private Throwable captureThrowable(ThrowingRunnable action) {
        try {
            action.run();
            return null;
        } catch (Throwable throwable) {
            return throwable;
        }
    }

    private void assertEquals(Object actual, Object expected) {
        SHAFT.Validations.assertThat().object(actual).isEqualTo(expected).perform();
    }

    private void assertTrue(boolean actual) {
        SHAFT.Validations.assertThat().object(actual).isTrue().perform();
    }

    @FunctionalInterface
    private interface ThrowingRunnable {
        void run() throws Exception;
    }
}
