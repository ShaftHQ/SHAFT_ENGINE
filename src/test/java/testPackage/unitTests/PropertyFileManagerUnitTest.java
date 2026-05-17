package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.properties.internal.PropertyFileManager;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

public class PropertyFileManagerUnitTest {
    private String savedMobileApp;
    private String savedMobileAppPackage;
    private String savedMobileAppActivity;
    private String savedSkipApkInference;

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        if (savedMobileApp != null) {
            SHAFT.Properties.mobile.set().app(savedMobileApp);
        }
        if (savedMobileAppPackage != null) {
            SHAFT.Properties.mobile.set().appPackage(savedMobileAppPackage);
        }
        if (savedMobileAppActivity != null) {
            SHAFT.Properties.mobile.set().appActivity(savedMobileAppActivity);
        }
        if (savedSkipApkInference != null) {
            System.setProperty("shaft.skipApkPackageActivityInference", savedSkipApkInference);
        } else {
            System.clearProperty("shaft.skipApkPackageActivityInference");
        }
        Properties.clearForCurrentThread();
    }

    @Test(description = "getAppiumDesiredCapabilities should preserve BrowserStack bs:// app URL without path resolution")
    public void getAppiumDesiredCapabilities_preservesBrowserStackAppUrl() {
        savedMobileApp = SHAFT.Properties.mobile.app();
        String bsUrl = "bs://ce7b85a493195b37af78ec03750a7ecda18284cf";
        SHAFT.Properties.mobile.set().app(bsUrl);

        Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();

        SHAFT.Validations.assertThat().object(caps.get("mobile_app")).isEqualTo(bsUrl).perform();
    }

    @Test(description = "getAppiumDesiredCapabilities should preserve LambdaTest lt:// app URL without path resolution")
    public void getAppiumDesiredCapabilities_preservesLambdaTestAppUrl() {
        savedMobileApp = SHAFT.Properties.mobile.app();
        String ltUrl = "lt://APP100201061735398359698453";
        SHAFT.Properties.mobile.set().app(ltUrl);

        Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();

        SHAFT.Validations.assertThat().object(caps.get("mobile_app")).isEqualTo(ltUrl).perform();
    }

    @Test(description = "getAppiumDesiredCapabilities should preserve https:// app URL without path resolution")
    public void getAppiumDesiredCapabilities_preservesHttpsAppUrl() {
        savedMobileApp = SHAFT.Properties.mobile.app();
        String httpsUrl = "https://example.com/app.apk";
        SHAFT.Properties.mobile.set().app(httpsUrl);

        Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();

        SHAFT.Validations.assertThat().object(caps.get("mobile_app")).isEqualTo(httpsUrl).perform();
    }

    @Test(description = "getAppiumDesiredCapabilities should resolve src/ relative app paths to absolute paths")
    public void getAppiumDesiredCapabilities_resolvesSrcRelativePath() {
        savedMobileApp = SHAFT.Properties.mobile.app();
        String relativePath = "src/test/resources/testDataFiles/simpleJSON.json";
        SHAFT.Properties.mobile.set().app(relativePath);

        Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();

        String expectedPath = new File(relativePath).getAbsolutePath();
        SHAFT.Validations.assertThat().object(caps.get("mobile_app")).isEqualTo(expectedPath).perform();
    }

    @Test(description = "getAppiumDesiredCapabilities should resolve non-absolute app paths under user.dir")
    public void getAppiumDesiredCapabilities_resolvesUserDirRelativePath() {
        savedMobileApp = SHAFT.Properties.mobile.app();
        String relativePath = "pom.xml";
        SHAFT.Properties.mobile.set().app(relativePath);

        Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();

        String expectedPath = new File(System.getProperty("user.dir"), relativePath).getAbsolutePath();
        SHAFT.Validations.assertThat().object(caps.get("mobile_app")).isEqualTo(expectedPath).perform();
    }

    @Test(description = "getAppiumDesiredCapabilities should skip APK package/activity inference when opt-out flag is true")
    public void getAppiumDesiredCapabilities_skipsApkInferenceWhenFlagIsEnabled() throws Exception {
        savedMobileApp = SHAFT.Properties.mobile.app();
        savedMobileAppPackage = SHAFT.Properties.mobile.appPackage();
        savedMobileAppActivity = SHAFT.Properties.mobile.appActivity();
        savedSkipApkInference = System.getProperty("shaft.skipApkPackageActivityInference");

        File tempApk = Files.createTempFile("shaft-skip-inference", ".apk").toFile();
        try {
            SHAFT.Properties.mobile.set().app(tempApk.getAbsolutePath());
            SHAFT.Properties.mobile.set().appPackage("");
            SHAFT.Properties.mobile.set().appActivity("");
            System.setProperty("shaft.skipApkPackageActivityInference", "true");

            Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();

            SHAFT.Validations.assertThat().object(caps.get("mobile_appPackage")).isEqualTo("").perform();
            SHAFT.Validations.assertThat().object(caps.get("mobile_appActivity")).isEqualTo("").perform();
        } finally {
            Files.deleteIfExists(tempApk.toPath());
        }
    }

    @Test(description = "getAppiumDesiredCapabilities should keep provided appPackage/appActivity values for APK app")
    public void getAppiumDesiredCapabilities_keepsProvidedPackageAndActivity() throws Exception {
        savedMobileApp = SHAFT.Properties.mobile.app();
        savedMobileAppPackage = SHAFT.Properties.mobile.appPackage();
        savedMobileAppActivity = SHAFT.Properties.mobile.appActivity();
        savedSkipApkInference = System.getProperty("shaft.skipApkPackageActivityInference");

        File tempApk = Files.createTempFile("shaft-existing-identifiers", ".apk").toFile();
        try {
            SHAFT.Properties.mobile.set().app(tempApk.getAbsolutePath());
            SHAFT.Properties.mobile.set().appPackage("com.example.app");
            SHAFT.Properties.mobile.set().appActivity("com.example.app.MainActivity");
            System.setProperty("shaft.skipApkPackageActivityInference", "false");

            Map<String, String> caps = PropertyFileManager.getAppiumDesiredCapabilities();

            SHAFT.Validations.assertThat().object(caps.get("mobile_appPackage")).isEqualTo("com.example.app").perform();
            SHAFT.Validations.assertThat().object(caps.get("mobile_appActivity")).isEqualTo("com.example.app.MainActivity").perform();
        } finally {
            Files.deleteIfExists(tempApk.toPath());
        }
    }

    @Test(description = "private constructor should throw IllegalStateException")
    public void constructor_throwsIllegalStateException() throws Exception {
        Constructor<PropertyFileManager> constructor = PropertyFileManager.class.getDeclaredConstructor();
        constructor.setAccessible(true);

        InvocationTargetException thrown = Assert.expectThrows(InvocationTargetException.class, constructor::newInstance);
        SHAFT.Validations.assertThat().object(thrown.getCause() instanceof IllegalStateException).isEqualTo(true).perform();
    }

    @Test(description = "readPropertyFiles should handle missing directory and invalid jar URI gracefully")
    public void readPropertyFiles_handlesMissingAndInvalidJarPaths() throws Exception {
        Method readPropertyFiles = PropertyFileManager.class.getDeclaredMethod("readPropertyFiles", String.class);
        readPropertyFiles.setAccessible(true);
        String markerKey = "shaft.property.manager.read.marker";
        String markerValue = "stable-value";
        System.setProperty(markerKey, markerValue);

        try {
            int beforePropertiesCount = System.getProperties().size();
            readPropertyFiles.invoke(null, "/path/that/does/not/exist");
            readPropertyFiles.invoke(null, "invalid-archive.jar!broken");
            SHAFT.Validations.assertThat().object(System.getProperty(markerKey)).isEqualTo(markerValue).perform();
            SHAFT.Validations.assertThat().object(System.getProperties().size() >= beforePropertiesCount).isEqualTo(true).perform();
        } catch (InvocationTargetException e) {
            Assert.fail("readPropertyFiles should handle invalid paths internally", e.getCause());
        } finally {
            System.clearProperty(markerKey);
        }
    }

    @Test(description = "loadPropertiesFromFile should gracefully handle non-readable file input")
    public void loadPropertiesFromFile_handlesIOException() throws Exception {
        Method loadMethod = PropertyFileManager.class.getDeclaredMethod(
                "loadPropertiesFromFile",
                java.util.Properties.class,
                File.class
        );
        loadMethod.setAccessible(true);
        File directory = Files.createTempDirectory("shaft-property-file-dir").toFile();
        String key = "shaft.property.manager.ioexception";
        String previousValue = "before-load";
        System.setProperty(key, previousValue);
        java.util.Properties properties = new java.util.Properties();
        properties.setProperty(key, "from-file-load");

        try {
            boolean loaded = (boolean) loadMethod.invoke(null, properties, directory);
            SHAFT.Validations.assertThat().object(loaded).isEqualTo(false).perform();
            SHAFT.Validations.assertThat().object(properties.getProperty(key)).isEqualTo("from-file-load").perform();
        } catch (InvocationTargetException e) {
            Assert.fail("loadPropertiesFromFile should handle IO errors internally", e.getCause());
        } finally {
            Files.deleteIfExists(directory.toPath());
        }
        SHAFT.Validations.assertThat().object(System.getProperty(key)).isEqualTo(previousValue).perform();
        System.clearProperty(key);
    }

    @Test(description = "readPropertyFiles should load .properties files in deterministic sorted order")
    public void readPropertyFiles_loadsPropertiesInDeterministicOrder() throws Exception {
        Method readPropertyFiles = PropertyFileManager.class.getDeclaredMethod("readPropertyFiles", String.class);
        readPropertyFiles.setAccessible(true);
        Path propertiesDirectory = Files.createTempDirectory("shaft-property-order");
        String orderKey = "shaft.property.manager.load.order";
        String previousValue = System.getProperty(orderKey);

        try {
            Files.writeString(propertiesDirectory.resolve("z-order.properties"), orderKey + "=from-z\n");
            Files.writeString(propertiesDirectory.resolve("a-order.properties"), orderKey + "=from-a\n");
            Files.writeString(propertiesDirectory.resolve("b-order.properties"), orderKey + "=from-b\n");

            readPropertyFiles.invoke(null, propertiesDirectory.toString());

            SHAFT.Validations.assertThat().object(System.getProperty(orderKey)).isEqualTo("from-z").perform();
        } catch (InvocationTargetException e) {
            Assert.fail("readPropertyFiles should load sorted property files", e.getCause());
        } finally {
            if (previousValue != null) {
                System.setProperty(orderKey, previousValue);
            } else {
                System.clearProperty(orderKey);
            }
            Files.walk(propertiesDirectory)
                    .sorted(java.util.Comparator.reverseOrder())
                    .forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (Exception ignored) {
                            // best-effort temp cleanup
                        }
                    });
        }
    }
}
