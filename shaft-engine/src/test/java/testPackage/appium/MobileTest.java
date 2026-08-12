package testPackage.appium;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;

import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;

public abstract class MobileTest {
    private static final String COMPATIBLE_PROVIDER = "shaft.mobile.compatibleProvider";
    private static final String EXECUTION_ADDRESS = "executionAddress";
    private static final String MOBILE_APP = "mobile_app";
    public static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod(onlyForGroups = {"NativeAndroidDemo"})
    public void setupNativeAndroidDemoApk() {
        Properties.clearForCurrentThread();
        System.setProperty("mobile_autoGrantPermissions", "true");

        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(false);
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2).app("");

        // self-managed execution
//        SHAFT.Properties.mobile.set().selfManaged(true);
//        SHAFT.Properties.mobile.set().selfManagedAndroidSDKVersion(31);

        // local appium server (for local and GitHub actions execution)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/Android-NativeDemoApp-0.4.0.apk");

        // local appium server (android-emulator docker-compose)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4725");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/Android-NativeDemoApp-0.4.0.apk");

        // remote browserstack server (new app version)
        SHAFT.Properties.platform.set().executionAddress("browserstack");
        SHAFT.Properties.browserStack.set().platformVersion("13.0");
        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
        SHAFT.Properties.browserStack.set().appName("Android-NativeDemoApp.apk");
        SHAFT.Properties.browserStack.set().appRelativeFilePath("src/test/resources/testDataFiles/apps/Android-NativeDemoApp-0.4.0.apk");
        SHAFT.Properties.browserStack.set().appUrl("");

        // remote browserstack server (existing app version)
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().platformVersion("13.0");
//        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
//        SHAFT.Properties.browserStack.set().appName("Android-NativeDemoApp.apk");
//        SHAFT.Properties.browserStack.set().appRelativeFilePath("");
//        SHAFT.Properties.browserStack.set().appUrl("bs://61abe95b5ed5bb6dc169f8df6b7141db120167d3");
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @BeforeMethod(onlyForGroups = {"ApiDemosDebug"})
    public void setupApiDemosDebug() {
        Properties.clearForCurrentThread();
        System.setProperty("mobile_autoGrantPermissions", "true");

        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(false);
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2).app("");

        // self-managed execution
//        SHAFT.Properties.mobile.set().selfManaged(true);
//        SHAFT.Properties.mobile.set().selfManagedAndroidSDKVersion(31);

        // local appium server (for local and GitHub actions execution)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");

        // local appium server (android-emulator docker-compose)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4725");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");

        if (!configureCompatibleProvider()) {
            // remote browserstack server (new app version)
            SHAFT.Properties.platform.set().executionAddress("browserstack");
            SHAFT.Properties.browserStack.set().platformVersion("13.0");
            SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
            SHAFT.Properties.browserStack.set().appName("ApiDemos-debug.apk");
            SHAFT.Properties.browserStack.set().appRelativeFilePath("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");
            SHAFT.Properties.browserStack.set().appUrl("");
        }

        // remote browserstack server (existing app version)
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().platformVersion("13.0");
//        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
//        SHAFT.Properties.browserStack.set().appName("ApiDemos-debug.apk");
//        SHAFT.Properties.browserStack.set().appRelativeFilePath("");
//        SHAFT.Properties.browserStack.set().appUrl("bs://61abe95b5ed5bb6dc169f8df6b7141db120167d3");
        createDriver();
    }

    void createDriver() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    private boolean configureCompatibleProvider() {
        if (!Boolean.parseBoolean(System.getProperty(COMPATIBLE_PROVIDER, "false"))) {
            return false;
        }
        String executionAddress = System.getProperty(EXECUTION_ADDRESS, "").trim();
        if (executionAddress.isEmpty()) {
            throw new IllegalArgumentException("Compatible mobile provider requires executionAddress.");
        }
        String configuredApp = System.getProperty(MOBILE_APP, "").trim();
        Path app;
        try {
            app = Path.of(configuredApp).toAbsolutePath().normalize();
        } catch (InvalidPathException exception) {
            throw new IllegalArgumentException("Compatible mobile provider requires an existing mobile_app APK.");
        }
        if (!Files.isRegularFile(app)) {
            throw new IllegalArgumentException("Compatible mobile provider requires an existing mobile_app APK.");
        }
        SHAFT.Properties.platform.set().executionAddress(executionAddress);
        SHAFT.Properties.mobile.set().app(app.toString());
        return true;
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        try {
            SHAFT.GUI.WebDriver shaftDriver = driver.get();
            if (shaftDriver != null) {
                shaftDriver.quit();
            }
        } finally {
            driver.remove();
            System.clearProperty("mobile_autoGrantPermissions");
            Properties.clearForCurrentThread();
        }
    }
}
