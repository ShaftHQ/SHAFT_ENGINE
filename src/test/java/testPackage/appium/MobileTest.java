package testPackage.appium;

import com.shaft.driver.SHAFT;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;

import java.util.concurrent.atomic.AtomicReference;

public abstract class MobileTest {
    public static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    // Cache uploaded BrowserStack app URLs to avoid re-uploading the APK on every test method.
    // These tests are expected to run sequentially (not in parallel) since only one native app
    // session can run on a BrowserStack device at a time. AtomicReference is used for safe
    // publication of the cached URL across test methods.
    private static final AtomicReference<String> cachedNativeDemoAppUrl = new AtomicReference<>("");
    private static final AtomicReference<String> cachedApiDemosAppUrl = new AtomicReference<>("");

    @BeforeMethod(onlyForGroups = {"NativeAndroidDemo"})
    public void setupNativeAndroidDemoApk() {
        System.setProperty("mobile_autoGrantPermissions", "true");

        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(false);
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);

        // self-managed execution
//        SHAFT.Properties.mobile.set().selfManaged(true);
//        SHAFT.Properties.mobile.set().selfManagedAndroidSDKVersion(31);

        // local appium server (for local and GitHub actions execution)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/Android-NativeDemoApp-0.4.0.apk");

        // local appium server (android-emulator docker-compose)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4725");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/Android-NativeDemoApp-0.4.0.apk");

        // remote browserstack server
        SHAFT.Properties.platform.set().executionAddress("browserstack");
        SHAFT.Properties.browserStack.set().platformVersion("13.0");
        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
        SHAFT.Properties.browserStack.set().appName("Android-NativeDemoApp.apk");

        String cached = cachedNativeDemoAppUrl.get();
        if (cached.isEmpty()) {
            // First run: upload the APK to BrowserStack
            SHAFT.Properties.browserStack.set().appRelativeFilePath("src/test/resources/testDataFiles/apps/Android-NativeDemoApp-0.4.0.apk");
            SHAFT.Properties.browserStack.set().appUrl("");
        } else {
            // Subsequent runs: reuse the previously uploaded app URL
            SHAFT.Properties.browserStack.set().appRelativeFilePath("");
            SHAFT.Properties.browserStack.set().appUrl(cached);
        }

        // remote browserstack server (existing app version)
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().platformVersion("13.0");
//        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
//        SHAFT.Properties.browserStack.set().appName("Android-NativeDemoApp.apk");
//        SHAFT.Properties.browserStack.set().appRelativeFilePath("");
//        SHAFT.Properties.browserStack.set().appUrl("bs://61abe95b5ed5bb6dc169f8df6b7141db120167d3");
        driver.set(new SHAFT.GUI.WebDriver());

        // After driver creation, BrowserStackHelper has set SHAFT.Properties.mobile.app() to the
        // uploaded bs:// URL. Cache it so subsequent test methods skip the upload entirely.
        if (cached.isEmpty()) {
            String uploadedUrl = SHAFT.Properties.mobile.app();
            if (uploadedUrl != null && !uploadedUrl.isEmpty()) {
                cachedNativeDemoAppUrl.compareAndSet("", uploadedUrl);
            }
        }
    }

    @BeforeMethod(onlyForGroups = {"ApiDemosDebug"})
    public void setupApiDemosDebug() {
        System.setProperty("mobile_autoGrantPermissions", "true");

        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(false);
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);

        // self-managed execution
//        SHAFT.Properties.mobile.set().selfManaged(true);
//        SHAFT.Properties.mobile.set().selfManagedAndroidSDKVersion(31);

        // local appium server (for local and GitHub actions execution)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");

        // local appium server (android-emulator docker-compose)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4725");
//        SHAFT.Properties.mobile.set().app("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");

        // remote browserstack server
        SHAFT.Properties.platform.set().executionAddress("browserstack");
        SHAFT.Properties.browserStack.set().platformVersion("13.0");
        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
        SHAFT.Properties.browserStack.set().appName("ApiDemos-debug.apk");

        String cached = cachedApiDemosAppUrl.get();
        if (cached.isEmpty()) {
            // First run: upload the APK to BrowserStack
            SHAFT.Properties.browserStack.set().appRelativeFilePath("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");
            SHAFT.Properties.browserStack.set().appUrl("");
        } else {
            // Subsequent runs: reuse the previously uploaded app URL
            SHAFT.Properties.browserStack.set().appRelativeFilePath("");
            SHAFT.Properties.browserStack.set().appUrl(cached);
        }

        // remote browserstack server (existing app version)
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().platformVersion("13.0");
//        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
//        SHAFT.Properties.browserStack.set().appName("ApiDemos-debug.apk");
//        SHAFT.Properties.browserStack.set().appRelativeFilePath("");
//        SHAFT.Properties.browserStack.set().appUrl("bs://61abe95b5ed5bb6dc169f8df6b7141db120167d3");
        driver.set(new SHAFT.GUI.WebDriver());

        // After driver creation, BrowserStackHelper has set SHAFT.Properties.mobile.app() to the
        // uploaded bs:// URL. Cache it so subsequent test methods skip the upload entirely.
        if (cached.isEmpty()) {
            String uploadedUrl = SHAFT.Properties.mobile.app();
            if (uploadedUrl != null && !uploadedUrl.isEmpty()) {
                cachedApiDemosAppUrl.compareAndSet("", uploadedUrl);
            }
        }
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        driver.get().quit();
    }
}
