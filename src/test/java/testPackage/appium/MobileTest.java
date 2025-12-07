package testPackage.appium;

import com.shaft.driver.SHAFT;
import io.appium.java_client.remote.AutomationName;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;

public abstract class MobileTest {
    public static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

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

        // remote browserstack server (new app version)
        SHAFT.Properties.platform.set().executionAddress("browserstack");
        SHAFT.Properties.browserStack.set().platformVersion("13.0");
        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
        SHAFT.Properties.browserStack.set().appName("ApiDemos-debug.apk");
        SHAFT.Properties.browserStack.set().appRelativeFilePath("src/test/resources/testDataFiles/apps/ApiDemos-debug.apk");
        SHAFT.Properties.browserStack.set().appUrl("");

        // remote browserstack server (existing app version)
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().platformVersion("13.0");
//        SHAFT.Properties.browserStack.set().deviceName("Google Pixel 7");
//        SHAFT.Properties.browserStack.set().appName("ApiDemos-debug.apk");
//        SHAFT.Properties.browserStack.set().appRelativeFilePath("");
//        SHAFT.Properties.browserStack.set().appUrl("bs://61abe95b5ed5bb6dc169f8df6b7141db120167d3");
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        driver.get().quit();
    }
}
