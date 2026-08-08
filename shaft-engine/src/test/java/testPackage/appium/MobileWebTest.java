package testPackage.appium;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class MobileWebTest {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    SHAFT.TestData.JSON testData;

    @Test
    public void test() {
        driver.get().element().type(By.id("user-name"), "standard_user")
                .type(By.id("password"), "secret_sauce")
                .captureScreenshot(By.id("password"))
                .and().browser().captureScreenshot()
                .and().assertThat().url().contains("saucedemo").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL("https://www.saucedemo.com/");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }

    @SuppressWarnings("CommentedOutCode")
    @BeforeClass
    public void beforeClass() {
        // common attributes android
//        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
//        SHAFT.Properties.mobile.set().automationName(AutomationName.ANDROID_UIAUTOMATOR2);

        // common attributes ios
//        SHAFT.Properties.platform.set().targetPlatform(Platform.IOS.name());
//        SHAFT.Properties.mobile.set().automationName(AutomationName.IOS_XCUI_TEST);

        // self-managed execution (android only) [WARNING: WORK IN PROGRESS]
//        SHAFT.Properties.mobile.set().selfManaged(true);
//        SHAFT.Properties.mobile.set().selfManagedAndroidSDKVersion(31);

        // local appium server (for local and GitHub actions execution)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
//        SHAFT.Properties.mobile.set().app("");

        // local appium server (android-emulator docker-compose)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4725");
//        SHAFT.Properties.mobile.set().app("");

//         remote browserstack server (common for web execution)
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().appName("");
//        SHAFT.Properties.browserStack.set().appRelativeFilePath("");
//        SHAFT.Properties.browserStack.set().appUrl("");

//         remote browserstack server (android) [NATIVE SAMSUNG BROWSER] || [CHROME]
//        SHAFT.Properties.browserStack.set().osVersion("13.0");
//        SHAFT.Properties.browserStack.set().deviceName("Samsung Galaxy S23");
//        SHAFT.Properties.mobile.set().browserName(Browser.CHROME.browserName());
//        SHAFT.Properties.mobile.set().browserName("samsung");

        // remote browserstack server (ios) [SAFARI BROWSER]
//        SHAFT.Properties.browserStack.set().osVersion("16");
//        SHAFT.Properties.browserStack.set().deviceName("iPhone 14");
//        SHAFT.Properties.mobile.set().browserName(Browser.SAFARI.browserName());

        testData = new SHAFT.TestData.JSON("simpleJSON.json");
    }
}
