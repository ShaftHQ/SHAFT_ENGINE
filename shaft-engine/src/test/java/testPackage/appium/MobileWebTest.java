package testPackage.appium;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class MobileWebTest {
    private static final String FORM_URL = "https://www.selenium.dev/selenium/web/web-form.html";
    private static final By TEXT_INPUT = By.id("my-text-id");
    private static final By PASSWORD = By.name("my-password");
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    SHAFT.TestData.JSON testData;

    @Test
    public void test() {
        driver.get().element().type(TEXT_INPUT, "SHAFT")
                .type(PASSWORD, "stable fixture")
                .captureScreenshot(PASSWORD)
                .and().browser().captureScreenshot()
                .and().assertThat().url().contains("web-form.html").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        driver.get().browser().navigateToURL(FORM_URL);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (driver.get() != null) {
            driver.get().quit();
        }
        driver.remove();
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
