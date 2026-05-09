package testPackage.appium;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.ElementActions;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.Validations;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.Platform;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class IOSBasicInteractionsTest {
    private static final String ENABLE_NATIVE_IOS_E2E_PROPERTY = "shaft.enableNativeIosE2E";
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void test() {
        new ElementActions(driver.get().getDriver()).performTouchAction().tap(AppiumBy.accessibilityId("Text Button"));
        new ElementActions(driver.get().getDriver()).type(AppiumBy.accessibilityId("Text Input"), "hello@browserstack.com" + "\n");
        Validations.assertThat()
                .element(driver.get().getDriver(), AppiumBy.accessibilityId("Text Output"))
                .text()
                .isEqualTo("hello@browserstack.com")
                .perform();
    }

    @SuppressWarnings("CommentedOutCode")
    @BeforeMethod
    public void setup() {
        if (!Boolean.getBoolean(ENABLE_NATIVE_IOS_E2E_PROPERTY)) {
            throw new SkipException("Native iOS BrowserStack E2E is disabled for the web Safari matrix. Set -D"
                    + ENABLE_NATIVE_IOS_E2E_PROPERTY + "=true in a native iOS job after validating the app upload.");
        }
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.IOS.toString());
        SHAFT.Properties.mobile.set().automationName("XCUITest");
        SHAFT.Properties.mobile.set().browserName("");
        System.setProperty("mobile_appWaitActivity", "*");

        // local self-managed instance routing to browserstack for ios
//        SHAFT.Properties.mobile.set().selfManaged(true);

//         local appium server (for local and GitHub actions execution)
//        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
//        SHAFT.Properties.mobile.set().app(SHAFT.Properties.paths.testData() + "apps/BStackSampleApp.ipa");

        // remote browserstack server (new app version)
        SHAFT.Properties.platform.set().executionAddress("browserstack");
        SHAFT.Properties.browserStack.set().osVersion("16");
        SHAFT.Properties.browserStack.set().deviceName("iPhone 14");
        SHAFT.Properties.browserStack.set().appName("BStackSampleApp.ipa");
        SHAFT.Properties.browserStack.set().appRelativeFilePath(SHAFT.Properties.paths.testData() +  "apps/BStackSampleApp.ipa");
        SHAFT.Properties.browserStack.set().appUrl("");

        // remote browserstack server (existing app version)
//        System.setProperty("browserStack.platformVersion", "14");
//        System.setProperty("browserStack.deviceName", "iPhone 12 Pro Max");
//        System.setProperty("browserStack.appName", "");
//        System.setProperty("browserStack.appRelativeFilePath", "");
//        System.setProperty("browserStack.appUrl", "bs://e2c374a22cf954e582b5c02e9a9f7cfd650a8325");
        driver.set(new SHAFT.GUI.WebDriver());

    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        if (driver.get() != null) {
            driver.get().quit();
            driver.remove();
        }
        Properties.clearForCurrentThread();
    }
}
