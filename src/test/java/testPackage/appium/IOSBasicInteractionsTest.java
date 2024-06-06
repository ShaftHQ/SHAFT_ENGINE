package testPackage.appium;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class IOSBasicInteractionsTest {
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
    @BeforeClass
    public void setup() {
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.IOS.toString());
        SHAFT.Properties.mobile.set().automationName("XCUITest");
        System.setProperty("mobile_appWaitActivity", "*");

        // local self-managed instance routing to browserstack for ios
//        SHAFT.Properties.mobile.set().selfManaged(true);

//         local appium server (for local and GitHub actions execution)
        SHAFT.Properties.platform.set().executionAddress("localhost:4723");
        SHAFT.Properties.mobile.set().app(SHAFT.Properties.paths.testData() + "apps/BStackSampleApp.ipa");

        // remote browserstack server (new app version)
//        SHAFT.Properties.platform.set().executionAddress("browserstack");
//        SHAFT.Properties.browserStack.set().platformVersion("14");
//        SHAFT.Properties.browserStack.set().deviceName("iPhone 12 Pro Max");
//        SHAFT.Properties.browserStack.set().appName("TestApp");
//        SHAFT.Properties.browserStack.set().appRelativeFilePath(SHAFT.Properties.paths.testData() +  "apps/BStackSampleApp.ipa");
//        SHAFT.Properties.browserStack.set().appUrl("");

        // remote browserstack server (existing app version)
//        System.setProperty("browserStack.platformVersion", "14");
//        System.setProperty("browserStack.deviceName", "iPhone 12 Pro Max");
//        System.setProperty("browserStack.appName", "");
//        System.setProperty("browserStack.appRelativeFilePath", "");
//        System.setProperty("browserStack.appUrl", "bs://e2c374a22cf954e582b5c02e9a9f7cfd650a8325");
        driver.set(new SHAFT.GUI.WebDriver());

    }

    @AfterClass(alwaysRun = true)
    public void teardown() {
        driver.get().quit();
    }
}