package testPackage01.appium;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class IOSBasicInteractionsTest {
    private WebDriver driver;

    @Test
    public void test() {
        ElementActions.performTouchAction(driver).tap(AppiumBy.accessibilityId("Text Button"));
        System.setProperty("forceCheckTextWasTypedCorrectly","false");
        ElementActions.type(driver, AppiumBy.accessibilityId("Text Input"), "hello@browserstack.com"+"\n");
        Validations.assertThat()
                .element(driver, AppiumBy.accessibilityId("Text Output"))
                .text()
                .isEqualTo("hello@browserstack.com")
                .perform();
    }

    @BeforeClass
    public void setup() {
        // common attributes
        System.setProperty("targetOperatingSystem", "iOS");
        System.setProperty("mobile_automationName", "XCUITest");
        System.setProperty("mobile_appWaitActivity","*");

        // local appium server (for local and github actions execution)
//        System.setProperty("executionAddress", "0.0.0.0:4723");
//        System.setProperty("mobile_app", "src/test/resources/TestDataFiles/apps/BStackSampleApp.ipa");
//        driver = DriverFactory.getDriver();

        // remote browserstack server (new app version)
//        System.setProperty("browserStack.platformVersion", "14");
//        System.setProperty("browserStack.deviceName", "iPhone 12 Pro Max");
//        System.setProperty("browserStack.appName", "TestApp");
//        System.setProperty("browserStack.appRelativeFilePath", "src/test/resources/TestDataFiles/apps/BStackSampleApp.ipa");
//        System.setProperty("browserStack.appUrl", "");
//        driver = DriverFactory.getBrowserStackDriver();

        // remote browserstack server (existing app version)
        System.setProperty("browserStack.platformVersion", "14");
        System.setProperty("browserStack.deviceName", "iPhone 12 Pro Max");
        System.setProperty("browserStack.appName", "");
        System.setProperty("browserStack.appRelativeFilePath", "");
        System.setProperty("browserStack.appUrl", "bs://e2c374a22cf954e582b5c02e9a9f7cfd650a8325");
        driver = DriverFactory.getBrowserStackDriver();

    }

    @AfterClass
    public void teardown() {
        BrowserFactory.closeAllBrowsers();
    }
}