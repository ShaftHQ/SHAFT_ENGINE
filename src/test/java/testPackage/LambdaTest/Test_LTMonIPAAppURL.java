package testPackage.LambdaTest;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_LTMonIPAAppURL {



    private WebDriver driver;

    @Test
    public void test() {
        ElementActions.getInstance().performTouchAction().tap(AppiumBy.accessibilityId("Text Button"));
        ElementActions.getInstance().type(AppiumBy.accessibilityId("Text Input"), "hello@browserstack.com" + "\n");
        Validations.assertThat()
                .element(driver, AppiumBy.accessibilityId("Text Output"))
                .text()
                .isEqualTo("hello@browserstack.com")
                .perform();
    }

    @BeforeClass
    public void setup() {
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.IOS.toString());
        SHAFT.Properties.mobile.set().automationName("XCUITest");
        SHAFT.Properties.platform.set().executionAddress("lambdatest");
        SHAFT.Properties.lambdaTest.set().platformVersion("15");
        SHAFT.Properties.lambdaTest.set().deviceName("iPhone 13");
        SHAFT.Properties.lambdaTest.set().appUrl("lt://APP1016039251685365091368730");
        driver = DriverFactory.getDriver();

    }

    @AfterClass(alwaysRun = true)
    public void teardown() {
        DriverFactory.closeAllDrivers();
    }
}