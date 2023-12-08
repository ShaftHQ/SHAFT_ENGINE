package testPackage.LambdaTest;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.gui.element.ElementActions;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.Platform;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_LTMobIPAAppURL {
    SHAFT.TestData.JSON testData;
    private SHAFT.GUI.WebDriver driver;

    @Test
    public void test() {
        ElementActions.getInstance().performTouchAction().tap(AppiumBy.accessibilityId("Text Button"));
        ElementActions.getInstance().type(AppiumBy.accessibilityId("Text Input"), "hello@browserstack.com" + "\n");
        SHAFT.Validations.assertThat().object(driver.element().getText(AppiumBy.accessibilityId("Text Output"))).
                isEqualTo("hello@browserstack.com").perform();
    }

    @BeforeClass
    public void setup() {
        testData = new SHAFT.TestData.JSON("credentials.json");
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.IOS.toString());
        SHAFT.Properties.mobile.set().automationName("XCUITest");
        SHAFT.Properties.platform.set().executionAddress("lambdatest");
        SHAFT.Properties.lambdaTest.set().platformVersion("15");
        SHAFT.Properties.lambdaTest.set().deviceName("iPhone 13");
        SHAFT.Properties.lambdaTest.set().appUrl("lt://APP10160612541701952809945764");
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.lambdaTest.set().username(testData.getTestData("LambdaTestUserName"));
        SHAFT.Properties.lambdaTest.set().accessKey(testData.getTestData("LambdaTestAccessKey"));
        SHAFT.Properties.flags.set().attemptClearBeforeTyping(false);
        driver = new SHAFT.GUI.WebDriver();

    }

    @AfterClass(alwaysRun = true)
    public void teardown() {
        DriverFactory.closeAllDrivers();
    }
}