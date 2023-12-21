package testPackage.LambdaTest;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import io.appium.java_client.AppiumBy;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.*;

public class Test_LTMobIPARelativePath {
    SHAFT.TestData.JSON testData;
    private WebDriver driver;

    @Test
    public void test() {

        new ElementActions(driver).performTouchAction().tap(AppiumBy.accessibilityId("Text Button"));
        new ElementActions(driver).type(AppiumBy.accessibilityId("Text Input"), "hello@browserstack.com" + "\n");
        Validations.assertThat()
                .element(driver, AppiumBy.accessibilityId("Text Output"))
                .text()
                .isEqualTo("hello@browserstack.com")
                .perform();
    }

    @BeforeMethod
    public void setup() {
        testData = new SHAFT.TestData.JSON("credentials.json");
        // common attributes
        SHAFT.Properties.platform.set().targetPlatform(Platform.IOS.toString());
        SHAFT.Properties.mobile.set().automationName("XCUITest");
        System.setProperty("mobile_appWaitActivity", "*");
        SHAFT.Properties.platform.set().executionAddress("lambdatest");
        SHAFT.Properties.lambdaTest.set().appRelativeFilePath("src/test/resources/testDataFiles/apps/BStackSampleApp.ipa");
        SHAFT.Properties.lambdaTest.set().appName("BStackSampleApp.ipa");
        SHAFT.Properties.lambdaTest.set().platformVersion("16");
        SHAFT.Properties.lambdaTest.set().deviceName("iPhone 14");
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.lambdaTest.set().isRealMobile(true);
        SHAFT.Properties.lambdaTest.set().username(testData.getTestData("LambdaTestUserName"));
        SHAFT.Properties.lambdaTest.set().accessKey(testData.getTestData("LambdaTestAccessKey"));
        SHAFT.Properties.flags.set().attemptClearBeforeTyping(false);
        driver = DriverFactory.getHelper().getDriver();

    }

    @AfterMethod(alwaysRun = true)
    public void teardown() {
        DriverFactory.closeAllDrivers();
    }
}