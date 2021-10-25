package testPackage01;

import com.shaft.api.BrowserStack;
import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.TouchActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_AppiumNative {
    private final By egypt = By.xpath("//android.widget.TextView[@text='Egypt']");
    private final By english = By.xpath("//android.widget.TextView[@text='English']");
    private final By search = By.id("com.jumia.android:id/search_input_text");
    private final By scrollable_element = By.xpath("//android.widget.TextView[contains(@text,'XMXCZKJ')]");
    private WebDriver driver;

    @Test
    public void assertThatSearchIsDisplayed() {
        new TouchActions(driver)
                .tap(egypt)
                .tap(english);
        Validations.assertThat()
                .element(driver, search)
                .exists()
                .perform();
    }

    @Test(dependsOnMethods = {"assertThatSearchIsDisplayed"})
    public void searchJumia() {
        new TouchActions(driver)
                .tap(search)
                .performElementAction()
                .type(search, "Batman")
                .performTouchAction()
                .nativeKeyboardKeyPress(TouchActions.KeyboardKeys.SEARCH)
                .swipeElementIntoView(scrollable_element, TouchActions.SwipeDirection.DOWN);
        Validations.assertThat()
                .element(driver, scrollable_element)
                .matchesReferenceImage()
                .withCustomReportMessage("Asserting that the scrollable element is displayed")
                .perform();
    }

    @BeforeClass
    public void setup() {
        // common attributes
        System.setProperty("targetOperatingSystem", "Android");
        System.setProperty("mobile_automationName", "UIAutomator2");
        System.setProperty("mobile_appWaitActivity","*");

        // local appium server (for local and github actions execution)
        System.setProperty("executionAddress", "0.0.0.0:4723");
        System.setProperty("mobile_app", "src/test/resources/TestDataFiles/jumia-7-9-3.apk");
        driver = DriverFactory.getDriver();

        // remote browserstack server (new apk version)
//        System.setProperty("browserStack.appName", "JUMIA");
//        System.setProperty("browserStack.appRelativeFilePath", "src/test/resources/TestDataFiles/jumia-7-9-3.apk");
//        driver = DriverFactory.getBrowserStackDriver();

        // remote browserstack server (existing apk version)
//        System.setProperty("browserStack.appUrl", "bs://a66c4f868f65cfdd76ff17554e5f9c650c7773ce");
//        driver = DriverFactory.getBrowserStackDriver();
    }

    @AfterClass
    public void teardown() {
        BrowserFactory.closeAllBrowsers();
        // extra code to switch between web and mobile execution in the pipeline
//        System.setProperty("executionAddress", executionAddress);
//        System.setProperty("targetOperatingSystem", targetOperatingSystem);
//        System.setProperty("mobile_automationName", mobile_automationName);
//        System.setProperty("mobile_app", mobile_app);
    }
}
