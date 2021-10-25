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
    // extra code to switch between web and mobile execution in the pipeline
//    private final String executionAddress = System.getProperty("executionAddress");
//    private final String targetOperatingSystem = System.getProperty("targetOperatingSystem");
//    private final String mobile_automationName = System.getProperty("mobile_automationName");
//    private final String mobile_app = System.getProperty("mobile_app");

    private final By egypt = By.xpath("//android.widget.TextView[@text='Egypt']");
    private final By english = By.xpath("//android.widget.TextView[@text='English']");
    private final By search = By.id("com.jumia.android:id/search_input_text");
    private final By scrollable_element = By.xpath("//android.widget.TextView[contains(@text,'Batman Vs Superman Key Chain')]");
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

        // local appium server (for local and github actions execution)
//        System.setProperty("executionAddress", "0.0.0.0:4723");
//        System.setProperty("mobile_app", "src/test/resources/TestDataFiles/jumia-7-9-3.apk");

        // remote browserstack server (new apk version)
//        BrowserStack.setupNativeAppExecution("mohabmohie1", "7E7PgzBtwk4sWLUcF8Y5",
//                "Google Pixel 3", "9.0", "src/test/resources/TestDataFiles/jumia-7-9-3.apk", "JUMIA");

        // remote browserstack server (existing apk version)
        var browserStackOptions = BrowserStack.setupNativeAppExecution("mohabmohie1", "7E7PgzBtwk4sWLUcF8Y5",
                "Google Pixel 5", "11.0", "bs://a66c4f868f65cfdd76ff17554e5f9c650c7773ce");

        driver = BrowserFactory.getBrowser(DriverFactory.DriverType.APPIUM_MOBILE_NATIVE, browserStackOptions);
        /**
         * DriverFactory.getBrowserStackDriver();
         * Add browserStack properties to the mobile properties file or create a separate one?
         */
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
