package testPackage01;

import com.shaft.api.BrowserStack;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.TouchActions;
import com.shaft.validation.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_AppiumNative {
    // extra code to switch between web and mobile execution in the pipeline
    private final String executionAddress = System.getProperty("executionAddress");
    private final String targetOperatingSystem = System.getProperty("targetOperatingSystem");
    private final String mobile_automationName = System.getProperty("mobile_automationName");
    private final String mobile_app = System.getProperty("mobile_app");

    private final By egypt = By.xpath("//android.widget.TextView[@text='Egypt']");
    private final By english = By.xpath("//android.widget.TextView[@text='English']");
    private final By search = By.id("com.jumia.android:id/search_input_text");
    private final By scrollable_element = By.xpath("//android.widget.TextView[contains(@text,'Batman Vs Superman Key Chain')]");
    private final By batman_armor = By.xpath("//android.widget.TextView[contains(@text,'Batman Armor')]/ancestor::android.view.ViewGroup[2]");
    private WebDriver driver;

    @Test
    public void searchJumia() {
        new TouchActions(driver)
                .tap(egypt)
                .tap(english)
                .tap(search)
                .performElementAction()
                .type(search, "Batman")
                .performTouchAction()
                .keyPress(TouchActions.KeyboardKeys.SEARCH)
                .swipeElementIntoView(scrollable_element, TouchActions.SwipeDirection.DOWN);
        Assertions.assertElementMatches(driver, scrollable_element, "Asserting that the scrollable element is displayed");
    }

    @BeforeClass
    public void setup() {
        // common attributes
        System.setProperty("targetOperatingSystem", "Android");
        System.setProperty("mobile_automationName", "Appium");

        // local appium server
        //System.setProperty("executionAddress", "0.0.0.0:4723");
        //System.setProperty("mobile_app", FileActions.getAbsolutePath("src/test/resources/", "JUMIA_v7.1.1.apk"));

        // remote browserstack server
        BrowserStack.setupNativeAppExecution("mohabmohie1", "7E7PgzBtwk4sWLUcF8Y5", "Samsung Galaxy Note 20", "10.0", "src/test/resources/JUMIA_v7.1.1.apk", "JUMIA");

        driver = BrowserFactory.getBrowser();
    }

    @AfterClass
    public void teardown() {
        BrowserFactory.closeAllDrivers();
        // extra code to switch between web and mobile execution in the pipeline
        System.setProperty("executionAddress", executionAddress);
        System.setProperty("targetOperatingSystem", targetOperatingSystem);
        System.setProperty("mobile_automationName", mobile_automationName);
        System.setProperty("mobile_app", mobile_app);
    }
}
