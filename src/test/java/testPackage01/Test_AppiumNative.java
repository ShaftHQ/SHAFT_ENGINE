package testPackage01;

import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.TouchActions;
import com.shaft.validation.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_AppiumNative {
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
                .tap("src/test/resources/search.png")
                .swipeElementIntoView(scrollable_element, TouchActions.SwipeDirection.DOWN);
        Assertions.assertElementMatches(driver, scrollable_element, "Asserting that the scrollable element is displayed");
    }

    @BeforeClass
    public void setup() {
        System.setProperty("executionAddress", "0.0.0.0:4723");
        System.setProperty("targetOperatingSystem", "Android");
        System.setProperty("mobile_automationName", "Appium");
        System.setProperty("mobile_app", FileActions.getAbsolutePath("src/test/resources/", "JUMIA_v7.1.1.apk"));
        driver = BrowserFactory.getBrowser();
    }

    @AfterClass
    public void teardown() {
        BrowserActions.closeCurrentWindow(driver);
    }
}
