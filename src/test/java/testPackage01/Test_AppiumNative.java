package testPackage01;

import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
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
    private final By batman_armor = By.xpath("//android.widget.TextView[contains(@text,'Batman Armor')]/ancestor::android.view.ViewGroup[2]");
    private WebDriver driver;

    @Test
    public void searchJumia() {
        ElementActions.performTouchAction(driver).tap(egypt);
        ElementActions.performTouchAction(driver).tap(english);
        ElementActions.performTouchAction(driver).tap(search);
        ElementActions.type(driver, search, "Batman");
        ElementActions.performTouchAction(driver).tap("src/test/resources/search.png");
        Assertions.assertElementExists(driver, batman_armor, "Asserting that the Batman Armor Item is displayed");
    }

    @BeforeClass
    public void setup() {
//        System.setProperty("executionAddress", "0.0.0.0:4723");
//        System.setProperty("mobile_platformName", "Android");
        System.setProperty("mobile_app", FileActions.getAbsolutePath("src/test/resources/", "JUMIA_v7.1.1.apk"));
        driver = BrowserFactory.getBrowser();
    }

    @AfterClass
    public void teardown() {
        BrowserActions.closeCurrentWindow(driver);
    }
}
