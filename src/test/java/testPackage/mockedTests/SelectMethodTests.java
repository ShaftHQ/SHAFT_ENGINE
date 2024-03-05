package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class SelectMethodTests {
    private SHAFT.GUI.WebDriver driver;
    private final By dropDownList = By.className("dropdown");

    @Test
    public void testValidSelect() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            driver.browser().navigateToURL(SHAFT.Properties.paths.testData() + "selectDemo.html");
            clickDropDownList("Div 1");
            clickDropDownList("Div 2");
            clickDropDownList("Div 3");
        }
    }

    @Test(expectedExceptions = AssertionError.class, alwaysRun = true)
    public void testInvalidSelect() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            driver.browser().navigateToURL(SHAFT.Properties.paths.testData() + "selectDemo.html");
            clickDropDownList("Div 1000");
        }
    }

    private void clickDropDownList(String text) {
        driver.element().select(dropDownList, text);

    }

    @BeforeMethod
    protected void setUp() {
        driver = new SHAFT.GUI.WebDriver();
    }

    @AfterMethod
    protected void tearDown() {
        if (driver != null) {
            driver.quit();
        }
    }

}
