package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class SelectMethodTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    private final By dropDownList = By.className("dropdown");

    @Test
    public void testValidSelect() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            driver.get().browser().navigateToURL(SHAFT.Properties.paths.testData() + "selectDemo.html");
            clickDropDownList("Div 1");
            clickDropDownList("Div 2");
            clickDropDownList("Div 3");
        }
    }

    @Test
    public void testInvalidSelect() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            driver.get().browser().navigateToURL(SHAFT.Properties.paths.testData() + "selectDemo.html");
            try {

                clickDropDownList("Div 1000");

            } catch (AssertionError error){
                
            }
        }
    }

    private void clickDropDownList(String text) {
        driver.get().element().select(dropDownList, text);

    }

    @BeforeMethod
    protected void setUp() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    protected void tearDown() {
        if (driver.get() != null) {
            driver.get().quit();
        }
    }

}
