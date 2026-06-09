package testPackage.mockedTests;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.openqa.selenium.remote.Browser;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class SelectedValueTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();
    String baseURL = SHAFT.Properties.paths.testData() + "selectedValueTests/";
    By select = By.tagName("select");

    @Test
    public void simpleSelect() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            String textToSelect = "Third Value";
            driver.get().browser().navigateToURL(baseURL + "Basic_select.html")
                    .performElementAction().select(select, textToSelect);
            driver.get().assertThat().element(select).attribute("selectedText").isEqualTo(textToSelect).perform();
        }
    }

    @Test
    public void multipleSelect() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")
                && !SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            driver.get().browser().navigateToURL(baseURL + "Advanced_select_with_multiple_features.html")
                    .performElementAction().select(select, "Dog")
                    .select(select, "Cat");
            driver.get().assertThat().element(select).attribute("selectedText").isEqualTo("DogCat").perform();
        }
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        if (driver.get() != null) {
            driver.get().quit();
        }
    }

}
