package testPackage.validationsWizard;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class AttributeWaitTests {
    private final String URL = "https://the-internet.herokuapp.com/dynamic_loading/1";
    private final By buttonStart = By.cssSelector("#start button");
    private final By divFinish = By.id("finish");
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @BeforeMethod
    void setup() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @Test
    void testWaitToAttribute() {
        driver.get().browser().navigateToURL(URL);
        driver.get().element().click(buttonStart);
        if (SHAFT.Properties.web.targetBrowserName().equalsIgnoreCase("safari")) {
            driver.get().element().waitToAttribute(divFinish, "style", "display: none;");
        } else {
            driver.get().element().waitToAttribute(divFinish, "style", "");
        }
        driver.get().assertThat().element(divFinish).isVisible().perform();
    }

    @AfterMethod
    void tearDown() {
        driver.get().quit();
    }
}
