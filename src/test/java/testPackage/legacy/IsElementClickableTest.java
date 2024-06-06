package testPackage.legacy;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class IsElementClickableTest {

    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void testIsElementClickable() {
        new BrowserActions(driver.get().getDriver()).navigateToURL("https://the-internet.herokuapp.com/");
        Validations.assertThat().object(new ElementActions(driver.get().getDriver()).isElementClickable(By.linkText("File Upload"))).isTrue().perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
