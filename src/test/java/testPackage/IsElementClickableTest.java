package testPackage;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class IsElementClickableTest {

    private static final ThreadLocal<WebDriver> driver = new ThreadLocal<>();

    @Test
    public void testIsElementClickable() {
        new BrowserActions(driver.get()).navigateToURL("https://the-internet.herokuapp.com/");
        Validations.assertThat().object(new ElementActions(driver.get()).isElementClickable(By.linkText("File Upload"))).isTrue().perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new DriverFactory().getDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        new BrowserActions(driver.get()).closeCurrentWindow();
    }
}
