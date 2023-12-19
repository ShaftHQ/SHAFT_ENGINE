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

    private WebDriver driver;

    @Test
    public void testIsElementClickable() {
        BrowserActions.getInstance(driver).navigateToURL("https://the-internet.herokuapp.com/");
        Validations.assertThat().object(ElementActions.getInstance(driver).isElementClickable(By.linkText("File Upload"))).isTrue().perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = DriverFactory.getHelper().getDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        BrowserActions.getInstance(driver).closeCurrentWindow();
    }
}
