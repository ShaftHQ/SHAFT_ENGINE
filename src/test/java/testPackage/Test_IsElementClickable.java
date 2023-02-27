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

public class Test_IsElementClickable {

    private WebDriver driver;

    @Test
    public void testIsElementClickable() {
        BrowserActions.navigateToURL(driver, "https://the-internet.herokuapp.com/");
        Validations.assertThat().object(ElementActions.isElementClickable(driver, By.linkText("File Upload"))).isTrue().perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = DriverFactory.getDriver();
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        BrowserActions.closeCurrentWindow(driver);
    }
}
