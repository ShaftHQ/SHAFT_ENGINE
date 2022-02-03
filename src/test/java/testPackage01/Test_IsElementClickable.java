package testPackage01;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Assertions.AssertionType;
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
        Assertions.assertTrue(ElementActions.isElementClickable(driver, By.linkText("File Upload")),
                AssertionType.POSITIVE);
    }

    @BeforeMethod
    public void beforeMethod() {
        driver = BrowserFactory.getBrowser();
    }

    @AfterMethod
    public void afterMethod() {
        BrowserActions.closeCurrentWindow(driver);
    }
}
