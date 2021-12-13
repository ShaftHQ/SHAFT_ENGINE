package testPackage01;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterTest;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class TestJSAlertBox {

    private static final By JS_AlertBox = By.xpath("//button[contains(text(),'Click for JS Alert')]");
    private static final By JS_ResultText = By.id("result");
    private static WebDriver driver;

    @BeforeTest
    public void navigateToJSAlertPage() {
        driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver, "http://the-internet.herokuapp.com/javascript_alerts");
    }

    @Test
    public void getJSAlertText() {
        ElementActions.click(driver, JS_AlertBox);
        System.out.println("Alert text is: " + AlertActions.getAlertText(driver));
        Validations.assertThat().element(driver, JS_ResultText).attribute(ValidationEnums.ElementAttribute.TEXT).
                matchesRegex("I am a JS Alert").
                perform();
    }

    @Test(dependsOnMethods = "getJSAlertText")
    public void acceptAlert() {
        ElementActions.performAlertAction().acceptAlert();
    }

    @Test(dependsOnMethods = "acceptAlert")
    public void assertOnConfirmAlertResultText() {
        Validations.assertThat().element(driver, JS_ResultText).attribute(ValidationEnums.ElementAttribute.TEXT).
                matchesRegex("You successfully clicked an alert").
                perform();
    }

    @AfterTest
    public void tearDownDriver() {
        BrowserActions.closeCurrentWindow(driver);
    }
}