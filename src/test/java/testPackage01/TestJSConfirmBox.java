package testPackage01;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class TestJSConfirmBox {

    private static final By JS_ConfirmAlert = By.xpath("//button[contains(text(),'Click for JS Confirm')]");
    private static final By JS_ResultText = By.id("result");
    private static WebDriver driver;

    @BeforeClass
    public void navigateToJSAlertPage() {
        driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver, "http://the-internet.herokuapp.com/javascript_alerts");
    }

    @AfterClass
    public void tearDownDriver() {
        BrowserActions.closeCurrentWindow(driver);
    }

    @Test
    public void dismissAlert() {
        ElementActions.click(driver, JS_ConfirmAlert);
        ElementActions.performAlertAction(driver).dismissAlert();
    }

    @Test(dependsOnMethods = "dismissAlert")
    public void assertOnCancelAlertResultText() {
        Validations.assertThat().element(driver, JS_ResultText).attribute(ValidationEnums.ElementAttribute.TEXT).
                matchesRegex("You clicked: Cancel").
                perform();
    }

    @Test(dependsOnMethods = "assertOnCancelAlertResultText")
    public void getAlertText() {
        ElementActions.click(driver, JS_ConfirmAlert);
        System.out.println("Alert text is: " + AlertActions.getAlertText(driver));
        Validations.assertThat().element(driver, JS_ResultText).attribute(ValidationEnums.ElementAttribute.TEXT).
                matchesRegex("I am a JS Confirm").
                perform();
    }

    @Test(dependsOnMethods = "getAlertText")
    public void acceptAlert() {
        ElementActions.performAlertAction().acceptAlert();
    }

    @Test(dependsOnMethods = "acceptAlert")
    public void assertOnConfirmAlertResultText() {
        Validations.assertThat().element(driver, JS_ResultText).attribute(ValidationEnums.ElementAttribute.TEXT).
                matchesRegex("You clicked: Ok").
                perform();
    }
}