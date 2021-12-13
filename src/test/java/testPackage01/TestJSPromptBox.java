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

public class TestJSPromptBox {
    private static final By JS_PromptAlert = By.xpath("//button[contains(text(),'Click for JS Prompt')]");
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
        ElementActions.click(driver, JS_PromptAlert);
        ElementActions.performAlertAction(driver).dismissAlert();
    }

    @Test(dependsOnMethods = "dismissAlert")
    public void assertOnCancelAlertResultText() {
        Validations.assertThat().element(driver, JS_ResultText).attribute(ValidationEnums.ElementAttribute.TEXT).
                matchesRegex("You entered: null").
                perform();
    }

    @Test(dependsOnMethods = "assertOnCancelAlertResultText")
    public void getAlertText() {
        ElementActions.click(driver, JS_PromptAlert);
        System.out.println("Alert text is: " + AlertActions.getAlertText(driver));
        Validations.assertThat().element(driver, JS_ResultText).attribute(ValidationEnums.ElementAttribute.TEXT).
                matchesRegex("I am a JS prompt").
                perform();
    }

    @Test(dependsOnMethods = "getAlertText")
    public void acceptPromptAlertWithoutTypingTextMessage() {
        ElementActions.performAlertAction().acceptAlert();
    }

    @Test(dependsOnMethods = "acceptPromptAlertWithoutTypingTextMessage")
    public void assertOnConfirmPromptAlertWithoutTypingTextMessageResultText() {
        Validations.assertThat().element(driver, JS_ResultText).attribute(ValidationEnums.ElementAttribute.TEXT).
                matchesRegex("You entered:").
                perform();
    }

    @Test(dependsOnMethods = "assertOnConfirmPromptAlertWithoutTypingTextMessageResultText")
    public void acceptPromptAlertWithTextMessage() {
        ElementActions.click(driver, JS_PromptAlert);
        AlertActions.typeIntoPromptAlert(driver, "Prompt Alert text message");
        ElementActions.performAlertAction().acceptAlert();
    }

    @Test(dependsOnMethods = "acceptPromptAlertWithTextMessage")
    public void assertOnConfirmPromptAlertWithTextMessageResultText() {
        Validations.assertThat().element(driver, JS_ResultText).attribute(ValidationEnums.ElementAttribute.TEXT).
                matchesRegex("You entered: Prompt Alert text message").
                perform();
    }
}