package testPackage01;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_JSPromptBox {
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
        Validations.assertThat().element(driver, JS_ResultText).text().isEqualTo("You entered: null").perform();
    }

    @Test(dependsOnMethods = "assertOnCancelAlertResultText")
    public void getAlertText() {
        ElementActions.click(driver, JS_PromptAlert);
        ReportManager.logDiscrete("Alert text is: [" + ElementActions.performAlertAction(driver).getAlertText() + "]");
        Validations.assertThat().object(ElementActions.performAlertAction(driver).getAlertText()).isEqualTo("I am a JS prompt").perform();
    }

    @Test(dependsOnMethods = "getAlertText")
    public void acceptPromptAlertWithoutTypingTextMessage() {
        ElementActions.click(driver, JS_PromptAlert);
        ElementActions.performAlertAction(driver).acceptAlert();
    }

    @Test(dependsOnMethods = "acceptPromptAlertWithoutTypingTextMessage")
    public void assertOnConfirmPromptAlertWithoutTypingTextMessageResultText() {
        Validations.assertThat().element(driver, JS_ResultText).text().isEqualTo("You entered:").perform();
    }

    @Test(dependsOnMethods = "assertOnConfirmPromptAlertWithoutTypingTextMessageResultText")
    public void acceptPromptAlertWithTextMessage() {
        ElementActions.click(driver, JS_PromptAlert);
        ElementActions.performAlertAction(driver).typeIntoPromptAlert("Prompt Alert text message");
        ElementActions.performAlertAction(driver).acceptAlert();
    }

    @Test(dependsOnMethods = "acceptPromptAlertWithTextMessage")
    public void assertOnConfirmPromptAlertWithTextMessageResultText() {
        Validations.assertThat().element(driver, JS_ResultText).text().isEqualTo("You entered: Prompt Alert text message").perform();
    }
}