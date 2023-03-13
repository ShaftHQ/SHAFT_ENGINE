package testPackage;

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

public class JSPromptBoxTests {
    private static final By JS_PromptAlert = By.xpath("//button[contains(text(),'Click for JS Prompt')]");
    private static final By JS_ResultText = By.id("result");
    private static WebDriver driver;

    @BeforeClass
    public void navigateToJSAlertPage() {
        driver = DriverFactory.getDriver();
        BrowserActions.getInstance().navigateToURL("http://the-internet.herokuapp.com/javascript_alerts");
    }

    @AfterClass(alwaysRun = true)
    public void tearDownDriver() {
        BrowserActions.getInstance().closeCurrentWindow();
    }

    @Test
    public void dismissAlert() {
        ElementActions.getInstance().click(JS_PromptAlert);
        ElementActions.getInstance().performAlertAction().dismissAlert();
    }

    @Test(dependsOnMethods = "dismissAlert")
    public void assertOnCancelAlertResultText() {
        Validations.assertThat().element(driver, JS_ResultText).text().isEqualTo("You entered: null").perform();
    }

    @Test(dependsOnMethods = "assertOnCancelAlertResultText")
    public void getAlertText() {
        ElementActions.getInstance().click(JS_PromptAlert);
        ReportManager.logDiscrete("Alert text is: [" + ElementActions.getInstance().performAlertAction().getAlertText() + "]");
        Validations.assertThat().object(ElementActions.getInstance().performAlertAction().getAlertText()).isEqualTo("I am a JS prompt").perform();
    }

    @Test(dependsOnMethods = "getAlertText")
    public void acceptPromptAlertWithoutTypingTextMessage() {
        ElementActions.getInstance().click(JS_PromptAlert);
        ElementActions.getInstance().performAlertAction().acceptAlert();
    }

    @Test(dependsOnMethods = "acceptPromptAlertWithoutTypingTextMessage")
    public void assertOnConfirmPromptAlertWithoutTypingTextMessageResultText() {
        Validations.assertThat().element(JS_ResultText).text().contains("You entered:").perform();
    }

    @Test(dependsOnMethods = "assertOnConfirmPromptAlertWithoutTypingTextMessageResultText")
    public void acceptPromptAlertWithTextMessage() {
        ElementActions.getInstance().click(JS_PromptAlert);
        ElementActions.getInstance().performAlertAction().typeIntoPromptAlert("Prompt Alert text message");
        ElementActions.getInstance().performAlertAction().acceptAlert();
    }

    @Test(dependsOnMethods = "acceptPromptAlertWithTextMessage")
    public void assertOnConfirmPromptAlertWithTextMessageResultText() {
        Validations.assertThat().element(driver, JS_ResultText).text().isEqualTo("You entered: Prompt Alert text message").perform();
    }
}