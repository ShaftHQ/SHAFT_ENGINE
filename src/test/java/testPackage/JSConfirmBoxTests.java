/*
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

public class JSConfirmBoxTests {

    private static final By JS_ConfirmAlert = By.xpath("//button[contains(text(),'Click for JS Confirm')]");
    private static final By JS_ResultText = By.id("result");
    private static WebDriver driver;

    @BeforeClass
    public void navigateToJSAlertPage() {
        driver.set(new DriverFactory().getDriver());
        new BrowserActions(driver.get()).navigateToURL("http://the-internet.herokuapp.com/javascript_alerts");
    }

    @AfterClass(alwaysRun = true)
    public void tearDownDriver() {
        new BrowserActions(driver.get()).closeCurrentWindow();
    }

    @Test
    public void dismissAlert() {
        new ElementActions(driver.get()).click(JS_ConfirmAlert);
        ElementActions.getInstance().performAlertAction().dismissAlert();
    }

    @Test(dependsOnMethods = "dismissAlert")
    public void assertOnCancelAlertResultText() {
        Validations.assertThat().element(driver.get(), JS_ResultText).text().isEqualTo("You clicked: Cancel").perform();
    }

    @Test(dependsOnMethods = "assertOnCancelAlertResultText")
    public void getAlertText() {
        new ElementActions(driver.get()).click(JS_ConfirmAlert);
        ReportManager.logDiscrete("Alert text is: [" + ElementActions.getInstance().performAlertAction().getAlertText() + "]");
        Validations.assertThat().object(ElementActions.getInstance().performAlertAction().getAlertText()).isEqualTo("I am a JS Confirm").perform();
    }

    @Test(dependsOnMethods = "getAlertText")
    public void acceptAlert() {
        new ElementActions(driver.get()).click(JS_ConfirmAlert);
        ElementActions.getInstance().performAlertAction().acceptAlert();
    }

    @Test(dependsOnMethods = "acceptAlert")
    public void assertOnConfirmAlertResultText() {
        Validations.assertThat().element(driver.get(), JS_ResultText).text().isEqualTo("You clicked: Ok").perform();
    }
}
*/
