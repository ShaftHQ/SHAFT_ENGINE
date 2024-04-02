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

public class JSAlertBoxTests {

    private static final By JS_AlertBox = By.xpath("//button[contains(text(),'Click for JS Alert')]");
    private static final By JS_ResultText = By.id("result");
    private static private static final ThreadLocal<WebDriver> driver = new ThreadLocal<>();

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
    public void getJSAlertText() {
        new ElementActions(driver.get()).click(JS_AlertBox);
        ReportManager.logDiscrete("Alert text is: [" + ElementActions.getInstance().performAlertAction().getAlertText() + "]");
        Validations.assertThat().object(ElementActions.getInstance().performAlertAction().getAlertText()).isEqualTo("I am a JS Alert").perform();
    }

    @Test(dependsOnMethods = "getJSAlertText")
    public void acceptAlert() {
        new ElementActions(driver.get()).click(JS_AlertBox);
        ElementActions.getInstance().performAlertAction().acceptAlert();
    }

    @Test(dependsOnMethods = "acceptAlert")
    public void assertOnConfirmAlertResultText() {
        Validations.assertThat().element(driver.get(), JS_ResultText).text().isEqualTo("You successfully clicked an alert").perform();
    }
}
*/
