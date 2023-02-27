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

public class Test_JSAlertBox {

    private static final By JS_AlertBox = By.xpath("//button[contains(text(),'Click for JS Alert')]");
    private static final By JS_ResultText = By.id("result");
    private static WebDriver driver;

    @BeforeClass
    public void navigateToJSAlertPage() {
        driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver, "http://the-internet.herokuapp.com/javascript_alerts");
    }

    @AfterClass(alwaysRun = true)
    public void tearDownDriver() {
        BrowserActions.closeCurrentWindow(driver);
    }

    @Test
    public void getJSAlertText() {
        ElementActions.click(driver, JS_AlertBox);
        ReportManager.logDiscrete("Alert text is: [" + ElementActions.performAlertAction(driver).getAlertText() + "]");
        Validations.assertThat().object(ElementActions.performAlertAction(driver).getAlertText()).isEqualTo("I am a JS Alert").perform();
    }

    @Test(dependsOnMethods = "getJSAlertText")
    public void acceptAlert() {
        ElementActions.click(driver, JS_AlertBox);
        ElementActions.performAlertAction(driver).acceptAlert();
    }

    @Test(dependsOnMethods = "acceptAlert")
    public void assertOnConfirmAlertResultText() {
        Validations.assertThat().element(driver, JS_ResultText).text().isEqualTo("You successfully clicked an alert").perform();
    }
}