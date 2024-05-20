package testPackage.resettingCapabilitiesIssue;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class ShadowDOMTests {
    private static final ThreadLocal<WebDriver> driver = new ThreadLocal<>();

    @Test
    public void shadowDom() {
        String targetText = "Typing into SHADOW DOM...";
        new BrowserActions(driver.get()).navigateToURL("https://mdn.github.io/web-components-examples/popup-info-box-web-component/");
        new ElementActions(driver.get()).type(By.id("cvc"), targetText);
        Validations.assertThat().element(driver.get(), By.id("cvc")).text().isEqualTo(targetText).perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new DriverFactory().getDriver());
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        new BrowserActions(driver.get()).closeCurrentWindow();
    }
}
