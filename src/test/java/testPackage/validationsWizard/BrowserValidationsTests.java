package testPackage.validationsWizard;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.validation.Validations;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class BrowserValidationsTests {
    private static final ThreadLocal<SHAFT.GUI.WebDriver> driver = new ThreadLocal<>();

    @Test
    public void url() {
        Validations.assertThat().browser(driver.get().getDriver()).url().contains("google.com").perform();
    }

    @Test
    public void title() {
        Validations.assertThat().browser(driver.get().getDriver()).title().contains("oogle").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new SHAFT.GUI.WebDriver());
        String url = "https://www.google.com/";
        new BrowserActions(driver.get().getDriver()).navigateToURL(url);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
