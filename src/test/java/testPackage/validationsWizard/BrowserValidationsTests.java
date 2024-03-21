package testPackage.validationsWizard;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

public class BrowserValidationsTests {
    private static final ThreadLocal<WebDriver> driver = new ThreadLocal<>();

    @Test
    public void url() {
        Validations.assertThat().browser(driver.get()).url().contains("google.com").perform();
    }

    @Test
    public void title() {
        Validations.assertThat().browser(driver.get()).title().contains("oogle").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new DriverFactory().getDriver());
        String url = "https://www.google.com/";
        new BrowserActions(driver.get()).navigateToURL(url);
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
