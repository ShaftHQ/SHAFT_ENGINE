package testPackage.validationsWizard;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_BrowserValidations {
    private WebDriver driver;

    @Test
    public void url() {
        Validations.assertThat().browser(driver).url().contains("google.com").perform();
    }

    @Test
    public void title() {
        Validations.assertThat().browser(driver).title().contains("oogle").perform();
    }

    @BeforeClass
    public void beforeClass() {
        driver = DriverFactory.getDriver();
        String url = "https://www.google.com/";
        BrowserActions.getInstance().navigateToURL(url);
    }

    @AfterClass(alwaysRun = true)
    public void afterClass() {
        DriverFactory.closeAllDrivers();
    }
}
