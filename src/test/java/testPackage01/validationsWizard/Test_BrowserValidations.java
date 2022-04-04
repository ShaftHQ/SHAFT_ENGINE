package testPackage01.validationsWizard;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_BrowserValidations {
    private WebDriver driver;
    private String url = "https://www.google.com/";

    @Test
    public void url() {
        Validations.assertThat().browser(driver).url().contains("google.com").perform();
    }

    @Test
    public void title() {
        Validations.assertThat().browser(driver).title().isEqualTo("Google").perform();
    }

    @BeforeClass
    public void beforeClass() {
        driver = DriverFactory.getDriver();
        new SHAFT.GUI.WebDriver().browser().navigateToURL(url);
    }

    @AfterClass
    public void afterClass() {
        DriverFactory.closeAllDrivers();
    }
}
