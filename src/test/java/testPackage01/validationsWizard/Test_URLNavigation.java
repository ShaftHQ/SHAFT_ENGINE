package testPackage01.validationsWizard;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.validation.Validations;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_URLNavigation {
    private WebDriver driver;

    @Test
    public void testURLPathWithTheAbbreviation() {
        String URL = "./elements";
        BrowserActions.navigateToURL(driver, URL);
        Validations.assertThat().object(driver.getCurrentUrl()).equalsIgnoringCaseSensitivity("https://demoqa.com/elements");
    }

    @Test
    public void testURLPathWithoutTheAbbreviation() {
        String URL = "https://demoqa.com/elements";
        BrowserActions.navigateToURL(driver, URL);
        Validations.assertThat().object(driver.getCurrentUrl()).equalsIgnoringCaseSensitivity("https://demoqa.com/elements");
    }

    @BeforeClass
    public void beforeClass() {
        driver = DriverFactory.getDriver();
    }

    @AfterClass
    public void afterClass() {
        DriverFactory.closeAllDrivers();
    }
}
