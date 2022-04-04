package testPackage01.validationsWizard;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_ElementValidations {
    private WebDriver driver;
    private String url = "https://www.google.com/";
    private By googleLogo = By.xpath("//img[@alt='Google']");
    private By searchBox = By.name("q");

    @Test
    public void exists() {
        Validations.assertThat().element(driver, googleLogo).exists().perform();
    }

    @Test
    public void matchesReferenceImage() {
        Validations.assertThat().element(driver, googleLogo).matchesReferenceImage().perform();
    }

    @Test
    public void isVisible() {
        Validations.assertThat().element(driver, googleLogo).isVisible().perform();
    }

    @Test
    public void isEnabled() {
        Validations.assertThat().element(driver, googleLogo).isEnabled().perform();
    }

    @Test
    public void isNotChecked() {
        Validations.assertThat().element(driver, googleLogo).isNotChecked().perform();
    }

    @Test
    public void isNotSelected() {
        Validations.assertThat().element(driver, googleLogo).isNotSelected().perform();
    }

    @Test
    public void attribute() {
        Validations.assertThat().element(driver, googleLogo).attribute("alt").isEqualTo("Google").perform();
    }

    @Test
    public void text() {
        new SHAFT.GUI.WebDriver().element().type(searchBox, "DriverFactory_Engine");
        Validations.assertThat().element(driver, searchBox).text().isEqualTo("DriverFactory_Engine").perform();
    }

    @Test
    public void cssProperty() {
        Validations.assertThat().element(driver, googleLogo).cssProperty("max-width").isEqualTo("100%").perform();
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
