package testPackage01.validationsWizard;

import com.shaft.driver.DriverFactory;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

public class Test_ElementValidations {
    private final ThreadLocal<WebDriver> driver = new ThreadLocal<>();
    private final By button = By.cssSelector("button");

    @Test
    public void exists() {
        Validations.assertThat().element(driver.get(), button).exists().perform();
    }

    @Test
    public void matchesReferenceImage() {
        Validations.assertThat().element(driver.get(), button).matchesReferenceImage().perform();
    }

    @Test
    public void isVisible() {
        Validations.assertThat().element(driver.get(), button).isVisible().perform();
    }

    @Test
    public void isEnabled() {
        Validations.assertThat().element(driver.get(), button).isEnabled().perform();
    }

    @Test
    public void isNotChecked() {
        Validations.assertThat().element(driver.get(), button).isNotChecked().perform();
    }

    @Test
    public void isNotSelected() {
        Validations.assertThat().element(driver.get(), button).isNotSelected().perform();
    }

    @Test
    public void attribute() {
        Validations.assertThat().element(driver.get(), button).attribute("alt").isEqualTo("Google").perform();
    }

    @Test
    public void text() {
        Validations.assertThat().element(driver.get(), button).text().isEqualTo("Go").perform();
    }

    @Test
    public void cssProperty() {
        if (!System.getProperty("targetOperatingSystem").toLowerCase().contains("mac")) {
            Validations.assertThat().element(driver.get(), button).cssProperty("appearance").isEqualTo("auto").perform();
        } else{
            // the property is parsed differently on mac
            Validations.assertThat().element(driver.get(), button).cssProperty("appearance").isEqualTo("button").perform();
        }
    }

    @BeforeClass
    public void beforeClass() {
        driver.set(DriverFactory.getDriver());
        driver.get().navigate().to("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
    }

    @AfterClass
    public void afterClass() {
        DriverFactory.closeAllDrivers();
    }
}
