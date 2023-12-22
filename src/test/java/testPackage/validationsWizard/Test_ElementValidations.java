package testPackage.validationsWizard;

import com.shaft.driver.DriverFactory;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
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
        Validations.assertThat().element(driver.get(), button).cssProperty("appearance").matchesRegex("(auto|button)").perform();
    }

    @BeforeMethod
    public void beforeMethod() {
        driver.set(new DriverFactory().getDriver());
        driver.get().navigate().to("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
    }

    @AfterMethod(alwaysRun = true)
    public void afterMethod() {
        driver.get().quit();
    }
}
