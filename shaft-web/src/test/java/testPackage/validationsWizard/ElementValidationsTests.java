package testPackage.validationsWizard;

import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.Tests;

public class ElementValidationsTests extends Tests {
    private final By button = By.cssSelector("button");

    @Test
    public void exists() {
        Validations.assertThat().element(driver.get().getDriver(), button).exists().perform();
    }

    @Test
    public void matchesReferenceImage() {
        Validations.assertThat().element(driver.get().getDriver(), button).matchesReferenceImage().perform();
    }

    @Test
    public void isVisible() {
        Validations.assertThat().element(driver.get().getDriver(), button).isVisible().perform();
    }

    @Test
    public void isEnabled() {
        Validations.assertThat().element(driver.get().getDriver(), button).isEnabled().perform();
    }

    @Test
    public void isNotChecked() {
        Validations.assertThat().element(driver.get().getDriver(), button).isNotChecked().perform();
    }

    @Test
    public void isNotSelected() {
        Validations.assertThat().element(driver.get().getDriver(), button).isNotSelected().perform();
    }

    @Test
    public void attribute() {
        Validations.assertThat().element(driver.get().getDriver(), button).attribute("alt").isEqualTo("Google").perform();
    }

    @Test
    public void text() {
        Validations.assertThat().element(driver.get().getDriver(), button).text().isEqualTo("Go").perform();
    }

    @Test
    public void cssProperty() {
        Validations.assertThat().element(driver.get().getDriver(), button).cssProperty("appearance").matchesRegex("(auto|button)").perform();
    }

    @BeforeMethod
    public void navigate() {
        driver.get().browser().navigateToURL("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
    }
}
