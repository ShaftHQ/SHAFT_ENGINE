package testPackage.validationsWizard;

import com.shaft.validation.WebValidations;
import org.openqa.selenium.By;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;
import testPackage.Tests;

public class ElementValidationsTests extends Tests {
    private final By button = By.cssSelector("button");

    @Test
    public void exists() {
        WebValidations.assertThat().element(driver.get().getDriver(), button).exists().perform();
    }

    @Test
    public void matchesReferenceImage() {
        WebValidations.assertThat().element(driver.get().getDriver(), button).matchesReferenceImage().perform();
    }

    @Test
    public void isVisible() {
        WebValidations.assertThat().element(driver.get().getDriver(), button).isVisible().perform();
    }

    @Test
    public void isEnabled() {
        WebValidations.assertThat().element(driver.get().getDriver(), button).isEnabled().perform();
    }

    @Test
    public void isNotChecked() {
        WebValidations.assertThat().element(driver.get().getDriver(), button).isNotChecked().perform();
    }

    @Test
    public void isNotSelected() {
        WebValidations.assertThat().element(driver.get().getDriver(), button).isNotSelected().perform();
    }

    @Test
    public void attribute() {
        WebValidations.assertThat().element(driver.get().getDriver(), button).attribute("alt").isEqualTo("Google").perform();
    }

    @Test
    public void text() {
        WebValidations.assertThat().element(driver.get().getDriver(), button).text().isEqualTo("Go").perform();
    }

    @Test
    public void cssProperty() {
        WebValidations.assertThat().element(driver.get().getDriver(), button).cssProperty("appearance").matchesRegex("(auto|button)").perform();
    }

    @BeforeMethod
    public void navigate() {
        driver.get().browser().navigateToURL("data:text/html,<script>var result;</script><button alt='Google' onclick='result=\"Clicked\"'>Go</button>");
    }
}
