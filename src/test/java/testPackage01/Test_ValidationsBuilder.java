package testPackage01;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.Validations;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

public class Test_ValidationsBuilder {
    By googleLogo = By.xpath("//img[@alt='Google']");

    @Test
    public void elementValidations() {
        WebDriver driver;
        driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
        Validations.assertThat(driver)
                .element(googleLogo)
                .exists()
                .withCustomReportMessage("Check that google logo exists")
                .perform();

        Assertions.assertElementAttribute(driver, By.xpath(""), "text", "Google", Assertions.AssertionComparisonType.EQUALS, Assertions.AssertionType.POSITIVE);

        Validations.assertThat(driver)
                .element(googleLogo)
                .attribute("text")
                .isEqualTo("")
                .withCustomReportMessage("")
                .perform();

        Validations.verifyThat(driver)
                .element(googleLogo)
                .doesNotExist()
                .withCustomReportMessage("Check that google logo does not exist")
                .perform();

        Validations.verifyThat(driver)
                .element(googleLogo)
                .matchesReferenceImage()
                .perform();

        Validations.verifyThat(driver)
                .element(googleLogo)
                .doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine.EXACT_OPENCV)
                .perform();

        Validations.verifyThat(driver)
                .element(googleLogo)
                .cssProperty("font-family")
                .contains("arial")
                .perform();

        Validations.verifyThat(driver)
                .element(googleLogo)
                .attribute(ValidationEnums.ElementAttribute.TEXT)
                .doesNotEqual("dummy text")
                .withCustomReportMessage("Checking to confirm that google logo text doesn't contain dummy text")
                .perform();

        Validations.verifyThat(driver)
                .element(googleLogo)
                .exists()
                .perform();

        Validations.verifyThat(driver)
                .element(googleLogo)
                .attribute("text")
                .contains("google")
                .withCustomReportMessage("checking that the text attribute contains google")
                .perform();

        DriverFactory.closeAllDrivers();
    }

    @Test
    public void browserValidations() {
        WebDriver driver;
        driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
        Validations.assertThat(driver)
                .browser()
                .attribute(ValidationEnums.BrowserAttribute.CURRENT_URL)
                .contains("google")
                .perform();

        DriverFactory.closeAllDrivers();
    }

    @Test
    public void nativeValidations() {
        Validations.verifyThat()
                .forceFail()
                .withCustomReportMessage("check that you can force fail a verification")
                .perform();

        Validations.assertThat()
                .conditionIsTrue(true)
                .withCustomReportMessage("check that true is true")
                .perform();

        Validations.verifyThat()
                .conditionIsFalse(false)
                .perform();

        Validations.verifyThat()
                .objectIsNull(1)
                .withCustomReportMessage("check that 1 is null, expected to fail")
                .perform();

        Validations.verifyThat()
                .objectIsNotNull(null)
                .perform();

        Validations.verifyThat()
                .object("123")
                .isEqualTo("123")
                .perform();

        Validations.verifyThat()
                .object("123")
                .doesNotEqual("123")
                .perform();

        Validations.verifyThat()
                .object("123")
                .doesNotContain("2")
                .perform();
    }
}
