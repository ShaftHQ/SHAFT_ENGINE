package testPackage01;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.Validations;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class Test_ValidationsBuilder {
    By googleLogo = By.xpath("//img[@alt='Google']");

    //@Test
    public void test() {
        Validations.verifyThat().forceFail().perform();
        Validations.verifyThat().objectsAreEqual("", "").negatively().perform();
        Validations.verifyThat().objectIsNull(null).withCustomLogMessage("no custom message here").perform();
        Validations.assertThat().objectsAreEqual("", "").perform();
    }

    //@Test
    public void numbersComparisonTest() {
        Validations.verifyThat()
                .comparativeRelationBetweenNumbers(1, 2)
                .withNumbersComparisonRelation(ValidationEnums.NumbersComparativeRelation.LESS_THAN)
                .perform();
        Validations.verifyThat()
                .comparativeRelationBetweenNumbers(1, 2)
                .withNumbersComparisonRelation(ValidationEnums.NumbersComparativeRelation.GREATER_THAN_OR_EQUALS)
                .perform();
        Validations.assertThat()
                .comparativeRelationBetweenNumbers(1, 2)
                .withNumbersComparisonRelation(ValidationEnums.NumbersComparativeRelation.EQUALS)
                .perform();
    }

    //@Test
    public void elementExistsTest() {
        WebDriver driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
        Validations.assertThat().element(driver, googleLogo)
                .exists()
                .withCustomLogMessage("Checking to confirm that the google logo image exists")
                .perform();
        Validations.verifyThat().element(driver, googleLogo)
                .exists().negatively().withCustomLogMessage("Checking to confirm that the google logo image doesn't exist").perform();
        DriverFactory.closeAllDrivers();
    }

    //@Test
    public void elementAttributeTest() {
        WebDriver driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
        Validations.verifyThat()
                .element(driver, googleLogo)
                .attributeEquals(ValidationEnums.ElementAttribute.TEXT, "")
                .withCaseInsensitiveComparison()
                .negatively()
                .perform();
        Validations.assertThat()
                .element(driver, googleLogo)
                .attributeEquals("src", "googlelogo")
                .withContainsComparison()
                .perform();
        DriverFactory.closeAllDrivers();
    }

    //@Test
    public void elementCssProperty() {
        WebDriver driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");
        Validations.verifyThat()
                .element(driver, googleLogo)
                .cssPropertyEquals("font-family", "arial")
                .withContainsComparison()
                .negatively()
                .perform();
        Validations.assertThat()
                .element(driver, googleLogo)
                .cssPropertyEquals("font-family", "Arial, Sans-Serif")
                .withCaseInsensitiveComparison()
                .perform();
        DriverFactory.closeAllDrivers();
    }

    //@Test
    public void browserAttribute() {
        WebDriver driver = DriverFactory.getDriver();
        BrowserActions.navigateToURL(driver, "https://www.google.com/ncr", "https://www.google.com");

        Validations.verifyThat()
                .browser(driver)
                .attributeEquals("url", "google")
                .withContainsComparison()
                .negatively()
                .perform();

        Validations.assertThat()
                .browser(driver)
                .attributeEquals(ValidationEnums.BrowserAttribute.CURRENT_URL, ".*google.*")
                .withRegexMatchingComparison()
                .perform();

        DriverFactory.closeAllDrivers();
    }

    //@Test
    public void jsonTest() {
        Response response = null;
        Validations.assertThat().json(response)
                .responseEqualsFileContent("relative file path")
                .withContainsComparison()
                .perform();

        Validations.verifyThat().json(response)
                .jsonPathValueEquals("", "")
                .negatively()
                .perform();
    }
}
