package testPackage;

import com.shaft.cli.FileActions;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.Validations;
import io.restassured.builder.ResponseBuilder;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

public class ValidationsBuilderTests {
    By googleLogo = By.xpath("//img[@alt='Google']");

    //@Test
    public void elementValidations() {
        WebDriver driver;
        driver = new DriverFactory().getDriver();
        new BrowserActions(driver).navigateToURL("https://www.google.com/ncr", "https://www.google.com");
        Validations.assertThat()
                .element(driver, googleLogo)
                .exists()
                .withCustomReportMessage("Check that google logo exists")
                .perform();

        Validations.assertThat()
                .element(driver, googleLogo)
                .attribute("text")
                .isEqualTo("")
                .withCustomReportMessage("")
                .perform();

        Validations.verifyThat()
                .element(driver, googleLogo)
                .doesNotExist()
                .withCustomReportMessage("Check that google logo does not exist")
                .perform();

        Validations.verifyThat()
                .element(driver, googleLogo)
                .matchesReferenceImage()
                .perform();

        Validations.verifyThat()
                .element(driver, googleLogo)
                .doesNotMatchReferenceImage(ValidationEnums.VisualValidationEngine.EXACT_OPENCV)
                .perform();

        Validations.verifyThat()
                .element(driver, googleLogo)
                .cssProperty("font-family")
                .contains("arial")
                .perform();

        Validations.verifyThat()
                .element(driver, googleLogo)
                .text()
                .doesNotEqual("dummy text")
                .withCustomReportMessage("Checking to confirm that google logo text doesn't contain dummy text")
                .perform();

        Validations.verifyThat()
                .element(driver, googleLogo)
                .exists()
                .perform();

        Validations.verifyThat()
                .element(driver, googleLogo)
                .attribute("text")
                .contains("google")
                .withCustomReportMessage("checking that the text attribute contains google")
                .perform();

        driver.quit();
    }

    //@Test
    public void browserValidations() {
        WebDriver driver;
        driver = new DriverFactory().getDriver();
        new BrowserActions(driver).navigateToURL("https://www.google.com/ncr", "https://www.google.com");
        Validations.assertThat()
                .browser(driver)
                .url()
                .contains("google")
                .perform();

        driver.quit();
    }

    //@Test
    public void nativeValidations() {
        Validations.verifyThat()
                .forceFail()
                .withCustomReportMessage("check that you can force fail a verification")
                .perform();

        Validations.assertThat()
                .object("")
                .isNull()
                .perform();

        Validations.verifyThat()
                .object(null)
                .isNull()
                .perform();

        Validations.verifyThat()
                .object(false)
                .isEqualTo(false)
                .perform();

        Validations.verifyThat()
                .object(false)
                .isFalse()
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

    //@Test
    public void numberValidations() {
        Validations.assertThat()
                .number(10)
                .isEqualTo(10)
                .withCustomReportMessage("check that 10 equals 10")
                .perform();

        Validations.verifyThat()
                .number(10)
                .isGreaterThan(9)
                .withCustomReportMessage("check that 10 is greater than 9")
                .perform();

        Validations.verifyThat()
                .number(10)
                .isGreaterThanOrEquals(10)
                .perform();

        Validations.verifyThat()
                .number(10)
                .isLessThan(9)
                .perform();

        Validations.verifyThat()
                .number(10)
                .isLessThanOrEquals(10)
                .perform();

        Validations.verifyThat()
                .number(10)
                .doesNotEqual(10)
                .perform();
    }

    //@Test
    public void restValidations() {
        String referenceJsonFilePath = SHAFT.Properties.paths.testData() + "specialCharacters.json";
        Response response = (new ResponseBuilder()).setBody(FileActions.getInstance().readFile(referenceJsonFilePath))
                .setStatusCode(200).build();

        Validations.verifyThat()
                .response(response)
                .isEqualToFileContent(referenceJsonFilePath)
                .withCustomReportMessage("checking that the response is equal to the json file")
                .perform();

        Validations.verifyThat()
                .response(response)
                .extractedJsonValue("data.totalRows")
                .isEqualTo("107")
                .withCustomReportMessage("checking that data.totalRows is equal to 107")
                .perform();
    }

    //@Test
    public void fileValidations() {
        Validations.assertThat()
                .file("", "pom.xml")
                .exists()
                .withCustomReportMessage("checking that pom.xml exists")
                .perform();
    }
}
