package testPackage.legacy;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import io.restassured.builder.ResponseBuilder;
import io.restassured.response.Response;

public class ValidationsBuilderTests {

    //    @Test(expectedExceptions = AssertionError.class)
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

    //    @Test(expectedExceptions = {java.lang.AssertionError.class})
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

    //    @Test
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

    //    @Test
    public void fileValidations() {
        Validations.assertThat()
                .file("", "pom.xml")
                .exists()
                .withCustomReportMessage("checking that pom.xml exists")
                .perform();
    }
}
