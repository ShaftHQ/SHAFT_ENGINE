package testPackage.unitTests;

import com.shaft.cli.FileActions;
import com.shaft.cli.TerminalActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.Validations;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.internal.ValidationsBuilder;
import com.shaft.validation.internal.ValidationsExecutor;
import com.shaft.validation.internal.WebDriverValidationsExecutor;
import com.shaft.validation.internal.ValidationsHelper;
import io.restassured.builder.ResponseBuilder;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;

public class ValidationsExecutorCoverageUnitTest {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "Covers native, number, file, force-fail, and custom-message execution paths")
    public void shouldCoverNativeNumberAndFileExecutionPaths() {
        String folder = SHAFT.Properties.paths.testData();
        String fileName = "post1Response.json";

        Validations.verifyThat().forceFail("Coverage soft force fail");
        ValidationsHelper.resetVerificationStateAfterFailing();
        Validations.verifyThat().object("alpha").isEqualTo("alpha").perform();
        Validations.verifyThat().number(10).isGreaterThan(9).perform();
        Validations.verifyThat().file(folder, fileName).exists().perform();
        Validations.verifyThat().file(folder, fileName).content().contains("userId").perform();

        String checksum = FileActions.getInstance(true).getFileChecksum(new TerminalActions(), folder, fileName);
        Validations.verifyThat().file(folder, fileName).checksum().isEqualTo(checksum).perform();
    }

    @Test(description = "Covers response and JSON-based execution paths")
    public void shouldCoverRestExecutionPaths() throws Exception {
        String filePath = SHAFT.Properties.paths.testData() + "post1Response.json";
        Response response = new ResponseBuilder()
                .setBody(FileActions.getInstance().readFile(filePath))
                .setStatusCode(200)
                .build();

        Validations.verifyThat().response(response).isEqualToFileContent("post1Response.json").perform();
        Validations.verifyThat().response(response).extractedJsonValue("id").isEqualTo("1").perform();
        Validations.verifyThat().response(response).extractedJsonValueAsList("$..id").isEqualTo("1").perform();
        Validations.verifyThat().response(response).body().contains("sunt aut facere").perform();
        Validations.verifyThat().response(response).time().isLessThanOrEquals(0).perform();
        Path schemaFile = Files.createTempFile("post1-schema-", ".json");
        Files.writeString(schemaFile, """
                {
                  "$schema":"http://json-schema.org/draft-07/schema#",
                  "type":"object",
                  "required":["userId","id","title","body"],
                  "properties":{
                    "userId":{"type":"integer"},
                    "id":{"type":"integer"},
                    "title":{"type":"string"},
                    "body":{"type":"string"}
                  }
                }
                """);
        Validations.verifyThat().response(response).matchesSchema(schemaFile.toAbsolutePath().toString()).perform();
    }

    @Test(description = "Covers condition/default branches and constructor paths not reached through fluent APIs")
    public void shouldCoverConditionDefaultAndConstructors() throws Exception {
        ValidationsBuilder conditionBuilder = new ValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT);
        setBuilderField(conditionBuilder, "validationMethod", "conditionIsTrue");
        setBuilderField(conditionBuilder, "validationType", ValidationEnums.ValidationType.POSITIVE);
        setBuilderField(conditionBuilder, "condition", true);

        ValidationsExecutor conditionExecutor = new ValidationsExecutor(conditionBuilder);
        invokeInternalPerform(conditionExecutor);

        ValidationsBuilder defaultBuilder = new ValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT);
        setBuilderField(defaultBuilder, "validationMethod", "unsupportedMethodForCoverage");
        ValidationsExecutor defaultExecutor = new ValidationsExecutor(defaultBuilder);
        invokeInternalPerform(defaultExecutor);

        WebDriver mockedDriver = org.mockito.Mockito.mock(WebDriver.class);
        var webBuilder = new ValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT)
                .element(mockedDriver, By.id("coverage-id"));
        Assert.assertNotNull(new WebDriverValidationsExecutor(webBuilder));
    }

    private static void setBuilderField(Object target, String fieldName, Object value) throws Exception {
        var field = ValidationsBuilder.class.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(target, value);
    }

    private static void invokeInternalPerform(ValidationsExecutor executor) throws Exception {
        var internalPerform = ValidationsExecutor.class.getDeclaredMethod("internalPerform");
        internalPerform.setAccessible(true);
        internalPerform.invoke(executor);
    }
}
