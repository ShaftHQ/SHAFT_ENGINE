package testPackage.legacy;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.api.validation.ApiValidations;
import io.restassured.builder.ResponseBuilder;
import io.restassured.response.Response;
import org.testng.annotations.Test;

public class JsonCompareWithSpecialCharactersTests {
    @Test
    public void specialCharacters() {
        String referenceJsonFilePath = SHAFT.Properties.paths.testData() + "specialCharacters.json";
        Response response = (new ResponseBuilder()).setBody(FileActions.getInstance().readFile(referenceJsonFilePath))
                .setStatusCode(200).build();
        ApiValidations.verifyThat(response).isEqualToFileContent(referenceJsonFilePath).perform();
    }

    @Test
    public void specialCharacters_assertion() {
        String referenceJsonFilePath = SHAFT.Properties.paths.testData() + "specialCharacters.json";
        Response response = (new ResponseBuilder()).setBody(FileActions.getInstance().readFile(referenceJsonFilePath))
                .setStatusCode(200).build();
        ApiValidations.assertThat(response).containsFileContent(referenceJsonFilePath)
                .withCustomReportMessage("trying out the log message")
                .perform();
    }
}
