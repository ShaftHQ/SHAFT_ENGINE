package testPackage;

import com.shaft.cli.FileActions;
import com.shaft.validation.Validations;
import io.restassured.builder.ResponseBuilder;
import io.restassured.response.Response;
import org.testng.annotations.Test;

public class Test_JsonCompareWithSpecialCharacters {
    @Test
    public void specialCharacters() {
        String referenceJsonFilePath = System.getProperty("testDataFolderPath") + "specialCharacters.json";
        Response response = (new ResponseBuilder()).setBody(FileActions.getInstance().readFile(referenceJsonFilePath))
                .setStatusCode(200).build();
        Validations.verifyThat().response(response).isEqualToFileContent(referenceJsonFilePath).perform();
    }

    @Test
    public void specialCharacters_assertion() {
        String referenceJsonFilePath = System.getProperty("testDataFolderPath") + "specialCharacters.json";
        Response response = (new ResponseBuilder()).setBody(FileActions.getInstance().readFile(referenceJsonFilePath))
                .setStatusCode(200).build();
        Validations.assertThat().response(response).containsFileContent(referenceJsonFilePath)
                .withCustomReportMessage("trying out the log message")
                .perform();
    }
}
