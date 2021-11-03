package testPackage01;

import com.shaft.api.RestActions.ComparisonType;
import com.shaft.cli.FileActions;
import com.shaft.validation.Assertions;
import com.shaft.validation.Verifications;
import com.shaft.validation.Verifications.VerificationType;
import io.restassured.builder.ResponseBuilder;
import io.restassured.response.Response;
import org.testng.annotations.Test;

public class Test_JsonCompareWithSpecialCharacters {
    @Test
    public void specialCharacters() {
        String referenceJsonFilePath = System.getProperty("testDataFolderPath") + "specialCharacters.json";
        Response response = (new ResponseBuilder()).setBody(FileActions.readFromFile(referenceJsonFilePath))
                .setStatusCode(200).build();
        Verifications.verifyJSONFileContent(response, referenceJsonFilePath, ComparisonType.EQUALS,
                VerificationType.POSITIVE);
    }

    @Test
    public void specialCharacters_assertion() {
        String referenceJsonFilePath = System.getProperty("testDataFolderPath") + "specialCharacters.json";
        Response response = (new ResponseBuilder()).setBody(FileActions.readFromFile(referenceJsonFilePath))
                .setStatusCode(200).build();
        Assertions.assertJSONFileContent(response, referenceJsonFilePath, ComparisonType.CONTAINS, Assertions.AssertionType.POSITIVE, "trying out the log message");
    }
}
