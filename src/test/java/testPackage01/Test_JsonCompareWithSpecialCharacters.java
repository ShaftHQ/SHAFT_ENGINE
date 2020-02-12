package testPackage01;

import org.testng.annotations.Test;

import com.shaft.api.RestActions.ComparisonType;
import com.shaft.cli.FileActions;
import com.shaft.validation.Verifications;
import com.shaft.validation.Verifications.VerificationType;

import io.restassured.builder.ResponseBuilder;
import io.restassured.response.Response;

public class Test_JsonCompareWithSpecialCharacters {
    @Test
    public void specialCharacters() {
	String referenceJsonFilePath = System.getProperty("jsonFolderPath") + "specialCharacters.json";
	Response response = (new ResponseBuilder()).setBody(FileActions.readFromFile(referenceJsonFilePath))
		.setStatusCode(200).build();
	Verifications.verifyJSONFileContent(response, referenceJsonFilePath, ComparisonType.EQUALS,
		VerificationType.POSITIVE);
    }
}
