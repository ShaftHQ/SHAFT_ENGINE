package com.shaft.validation.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.validation.ValidationEnums.ValidationCategory;
import com.shaft.validation.ValidationEnums.ValidationType;
import io.restassured.response.ExtractableResponse;
import io.restassured.response.Response;
import io.restassured.response.ValidatableResponse;
import org.mockito.Answers;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ValidationsHelperCoverageUnitTest {
    private final ValidationsHelper helper = new ValidationsHelper();

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "validateTrue should cover positive/negative pass/fail branches")
    public void validateTrueShouldCoverPositiveAndNegativeBranches() {
        helper.validateTrue(ValidationCategory.SOFT_ASSERT, true, ValidationType.POSITIVE, "positive pass");
        Assert.assertNull(ValidationsHelper.getVerificationErrorToForceFail());

        helper.validateTrue(ValidationCategory.SOFT_ASSERT, false, ValidationType.POSITIVE, "positive fail");
        Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());

        ValidationsHelper.resetVerificationStateAfterFailing();
        helper.validateTrue(ValidationCategory.SOFT_ASSERT, false, ValidationType.NEGATIVE, "negative pass");
        Assert.assertNull(ValidationsHelper.getVerificationErrorToForceFail());

        helper.validateTrue(ValidationCategory.SOFT_ASSERT, true, ValidationType.NEGATIVE, "negative fail");
        Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "validateFail should cover soft assertion branch")
    public void validateFailShouldCoverSoftAssertionBranch() {
        helper.validateFail(ValidationCategory.SOFT_ASSERT, "soft fail message");
        Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "validateJSONFileContent should cover pass/fail and json path branches")
    public void validateJSONFileContentShouldCoverPassAndFailBranches() {
        Response response = mock(Response.class);
        FileActions fileActions = mock(FileActions.class);

        try (MockedStatic<FileActions> fileActionsMock = Mockito.mockStatic(FileActions.class);
             MockedStatic<RestActions> restActionsMock = Mockito.mockStatic(RestActions.class)) {
            fileActionsMock.when(() -> FileActions.getInstance(true)).thenReturn(fileActions);
            when(fileActions.readFile(anyString())).thenReturn("{\"key\":\"value\"}");

            restActionsMock.when(() -> RestActions.compareJSON(any(Response.class), anyString(), any(RestActions.ComparisonType.class), anyString()))
                    .thenReturn(true, false);
            restActionsMock.when(() -> RestActions.parseBodyToJson(any())).thenReturn(new ByteArrayInputStream("{}".getBytes()));
            restActionsMock.when(() -> RestActions.parseBodyToJson(any(Response.class))).thenReturn(new ByteArrayInputStream("{}".getBytes()));

            helper.validateJSONFileContent(ValidationCategory.SOFT_ASSERT, response, "ref.json",
                    RestActions.ComparisonType.EQUALS, "$.items", ValidationType.POSITIVE, "json pass");
            Assert.assertNull(ValidationsHelper.getVerificationErrorToForceFail());

            helper.validateJSONFileContent(ValidationCategory.SOFT_ASSERT, response, "ref.json",
                    RestActions.ComparisonType.EQUALS, "", ValidationType.POSITIVE, "json fail");
            Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
            ValidationsHelper.resetVerificationStateAfterFailing();
        }
    }

    @SuppressWarnings("unchecked")
    @Test(description = "validateResponseFileSchema should cover pass/fail and json path branches")
    public void validateResponseFileSchemaShouldCoverPassAndFailBranches() throws Exception {
        Response response = mock(Response.class);
        Response differentResponse = mock(Response.class);
        ValidatableResponse validatableResponse = mock(ValidatableResponse.class, Answers.RETURNS_SELF);
        ExtractableResponse<Response> extractableResponse = mock(ExtractableResponse.class);
        FileActions fileActions = mock(FileActions.class);

        Path tempSchema = Files.createTempFile("validations-helper-schema", ".json");
        Files.writeString(tempSchema, "{\"type\":\"object\"}");

        try (MockedStatic<FileActions> fileActionsMock = Mockito.mockStatic(FileActions.class);
             MockedStatic<RestActions> restActionsMock = Mockito.mockStatic(RestActions.class)) {
            when(response.then()).thenReturn(validatableResponse);
            when(validatableResponse.extract()).thenReturn(extractableResponse);
            when(extractableResponse.response()).thenReturn(response, differentResponse);

            fileActionsMock.when(() -> FileActions.getInstance(true)).thenReturn(fileActions);
            when(fileActions.readFile(anyString())).thenReturn("{\"type\":\"object\"}");

            restActionsMock.when(() -> RestActions.parseBodyToJson(any())).thenReturn(new ByteArrayInputStream("{}".getBytes()));
            restActionsMock.when(() -> RestActions.parseBodyToJson(any(Response.class))).thenReturn(new ByteArrayInputStream("{}".getBytes()));

            helper.validateResponseFileSchema(ValidationCategory.SOFT_ASSERT, response, tempSchema.toString(),
                    RestActions.ComparisonType.EQUALS, "$.items", ValidationType.POSITIVE, "schema pass");
            Assert.assertNull(ValidationsHelper.getVerificationErrorToForceFail());

            helper.validateResponseFileSchema(ValidationCategory.SOFT_ASSERT, response, tempSchema.toString(),
                    RestActions.ComparisonType.EQUALS, "", ValidationType.POSITIVE, "schema fail");
            Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
            ValidationsHelper.resetVerificationStateAfterFailing();
        } finally {
            Files.deleteIfExists(tempSchema);
        }
    }
}
