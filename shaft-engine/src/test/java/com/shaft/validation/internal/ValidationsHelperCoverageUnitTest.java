package com.shaft.validation.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums.ValidationCategory;
import com.shaft.validation.ValidationEnums.ValidationType;
import io.restassured.response.Response;
import io.restassured.response.ValidatableResponse;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ValidationsHelperCoverageUnitTest {

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "validateTrue should cover positive/negative pass/fail branches")
    public void validateTrueShouldCoverPositiveAndNegativeBranches() {
        ValidationsHelper helper = new ValidationsHelper(ValidationCategory.SOFT_ASSERT);

        helper.validateTrue(true, ValidationType.POSITIVE);
        Assert.assertNull(ValidationsHelper.getVerificationErrorToForceFail());

        helper.validateTrue(false, ValidationType.POSITIVE);
        Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());

        ValidationsHelper.resetVerificationStateAfterFailing();
        helper.validateTrue(false, ValidationType.NEGATIVE);
        Assert.assertNull(ValidationsHelper.getVerificationErrorToForceFail());

        helper.validateTrue(true, ValidationType.NEGATIVE);
        Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "validateFail should cover soft assertion branch")
    public void validateFailShouldCoverSoftAssertionBranch() {
        new ValidationsHelper(ValidationCategory.SOFT_ASSERT).validateFail("soft fail message");
        Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    @Test(description = "verificationSummaryText numbers every accumulated soft failure (#3516 C)")
    public void verificationSummaryTextNumbersAllSoftFailures() {
        Assert.assertNull(ValidationsHelper.verificationSummaryText(),
                "with no accumulated failures the summary must be null");
        try {
            ValidationsHelper.recordVerificationFailure("first soft failure");
            ValidationsHelper.recordVerificationFailure("second soft failure");

            String summary = ValidationsHelper.verificationSummaryText();
            Assert.assertNotNull(summary);
            Assert.assertTrue(summary.contains("2 failure(s)"), summary);
            Assert.assertTrue(summary.contains("1. first soft failure"), summary);
            Assert.assertTrue(summary.contains("2. second soft failure"), summary);
        } finally {
            ValidationsHelper.resetVerificationStateAfterFailing();
        }
    }

    @Test(description = "validateJSONFileContent should cover pass/fail and json path branches")
    public void validateJSONFileContentShouldCoverPassAndFailBranches() {
        ValidationsHelper helper = new ValidationsHelper(ValidationCategory.SOFT_ASSERT);
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

            helper.validateJSONFileContent(response, "ref.json",
                    RestActions.ComparisonType.EQUALS, "$.items", ValidationType.POSITIVE);
            Assert.assertNull(ValidationsHelper.getVerificationErrorToForceFail());

            helper.validateJSONFileContent(response, "ref.json",
                    RestActions.ComparisonType.EQUALS, "", ValidationType.POSITIVE);
            Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
            ValidationsHelper.resetVerificationStateAfterFailing();
        }
    }

    @SuppressWarnings("unchecked")
    @Test(description = "validateResponseFileSchema should cover pass/fail and json path branches")
    public void validateResponseFileSchemaShouldCoverPassAndFailBranches() throws Exception {
        ValidationsHelper helper = new ValidationsHelper(ValidationCategory.SOFT_ASSERT);
        Response response = mock(Response.class);
        ValidatableResponse validatableResponse = mock(ValidatableResponse.class);
        FileActions fileActions = mock(FileActions.class);

        Path tempSchema = Files.createTempFile("validations-helper-schema", ".json");
        Files.writeString(tempSchema, "{\"type\":\"object\"}");

        try (MockedStatic<FileActions> fileActionsMock = Mockito.mockStatic(FileActions.class);
             MockedStatic<RestActions> restActionsMock = Mockito.mockStatic(RestActions.class)) {
            when(response.then()).thenReturn(validatableResponse);
            when(validatableResponse.body(Mockito.any(org.hamcrest.Matcher.class)))
                    .thenReturn(validatableResponse)
                    .thenThrow(new AssertionError("schema mismatch"));

            fileActionsMock.when(() -> FileActions.getInstance(true)).thenReturn(fileActions);
            when(fileActions.readFile(anyString())).thenReturn("{\"type\":\"object\"}");

            restActionsMock.when(() -> RestActions.parseBodyToJson(any())).thenReturn(new ByteArrayInputStream("{}".getBytes()));
            restActionsMock.when(() -> RestActions.parseBodyToJson(any(Response.class))).thenReturn(new ByteArrayInputStream("{}".getBytes()));

            helper.validateResponseFileSchema(response, tempSchema.toString(),
                    RestActions.ComparisonType.EQUALS, "$.items", ValidationType.POSITIVE);
            Assert.assertNull(ValidationsHelper.getVerificationErrorToForceFail());

            helper.validateResponseFileSchema(response, tempSchema.toString(),
                    RestActions.ComparisonType.EQUALS, "", ValidationType.POSITIVE);
            Assert.assertNotNull(ValidationsHelper.getVerificationErrorToForceFail());
            ValidationsHelper.resetVerificationStateAfterFailing();
        } finally {
            Files.deleteIfExists(tempSchema);
        }
    }

    @Test(description = "reportValidationState should attach one primary Assertion evidence card and keep the legacy Expected/Actual attachments")
    public void reportValidationStateAttachesPrimaryEvidenceCard() {
        List<List<Object>> captured = new ArrayList<>();
        try (MockedStatic<ReportManagerHelper> reportMock = Mockito.mockStatic(ReportManagerHelper.class)) {
            reportMock.when(() -> ReportManagerHelper.attach(any()))
                    .thenAnswer(invocation -> {
                        captured.addAll(invocation.getArgument(0));
                        return null;
                    });

            ValidationsHelper.reportValidationState(ValidationCategory.SOFT_ASSERT, false,
                    "expected-alpha", "actual-beta", null);
            ValidationsHelper.resetVerificationStateAfterFailing();
        }

        Assert.assertFalse(captured.isEmpty(), "reportValidationState must attach evidence.");
        // The evidence card headlines the step: it is the first attachment and is HTML.
        List<Object> primary = captured.get(0);
        Assert.assertEquals(primary.get(0), "Assertion evidence");
        Assert.assertEquals(primary.get(1), "assertion-evidence.html");
        String cardHtml = String.valueOf(primary.get(2));
        Assert.assertTrue(cardHtml.toLowerCase().contains("<html"), "The card must be an HTML document.");
        Assert.assertTrue(cardHtml.contains("expected-alpha"), "The card must show the expected value.");
        Assert.assertTrue(cardHtml.contains("actual-beta"), "The card must show the actual value.");

        // Non-regression (issue #3502): the legacy plain-text attachments stay for consumers that
        // scrape the "Expected Value" / "Actual Value" names.
        Assert.assertTrue(captured.stream().anyMatch(attachment -> "Expected Value".equals(attachment.get(1))),
                "Legacy Expected Value attachment must remain.");
        Assert.assertTrue(captured.stream().anyMatch(attachment -> "Actual Value".equals(attachment.get(1))),
                "Legacy Actual Value attachment must remain.");
    }

    @Test(description = "reportValidationState must not attach redundant Expected/Actual text or an " +
            "Assertion evidence card when the caller already attached rich comparison evidence " +
            "(e.g. a Playwright visual-diff image) — issue #3804")
    public void reportValidationStateSkipsSupplementaryEvidenceWhenRichEvidenceAlreadyAttached() {
        List<List<Object>> captured = new ArrayList<>();
        try (MockedStatic<ReportManagerHelper> reportMock = Mockito.mockStatic(ReportManagerHelper.class)) {
            reportMock.when(() -> ReportManagerHelper.attach(any()))
                    .thenAnswer(invocation -> {
                        captured.addAll(invocation.getArgument(0));
                        return null;
                    });

            ValidationsHelper.reportValidationState(ValidationCategory.HARD_ASSERT, true,
                    true, true, new ArrayList<>(), System.currentTimeMillis(), true);
        }

        Assert.assertTrue(captured.isEmpty(),
                "No Expected/Actual text or Assertion evidence card should be attached when the "
                        + "caller already reported rich comparison evidence through another channel.");
    }
}
