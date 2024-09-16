package com.shaft.validation.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums.*;
import io.restassured.response.Response;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.remote.Browser;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchema;

public class ValidationsHelper {
    //TODO: implement element attribute and element exists validations for sikuli actions
    static final ThreadLocal<ArrayList<String>> optionalCustomLogMessage = new ThreadLocal<>();
    private static final Boolean discreetLoggingState = SHAFT.Properties.reporting.alwaysLogDiscreetly();
    private static final String WHEN_TO_TAKE_PAGE_SOURCE_SNAPSHOT = SHAFT.Properties.visuals.whenToTakePageSourceSnapshot().trim();
    protected static List<String> verificationFailuresList = new ArrayList<>();
    protected static AssertionError verificationError = null;
    private static By lastUsedElementLocator = null;
    private final ElementActionsHelper elementActionsHelper;

    ValidationsHelper() {
        this.elementActionsHelper = new ElementActionsHelper(false);
    }

    public static AssertionError getVerificationErrorToForceFail() {
        return verificationError;
    }

    public static void resetVerificationStateAfterFailing() {
        verificationFailuresList = new ArrayList<>();
        verificationError = null;
    }

    private static void reportValidationState(WebDriver driver, ValidationCategory validationCategory, String expectedValue, String actualValue,
                                              Object validationComparisonOrComparativeRelationType, ValidationType validationType,
                                              ValidationState validationState, Throwable failureReason, List<List<Object>> externalAttachments) {
        // prepare message and attachments
        StringBuilder message = new StringBuilder();
        List<List<Object>> attachments = new ArrayList<>();
        if (externalAttachments != null && !externalAttachments.isEmpty()) {
            attachments.addAll(externalAttachments);
        }
        var stacktrace = (new Throwable()).getStackTrace();
        // get validation method name
        String validationMethodName = stacktrace[2].getMethodName();
        if (validationMethodName.contains("reportValidationResult")) {
            validationMethodName = stacktrace[3].getMethodName();
        }
        String validationTypeString = "Assertion";
        if (validationCategory.equals(ValidationCategory.SOFT_ASSERT)) {
            validationTypeString = "Verification";
        }
        validationMethodName = validationMethodName.substring(0, 1).toUpperCase() + validationMethodName.substring(1);
        message.append(validationTypeString).append(" \"").append(validationMethodName).append("\" ");
        if (validationMethodName.equals("ValidateFail")) {
            //validationState = ValidationState.PASSED;
            message.append(validationState).append(". ");
            message.append("Successfully force failed the test.");
        } else {
            message.append(validationState).append(". ");
            // prepare expected/actual results as an attachment or in the message
            boolean isExpectedOrActualValueLong = isExpectedOrActualValueLong(expectedValue, actualValue);
            if (Boolean.TRUE.equals(isExpectedOrActualValueLong)) {
                List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Expected Value",
                        expectedValue);
                List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Value", actualValue);
                attachments.add(expectedValueAttachment);
                attachments.add(actualValueAttachment);
                message.append("Expected and Actual values are attached.");
            } else {
                message.append("Expected \"").append(expectedValue).append("\" and Actual \"").append(actualValue).append("\".");
            }
            if (validationComparisonOrComparativeRelationType != null) {
                message.append(" Comparison Type \"").append(validationComparisonOrComparativeRelationType).append("\".");
            }
            if (validationType != null) {
                message.append(" Validation Type \"").append(validationType).append("\".");
            }
        }
        if (driver != null) {
            // create a screenshot attachment if needed for webdriver
            attachments.add(new ScreenshotManager().takeScreenshot(driver, lastUsedElementLocator,
                    validationMethodName, validationState.getValue()));
            // reset lastUsed variables
            lastUsedElementLocator = null;
            //}
        }
        if (driver != null && !WHEN_TO_TAKE_PAGE_SOURCE_SNAPSHOT.equalsIgnoreCase("Never")) {
            if ((WHEN_TO_TAKE_PAGE_SOURCE_SNAPSHOT.equalsIgnoreCase("Always") || WHEN_TO_TAKE_PAGE_SOURCE_SNAPSHOT.equalsIgnoreCase("ValidationPointsOnly"))
                    || (Boolean.FALSE.equals(validationState.getValue()) && WHEN_TO_TAKE_PAGE_SOURCE_SNAPSHOT.equalsIgnoreCase("FailuresOnly"))) {
                var logMessage = "";
                var pageSnapshot = new BrowserActionsHelper(true).capturePageSnapshot(driver);
                if (pageSnapshot.startsWith("From: <Saved by Blink>")) {
                    logMessage = "page snapshot";
                } else if (pageSnapshot.startsWith("<html")) {
                    logMessage = "page HTML";
                }
                List<Object> sourceAttachment = Arrays.asList(validationMethodName, logMessage, pageSnapshot);
                attachments.add(sourceAttachment);
            }
        }
        // attach failure reason
        if (failureReason != null) {
            List<Object> failureReasonAttachment = Arrays.asList("Validation Test Data", "Failure Reason",
                    ReportManagerHelper.formatStackTraceToLogEntry(failureReason));
            attachments.add(failureReasonAttachment);
        } else if (Boolean.FALSE.equals(validationState.getValue())) {
            List<Object> failureReasonAttachment = Arrays.asList("Validation Test Data", "Failure Reason",
                    ReportManagerHelper.formatStackTraceToLogEntry(new AssertionError(message)));
            attachments.add(failureReasonAttachment);
        }
        // create the log entry with or without attachments
        if (!attachments.isEmpty()) {
            ReportManagerHelper.logNestedSteps(message.toString(), optionalCustomLogMessage.get(), attachments);
        } else {
            ReportManagerHelper.logNestedSteps(message.toString(), optionalCustomLogMessage.get(), null);
        }
        // handling changes as per validationCategory hard/soft
        switch (validationCategory) {
            case HARD_ASSERT -> {
                // set test state in case of failure
                if (!validationState.getValue()) {
                    if (failureReason != null) {
                        FailureReporter.fail(ValidationsHelper.class, message.toString(), failureReason);
                    } else {
                        FailureReporter.fail(message.toString());
                    }
                }
            }
            case SOFT_ASSERT -> {
                // set test state in case of failure
                if (!validationState.getValue()) {
                    verificationFailuresList.add(message.toString());
                    verificationError = new AssertionError(String.join("\nAND ", verificationFailuresList));
                }
            }
            default -> {
            }
        }
    }

    private static void pass(WebDriver driver, ValidationCategory validationCategory, String expectedValue, String actualValue,
                             Object validationComparisonType, ValidationType validationType, List<List<Object>> externalAttachments) {
        reportValidationState(driver, validationCategory, expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.PASSED, null, externalAttachments);
    }

    private static void pass(WebDriver driver, ValidationCategory validationCategory, String expectedValue, String actualValue,
                             Object validationComparisonType, ValidationType validationType) {
        reportValidationState(driver, validationCategory, expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.PASSED, null, null);
    }

    private static void pass(ValidationCategory validationCategory, Number expectedValue, Number actualValue,
                             Object comparativeRelationType, ValidationType validationType) {
        reportValidationState(null, validationCategory, String.valueOf(expectedValue), String.valueOf(actualValue), comparativeRelationType, validationType,
                ValidationState.PASSED, null, null);
    }

    private static void fail(WebDriver driver, ValidationCategory validationCategory, String expectedValue, String actualValue,
                             Object validationComparisonType, ValidationType validationType, @SuppressWarnings("SameParameterValue") Throwable failureReason, List<List<Object>> externalAttachments) {
        // reset state in case of failure to force reporting the failure
        ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        reportValidationState(driver, validationCategory, expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.FAILED, failureReason, externalAttachments);
    }

    private static void fail(WebDriver driver, ValidationCategory validationCategory, String expectedValue, String actualValue,
                             Object validationComparisonType, ValidationType validationType, Throwable failureReason) {
        // reset state in case of failure to force reporting the failure
        ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        reportValidationState(driver, validationCategory, expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.FAILED, failureReason, null);
    }

    private static void fail(ValidationCategory validationCategory, Number expectedValue, Number actualValue,
                             Object comparativeRelationType, ValidationType validationType) {
        // reset state in case of failure to force reporting the failure
        ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        reportValidationState(null, validationCategory, String.valueOf(expectedValue), String.valueOf(actualValue), comparativeRelationType, validationType,
                ValidationState.FAILED, null, null);
    }

    private static void reportValidationResultOfBrowserAttribute(WebDriver driver, Object[] args) {
        String[] expectedAttributeStates = (String[]) args[0];
        String propertySeparator = (String) args[1];
        String attributeClosure = (String) args[2];
        int comparisonResult = (int) args[3];
        String propertyName = (String) args[5];
        String expectedValue = (String) args[6];
        String actualValue = (String) args[7];
        ValidationComparisonType validationComparisonType = (ValidationComparisonType) args[8];
        ValidationType validationType = (ValidationType) args[9];
        ValidationCategory validationCategory = (ValidationCategory) args[10];
        if (validationType.getValue()) {
            // expecting element attribute to have the correct value
            if (comparisonResult == 1) {
                // match
                pass(driver, validationCategory, expectedAttributeStates[0] + " '" + expectedValue + propertySeparator + propertyName
                                + attributeClosure, actualValue, validationComparisonType,
                        validationType);
            } else {
                // no match, or unhandled issue
                fail(driver, validationCategory, expectedAttributeStates[0] + " '" + expectedValue + propertySeparator + propertyName
                                + attributeClosure, actualValue, validationComparisonType,
                        validationType, null);
            }
        } else {
            // expecting element attribute to not have the correct value
            if (comparisonResult == 1) {
                // match
                pass(driver, validationCategory, expectedAttributeStates[1] + " '" + expectedValue + propertySeparator + propertyName
                                + attributeClosure, actualValue, validationComparisonType,
                        validationType);
            } else {
                // no match, or unhandled issue
                fail(driver, validationCategory, expectedAttributeStates[1] + " '" + expectedValue + propertySeparator + propertyName
                                + attributeClosure, actualValue, validationComparisonType,
                        validationType, null);
            }
        }
    }

    static boolean isExpectedOrActualValueLong(String expectedValue, String actualValue) {
        boolean isExpectedOrActualValueLong = false;
        if (actualValue == null && expectedValue != null) {
            isExpectedOrActualValueLong = expectedValue.length() >= 500;
        } else if (actualValue != null && expectedValue == null) {
            isExpectedOrActualValueLong = actualValue.length() >= 500;
        } else if (actualValue != null) {
            isExpectedOrActualValueLong = expectedValue.length() >= 500 || actualValue.length() >= 500;
        }
        return isExpectedOrActualValueLong;
    }

    private static void processCustomLogMessage(String... optionalCustomLogMessage) {
        ValidationsHelper.optionalCustomLogMessage.set(new ArrayList<>());
        for (String customMessage : optionalCustomLogMessage) {
            if (customMessage != null && !customMessage.isBlank()) {
                ValidationsHelper.optionalCustomLogMessage.get().add(customMessage);
                //ReportManager.log(customMessage + "...");
            }
        }
    }

    protected void validateFail(ValidationCategory validationCategory, String customReportMessage) {
        processCustomLogMessage(customReportMessage);
        fail(null, validationCategory, null, null, null, null, null);
    }

    protected void validateTrue(ValidationCategory validationCategory, Boolean conditionalStatement, ValidationType validationType, String customReportMessage) {
        processCustomLogMessage(customReportMessage);
        Boolean expectedValue = false;
        if (ValidationType.POSITIVE.equals(validationType)) {
            expectedValue = true;
        }
        if ((expectedValue && conditionalStatement) || (!expectedValue && !conditionalStatement)) {
            pass(null, validationCategory, String.valueOf(expectedValue).toUpperCase(), String.valueOf(conditionalStatement).toUpperCase(), null, validationType);
        } else {
            fail(null, validationCategory, String.valueOf(expectedValue).toUpperCase(), String.valueOf(conditionalStatement).toUpperCase(), null, validationType, null);
        }
    }

    protected void validateFileExists(ValidationCategory validationCategory, String fileFolderName, String fileName, @SuppressWarnings("SameParameterValue") int numberOfRetries,
                                      ValidationType validationType, String customReportMessage) {
        processCustomLogMessage(customReportMessage);
        boolean expectedValue = ValidationType.POSITIVE.equals(validationType);
        boolean actualValue = FileActions.getInstance(true).doesFileExist(fileFolderName, fileName, numberOfRetries);
        String filePrefix = "File '";
        String[] expectedAttributeStates = {"' should exist, after up to '", "' should not exist, after up to '"};
        String numberOfRetriesPostfix = "' retries";
        String reportedExpectedValue = filePrefix + fileFolderName + fileName + expectedAttributeStates[0] + numberOfRetries + numberOfRetriesPostfix;
        if (!expectedValue) {
            reportedExpectedValue = filePrefix + fileFolderName + fileName + expectedAttributeStates[1] + numberOfRetries + numberOfRetriesPostfix;
        }
        String reportedActualValue = "File exists";
        if (!actualValue) {
            reportedActualValue = "File does not exist";
        }
        if ((expectedValue && actualValue) || (!expectedValue && !actualValue)) {
            pass(null, validationCategory, reportedExpectedValue, reportedActualValue, null, validationType);
        } else {
            fail(null, validationCategory, reportedExpectedValue, reportedActualValue, null, validationType, null);
        }
    }

    protected void validateJSONFileContent(ValidationCategory validationCategory, Response response, String referenceJsonFilePath,
                                           RestActions.ComparisonType comparisonType, @SuppressWarnings("SameParameterValue") String jsonPathToTargetArray, ValidationType validationType, String customReportMessage) {
        processCustomLogMessage(customReportMessage);
        boolean expectedValue = ValidationType.POSITIVE.equals(validationType);
        StringBuilder reportedExpectedValue = new StringBuilder();
        reportedExpectedValue.append("Response data should ");
        if (!expectedValue) {
            reportedExpectedValue.append("not ");
        }
        reportedExpectedValue.append("match the JSON File in this path '").append(referenceJsonFilePath).append("'");
        if (!jsonPathToTargetArray.isBlank()) {
            reportedExpectedValue.append(", with path to Target Array '").append(jsonPathToTargetArray).append("'");
        }

        Boolean comparisonResult = RestActions.compareJSON(response, referenceJsonFilePath, comparisonType,
                jsonPathToTargetArray);
        // prepare attachments
        List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Expected JSON Value",
                RestActions.parseBodyToJson(FileActions.getInstance(true).readFile(referenceJsonFilePath)));
        List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual JSON Value",
                RestActions.parseBodyToJson(response));
        List<List<Object>> attachments = new ArrayList<>();
        attachments.add(expectedValueAttachment);
        attachments.add(actualValueAttachment);

        if ((comparisonResult && expectedValue) || (!comparisonResult && !expectedValue)) {
            pass(null, validationCategory, reportedExpectedValue.toString(), String.valueOf(comparisonResult).toUpperCase(), comparisonType, validationType, attachments);
        } else {
            fail(null, validationCategory, reportedExpectedValue.toString(), String.valueOf(comparisonResult).toUpperCase(), comparisonType, validationType, null, attachments);
        }
    }

    protected void validateResponseFileSchema(ValidationCategory validationCategory, Response response, String referenceJsonFilePath,
                                              RestActions.ComparisonType comparisonType, @SuppressWarnings("SameParameterValue") String jsonPathToTargetArray, ValidationType validationType, String customReportMessage) {
        processCustomLogMessage(customReportMessage);
        boolean expectedValue = ValidationType.POSITIVE.equals(validationType);
        StringBuilder reportedExpectedValue = new StringBuilder();
        reportedExpectedValue.append("Response data should ");
        if (!expectedValue) {
            reportedExpectedValue.append("not ");
        }
        reportedExpectedValue.append("match the JSON File in this path '").append(referenceJsonFilePath).append("'");
        if (!jsonPathToTargetArray.isBlank()) {
            reportedExpectedValue.append(", with path to Target Array '").append(jsonPathToTargetArray).append("'");
        }
        var validatableResponse = response.then().body(matchesJsonSchema(new File(referenceJsonFilePath)));
        var responseAfter = validatableResponse.extract().response();
        Boolean comparisonResult = response.equals(responseAfter);
        // prepare attachments
        List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Expected JSON Value",
                RestActions.parseBodyToJson(FileActions.getInstance(true).readFile(referenceJsonFilePath)));
        List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual JSON Value",
                RestActions.parseBodyToJson(response));
        List<List<Object>> attachments = new ArrayList<>();
        attachments.add(expectedValueAttachment);
        attachments.add(actualValueAttachment);

        if ((comparisonResult && expectedValue) || (!comparisonResult && !expectedValue)) {
            pass(null, validationCategory, reportedExpectedValue.toString(), String.valueOf(comparisonResult).toUpperCase(), comparisonType, validationType, attachments);
        } else {
            fail(null, validationCategory, reportedExpectedValue.toString(), String.valueOf(comparisonResult).toUpperCase(), comparisonType, validationType, null, attachments);
        }
    }

    protected void validateElementMatches(ValidationCategory validationCategory, WebDriver driver, By elementLocator, VisualValidationEngine visualValidationEngine, ValidationType validationType,
                                          String customReportMessage) {
        lastUsedElementLocator = elementLocator;
        //TODO: remove this temporary fix when this bug is fixed with shutterbug
        //https://github.com/assertthat/selenium-shutterbug/issues/105
        if (Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            visualValidationEngine = VisualValidationEngine.EXACT_OPENCV;
        }
        processCustomLogMessage(customReportMessage);
        StringBuilder reportedExpectedResult = new StringBuilder();
        reportedExpectedResult.append("Element should ");
        Boolean expectedResult = validationType.getValue();
        if (!expectedResult) {
            reportedExpectedResult.append("not ");
        }
        reportedExpectedResult.append("match the reference screenshot");
        List<List<Object>> attachments = new ArrayList<>();
        byte[] referenceImage = ImageProcessingActions.getReferenceImage(elementLocator);
        if (!Arrays.equals(new byte[0], referenceImage)) {
            ReportManagerHelper.logDiscrete("Reference image found.", Level.INFO);
            List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Reference Screenshot",
                    referenceImage);
            attachments.add(expectedValueAttachment);
        } else {
            ReportManagerHelper.logDiscrete("Reference image not found, attempting to capture new reference.", Level.INFO);
        }
        if (elementActionsHelper.getElementsCount(driver, elementLocator) == 1) {
            byte[] elementScreenshot;
            Boolean actualResult;

            elementScreenshot = new ScreenshotManager().takeElementScreenshot(driver, elementLocator);
            actualResult = ImageProcessingActions.compareAgainstBaseline(driver, elementLocator, elementScreenshot, ImageProcessingActions.VisualValidationEngine.valueOf(visualValidationEngine.name()));

            List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Screenshot",
                    elementScreenshot);
            attachments.add(actualValueAttachment);

            if (visualValidationEngine.equals(VisualValidationEngine.EXACT_SHUTTERBUG) && !actualResult) {
                //if shutterbug and failed, get differences screenshot
                byte[] shutterbugDifferencesImage = ImageProcessingActions.getShutterbugDifferencesImage(elementLocator);
                if (!Arrays.equals(new byte[0], shutterbugDifferencesImage)) {
                    List<Object> differencesAttachment = Arrays.asList("Validation Test Data", "Differences",
                            shutterbugDifferencesImage);
                    attachments.add(differencesAttachment);
                }
            }
            if (expectedResult.equals(actualResult)) {
                pass(driver, validationCategory, reportedExpectedResult.toString(), String.valueOf(actualResult).toUpperCase(), visualValidationEngine, validationType, attachments);
            } else {
                fail(driver, validationCategory, reportedExpectedResult.toString(), String.valueOf(actualResult).toUpperCase(), visualValidationEngine, validationType, null, attachments);
            }
        } else {
            byte[] pageScreenshot = new ScreenshotManager().takeScreenshot(driver, null);
            List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Screenshot",
                    pageScreenshot);
            attachments.add(actualValueAttachment);
            fail(driver, validationCategory, reportedExpectedResult.toString(), "Element not found".toUpperCase(), visualValidationEngine, validationType, null, attachments);
        }
    }
}
