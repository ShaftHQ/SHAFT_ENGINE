package com.shaft.validation;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.image.ImageProcessingActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.support.JavaActions;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class ValidationActions {
    //TODO: implement element attribute and element exists validations for sikuli actions
    private static final int ATTEMPTS_ELEMENTNOTFOUNDEXCEPTION = Integer
            .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static final int ATTEMPTS_ELEMENTNOTFOUNDEXCEPTION_ELEMENTSHOULDNOTEXIST = 1;
    private static WebDriver lastUsedDriver = null;
    private static By lastUsedElementLocator = null;
    private static Boolean discreetLoggingState = Boolean.valueOf(System.getProperty("alwaysLogDiscreetly"));
    private static List<String> verificationFailuresList = new ArrayList<>();
    private static AssertionError verificationError = null;

    private ValidationActions() {
        throw new IllegalStateException("Utility class");
    }

    protected static AssertionError getVerificationErrorToForceFail() {
        return verificationError;
    }

    protected static void resetVerificationStateAfterFailing() {
        verificationFailuresList = new ArrayList<>();
        verificationError = null;
    }

    protected static void validateFail(ValidationCategory validationCategory, String... optionalCustomLogMessage) {
        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }
        fail(validationCategory, null, null, null, null, null);
    }

    protected static void validateEquals(ValidationCategory validationCategory, Object expectedValue, Object actualValue,
                                         ValidationComparisonType validationComparisonType, ValidationType validationType,
                                         String... optionalCustomLogMessage) {

        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        if (JavaActions.compareTwoObjects(expectedValue, actualValue, validationComparisonType.getValue(),
                validationType.getValue()) == 1) {
            pass(validationCategory, String.valueOf(expectedValue), String.valueOf(actualValue), validationComparisonType, validationType);
        } else {
            // failed comparison, invalid operator (not reachable) or exception
            fail(validationCategory, String.valueOf(expectedValue), String.valueOf(actualValue), validationComparisonType, validationType,
                    null);
        }
    }

    protected static void validateNull(ValidationCategory validationCategory, Object object, ValidationType validationType, String... optionalCustomLogMessage) {

        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        if (validationType.getValue()) {
            try {
                Assert.assertNull(object);
                pass(validationCategory, "NULL", "NULL", ValidationComparisonType.EQUALS, validationType);
            } catch (Exception | AssertionError failureReason) {
                fail(validationCategory, "NULL", String.valueOf(object), ValidationComparisonType.EQUALS, validationType, failureReason);
            }
        } else {
            try {
                Assert.assertNotNull(object);
                pass(validationCategory, "NULL", String.valueOf(object), ValidationComparisonType.EQUALS, validationType);
            } catch (Exception | AssertionError failureReason) {
                fail(validationCategory, "NULL", "NULL", ValidationComparisonType.EQUALS, validationType, failureReason);
            }
        }
    }

    protected static void validateElementExists(ValidationCategory validationCategory, WebDriver driver, By elementLocator, ValidationType validationType,
                                                String... optionalCustomLogMessage) {

        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        int customAttempts = ATTEMPTS_ELEMENTNOTFOUNDEXCEPTION;
        if (!validationType.getValue()) {
            customAttempts = ATTEMPTS_ELEMENTNOTFOUNDEXCEPTION_ELEMENTSHOULDNOTEXIST;
        }

        String[] expectedElementStates = {"Element Should Exist", "Element Should not Exist"};
        String[] actualElementStates = {"Element Exists", "Element Doesn't Exists",
                "Element Exists but is not unique"};
        String locatorSeparator = ", locator '";

        lastUsedDriver = driver;
        lastUsedElementLocator = elementLocator;
        int elementsCount = ElementActions.getElementsCount(driver, elementLocator, customAttempts);

        if (validationType.getValue()) {
            // expecting a unique element to be present
            final String expectedValue = expectedElementStates[0] + locatorSeparator + elementLocator.toString() + "'";
            switch (elementsCount) {
                case 0 -> fail(validationCategory, expectedValue,
                        actualElementStates[1], ValidationComparisonType.EQUALS, validationType, null);
                case 1 -> pass(validationCategory, expectedValue,
                        actualElementStates[0], ValidationComparisonType.EQUALS, validationType);
                default -> fail(validationCategory, expectedValue,
                        actualElementStates[2], ValidationComparisonType.EQUALS, validationType, null);
            }
        } else {
            // not expecting the element to be present
            final String expectedValue = expectedElementStates[1] + locatorSeparator + elementLocator.toString() + "'";
            switch (elementsCount) {
                case 0 -> pass(validationCategory, expectedValue,
                        actualElementStates[1], ValidationComparisonType.EQUALS, validationType);
                case 1 -> fail(validationCategory, expectedValue,
                        actualElementStates[0], ValidationComparisonType.EQUALS, validationType, null);
                default -> fail(validationCategory, expectedValue,
                        actualElementStates[2], ValidationComparisonType.EQUALS, validationType, null);
            }
        }
    }

    protected static void validateElementAttribute(ValidationCategory validationCategory, WebDriver driver, By elementLocator, String elementAttribute,
                                                   String expectedValue, ValidationComparisonType validationComparisonType, ValidationType validationType,
                                                   String... optionalCustomLogMessage) {

        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String attributeSeparator = "' for the '";
        String locatorSeparator = "' attribute, element locator '";

        String actualValue;
        try {
            discreetLoggingState = ReportManager.isDiscreteLogging();
            ReportManager.setDiscreteLogging(true);
            actualValue = switch (elementAttribute.toLowerCase()) {
                case "text" -> ElementActions.getText(driver, elementLocator);
                case "tagname" -> ElementActions.getTagName(driver, elementLocator);
                case "size" -> ElementActions.getSize(driver, elementLocator);
                case "selectedtext" -> ElementActions.getSelectedText(driver, elementLocator);
                default -> ElementActions.getAttribute(driver, elementLocator, elementAttribute);
            };
            ReportManager.setDiscreteLogging(discreetLoggingState);
        } catch (AssertionError e) {
            // force fail due to upstream failure
            if (validationType.getValue()) {
                fail(validationCategory, expectedAttributeStates[0] + " '" + expectedValue + attributeSeparator + elementAttribute
                                + locatorSeparator + elementLocator.toString() + "'",
                        "Failed to read the desired element attribute", validationComparisonType, validationType, e);
            } else {
                fail(validationCategory, expectedAttributeStates[1] + " '" + expectedValue + attributeSeparator + elementAttribute
                                + locatorSeparator + elementLocator.toString() + "'",
                        "Failed to read the desired element attribute", validationComparisonType, validationType, e);
            }
            return;
        }

        lastUsedDriver = driver;
        lastUsedElementLocator = elementLocator;
        int comparisonResult = JavaActions.compareTwoObjects(expectedValue, actualValue,
                validationComparisonType.getValue(), validationType.getValue());

        reportValidationResultOfElementAttribute(new Object[]{expectedAttributeStates, attributeSeparator,
                locatorSeparator, comparisonResult, elementLocator, elementAttribute, expectedValue, actualValue,
                validationComparisonType, validationType, validationCategory});
    }

    protected static void validateElementCSSProperty(ValidationCategory validationCategory, WebDriver driver, By elementLocator, String propertyName,
                                                     String expectedValue, ValidationComparisonType validationComparisonType, ValidationType validationType,
                                                     String... optionalCustomLogMessage) {

        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String propertySeparator = "' for the '";
        String locatorSeparator = "' CSS property, element locator '";

        discreetLoggingState = ReportManager.isDiscreteLogging();
        ReportManager.setDiscreteLogging(true);
        String actualValue = ElementActions.getCSSProperty(driver, elementLocator, propertyName);
        ReportManager.setDiscreteLogging(discreetLoggingState);

        lastUsedDriver = driver;
        lastUsedElementLocator = elementLocator;
        int comparisonResult = JavaActions.compareTwoObjects(expectedValue, actualValue,
                validationComparisonType.getValue(), validationType.getValue());

        reportValidationResultOfElementAttribute(new Object[]{expectedAttributeStates, propertySeparator,
                locatorSeparator, comparisonResult, elementLocator, propertyName, expectedValue, actualValue,
                validationComparisonType, validationType, validationCategory});

    }

    protected static void validateBrowserAttribute(ValidationCategory validationCategory, WebDriver driver, String browserAttribute,
                                                   String expectedValue, ValidationComparisonType validationComparisonType, ValidationType validationType,
                                                   String... optionalCustomLogMessage) {

        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String attributeSeparator = "' for the '";
        String attributeClosure = "' attribute";

        String actualValue;
        try {
            discreetLoggingState = ReportManager.isDiscreteLogging();
            ReportManager.setDiscreteLogging(true);
            actualValue = switch (browserAttribute.toLowerCase()) {
                case "currenturl" -> BrowserActions.getCurrentURL(driver);
                case "pagesource" -> BrowserActions.getPageSource(driver);
                case "title" -> BrowserActions.getCurrentWindowTitle(driver);
                case "windowhandle" -> BrowserActions.getWindowHandle(driver);
                case "windowposition" -> BrowserActions.getWindowPosition(driver);
                case "windowsize" -> BrowserActions.getWindowSize(driver);
                default -> "";
            };
            ReportManager.setDiscreteLogging(discreetLoggingState);
        } catch (AssertionError e) {
            // force fail due to upstream failure
            if (validationType.getValue()) {
                fail(validationCategory, expectedAttributeStates[0] + " '" + expectedValue + attributeSeparator + browserAttribute
                                + attributeClosure,
                        "Failed to read the desired browser attribute", validationComparisonType, validationType, e);
            } else {
                fail(validationCategory, expectedAttributeStates[1] + " '" + expectedValue + attributeSeparator + browserAttribute
                                + attributeClosure,
                        "Failed to read the desired browser attribute", validationComparisonType, validationType, e);
            }
            return;
        }

        lastUsedDriver = driver;
        int comparisonResult = JavaActions.compareTwoObjects(expectedValue, actualValue,
                validationComparisonType.getValue(), validationType.getValue());

        reportValidationResultOfBrowserAttribute(new Object[]{expectedAttributeStates, attributeSeparator,
                attributeClosure, comparisonResult, null, browserAttribute, expectedValue, actualValue,
                validationComparisonType, validationType, validationCategory});
    }

    protected static void validateComparativeRelation(ValidationCategory validationCategory, Number expectedValue, Number actualValue,
                                                      ComparativeRelationType comparativeRelationType, ValidationType validationType,
                                                      String... optionalCustomLogMessage) {

        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        Boolean comparisonState = switch (comparativeRelationType.getValue()) {
            case ">" -> actualValue.floatValue() > expectedValue.floatValue();
            case ">=" -> actualValue.floatValue() >= expectedValue.floatValue();
            case "<" -> actualValue.floatValue() < expectedValue.floatValue();
            case "<=" -> actualValue.floatValue() <= expectedValue.floatValue();
            case "==" -> actualValue.floatValue() == expectedValue.floatValue();
            default -> false;
        };

        if ((ValidationType.POSITIVE.equals(validationType) && comparisonState.equals(true)) || (ValidationType.NEGATIVE.equals(validationType) && comparisonState.equals(false))) {
            pass(validationCategory, expectedValue, actualValue, comparativeRelationType, validationType);
        } else {
            fail(validationCategory, expectedValue, actualValue, comparativeRelationType, validationType);
        }
    }

    protected static void validateTrue(ValidationCategory validationCategory, Boolean conditionalStatement, ValidationType validationType, String... optionalCustomLogMessage) {
        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        Boolean expectedValue = false;
        if (ValidationType.POSITIVE.equals(validationType)) {
            expectedValue = true;
        }

        if ((expectedValue && conditionalStatement) || (!expectedValue && !conditionalStatement)) {
            pass(validationCategory, String.valueOf(expectedValue).toUpperCase(), String.valueOf(conditionalStatement).toUpperCase(), null, validationType);
        } else {
            fail(validationCategory, String.valueOf(expectedValue).toUpperCase(), String.valueOf(conditionalStatement).toUpperCase(), null, validationType, null);
        }
    }

    protected static void validateFileExists(ValidationCategory validationCategory, String fileFolderName, String fileName, int numberOfRetries,
                                             ValidationType validationType, String... optionalCustomLogMessage) {
        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        boolean expectedValue = false;
        if (ValidationType.POSITIVE.equals(validationType)) {
            expectedValue = true;
        }
        boolean actualValue = FileActions.doesFileExist(fileFolderName, fileName, numberOfRetries);

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
            pass(validationCategory, reportedExpectedValue, reportedActualValue, null, validationType);
        } else {
            fail(validationCategory, reportedExpectedValue, reportedActualValue, null, validationType, null);
        }
    }

    protected static void validateElementMatches(ValidationCategory validationCategory, WebDriver driver, By elementLocator, VisualValidationEngine visualValidationEngine, ValidationType validationType,
                                                 String... optionalCustomLogMessage) {
        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

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
            List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Reference Screenshot",
                    referenceImage);
            attachments.add(expectedValueAttachment);
        }

        if (ElementActions.getElementsCount(driver, elementLocator) == 1) {
            byte[] elementScreenshot = ScreenshotManager.takeElementScreenshot(driver, elementLocator);
            List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Screenshot",
                    elementScreenshot);
            attachments.add(actualValueAttachment);

            Boolean actualResult = ImageProcessingActions.compareAgainstBaseline(driver, elementLocator, elementScreenshot, ImageProcessingActions.VisualValidationEngine.valueOf(visualValidationEngine.name()));
            if (expectedResult.equals(actualResult)) {
                pass(validationCategory, reportedExpectedResult.toString(), String.valueOf(actualResult).toUpperCase(), visualValidationEngine, validationType, attachments);
            } else {
                fail(validationCategory, reportedExpectedResult.toString(), String.valueOf(actualResult).toUpperCase(), visualValidationEngine, validationType, null, attachments);
            }
        } else {
            byte[] pageScreenshot = ScreenshotManager.takeFullPageScreenshot(driver);
            List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Screenshot",
                    pageScreenshot);
            attachments.add(actualValueAttachment);

            fail(validationCategory, reportedExpectedResult.toString(), "Element not found".toUpperCase(), visualValidationEngine, validationType, null, attachments);
        }
    }

    protected static void validateJSONFileContent(ValidationCategory validationCategory, Response response, String referenceJsonFilePath,
                                                  RestActions.ComparisonType comparisonType, String jsonPathToTargetArray, ValidationType validationType, String... optionalCustomLogMessage) {
        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        boolean expectedValue = false;
        if (ValidationType.POSITIVE.equals(validationType)) {
            expectedValue = true;
        }
        StringBuilder reportedExpectedValue = new StringBuilder();
        reportedExpectedValue.append("Response data should ");
        if (!expectedValue) {
            reportedExpectedValue.append("not ");
        }
        reportedExpectedValue.append("match the JSON File in this path '").append(referenceJsonFilePath).append("'");
        if (!jsonPathToTargetArray.equals("")) {
            reportedExpectedValue.append(", with path to Target Array '").append(jsonPathToTargetArray).append("'");
        }

        Boolean comparisonResult = RestActions.compareJSON(response, referenceJsonFilePath, comparisonType,
                jsonPathToTargetArray);

        // prepare attachments
        List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Expected JSON Value",
                RestActions.parseBodyToJson(FileActions.readFromFile(referenceJsonFilePath)));
        List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual JSON Value",
                RestActions.parseBodyToJson(response));

        List<List<Object>> attachments = new ArrayList<>();
        attachments.add(expectedValueAttachment);
        attachments.add(actualValueAttachment);

        if ((comparisonResult && expectedValue) || (!comparisonResult && !expectedValue)) {
            pass(validationCategory, reportedExpectedValue.toString(), String.valueOf(comparisonResult).toUpperCase(), comparisonType, validationType, attachments);
        } else {
            fail(validationCategory, reportedExpectedValue.toString(), String.valueOf(comparisonResult).toUpperCase(), comparisonType, validationType, null, attachments);
        }
    }

    private static void pass(ValidationCategory validationCategory, String expectedValue, String actualValue,
                             Object validationComparisonType, ValidationType validationType, List<List<Object>> externalAttachments) {

        reportValidationState(validationCategory, expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.PASSED, null, externalAttachments);
    }

    private static void pass(ValidationCategory validationCategory, String expectedValue, String actualValue,
                             Object validationComparisonType, ValidationType validationType) {

        reportValidationState(validationCategory, expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.PASSED, null, null);
    }

    private static void pass(ValidationCategory validationCategory, Number expectedValue, Number actualValue,
                             Object comparativeRelationType, ValidationType validationType) {
        reportValidationState(validationCategory, String.valueOf(expectedValue), String.valueOf(actualValue), comparativeRelationType, validationType,
                ValidationState.PASSED, null, null);
    }

    @SuppressWarnings("SameParameterValue")
    private static void fail(ValidationCategory validationCategory, String expectedValue, String actualValue,
                             Object validationComparisonType, ValidationType validationType, Throwable failureReason, List<List<Object>> externalAttachments) {
        // reset state in case of failure to force reporting the failure
        ReportManager.setDiscreteLogging(discreetLoggingState);

        reportValidationState(validationCategory, expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.FAILED, failureReason, externalAttachments);
    }

    private static void fail(ValidationCategory validationCategory, String expectedValue, String actualValue,
                             Object validationComparisonType, ValidationType validationType, Throwable failureReason) {
        // reset state in case of failure to force reporting the failure
        ReportManager.setDiscreteLogging(discreetLoggingState);

        reportValidationState(validationCategory, expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.FAILED, failureReason, null);
    }

    private static void fail(ValidationCategory validationCategory, Number expectedValue, Number actualValue,
                             Object comparativeRelationType, ValidationType validationType) {
        // reset state in case of failure to force reporting the failure
        ReportManager.setDiscreteLogging(discreetLoggingState);

        reportValidationState(validationCategory, String.valueOf(expectedValue), String.valueOf(actualValue), comparativeRelationType, validationType,
                ValidationState.FAILED, null, null);
    }

    private static void reportValidationState(ValidationCategory validationCategory, String expectedValue, String actualValue,
                                              Object validationComparisonOrComparativeRelationType, ValidationType validationType,
                                              ValidationState validationState, Throwable failureReason, List<List<Object>> externalAttachments) {

        // prepare message and attachments
        StringBuilder message = new StringBuilder();
        List<List<Object>> attachments = new ArrayList<>();
        if (externalAttachments != null && !externalAttachments.isEmpty()) {
            attachments.addAll(externalAttachments);
        }

        // get validation method name
        String validationMethodName = (new Throwable()).getStackTrace()[2].getMethodName();
        String callingAssertionOrVerificationMethodName = (new Throwable()).getStackTrace()[3].getMethodName();

        if (validationMethodName.contains("reportValidationResult")) {
            callingAssertionOrVerificationMethodName = (new Throwable()).getStackTrace()[4].getMethodName();
        }
        validationMethodName = callingAssertionOrVerificationMethodName;

        validationMethodName = validationMethodName.substring(0, 1).toUpperCase() + validationMethodName.substring(1);
        if (validationMethodName.equals("ValidateFail")) {
            validationState = ValidationState.PASSED;
            message.append(validationMethodName).append(" ").append(validationState).append("; ");
            message.append("Successfully force failed the test.");
        } else {
            message.append(validationMethodName).append(" ").append(validationState).append("; ");
            // prepare expected/actual results as an attachment or in the message
            boolean isExpectedOrActualValueLong = false;
            if (actualValue == null && expectedValue != null) {
                isExpectedOrActualValueLong = expectedValue.length() >= 500;
            } else if (actualValue != null && expectedValue == null) {
                isExpectedOrActualValueLong = actualValue.length() >= 500;
            } else if (actualValue != null) {
                isExpectedOrActualValueLong = expectedValue.length() >= 500 || actualValue.length() >= 500;
            }

            if (Boolean.TRUE.equals(isExpectedOrActualValueLong)) {
                List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Expected Value",
                        expectedValue);
                List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Value", actualValue);

                attachments.add(expectedValueAttachment);
                attachments.add(actualValueAttachment);

                message.append("Expected and Actual values are attached.");
            } else {
                message.append("Expected [").append(expectedValue).append("] and Actual [").append(actualValue).append("].");
            }

            if (validationComparisonOrComparativeRelationType != null) {
                message.append(" Comparison Type [").append(validationComparisonOrComparativeRelationType.toString()).append("].");
            }

            if (validationType != null) {
                message.append(" Validation Type [").append(validationType).append("].");
            }
        }

        // create a screenshot attachment if needed
        if (expectedValue != null && expectedValue.toLowerCase().contains("locator")) {
            if (lastUsedDriver != null && lastUsedElementLocator != null) {
                attachments.add(ScreenshotManager.captureScreenShot(lastUsedDriver, lastUsedElementLocator,
                        validationMethodName, validationState.getValue()));
            } else if (lastUsedDriver != null) {
                attachments.add(ScreenshotManager.captureScreenShot(lastUsedDriver, validationMethodName,
                        validationState.getValue()));
            }
            // reset lastUsed variables
            lastUsedDriver = null;
            lastUsedElementLocator = null;
        }

        // handling changes as per validationCategory hard/soft
        switch (validationCategory) {
            case HARD_ASSERT:
                // create the log entry with or without attachments
                if (!attachments.isEmpty()) {
                    ReportManager.log(message.toString(), attachments);
                } else {
                    ReportManager.log(message.toString());
                }

                // set test state in case of failure
                if (!validationState.getValue()) {
                    if (failureReason != null) {
                        Assert.fail(message.toString(), failureReason);
                    } else {
                        Assert.fail(message.toString());
                    }
                }
                break;
            case SOFT_ASSERT:
                // handle failure reason in case of soft assert
                if (failureReason != null) {
                    List<Object> failureReasonAttachment = Arrays.asList("Validation Test Data", "Failure Reason",
                            ReportManager.formatStackTraceToLogEntry(failureReason));
                    attachments.add(failureReasonAttachment);
                }

                // create the log entry with or without attachments
                if (!attachments.isEmpty()) {
                    ReportManager.log(message.toString(), attachments);
                } else {
                    ReportManager.log(message.toString());
                }

                // set test state in case of failure
                if (!validationState.getValue()) {
                    verificationFailuresList.add(message.toString());
                    verificationError = new AssertionError(String.join("\nAND ", verificationFailuresList));
                }
                break;
            default:
                break;
        }
    }

    @SuppressWarnings("DuplicatedCode")
    private static void reportValidationResultOfElementAttribute(Object[] args) {
        String[] expectedAttributeStates = (String[]) args[0];
        String propertySeparator = (String) args[1];
        String locatorSeparator = (String) args[2];
        int comparisonResult = (int) args[3];
        By elementLocator = (By) args[4];
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
                pass(validationCategory, expectedAttributeStates[0] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + elementLocator.toString() + "'", actualValue, validationComparisonType,
                        validationType);
            } else {
                // no match, or unhandled issue
                fail(validationCategory, expectedAttributeStates[0] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + elementLocator.toString() + "'", actualValue, validationComparisonType,
                        validationType, null);
            }
        } else {
            // expecting element attribute to not have the correct value
            if (comparisonResult == 1) {
                // match
                pass(validationCategory, expectedAttributeStates[1] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + elementLocator.toString() + "'", actualValue, validationComparisonType,
                        validationType);
            } else {
                // no match, or unhandled issue
                fail(validationCategory, expectedAttributeStates[1] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + elementLocator.toString() + "'", actualValue, validationComparisonType,
                        validationType, null);
            }
        }
    }

    @SuppressWarnings("DuplicatedCode")
    private static void reportValidationResultOfBrowserAttribute(Object[] args) {
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
                pass(validationCategory, expectedAttributeStates[0] + " '" + expectedValue + propertySeparator + propertyName
                                + attributeClosure, actualValue, validationComparisonType,
                        validationType);
            } else {
                // no match, or unhandled issue
                fail(validationCategory, expectedAttributeStates[0] + " '" + expectedValue + propertySeparator + propertyName
                                + attributeClosure, actualValue, validationComparisonType,
                        validationType, null);
            }
        } else {
            // expecting element attribute to not have the correct value
            if (comparisonResult == 1) {
                // match
                pass(validationCategory, expectedAttributeStates[1] + " '" + expectedValue + propertySeparator + propertyName
                                + attributeClosure, actualValue, validationComparisonType,
                        validationType);
            } else {
                // no match, or unhandled issue
                fail(validationCategory, expectedAttributeStates[1] + " '" + expectedValue + propertySeparator + propertyName
                                + attributeClosure, actualValue, validationComparisonType,
                        validationType, null);
            }
        }
    }

    protected enum ValidationType {
        POSITIVE(true), NEGATIVE(false);

        private final Boolean value;

        ValidationType(Boolean type) {
            this.value = type;
        }

        protected boolean getValue() {
            return value;
        }
    }

    @SuppressWarnings("unused")
    protected enum ValidationComparisonType {
        EQUALS(1), CONTAINS(3), MATCHES(2), CASE_INSENSITIVE(4);

        private final int value;

        ValidationComparisonType(int type) {
            this.value = type;
        }

        protected int getValue() {
            return value;
        }
    }

    @SuppressWarnings("unused")
    protected enum VisualValidationEngine {
        EXACT_OPENCV,
        EXACT_EYES,
        STRICT_EYES,
        CONTENT_EYES,
        LAYOUT_EYES
    }

    protected enum ValidationCategory {
        HARD_ASSERT,
        SOFT_ASSERT
    }

    @SuppressWarnings("unused")
    protected enum ComparativeRelationType {
        GREATER_THAN(">"), GREATER_THAN_OR_EQUALS(">="), LESS_THAN("<"), LESS_THAN_OR_EQUALS("<="), EQUALS("==");

        private final String value;

        ComparativeRelationType(String type) {
            this.value = type;
        }

        protected String getValue() {
            return value;
        }
    }

    private enum ValidationState {
        PASSED(true), FAILED(false);

        private final Boolean value;

        ValidationState(Boolean type) {
            this.value = type;
        }

        protected boolean getValue() {
            return value;
        }
    }

}
