package com.shaft.validation.internal;

import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.gui.internal.image.ImageProcessingActions;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums.*;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.Browser;
import org.testng.Assert;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.shaft.gui.element.internal.ElementActionsHelper.formatLocatorToString;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchema;

public class ValidationsHelper {
    //TODO: implement element attribute and element exists validations for sikuli actions
    static final ThreadLocal<ArrayList<String>> optionalCustomLogMessage = new ThreadLocal<>();
    private static By lastUsedElementLocator = null;
    private static final Boolean discreetLoggingState = SHAFT.Properties.reporting.alwaysLogDiscreetly();
    private static List<String> verificationFailuresList = new ArrayList<>();
    private static AssertionError verificationError = null;
    private static final String WHEN_TO_TAKE_PAGE_SOURCE_SNAPSHOT = SHAFT.Properties.visuals.whenToTakePageSourceSnapshot().trim();

    private ValidationsHelper() {
        throw new IllegalStateException("Utility class");
    }

    public static AssertionError getVerificationErrorToForceFail() {
        return verificationError;
    }

    public static void resetVerificationStateAfterFailing() {
        verificationFailuresList = new ArrayList<>();
        verificationError = null;
    }

    protected static void validateFail(ValidationCategory validationCategory, String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
        fail(validationCategory, null, null, null, null, null);
    }

    protected static void validateEquals(ValidationCategory validationCategory, Object expectedValue, Object actualValue,
                                         ValidationComparisonType validationComparisonType, ValidationType validationType,
                                         String... optionalCustomLogMessage) {

        processCustomLogMessage(optionalCustomLogMessage);
        if (JavaHelper.compareTwoObjects(expectedValue, actualValue, validationComparisonType.getValue(),
                validationType.getValue()) == 1) {
            pass(validationCategory, String.valueOf(expectedValue), String.valueOf(actualValue), validationComparisonType, validationType);
        } else {
            // failed comparison, invalid operator (not reachable) or exception
            fail(validationCategory, String.valueOf(expectedValue), String.valueOf(actualValue), validationComparisonType, validationType,
                    null);
        }
    }

    protected static void validateNull(ValidationCategory validationCategory, Object object, ValidationType validationType, String... optionalCustomLogMessage) {

        processCustomLogMessage(optionalCustomLogMessage);
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

        processCustomLogMessage(optionalCustomLogMessage);

        String[] expectedElementStates = {"Element Should Exist", "Element Should not Exist"};
        String[] actualElementStates = {"Element Exists", "Element Doesn't Exists",
                "Element Exists but is not unique"};
        String locatorSeparator = ", locator '";

        lastUsedElementLocator = elementLocator;
        int elementsCount = ElementActionsHelper.getElementsCount(driver, elementLocator);

        if (validationType.getValue()) {
            // expecting a unique element to be present
            final String expectedValue = expectedElementStates[0] + locatorSeparator + formatLocatorToString(elementLocator) + "'";
            switch (elementsCount) {
                case 0 -> {
                    lastUsedElementLocator = null; //reset lastUsedElementLocator to avoid attempting to find the element again
                    fail(validationCategory, expectedValue,
                            actualElementStates[1], ValidationComparisonType.EQUALS, validationType, null);
                }
                case 1 -> pass(validationCategory, expectedValue,
                        actualElementStates[0], ValidationComparisonType.EQUALS, validationType);
                default -> fail(validationCategory, expectedValue,
                        actualElementStates[2], ValidationComparisonType.EQUALS, validationType, null);
            }
        } else {
            // not expecting the element to be present
            final String expectedValue = expectedElementStates[1] + locatorSeparator + formatLocatorToString(elementLocator) + "'";
            switch (elementsCount) {
                case 0 -> {
                    lastUsedElementLocator = null; //reset lastUsedElementLocator to avoid attempting to find the element again
                    pass(validationCategory, expectedValue,
                            actualElementStates[1], ValidationComparisonType.EQUALS, validationType);
                }
                case 1 -> fail(validationCategory, expectedValue,
                        actualElementStates[0], ValidationComparisonType.EQUALS, validationType, null);
                default -> fail(validationCategory, expectedValue,
                        actualElementStates[2], ValidationComparisonType.EQUALS, validationType, null);
            }
        }
    }

    @SuppressWarnings("SpellCheckingInspection")
    protected static void validateElementAttribute(ValidationCategory validationCategory, WebDriver driver, By elementLocator, String elementAttribute,
                                                   String expectedValue, ValidationComparisonType validationComparisonType, ValidationType validationType,
                                                   String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String attributeSeparator = "' for the '";
        String locatorSeparator = "' attribute, element locator '";
        var isDiscrete = ReportManagerHelper.getDiscreteLogging();
        ReportManagerHelper.setDiscreteLogging(true);
        String actualValue;
        try {
            actualValue = switch (elementAttribute.toLowerCase()) {
                case "text" -> ElementActions.getInstance().getText(elementLocator);
                case "texttrimmed" -> ElementActions.getInstance().getText(elementLocator).trim();
                case "tagname" ->
                        ((WebElement) ElementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).getTagName();
                case "size" ->
                        ((WebElement) ElementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).getSize().toString();
                case "selectedtext" -> ElementActions.getInstance().getSelectedText(elementLocator);
                default -> ElementActions.getInstance().getAttribute(elementLocator, elementAttribute);
            };
        } catch (Throwable e) {
            // force fail due to upstream failure
            if (validationType.getValue()) {
                fail(validationCategory, expectedAttributeStates[0] + " '" + expectedValue + attributeSeparator + elementAttribute
                                + locatorSeparator + formatLocatorToString(elementLocator) + "'",
                        "Failed to read the desired element attribute", validationComparisonType, validationType, e);
            } else {
                fail(validationCategory, expectedAttributeStates[1] + " '" + expectedValue + attributeSeparator + elementAttribute
                                + locatorSeparator + formatLocatorToString(elementLocator) + "'",
                        "Failed to read the desired element attribute", validationComparisonType, validationType, e);
            }
            return;
        }
        ReportManagerHelper.setDiscreteLogging(isDiscrete);
        lastUsedElementLocator = elementLocator;
        int comparisonResult = JavaHelper.compareTwoObjects(expectedValue, actualValue,
                validationComparisonType.getValue(), validationType.getValue());
        reportValidationResultOfElementAttribute(new Object[]{expectedAttributeStates, attributeSeparator,
                locatorSeparator, comparisonResult, elementLocator, elementAttribute, expectedValue, actualValue,
                validationComparisonType, validationType, validationCategory});
    }

    protected static void validateElementCSSProperty(ValidationCategory validationCategory, By elementLocator, String propertyName,
                                                     String expectedValue, ValidationComparisonType validationComparisonType, ValidationType validationType,
                                                     String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String propertySeparator = "' for the '";
        String locatorSeparator = "' CSS property, element locator '";
        String actualValue;
        try {
            actualValue = new ElementActions().getCSSProperty(elementLocator, propertyName);
        } catch (Throwable e) {
            // force fail due to upstream failure
            if (validationType.getValue()) {
                fail(validationCategory, expectedAttributeStates[0] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + formatLocatorToString(elementLocator) + "'",
                        "Failed to read the desired element CSS property", validationComparisonType, validationType, e);
            } else {
                fail(validationCategory, expectedAttributeStates[1] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + formatLocatorToString(elementLocator) + "'",
                        "Failed to read the desired element CSS property", validationComparisonType, validationType, e);
            }
            return;
        }
        lastUsedElementLocator = elementLocator;
        int comparisonResult = JavaHelper.compareTwoObjects(expectedValue, actualValue,
                validationComparisonType.getValue(), validationType.getValue());
        reportValidationResultOfElementAttribute(new Object[]{expectedAttributeStates, propertySeparator,
                locatorSeparator, comparisonResult, elementLocator, propertyName, expectedValue, actualValue,
                validationComparisonType, validationType, validationCategory});
    }

    @SuppressWarnings({"SpellCheckingInspection", "unused"})
    protected static void validateBrowserAttribute(ValidationCategory validationCategory, WebDriver driver, String browserAttribute,
                                                   String expectedValue, ValidationComparisonType validationComparisonType, ValidationType validationType,
                                                   String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String attributeSeparator = "' for the '";
        String attributeClosure = "' attribute";
        String actualValue;
        try {
            actualValue = switch (browserAttribute.toLowerCase()) {
                case "currenturl", "url" -> BrowserActions.getInstance().getCurrentURL();
                case "pagesource" -> BrowserActions.getInstance().getPageSource();
                case "title" -> BrowserActions.getInstance().getCurrentWindowTitle();
                case "windowhandle" -> BrowserActions.getInstance().getWindowHandle();
                case "windowposition" -> BrowserActions.getInstance().getWindowPosition();
                case "windowsize" -> BrowserActions.getInstance().getWindowSize();
                default -> "";
            };
        } catch (Throwable e) {
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
        int comparisonResult = JavaHelper.compareTwoObjects(expectedValue, actualValue,
                validationComparisonType.getValue(), validationType.getValue());
        reportValidationResultOfBrowserAttribute(new Object[]{expectedAttributeStates, attributeSeparator,
                attributeClosure, comparisonResult, null, browserAttribute, expectedValue, actualValue,
                validationComparisonType, validationType, validationCategory});
    }

    protected static void validateComparativeRelation(ValidationCategory validationCategory, Number expectedValue, Number actualValue,
                                                      NumbersComparativeRelation numbersComparativeRelation, ValidationType validationType,
                                                      String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
        Boolean comparisonState = switch (numbersComparativeRelation.getValue()) {
            case ">" -> actualValue.floatValue() > expectedValue.floatValue();
            case ">=" -> actualValue.floatValue() >= expectedValue.floatValue();
            case "<" -> actualValue.floatValue() < expectedValue.floatValue();
            case "<=" -> actualValue.floatValue() <= expectedValue.floatValue();
            case "==" -> actualValue.floatValue() == expectedValue.floatValue();
            default -> false;
        };
        if ((ValidationType.POSITIVE.equals(validationType) && comparisonState.equals(true)) || (ValidationType.NEGATIVE.equals(validationType) && comparisonState.equals(false))) {
            pass(validationCategory, expectedValue, actualValue, numbersComparativeRelation, validationType);
        } else {
            fail(validationCategory, expectedValue, actualValue, numbersComparativeRelation, validationType);
        }
    }

    protected static void validateTrue(ValidationCategory validationCategory, Boolean conditionalStatement, ValidationType validationType, String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
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

    protected static void validateFileExists(ValidationCategory validationCategory, String fileFolderName, String fileName, @SuppressWarnings("SameParameterValue") int numberOfRetries,
                                             ValidationType validationType, String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
        boolean expectedValue = ValidationType.POSITIVE.equals(validationType);
        boolean actualValue = FileActions.getInstance().doesFileExist(fileFolderName, fileName, numberOfRetries);
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
        lastUsedElementLocator = elementLocator;
        //TODO: remove this temporary fix when this bug is fixed with shutterbug
        //https://github.com/assertthat/selenium-shutterbug/issues/105
        if (Properties.web.targetBrowserName().equalsIgnoreCase(Browser.SAFARI.browserName())) {
            visualValidationEngine = VisualValidationEngine.EXACT_OPENCV;
        }
        processCustomLogMessage(optionalCustomLogMessage);
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
        if (ElementActionsHelper.getElementsCount(driver, elementLocator) == 1) {
            byte[] elementScreenshot;
            Boolean actualResult;

            elementScreenshot = ScreenshotManager.takeElementScreenshot(driver, elementLocator);
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

    private static void fail(ValidationCategory validationCategory, String expectedValue, String actualValue,
                             Object validationComparisonType, ValidationType validationType, @SuppressWarnings("SameParameterValue") Throwable failureReason, List<List<Object>> externalAttachments) {
        // reset state in case of failure to force reporting the failure
        ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        reportValidationState(validationCategory, expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.FAILED, failureReason, externalAttachments);
    }

    private static void fail(ValidationCategory validationCategory, String expectedValue, String actualValue,
                             Object validationComparisonType, ValidationType validationType, Throwable failureReason) {
        // reset state in case of failure to force reporting the failure
        ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        reportValidationState(validationCategory, expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.FAILED, failureReason, null);
    }

    private static void fail(ValidationCategory validationCategory, Number expectedValue, Number actualValue,
                             Object comparativeRelationType, ValidationType validationType) {
        // reset state in case of failure to force reporting the failure
        ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        reportValidationState(validationCategory, String.valueOf(expectedValue), String.valueOf(actualValue), comparativeRelationType, validationType,
                ValidationState.FAILED, null, null);
    }

    protected static void validateJSONFileContent(ValidationCategory validationCategory, Response response, String referenceJsonFilePath,
                                                  RestActions.ComparisonType comparisonType, @SuppressWarnings("SameParameterValue") String jsonPathToTargetArray, ValidationType validationType, String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
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
                RestActions.parseBodyToJson(FileActions.getInstance().readFile(referenceJsonFilePath)));
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

    protected static void validateResponseFileSchema(ValidationCategory validationCategory, Response response, String referenceJsonFilePath,
                                                     RestActions.ComparisonType comparisonType, @SuppressWarnings("SameParameterValue") String jsonPathToTargetArray, ValidationType validationType, String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
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
                RestActions.parseBodyToJson(FileActions.getInstance().readFile(referenceJsonFilePath)));
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


    private static void reportValidationResultOfElementAttribute(Object[] args) {
        String[] expectedAttributeStates = (String[]) args[0];
        String propertySeparator = (String) args[1];
        String locatorSeparator = (String) args[2];
        int comparisonResult = (int) args[3];
        var elementLocator = (args[4] != null) ? (By) args[4] : null;
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
                                + locatorSeparator + formatLocatorToString(elementLocator) + "'", actualValue, validationComparisonType,
                        validationType);
            } else {
                // no match, or unhandled issue
                fail(validationCategory, expectedAttributeStates[0] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + formatLocatorToString(elementLocator) + "'", actualValue, validationComparisonType,
                        validationType, null);
            }
        } else {
            // expecting element attribute to not have the correct value
            if (comparisonResult == 1) {
                // match
                pass(validationCategory, expectedAttributeStates[1] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + formatLocatorToString(elementLocator) + "'", actualValue, validationComparisonType,
                        validationType);
            } else {
                // no match, or unhandled issue
                fail(validationCategory, expectedAttributeStates[1] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + formatLocatorToString(elementLocator) + "'", actualValue, validationComparisonType,
                        validationType, null);
            }
        }
    }

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

    private static void reportValidationState(ValidationCategory validationCategory, String expectedValue, String actualValue,
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
        if (DriverFactoryHelper.getDriver() != null && DriverFactoryHelper.getDriver() != null) {
            // create a screenshot attachment if needed for webdriver
            if (lastUsedElementLocator != null) {
                attachments.add(ScreenshotManager.captureScreenShot(DriverFactoryHelper.getDriver(), lastUsedElementLocator,
                        validationMethodName, validationState.getValue()));
            } else {
                attachments.add(ScreenshotManager.captureScreenShot(DriverFactoryHelper.getDriver(), validationMethodName,
                        validationState.getValue()));
            }
            // reset lastUsed variables
            lastUsedElementLocator = null;
            //}
        }
        if (DriverFactoryHelper.getDriver() != null && DriverFactoryHelper.getDriver() != null && !WHEN_TO_TAKE_PAGE_SOURCE_SNAPSHOT.equalsIgnoreCase("Never")) {
            if ((WHEN_TO_TAKE_PAGE_SOURCE_SNAPSHOT.equalsIgnoreCase("Always") || WHEN_TO_TAKE_PAGE_SOURCE_SNAPSHOT.equalsIgnoreCase("ValidationPointsOnly"))
                    || (Boolean.FALSE.equals(validationState.getValue()) && WHEN_TO_TAKE_PAGE_SOURCE_SNAPSHOT.equalsIgnoreCase("FailuresOnly"))) {
                var logMessage = "";
                var pageSnapshot = BrowserActionsHelper.capturePageSnapshot(DriverFactoryHelper.getDriver());
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

    private static boolean isExpectedOrActualValueLong(String expectedValue, String actualValue) {
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
}
