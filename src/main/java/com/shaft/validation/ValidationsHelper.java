package com.shaft.validation;

import com.microsoft.playwright.Page;
import com.shaft.api.RestActions;
import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.image.ImageProcessingActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManagerHelper;
import com.shaft.tools.support.JavaActions;
import com.shaft.validation.ValidationEnums.*;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchema;

public class ValidationsHelper {
    //TODO: implement element attribute and element exists validations for sikuli actions
    private static final int ATTEMPTS_ELEMENTNOTFOUNDEXCEPTION = Integer
            .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static final int ATTEMPTS_ELEMENTNOTFOUNDEXCEPTION_ELEMENTSHOULDNOTEXIST = 1;
    private static WebDriver lastUsedDriver = null;
    private static By lastUsedElementLocator = null;

    private static Page lastUsedPage = null;
    private static String lastUsedElementLocatorString = null;
    private static Boolean discreetLoggingState = Boolean.valueOf(System.getProperty("alwaysLogDiscreetly"));
    private static List<String> verificationFailuresList = new ArrayList<>();
    private static AssertionError verificationError = null;

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

    static ArrayList<String> optionalCustomLogMessage = new ArrayList<>();

    protected static void validateFail(ValidationCategory validationCategory, String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
        fail(validationCategory, null, null, null, null, null);
    }

    protected static void validateEquals(ValidationCategory validationCategory, Object expectedValue, Object actualValue,
                                         ValidationComparisonType validationComparisonType, ValidationType validationType,
                                         String... optionalCustomLogMessage) {

        processCustomLogMessage(optionalCustomLogMessage);
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

    protected static void validateElementExists(ValidationCategory validationCategory, Page page, String elementLocator, ValidationType validationType,
                                                String... optionalCustomLogMessage) {

        processCustomLogMessage(optionalCustomLogMessage);
        String[] expectedElementStates = {"Element Should Exist", "Element Should not Exist"};
        String[] actualElementStates = {"Element Exists", "Element Doesn't Exists",
                "Element Exists but is not unique"};
        String locatorSeparator = ", locator '";

        lastUsedPage = page;
        lastUsedElementLocatorString = elementLocator;
        int elementsCount = ElementActions.performElementAction(page).getElementsCount(elementLocator);

        if (validationType.getValue()) {
            // expecting a unique element to be present
            final String expectedValue = expectedElementStates[0] + locatorSeparator + elementLocator + "'";
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
            final String expectedValue = expectedElementStates[1] + locatorSeparator + elementLocator + "'";
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

    protected static void validateElementExists(ValidationCategory validationCategory, WebDriver driver, By elementLocator, ValidationType validationType,
                                                String... optionalCustomLogMessage) {

        processCustomLogMessage(optionalCustomLogMessage);
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

    protected static void validateElementAttribute(ValidationCategory validationCategory, Page page,
                                                   String elementLocator, String elementAttribute, String expectedValue,
                                                   ValidationComparisonType validationComparisonType, ValidationType validationType,
                                                   String... optionalCustomLogMessage) {

        processCustomLogMessage(optionalCustomLogMessage);
        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String attributeSeparator = "' for the '";
        String locatorSeparator = "' attribute, element locator '";

        String actualValue;
        try {
            discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            actualValue = switch (elementAttribute.toLowerCase()) {
                case "text", "selectedtext" -> ElementActions.performElementAction(page).getText(elementLocator);
                case "tagname" -> ElementActions.performElementAction(page).getAttribute(elementLocator, "tagName");
                case "size" -> ElementActions.performElementAction(page).getSize(elementLocator);
                default -> ElementActions.performElementAction(page).getAttribute(elementLocator, elementAttribute);
            };
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (AssertionError e) {
            // force fail due to upstream failure
            if (validationType.getValue()) {
                fail(validationCategory,
                        expectedAttributeStates[0] + " '" + expectedValue + attributeSeparator + elementAttribute
                                + locatorSeparator + elementLocator + "'",
                        "Failed to read the desired element attribute", validationComparisonType, validationType, e);
            } else {
                fail(validationCategory,
                        expectedAttributeStates[1] + " '" + expectedValue + attributeSeparator + elementAttribute
                                + locatorSeparator + elementLocator + "'",
                        "Failed to read the desired element attribute", validationComparisonType, validationType, e);
            }
            return;
        }

        lastUsedPage = page;
        lastUsedElementLocatorString = elementLocator;
        int comparisonResult = JavaActions.compareTwoObjects(expectedValue, actualValue,
                validationComparisonType.getValue(), validationType.getValue());

        reportValidationResultOfElementAttribute(new Object[]{expectedAttributeStates, attributeSeparator,
                locatorSeparator, comparisonResult, elementLocator, elementAttribute, expectedValue, actualValue,
                validationComparisonType, validationType, validationCategory});
    }

    protected static void validateElementAttribute(ValidationCategory validationCategory, WebDriver driver, By elementLocator, String elementAttribute,
                                                   String expectedValue, ValidationComparisonType validationComparisonType, ValidationType validationType,
                                                   String... optionalCustomLogMessage) {

        processCustomLogMessage(optionalCustomLogMessage);
        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String attributeSeparator = "' for the '";
        String locatorSeparator = "' attribute, element locator '";

        String actualValue;
        try {
            discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            actualValue = switch (elementAttribute.toLowerCase()) {
                case "text" -> ElementActions.getText(driver, elementLocator);
                case "tagname" -> ElementActions.getTagName(driver, elementLocator);
                case "size" -> ElementActions.getSize(driver, elementLocator);
                case "selectedtext" -> ElementActions.getSelectedText(driver, elementLocator);
                default -> ElementActions.getAttribute(driver, elementLocator, elementAttribute);
            };
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
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

        processCustomLogMessage(optionalCustomLogMessage);
        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String propertySeparator = "' for the '";
        String locatorSeparator = "' CSS property, element locator '";

        discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
        ReportManagerHelper.setDiscreteLogging(true);
        String actualValue = ElementActions.getCSSProperty(driver, elementLocator, propertyName);
        ReportManagerHelper.setDiscreteLogging(discreetLoggingState);

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

        processCustomLogMessage(optionalCustomLogMessage);
        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String attributeSeparator = "' for the '";
        String attributeClosure = "' attribute";

        String actualValue;
        try {
            discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            actualValue = switch (browserAttribute.toLowerCase()) {
                case "currenturl", "url" -> BrowserActions.getCurrentURL(driver);
                case "pagesource" -> BrowserActions.getPageSource(driver);
                case "title" -> BrowserActions.getCurrentWindowTitle(driver);
                case "windowhandle" -> BrowserActions.getWindowHandle(driver);
                case "windowposition" -> BrowserActions.getWindowPosition(driver);
                case "windowsize" -> BrowserActions.getWindowSize(driver);
                default -> "";
            };
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
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

    protected static void validateBrowserAttribute(ValidationCategory validationCategory, Page page, String browserAttribute,
                                                   String expectedValue, ValidationComparisonType validationComparisonType, ValidationType validationType,
                                                   String... optionalCustomLogMessage) {

        processCustomLogMessage(optionalCustomLogMessage);
        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String attributeSeparator = "' for the '";
        String attributeClosure = "' attribute";

        String actualValue;
        try {
            discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            actualValue = switch (browserAttribute.toLowerCase()) {
                case "currenturl", "url" -> BrowserActions.performBrowserAction(page).getCurrentURL();
                case "pagesource" -> BrowserActions.performBrowserAction(page).getPageSource();
                case "title" -> BrowserActions.performBrowserAction(page).getCurrentWindowTitle();
                case "windowsize" -> BrowserActions.performBrowserAction(page).getWindowSize();
                default -> "";
            };
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
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

        lastUsedPage = page;
        int comparisonResult = JavaActions.compareTwoObjects(expectedValue, actualValue,
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

    protected static void validateFileExists(ValidationCategory validationCategory, String fileFolderName, String fileName, int numberOfRetries,
                                             ValidationType validationType, String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
        boolean expectedValue = ValidationType.POSITIVE.equals(validationType);
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

    protected static void validateFileContentEquals(ValidationCategory validationCategory, String fileFolderName, String fileName, int numberOfRetries,
                                             ValidationType validationType, String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
        boolean expectedValue = ValidationType.POSITIVE.equals(validationType);
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

    protected static void validateElementMatches(ValidationCategory validationCategory, Page page, String elementLocator, VisualValidationEngine visualValidationEngine, ValidationType validationType,
                                                 String... optionalCustomLogMessage) {
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

        if (ElementActions.performElementAction(page).getElementsCount(elementLocator) == 1) {
            byte[] elementScreenshot = ScreenshotManager.takeElementScreenshot(page, elementLocator);
            List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Screenshot",
                    elementScreenshot);
            attachments.add(actualValueAttachment);

            Boolean actualResult = ImageProcessingActions.compareAgainstBaseline(page, elementLocator, elementScreenshot, ImageProcessingActions.VisualValidationEngine.valueOf(visualValidationEngine.name()));
            if (expectedResult.equals(actualResult)) {
                pass(validationCategory, reportedExpectedResult.toString(), String.valueOf(actualResult).toUpperCase(), visualValidationEngine, validationType, attachments);
            } else {
                fail(validationCategory, reportedExpectedResult.toString(), String.valueOf(actualResult).toUpperCase(), visualValidationEngine, validationType, null, attachments);
            }
        } else {
            byte[] pageScreenshot = ScreenshotManager.takeFullPageScreenshot(page);
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
                             Object validationComparisonType, ValidationType validationType, Throwable failureReason, List<List<Object>> externalAttachments) {
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
                                                  RestActions.ComparisonType comparisonType, String jsonPathToTargetArray, ValidationType validationType, String... optionalCustomLogMessage) {
        processCustomLogMessage(optionalCustomLogMessage);
        boolean expectedValue = ValidationType.POSITIVE.equals(validationType);
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

    protected static void validateResponseFileSchema(ValidationCategory validationCategory, Response response,  String referenceJsonFilePath,
                                                     RestActions.ComparisonType comparisonType, String jsonPathToTargetArray, ValidationType validationType, String... optionalCustomLogMessage) {

        processCustomLogMessage(optionalCustomLogMessage);
        boolean expectedValue = ValidationType.POSITIVE.equals(validationType);
        StringBuilder reportedExpectedValue = new StringBuilder();
        reportedExpectedValue.append("Response data should ");
        if (!expectedValue) {
            reportedExpectedValue.append("not ");
        }
        reportedExpectedValue.append("match the JSON File in this path '").append(referenceJsonFilePath).append("'");
        if (!jsonPathToTargetArray.equals("")) {
            reportedExpectedValue.append(", with path to Target Array '").append(jsonPathToTargetArray).append("'");
        }

        var validatableResponse = response.then().body(matchesJsonSchema(new File(referenceJsonFilePath)));
        var responseAfter = validatableResponse.extract().response();

        Boolean comparisonResult = response.equals(responseAfter);

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


    private static void reportValidationResultOfElementAttribute(Object[] args) {
        String[] expectedAttributeStates = (String[]) args[0];
        String propertySeparator = (String) args[1];
        String locatorSeparator = (String) args[2];
        int comparisonResult = (int) args[3];
        var elementLocator = args[4];
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
        message.append(validationTypeString).append(" [").append(validationMethodName).append("] ");
        if (validationMethodName.equals("ValidateFail")) {
            //validationState = ValidationState.PASSED;
            message.append(validationState).append(". ");
            message.append("Successfully force failed the test.");
        } else {
            message.append(validationState).append(". ");
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
                message.append(" Comparison Type [").append(validationComparisonOrComparativeRelationType).append("].");
            }

            if (validationType != null) {
                message.append(" Validation Type [").append(validationType).append("].");
            }
        }

        if (lastUsedDriver != null) {
            // create a screenshot attachment if needed for webdriver
            if (lastUsedElementLocator != null) {
                attachments.add(ScreenshotManager.captureScreenShot(lastUsedDriver, lastUsedElementLocator,
                        validationMethodName, validationState.getValue()));
            } else {
                attachments.add(ScreenshotManager.captureScreenShot(lastUsedDriver, validationMethodName,
                        validationState.getValue()));
            }
            // reset lastUsed variables
            lastUsedDriver = null;
            lastUsedElementLocator = null;
            //}
        } else if (lastUsedPage != null) {
            // create a screenshot attachment if needed for Playwright
//        if (expectedValue != null && expectedValue.toLowerCase().contains("locator")) {
            attachments.add(ScreenshotManager.captureScreenShot(lastUsedPage, Objects.requireNonNullElse(lastUsedElementLocatorString, ""), validationMethodName, validationState.getValue()));
            // reset lastUsed variables
            lastUsedPage = null;
            lastUsedElementLocatorString = null;
//        }
        }

        // handling changes as per validationCategory hard/soft
        switch (validationCategory) {
            case HARD_ASSERT -> {
                // create the log entry with or without attachments
                if (!attachments.isEmpty()) {
                    //ReportManagerHelper.log(message.toString(), attachments);
                    ReportManagerHelper.logNestedSteps(message.toString(), optionalCustomLogMessage, attachments);
                } else {
                    //ReportManager.log(message.toString());
                    ReportManagerHelper.logNestedSteps(message.toString(), optionalCustomLogMessage, null);
                }

                // set test state in case of failure
                if (!validationState.getValue()) {
                    if (failureReason != null) {
                        Assert.fail(message.toString(), failureReason);
                    } else {
                        Assert.fail(message.toString());
                    }
                }
            }
            case SOFT_ASSERT -> {
                // handle failure reason in case of soft assert
                if (failureReason != null) {
                    List<Object> failureReasonAttachment = Arrays.asList("Validation Test Data", "Failure Reason",
                            ReportManagerHelper.formatStackTraceToLogEntry(failureReason));
                    attachments.add(failureReasonAttachment);
                }

                // create the log entry with or without attachments
                if (!attachments.isEmpty()) {
//                    ReportManagerHelper.log(message.toString(), attachments);
                    ReportManagerHelper.logNestedSteps(message.toString(), optionalCustomLogMessage, attachments);
                } else {
//                    ReportManager.log(message.toString());
                    ReportManagerHelper.logNestedSteps(message.toString(), optionalCustomLogMessage, null);
                }

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

    private static void processCustomLogMessage(String... optionalCustomLogMessage) {
        ValidationsHelper.optionalCustomLogMessage = new ArrayList<>();
        for (String customMessage : optionalCustomLogMessage) {
            if (customMessage != null && !"".equals(customMessage.trim())) {
                ValidationsHelper.optionalCustomLogMessage.add(customMessage);
                //ReportManager.log(customMessage + "...");
            }
        }
    }
}
