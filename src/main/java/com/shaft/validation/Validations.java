package com.shaft.validation;

import com.shaft.gui.element.ElementActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.support.JavaActions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class Validations {
    /*
     * Variables
     */
    private static final int attemptsBeforeThrowingElementNotFoundException = Integer
            .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static final int attemptsBeforeThrowingElementNotFoundExceptionInCaseElementShouldntExist = 1;
    private static WebDriver lastUsedDriver = null;
    private static By lastUsedElementLocator = null;

    private static Boolean discreetLoggingState = Boolean.valueOf(System.getProperty("alwaysLogDiscreetly"));

    // TODO: refactor to base validations class, and implementing assertions and
    // verifications classes

    /*
     * Constructor
     */
    private Validations() {
        throw new IllegalStateException("Utility class");
    }

    /*
     * Reporting Methods
     */
    private static void pass(String expectedValue, String actualValue,
                             ValidationComparisonType validationComparisonType, ValidationType validationType) {

        reportValidationState(expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.PASSED, null);
    }

    private static void fail(String expectedValue, String actualValue,
                             ValidationComparisonType validationComparisonType, ValidationType validationType, Throwable failureReason) {
        // reset state in case of failure to force reporting the failure
        ReportManager.setDiscreteLogging(discreetLoggingState);

        reportValidationState(expectedValue, actualValue, validationComparisonType, validationType,
                ValidationState.FAILED, failureReason);
    }

    private static void reportValidationState(String expectedValue, String actualValue,
                                              ValidationComparisonType validationComparisonType, ValidationType validationType,
                                              ValidationState validationState, Throwable failureReason) {

        // prepare message and attachments
        StringBuilder message = new StringBuilder();
        List<List<Object>> attachments = new ArrayList<>();

        // get validation method name
        String validationMethodName = (new Throwable()).getStackTrace()[2].getMethodName();
        if (validationMethodName.equals("reportAssertionResultForElementPropertyOrAttribute")) {
            validationMethodName = (new Throwable()).getStackTrace()[4].getMethodName();
        }
        validationMethodName = validationMethodName.substring(0, 1).toUpperCase() + validationMethodName.substring(1);
        if (validationMethodName.equals("AssertFail")) {
            validationState = ValidationState.PASSED;
            message.append(validationMethodName + " " + validationState + "; ");
            message.append("Successfully force failed the test.");
        } else {
            message.append(validationMethodName + " " + validationState + "; ");
            // prepare expected/actual results as an attachment or in the message
            Boolean isExpectedOrActualValueLong = false;
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
                message.append("Expected [" + expectedValue + "] and Actual [" + actualValue + "].");
            }

            if (validationComparisonType != null && validationType != null) {
                message.append(
                        " [" + validationComparisonType + "] Comparison, and [" + validationType + "] Validation.");
            }
        }

        // create a screenshot attachment if needed
        if (expectedValue != null && expectedValue.toLowerCase().contains("locator")) {
            if (lastUsedDriver != null && lastUsedElementLocator != null) {
                attachments.add(ScreenshotManager.captureScreenShot(lastUsedDriver, lastUsedElementLocator,
                        validationMethodName, validationState.getValue()));
            } else if (lastUsedDriver != null && lastUsedElementLocator == null) {
                attachments.add(ScreenshotManager.captureScreenShot(lastUsedDriver, validationMethodName,
                        validationState.getValue()));
            }
            // reset lastUsed variables
            lastUsedDriver = null;
            lastUsedElementLocator = null;
        }

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
    }

    protected static void assertFail(String... optionalCustomLogMessage) {
        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }
        fail(null, null, null, null, null);
    }

    protected static void assertEquals(Object expectedValue, Object actualValue,
                                       ValidationComparisonType validationComparisonType, ValidationType validationType,
                                       String... optionalCustomLogMessage) {

        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        if (JavaActions.compareTwoObjects(expectedValue, actualValue, validationComparisonType.getValue(),
                validationType.getValue()) == 1) {
            pass(String.valueOf(expectedValue), String.valueOf(actualValue), validationComparisonType, validationType);
        } else {
            // failed comparison, invalid operator (not reachable) or exception
            fail(String.valueOf(expectedValue), String.valueOf(actualValue), validationComparisonType, validationType,
                    null);
        }
    }

    protected static void assertNull(Object object, ValidationType validationType, String... optionalCustomLogMessage) {

        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        if (validationType.getValue()) {
            try {
                Assert.assertNull(object);
                pass("NULL", "NULL", ValidationComparisonType.EQUALS, validationType);
            } catch (Exception | AssertionError failureReason) {
                fail("NULL", String.valueOf(object), ValidationComparisonType.EQUALS, validationType, failureReason);
            }
        } else {
            try {
                Assert.assertNotNull(object);
                pass("NULL", String.valueOf(object), ValidationComparisonType.EQUALS, validationType);
            } catch (Exception | AssertionError failureReason) {
                fail("NULL", "NULL", ValidationComparisonType.EQUALS, validationType, failureReason);
            }
        }
    }

    protected static void assertElementExists(WebDriver driver, By elementLocator, ValidationType validationType,
                                              String... optionalCustomLogMessage) {

        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        int customAttempts = attemptsBeforeThrowingElementNotFoundException;
        if (!validationType.getValue()) {
            customAttempts = attemptsBeforeThrowingElementNotFoundExceptionInCaseElementShouldntExist;
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
            switch (elementsCount) {
                case 0:
                    fail(expectedElementStates[0] + locatorSeparator + elementLocator.toString() + "'",
                            actualElementStates[1], ValidationComparisonType.EQUALS, validationType, null);
                    break;

                case 1:
                    pass(expectedElementStates[0] + locatorSeparator + elementLocator.toString() + "'",
                            actualElementStates[0], ValidationComparisonType.EQUALS, validationType);
                    break;

                default:
                    fail(expectedElementStates[0] + locatorSeparator + elementLocator.toString() + "'",
                            actualElementStates[2], ValidationComparisonType.EQUALS, validationType, null);
                    break;
            }
        } else {
            // not expecting the element to be present
            switch (elementsCount) {
                case 0:
                    pass(expectedElementStates[1] + locatorSeparator + elementLocator.toString() + "'",
                            actualElementStates[1], ValidationComparisonType.EQUALS, validationType);
                    break;

                case 1:
                    fail(expectedElementStates[1] + locatorSeparator + elementLocator.toString() + "'",
                            actualElementStates[0], ValidationComparisonType.EQUALS, validationType, null);
                    break;

                default:
                    fail(expectedElementStates[1] + locatorSeparator + elementLocator.toString() + "'",
                            actualElementStates[2], ValidationComparisonType.EQUALS, validationType, null);
                    break;
            }
        }
    }

    /*
     * Core Methods
     */

    protected static void assertElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
                                                 String expectedValue, ValidationComparisonType validationComparisonType, ValidationType validationType,
                                                 String... optionalCustomLogMessage) {

        for (String customMessage : optionalCustomLogMessage) {
            ReportManager.log(customMessage + "...");
        }

        String[] expectedAttributeStates = {"Value Should be", "Value Should not be"};
        String attributeSeparator = "' for the '";
        String locatorSeparator = "' attribute, element locator '";

        String actualValue = null;
        try {
            discreetLoggingState = ReportManager.isDiscreteLogging();
            ReportManager.setDiscreteLogging(true);
            switch (elementAttribute.toLowerCase()) {
                case "text":
                    actualValue = ElementActions.getText(driver, elementLocator);
                    break;
                case "tagname":
                    actualValue = ElementActions.getTagName(driver, elementLocator);
                    break;
                case "size":
                    actualValue = ElementActions.getSize(driver, elementLocator);
                    break;
                default:
                    actualValue = ElementActions.getAttribute(driver, elementLocator, elementAttribute);
                    break;
            }
            ReportManager.setDiscreteLogging(discreetLoggingState);
        } catch (AssertionError e) {
            // force fail due to upstream failure
            if (validationType.getValue()) {
                fail(expectedAttributeStates[0] + " '" + expectedValue + attributeSeparator + elementAttribute
                                + locatorSeparator + elementLocator.toString() + "'",
                        "Failed to read the desired element attribute", validationComparisonType, validationType, e);
            } else {
                fail(expectedAttributeStates[1] + " '" + expectedValue + attributeSeparator + elementAttribute
                                + locatorSeparator + elementLocator.toString() + "'",
                        "Failed to read the desired element attribute", validationComparisonType, validationType, e);
            }
        }

        lastUsedDriver = driver;
        lastUsedElementLocator = elementLocator;
        int comparisonResult = JavaActions.compareTwoObjects(expectedValue, actualValue,
                validationComparisonType.getValue(), validationType.getValue());

        reportAssertionResultForElementPropertyOrAttribute(new Object[]{expectedAttributeStates, attributeSeparator,
                locatorSeparator, comparisonResult, elementLocator, elementAttribute, expectedValue, actualValue,
                validationComparisonType, validationType});
    }

    protected static void assertElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
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

        reportAssertionResultForElementPropertyOrAttribute(new Object[]{expectedAttributeStates, propertySeparator,
                locatorSeparator, comparisonResult, elementLocator, propertyName, expectedValue, actualValue,
                validationComparisonType, validationType});

    }

    private static void reportAssertionResultForElementPropertyOrAttribute(Object[] args) {

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

        if (validationType.getValue()) {
            // expecting element attribute to have the correct value
            if (comparisonResult == 1) {
                // match
                pass(expectedAttributeStates[0] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + elementLocator.toString() + "'", actualValue, validationComparisonType,
                        validationType);
            } else {
                // no match, or unhandled issue
                fail(expectedAttributeStates[0] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + elementLocator.toString() + "'", actualValue, validationComparisonType,
                        validationType, null);
            }
        } else {
            // expecting element attribute to not have the correct value
            if (comparisonResult == 1) {
                // match
                pass(expectedAttributeStates[1] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + elementLocator.toString() + "'", actualValue, validationComparisonType,
                        validationType);
            } else {
                // no match, or unhandled issue
                fail(expectedAttributeStates[1] + " '" + expectedValue + propertySeparator + propertyName
                                + locatorSeparator + elementLocator.toString() + "'", actualValue, validationComparisonType,
                        validationType, null);
            }
        }
    }

    /*
     * Enums
     */
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
