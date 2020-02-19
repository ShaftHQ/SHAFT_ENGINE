package com.shaft.validation;

import com.shaft.api.RestActions.ComparisonType;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.validation.Validations.ValidationComparisonType;
import com.shaft.validation.Validations.ValidationType;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;

import java.util.Collections;
import java.util.List;

//TODO: Assert Element matches reference file

//TODO: Add optional message to be added to the log of the assertion to describe what it does

public class Assertions {
    private static final String ERROR_INVALID_COMPARISON_OPERATOR = "Assertion Failed; invalid comparison operator used.";
    private static final String ERROR_UNHANDLED_EXCEPTION = "Assertion Failed; an unhandled exception occured.";

    private static final Boolean discreetLoggingState = Boolean.valueOf(System.getProperty("alwaysLogDiscreetly"));

    private Assertions() {
        throw new IllegalStateException("Utility class");
    }

    private static void fail(String message, Throwable realCause) {
        ReportManager.setDiscreteLogging(discreetLoggingState); // reset state in case of failure
        ReportManager.log(message);
        Assert.fail(message, realCause);
    }

    private static void fail(String message) {
        ReportManager.setDiscreteLogging(discreetLoggingState); // reset state in case of failure
        ReportManager.log(message);
        Assert.fail(message);
    }

    private static void fail(String message, List<List<Object>> attachments) {
        ReportManager.setDiscreteLogging(discreetLoggingState); // reset state in case of failure
        ReportManager.log(message, attachments);
        Assert.fail(message);
    }

    private static void fail(String actionName, WebDriver driver, String message) {
        fail(message, List.of(ScreenshotManager.captureScreenShot(driver, actionName, false)));
    }

    private static void pass(String message) {
        pass(message, null);
    }

    private static void pass(String message, List<List<Object>> attachments) {
        if (attachments != null) {
            ReportManager.log(message, attachments);
        } else {
            ReportManager.log(message);
        }
    }

    private static void pass(String actionName, WebDriver driver, String message) {
        pass(message, Collections.singletonList(ScreenshotManager.captureScreenShot(driver, actionName, true)));
    }

    /**
     * Force fail the current test.
     *
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertFail(String... customLogMessage) {
        Validations.assertFail(customLogMessage);
    }

    /**
     * Asserts that two objects are equal.
     *
     * @param expectedValue    the expected value (test data) of this assertion
     * @param actualValue      the actual value (calculated data) of this assertion
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertEquals(Object expectedValue, Object actualValue, String... customLogMessage) {
        Validations.assertEquals(expectedValue, actualValue, ValidationComparisonType.EQUALS, ValidationType.POSITIVE,
                customLogMessage);
    }

    /**
     * Asserts that two objects are equal if AssertionType is POSITIVE, or not equal
     * if AssertionType is NEGATIVE.
     *
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param actualValue             the actual value (calculated data) of this
     *                                assertion
     * @param assertionComparisonType AssertionComparisonType.LITERAL, CONTAINS,
     *                                REGEX, CASE_INSENSITIVE
     * @param assertionType           AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void assertEquals(Object expectedValue, Object actualValue,
                                    AssertionComparisonType assertionComparisonType, AssertionType assertionType, String... customLogMessage) {

        Validations.assertEquals(expectedValue, actualValue,
                ValidationComparisonType.valueOf(assertionComparisonType.toString()),
                ValidationType.valueOf(assertionType.toString()), customLogMessage);

    }

    /**
     * Asserts that the object is null.
     *
     * @param object           the object under test
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertNull(Object object, String... customLogMessage) {
        Validations.assertNull(object, ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that the object is null if AssertionType is POSITIVE, or is not null
     * if AssertionType is NEGATIVE.
     *
     * @param object           the object under test
     * @param assertionType    AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertNull(Object object, AssertionType assertionType, String... customLogMessage) {
        Validations.assertNull(object, ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts that the webElement found using the provided driver and locator
     * exists.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertElementExists(WebDriver driver, By elementLocator, String... customLogMessage) {
        Validations.assertElementExists(driver, elementLocator, ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that the webElement found using the provided driver and locator
     * exists if AssertionType is POSITIVE, or does not exist if AssertionType is
     * NEGATIVE.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param assertionType    AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertElementExists(WebDriver driver, By elementLocator, AssertionType assertionType,
                                           String... customLogMessage) {
        Validations.assertElementExists(driver, elementLocator, ValidationType.valueOf(assertionType.toString()),
                customLogMessage);
    }

    /**
     * Asserts webElement attribute equals expectedValue.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param elementAttribute the desired attribute of the webElement under test
     * @param expectedValue    the expected value (test data) of this assertion
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
                                              String expectedValue, String... customLogMessage) {
        Validations.assertElementAttribute(driver, elementLocator, elementAttribute, expectedValue,
                ValidationComparisonType.EQUALS, ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts webElement attribute equals expectedValue if AssertionType is
     * POSITIVE, or does not equal expectedValue if AssertionType is NEGATIVE.
     * Supports Text, TagName, Size, Other Attributes
     *
     * @param driver                  the current instance of Selenium webdriver
     * @param elementLocator          the locator of the webElement under test (By
     *                                xpath, id, selector, name ...etc)
     * @param elementAttribute        the desired attribute of the webElement under
     *                                test
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param assertionComparisonType AssertionComparisonType.LITERAL, CONTAINS,
     *                                REGEX, CASE_INSENSITIVE
     * @param assertionType           AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void assertElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
                                              String expectedValue, AssertionComparisonType assertionComparisonType, AssertionType assertionType,
                                              String... customLogMessage) {
        Validations.assertElementAttribute(driver, elementLocator, elementAttribute, expectedValue,
                ValidationComparisonType.valueOf(assertionComparisonType.toString()),
                ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts webElement CSSProperty equals expectedValue.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param propertyName     the target CSS property of the webElement under test
     * @param expectedValue    the expected value (test data) of this assertion
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
                                                String expectedValue, String... customLogMessage) {
        Validations.assertElementCSSProperty(driver, elementLocator, propertyName, expectedValue,
                ValidationComparisonType.EQUALS, ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts webElement CSSProperty equals expectedValue if AssertionType is
     * POSITIVE, or does not equal expectedValue if AssertionType is NEGATIVE.
     *
     * @param driver                  the current instance of Selenium webdriver
     * @param elementLocator          the locator of the webElement under test (By
     *                                xpath, id, selector, name ...etc)
     * @param propertyName            the target CSS property of the webElement
     *                                under test
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param assertionComparisonType AssertionComparisonType.LITERAL, CONTAINS,
     *                                REGEX, CASE_INSENSITIVE
     * @param assertionType           AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void assertElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
                                                String expectedValue, AssertionComparisonType assertionComparisonType, AssertionType assertionType,
                                                String... customLogMessage) {
        Validations.assertElementCSSProperty(driver, elementLocator, propertyName, expectedValue,
                ValidationComparisonType.valueOf(assertionComparisonType.toString()),
                ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts browser attribute equals expectedValue. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize
     *
     * @param driver           the current instance of Selenium webdriver
     * @param browserAttribute the desired attribute of the browser window
     *                         under test
     * @param expectedValue    the expected value (test data) of this
     *                         assertion
     * @param customLogMessage a custom message that will appended to this
     *                         step in the execution report
     */
    public static void assertBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
                                              String... customLogMessage) {
        Validations.assertBrowserAttribute(driver, browserAttribute, expectedValue, ValidationComparisonType.EQUALS,
                ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts browser attribute equals expectedValue if AssertionType is POSITIVE,
     * or does not equal expectedValue if AssertionType is NEGATIVE. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize
     *
     * @param driver                  the current instance of Selenium webdriver
     * @param browserAttribute        the desired attribute of the browser window
     *                                under test
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param assertionComparisonType AssertionComparisonType.LITERAL, CONTAINS,
     *                                REGEX, CASE_INSENSITIVE
     * @param assertionType           AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void assertBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
                                              AssertionComparisonType assertionComparisonType, AssertionType assertionType, String... customLogMessage) {
        Validations.assertBrowserAttribute(driver, browserAttribute, expectedValue, ValidationComparisonType.valueOf(assertionComparisonType.toString()),
                ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts that the expectedValue is related to the actualValue using the
     * desired comparativeRelationType.
     *
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param actualValue             the actual value (calculated data) of this
     *                                assertion
     * @param comparativeRelationType assertComparativeRelation.GREATER_THAN,
     *                                GREATER_THAN_OR_EQUALS, LESS_THAN,
     *                                LESS_THAN_OR_EQUALS, EQUALS
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void assertComparativeRelation(Number expectedValue, Number actualValue,
                                                 ComparativeRelationType comparativeRelationType, String... customLogMessage) {
        Validations.assertComparativeRelation(expectedValue, actualValue, Validations.ComparativeRelationType.valueOf(comparativeRelationType.toString()), ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that the expectedValue is related to the actualValue using the
     * desired comparativeRelationType if AssertionType is POSITIVE, or not related
     * if AssertionType is NEGATIVE.
     *
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param actualValue             the actual value (calculated data) of this
     *                                assertion
     * @param comparativeRelationType assertComparativeRelation.GREATER_THAN,
     *                                GREATER_THAN_OR_EQUALS, LESS_THAN,
     *                                LESS_THAN_OR_EQUALS, EQUALS
     * @param assertionType           AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void assertComparativeRelation(Number expectedValue, Number actualValue,
                                                 ComparativeRelationType comparativeRelationType, AssertionType assertionType, String... customLogMessage) {
        Validations.assertComparativeRelation(expectedValue, actualValue, Validations.ComparativeRelationType.valueOf(comparativeRelationType.toString()), ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts that a certain file exists. Attempts to find the file only once.
     *
     * @param fileFolderName The location of the folder that contains the target
     *                       file, relative to the project's root folder, ending
     *                       with a /
     * @param fileName       The name of the target file (including its extension
     *                       if any)
     */
    public static void assertFileExists(String fileFolderName, String fileName,
                                        String... customLogMessage) {
        Validations.assertFileExists(fileFolderName, fileName, 1, ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that a certain file exists. Attempts to find the file for the desired NumberOfRetries.
     *
     * @param fileFolderName   The location of the folder that contains the target
     *                         file, relative to the project's root folder, ending
     *                         with a /
     * @param fileName         The name of the target file (including its extension
     *                         if any)
     * @param numberOfRetries  number of times to try to find the file, given that
     *                         each retry is separated by a 500 millisecond wait time
     * @param customLogMessage a custom message that will appended to this
     *                         step in the execution report
     */
    public static void assertFileExists(String fileFolderName, String fileName, int numberOfRetries,
                                        String... customLogMessage) {
        Validations.assertFileExists(fileFolderName, fileName, numberOfRetries, ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that a certain file exists if AssertionType is POSITIVE, or doesn't
     * exist if AssertionType is NEGATIVE. Attempts to find the file for the desired NumberOfRetries.
     *
     * @param fileFolderName   The location of the folder that contains the target
     *                         file, relative to the project's root folder, ending
     *                         with a /
     * @param fileName         The name of the target file (including its extension
     *                         if any)
     * @param numberOfRetries  number of times to try to find the file, given that
     *                         each retry is separated by a 500 millisecond wait time
     * @param assertionType    AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage a custom message that will appended to this
     *                         step in the execution report
     */
    public static void assertFileExists(String fileFolderName, String fileName, int numberOfRetries,
                                        AssertionType assertionType, String... customLogMessage) {
        Validations.assertFileExists(fileFolderName, fileName, numberOfRetries, ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts that the provided conditional statement evaluates to true.
     *
     * @param conditionalStatement the statement that will be evaluated to see if it
     *                             matches the expected result
     * @param customLogMessage     a custom message that will appended to this
     *                             step in the execution report
     */
    public static void assertTrue(Boolean conditionalStatement, String... customLogMessage) {
        Validations.assertTrue(conditionalStatement, ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that the provided conditional statement evaluates to true if
     * AssertionType is POSITIVE, or to false if AssertionType is NEGATIVE.
     *
     * @param conditionalStatement the statement that will be evaluated to see if it
     *                             matches the expected result
     * @param assertionType        AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage     a custom message that will appended to this
     *                             step in the execution report
     */
    public static void assertTrue(Boolean conditionalStatement, AssertionType assertionType, String... customLogMessage) {
        Validations.assertTrue(conditionalStatement, ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts that the API Response object
     * matches the expected referenceJsonFile.
     *
     * @param response              the full response object returned by
     *                              performRequest method.
     * @param referenceJsonFilePath the full absolute path to the test data file
     *                              that will be used as a reference for this
     *                              comparison
     * @param customLogMessage      a custom message that will appended to this
     *                              step in the execution report
     */
    public static void assertJSONFileContent(Response response, String referenceJsonFilePath, String... customLogMessage) {
        Validations.assertJSONFileContent(response, referenceJsonFilePath, ComparisonType.EQUALS, "", ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that the API Response object
     * matches the expected referenceJsonFile.
     *
     * @param response              the full response object returned by
     *                              performRequest method.
     * @param referenceJsonFilePath the full absolute path to the test data file
     *                              that will be used as a reference for this
     *                              comparison
     * @param comparisonType        ComparisonType.EQUALS, CONTAINS, MATCHES,
     *                              EQUALS_STRICT; Note that MATCHES ignores the
     *                              content ordering inside the JSON
     * @param customLogMessage      a custom message that will appended to this
     *                              step in the execution report
     */
    public static void assertJSONFileContent(Response response, String referenceJsonFilePath,
                                             ComparisonType comparisonType, String... customLogMessage) {
        Validations.assertJSONFileContent(response, referenceJsonFilePath, comparisonType, "", ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that the API Response object
     * matches the expected referenceJsonFile if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE.
     *
     * @param response              the full response object returned by
     *                              performRequest method.
     * @param referenceJsonFilePath the full absolute path to the test data file
     *                              that will be used as a reference for this
     *                              comparison
     * @param comparisonType        ComparisonType.EQUALS, CONTAINS, MATCHES,
     *                              EQUALS_STRICT; Note that MATCHES ignores the
     *                              content ordering inside the JSON
     * @param assertionType         AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage      a custom message that will appended to this
     *                              step in the execution report
     */
    public static void assertJSONFileContent(Response response, String referenceJsonFilePath,
                                             ComparisonType comparisonType, AssertionType assertionType, String... customLogMessage) {
        Validations.assertJSONFileContent(response, referenceJsonFilePath, comparisonType, "", ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }


    /**
     * Asserts that the target array extracted by parsing the API Response object
     * matches the expected referenceJsonFile.
     *
     * @param response              the full response object returned by
     *                              performRequest method.
     * @param referenceJsonFilePath the full absolute path to the test data file
     *                              that will be used as a reference for this
     *                              comparison
     * @param comparisonType        ComparisonType.EQUALS, CONTAINS, MATCHES,
     *                              EQUALS_STRICT; Note that MATCHES ignores the
     *                              content ordering inside the JSON
     * @param jsonPathToTargetArray a jsonpath that will be parsed to point to the
     *                              target JSON Array
     * @param customLogMessage      a custom message that will appended to this
     *                              step in the execution report
     */
    public static void assertJSONFileContent(Response response, String referenceJsonFilePath,
                                             ComparisonType comparisonType, String jsonPathToTargetArray, String... customLogMessage) {
        Validations.assertJSONFileContent(response, referenceJsonFilePath, comparisonType, jsonPathToTargetArray, ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that the target array extracted by parsing the API Response object
     * matches the expected referenceJsonFile if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE.
     *
     * @param response              the full response object returned by
     *                              performRequest method.
     * @param referenceJsonFilePath the full absolute path to the test data file
     *                              that will be used as a reference for this
     *                              comparison
     * @param comparisonType        ComparisonType.EQUALS, CONTAINS, MATCHES,
     *                              EQUALS_STRICT; Note that MATCHES ignores the
     *                              content ordering inside the JSON
     * @param jsonPathToTargetArray a jsonpath that will be parsed to point to the
     *                              target JSON Array
     * @param assertionType         AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage      a custom message that will appended to this
     *                              step in the execution report
     */
    public static void assertJSONFileContent(Response response, String referenceJsonFilePath,
                                             ComparisonType comparisonType, String jsonPathToTargetArray, AssertionType assertionType, String... customLogMessage) {
        Validations.assertJSONFileContent(response, referenceJsonFilePath, comparisonType, jsonPathToTargetArray, ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    public enum AssertionType {
        POSITIVE(true), NEGATIVE(false);

        private final Boolean value;

        AssertionType(Boolean type) {
            this.value = type;
        }

        protected boolean getValue() {
            return value;
        }
    }

    public enum AssertionComparisonType {
        EQUALS(1), CONTAINS(3), MATCHES(2), CASE_INSENSITIVE(4);

        private final int value;

        AssertionComparisonType(int type) {
            this.value = type;
        }

        protected int getValue() {
            return value;
        }
    }

    public enum ComparativeRelationType {
        GREATER_THAN(">"), GREATER_THAN_OR_EQUALS(">="), LESS_THAN("<"), LESS_THAN_OR_EQUALS("<="), EQUALS("==");

        private final String value;

        ComparativeRelationType(String type) {
            this.value = type;
        }

        protected String getValue() {
            return value;
        }
    }
}