package com.shaft.validation;

import com.shaft.api.RestActions.ComparisonType;
import com.shaft.validation.ValidationActions.ValidationComparisonType;
import com.shaft.validation.ValidationActions.ValidationType;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

@SuppressWarnings("unused")
public class Assertions {

    private Assertions() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Force fail the current test.
     *
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertFail(String... customLogMessage) {
        ValidationActions.validateFail(ValidationActions.ValidationCategory.HARD_ASSERT, customLogMessage);
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
        ValidationActions.validateEquals(ValidationActions.ValidationCategory.HARD_ASSERT, expectedValue, actualValue, ValidationComparisonType.EQUALS, ValidationType.POSITIVE,
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
     * @param assertionComparisonType AssertionComparisonType.EQUALS, CONTAINS,
     *                                MATCHES, CASE_INSENSITIVE
     * @param assertionType           AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void assertEquals(Object expectedValue, Object actualValue,
                                    AssertionComparisonType assertionComparisonType, AssertionType assertionType, String... customLogMessage) {

        ValidationActions.validateEquals(ValidationActions.ValidationCategory.HARD_ASSERT, expectedValue, actualValue,
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
        ValidationActions.validateNull(ValidationActions.ValidationCategory.HARD_ASSERT, object, ValidationType.POSITIVE, customLogMessage);
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
        ValidationActions.validateNull(ValidationActions.ValidationCategory.HARD_ASSERT, object, ValidationType.valueOf(assertionType.toString()), customLogMessage);
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
        ValidationActions.validateElementExists(ValidationActions.ValidationCategory.HARD_ASSERT, driver, elementLocator, ValidationType.POSITIVE, customLogMessage);
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
        ValidationActions.validateElementExists(ValidationActions.ValidationCategory.HARD_ASSERT, driver, elementLocator, ValidationType.valueOf(assertionType.toString()),
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
        ValidationActions.validateElementAttribute(ValidationActions.ValidationCategory.HARD_ASSERT, driver, elementLocator, elementAttribute, expectedValue,
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
     * @param assertionComparisonType AssertionComparisonType.EQUALS, CONTAINS,
     *                                MATCHES, CASE_INSENSITIVE
     * @param assertionType           AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void assertElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
                                              String expectedValue, AssertionComparisonType assertionComparisonType, AssertionType assertionType,
                                              String... customLogMessage) {
        ValidationActions.validateElementAttribute(ValidationActions.ValidationCategory.HARD_ASSERT, driver, elementLocator, elementAttribute, expectedValue,
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
        ValidationActions.validateElementCSSProperty(ValidationActions.ValidationCategory.HARD_ASSERT, driver, elementLocator, propertyName, expectedValue,
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
     * @param assertionComparisonType AssertionComparisonType.EQUALS, CONTAINS,
     *                                MATCHES, CASE_INSENSITIVE
     * @param assertionType           AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void assertElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
                                                String expectedValue, AssertionComparisonType assertionComparisonType, AssertionType assertionType,
                                                String... customLogMessage) {
        ValidationActions.validateElementCSSProperty(ValidationActions.ValidationCategory.HARD_ASSERT, driver, elementLocator, propertyName, expectedValue,
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
        ValidationActions.validateBrowserAttribute(ValidationActions.ValidationCategory.HARD_ASSERT, driver, browserAttribute, expectedValue, ValidationComparisonType.EQUALS,
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
     * @param assertionComparisonType AssertionComparisonType.EQUALS, CONTAINS,
     *                                MATCHES, CASE_INSENSITIVE
     * @param assertionType           AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void assertBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
                                              AssertionComparisonType assertionComparisonType, AssertionType assertionType, String... customLogMessage) {
        ValidationActions.validateBrowserAttribute(ValidationActions.ValidationCategory.HARD_ASSERT, driver, browserAttribute, expectedValue, ValidationComparisonType.valueOf(assertionComparisonType.toString()),
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
        ValidationActions.validateComparativeRelation(ValidationActions.ValidationCategory.HARD_ASSERT, expectedValue, actualValue, ValidationActions.ComparativeRelationType.valueOf(comparativeRelationType.toString()), ValidationType.POSITIVE, customLogMessage);
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
        ValidationActions.validateComparativeRelation(ValidationActions.ValidationCategory.HARD_ASSERT, expectedValue, actualValue, ValidationActions.ComparativeRelationType.valueOf(comparativeRelationType.toString()), ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts that a certain file exists. Attempts to find the file only once.
     *
     * @param fileFolderName   The location of the folder that contains the target
     *                         file, relative to the project's root folder, ending
     *                         with a /
     * @param fileName         The name of the target file (including its extension
     *                         if any)
     * @param customLogMessage a custom message that will appended to this
     *                         step in the execution report
     */
    public static void assertFileExists(String fileFolderName, String fileName,
                                        String... customLogMessage) {
        ValidationActions.validateFileExists(ValidationActions.ValidationCategory.HARD_ASSERT, fileFolderName, fileName, 1, ValidationType.POSITIVE, customLogMessage);
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
        ValidationActions.validateFileExists(ValidationActions.ValidationCategory.HARD_ASSERT, fileFolderName, fileName, numberOfRetries, ValidationType.POSITIVE, customLogMessage);
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
        ValidationActions.validateFileExists(ValidationActions.ValidationCategory.HARD_ASSERT, fileFolderName, fileName, numberOfRetries, ValidationType.valueOf(assertionType.toString()), customLogMessage);
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
        ValidationActions.validateTrue(ValidationActions.ValidationCategory.HARD_ASSERT, conditionalStatement, ValidationType.POSITIVE, customLogMessage);
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
        ValidationActions.validateTrue(ValidationActions.ValidationCategory.HARD_ASSERT, conditionalStatement, ValidationType.valueOf(assertionType.toString()), customLogMessage);
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
        ValidationActions.validateJSONFileContent(ValidationActions.ValidationCategory.HARD_ASSERT, response, referenceJsonFilePath, ComparisonType.EQUALS, "", ValidationType.POSITIVE, customLogMessage);
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
        ValidationActions.validateJSONFileContent(ValidationActions.ValidationCategory.HARD_ASSERT, response, referenceJsonFilePath, comparisonType, "", ValidationType.POSITIVE, customLogMessage);
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
        ValidationActions.validateJSONFileContent(ValidationActions.ValidationCategory.HARD_ASSERT, response, referenceJsonFilePath, comparisonType, "", ValidationType.valueOf(assertionType.toString()), customLogMessage);
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
        ValidationActions.validateJSONFileContent(ValidationActions.ValidationCategory.HARD_ASSERT, response, referenceJsonFilePath, comparisonType, jsonPathToTargetArray, ValidationType.POSITIVE, customLogMessage);
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
        ValidationActions.validateJSONFileContent(ValidationActions.ValidationCategory.HARD_ASSERT, response, referenceJsonFilePath, comparisonType, jsonPathToTargetArray, ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image. Uses OpenCV natively.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param customLogMessage a custom message that will appended to this step in
     *                         *                         the execution report
     */
    public static void assertElementMatches(WebDriver driver, By elementLocator,
                                            String... customLogMessage) {
        ValidationActions.validateElementMatches(ValidationActions.ValidationCategory.HARD_ASSERT, driver, elementLocator, ValidationActions.VisualValidationEngine.EXACT_OPENCV, ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Uses OpenCV natively.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param assertionType    AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage a custom message that will appended to this step in
     *                         *                         the execution report
     */
    public static void assertElementMatches(WebDriver driver, By elementLocator, AssertionType assertionType,
                                            String... customLogMessage) {
        ValidationActions.validateElementMatches(ValidationActions.ValidationCategory.HARD_ASSERT, driver, elementLocator, ValidationActions.VisualValidationEngine.EXACT_OPENCV, ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param driver                 the current instance of Selenium webdriver
     * @param elementLocator         the locator of the webElement under test (By xpath,
     *                               id, selector, name ...etc)
     * @param visualValidationEngine VisualValidationEngine.EXACT_OPENCV, EXACT_EYES, STRICT_EYES, CONTENT_EYES, LAYOUT_EYES
     * @param customLogMessage       a custom message that will appended to this step in
     *                               *                         the execution report
     */
    public static void assertElementMatches(WebDriver driver, By elementLocator, VisualValidationEngine visualValidationEngine,
                                            String... customLogMessage) {
        ValidationActions.validateElementMatches(ValidationActions.ValidationCategory.HARD_ASSERT, driver, elementLocator, ValidationActions.VisualValidationEngine.valueOf(visualValidationEngine.name()), ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if AssertionType is POSITIVE, or
     * doesn't match it if AssertionType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param driver                 the current instance of Selenium webdriver
     * @param elementLocator         the locator of the webElement under test (By xpath,
     *                               id, selector, name ...etc)
     * @param visualValidationEngine VisualValidationEngine.EXACT_OPENCV, EXACT_EYES, STRICT_EYES, CONTENT_EYES, LAYOUT_EYES
     * @param assertionType          AssertionType.POSITIVE, NEGATIVE
     * @param customLogMessage       a custom message that will appended to this step in
     *                               *                         the execution report
     */
    public static void assertElementMatches(WebDriver driver, By elementLocator, VisualValidationEngine visualValidationEngine, AssertionType assertionType,
                                            String... customLogMessage) {
        ValidationActions.validateElementMatches(ValidationActions.ValidationCategory.HARD_ASSERT, driver, elementLocator, ValidationActions.VisualValidationEngine.valueOf(visualValidationEngine.name()), ValidationType.valueOf(assertionType.toString()), customLogMessage);
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

    public enum VisualValidationEngine {
        EXACT_OPENCV,
        EXACT_EYES,
        STRICT_EYES,
        CONTENT_EYES,
        LAYOUT_EYES
    }
}