package com.shaft.validation;

import com.shaft.api.RestActions;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

@SuppressWarnings("unused")
public class Verifications {

    private Verifications() {
        throw new IllegalStateException("Utility class");
    }

    public static AssertionError getVerificationErrorToForceFail() {
        return ValidationActions.getVerificationErrorToForceFail();
    }

    public static void resetVerificationStateAfterFailing() {
        ValidationActions.resetVerificationStateAfterFailing();
    }

    /**
     * Force fail the current test.
     *
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyFail(String... customLogMessage) {
        ValidationActions.validateFail(ValidationActions.ValidationCategory.SOFT_ASSERT, customLogMessage);
    }

    /**
     * verifies that two objects are equal.
     *
     * @param expectedValue    the expected value (test data) of this verification
     * @param actualValue      the actual value (calculated data) of this verification
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyEquals(Object expectedValue, Object actualValue, String... customLogMessage) {
        ValidationActions.validateEquals(ValidationActions.ValidationCategory.SOFT_ASSERT, expectedValue, actualValue, ValidationActions.ValidationComparisonType.EQUALS, ValidationActions.ValidationType.POSITIVE,
                customLogMessage);
    }

    /**
     * verifies that two objects are equal if verificationType is POSITIVE, or not equal
     * if verificationType is NEGATIVE.
     *
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param actualValue                the actual value (calculated data) of this
     *                                   verification
     * @param verificationComparisonType verificationComparisonType.EQUALS, CONTAINS,
     *                                   MATCHES, CASE_INSENSITIVE
     * @param verificationType           verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage           a custom message that will appended to this
     *                                   step in the execution report
     */
    public static void verifyEquals(Object expectedValue, Object actualValue,
                                    Verifications.VerificationComparisonType verificationComparisonType, Verifications.VerificationType verificationType, String... customLogMessage) {

        ValidationActions.validateEquals(ValidationActions.ValidationCategory.SOFT_ASSERT, expectedValue, actualValue,
                ValidationActions.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);

    }

    /**
     * verifies that the object is null.
     *
     * @param object           the object under test
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyNull(Object object, String... customLogMessage) {
        ValidationActions.validateNull(ValidationActions.ValidationCategory.SOFT_ASSERT, object, ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the object is null if verificationType is POSITIVE, or is not null
     * if verificationType is NEGATIVE.
     *
     * @param object           the object under test
     * @param verificationType verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyNull(Object object, Verifications.VerificationType verificationType, String... customLogMessage) {
        ValidationActions.validateNull(ValidationActions.ValidationCategory.SOFT_ASSERT, object, ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies that the webElement found using the provided driver and locator
     * exists.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyElementExists(WebDriver driver, By elementLocator, String... customLogMessage) {
        ValidationActions.validateElementExists(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the webElement found using the provided driver and locator
     * exists if verificationType is POSITIVE, or does not exist if verificationType is
     * NEGATIVE.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param verificationType verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyElementExists(WebDriver driver, By elementLocator, Verifications.VerificationType verificationType,
                                           String... customLogMessage) {
        ValidationActions.validateElementExists(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationActions.ValidationType.valueOf(verificationType.toString()),
                customLogMessage);
    }

    /**
     * verifies webElement attribute equals expectedValue.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param elementAttribute the desired attribute of the webElement under test
     * @param expectedValue    the expected value (test data) of this verification
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
                                              String expectedValue, String... customLogMessage) {
        ValidationActions.validateElementAttribute(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, elementLocator, elementAttribute, expectedValue,
                ValidationActions.ValidationComparisonType.EQUALS, ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies webElement attribute equals expectedValue if verificationType is
     * POSITIVE, or does not equal expectedValue if verificationType is NEGATIVE.
     * Supports Text, TagName, Size, Other Attributes
     *
     * @param driver                     the current instance of Selenium webdriver
     * @param elementLocator             the locator of the webElement under test (By
     *                                   xpath, id, selector, name ...etc)
     * @param elementAttribute           the desired attribute of the webElement under
     *                                   test
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param verificationComparisonType verificationComparisonType.EQUALS, CONTAINS,
     *                                   MATCHES, CASE_INSENSITIVE
     * @param verificationType           verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage           a custom message that will appended to this
     *                                   step in the execution report
     */
    public static void verifyElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
                                              String expectedValue, Verifications.VerificationComparisonType verificationComparisonType, Verifications.VerificationType verificationType,
                                              String... customLogMessage) {
        ValidationActions.validateElementAttribute(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, elementLocator, elementAttribute, expectedValue,
                ValidationActions.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies webElement CSSProperty equals expectedValue.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param propertyName     the target CSS property of the webElement under test
     * @param expectedValue    the expected value (test data) of this verification
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
                                                String expectedValue, String... customLogMessage) {
        ValidationActions.validateElementCSSProperty(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, elementLocator, propertyName, expectedValue,
                ValidationActions.ValidationComparisonType.EQUALS, ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies webElement CSSProperty equals expectedValue if verificationType is
     * POSITIVE, or does not equal expectedValue if verificationType is NEGATIVE.
     *
     * @param driver                     the current instance of Selenium webdriver
     * @param elementLocator             the locator of the webElement under test (By
     *                                   xpath, id, selector, name ...etc)
     * @param propertyName               the target CSS property of the webElement
     *                                   under test
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param verificationComparisonType verificationComparisonType.EQUALS, CONTAINS,
     *                                   MATCHES, CASE_INSENSITIVE
     * @param verificationType           verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage           a custom message that will appended to this
     *                                   step in the execution report
     */
    public static void verifyElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
                                                String expectedValue, Verifications.VerificationComparisonType verificationComparisonType, Verifications.VerificationType verificationType,
                                                String... customLogMessage) {
        ValidationActions.validateElementCSSProperty(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, elementLocator, propertyName, expectedValue,
                ValidationActions.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies browser attribute equals expectedValue. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize
     *
     * @param driver           the current instance of Selenium webdriver
     * @param browserAttribute the desired attribute of the browser window
     *                         under test
     * @param expectedValue    the expected value (test data) of this
     *                         verification
     * @param customLogMessage a custom message that will appended to this
     *                         step in the execution report
     */
    public static void verifyBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
                                              String... customLogMessage) {
        ValidationActions.validateBrowserAttribute(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, browserAttribute, expectedValue, ValidationActions.ValidationComparisonType.EQUALS,
                ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies browser attribute equals expectedValue if verificationType is POSITIVE,
     * or does not equal expectedValue if verificationType is NEGATIVE. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize
     *
     * @param driver                     the current instance of Selenium webdriver
     * @param browserAttribute           the desired attribute of the browser window
     *                                   under test
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param verificationComparisonType verificationComparisonType.EQUALS, CONTAINS,
     *                                   MATCHES, CASE_INSENSITIVE
     * @param verificationType           verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage           a custom message that will appended to this
     *                                   step in the execution report
     */
    public static void verifyBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
                                              Verifications.VerificationComparisonType verificationComparisonType, Verifications.VerificationType verificationType, String... customLogMessage) {
        ValidationActions.validateBrowserAttribute(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, browserAttribute, expectedValue, ValidationActions.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies that the expectedValue is related to the actualValue using the
     * desired comparativeRelationType.
     *
     * @param expectedValue           the expected value (test data) of this
     *                                verification
     * @param actualValue             the actual value (calculated data) of this
     *                                verification
     * @param comparativeRelationType verifyComparativeRelation.GREATER_THAN,
     *                                GREATER_THAN_OR_EQUALS, LESS_THAN,
     *                                LESS_THAN_OR_EQUALS, EQUALS
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void verifyComparativeRelation(Number expectedValue, Number actualValue,
                                                 Verifications.ComparativeRelationType comparativeRelationType, String... customLogMessage) {
        ValidationActions.validateComparativeRelation(ValidationActions.ValidationCategory.SOFT_ASSERT, expectedValue, actualValue, ValidationActions.ComparativeRelationType.valueOf(comparativeRelationType.toString()), ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the expectedValue is related to the actualValue using the
     * desired comparativeRelationType if verificationType is POSITIVE, or not related
     * if verificationType is NEGATIVE.
     *
     * @param expectedValue           the expected value (test data) of this
     *                                verification
     * @param actualValue             the actual value (calculated data) of this
     *                                verification
     * @param comparativeRelationType verifyComparativeRelation.GREATER_THAN,
     *                                GREATER_THAN_OR_EQUALS, LESS_THAN,
     *                                LESS_THAN_OR_EQUALS, EQUALS
     * @param verificationType        verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage        a custom message that will appended to this
     *                                step in the execution report
     */
    public static void verifyComparativeRelation(Number expectedValue, Number actualValue,
                                                 Verifications.ComparativeRelationType comparativeRelationType, Verifications.VerificationType verificationType, String... customLogMessage) {
        ValidationActions.validateComparativeRelation(ValidationActions.ValidationCategory.SOFT_ASSERT, expectedValue, actualValue, ValidationActions.ComparativeRelationType.valueOf(comparativeRelationType.toString()), ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies that a certain file exists. Attempts to find the file only once.
     *
     * @param fileFolderName   The location of the folder that contains the target
     *                         file, relative to the project's root folder, ending
     *                         with a /
     * @param fileName         The name of the target file (including its extension
     *                         if any)
     * @param customLogMessage a custom message that will appended to this
     *                         step in the execution report
     */
    public static void verifyFileExists(String fileFolderName, String fileName,
                                        String... customLogMessage) {
        ValidationActions.validateFileExists(ValidationActions.ValidationCategory.SOFT_ASSERT, fileFolderName, fileName, 1, ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that a certain file exists. Attempts to find the file for the desired NumberOfRetries.
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
    public static void verifyFileExists(String fileFolderName, String fileName, int numberOfRetries,
                                        String... customLogMessage) {
        ValidationActions.validateFileExists(ValidationActions.ValidationCategory.SOFT_ASSERT, fileFolderName, fileName, numberOfRetries, ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that a certain file exists if verificationType is POSITIVE, or doesn't
     * exist if verificationType is NEGATIVE. Attempts to find the file for the desired NumberOfRetries.
     *
     * @param fileFolderName   The location of the folder that contains the target
     *                         file, relative to the project's root folder, ending
     *                         with a /
     * @param fileName         The name of the target file (including its extension
     *                         if any)
     * @param numberOfRetries  number of times to try to find the file, given that
     *                         each retry is separated by a 500 millisecond wait time
     * @param verificationType verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage a custom message that will appended to this
     *                         step in the execution report
     */
    public static void verifyFileExists(String fileFolderName, String fileName, int numberOfRetries,
                                        Verifications.VerificationType verificationType, String... customLogMessage) {
        ValidationActions.validateFileExists(ValidationActions.ValidationCategory.SOFT_ASSERT, fileFolderName, fileName, numberOfRetries, ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies that the provided conditional statement evaluates to true.
     *
     * @param conditionalStatement the statement that will be evaluated to see if it
     *                             matches the expected result
     * @param customLogMessage     a custom message that will appended to this
     *                             step in the execution report
     */
    public static void verifyTrue(Boolean conditionalStatement, String... customLogMessage) {
        ValidationActions.validateTrue(ValidationActions.ValidationCategory.SOFT_ASSERT, conditionalStatement, ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the provided conditional statement evaluates to true if
     * verificationType is POSITIVE, or to false if verificationType is NEGATIVE.
     *
     * @param conditionalStatement the statement that will be evaluated to see if it
     *                             matches the expected result
     * @param verificationType     verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage     a custom message that will appended to this
     *                             step in the execution report
     */
    public static void verifyTrue(Boolean conditionalStatement, Verifications.VerificationType verificationType, String... customLogMessage) {
        ValidationActions.validateTrue(ValidationActions.ValidationCategory.SOFT_ASSERT, conditionalStatement, ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies that the API Response object
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
    public static void verifyJSONFileContent(Response response, String referenceJsonFilePath, String... customLogMessage) {
        ValidationActions.validateJSONFileContent(ValidationActions.ValidationCategory.SOFT_ASSERT, response, referenceJsonFilePath, RestActions.ComparisonType.EQUALS, "", ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the API Response object
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
    public static void verifyJSONFileContent(Response response, String referenceJsonFilePath,
                                             RestActions.ComparisonType comparisonType, String... customLogMessage) {
        ValidationActions.validateJSONFileContent(ValidationActions.ValidationCategory.SOFT_ASSERT, response, referenceJsonFilePath, comparisonType, "", ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the API Response object
     * matches the expected referenceJsonFile if verificationType is POSITIVE, or
     * doesn't match it if verificationType is NEGATIVE.
     *
     * @param response              the full response object returned by
     *                              performRequest method.
     * @param referenceJsonFilePath the full absolute path to the test data file
     *                              that will be used as a reference for this
     *                              comparison
     * @param comparisonType        ComparisonType.EQUALS, CONTAINS, MATCHES,
     *                              EQUALS_STRICT; Note that MATCHES ignores the
     *                              content ordering inside the JSON
     * @param verificationType      verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage      a custom message that will appended to this
     *                              step in the execution report
     */
    public static void verifyJSONFileContent(Response response, String referenceJsonFilePath,
                                             RestActions.ComparisonType comparisonType, Verifications.VerificationType verificationType, String... customLogMessage) {
        ValidationActions.validateJSONFileContent(ValidationActions.ValidationCategory.SOFT_ASSERT, response, referenceJsonFilePath, comparisonType, "", ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }


    /**
     * verifies that the target array extracted by parsing the API Response object
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
    public static void verifyJSONFileContent(Response response, String referenceJsonFilePath,
                                             RestActions.ComparisonType comparisonType, String jsonPathToTargetArray, String... customLogMessage) {
        ValidationActions.validateJSONFileContent(ValidationActions.ValidationCategory.SOFT_ASSERT, response, referenceJsonFilePath, comparisonType, jsonPathToTargetArray, ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the target array extracted by parsing the API Response object
     * matches the expected referenceJsonFile if verificationType is POSITIVE, or
     * doesn't match it if verificationType is NEGATIVE.
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
     * @param verificationType      verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage      a custom message that will appended to this
     *                              step in the execution report
     */
    public static void verifyJSONFileContent(Response response, String referenceJsonFilePath,
                                             RestActions.ComparisonType comparisonType, String jsonPathToTargetArray, Verifications.VerificationType verificationType, String... customLogMessage) {
        ValidationActions.validateJSONFileContent(ValidationActions.ValidationCategory.SOFT_ASSERT, response, referenceJsonFilePath, comparisonType, jsonPathToTargetArray, ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies that the current image of the target element matches the expected reference image. Uses OpenCV natively.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param customLogMessage a custom message that will appended to this step in
     *                         *                         the execution report
     */
    public static void verifyElementMatches(WebDriver driver, By elementLocator,
                                            String... customLogMessage) {
        ValidationActions.validateElementMatches(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationActions.VisualValidationEngine.EXACT_OPENCV, ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the current image of the target element matches the expected reference image if verificationType is POSITIVE, or
     * doesn't match it if verificationType is NEGATIVE. Uses OpenCV natively.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param verificationType verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage a custom message that will appended to this step in
     *                         *                         the execution report
     */
    public static void verifyElementMatches(WebDriver driver, By elementLocator, Verifications.VerificationType verificationType,
                                            String... customLogMessage) {
        ValidationActions.validateElementMatches(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationActions.VisualValidationEngine.EXACT_OPENCV, ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies that the current image of the target element matches the expected reference image using the desired VisualValidationEngine. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param driver                 the current instance of Selenium webdriver
     * @param elementLocator         the locator of the webElement under test (By xpath,
     *                               id, selector, name ...etc)
     * @param visualValidationEngine VisualValidationEngine.EXACT_OPENCV, EXACT_EYES, STRICT_EYES, CONTENT_EYES, LAYOUT_EYES
     * @param customLogMessage       a custom message that will appended to this step in
     *                               *                         the execution report
     */
    public static void verifyElementMatches(WebDriver driver, By elementLocator, Verifications.VisualValidationEngine visualValidationEngine,
                                            String... customLogMessage) {
        ValidationActions.validateElementMatches(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationActions.VisualValidationEngine.valueOf(visualValidationEngine.name()), ValidationActions.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if verificationType is POSITIVE, or
     * doesn't match it if verificationType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param driver                 the current instance of Selenium webdriver
     * @param elementLocator         the locator of the webElement under test (By xpath,
     *                               id, selector, name ...etc)
     * @param visualValidationEngine VisualValidationEngine.EXACT_OPENCV, EXACT_EYES, STRICT_EYES, CONTENT_EYES, LAYOUT_EYES
     * @param verificationType       verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage       a custom message that will appended to this step in
     *                               *                         the execution report
     */
    public static void verifyElementMatches(WebDriver driver, By elementLocator, Verifications.VisualValidationEngine visualValidationEngine, Verifications.VerificationType verificationType,
                                            String... customLogMessage) {
        ValidationActions.validateElementMatches(ValidationActions.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationActions.VisualValidationEngine.valueOf(visualValidationEngine.name()), ValidationActions.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    public enum VerificationType {
        POSITIVE(true), NEGATIVE(false);

        private final Boolean value;

        VerificationType(Boolean type) {
            this.value = type;
        }

        protected boolean getValue() {
            return value;
        }
    }

    public enum VerificationComparisonType {
        EQUALS(1), CONTAINS(3), MATCHES(2), CASE_INSENSITIVE(4);

        private final int value;

        VerificationComparisonType(int type) {
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
