package com.shaft.validation;

import com.microsoft.playwright.Page;
import com.shaft.api.RestActions;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

@Deprecated(forRemoval = true)
public class Verifications {

    private Verifications() {
        throw new IllegalStateException("Utility class");
    }

    public static AssertionError getVerificationErrorToForceFail() {
        return ValidationsHelper.getVerificationErrorToForceFail();
    }

    public static void resetVerificationStateAfterFailing() {
        ValidationsHelper.resetVerificationStateAfterFailing();
    }

    /**
     * Force fail the current test.
     *
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyFail(String... customLogMessage) {
        ValidationsHelper.validateFail(ValidationEnums.ValidationCategory.SOFT_ASSERT, customLogMessage);
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
        ValidationsHelper.validateEquals(ValidationEnums.ValidationCategory.SOFT_ASSERT, expectedValue, actualValue, ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE,
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

        ValidationsHelper.validateEquals(ValidationEnums.ValidationCategory.SOFT_ASSERT, expectedValue, actualValue,
                ValidationEnums.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);

    }

    /**
     * verifies that the object is null.
     *
     * @param object           the object under test
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyNull(Object object, String... customLogMessage) {
        ValidationsHelper.validateNull(ValidationEnums.ValidationCategory.SOFT_ASSERT, object, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateNull(ValidationEnums.ValidationCategory.SOFT_ASSERT, object, ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
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
        ValidationsHelper.validateElementExists(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateElementExists(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationEnums.ValidationType.valueOf(verificationType.toString()),
                customLogMessage);
    }

    /**
     * verifies webElement attribute equals expectedValue.
     *
     * @param driver                the current instance of Selenium webdriver
     * @param elementLocator        the locator of the webElement under test (By xpath,
     *                              id, selector, name ...etc)
     * @param elementAttributeType  the desired attribute type of the webElement under test
     * @param expectedValue         the expected value (test data) of this verification
     * @param customLogMessage      a custom message that will appended to this step in
     *                              the execution report
     */
    public static void verifyElementAttribute(WebDriver driver, By elementLocator, ElementAttributeType elementAttributeType,
                                              String expectedValue, String... customLogMessage) {
        ValidationsHelper.validateElementAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, elementAttributeType.getValue(), expectedValue,
                ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateElementAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, elementAttribute, expectedValue,
                ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies webElement attribute equals expectedValue if verificationType is
     * POSITIVE, or does not equal expectedValue if verificationType is NEGATIVE.
     *
     * @param driver                     the current instance of Selenium webdriver
     * @param elementLocator             the locator of the webElement under test (By
     *                                   xpath, id, selector, name ...etc)
     * @param elementAttributeType       the desired attribute type of the webElement under
     *                                   test
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param verificationComparisonType verificationComparisonType.EQUALS, CONTAINS,
     *                                   MATCHES, CASE_INSENSITIVE
     * @param verificationType           verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage           a custom message that will appended to this
     *                                   step in the execution report
     */
    public static void verifyElementAttribute(WebDriver driver, By elementLocator, ElementAttributeType elementAttributeType,
                                              String expectedValue, Verifications.VerificationComparisonType verificationComparisonType, Verifications.VerificationType verificationType,
                                              String... customLogMessage) {
        ValidationsHelper.validateElementAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, elementAttributeType.getValue(), expectedValue,
                ValidationEnums.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies webElement attribute equals expectedValue if verificationType is
     * POSITIVE, or does not equal expectedValue if verificationType is NEGATIVE.
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
        ValidationsHelper.validateElementAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, elementAttribute, expectedValue,
                ValidationEnums.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }


    /**
     * verifies that the webElement found using the provided driver and locator
     * exists.
     *
     * @param page           the current instance of Playwright
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyElementExists(Page page, String elementLocator, String... customLogMessage) {
        ValidationsHelper.validateElementExists(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, elementLocator, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the webElement found using the provided driver and locator
     * exists if verificationType is POSITIVE, or does not exist if verificationType is
     * NEGATIVE.
     *
     * @param page           the current instance of Playwright
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param verificationType verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyElementExists(Page page, String elementLocator, Verifications.VerificationType verificationType,
                                           String... customLogMessage) {
        ValidationsHelper.validateElementExists(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, elementLocator, ValidationEnums.ValidationType.valueOf(verificationType.toString()),
                customLogMessage);
    }

    /**
     * verifies webElement attribute equals expectedValue.
     *
     * @param page           the current instance of Playwright
     * @param elementLocator        the locator of the webElement under test (By xpath,
     *                              id, selector, name ...etc)
     * @param elementAttributeType  the desired attribute type of the webElement under test
     * @param expectedValue         the expected value (test data) of this verification
     * @param customLogMessage      a custom message that will appended to this step in
     *                              the execution report
     */
    public static void verifyElementAttribute(Page page, String elementLocator, ElementAttributeType elementAttributeType,
                                              String expectedValue, String... customLogMessage) {
        ValidationsHelper.validateElementAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, elementLocator, elementAttributeType.getValue(), expectedValue,
                ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies webElement attribute equals expectedValue.
     *
     * @param page           the current instance of Playwright
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param elementAttribute the desired attribute of the webElement under test
     * @param expectedValue    the expected value (test data) of this verification
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyElementAttribute(Page page, String elementLocator, String elementAttribute,
                                              String expectedValue, String... customLogMessage) {
        ValidationsHelper.validateElementAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, elementLocator, elementAttribute, expectedValue,
                ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies webElement attribute equals expectedValue if verificationType is
     * POSITIVE, or does not equal expectedValue if verificationType is NEGATIVE.
     *
     * @param page           the current instance of Playwright
     * @param elementLocator             the locator of the webElement under test (By
     *                                   xpath, id, selector, name ...etc)
     * @param elementAttributeType       the desired attribute type of the webElement under
     *                                   test
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param verificationComparisonType verificationComparisonType.EQUALS, CONTAINS,
     *                                   MATCHES, CASE_INSENSITIVE
     * @param verificationType           verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage           a custom message that will appended to this
     *                                   step in the execution report
     */
    public static void verifyElementAttribute(Page page, String elementLocator, ElementAttributeType elementAttributeType,
                                              String expectedValue, Verifications.VerificationComparisonType verificationComparisonType, Verifications.VerificationType verificationType,
                                              String... customLogMessage) {
        ValidationsHelper.validateElementAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, elementLocator, elementAttributeType.getValue(), expectedValue,
                ValidationEnums.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies webElement attribute equals expectedValue if verificationType is
     * POSITIVE, or does not equal expectedValue if verificationType is NEGATIVE.
     *
     * @param page           the current instance of Playwright
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
    public static void verifyElementAttribute(Page page, String elementLocator, String elementAttribute,
                                              String expectedValue, Verifications.VerificationComparisonType verificationComparisonType, Verifications.VerificationType verificationType,
                                              String... customLogMessage) {
        ValidationsHelper.validateElementAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, elementLocator, elementAttribute, expectedValue,
                ValidationEnums.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
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
        ValidationsHelper.validateElementCSSProperty(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, propertyName, expectedValue,
                ValidationEnums.ValidationComparisonType.EQUALS, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateElementCSSProperty(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, propertyName, expectedValue,
                ValidationEnums.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
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
        ValidationsHelper.validateBrowserAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, browserAttribute, expectedValue, ValidationEnums.ValidationComparisonType.EQUALS,
                ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateBrowserAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, browserAttribute, expectedValue, ValidationEnums.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies browser attribute equals expectedValue. Supports
     *
     * @param driver                the current instance of Selenium webdriver
     * @param browserAttributeType  the desired attribute type of the browser window
     *                              under test
     * @param expectedValue         the expected value (test data) of this
     *                              verification
     * @param customLogMessage      a custom message that will appended to this
     *                              step in the execution report
     */
    public static void verifyBrowserAttribute(WebDriver driver, BrowserAttributeType browserAttributeType, String expectedValue,
                                              String... customLogMessage) {
        ValidationsHelper.validateBrowserAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, browserAttributeType.getValue(), expectedValue, ValidationEnums.ValidationComparisonType.EQUALS,
                ValidationEnums.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies browser attribute equals expectedValue if verificationType is POSITIVE,
     * or does not equal expectedValue if verificationType is NEGATIVE. Supports
     *
     * @param driver                     the current instance of Selenium webdriver
     * @param browserAttributeType       the desired attribute type of the browser window
     *                                   under test
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param verificationComparisonType verificationComparisonType.EQUALS, CONTAINS,
     *                                   MATCHES, CASE_INSENSITIVE
     * @param verificationType           verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage           a custom message that will appended to this
     *                                   step in the execution report
     */
    public static void verifyBrowserAttribute(WebDriver driver, BrowserAttributeType browserAttributeType, String expectedValue,
                                              Verifications.VerificationComparisonType verificationComparisonType, Verifications.VerificationType verificationType, String... customLogMessage) {
        ValidationsHelper.validateBrowserAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, browserAttributeType.getValue(), expectedValue, ValidationEnums.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }


    /**
     * verifies browser attribute equals expectedValue. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize
     *
     * @param page           the current instance of Playwright
     * @param browserAttribute the desired attribute of the browser window
     *                         under test
     * @param expectedValue    the expected value (test data) of this
     *                         verification
     * @param customLogMessage a custom message that will appended to this
     *                         step in the execution report
     */
    public static void verifyBrowserAttribute(Page page, String browserAttribute, String expectedValue,
                                              String... customLogMessage) {
        ValidationsHelper.validateBrowserAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, browserAttribute, expectedValue, ValidationEnums.ValidationComparisonType.EQUALS,
                ValidationEnums.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies browser attribute equals expectedValue if verificationType is POSITIVE,
     * or does not equal expectedValue if verificationType is NEGATIVE. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize
     *
     * @param page           the current instance of Playwright
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
    public static void verifyBrowserAttribute(Page page, String browserAttribute, String expectedValue,
                                              Verifications.VerificationComparisonType verificationComparisonType, Verifications.VerificationType verificationType, String... customLogMessage) {
        ValidationsHelper.validateBrowserAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, browserAttribute, expectedValue, ValidationEnums.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies browser attribute equals expectedValue. Supports
     *
     * @param page           the current instance of Playwright
     * @param browserAttributeType  the desired attribute type of the browser window
     *                              under test
     * @param expectedValue         the expected value (test data) of this
     *                              verification
     * @param customLogMessage      a custom message that will appended to this
     *                              step in the execution report
     */
    public static void verifyBrowserAttribute(Page page, BrowserAttributeType browserAttributeType, String expectedValue,
                                              String... customLogMessage) {
        ValidationsHelper.validateBrowserAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, browserAttributeType.getValue(), expectedValue, ValidationEnums.ValidationComparisonType.EQUALS,
                ValidationEnums.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies browser attribute equals expectedValue if verificationType is POSITIVE,
     * or does not equal expectedValue if verificationType is NEGATIVE. Supports
     *
     * @param page           the current instance of Playwright
     * @param browserAttributeType       the desired attribute type of the browser window
     *                                   under test
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param verificationComparisonType verificationComparisonType.EQUALS, CONTAINS,
     *                                   MATCHES, CASE_INSENSITIVE
     * @param verificationType           verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage           a custom message that will appended to this
     *                                   step in the execution report
     */
    public static void verifyBrowserAttribute(Page page, BrowserAttributeType browserAttributeType, String expectedValue,
                                              Verifications.VerificationComparisonType verificationComparisonType, Verifications.VerificationType verificationType, String... customLogMessage) {
        ValidationsHelper.validateBrowserAttribute(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, browserAttributeType.getValue(), expectedValue, ValidationEnums.ValidationComparisonType.valueOf(verificationComparisonType.toString()),
                ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
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
        ValidationsHelper.validateComparativeRelation(ValidationEnums.ValidationCategory.SOFT_ASSERT, expectedValue, actualValue, ValidationEnums.NumbersComparativeRelation.valueOf(comparativeRelationType.toString()), ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateComparativeRelation(ValidationEnums.ValidationCategory.SOFT_ASSERT, expectedValue, actualValue, ValidationEnums.NumbersComparativeRelation.valueOf(comparativeRelationType.toString()), ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
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
        ValidationsHelper.validateFileExists(ValidationEnums.ValidationCategory.SOFT_ASSERT, fileFolderName, fileName, 1, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateFileExists(ValidationEnums.ValidationCategory.SOFT_ASSERT, fileFolderName, fileName, numberOfRetries, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateFileExists(ValidationEnums.ValidationCategory.SOFT_ASSERT, fileFolderName, fileName, numberOfRetries, ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
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
        ValidationsHelper.validateTrue(ValidationEnums.ValidationCategory.SOFT_ASSERT, conditionalStatement, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateTrue(ValidationEnums.ValidationCategory.SOFT_ASSERT, conditionalStatement, ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
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
        ValidationsHelper.validateJSONFileContent(ValidationEnums.ValidationCategory.SOFT_ASSERT, response, referenceJsonFilePath, RestActions.ComparisonType.EQUALS, "", ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateJSONFileContent(ValidationEnums.ValidationCategory.SOFT_ASSERT, response, referenceJsonFilePath, comparisonType, "", ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateJSONFileContent(ValidationEnums.ValidationCategory.SOFT_ASSERT, response, referenceJsonFilePath, comparisonType, "", ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
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
        ValidationsHelper.validateJSONFileContent(ValidationEnums.ValidationCategory.SOFT_ASSERT, response, referenceJsonFilePath, comparisonType, jsonPathToTargetArray, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateJSONFileContent(ValidationEnums.ValidationCategory.SOFT_ASSERT, response, referenceJsonFilePath, comparisonType, jsonPathToTargetArray, ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
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
        ValidationsHelper.validateElementMatches(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationEnums.VisualValidationEngine.EXACT_OPENCV, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateElementMatches(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationEnums.VisualValidationEngine.EXACT_OPENCV, ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
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
        ValidationsHelper.validateElementMatches(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationEnums.VisualValidationEngine.valueOf(visualValidationEngine.name()), ValidationEnums.ValidationType.POSITIVE, customLogMessage);
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
        ValidationsHelper.validateElementMatches(ValidationEnums.ValidationCategory.SOFT_ASSERT, driver, elementLocator, ValidationEnums.VisualValidationEngine.valueOf(visualValidationEngine.name()), ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }
    /**
     * Verify that two objects are equal
     *
     * @param response         the full response object
     * @param expectedValue    the expected value (test data) of this verification
     * @param JSONPath         JSONPath of the actual value of this verification;
     *                         the JSONPath expression that will be evaluated in
     *                         order to extract the desired value [without the
     *                         trailing $.], please refer to these urls for
     *                         examples:
     *                         https://support.smartbear.com/alertsite/docs/monitors/api/endpoint/jsonpath.html
     *                         http://jsonpath.com/
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyApiResponseEquals(Response response, String expectedValue, String JSONPath, String... customLogMessage) {
        ValidationsHelper.validateEquals(ValidationEnums.ValidationCategory.SOFT_ASSERT, expectedValue,
                RestActions.getResponseJSONValue(response, JSONPath), ValidationEnums.ValidationComparisonType.EQUALS,
                ValidationEnums.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Verify that two objects are equal
     *
     * @param response         the full response object
     * @param expectedValue    the expected value (test data) of this verification
     * @param JSONPath         JSONPath of the actual value of this verification;
     *                         the JSONPath expression that will be evaluated in
     *                         order to extract the desired value [without the
     *                         trailing $.], please refer to these urls for
     *                         examples:
     *                         https://support.smartbear.com/alertsite/docs/monitors/api/endpoint/jsonpath.html
     *                         http://jsonpath.com/
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void verifyApiResponseEquals(Object response, String expectedValue, String JSONPath, String... customLogMessage) {
        ValidationsHelper.validateEquals(ValidationEnums.ValidationCategory.SOFT_ASSERT, expectedValue,
                RestActions.getResponseJSONValue(response, JSONPath), ValidationEnums.ValidationComparisonType.EQUALS,
                ValidationEnums.ValidationType.POSITIVE, customLogMessage);
    }


    /**
     * verifies that the current image of the target element matches the expected reference image. Uses OpenCV natively.
     *
     * @param page           the current instance of Playwright
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param customLogMessage a custom message that will appended to this step in
     *                         *                         the execution report
     */
    public static void verifyElementMatches(Page page, String elementLocator,
                                            String... customLogMessage) {
        ValidationsHelper.validateElementMatches(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, elementLocator, ValidationEnums.VisualValidationEngine.EXACT_OPENCV, ValidationEnums.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the current image of the target element matches the expected reference image if verificationType is POSITIVE, or
     * doesn't match it if verificationType is NEGATIVE. Uses OpenCV natively.
     *
     * @param page           the current instance of Playwright
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param verificationType verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage a custom message that will appended to this step in
     *                         *                         the execution report
     */
    public static void verifyElementMatches(Page page, String elementLocator, Verifications.VerificationType verificationType,
                                            String... customLogMessage) {
        ValidationsHelper.validateElementMatches(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, elementLocator, ValidationEnums.VisualValidationEngine.EXACT_OPENCV, ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    /**
     * verifies that the current image of the target element matches the expected reference image using the desired VisualValidationEngine. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param page           the current instance of Playwright
     * @param elementLocator         the locator of the webElement under test (By xpath,
     *                               id, selector, name ...etc)
     * @param visualValidationEngine VisualValidationEngine.EXACT_OPENCV, EXACT_EYES, STRICT_EYES, CONTENT_EYES, LAYOUT_EYES
     * @param customLogMessage       a custom message that will appended to this step in
     *                               *                         the execution report
     */
    public static void verifyElementMatches(Page page, String elementLocator, Verifications.VisualValidationEngine visualValidationEngine,
                                            String... customLogMessage) {
        ValidationsHelper.validateElementMatches(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, elementLocator, ValidationEnums.VisualValidationEngine.valueOf(visualValidationEngine.name()), ValidationEnums.ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * verifies that the current image of the target element matches the expected reference image using the desired VisualValidationEngine if verificationType is POSITIVE, or
     * doesn't match it if verificationType is NEGATIVE. Supports OpenCV natively, and Applitools Eyes. To use Eyes you need to configure your applitoolsApiKey in the path.properties file
     *
     * @param page           the current instance of Playwright
     * @param elementLocator         the locator of the webElement under test (By xpath,
     *                               id, selector, name ...etc)
     * @param visualValidationEngine VisualValidationEngine.EXACT_OPENCV, EXACT_EYES, STRICT_EYES, CONTENT_EYES, LAYOUT_EYES
     * @param verificationType       verificationType.POSITIVE, NEGATIVE
     * @param customLogMessage       a custom message that will appended to this step in
     *                               *                         the execution report
     */
    public static void verifyElementMatches(Page page, String elementLocator, Verifications.VisualValidationEngine visualValidationEngine, Verifications.VerificationType verificationType,
                                            String... customLogMessage) {
        ValidationsHelper.validateElementMatches(ValidationEnums.ValidationCategory.SOFT_ASSERT, page, elementLocator, ValidationEnums.VisualValidationEngine.valueOf(visualValidationEngine.name()), ValidationEnums.ValidationType.valueOf(verificationType.toString()), customLogMessage);
    }

    public enum VerificationType {
        POSITIVE(true), NEGATIVE(false);

        private final Boolean value;

        VerificationType(Boolean type) {
            this.value = type;
        }

        private boolean getValue() {
            return value;
        }
    }

    public enum VerificationComparisonType {
        EQUALS(1), CONTAINS(3), MATCHES(2), CASE_INSENSITIVE(4);

        private final int value;

        VerificationComparisonType(int type) {
            this.value = type;
        }

        private int getValue() {
            return value;
        }
    }

    public enum ComparativeRelationType {
        GREATER_THAN(">"), GREATER_THAN_OR_EQUALS(">="), LESS_THAN("<"), LESS_THAN_OR_EQUALS("<="), EQUALS("==");

        private final String value;

        ComparativeRelationType(String type) {
            this.value = type;
        }

        private String getValue() {
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

    public enum ElementAttributeType {
        TEXT("text"), TAG_NAME("tagname"), SIZE("size"), SELECTED_TEXT("selectedtext");

        private final String value;

        ElementAttributeType(String type) {
            this.value = type;
        }

        private String getValue() {
            return value;
        }
    }

    public enum BrowserAttributeType {
        CURRENT_URL("currenturl"), PAGE_SOURCE("pagesource"), TITLE("title"), WINDOW_HANDLE("windowhandle"), WINDOW_POSITION("windowposition"), WINDOW_SIZE("windowsize");

        private final String value;

        BrowserAttributeType(String type) {
            this.value = type;
        }

        private String getValue() {
            return value;
        }
    }
}
