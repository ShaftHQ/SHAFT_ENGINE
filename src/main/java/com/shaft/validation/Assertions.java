package com.shaft.validation;

import com.shaft.api.RestActions;
import com.shaft.api.RestActions.ComparisonType;
import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.JavaScriptWaitManager;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.support.JavaActions;
import com.shaft.validation.Validations.ValidationComparisonType;
import com.shaft.validation.Validations.ValidationType;
import io.restassured.response.Response;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

//TODO: Assert Element matches reference file

//TODO: Add optional message to be added to the log of the assertion to describe what it does

public class Assertions {
    private static final String ERROR_INVALID_COMPARISON_OPERATOR = "Assertion Failed; invalid comparison operator used.";
    private static final String ERROR_UNHANDLED_EXCEPTION = "Assertion Failed; an unhandled exception occured.";

    private static Boolean discreetLoggingState = Boolean.valueOf(System.getProperty("alwaysLogDiscreetly"));

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
     */
    public static void assertFail() {
        Validations.assertFail();
    }

    /**
     * Force fail the current test.
     *
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertFail(String customLogMessage) {
        Validations.assertFail(customLogMessage);
    }

    /**
     * Asserts that two objects are equal.
     *
     * @param expectedValue the expected value (test data) of this assertion
     * @param actualValue   the actual value (calculated data) of this assertion
     */
    public static void assertEquals(Object expectedValue, Object actualValue) {
        Validations.assertEquals(expectedValue, actualValue, ValidationComparisonType.EQUALS, ValidationType.POSITIVE);
    }

    /**
     * Asserts that two objects are equal.
     *
     * @param expectedValue    the expected value (test data) of this assertion
     * @param actualValue      the actual value (calculated data) of this assertion
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertEquals(Object expectedValue, Object actualValue, String customLogMessage) {
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
     */
    public static void assertEquals(Object expectedValue, Object actualValue,
                                    AssertionComparisonType assertionComparisonType, AssertionType assertionType) {

        Validations.assertEquals(expectedValue, actualValue,
                ValidationComparisonType.valueOf(assertionComparisonType.toString()),
                ValidationType.valueOf(assertionType.toString()));

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
                                    AssertionComparisonType assertionComparisonType, AssertionType assertionType, String customLogMessage) {

        Validations.assertEquals(expectedValue, actualValue,
                ValidationComparisonType.valueOf(assertionComparisonType.toString()),
                ValidationType.valueOf(assertionType.toString()), customLogMessage);

    }

    /**
     * Asserts that the object is null.
     *
     * @param object the object under test
     */
    public static void assertNull(Object object) {
        Validations.assertNull(object, ValidationType.POSITIVE);
    }

    /**
     * Asserts that the object is null.
     *
     * @param object           the object under test
     * @param customLogMessage a custom message that will appended to this step in
     *                         the execution report
     */
    public static void assertNull(Object object, String customLogMessage) {
        Validations.assertNull(object, ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that the object is null if AssertionType is POSITIVE, or is not null
     * if AssertionType is NEGATIVE.
     *
     * @param object        the object under test
     * @param assertionType AssertionType.POSITIVE, NEGATIVE
     */
    public static void assertNull(Object object, AssertionType assertionType) {
        Validations.assertNull(object, ValidationType.valueOf(assertionType.toString()));
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
    public static void assertNull(Object object, AssertionType assertionType, String customLogMessage) {
        Validations.assertNull(object, ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts that the webElement found using the provided driver and locator
     * exists.
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    public static void assertElementExists(WebDriver driver, By elementLocator) {
        Validations.assertElementExists(driver, elementLocator, ValidationType.POSITIVE);
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
    public static void assertElementExists(WebDriver driver, By elementLocator, String customLogMessage) {
        Validations.assertElementExists(driver, elementLocator, ValidationType.POSITIVE, customLogMessage);
    }

    /**
     * Asserts that the webElement found using the provided driver and locator
     * exists if AssertionType is POSITIVE, or does not exist if AssertionType is
     * NEGATIVE.
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param assertionType  AssertionType.POSITIVE, NEGATIVE
     */
    public static void assertElementExists(WebDriver driver, By elementLocator, AssertionType assertionType) {
        Validations.assertElementExists(driver, elementLocator, ValidationType.valueOf(assertionType.toString()));
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
                                           String customLogMessage) {
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
     */
    public static void assertElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
                                              String expectedValue) {
        Validations.assertElementAttribute(driver, elementLocator, elementAttribute, expectedValue,
                ValidationComparisonType.EQUALS, ValidationType.POSITIVE);
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
                                              String expectedValue, String customLogMessage) {
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
     */
    public static void assertElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
                                              String expectedValue, AssertionComparisonType assertionComparisonType, AssertionType assertionType) {
        Validations.assertElementAttribute(driver, elementLocator, elementAttribute, expectedValue,
                ValidationComparisonType.valueOf(assertionComparisonType.toString()),
                ValidationType.valueOf(assertionType.toString()));
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
                                              String customLogMessage) {
        Validations.assertElementAttribute(driver, elementLocator, elementAttribute, expectedValue,
                ValidationComparisonType.valueOf(assertionComparisonType.toString()),
                ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts webElement CSSProperty equals expectedValue.
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param propertyName   the target CSS property of the webElement under test
     * @param expectedValue  the expected value (test data) of this assertion
     */
    public static void assertElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
                                                String expectedValue) {
        Validations.assertElementCSSProperty(driver, elementLocator, propertyName, expectedValue,
                ValidationComparisonType.EQUALS, ValidationType.POSITIVE);

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
                                                String expectedValue, String customLogMessage) {
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
     */
    public static void assertElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
                                                String expectedValue, AssertionComparisonType assertionComparisonType, AssertionType assertionType) {
        Validations.assertElementCSSProperty(driver, elementLocator, propertyName, expectedValue,
                ValidationComparisonType.valueOf(assertionComparisonType.toString()),
                ValidationType.valueOf(assertionType.toString()));
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
                                                String customLogMessage) {
        Validations.assertElementCSSProperty(driver, elementLocator, propertyName, expectedValue,
                ValidationComparisonType.valueOf(assertionComparisonType.toString()),
                ValidationType.valueOf(assertionType.toString()), customLogMessage);
    }

    /**
     * Asserts browser attribute equals expectedValue if AssertionType is POSITIVE,
     * or does not equal expectedValue if AssertionType is NEGATIVE. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize
     * <p>
     * *
     * <p>
     * This method will be removed soon. Use
     * {@link Assertions#assertBrowserAttribute(WebDriver, String, String, AssertionComparisonType, AssertionType)}
     * instead.
     *
     * @param driver                  the current instance of Selenium webdriver
     * @param browserAttribute        the desired attribute of the browser window
     *                                under test
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param assertionComparisonType AssertionComparisonType.LITERAL, CONTAINS,
     *                                REGEX, CASE_INSENSITIVE
     * @param assertionType           AssertionType.POSITIVE, NEGATIVE
     */
    public static void assertBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
                                              AssertionComparisonType assertionComparisonType, AssertionType assertionType) {
        JavaScriptWaitManager.waitForLazyLoading();

        ReportManager.logDiscrete("Assertion [" + "assertBrowserAttribute"
                + "] is being performed for target attribute [" + browserAttribute + "].");
        String actualValue = null;

        discreetLoggingState = ReportManager.isDiscreteLogging();
        ReportManager.setDiscreteLogging(true);
        switch (browserAttribute.toLowerCase()) {
            case "currenturl":
                actualValue = BrowserActions.getCurrentURL(driver);
                break;
            case "pagesource":
                actualValue = BrowserActions.getPageSource(driver);
                break;
            case "title":
                actualValue = BrowserActions.getCurrentWindowTitle(driver);
                break;
            case "windowhandle":
                actualValue = BrowserActions.getWindowHandle(driver);
                break;
            case "windowposition":
                actualValue = BrowserActions.getWindowPosition(driver);
                break;
            case "windowsize":
                actualValue = BrowserActions.getWindowSize(driver);
                break;
            default:
                actualValue = "";
                break;
        }
        ReportManager.setDiscreteLogging(discreetLoggingState);

        switch (JavaActions.compareTwoObjects(expectedValue, actualValue, assertionComparisonType.getValue(),
                assertionType.getValue())) {
            case 1:
                if (Boolean.TRUE.equals(assertionType.getValue())) {
                    pass("assertBrowserAttribute", driver, "Assertion Passed; actual value of [" + browserAttribute
                            + "] does match expected value [" + expectedValue + "].");
                } else {
                    pass("assertBrowserAttribute", driver,
                            "Assertion Passed; actual value of [" + browserAttribute + "] equals [" + actualValue
                                    + "] which does not match expected value [" + expectedValue + "].");
                }
                break;
            case 0:
                if (Boolean.TRUE.equals(assertionType.getValue())) {
                    fail("assertBrowserAttribute", driver,
                            "Assertion Failed; actual value of [" + browserAttribute + "] equals [" + actualValue
                                    + "] which does not match expected value [" + expectedValue + "].");
                } else {
                    fail("assertBrowserAttribute", driver, "Assertion Failed; actual value of [" + browserAttribute
                            + "] does match expected value [" + expectedValue + "].");
                }
                break;
            case -1:
                fail(ERROR_INVALID_COMPARISON_OPERATOR);
                break;
            default:
                fail(ERROR_UNHANDLED_EXCEPTION);
                break;
        }
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
     */
    public static void assertComparativeRelation(Number expectedValue, Number actualValue,
                                                 ComparativeRelationType comparativeRelationType, AssertionType assertionType) {
        ReportManager.logDiscrete("Assertion [" + "assertComparativeRelation"
                + "] is being performed, with expectedValue [" + expectedValue + "], comparativeRelationType ["
                + comparativeRelationType.getValue() + "], actualValue [" + actualValue + "], and assertionType ["
                + assertionType.getValue() + "].");

        if (Boolean.TRUE.equals(assertionType.getValue())) {
            try {
                switch (comparativeRelationType.getValue()) {
                    case ">":
                        Assert.assertTrue(actualValue.floatValue() > expectedValue.floatValue());
                        break;
                    case ">=":
                        Assert.assertTrue(actualValue.floatValue() >= expectedValue.floatValue());
                        break;
                    case "<":
                        Assert.assertTrue(actualValue.floatValue() < expectedValue.floatValue());
                        break;
                    case "<=":
                        Assert.assertTrue(actualValue.floatValue() <= expectedValue.floatValue());
                        break;
                    case "==":
                        Assert.assertTrue(actualValue.floatValue() == expectedValue.floatValue());
                        break;
                    default:
                        fail(ERROR_INVALID_COMPARISON_OPERATOR);
                        break;
                }
                pass("Assertion Passed; actual value [" + actualValue + "] is " + comparativeRelationType.getValue()
                        + " expected value [" + expectedValue + "].");
            } catch (AssertionError e) {
                fail("Assertion Failed; actual value [" + actualValue + "] is not " + comparativeRelationType.getValue()
                        + " expected value [" + expectedValue + "].", e);
            } catch (Exception e) {
                ReportManager.log(e);
                fail(ERROR_UNHANDLED_EXCEPTION, e);
            }
        } else {
            try {
                switch (comparativeRelationType.getValue()) {
                    case ">":
                        Assert.assertFalse(actualValue.floatValue() > expectedValue.floatValue());
                        break;
                    case ">=":
                        Assert.assertFalse(actualValue.floatValue() >= expectedValue.floatValue());
                        break;
                    case "<":
                        Assert.assertFalse(actualValue.floatValue() < expectedValue.floatValue());
                        break;
                    case "<=":
                        Assert.assertFalse(actualValue.floatValue() <= expectedValue.floatValue());
                        break;
                    case "==":
                        Assert.assertFalse(actualValue.floatValue() == expectedValue.floatValue());
                        break;
                    default:
                        fail(ERROR_INVALID_COMPARISON_OPERATOR);
                        break;
                }

                pass("Assertion Passed; actual value [" + actualValue + "] is not " + comparativeRelationType.getValue()
                        + " expected value [" + expectedValue + "].");
            } catch (AssertionError e) {
                fail("Assertion Failed; actual value [" + actualValue + "] is " + comparativeRelationType.getValue()
                        + " expected value [" + expectedValue + "].", e);
            } catch (Exception e) {
                ReportManager.log(e);
                fail(ERROR_UNHANDLED_EXCEPTION, e);
            }
        }
    }

    /**
     * Asserts that a certain file exists if AssertionType is POSITIVE, or doesn't
     * exist if AssertionType is NEGATIVE.
     *
     * @param fileFolderName  The location of the folder that contains the target
     *                        file, relative to the project's root folder, ending
     *                        with a /
     * @param fileName        The name of the target file (including its extension
     *                        if any)
     * @param numberOfRetries number of times to try to find the file, given that
     *                        each retry is separated by a 500 millisecond wait time
     * @param assertionType   AssertionType.POSITIVE, NEGATIVE
     */
    public static void assertFileExists(String fileFolderName, String fileName, int numberOfRetries,
                                        AssertionType assertionType) {

        ReportManager.logDiscrete("Assertion [" + "assertFileExists" + "] is being performed for target directory ["
                + fileFolderName + "], and target file [" + fileName + "].");
        if (FileActions.doesFileExist(fileFolderName, fileName, numberOfRetries)) {
            if (Boolean.TRUE.equals(assertionType.getValue())) {
                pass("Assertion Passed; target file [" + fileName + "] exists under the target path ["
                        + FileActions.getAbsolutePath(fileFolderName, fileName) + "].");
            } else {
                fail("Assertion Failed; target file [" + fileName + "] exists under the target path ["
                        + FileActions.getAbsolutePath(fileFolderName, fileName) + "].");
            }

        } else {
            if (Boolean.TRUE.equals(assertionType.getValue())) {
                fail("Assertion Failed; target file [" + fileName + "] doesn't exist under the target path ["
                        + FileActions.getAbsolutePath(fileFolderName, fileName) + "], tried for ["
                        + numberOfRetries * 500 + "] milliseconds.");
            } else {
                pass("Assertion Passed; target file [" + fileName + "] doesn't exist under the target path ["
                        + FileActions.getAbsolutePath(fileFolderName, fileName) + "], tried for ["
                        + numberOfRetries * 500 + "] milliseconds.");
            }
        }
    }

    // TODO: implement the below methods in the Validations class

    /**
     * Asserts that the provided conditional statement evaluates to true if
     * AssertionType is POSITIVE, or to false if AssertionType is NEGATIVE.
     *
     * @param conditionalStatement the statement that will be evaluated to see if it
     *                             matches the expected result
     * @param assertionType        AssertionType.POSITIVE, NEGATIVE
     */
    public static void assertTrue(Boolean conditionalStatement, AssertionType assertionType) {
        ReportManager.logDiscrete("Assertion [" + "assertTrue" + "] is being performed for target value ["
                + conditionalStatement + "], with AssertionType [" + assertionType + "].");
        if (assertionType.getValue()) {
            if (conditionalStatement == null) {
                fail("Assertion Failed; conditional statement evaluated to NULL while it was expected to evaluate to true.");
            }
            try {
                Assert.assertTrue(conditionalStatement);
                pass("Assertion Passed; conditional statement evaluated to true as expected.");
            } catch (AssertionError e) {
                fail("Assertion Failed; conditional statement evaluated to false while it was expected to evaluate to true.",
                        e);
            } catch (Exception e) {
                ReportManager.log(e);
                fail(ERROR_UNHANDLED_EXCEPTION, e);
            }
        } else {
            if (conditionalStatement == null) {
                fail("Assertion Failed; conditional statement evaluated to NULL while it was expected to evaluate to false.");
            }
            try {
                Assert.assertFalse(conditionalStatement);
                pass("Assertion Passed; conditional statement evaluated to false as expected.");
            } catch (AssertionError e) {
                fail("Assertion Failed; conditional statement evaluated to true while it was expected to evaluate to false.",
                        e);
            } catch (Exception e) {
                ReportManager.log(e);
                fail(ERROR_UNHANDLED_EXCEPTION, e);
            }
        }

    }

    /**
     * Asserts that the target API Response object matches the expected
     * referenceJsonFile if AssertionType is POSITIVE, or doesn't match it if
     * AssertionType is NEGATIVE.
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
     */
    public static void assertJSONFileContent(Response response, String referenceJsonFilePath,
                                             ComparisonType comparisonType, AssertionType assertionType) {
        assertJSONFileContent(response, referenceJsonFilePath, comparisonType, "", assertionType);
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
     */
    public static void assertJSONFileContent(Response response, String referenceJsonFilePath,
                                             ComparisonType comparisonType, String jsonPathToTargetArray, AssertionType assertionType) {
        if (jsonPathToTargetArray.equals("")) {
            ReportManager.logDiscrete("Assertion [" + "assertJSONFileContent"
                    + "] is being performed, with referenceJsonFile [" + referenceJsonFilePath + "], comparisonType ["
                    + comparisonType + "], and assertionType [" + assertionType + "].");
        } else {
            ReportManager.logDiscrete(
                    "Assertion [" + "assertJSONFileContent" + "] is being performed, with referenceJsonFile ["
                            + referenceJsonFilePath + "], jsonPathToTargetArray [" + jsonPathToTargetArray
                            + "], comparisonType [" + comparisonType + "], and assertionType [" + assertionType + "].");
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

        if (Boolean.TRUE.equals(comparisonResult)) {
            if (assertionType.getValue()) {
                // comparison passed and is expected to pass
                pass("Assertion Passed; the actual API response does match the expected JSON file at this path \""
                        + referenceJsonFilePath + "\".", attachments);
            } else {
                // comparison passed and is expected to fail
                fail("Assertion Failed; the actual API response does match the expected JSON file at this path \""
                        + referenceJsonFilePath + "\".", attachments);
            }
        } else {
            if (assertionType.getValue()) {
                // comparison failed and is expected to pass
                fail("Assertion Failed; the actual API response does not match the expected JSON file at this path \""
                        + referenceJsonFilePath + "\".", attachments);
            } else {
                // comparison failed and is expected to fail
                pass("Assertion Passed; the actual API response does not match the expected JSON file at this path \""
                        + referenceJsonFilePath + "\".", attachments);
            }
        }
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