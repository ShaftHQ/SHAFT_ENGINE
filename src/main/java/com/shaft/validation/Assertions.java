package com.shaft.validation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;

import com.shaft.api.RestActions;
import com.shaft.api.RestActions.ComparisonType;
import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.JSWaiter;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.support.JavaActions;

import io.restassured.response.Response;

//TODO: Assert Element matches reference file

//TODO: Add optional message to be added to the log of the assertion to describe what it does

public class Assertions {
    private static int attemptsBeforeThrowingElementNotFoundException = Integer
	    .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static int attemptsBeforeThrowingElementNotFoundExceptionInCaseElementShouldntExist = 1;

    private static final String ERROR_INVALID_COMPARISON_OPERATOR = "Assertion Failed; invalid comparison operator used.";
    private static final String ERROR_UNHANDLED_EXCEPTION = "Assertion Failed; an unhandled exception occured.";

    private static Boolean discreetLoggingState = Boolean.valueOf(System.getProperty("alwaysLogDiscreetly"));

    public enum AssertionType {
	POSITIVE(true), NEGATIVE(false);

	private Boolean value;

	AssertionType(Boolean type) {
	    this.value = type;
	}

	protected boolean getValue() {
	    return value;
	}
    }

    public enum AssertionComparisonType {
	EQUALS(1), CONTAINS(3), MATCHES(2), CASE_INSENSITIVE(4);

	private int value;

	AssertionComparisonType(int type) {
	    this.value = type;
	}

	protected int getValue() {
	    return value;
	}
    }

    public enum ComparativeRelationType {
	GREATER_THAN(">"), GREATER_THAN_OR_EQUALS(">="), LESS_THAN("<"), LESS_THAN_OR_EQUALS("<="), EQUALS("==");

	private String value;

	ComparativeRelationType(String type) {
	    this.value = type;
	}

	protected String getValue() {
	    return value;
	}
    }

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

    private static void fail(String message, String expectedValue, String actualValue) {
	List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Expected Value", expectedValue);
	List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Value", actualValue);

	List<List<Object>> attachments = new ArrayList<>();
	attachments.add(expectedValueAttachment);
	attachments.add(actualValueAttachment);
	fail(message, attachments);
    }

    private static void fail(String actionName, WebDriver driver, String message) {
	fail(message, Arrays.asList(ScreenshotManager.captureScreenShot(driver, actionName, false)));
    }

    private static void fail(String actionName, WebDriver driver, By elementLocator, String message) {
	fail(message, Arrays.asList(ScreenshotManager.captureScreenShot(driver, elementLocator, actionName, false)));
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

    private static void pass(String message, String expectedValue, String actualValue) {
	List<Object> expectedValueAttachment = Arrays.asList("Validation Test Data", "Expected Value", expectedValue);
	List<Object> actualValueAttachment = Arrays.asList("Validation Test Data", "Actual Value", actualValue);

	List<List<Object>> attachments = new ArrayList<>();
	attachments.add(expectedValueAttachment);
	attachments.add(actualValueAttachment);

	pass(message, attachments);
    }

    private static void pass(String actionName, WebDriver driver, String message) {
	pass(message, Arrays.asList(ScreenshotManager.captureScreenShot(driver, actionName, true)));
    }

    private static void pass(String actionName, WebDriver driver, By elementLocator, String message) {
	pass(message, Arrays.asList(ScreenshotManager.captureScreenShot(driver, elementLocator, actionName, true)));
    }

    /**
     * Asserts that two objects are equal if AssertionType is true, or not equal if
     * AssertionType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Assertions#assertEquals(Object , Object , AssertionComparisonType , AssertionType)}
     * instead.
     * 
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param actualValue             the actual value (calculated data) of this
     *                                assertion
     * @param assertionComparisonType 1 is literalComparison, 2 is regexComparison,
     *                                3 is containsComparison, 4 is
     *                                caseInsensitiveComparison
     * @param assertionType           either 'true' for a positive assertion that
     *                                the objects are equal, or 'false' for a
     *                                negative assertion that the objects are not
     *                                equal
     */
    public static void assertEquals(Object expectedValue, Object actualValue, int assertionComparisonType,
	    Boolean assertionType) {
	ReportManager.logDiscrete("Assertion [" + "assertEquals" + "] is being performed, with expectedValue ["
		+ expectedValue + "], actualValue [" + actualValue + "], comparisonType [" + assertionComparisonType
		+ "], and assertionType [" + assertionType + "].");

	Boolean isExpectedOrActualValueLong = expectedValue.toString().length() >= 500
		|| actualValue.toString().length() >= 500;

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, assertionComparisonType, assertionType)) {
	case 1:
	    if (Boolean.TRUE.equals(assertionType)) {
		if (Boolean.FALSE.equals(isExpectedOrActualValueLong)) {
		    pass("Assertion Passed; actual value [" + actualValue + "] does match expected value ["
			    + expectedValue + "].");
		} else {
		    pass("Assertion Passed; actual value does match expected value. Kindly check the attachments for more details.",
			    String.valueOf(expectedValue), String.valueOf(actualValue));
		}

	    } else {
		if (Boolean.FALSE.equals(isExpectedOrActualValueLong)) {
		    pass("Assertion Passed; actual value [" + actualValue + "] does not match expected value ["
			    + expectedValue + "].");
		} else {
		    pass("Assertion Passed; actual value does not match expected value. Kindly check the attachments for more details.",
			    String.valueOf(expectedValue), String.valueOf(actualValue));
		}
	    }
	    break;
	case 0:
	    if (Boolean.TRUE.equals(assertionType)) {
		if (Boolean.FALSE.equals(isExpectedOrActualValueLong)) {
		    fail("Assertion Failed; actual value [" + actualValue + "] does not match expected value ["
			    + expectedValue + "].");
		} else {
		    fail("Assertion Failed; actual value does not match expected value.", String.valueOf(expectedValue),
			    String.valueOf(actualValue));
		}
	    } else {
		if (Boolean.FALSE.equals(isExpectedOrActualValueLong)) {
		    fail("Assertion Failed; actual value [" + actualValue + "] does match expected value ["
			    + expectedValue + "].");
		} else {
		    fail("Assertion Failed; actual value does match expected value.", String.valueOf(expectedValue),
			    String.valueOf(actualValue));
		}
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
	assertEquals(expectedValue, actualValue, assertionComparisonType.getValue(), assertionType.getValue());
    }

    /**
     * Asserts that object is null if AssertionType is true, or is not null if
     * AssertionType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Assertions#assertNull(Object , AssertionType)} instead.
     * 
     * @param object        the object under test
     * @param assertionType either 'true' for a positive assertion that the object
     *                      refers to null, or 'false' for a negative assertion that
     *                      the object doesn't refer to null
     */
    public static void assertNull(Object object, Boolean assertionType) {
	ReportManager.logDiscrete("Assertion [" + "assertNull" + "] is being performed.");

	if (Boolean.TRUE.equals(assertionType)) {
	    try {
		Assert.assertNull(object);
		pass("Assertion Passed; actual value is null.");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value is not null.", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail(ERROR_UNHANDLED_EXCEPTION, e);
	    }
	} else {
	    try {
		Assert.assertNotNull(object);
		pass("Assertion Passed; actual value is not null.");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value is null.", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail(ERROR_UNHANDLED_EXCEPTION, e);
	    }
	}
    }

    /**
     * Asserts that object is null if AssertionType is POSITIVE, or is not null if
     * AssertionType is NEGATIVE.
     * 
     * @param object        the object under test
     * @param assertionType AssertionType.POSITIVE, NEGATIVE
     */
    public static void assertNull(Object object, AssertionType assertionType) {
	assertNull(object, assertionType.getValue());
    }

    /**
     * Asserts that webElement found using the provided driver and locator exists if
     * AssertionType is true, or does not exist if AssertionType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Assertions#assertElementExists(WebDriver, By, AssertionType)} instead.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param assertionType  either 'true' for a positive assertion that the element
     *                       exists, or 'false' for a negative assertion that the
     *                       element doesn't exist
     */
    public static void assertElementExists(WebDriver driver, By elementLocator, Boolean assertionType) {
	ReportManager.logDiscrete("Assertion [" + "assertElementExists" + "] is being performed.");
	try {
	    int customAttempts = attemptsBeforeThrowingElementNotFoundException;
	    if (Boolean.FALSE.equals(assertionType)) {
		customAttempts = attemptsBeforeThrowingElementNotFoundExceptionInCaseElementShouldntExist;
	    }

	    int elementsCount = ElementActions.getElementsCount(driver, elementLocator, customAttempts);

	    switch (elementsCount) {
	    case 0:
		if (Boolean.TRUE.equals(assertionType)) {
		    fail("assertElementExists", driver,
			    "Assertion Failed; element does not exist. Locator [" + elementLocator.toString() + "].");
		} else {
		    pass("assertElementExists", driver,
			    "Assertion Passed; element does not exist. Locator [" + elementLocator.toString() + "].");
		}
		break;
	    case 1:
		if (Boolean.TRUE.equals(assertionType)) {
		    pass("assertElementExists", driver, elementLocator,
			    "Assertion Passed; element exists and is unique. Locator [" + elementLocator.toString()
				    + "].");
		} else {
		    fail("assertElementExists", driver, elementLocator,
			    "Assertion Failed; element exists and is unique. Locator [" + elementLocator.toString()
				    + "].");
		}
		break;
	    default:
		fail("assertElementExists", driver,
			"Assertion Failed; element is not unique. Locator [" + elementLocator.toString() + "].");
		break;
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    fail(ERROR_UNHANDLED_EXCEPTION, e);
	}
    }

    /**
     * Asserts that webElement found using the provided driver and locator exists if
     * AssertionType is POSITIVE, or does not exist if AssertionType is NEGATIVE.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param assertionType  AssertionType.POSITIVE, NEGATIVE
     */
    public static void assertElementExists(WebDriver driver, By elementLocator, AssertionType assertionType) {
	assertElementExists(driver, elementLocator, assertionType.getValue());
    }

    /**
     * Asserts webElement attribute equals expectedValue if AssertionType is true,
     * or does not equal expectedValue if AssertionType is false. Supports Text,
     * TagName, Size, Other Attributes
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Assertions#assertElementAttribute(WebDriver , By , String , String , AssertionComparisonType , AssertionType)}
     * instead.
     * 
     * @param driver                  the current instance of Selenium webdriver
     * @param elementLocator          the locator of the webElement under test (By
     *                                xpath, id, selector, name ...etc)
     * @param elementAttribute        the desired attribute of the webElement under
     *                                test
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param assertionComparisonType 1 is literalComparison, 2 is regexComparison,
     *                                3 is containsComparison, 4 is
     *                                caseInsensitiveComparison
     * @param assertionType           either 'true' for a positive assertion that
     *                                the element attribute actual value matches the
     *                                expected value, or 'false' for a negative
     *                                assertion that the element attribute actual
     *                                value doesn't match the expected value
     */
    public static void assertElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
	    String expectedValue, int assertionComparisonType, Boolean assertionType) {
	ReportManager.logDiscrete("Assertion [" + "assertElementAttribute"
		+ "] is being performed for target attribute [" + elementAttribute + "].");
	String actualValue = null;

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

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, assertionComparisonType, assertionType)) {
	case 1:
	    if (Boolean.TRUE.equals(assertionType)) {
		pass("assertElementAttribute", driver, elementLocator, "Assertion Passed; actual value of ["
			+ elementAttribute + "] does match expected value [" + expectedValue + "].");
	    } else {
		pass("assertElementAttribute", driver, elementLocator,
			"Assertion Passed; actual value of [" + elementAttribute + "] equals [" + actualValue
				+ "] which does not match expected value [" + expectedValue + "].");
	    }
	    break;
	case 0:
	    if (Boolean.TRUE.equals(assertionType)) {
		fail("assertElementAttribute", driver, elementLocator,
			"Assertion Failed; actual value of [" + elementAttribute + "] equals [" + actualValue
				+ "] which does not match expected value [" + expectedValue + "].");
	    } else {
		fail("assertElementAttribute", driver, elementLocator, "Assertion Failed; actual value of ["
			+ elementAttribute + "] does match expected value [" + expectedValue + "].");
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
	assertElementAttribute(driver, elementLocator, elementAttribute, expectedValue,
		assertionComparisonType.getValue(), assertionType.getValue());
    }

    /**
     * Asserts webElement CSSProperty equals expectedValue if AssertionType is true,
     * or does not equal expectedValue if AssertionType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Assertions#assertElementCSSProperty(WebDriver, By, String, String, AssertionComparisonType, AssertionType)}
     * instead.
     * 
     * @param driver                  the current instance of Selenium webdriver
     * @param elementLocator          the locator of the webElement under test (By
     *                                xpath, id, selector, name ...etc)
     * @param propertyName            the target CSS property of the webElement
     *                                under test
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param assertionComparisonType 1 is literalComparison, 2 is regexComparison,
     *                                3 is containsComparison, 4 is
     *                                caseInsensitiveComparison
     * @param assertionType           either 'true' for a positive assertion that
     *                                the element CSSProperty actual value matches
     *                                the expected value, or 'false' for a negative
     *                                assertion that the element CSSProperty actual
     *                                value doesn't match the expected value
     */
    public static void assertElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
	    String expectedValue, int assertionComparisonType, Boolean assertionType) {
	ReportManager.logDiscrete("Assertion [" + "assertElementCSSProperty"
		+ "] is being performed for target CSS Property [" + propertyName + "].");

	discreetLoggingState = ReportManager.isDiscreteLogging();
	ReportManager.setDiscreteLogging(true);
	String actualValue = ElementActions.getCSSProperty(driver, elementLocator, propertyName);
	ReportManager.setDiscreteLogging(discreetLoggingState);

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, assertionComparisonType, assertionType)) {
	case 1:
	    if (Boolean.TRUE.equals(assertionType)) {
		pass("assertElementCSSProperty", driver, elementLocator,
			"Assertion Passed; actual CSS Property value of [" + propertyName
				+ "] does match expected value [" + expectedValue + "].");
	    } else {
		pass("assertElementCSSProperty", driver, elementLocator,
			"Assertion Passed; actual CSS Property value of [" + propertyName + "] equals [" + actualValue
				+ "] which does not match expected value [" + expectedValue + "].");
	    }
	    break;
	case 0:
	    if (Boolean.TRUE.equals(assertionType)) {
		fail("assertElementCSSProperty", driver, elementLocator,
			"Assertion Failed; actual CSS Property value of [" + propertyName + "] equals [" + actualValue
				+ "] which does not match expected value [" + expectedValue + "].");
	    } else {
		fail("assertElementCSSProperty", driver, elementLocator,
			"Assertion Failed; actual CSS Property value of [" + propertyName
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
	assertElementCSSProperty(driver, elementLocator, propertyName, expectedValue,
		assertionComparisonType.getValue(), assertionType.getValue());
    }

    /**
     * Asserts browser attribute equals expectedValue if AssertionType is true, or
     * does not equal expectedValue if AssertionType is false. Supports CurrentUrl,
     * PageSource, Title, WindowHandle, WindowPosition, WindowSize
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Assertions#assertBrowserAttribute(WebDriver , String , String , AssertionComparisonType , AssertionType )}
     * instead.
     * 
     * @param driver                  the current instance of Selenium webdriver
     * @param browserAttribute        the desired attribute of the browser window
     *                                under test
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param assertionComparisonType 1 is literalComparison, 2 is regexComparison,
     *                                3 is containsComparison, 4 is
     *                                caseInsensitiveComparison
     * @param assertionType           either 'true' for a positive assertion that
     *                                the browser attribute actual value matches the
     *                                expected value, or 'false' for a negative
     *                                assertion that the browser attribute actual
     *                                value doesn't match the expected value
     */
    public static void assertBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
	    int assertionComparisonType, Boolean assertionType) {
	JSWaiter.waitForLazyLoading();

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

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, assertionComparisonType, assertionType)) {
	case 1:
	    if (Boolean.TRUE.equals(assertionType)) {
		pass("assertBrowserAttribute", driver, "Assertion Passed; actual value of [" + browserAttribute
			+ "] does match expected value [" + expectedValue + "].");
	    } else {
		pass("assertBrowserAttribute", driver,
			"Assertion Passed; actual value of [" + browserAttribute + "] equals [" + actualValue
				+ "] which does not match expected value [" + expectedValue + "].");
	    }
	    break;
	case 0:
	    if (Boolean.TRUE.equals(assertionType)) {
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
     * Asserts browser attribute equals expectedValue if AssertionType is POSITIVE,
     * or does not equal expectedValue if AssertionType is NEGATIVE. Supports
     * CurrentUrl, PageSource, Title, WindowHandle, WindowPosition, WindowSize
     * 
     * *
     * <p>
     * This method will be removed soon. Use
     * {@link Assertions#assertBrowserAttribute(WebDriver , String , String , AssertionComparisonType , AssertionType )}
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
	assertBrowserAttribute(driver, browserAttribute, expectedValue, assertionComparisonType.getValue(),
		assertionType.getValue());
    }

    /**
     * Asserts that the expectedValue is related to the actualValue using the
     * desired comparativeRelationType if AssertionType is true, or not related if
     * AssertionType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Assertions#assertComparativeRelation(Number, Number, ComparativeRelationType, AssertionType)}
     * instead.
     * 
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param actualValue             the actual value (calculated data) of this
     *                                assertion
     * @param comparativeRelationType accepts standard java Equality, Relational,
     *                                and Conditional Operators, except [not equal
     *                                to]:
     *                                https://docs.oracle.com/javase/tutorial/java/nutsandbolts/op2.html
     * @param assertionType           either 'true' for a positive assertion that
     *                                the expectedValue is related to the
     *                                actualValue using the desired
     *                                comparativeRelationType, or 'false' for a
     *                                negative assertion that the expectedValue is
     *                                not related to the actualValue using the
     *                                desired comparativeRelationType
     */
    public static void assertComparativeRelation(Number expectedValue, Number actualValue,
	    String comparativeRelationType, Boolean assertionType) {
	ReportManager
		.logDiscrete("Assertion [" + "assertComparativeRelation" + "] is being performed, with expectedValue ["
			+ expectedValue + "], comparativeRelationType [" + comparativeRelationType + "], actualValue ["
			+ actualValue + "], and assertionType [" + assertionType + "].");

	if (Boolean.TRUE.equals(assertionType)) {
	    try {
		switch (comparativeRelationType) {
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
		pass("Assertion Passed; actual value [" + actualValue + "] is " + comparativeRelationType
			+ " expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value [" + actualValue + "] is not " + comparativeRelationType
			+ " expected value [" + expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail(ERROR_UNHANDLED_EXCEPTION, e);
	    }
	} else {
	    try {
		switch (comparativeRelationType) {
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

		pass("Assertion Passed; actual value [" + actualValue + "] is not " + comparativeRelationType
			+ " expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value [" + actualValue + "] is " + comparativeRelationType
			+ " expected value [" + expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail(ERROR_UNHANDLED_EXCEPTION, e);
	    }
	}
    }

    /**
     * Asserts that the expectedValue is related to the actualValue using the
     * desired comparativeRelationType if AssertionType is POSITIVE, or not related
     * if AssertionType is NEGATIVE.
     * 
     * 
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param actualValue             the actual value (calculated data) of this
     *                                assertion
     * @param comparativeRelationType assertComparativeRelation.GREATER_THAN,
     *                                GREATER_THAN_OR_EQUALS, LESS_THAN,
     *                                LESS_THAN_OR_EQUALS, EQUALS
     * @param assertionType           AssertionType.POSITIVE, NEGATIVE
     * 
     */
    public static void assertComparativeRelation(Number expectedValue, Number actualValue,
	    ComparativeRelationType comparativeRelationType, AssertionType assertionType) {
	assertComparativeRelation(expectedValue, actualValue, comparativeRelationType.getValue(),
		assertionType.getValue());
    }

    /**
     * Asserts that a certain file exists if AssertionType is true, or doesn't exist
     * if AssertionType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Assertions#assertFileExists(String , String , int , AssertionType )}
     * instead.
     * 
     * @param fileFolderName  The location of the folder that contains the target
     *                        file, relative to the project's root folder, ending
     *                        with a /
     * @param fileName        The name of the target file (including its extension
     *                        if any)
     * @param numberOfRetries number of times to try to find the file, given that
     *                        each retry is separated by a 500 millisecond wait time
     * @param assertionType   either 'true' for a positive assertion that the file
     *                        exists, or 'false' for a negative assertion that the
     *                        file doesn't exist
     */
    public static void assertFileExists(String fileFolderName, String fileName, int numberOfRetries,
	    Boolean assertionType) {
	ReportManager.logDiscrete("Assertion [" + "assertFileExists" + "] is being performed for target directory ["
		+ fileFolderName + "], and target file [" + fileName + "].");
	if (FileActions.doesFileExist(fileFolderName, fileName, numberOfRetries)) {
	    if (Boolean.TRUE.equals(assertionType)) {
		pass("Assertion Passed; target file [" + fileName + "] exists under the target path ["
			+ FileActions.getAbsolutePath(fileFolderName, fileName) + "].");
	    } else {
		fail("Assertion Failed; target file [" + fileName + "] exists under the target path ["
			+ FileActions.getAbsolutePath(fileFolderName, fileName) + "].");
	    }

	} else {
	    if (Boolean.TRUE.equals(assertionType)) {
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
	assertFileExists(fileFolderName, fileName, numberOfRetries, assertionType.getValue());
    }

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
}