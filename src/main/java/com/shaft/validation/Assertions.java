package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;

import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.support.JavaActions;

public class Assertions {
    private static int attemptsBeforeThrowingElementNotFoundException = Integer
	    .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static int attemptsBeforeThrowingElementNotFoundExceptionInCaseElementShouldntExist = 1;

    private static Boolean discreetLoggingState = Boolean.valueOf(System.getProperty("alwaysLogDiscreetly"));

    public enum AssertionType {
	POSITIVE(true), NEGATIVE(false);

	private Boolean value;

	AssertionType(Boolean type) {
	    this.value = type;
	}

	protected boolean value() {
	    return value;
	}
    }

    public enum AssertionComparisonType {
	LITERAL(1), CONTAINS(3), REGEX(2), CASE_INSENSITIVE(4);

	private int value;

	AssertionComparisonType(int type) {
	    this.value = type;
	}

	protected int value() {
	    return value;
	}
    }

    public enum ComparativeRelationType {
	GREATER_THAN(">"), GREATER_THAN_OR_EQUALS(">="), LESS_THAN("<"), LESS_THAN_OR_EQUALS("<="), EQUALS("==");

	private String value;

	ComparativeRelationType(String type) {
	    this.value = type;
	}

	protected String value() {
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

    private static void fail(String actionName, WebDriver driver, String message) {
	ScreenshotManager.captureScreenShot(driver, actionName, false);
	fail(message);
    }

    private static void fail(String actionName, WebDriver driver, By elementLocator, String message) {
	ScreenshotManager.captureScreenShot(driver, elementLocator, actionName, false);
	fail(message);
    }

    private static void pass(String message) {
	ReportManager.log(message);
    }

    private static void pass(String actionName, WebDriver driver, String message) {
	ScreenshotManager.captureScreenShot(driver, actionName, true);
	pass(message);
    }

    private static void pass(String actionName, WebDriver driver, By elementLocator, String message) {
	ScreenshotManager.captureScreenShot(driver, elementLocator, actionName, true);
	pass(message);
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
     * @param expectedValue  the expected value (test data) of this assertion
     * @param actualValue    the actual value (calculated data) of this assertion
     * @param comparisonType 1 is literalComparison, 2 is regexComparison, 3 is
     *                       containsComparison, 4 is caseInsensitiveComparison
     * @param assertionType  either 'true' for a positive assertion that the objects
     *                       are equal, or 'false' for a negative assertion that the
     *                       objects are not equal
     */
    public static void assertEquals(Object expectedValue, Object actualValue, int comparisonType,
	    Boolean assertionType) {
	ReportManager.logDiscrete("Assertion [" + "assertEquals" + "] is being performed, with expectedValue ["
		+ expectedValue + "], actualValue [" + actualValue + "], comparisonType [" + comparisonType
		+ "], and assertionType [" + assertionType + "].");

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, comparisonType, assertionType)) {
	case 1:
	    if (assertionType) {
		pass("Assertion Passed; actual value [" + actualValue + "] does match expected value [" + expectedValue
			+ "].");

	    } else {
		pass("Assertion Passed; actual value [" + actualValue + "] does not match expected value ["
			+ expectedValue + "].");
	    }
	    break;
	case 0:
	    if (assertionType) {
		fail("Assertion Failed; actual value [" + actualValue + "] does not match expected value ["
			+ expectedValue + "].");
	    } else {
		fail("Assertion Failed; actual value [" + actualValue + "] does match expected value [" + expectedValue
			+ "].");
	    }
	    break;
	case -1:
	    fail("Assertion Failed; invalid comparison operator used.");
	    break;
	default:
	    fail("Assertion Failed; an unhandled exception occured.");
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
	assertEquals(expectedValue, actualValue, assertionComparisonType.value(), assertionType.value());
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

	if (assertionType) {
	    try {
		Assert.assertNull(object);
		pass("Assertion Passed; actual value is null.");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value is not null.", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
	    }
	} else {
	    try {
		Assert.assertNotNull(object);
		pass("Assertion Passed; actual value is not null.");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value is null.", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
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
	assertNull(object, assertionType.value);
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
	    if (!assertionType) {
		customAttempts = attemptsBeforeThrowingElementNotFoundExceptionInCaseElementShouldntExist;
	    }

	    switch (ElementActions.getElementsCount(driver, elementLocator, customAttempts)) {
	    case 0:
		if (assertionType) {
		    fail("assertElementExists", driver,
			    "Assertion Failed; element does not exist. Locator [" + elementLocator.toString() + "].");
		} else {
		    pass("assertElementExists", driver,
			    "Assertion Passed; element does not exist. Locator [" + elementLocator.toString() + "].");
		}
		break;
	    case 1:
		if (assertionType) {
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
	    fail("Assertion Failed; an unhandled exception occured.", e);
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
	assertElementExists(driver, elementLocator, assertionType.value());
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
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param elementAttribute the desired attribute of the webElement under test
     * @param expectedValue    the expected value (test data) of this assertion
     * @param comparisonType   1 is literalComparison, 2 is regexComparison, 3 is
     *                         containsComparison, 4 is caseInsensitiveComparison
     * @param assertionType    either 'true' for a positive assertion that the
     *                         element attribute actual value matches the expected
     *                         value, or 'false' for a negative assertion that the
     *                         element attribute actual value doesn't match the
     *                         expected value
     */
    public static void assertElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
	    String expectedValue, int comparisonType, Boolean assertionType) {
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

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, comparisonType, assertionType)) {
	case 1:
	    if (assertionType) {
		pass("assertElementAttribute", driver, elementLocator, "Assertion Passed; actual value of ["
			+ elementAttribute + "] does match expected value [" + expectedValue + "].");
	    } else {
		pass("assertElementAttribute", driver, elementLocator,
			"Assertion Passed; actual value of [" + elementAttribute + "] equals [" + actualValue
				+ "] which does not match expected value [" + expectedValue + "].");
	    }
	    break;
	case 0:
	    if (assertionType) {
		fail("assertElementAttribute", driver, elementLocator,
			"Assertion Failed; actual value of [" + elementAttribute + "] equals [" + actualValue
				+ "] which does not match expected value [" + expectedValue + "].");
	    } else {
		fail("assertElementAttribute", driver, elementLocator, "Assertion Failed; actual value of ["
			+ elementAttribute + "] does match expected value [" + expectedValue + "].");
	    }
	    break;
	case -1:
	    fail("Assertion Failed; invalid comparison operator used.");
	    break;
	default:
	    fail("Assertion Failed; an unhandled exception occured.");
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
	assertElementAttribute(driver, elementLocator, elementAttribute, expectedValue, assertionComparisonType.value(),
		assertionType.value());
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
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param propertyName   the target CSS property of the webElement under test
     * @param expectedValue  the expected value (test data) of this assertion
     * @param comparisonType 1 is literalComparison, 2 is regexComparison, 3 is
     *                       containsComparison, 4 is caseInsensitiveComparison
     * @param assertionType  either 'true' for a positive assertion that the element
     *                       CSSProperty actual value matches the expected value, or
     *                       'false' for a negative assertion that the element
     *                       CSSProperty actual value doesn't match the expected
     *                       value
     */
    public static void assertElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
	    String expectedValue, int comparisonType, Boolean assertionType) {
	ReportManager.logDiscrete("Assertion [" + "assertElementCSSProperty"
		+ "] is being performed for target CSS Property [" + propertyName + "].");

	discreetLoggingState = ReportManager.isDiscreteLogging();
	ReportManager.setDiscreteLogging(true);
	String actualValue = ElementActions.getCSSProperty(driver, elementLocator, propertyName);
	ReportManager.setDiscreteLogging(discreetLoggingState);

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, comparisonType, assertionType)) {
	case 1:
	    if (assertionType) {
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
	    if (assertionType) {
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
	    fail("Assertion Failed; invalid comparison operator used.");
	    break;
	default:
	    fail("Assertion Failed; an unhandled exception occured.");
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
	assertElementCSSProperty(driver, elementLocator, propertyName, expectedValue, assertionComparisonType.value(),
		assertionType.value());
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
     * @param driver           the current instance of Selenium webdriver
     * @param browserAttribute the desired attribute of the browser window under
     *                         test
     * @param expectedValue    the expected value (test data) of this assertion
     * @param comparisonType   1 is literalComparison, 2 is regexComparison, 3 is
     *                         containsComparison, 4 is caseInsensitiveComparison
     * @param assertionType    either 'true' for a positive assertion that the
     *                         browser attribute actual value matches the expected
     *                         value, or 'false' for a negative assertion that the
     *                         browser attribute actual value doesn't match the
     *                         expected value
     */
    public static void assertBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
	    int comparisonType, Boolean assertionType) {

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

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, comparisonType, assertionType)) {
	case 1:
	    if (assertionType) {
		pass("assertBrowserAttribute", driver, "Assertion Passed; actual value of [" + browserAttribute
			+ "] does match expected value [" + expectedValue + "].");
	    } else {
		pass("assertBrowserAttribute", driver,
			"Assertion Passed; actual value of [" + browserAttribute + "] equals [" + actualValue
				+ "] which does not match expected value [" + expectedValue + "].");
	    }
	    break;
	case 0:
	    if (assertionType) {
		fail("assertBrowserAttribute", driver,
			"Assertion Failed; actual value of [" + browserAttribute + "] equals [" + actualValue
				+ "] which does not match expected value [" + expectedValue + "].");
	    } else {
		fail("assertBrowserAttribute", driver, "Assertion Failed; actual value of [" + browserAttribute
			+ "] does match expected value [" + expectedValue + "].");
	    }
	    break;
	case -1:
	    fail("Assertion Failed; invalid comparison operator used.");
	    break;
	default:
	    fail("Assertion Failed; an unhandled exception occured.");
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
	assertBrowserAttribute(driver, browserAttribute, expectedValue, assertionComparisonType.value(),
		assertionType.value());
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

	if (assertionType) {
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
		    fail("Assertion Failed; invalid comparison operator used.");
		    break;
		}
		pass("Assertion Passed; actual value [" + actualValue + "] is " + comparativeRelationType
			+ " expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value [" + actualValue + "] is not " + comparativeRelationType
			+ " expected value [" + expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
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
		    fail("Assertion Failed; invalid comparison operator used.");
		    break;
		}

		pass("Assertion Passed; actual value [" + actualValue + "] is not " + comparativeRelationType
			+ " expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value [" + actualValue + "] is " + comparativeRelationType
			+ " expected value [" + expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
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
	assertComparativeRelation(expectedValue, actualValue, comparativeRelationType.value(), assertionType.value());
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
	    if (assertionType) {
		pass("Assertion Passed; target file [" + fileName + "] exists under the target path ["
			+ FileActions.getAbsolutePath(fileFolderName, fileName) + "].");
	    } else {
		fail("Assertion Failed; target file [" + fileName + "] exists under the target path ["
			+ FileActions.getAbsolutePath(fileFolderName, fileName) + "].");
	    }

	} else {
	    if (assertionType) {
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
	assertFileExists(fileFolderName, fileName, numberOfRetries, assertionType.value());
    }
}