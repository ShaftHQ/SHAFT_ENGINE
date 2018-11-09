package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;

import com.shaft.browser.BrowserActions;
import com.shaft.element.ElementActions;
import com.shaft.io.ReportManager;
import com.shaft.io.ScreenshotManager;

public class Assertions {

    private static int elementDoesntExistTimeout = 4;
    private static int retriesBeforeThrowingElementNotFoundException = 1;

    private Assertions() {
	throw new IllegalStateException("Utility class");
    }

    private static void fail(String message, Throwable realCause) {
	ReportManager.log(message);
	Assert.fail(message, realCause);
    }

    private static void fail(String message) {
	ReportManager.log(message);
	Assert.fail(message);
    }

    private static void fail(WebDriver driver, String message, Throwable realCause) {
	ScreenshotManager.captureScreenShot(driver, false);
	fail(message, realCause);
    }

    private static void fail(WebDriver driver, String message) {
	ScreenshotManager.captureScreenShot(driver, false);
	fail(message);
    }

    private static void fail(WebDriver driver, By elementLocator, String message, Throwable realCause) {
	ScreenshotManager.captureScreenShot(driver, elementLocator, false);
	fail(message, realCause);
    }

    private static void fail(WebDriver driver, By elementLocator, String message) {
	ScreenshotManager.captureScreenShot(driver, elementLocator, false);
	fail(message);
    }

    private static void pass(String message) {
	ReportManager.log(message);
    }

    private static void pass(WebDriver driver, String message) {
	ScreenshotManager.captureScreenShot(driver, true);
	pass(message);
    }

    private static void pass(WebDriver driver, By elementLocator, String message) {
	ScreenshotManager.captureScreenShot(driver, elementLocator, true);
	pass(message);
    }

    /**
     * Asserts that two objects are equal if AssertionType is true, or not equal if
     * AssertionType is false.
     * 
     * @param expectedValue
     *            the expected value (test data) of this assertion
     * @param actualValue
     *            the actual value (calculated data) of this assertion
     * @param assertionType
     *            either 'true' for a positive assertion that the objects are equal,
     *            or 'false' for a negative assertion that the objects are not equal
     */
    public static void assertEquals(Object expectedValue, Object actualValue, Boolean assertionType) {
	// enhance to handle different comparison types and leave regex as default

	ReportManager.logDiscreet("Assertion [" + "assertEquals" + "] is being performed, with expectedValue ["
		+ expectedValue + "], actualValue [" + actualValue + "], and assertionType [" + assertionType + "].");
	// String escapedExpectedValue = String.valueOf(expectedValue);
	// escapedExpectedValue =
	// escapeSpecialCharacters(String.valueOf(expectedValue));

	if (assertionType) {
	    try {
		// Assert.assertEquals(expectedValue, actualValue);
		Assert.assertTrue((String.valueOf(actualValue)).matches(String.valueOf(expectedValue)));
		pass("Assertion Passed; actual value does match expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value [" + actualValue + "] does not match expected value ["
			+ expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
	    }
	} else {
	    try {
		// Assert.assertNotEquals(expectedValue, actualValue);
		Assert.assertFalse((String.valueOf(actualValue)).matches(String.valueOf(expectedValue)));
		pass("Assertion Passed; actual value [" + actualValue + "] does not match expected value ["
			+ expectedValue + "].");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value does match expected value [" + expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
	    }
	}
    }

    /**
     * Asserts that object is null if AssertionType is true, or is not null if
     * AssertionType is false.
     * 
     * @param object
     *            the object under test
     * @param assertionType
     *            either 'true' for a positive assertion that the object refers to
     *            null, or 'false' for a negative assertion that the object doesn't
     *            refer to null
     */
    public static void assertNull(Object object, Boolean assertionType) {
	ReportManager.logDiscreet("Assertion [" + "assertNull" + "] is being performed.");

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
     * Asserts that webElement found using the provided driver and locator exists if
     * AssertionType is true, or does not exist if AssertionType is false.
     * 
     * @param driver
     *            the current instance of Selenium webdriver
     * @param elementLocator
     *            the locator of the webElement under test (By xpath, id, selector,
     *            name ...etc)
     * @param assertionType
     *            either 'true' for a positive assertion that the element exists, or
     *            'false' for a negative assertion that the element doesn't exist
     */
    public static void assertElementExists(WebDriver driver, By elementLocator, Boolean assertionType) {
	ReportManager.logDiscreet("Assertion [" + "assertElementExists" + "] is being performed.");
	try {
	    switch (ElementActions.getElementsCount(driver, elementLocator, elementDoesntExistTimeout,
		    retriesBeforeThrowingElementNotFoundException)) {
	    case 0:
		if (assertionType) {
		    fail(driver,
			    "Assertion Failed; element does not exist. Locator [" + elementLocator.toString() + "].");
		} else {
		    pass(driver,
			    "Assertion Passed; element does not exist. Locator [" + elementLocator.toString() + "].");
		}
		break;
	    case 1:
		if (assertionType) {
		    pass(driver, elementLocator, "Assertion Passed; element exists and is unique. Locator ["
			    + elementLocator.toString() + "].");
		} else {
		    fail(driver, elementLocator, "Assertion Failed; element exists and is unique. Locator ["
			    + elementLocator.toString() + "].");
		}
		break;
	    default:
		fail(driver, "Assertion Failed; element is not unique. Locator [" + elementLocator.toString() + "].");
		break;
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    fail("Assertion Failed; an unhandled exception occured.", e);
	}
    }

    public static void assertElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
	    String expectedValue, Boolean assertionType) {
	assertElementAttribute(driver, elementLocator, elementAttribute, expectedValue, 2, assertionType);
    }

    /**
     * Asserts webElement attribute equals expectedValue if AssertionType is true,
     * or does not equal expectedValue if AssertionType is false. Supports Text,
     * TagName, Size, Other Attributes
     * 
     * @param driver
     *            the current instance of Selenium webdriver
     * @param elementLocator
     *            the locator of the webElement under test (By xpath, id, selector,
     *            name ...etc)
     * @param elementAttribute
     *            the desired attribute of the webElement under test
     * @param expectedValue
     *            the expected value (test data) of this assertion
     * @param comparisonType
     *            1 is literalComparison, 2 is regexComparison, 3 is
     *            containsComparison, 4 is caseInsensitiveComparison
     * @param assertionType
     *            either 'true' for a positive assertion that the element attribute
     *            actual value matches the expected value, or 'false' for a negative
     *            assertion that the element attribute actual value doesn't match
     *            the expected value
     */
    public static void assertElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
	    String expectedValue, int comparisonType, Boolean assertionType) {
	ReportManager.logDiscreet("Assertion [" + "assertElementAttribute"
		+ "] is being performed for target attribute [" + elementAttribute + "].");
	// String escapedExpectedValue = expectedValue;
	// escapedExpectedValue = escapeSpecialCharacters(expectedValue);

	String actualValue = null;

	ReportManager.setDiscreetLogging(true);
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
	ReportManager.setDiscreetLogging(false);

	if (assertionType) {
	    // handle returned special characters that are seen as regex
	    try {
		switch (comparisonType) {
		case 1:
		    // case sensitive literal equivalence
		    Assert.assertTrue((String.valueOf(actualValue)).equals(String.valueOf(expectedValue)));
		    break;
		case 2:
		    // regex comparison
		    Assert.assertTrue((String.valueOf(actualValue)).matches(String.valueOf(expectedValue)));
		    break;
		case 3:
		    // contains
		    Assert.assertTrue((String.valueOf(actualValue)).contains(String.valueOf(expectedValue)));
		    break;
		case 4:
		    // case insensitive equivalence
		    Assert.assertTrue((String.valueOf(actualValue)).equalsIgnoreCase(String.valueOf(expectedValue)));
		    break;
		default:
		    // unhandled case
		    fail("Assertion Failed; an unhandled comparison case was selected.");
		    break;
		}
		pass(driver, elementLocator, "Assertion Passed; actual value of [" + elementAttribute
			+ "] does match expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		fail(driver, elementLocator, "Assertion Failed; actual value of [" + elementAttribute + "] equals ["
			+ actualValue + "] which does not match expected value [" + expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
	    }
	} else {
	    try {
		switch (comparisonType) {
		case 1:
		    // case sensitive literal equivalence
		    Assert.assertFalse((String.valueOf(actualValue)).equals(String.valueOf(expectedValue)));
		    break;
		case 2:
		    // regex comparison
		    Assert.assertFalse((String.valueOf(actualValue)).matches(String.valueOf(expectedValue)));
		    break;
		case 3:
		    // contains
		    Assert.assertFalse((String.valueOf(actualValue)).contains(String.valueOf(expectedValue)));
		    break;
		case 4:
		    // case insensitive equivalence
		    Assert.assertFalse((String.valueOf(actualValue)).equalsIgnoreCase(String.valueOf(expectedValue)));
		    break;
		default:
		    // unhandled case
		    fail("Assertion Failed; an unhandled comparison case was selected.");
		    break;
		}
		pass(driver, elementLocator, "Assertion Passed; actual value of [" + elementAttribute + "] equals ["
			+ actualValue + "] which does not match expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		fail(driver, elementLocator, "Assertion Failed; actual value of [" + elementAttribute
			+ "] does match expected value [" + expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
	    }
	}
    }

    public static void assertBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
	    Boolean assertionType) {
	assertBrowserAttribute(driver, browserAttribute, expectedValue, 2, assertionType);
    }

    /**
     * Asserts browser attribute equals expectedValue if AssertionType is true, or
     * does not equal expectedValue if AssertionType is false. Supports CurrentUrl,
     * PageSource, Title, WindowHandle, WindowPosition, WindowSize
     * 
     * @param driver
     *            the current instance of Selenium webdriver
     * @param browserAttribute
     *            the desired attribute of the browser window under test
     * @param expectedValue
     *            the expected value (test data) of this assertion
     * @param comparisonType
     *            1 is literalComparison, 2 is regexComparison, 3 is
     *            containsComparison, 4 is caseInsensitiveComparison
     * @param assertionType
     *            either 'true' for a positive assertion that the browser attribute
     *            actual value matches the expected value, or 'false' for a negative
     *            assertion that the browser attribute actual value doesn't match
     *            the expected value
     */
    public static void assertBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
	    int comparisonType, Boolean assertionType) {

	ReportManager.logDiscreet("Assertion [" + "assertBrowserAttribute"
		+ "] is being performed for target attribute [" + browserAttribute + "].");
	// String escapedExpectedValue = expectedValue;
	// escapedExpectedValue = escapeSpecialCharacters(expectedValue);

	String actualValue = null;

	ReportManager.setDiscreetLogging(true);
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
	ReportManager.setDiscreetLogging(false);

	if (assertionType) {
	    try {
		switch (comparisonType) {
		case 1:
		    // case sensitive literal equivalence
		    Assert.assertTrue((String.valueOf(actualValue)).equals(String.valueOf(expectedValue)));
		    break;
		case 2:
		    // regex comparison
		    Assert.assertTrue((String.valueOf(actualValue)).matches(String.valueOf(expectedValue)));
		    break;
		case 3:
		    // contains
		    Assert.assertTrue((String.valueOf(actualValue)).contains(String.valueOf(expectedValue)));
		    break;
		case 4:
		    // case insensitive equivalence
		    Assert.assertTrue((String.valueOf(actualValue)).equalsIgnoreCase(String.valueOf(expectedValue)));
		    break;
		default:
		    // unhandled case
		    fail("Assertion Failed; an unhandled comparison case was selected.");
		    break;
		}
		pass(driver, "Assertion Passed; actual value of [" + browserAttribute + "] does match expected value ["
			+ expectedValue + "].");
	    } catch (AssertionError e) {
		fail(driver, "Assertion Failed; actual value of [" + browserAttribute + "] equals [" + actualValue
			+ "] which does not match expected value [" + expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
	    }
	} else {
	    try {
		switch (comparisonType) {
		case 1:
		    // case sensitive literal equivalence
		    Assert.assertFalse((String.valueOf(actualValue)).equals(String.valueOf(expectedValue)));
		    break;
		case 2:
		    // regex comparison
		    Assert.assertFalse((String.valueOf(actualValue)).matches(String.valueOf(expectedValue)));
		    break;
		case 3:
		    // contains
		    Assert.assertFalse((String.valueOf(actualValue)).contains(String.valueOf(expectedValue)));
		    break;
		case 4:
		    // case insensitive equivalence
		    Assert.assertFalse((String.valueOf(actualValue)).equalsIgnoreCase(String.valueOf(expectedValue)));
		    break;
		default:
		    // unhandled case
		    fail("Assertion Failed; an unhandled comparison case was selected.");
		    break;
		}
		pass(driver, "Assertion Passed; actual value of [" + browserAttribute + "] equals [" + actualValue
			+ "] which does not match expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		fail(driver, "Assertion Failed; actual value of [" + browserAttribute + "] does match expected value ["
			+ expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
	    }
	}
    }

    public static void assertGreaterThanOrEquals(Number expectedValue, Number actualValue, Boolean assertionType) {
	ReportManager.logDiscreet("Assertion [" + "assertGreaterThanOrEquals"
		+ "] is being performed, with expectedValue [" + expectedValue + "], actualValue [" + actualValue
		+ "], and assertionType [" + assertionType + "].");
	if (assertionType) {
	    try {
		Assert.assertTrue(actualValue.floatValue() >= expectedValue.floatValue());
		pass("Assertion Passed; actual value [" + actualValue + "] is greater than or equals expected value ["
			+ expectedValue + "].");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value [" + actualValue
			+ "] is not greater than or equals expected value [" + expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
	    }
	} else {
	    try {
		Assert.assertFalse(actualValue.floatValue() >= expectedValue.floatValue());
		pass("Assertion Passed; actual value [" + actualValue
			+ "] is not greater than or equals expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		fail("Assertion Failed; actual value [" + actualValue + "] is greater than or equals expected value ["
			+ expectedValue + "].", e);
	    } catch (Exception e) {
		ReportManager.log(e);
		fail("Assertion Failed; an unhandled exception occured.", e);
	    }
	}
    }

}