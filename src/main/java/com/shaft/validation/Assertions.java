package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;

import com.shaft.browser.BrowserActions;
import com.shaft.element.ElementActions;
import com.shaft.io.FileManager;
import com.shaft.io.ReportManager;
import com.shaft.io.ScreenshotManager;
import com.shaft.support.JavaActions;

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
	ReportManager.logDiscreet("Assertion [" + "assertEquals" + "] is being performed, with expectedValue ["
		+ expectedValue + "], actualValue [" + actualValue + "], and assertionType [" + assertionType + "].");

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, comparisonType, assertionType)) {
	case 1:
	    if (assertionType) {
		pass("Assertion Passed; actual value does match expected value [" + expectedValue + "].");

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
		fail("Assertion Failed; actual value does match expected value [" + expectedValue + "].");
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

    @Deprecated
    public static void assertEquals(Object expectedValue, Object actualValue, Boolean assertionType) {
	assertEquals(expectedValue, actualValue, 2, assertionType);
    }

    /**
     * Asserts that object is null if AssertionType is true, or is not null if
     * AssertionType is false.
     * 
     * @param object        the object under test
     * @param assertionType either 'true' for a positive assertion that the object
     *                      refers to null, or 'false' for a negative assertion that
     *                      the object doesn't refer to null
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
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param assertionType  either 'true' for a positive assertion that the element
     *                       exists, or 'false' for a negative assertion that the
     *                       element doesn't exist
     */
    public static void assertElementExists(WebDriver driver, By elementLocator, Boolean assertionType) {
	ReportManager.logDiscreet("Assertion [" + "assertElementExists" + "] is being performed.");
	try {
	    switch (ElementActions.getElementsCount(driver, elementLocator, elementDoesntExistTimeout,
		    retriesBeforeThrowingElementNotFoundException)) {
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

    @Deprecated
    public static void assertElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
	    String expectedValue, Boolean assertionType) {
	assertElementAttribute(driver, elementLocator, elementAttribute, expectedValue, 2, assertionType);
    }

    /**
     * Asserts webElement attribute equals expectedValue if AssertionType is true,
     * or does not equal expectedValue if AssertionType is false. Supports Text,
     * TagName, Size, Other Attributes
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
	ReportManager.logDiscreet("Assertion [" + "assertElementAttribute"
		+ "] is being performed for target attribute [" + elementAttribute + "].");
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
     * Asserts webElement CSSProperty equals expectedValue if AssertionType is true,
     * or does not equal expectedValue if AssertionType is false.
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
	ReportManager.logDiscreet("Assertion [" + "assertElementCSSProperty"
		+ "] is being performed for target CSS Property [" + propertyName + "].");

	ReportManager.setDiscreetLogging(true);
	String actualValue = ElementActions.getCSSProperty(driver, elementLocator, propertyName);
	ReportManager.setDiscreetLogging(false);

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

    @Deprecated
    public static void assertBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
	    Boolean assertionType) {
	assertBrowserAttribute(driver, browserAttribute, expectedValue, 2, assertionType);
    }

    /**
     * Asserts browser attribute equals expectedValue if AssertionType is true, or
     * does not equal expectedValue if AssertionType is false. Supports CurrentUrl,
     * PageSource, Title, WindowHandle, WindowPosition, WindowSize
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

    @Deprecated
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

    /**
     * Asserts that the expectedValue is related to the actualValue using the
     * desired comparativeRelationType if AssertionType is true, or not related if
     * AssertionType is false.
     * 
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param actualValue             the actual value (calculated data) of this
     *                                assertion
     * @param comparativeRelationType accepts >, >=, <, <=, ==
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
		.logDiscreet("Assertion [" + "assertComparativeRelation" + "] is being performed, with expectedValue ["
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
     * Asserts that a certain file exists if AssertionType is true, or doesn't exist
     * if AssertionType is false.
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
	ReportManager.logDiscreet("Assertion [" + "assertFileExists" + "] is being performed for target directory ["
		+ fileFolderName + "], and target file [" + fileName + "].");
	if (FileManager.doesFileExist(fileFolderName, fileName, numberOfRetries)) {
	    if (assertionType) {
		pass("Assertion Passed; target file [" + fileName + "] exists under the target path ["
			+ FileManager.getAbsolutePath(fileFolderName, fileName) + "].");
	    } else {
		fail("Assertion Failed; target file [" + fileName + "] exists under the target path ["
			+ FileManager.getAbsolutePath(fileFolderName, fileName) + "].");
	    }

	} else {
	    if (assertionType) {
		fail("Assertion Failed; target file [" + fileName + "] doesn't exist under the target path ["
			+ FileManager.getAbsolutePath(fileFolderName, fileName) + "], tried for ["
			+ numberOfRetries * 500 + "] milliseconds.");
	    } else {
		pass("Assertion Passed; target file [" + fileName + "] doesn't exist under the target path ["
			+ FileManager.getAbsolutePath(fileFolderName, fileName) + "], tried for ["
			+ numberOfRetries * 500 + "] milliseconds.");
	    }
	}
    }

}