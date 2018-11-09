package com.shaft.validation;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.ITestResult;
import org.testng.Reporter;

import com.shaft.browser.BrowserActions;
import com.shaft.element.ElementActions;
import com.shaft.io.ReportManager;
import com.shaft.io.ScreenshotManager;

public class Verifications {

    private static StringBuilder verificationFailures = new StringBuilder();
    private static StringBuilder verificationSuccesses = new StringBuilder();
    private static int elementDoesntExistTimeout = 4;
    private static int retriesBeforeThrowingElementNotFoundException = 1;

    private Verifications() {
	throw new IllegalStateException("Utility class");
    }

    private static void reportVerificationResults(WebDriver driver, By elementLocator) {
	String verificationSuccessesString = verificationSuccesses.toString().trim();
	if (!"".equals(verificationSuccessesString)) {
	    if (driver != null) {
		try {
		    ScreenshotManager.captureScreenShot(driver, elementLocator, true);
		} catch (NullPointerException e) {
		    // elementLocator is null, meaning that there is no element attached to this
		    // verification
		    ScreenshotManager.captureScreenShot(driver, true);
		}
	    }
	    ReportManager.log(verificationSuccessesString);
	    verificationSuccesses.delete(0, verificationSuccesses.length());
	}

	String verificationFailuresString = verificationFailures.toString().trim();
	if (!"".equals(verificationFailuresString)) {
	    if (driver != null) {
		try {
		    ScreenshotManager.captureScreenShot(driver, elementLocator, false);
		} catch (NullPointerException e) {
		    // elementLocator is null, meaning that there is no element attached to this
		    // verification
		    ScreenshotManager.captureScreenShot(driver, false);
		}
	    }
	    ReportManager.log(verificationFailuresString);
	    // Throw a new exception with the failure string, or append to current exception
	    // message
	    try {
		String oldMessage = Reporter.getCurrentTestResult().getThrowable().getMessage();
		Reporter.getCurrentTestResult()
			.setThrowable(new Throwable(oldMessage + "\nAND " + verificationFailuresString));
	    } catch (NullPointerException e) {
		Reporter.getCurrentTestResult().setThrowable(new Throwable(verificationFailuresString));
	    }
	    Reporter.getCurrentTestResult().setStatus(ITestResult.FAILURE);
	    verificationFailures.delete(0, verificationFailures.length());
	}
    }

    /**
     * Verifies that two strings are equal if VerificationType is true, or not equal
     * if VerificationType is false.
     * 
     * @param expectedValue
     *            the expected value (test data) of this verification
     * @param actualValue
     *            the actual value (calculated data) of this verification
     * @param verificationType
     *            either 'true' for a positive verification that the objects are
     *            equal, or 'false' for a negative verification that the objects are
     *            not equal
     */
    public static void verifyEquals(Object expectedValue, Object actualValue, Boolean verificationType) {
	ReportManager.logDiscreet(
		"Verification [" + "verifyEquals" + "] is being performed, with expectedValue [" + expectedValue
			+ "], actualValue [" + actualValue + "], and verificationType [" + verificationType + "].");
	// String escapedExpectedValue = String.valueOf(expectedValue);
	// escapedExpectedValue =
	// escapeSpecialCharacters(String.valueOf(expectedValue));
	if (verificationType) {
	    try {
		Assert.assertTrue((String.valueOf(actualValue)).matches(String.valueOf(expectedValue)));
		verificationSuccesses
			.append("Verification Passed; actual value does match expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value [" + actualValue
			+ "] does not match expected value [" + expectedValue + "].");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append("Verification Failed; an unhandled exception occured.");
	    }
	} else {
	    try {
		Assert.assertFalse((String.valueOf(actualValue)).matches(String.valueOf(expectedValue)));
		verificationSuccesses.append("Verification Passed; actual value [" + actualValue
			+ "] does not match expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		verificationFailures
			.append("Verification Failed; actual value does match expected value [" + actualValue + "].");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append("Verification Failed; an unhandled exception occured.");
	    }
	}
	reportVerificationResults(null, null);
    }

    /**
     * Verifies that object is null if VerificationType is true, or not equal if
     * VerificationType is false.
     * 
     * @param object
     *            the object under test
     * @param verificationType
     *            either 'true' for a positive verification that the object refers
     *            to null, or 'false' for a negative verification that the object
     *            doesn't refer to null
     */
    public static void verifyNull(Object object, Boolean verificationType) {
	ReportManager.logDiscreet("Verification [" + "verifyNull" + "] is being performed.");

	if (verificationType) {
	    try {
		Assert.assertNull(object);
		verificationSuccesses.append("Verification Passed; actual value is null.");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value is not null.");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append("Verification Failed; an unhandled exception occured.");
	    }
	} else {
	    try {
		Assert.assertNotNull(object);
		verificationSuccesses.append("Verification Passed; actual value is not null.");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value is null.");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append("Verification Failed; an unhandled exception occured.");
	    }
	}
	reportVerificationResults(null, null);
    }

    /**
     * Verifies that webElement exists if VerificationType is true, or does not
     * exist if VerificationType is false.
     * 
     * @param driver
     *            the current instance of Selenium webdriver
     * @param elementLocator
     *            the locator of the webElement under test (By xpath, id, selector,
     *            name ...etc)
     * @param verificationType
     *            either 'true' for a positive verification that the element exists,
     *            or 'false' for a negative verification that the element doesn't
     *            exist
     */
    public static void verifyElementExists(WebDriver driver, By elementLocator, Boolean verificationType) {
	ReportManager.logDiscreet("Verification [" + "verifyElementExists" + "] is being performed.");
	try {
	    switch (ElementActions.getElementsCount(driver, elementLocator, elementDoesntExistTimeout,
		    retriesBeforeThrowingElementNotFoundException)) {
	    case 0:
		if (verificationType) {
		    verificationFailures.append("Verification Failed; element does not exist. Locator ["
			    + elementLocator.toString() + "].");
		} else {
		    verificationSuccesses.append("Verification Passed; element does not exist. Locator ["
			    + elementLocator.toString() + "].");
		}
		elementLocator = null; // workaround to force take a screenshot of the whole page
		break;
	    case 1:
		if (verificationType) {
		    verificationSuccesses.append("Verification Passed; element exists and is unique. Locator ["
			    + elementLocator.toString() + "].");
		} else {
		    verificationFailures.append("Verification Failed; element exists and is unique. Locator ["
			    + elementLocator.toString() + "].");
		}
		break;
	    default:
		verificationFailures.append(
			"Verification Failed; element is not unique. Locator [" + elementLocator.toString() + "].");
		elementLocator = null; // workaround to force take a screenshot of the whole page
		break;
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    verificationFailures.append("Verification Failed; an unhandled exception occured.");
	}
	reportVerificationResults(driver, elementLocator);
    }

    public static void verifyElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
	    String expectedValue, Boolean verificationType) {
	verifyElementAttribute(driver, elementLocator, elementAttribute, expectedValue, 2, verificationType);
    }

    /**
     * Verifies that webElement attribute equals expectedValue if verificationType
     * is true, or does not equal expectedValue if verificationType is false.
     * 
     * @param driver
     *            the current instance of Selenium webdriver
     * @param elementLocator
     *            the locator of the webElement under test (By xpath, id, selector,
     *            name ...etc)
     * @param elementAttribute
     *            the desired attribute of the webElement under test
     * @param expectedValue
     *            the expected value (test data) of this verification
     * @param comparisonType
     *            1 is literalComparison, 2 is regexComparison, 3 is
     *            containsComparison, 4 is caseInsensitiveComparison
     * @param verificationType
     *            either 'true' for a positive verification that the element
     *            attribute actual value matches the expected value, or 'false' for
     *            a negative verification that the element attribute actual value
     *            doesn't match the expected value
     */
    public static void verifyElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
	    String expectedValue, int comparisonType, Boolean verificationType) {
	ReportManager.logDiscreet("Verification [" + "verifyElementAttribute"
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

	if (verificationType) {
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
		    verificationFailures.append("Verification Failed; an unhandled comparison case was selected.");
		    break;
		}
		verificationSuccesses.append("Verification Passed; actual value of [" + elementAttribute
			+ "] does match expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value of [" + elementAttribute + "] equals ["
			+ actualValue + "] which does not match expected value [" + expectedValue + "].");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append("Verification Failed; an unhandled exception occured.");
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
		    verificationFailures.append("Verification Failed; an unhandled comparison case was selected.");
		    break;
		}
		verificationSuccesses.append("Verification Passed; actual value of [" + elementAttribute + "] equals ["
			+ actualValue + "] which does not match expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value of [" + elementAttribute
			+ "] does match expected value [" + actualValue + "].");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append("Verification Failed; an unhandled exception occured.");
	    }
	}
	reportVerificationResults(driver, elementLocator);
    }

    public static void verifyBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
	    Boolean assertionType) {
	verifyBrowserAttribute(driver, browserAttribute, expectedValue, 2, assertionType);
    }

    /**
     * Verifies that browser attribute equals expectedValue if verificationType is
     * true, or does not equal expectedValue if verificationType is false.
     * 
     * @param driver
     *            the current instance of Selenium webdriver
     * @param browserAttribute
     *            the desired attribute of the browser window under test
     * @param expectedValue
     *            the expected value (test data) of this verification
     * @param comparisonType
     *            1 is literalComparison, 2 is regexComparison, 3 is
     *            containsComparison, 4 is caseInsensitiveComparison
     * @param verificationType
     *            either 'true' for a positive verification that the browser
     *            attribute actual value matches the expected value, or 'false' for
     *            a negative verification that the browser attribute actual value
     *            doesn't match the expected value
     */
    public static void verifyBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
	    int comparisonType, Boolean verificationType) {
	ReportManager.logDiscreet("Verification [" + "verifyBrowserAttribute"
		+ "] is being performed for target attribute [" + browserAttribute + "].");

	// String escapedExpectedValue = String.valueOf(expectedValue);
	// escapedExpectedValue =
	// escapeSpecialCharacters(String.valueOf(expectedValue));

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

	if (verificationType) {
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
		    verificationFailures.append("Verification Failed; an unhandled comparison case was selected.");
		    break;
		}
		verificationSuccesses.append("Verification Passed; actual value of [" + browserAttribute
			+ "] does match expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value [" + actualValue
			+ "] does not match expected value [" + expectedValue + "].");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append("Verification Failed; an unhandled exception occured.");
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
		    verificationFailures.append("Verification Failed; an unhandled comparison case was selected.");
		    break;
		}
		verificationSuccesses.append("Verification Passed; actual value of [" + browserAttribute + "] equals ["
			+ actualValue + "] which does not match expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value of [" + browserAttribute
			+ "] does match expected value [" + actualValue + "].");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append("Verification Failed; an unhandled exception occured.");
	    }
	}
	reportVerificationResults(driver, null);
    }

    public static void verifyGreaterThanOrEquals(Number expectedValue, Number actualValue, Boolean verificationType) {
	ReportManager.logDiscreet("Verification [" + "verifyGreaterThanOrEquals"
		+ "] is being performed, with expectedValue [" + expectedValue + "], actualValue [" + actualValue
		+ "], and verificationType [" + verificationType + "].");

	if (verificationType) {
	    try {
		Assert.assertTrue(actualValue.floatValue() >= expectedValue.floatValue());
		verificationSuccesses.append("Verification Passed; actual value [" + actualValue
			+ "] is greater than or equals expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value [" + actualValue
			+ "] is not greater than or equals expected value [" + expectedValue + "].");
	    }
	} else {
	    try {
		Assert.assertFalse(actualValue.floatValue() >= expectedValue.floatValue());
		verificationSuccesses.append("Verification Passed; actual value [" + actualValue
			+ "] is not greater than or equals expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value [" + actualValue
			+ "] is greater than or equals expected value [" + expectedValue + "].");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append("Verification Failed; an unhandled exception occured.");
	    }
	}
	reportVerificationResults(null, null);
    }
}
