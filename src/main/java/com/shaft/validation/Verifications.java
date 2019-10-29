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

//TODO: Add optional message to be added to the log of the verification to describe what it does

public class Verifications {
    private static StringBuilder verificationFailures = new StringBuilder();
    private static List<String> verificationFailuresList = new ArrayList<>();

    public static void resetVerificationFailuresMessage() {
	Verifications.verificationFailuresList = new ArrayList<>();
    }

    public static String getVerificationFailuresMessage() {
	return String.join("\nAND ", verificationFailuresList);
    }

    private static AssertionError verificationError = null;

    public static AssertionError getVerificationError() {
	return verificationError;
    }

    public static void resetVerificationError() {
	Verifications.verificationError = null;
    }

    private static StringBuilder verificationSuccesses = new StringBuilder();

    private static int attemptsBeforeThrowingElementNotFoundException = Integer
	    .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static int attemptsBeforeThrowingElementNotFoundExceptionInCaseElementShouldntExist = 1;

    private static final String ERROR_INVALID_COMPARISON_OPERATOR = "Verification Failed; invalid comparison operator used.";
    private static final String ERROR_UNHANDLED_EXCEPTION = "Verification Failed; an unhandled exception occured.";

    private static Boolean discreetLoggingState = Boolean.valueOf(System.getProperty("alwaysLogDiscreetly"));

    public enum VerificationType {
	POSITIVE(true), NEGATIVE(false);

	private Boolean value;

	VerificationType(Boolean type) {
	    this.value = type;
	}

	protected boolean getValue() {
	    return value;
	}
    }

    public enum VerificationComparisonType {
	EQUALS(1), CONTAINS(3), MATCHES(2), CASE_INSENSITIVE(4);

	private int value;

	VerificationComparisonType(int type) {
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

    private Verifications() {
	throw new IllegalStateException("Utility class");
    }

    private static void reportVerificationResults(String actionName, WebDriver driver, By elementLocator) {
	reportVerificationResults(actionName, driver, elementLocator, null);
    }

    private static void reportVerificationResults(String actionName, WebDriver driver, By elementLocator,
	    List<List<Object>> attachments) {
	String verificationSuccessesString = verificationSuccesses.toString().trim();
	if (!"".equals(verificationSuccessesString)) {
	    if (driver != null) {
		try {
		    ReportManager.log(verificationSuccessesString, Arrays
			    .asList(ScreenshotManager.captureScreenShot(driver, elementLocator, actionName, true)));
		} catch (NullPointerException e) {
		    // elementLocator is null, meaning that there is no element attached to this
		    // verification
		    ReportManager.log(verificationSuccessesString,
			    Arrays.asList(ScreenshotManager.captureScreenShot(driver, actionName, true)));
		}
	    } else {
		if (attachments != null) {
		    ReportManager.log(verificationSuccessesString, attachments);
		} else {
		    ReportManager.log(verificationSuccessesString);
		}
	    }
	    verificationSuccesses.delete(0, verificationSuccesses.length());
	}

	String verificationFailuresString = verificationFailures.toString().trim();
	if (!"".equals(verificationFailuresString)) {
	    Boolean initialDiscreteLoggingState = ReportManager.isDiscreteLogging();
	    ReportManager.setDiscreteLogging(false);
	    if (driver != null) {
		try {
		    ReportManager.log(verificationFailuresString, Arrays
			    .asList(ScreenshotManager.captureScreenShot(driver, elementLocator, actionName, false)));
		} catch (NullPointerException e) {
		    // elementLocator is null, meaning that there is no element attached to this
		    // verification
		    ReportManager.log(verificationFailuresString,
			    Arrays.asList(ScreenshotManager.captureScreenShot(driver, actionName, false)));
		}
	    } else {
		ReportManager.log(verificationFailuresString, attachments);
	    }
	    ReportManager.setDiscreteLogging(initialDiscreteLoggingState);
	    verificationFailuresList.add(verificationFailuresString);
	    verificationError = new AssertionError(getVerificationFailuresMessage());
	    verificationFailures.delete(0, verificationFailures.length());
	}
    }

    private static void reportVerificationResults(String actionName, String expectedValue, String actualValue,
	    WebDriver driver, By elementLocator) {

	List<Object> expectedTestDataAttachment = Arrays.asList("Validation Test Data", "Expected Value",
		expectedValue);
	List<Object> actualTestDataAttachment = Arrays.asList("Validation Test Data", "Actual Value", actualValue);

	reportVerificationResults(actionName, null, null,
		Arrays.asList(expectedTestDataAttachment, actualTestDataAttachment));
    }

    /**
     * Verifies that two strings are equal if VerificationType is POSITIVE, or not
     * equal if VerificationType is NEGATIVE.
     * 
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param actualValue                the actual value (calculated data) of this
     *                                   verification
     * @param verificationComparisonType VerificationComparisonType.LITERAL,
     *                                   CONTAINS, REGEX, CASE_INSENSITIVE
     * @param verificationType           VerificationType.POSITIVE, NEGATIVE
     */
    public static void verifyEquals(Object expectedValue, Object actualValue,
	    VerificationComparisonType verificationComparisonType, VerificationType verificationType) {
	verifyEquals(expectedValue, actualValue, verificationComparisonType.getValue(), verificationType.getValue());
    }

    /**
     * Verifies that two strings are equal if VerificationType is true, or not equal
     * if VerificationType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Verifications#verifyEquals(Object , Object , VerificationComparisonType , VerificationType)}
     * instead.
     * 
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param actualValue                the actual value (calculated data) of this
     *                                   verification
     * @param verificationComparisonType 1 is literalComparison, 2 is
     *                                   regexComparison, 3 is containsComparison, 4
     *                                   is caseInsensitiveComparison
     * @param verificationType           either 'true' for a positive verification
     *                                   that the objects are equal, or 'false' for
     *                                   a negative verification that the objects
     *                                   are not equal
     */
    public static void verifyEquals(Object expectedValue, Object actualValue, int verificationComparisonType,
	    Boolean verificationType) {
	ReportManager.logDiscrete("Verification [" + "verifyEquals" + "] is being performed, with expectedValue ["
		+ expectedValue + "], actualValue [" + actualValue + "], comparisonType [" + verificationComparisonType
		+ "], and verificationType [" + verificationType + "].");

	Boolean isExpectedOrActualValueLong = expectedValue.toString().length() >= 500
		|| actualValue.toString().length() >= 500;

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, verificationComparisonType,
		verificationType)) {
	case 1:
	    if (verificationType) {
		if (!isExpectedOrActualValueLong) {
		    verificationSuccesses.append("Verification Passed; actual value [" + actualValue
			    + "] does match expected value [" + expectedValue + "].");
		} else {
		    verificationSuccesses.append(
			    "Verification Passed; actual value does match expected value. Kindly check the attachments for more details.");
		}
	    } else {
		if (!isExpectedOrActualValueLong) {
		    verificationSuccesses.append("Verification Passed; actual value [" + actualValue
			    + "] does not match expected value [" + expectedValue + "].");
		} else {
		    verificationSuccesses.append(
			    "Verification Passed; actual value does not match expected value. Kindly check the attachments for more details.");
		}
	    }
	    break;
	case 0:
	    if (verificationType) {
		if (!isExpectedOrActualValueLong) {
		    verificationFailures.append("Verification Failed; actual value [" + actualValue
			    + "] does not match expected value [" + expectedValue + "].");
		} else {
		    verificationFailures.append("Verification Failed; actual value does not match expected value.");
		}
	    } else {
		if (!isExpectedOrActualValueLong) {
		    verificationFailures.append("Verification Failed; actual value [" + actualValue
			    + "] does match expected value [" + expectedValue + "].");
		} else {
		    verificationFailures.append("Verification Failed; actual value does match expected value.");
		}
	    }
	    break;
	case -1:
	    verificationFailures.append(ERROR_INVALID_COMPARISON_OPERATOR);
	    break;
	default:
	    verificationFailures.append(ERROR_UNHANDLED_EXCEPTION);
	    break;
	}

	if (isExpectedOrActualValueLong) {
	    reportVerificationResults("verifyEquals", expectedValue.toString(), actualValue.toString(), null, null);
	} else {
	    reportVerificationResults("verifyEquals", null, null);
	}
    }

    /**
     * Verifies that object is null if VerificationType is POSITIVE, or not equal if
     * VerificationType is NEGATIVE.
     * 
     * @param object           the object under test
     * @param verificationType VerificationType.POSITIVE, NEGATIVE
     */
    public static void verifyNull(Object object, VerificationType verificationType) {
	verifyNull(object, verificationType.getValue());
    }

    /**
     * Verifies that object is null if VerificationType is true, or not equal if
     * VerificationType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Verifications#verifyNull(Object , VerificationType)} instead.
     * 
     * @param object           the object under test
     * @param verificationType either 'true' for a positive verification that the
     *                         object refers to null, or 'false' for a negative
     *                         verification that the object doesn't refer to null
     */
    public static void verifyNull(Object object, Boolean verificationType) {
	ReportManager.logDiscrete("Verification [" + "verifyNull" + "] is being performed.");

	if (verificationType) {
	    try {
		Assert.assertNull(object);
		verificationSuccesses.append("Verification Passed; actual value is null.");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value is not null.");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append(ERROR_UNHANDLED_EXCEPTION);
	    }
	} else {
	    try {
		Assert.assertNotNull(object);
		verificationSuccesses.append("Verification Passed; actual value is not null.");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value is null.");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append(ERROR_UNHANDLED_EXCEPTION);
	    }
	}
	reportVerificationResults("verifyNull", null, null);
    }

    /**
     * Verifies that webElement exists if VerificationType is POSITIVE, or does not
     * exist if VerificationType is NEGATIVE.
     * 
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param verificationType VerificationType.POSITIVE, NEGATIVE
     */
    public static void verifyElementExists(WebDriver driver, By elementLocator, VerificationType verificationType) {
	verifyElementExists(driver, elementLocator, verificationType.getValue());
    }

    /**
     * Verifies that webElement exists if VerificationType is true, or does not
     * exist if VerificationType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Verifications#verifyElementExists(WebDriver , By , VerificationType)}
     * instead.
     * 
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param verificationType either 'true' for a positive verification that the
     *                         element exists, or 'false' for a negative
     *                         verification that the element doesn't exist
     */
    public static void verifyElementExists(WebDriver driver, By elementLocator, Boolean verificationType) {
	ReportManager.logDiscrete("Verification [" + "verifyElementExists" + "] is being performed.");
	try {
	    int customAttempts = attemptsBeforeThrowingElementNotFoundException;
	    if (!verificationType) {
		customAttempts = attemptsBeforeThrowingElementNotFoundExceptionInCaseElementShouldntExist;
	    }

	    int elementsCount = ElementActions.getElementsCount(driver, elementLocator, customAttempts);

	    switch (elementsCount) {
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
	    verificationFailures.append(ERROR_UNHANDLED_EXCEPTION);
	}
	reportVerificationResults("verifyElementExists", driver, elementLocator);
    }

    /**
     * Verifies that webElement attribute equals expectedValue if verificationType
     * is POSITIVE, or does not equal expectedValue if verificationType is NEGATIVE.
     * 
     * @param driver                     the current instance of Selenium webdriver
     * @param elementLocator             the locator of the webElement under test
     *                                   (By xpath, id, selector, name ...etc)
     * @param elementAttribute           the desired attribute of the webElement
     *                                   under test
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param verificationComparisonType VerificationComparisonType.LITERAL,
     *                                   CONTAINS, REGEX, CASE_INSENSITIVE
     * @param verificationType           VerificationType.POSITIVE, NEGATIVE
     *                                   expected value
     */
    public static void verifyElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
	    String expectedValue, VerificationComparisonType verificationComparisonType,
	    VerificationType verificationType) {
	verifyElementAttribute(driver, elementLocator, elementAttribute, expectedValue,
		verificationComparisonType.getValue(), verificationType.getValue());
    }

    /**
     * Verifies that webElement attribute equals expectedValue if verificationType
     * is true, or does not equal expectedValue if verificationType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Verifications#verifyElementAttribute(WebDriver , By , String, String, VerificationComparisonType , VerificationType)}
     * instead.
     * 
     * @param driver                     the current instance of Selenium webdriver
     * @param elementLocator             the locator of the webElement under test
     *                                   (By xpath, id, selector, name ...etc)
     * @param elementAttribute           the desired attribute of the webElement
     *                                   under test
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param verificationComparisonType 1 is literalComparison, 2 is
     *                                   regexComparison, 3 is containsComparison, 4
     *                                   is caseInsensitiveComparison
     * @param verificationType           either 'true' for a positive verification
     *                                   that the element attribute actual value
     *                                   matches the expected value, or 'false' for
     *                                   a negative verification that the element
     *                                   attribute actual value doesn't match the
     *                                   expected value
     */
    public static void verifyElementAttribute(WebDriver driver, By elementLocator, String elementAttribute,
	    String expectedValue, int verificationComparisonType, Boolean verificationType) {
	ReportManager.logDiscrete("Verification [" + "verifyElementAttribute"
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

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, verificationComparisonType,
		verificationType)) {
	case 1:
	    if (verificationType) {
		verificationSuccesses.append("Verification Passed; actual value of [" + elementAttribute
			+ "] does match expected value [" + expectedValue + "].");
	    } else {
		verificationSuccesses.append("Verification Passed; actual value of [" + elementAttribute + "] equals ["
			+ actualValue + "] which does not match expected value [" + expectedValue + "].");
	    }
	    break;
	case 0:
	    if (verificationType) {
		verificationFailures.append("Verification Failed; actual value of [" + elementAttribute + "] equals ["
			+ actualValue + "] which does not match expected value [" + expectedValue + "].");
	    } else {
		verificationFailures.append("Verification Failed; actual value of [" + elementAttribute
			+ "] does match expected value [" + actualValue + "].");
	    }
	    break;
	case -1:
	    verificationFailures.append(ERROR_INVALID_COMPARISON_OPERATOR);
	    break;
	default:
	    verificationFailures.append(ERROR_UNHANDLED_EXCEPTION);
	    break;
	}
	reportVerificationResults("verifyElementAttribute", driver, elementLocator);
    }

    /**
     * Verifies webElement CSSProperty equals expectedValue if verificationType is
     * POSITIVE, or does not equal expectedValue if verificationType is NEGATIVE.
     * 
     * @param driver                     the current instance of Selenium webdriver
     * @param elementLocator             the locator of the webElement under test
     *                                   (By xpath, id, selector, name ...etc)
     * @param propertyName               the target CSS property of the webElement
     *                                   under test
     * @param expectedValue              the expected value (test data) of this
     *                                   assertion
     * @param verificationComparisonType VerificationComparisonType.LITERAL,
     *                                   CONTAINS, REGEX, CASE_INSENSITIVE
     * @param verificationType           VerificationType.POSITIVE, NEGATIVE
     *                                   expected value
     */
    public static void verifyElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
	    String expectedValue, VerificationComparisonType verificationComparisonType,
	    VerificationType verificationType) {
	verifyElementCSSProperty(driver, elementLocator, propertyName, expectedValue,
		verificationComparisonType.getValue(), verificationType.getValue());
    }

    /**
     * Verifies webElement CSSProperty equals expectedValue if verificationType is
     * true, or does not equal expectedValue if verificationType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Verifications#verifyElementCSSProperty(WebDriver , By , String, String, VerificationComparisonType , VerificationType)}
     * instead.
     * 
     * @param driver                     the current instance of Selenium webdriver
     * @param elementLocator             the locator of the webElement under test
     *                                   (By xpath, id, selector, name ...etc)
     * @param propertyName               the target CSS property of the webElement
     *                                   under test
     * @param expectedValue              the expected value (test data) of this
     *                                   assertion
     * @param verificationComparisonType 1 is literalComparison, 2 is
     *                                   regexComparison, 3 is containsComparison, 4
     *                                   is caseInsensitiveComparison
     * @param verificationType           either 'true' for a positive assertion that
     *                                   the element CSSProperty actual value
     *                                   matches the expected value, or 'false' for
     *                                   a negative assertion that the element
     *                                   CSSProperty actual value doesn't match the
     *                                   expected value
     */
    public static void verifyElementCSSProperty(WebDriver driver, By elementLocator, String propertyName,
	    String expectedValue, int verificationComparisonType, Boolean verificationType) {
	ReportManager.logDiscrete("Verification [" + "verifyElementCSSProperty"
		+ "] is being performed for target CSS Property [" + propertyName + "].");

	discreetLoggingState = ReportManager.isDiscreteLogging();
	ReportManager.setDiscreteLogging(true);
	String actualValue = ElementActions.getCSSProperty(driver, elementLocator, propertyName);
	ReportManager.setDiscreteLogging(discreetLoggingState);

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, verificationComparisonType,
		verificationType)) {
	case 1:
	    if (verificationType) {
		verificationSuccesses.append("Verification Passed; actual CSS Property value of [" + propertyName
			+ "] does match expected value [" + expectedValue + "].");
	    } else {
		verificationSuccesses
			.append("Verification Passed; actual CSS Property value of [" + propertyName + "] equals ["
				+ actualValue + "] which does not match expected value [" + expectedValue + "].");
	    }
	    break;
	case 0:
	    if (verificationType) {
		verificationFailures
			.append("Verification Failed; actual CSS Property value of [" + propertyName + "] equals ["
				+ actualValue + "] which does not match expected value [" + expectedValue + "].");
	    } else {
		verificationFailures.append("Verification Failed; actual CSS Property value of [" + propertyName
			+ "] does match expected value [" + expectedValue + "].");
	    }
	    break;
	case -1:
	    verificationFailures.append(ERROR_INVALID_COMPARISON_OPERATOR);
	    break;
	default:
	    verificationFailures.append(ERROR_UNHANDLED_EXCEPTION);
	    break;
	}
	reportVerificationResults("verifyElementCSSProperty", driver, elementLocator);
    }

    /**
     * Verifies that browser attribute equals expectedValue if verificationType is
     * POSITIVE, or does not equal expectedValue if verificationType is NEGATIVE.
     * 
     * @param driver                     the current instance of Selenium webdriver
     * @param browserAttribute           the desired attribute of the browser window
     *                                   under test
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param verificationComparisonType VerificationComparisonType.LITERAL,
     *                                   CONTAINS, REGEX, CASE_INSENSITIVE
     * @param verificationType           VerificationType.POSITIVE, NEGATIVE
     *                                   expected value
     */
    public static void verifyBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
	    VerificationComparisonType verificationComparisonType, VerificationType verificationType) {
	verifyBrowserAttribute(driver, browserAttribute, expectedValue, verificationComparisonType.getValue(),
		verificationType.getValue());
    }

    /**
     * Verifies that browser attribute equals expectedValue if verificationType is
     * true, or does not equal expectedValue if verificationType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Verifications#verifyBrowserAttribute(WebDriver , String, String, VerificationComparisonType , VerificationType)}
     * instead.
     * 
     * @param driver                     the current instance of Selenium webdriver
     * @param browserAttribute           the desired attribute of the browser window
     *                                   under test
     * @param expectedValue              the expected value (test data) of this
     *                                   verification
     * @param verificationComparisonType 1 is literalComparison, 2 is
     *                                   regexComparison, 3 is containsComparison, 4
     *                                   is caseInsensitiveComparison
     * @param verificationType           either 'true' for a positive verification
     *                                   that the browser attribute actual value
     *                                   matches the expected value, or 'false' for
     *                                   a negative verification that the browser
     *                                   attribute actual value doesn't match the
     *                                   expected value
     */
    public static void verifyBrowserAttribute(WebDriver driver, String browserAttribute, String expectedValue,
	    int verificationComparisonType, Boolean verificationType) {
	JSWaiter.waitForLazyLoading();

	ReportManager.logDiscrete("Verification [" + "verifyBrowserAttribute"
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

	switch (JavaActions.compareTwoObjects(expectedValue, actualValue, verificationComparisonType,
		verificationType)) {
	case 1:
	    if (verificationType) {
		verificationSuccesses.append("Verification Passed; actual value of [" + browserAttribute
			+ "] does match expected value [" + expectedValue + "].");
	    } else {
		verificationSuccesses.append("Verification Passed; actual value of [" + browserAttribute + "] equals ["
			+ actualValue + "] which does not match expected value [" + expectedValue + "].");
	    }
	    break;
	case 0:
	    if (verificationType) {
		verificationFailures.append("Verification Failed; actual value [" + actualValue
			+ "] does not match expected value [" + expectedValue + "].");
	    } else {
		verificationFailures.append("Verification Failed; actual value of [" + browserAttribute
			+ "] does match expected value [" + actualValue + "].");
	    }
	    break;
	case -1:
	    verificationFailures.append(ERROR_INVALID_COMPARISON_OPERATOR);
	    break;
	default:
	    verificationFailures.append(ERROR_UNHANDLED_EXCEPTION);
	    break;
	}
	reportVerificationResults("verifyBrowserAttribute", driver, null);
    }

    /**
     * Verifies that the expectedValue is related to the actualValue using the
     * desired comparativeRelationType if verificationType is POSITIVE, or not
     * related if verificationType is NEGATIVE.
     * 
     * @param expectedValue           the expected value (test data) of this
     *                                assertion
     * @param actualValue             the actual value (calculated data) of this
     *                                assertion
     * @param comparativeRelationType assertComparativeRelation.GREATER_THAN,
     *                                GREATER_THAN_OR_EQUALS, LESS_THAN,
     *                                LESS_THAN_OR_EQUALS, EQUALS
     * @param verificationType        VerificationType.POSITIVE, NEGATIVE expected
     *                                value
     */
    public static void verifyComparativeRelation(Number expectedValue, Number actualValue,
	    ComparativeRelationType comparativeRelationType, VerificationType verificationType) {
	verifyComparativeRelation(expectedValue, actualValue, comparativeRelationType.getValue(),
		verificationType.getValue());
    }

    /**
     * Verifies that the expectedValue is related to the actualValue using the
     * desired comparativeRelationType if verificationType is true, or not related
     * if verificationType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Verifications#verifyComparativeRelation(Number , Number, ComparativeRelationType , VerificationType)}
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
     * @param verificationType        either 'true' for a positive assertion that
     *                                the expectedValue is related to the
     *                                actualValue using the desired
     *                                comparativeRelationType, or 'false' for a
     *                                negative assertion that the expectedValue is
     *                                not related to the actualValue using the
     *                                desired comparativeRelationType
     */
    public static void verifyComparativeRelation(Number expectedValue, Number actualValue,
	    String comparativeRelationType, Boolean verificationType) {
	ReportManager.logDiscrete(
		"Verification [" + "verifyComparativeRelation" + "] is being performed, with expectedValue ["
			+ expectedValue + "], comparativeRelationType [" + comparativeRelationType + "], actualValue ["
			+ actualValue + "], and verificationType [" + verificationType + "].");

	if (verificationType) {
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
		    verificationFailures.append(ERROR_INVALID_COMPARISON_OPERATOR);
		    break;
		}
		verificationSuccesses.append("Verification Passed; actual value [" + actualValue + "] is "
			+ comparativeRelationType + " expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value [" + actualValue + "] is not "
			+ comparativeRelationType + " expected value [" + expectedValue + "].");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append(ERROR_UNHANDLED_EXCEPTION);
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
		    verificationFailures.append(ERROR_INVALID_COMPARISON_OPERATOR);
		    break;
		}

		verificationSuccesses.append("Verification Passed; actual value [" + actualValue + "] is not "
			+ comparativeRelationType + " expected value [" + expectedValue + "].");
	    } catch (AssertionError e) {
		verificationFailures.append("Verification Failed; actual value [" + actualValue + "] is "
			+ comparativeRelationType + " expected value [" + expectedValue + "].");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append(ERROR_UNHANDLED_EXCEPTION);
	    }
	}
	reportVerificationResults("verifyComparativeRelation", null, null);
    }

    /**
     * Verifies that a certain file exists if verificationType is POSITIVE, or
     * doesn't exist if verificationType is NEGATIVE.
     * 
     * @param fileFolderName   The location of the folder that contains the target
     *                         file, relative to the project's root folder, ending
     *                         with a /
     * @param fileName         The name of the target file (including its extension
     *                         if any)
     * @param numberOfRetries  number of times to try to find the file, given that
     *                         each retry is separated by a 500 millisecond wait
     *                         time
     * @param verificationType VerificationType.POSITIVE, NEGATIVE expected value
     */
    public static void verifyFileExists(String fileFolderName, String fileName, int numberOfRetries,
	    VerificationType verificationType) {
	verifyFileExists(fileFolderName, fileName, numberOfRetries, verificationType.getValue());
    }

    /**
     * Verifies that a certain file exists if verificationType is true, or doesn't
     * exist if verificationType is false.
     * 
     * <p>
     * This method will be removed soon. Use
     * {@link Verifications#verifyFileExists(String , String, int , VerificationType)}
     * instead.
     * 
     * @param fileFolderName   The location of the folder that contains the target
     *                         file, relative to the project's root folder, ending
     *                         with a /
     * @param fileName         The name of the target file (including its extension
     *                         if any)
     * @param numberOfRetries  number of times to try to find the file, given that
     *                         each retry is separated by a 500 millisecond wait
     *                         time
     * @param verificationType either 'true' for a positive verification that the
     *                         file exists, or 'false' for a negative assertion that
     *                         the file doesn't exist
     */
    public static void verifyFileExists(String fileFolderName, String fileName, int numberOfRetries,
	    Boolean verificationType) {
	ReportManager.logDiscrete("Verification [" + "verifyFileExists" + "] is being performed for target directory ["
		+ fileFolderName + "], and target file [" + fileName + "].");
	if (FileActions.doesFileExist(fileFolderName, fileName, numberOfRetries)) {
	    if (verificationType) {
		verificationSuccesses
			.append("Verification Passed; target file [" + fileName + "] exists under the target path ["
				+ FileActions.getAbsolutePath(fileFolderName, fileName) + "].");
	    } else {
		verificationFailures
			.append("Verification Failed; target file [" + fileName + "] exists under the target path ["
				+ FileActions.getAbsolutePath(fileFolderName, fileName) + "].");
	    }

	} else {
	    if (verificationType) {
		verificationFailures.append(
			"Verification Failed; target file [" + fileName + "] doesn't exist under the target path ["
				+ FileActions.getAbsolutePath(fileFolderName, fileName) + "], tried for ["
				+ numberOfRetries * 500 + "] milliseconds.");
	    } else {
		verificationSuccesses.append(
			"Verification Passed; target file [" + fileName + "] doesn't exist under the target path ["
				+ FileActions.getAbsolutePath(fileFolderName, fileName) + "], tried for ["
				+ numberOfRetries * 500 + "] milliseconds.");
	    }
	}
	reportVerificationResults("verifyFileExists", null, null);
    }

    /**
     * Verifies that the provided conditional statement evaluates to true if
     * VerificationType is POSITIVE, or to false if VerificationType is NEGATIVE.
     * 
     * @param conditionalStatement the statement that will be evaluated to see if it
     *                             matches the expected result
     * @param verificationType     VerificationType.POSITIVE, NEGATIVE
     */
    public static void verifyTrue(Boolean conditionalStatement, VerificationType verificationType) {
	ReportManager.logDiscrete("Verification [" + "verifyTrue" + "] is being performed for target value ["
		+ conditionalStatement + "], with VerificationType [" + verificationType + "].");
	if (verificationType.getValue()) {
	    if (conditionalStatement == null) {
		verificationFailures.append(
			"Verification Failed; conditional statement evaluated to NULL while it was expected to evaluate to true.");
	    }
	    try {
		Assert.assertTrue(conditionalStatement);
		verificationSuccesses
			.append("Verification Passed; conditional statement evaluated to true as expected.");
	    } catch (AssertionError e) {
		verificationFailures.append(
			"Verification Failed; conditional statement evaluated to false while it was expected to evaluate to true.");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append(ERROR_UNHANDLED_EXCEPTION);
	    }
	} else {
	    if (conditionalStatement == null) {
		verificationFailures.append(
			"Verification Failed; conditional statement evaluated to NULL while it was expected to evaluate to false.");
	    }
	    try {
		Assert.assertFalse(conditionalStatement);
		verificationSuccesses
			.append("Verification Passed; conditional statement evaluated to false as expected.");
	    } catch (AssertionError e) {
		verificationFailures.append(
			"Verification Failed; conditional statement evaluated to true while it was expected to evaluate to false.");
	    } catch (Exception e) {
		ReportManager.log(e);
		verificationFailures.append(ERROR_UNHANDLED_EXCEPTION);
	    }
	}
	reportVerificationResults("verifyTrue", null, null);
    }

    /**
     * Verifies that the target API Response object matches the expected
     * referenceJsonFile if VerificationType is POSITIVE, or doesn't match it if
     * VerificationType is NEGATIVE.
     * 
     * @param response              the full response object returned by
     *                              performRequest method.
     * @param referenceJsonFilePath the full absolute path to the test data file
     *                              that will be used as a reference for this
     *                              comparison
     * @param comparisonType        ComparisonType.EQUALS, CONTAINS, MATCHES,
     *                              EQUALS_STRICT; Note that MATCHES ignores the
     *                              content ordering inside the JSON
     * @param verificationType      AssertionType.POSITIVE, NEGATIVE
     */
    public static void verifyJSONFileContent(Response response, String referenceJsonFilePath,
	    ComparisonType comparisonType, VerificationType verificationType) {
	verifyJSONFileContent(response, referenceJsonFilePath, comparisonType, "", verificationType);
    }

    /**
     * Verifies that the target array extracted by parsing the API Response object
     * matches the expected referenceJsonFile if VerificationType is POSITIVE, or
     * doesn't match it if VerificationType is NEGATIVE.
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
     * @param verificationType      AssertionType.POSITIVE, NEGATIVE
     */
    public static void verifyJSONFileContent(Response response, String referenceJsonFilePath,
	    ComparisonType comparisonType, String jsonPathToTargetArray, VerificationType verificationType) {
	if (jsonPathToTargetArray.equals("")) {
	    ReportManager.logDiscrete("Verification [" + "verifyJSONFileContent"
		    + "] is being performed, with referenceJsonFile [" + referenceJsonFilePath + "], comparisonType ["
		    + comparisonType + "], and verificationType [" + verificationType + "].");
	} else {
	    ReportManager.logDiscrete("Verification [" + "verifyJSONFileContent"
		    + "] is being performed, with referenceJsonFile [" + referenceJsonFilePath
		    + "], jsonPathToTargetArray [" + jsonPathToTargetArray + "], comparisonType [" + comparisonType
		    + "], and verificationType [" + verificationType + "].");
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
	    if (verificationType.getValue()) {
		// comparison passed and is expected to pass
		verificationSuccesses.append(
			"Verification Passed; the actual API response does match the expected JSON file at this path \""
				+ referenceJsonFilePath + "\".");
	    } else {
		// comparison passed and is expected to fail
		verificationFailures.append(
			"Verification Failed; the actual API response does match the expected JSON file at this path \""
				+ referenceJsonFilePath + "\".");
	    }
	} else {
	    if (verificationType.getValue()) {
		// comparison failed and is expected to pass
		verificationFailures.append(
			"Verification Failed; the actual API response does not match the expected JSON file at this path \""
				+ referenceJsonFilePath + "\".");
	    } else {
		// comparison failed and is expected to fail
		verificationSuccesses.append(
			"Verification Passed; the actual API response does not match the expected JSON file at this path \""
				+ referenceJsonFilePath + "\".");
	    }
	}
	reportVerificationResults("verifyJSONFileContent", null, null, attachments);
    }

}