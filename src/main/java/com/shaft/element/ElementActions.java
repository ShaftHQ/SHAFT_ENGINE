package com.shaft.element;

import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.openqa.selenium.By;
import org.openqa.selenium.ElementNotInteractableException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.interactions.Locatable;
import org.openqa.selenium.remote.UnreachableBrowserException;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.testng.Assert;

import com.shaft.browser.BrowserFactory;
import com.shaft.image.ScreenshotManager;
import com.shaft.io.ReportManager;

public class ElementActions {
    private static int defaultElementIdentificationTimeout = Integer
	    .parseInt(System.getProperty("defaultElementIdentificationTimeout").trim());
    private static int attemptsBeforeThrowingElementNotFoundException = Integer
	    .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());

    // this will only be used for switching back to default content
    static WebDriver lastUsedDriver = null;

    private ElementActions() {
	throw new IllegalStateException("Utility class");
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Reporting Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static void passAction(WebDriver driver, String actionName) {
	passAction(driver, null, actionName, null);
    }

    private static void passAction(WebDriver driver, By elementLocator, String actionName) {
	passAction(driver, elementLocator, actionName, null);
    }

    private static void passAction(WebDriver driver, String actionName, String testData) {
	passAction(driver, null, actionName, testData);
    }

    private static void passAction(WebDriver driver, By elementLocator, String actionName, String testData) {
	String message = "Element Action [" + actionName + "] successfully performed.";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	takeScreenshot(driver, elementLocator, actionName, testData, true);
	ReportManager.log(message);
    }

    private static void failAction(WebDriver driver, String actionName) {
	failAction(driver, actionName, null);
    }

    private static void failAction(WebDriver driver, String actionName, String testData) {
	String message = "[" + actionName + "] failed.";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	takeScreenshot(driver, null, actionName, testData, false);
	ReportManager.log(message);
	Assert.fail(message);
    }

    private static void takeScreenshot(WebDriver driver, By elementLocator, String actionName, String testData,
	    boolean passFailStatus) {
	if (passFailStatus) {
	    try {
		if ((elementLocator == null) && (testData == null)) {
		    // this only happens when switching to default content so there is no need to
		    // take a screenshot
		} else if (elementLocator != null) {
		    ScreenshotManager.captureScreenShot(driver, elementLocator, actionName, true);
		} else {
		    ScreenshotManager.captureScreenShot(driver, actionName, true);
		}
	    } catch (Exception e) {
		ReportManager.log(e);
		ReportManager.log(
			"Failed to take a screenshot of the element as it doesn't exist anymore. Taking a screenshot of the whole page.");
		ScreenshotManager.captureScreenShot(driver, actionName, true);
	    }
	} else {
	    ScreenshotManager.captureScreenShot(driver, actionName, false);
	}
	lastUsedDriver = driver;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Preparation and Support Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static boolean identifyUniqueElement(WebDriver driver, By elementLocator) {
	return identifyUniqueElement(driver, elementLocator, attemptsBeforeThrowingElementNotFoundException, true);
    }

    private static boolean identifyUniqueElement(WebDriver driver, By elementLocator, int numberOfAttempts,
	    boolean checkForVisibility) {
	int matchingElementsCount = getMatchingElementsCount(driver, elementLocator, numberOfAttempts);

	switch (matchingElementsCount) {
	case 0:
	    failAction(driver, "identifyUniqueElement",
		    "zero elements found matching this locator [" + elementLocator + "].");
	    break;
	case 1:
	    // unique element found
	    if (checkForVisibility && !elementLocator.toString().contains("input[@type='file']")
		    && !elementLocator.equals(By.tagName("html"))) {
		// scroll element into viewPort
		((Locatable) driver.findElement(elementLocator)).getCoordinates().inViewPort();

		// check for visibility
		try {
		    (new WebDriverWait(driver, defaultElementIdentificationTimeout))
			    .until(ExpectedConditions.visibilityOfElementLocated(elementLocator));
		} catch (TimeoutException e) {
		    ReportManager.log(e);
		    failAction(driver, "identifyUniqueElement",
			    "unique element matching this locator [" + elementLocator + "] is not visible.");
		}
	    }

	    if (elementLocator != null) {
		// ScreenshotManager.storeElementScreenshotForAISupportedElementIdentification(driver,
		// elementLocator);
	    }

	    return true;
	default:
	    failAction(driver, "identifyUniqueElement",
		    "multiple elements found matching this locator [" + elementLocator + "].");
	    break;
	}
	return false;
    }

    private static int getMatchingElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts) {
	return getMatchingElementsCount(driver, elementLocator, numberOfAttempts, true);
    }

    private static int getMatchingElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts,
	    boolean waitForLazyLoading) {
	if (waitForLazyLoading) {
	    JSWaiter.waitForLazyLoading();
	}

	if (elementLocator != null) {
	    int matchingElementsCount = 0;
	    int i = 0;
	    do {
		try {
		    (new WebDriverWait(driver, defaultElementIdentificationTimeout))
			    .until(ExpectedConditions.presenceOfElementLocated(elementLocator));

		    matchingElementsCount = driver.findElements(elementLocator).size();
		} catch (TimeoutException e) {
		    // in case of assert element doesn't exist, or if an element really doesn't
		    // exist this exception will be thrown from the fluent wait command

		    // this is expected and in this case the loop should just continue to iterate

		    // I've included the finElements line inside this try clause because it makes no
		    // added value to try again to find the element within the same attempt
		}
		i++;
	    } while ((matchingElementsCount == 0) && (i < numberOfAttempts));
	    return matchingElementsCount;
	} else {
	    return 0;
	}
    }

    private static String determineSuccessfulTextLocationStrategy(WebDriver driver, By elementLocator) {
	String elementText = driver.findElement(elementLocator).getText();
	String successfulTextLocationStrategy = "text";
	if (elementText.trim().equals("")) {
	    elementText = driver.findElement(elementLocator).getAttribute("textContent");
	    successfulTextLocationStrategy = "textContent";
	}
	if (elementText.trim().equals("")) {
	    successfulTextLocationStrategy = "value";
	}
	return successfulTextLocationStrategy;
    }

    private static String readTextBasedOnSuccessfulLocationStrategy(WebDriver driver, By elementLocator,
	    String successfulTextLocationStrategy) {
	String actualText = "";
	switch (successfulTextLocationStrategy) {
	case "text":
	    actualText = driver.findElement(elementLocator).getText();
	    break;
	case "textContent":
	    actualText = driver.findElement(elementLocator).getAttribute("textContent");
	    break;
	case "value":
	    actualText = driver.findElement(elementLocator).getAttribute("value");
	    break;
	default:
	    break;
	}
	return actualText;
    }

    private static void typeWrapper(WebDriver driver, By elementLocator, String targetText, Boolean isSecureTyping) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    // attempt to type
	    String successfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(driver, elementLocator);
	    String elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
		    successfulTextLocationStrategy);

	    if (!elementText.trim().equals("")) {
		// attempt to clear element then check text size
		clearBeforeTyping(driver, elementLocator, successfulTextLocationStrategy);
	    }
	    if ((getMatchingElementsCount(driver, elementLocator, attemptsBeforeThrowingElementNotFoundException) == 1)
		    && (!targetText.equals(""))) {
		performType(driver, elementLocator, targetText);
	    }
	    if ((getMatchingElementsCount(driver, elementLocator, attemptsBeforeThrowingElementNotFoundException) == 1)
		    && (!targetText.equals(""))) {
		// to confirm that the text was written successfully
		if (targetText.equals(readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
			successfulTextLocationStrategy))) {
		    if (isSecureTyping) {
			passAction(driver, elementLocator, "type", targetText.replaceAll(".", "*"));
		    } else {
			passAction(driver, elementLocator, "type", targetText);
		    }
		} else {
		    // attempt once to type using javascript then confirm typing was successful
		    // again
		    clearBeforeTyping(driver, elementLocator, successfulTextLocationStrategy);
		    performTypeUsingJavaScript(driver, elementLocator, targetText);
		    if (targetText.equals(readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
			    successfulTextLocationStrategy))) {
			if (isSecureTyping) {
			    passAction(driver, elementLocator, "type", targetText.replaceAll(".", "*"));
			} else {
			    passAction(driver, elementLocator, "type", targetText);
			}
		    } else {
			try {
			    Boolean discreetLoggingState = ReportManager.isDiscreteLogging();
			    ReportManager.setDiscreteLogging(true);
			    String actualText = getText(driver, elementLocator);
			    ReportManager.setDiscreteLogging(discreetLoggingState);
			    failAction(driver, "type", "Expected to type: \"" + targetText + "\", but ended up with: \""
				    + actualText + "\"");
			} catch (Exception e) {
			    failAction(driver, "type",
				    "Expected to type: \"" + targetText + "\", but ended up with something else");
			}
		    }
		}
	    }
	}
    }

    private static void clearBeforeTyping(WebDriver driver, By elementLocator, String successfulTextLocationStrategy) {
	// attempt clear using clear
	driver.findElement(elementLocator).clear();
	String elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
		successfulTextLocationStrategy);

	// attempt clear using sendKeys
	if (!elementText.trim().equals("")) {
	    driver.findElement(elementLocator).sendKeys("");
	}

	elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator, successfulTextLocationStrategy);
	// attempt clear using javascript
	performTypeUsingJavaScript(driver, elementLocator, "");

	elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator, successfulTextLocationStrategy);
	// attempt clear using letter by letter backspace
	if (!elementText.trim().equals("")) {
	    driver.findElement(elementLocator).sendKeys("");
	    for (int i = 0; i < elementText.length(); i++) {
		driver.findElement(elementLocator).sendKeys(Keys.BACK_SPACE);
	    }
	}
    }

    private static void performType(WebDriver driver, By elementLocator, String text) {
	// implementing loop to try and break out of the stale element exception issue
	for (int i = 0; i < attemptsBeforeThrowingElementNotFoundException; i++) {
	    try {
		// attempt to perform action
		driver.findElement(elementLocator).sendKeys(text);
		break;
	    } catch (StaleElementReferenceException | ElementNotInteractableException | UnreachableBrowserException
		    | NoSuchElementException | TimeoutException e) {
		if (i + 1 == attemptsBeforeThrowingElementNotFoundException) {
		    ReportManager.log(e);
		}
	    } catch (Exception e) {
		if (e.getMessage().contains("cannot focus element")
			&& (i + 1 == attemptsBeforeThrowingElementNotFoundException)) {
		    ReportManager.log(e);
		} else {
		    ReportManager.log(e);
		    ReportManager.log("Unhandled Exception: " + e.getMessage());
		}
	    }
	}
    }

    /**
     * Used in case the regular type text output didn't match with the expected type
     * text output
     * 
     * @param driver
     * @param elementLocator
     * @param text
     */
    private static void performTypeUsingJavaScript(WebDriver driver, By elementLocator, String text) {
	try {
	    ((JavascriptExecutor) driver).executeScript("arguments[0].value='" + text + "';",
		    driver.findElement(elementLocator));
	} catch (Exception e) {
	    ReportManager.log(e);
	}
    }

    private static void performClipboardActions(WebDriver driver, By elementLocator, String action) {

	try {
	    switch (action.toLowerCase()) {
	    case "copy":
		(Toolkit.getDefaultToolkit().getSystemClipboard())
			.setContents((new StringSelection(getText(driver, elementLocator))), null);
		break;
	    case "paste":
		pasteFromClipboard(driver, elementLocator);
		break;
	    case "cut":
		(Toolkit.getDefaultToolkit().getSystemClipboard())
			.setContents((new StringSelection(getText(driver, elementLocator))), null);
		type(driver, elementLocator, "");
		break;
	    case "select all":
		(new Actions(driver)).sendKeys(Keys.chord(Keys.CONTROL, "a")).perform();
		break;
	    case "unselect":
		(new Actions(driver)).sendKeys(Keys.ESCAPE).perform();
		break;
	    default:
		failAction(driver, "clipboardActions", "Unsupported Action");
		break;
	    }
	    passAction(driver, elementLocator, "clipboardActions", action);
	} catch (HeadlessException e) {
	    ReportManager.log(e);
	    ReportManager.log("Headless Exception: " + e.getMessage());
	}
    }

    private static void pasteFromClipboard(WebDriver driver, By elementLocator) {
	try {
	    typeAppend(driver, elementLocator,
		    (String) ((Toolkit.getDefaultToolkit().getSystemClipboard()).getContents(ElementActions.class))
			    .getTransferData(DataFlavor.stringFlavor));
	} catch (UnsupportedFlavorException e) {
	    ReportManager.log(e);
	    ReportManager.log("Unsupported Flavor Exception: " + e.getMessage());
	} catch (IOException e) {
	    ReportManager.log(e);
	    ReportManager.log("IO Exception: " + e.getMessage());
	}
    }

    private static void performClipboardActionsForMac(WebDriver driver, By elementLocator, String action) {
	switch (action.toLowerCase()) {
	case "copy":
	    (new Actions(driver)).sendKeys(Keys.chord(Keys.CONTROL, "c")).perform();
	    break;
	case "paste":
	    (new Actions(driver)).sendKeys(Keys.chord(Keys.CONTROL, "v")).perform();
	    break;
	case "cut":
	    (new Actions(driver)).sendKeys(Keys.chord(Keys.CONTROL, "x")).perform();
	    break;
	case "select all":
	    (new Actions(driver)).sendKeys(Keys.chord(Keys.CONTROL, "a")).perform();
	    break;
	case "unselect":
	    (new Actions(driver)).sendKeys(Keys.ESCAPE).perform();
	    break;
	default:
	    failAction(driver, "clipboardActions", "Unsupported Action");
	    break;
	}
	passAction(driver, elementLocator, "clipboardActions", action);
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [Public] Core Element Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * Returns the number of elements that match a certain elementLocator
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return integer value that represents the number of elements that match the
     *         desired elementLocator
     */
    public static int getElementsCount(WebDriver driver, By elementLocator) {
	return getMatchingElementsCount(driver, elementLocator, attemptsBeforeThrowingElementNotFoundException);
    }

    /**
     * Returns the number of elements that match a certain elementLocator
     * 
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param numberOfAttempts the number of retries before returning a count
     *                         [returns zero if no elements were found after all the
     *                         retries]
     * @return integer value that represents the number of elements that match the
     *         desired elementLocator
     */
    public static int getElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts) {
	return getMatchingElementsCount(driver, elementLocator, numberOfAttempts);
    }

    /**
     * Returns the number of elements that match a certain elementLocator
     * 
     * @param driver             the current instance of Selenium webdriver
     * @param elementLocator     the locator of the webElement under test (By xpath,
     *                           id, selector, name ...etc)
     * @param numberOfAttempts   the number of retries before returning a count
     *                           [returns zero if no elements were found after all
     *                           the retries]
     * @param waitForLazyLoading if true, will wait before lazy loading, else if
     *                           false skips this wait
     * @return integer value that represents the number of elements that match the
     *         desired elementLocator
     */
    public static int getElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts,
	    boolean waitForLazyLoading) {
	return getMatchingElementsCount(driver, elementLocator, numberOfAttempts, waitForLazyLoading);
    }

    /**
     * @deprecated Returns the number of elements that match a certain
     *             elementLocator, and respects the provided
     *             customElementIdentificationTimeout while attempting to locate
     *             those elements. This is multiplied by the provided
     *             retriesBeforeThrowingElementNotFoundException (default value is
     *             10). *Doesn't respect the customElementIdentificationTimeout
     *             parameter*
     * 
     * @param driver                                        the current instance of
     *                                                      Selenium webdriver
     * @param elementLocator                                the locator of the
     *                                                      webElement under test
     *                                                      (By xpath, id, selector,
     *                                                      name ...etc)
     * @param customElementIdentificationTimeout            the desired timeout in
     *                                                      seconds that should be
     *                                                      respected while
     *                                                      attempting to locate an
     *                                                      element using the
     *                                                      provided elementLocator
     * @param retriesBeforeThrowingElementNotFoundException the number of
     *                                                      retries/attempts for
     *                                                      each of which the
     *                                                      customElementIdentificationTimeout
     *                                                      is honored, and after
     *                                                      all of which an
     *                                                      ElementNotFoundException
     *                                                      is thrown
     * @return integer value that represents the number of elements that match the
     *         desired elementLocator
     */
    @Deprecated
    public static int getElementsCount(WebDriver driver, By elementLocator, int customElementIdentificationTimeout,
	    int retriesBeforeThrowingElementNotFoundException) {
	return getMatchingElementsCount(driver, elementLocator, retriesBeforeThrowingElementNotFoundException);
    }

    /**
     * @deprecated *Doesn't respect the customElementIdentificationTimeout or
     *             waitForLazyLoading parameters*
     * @param driver                                        the current instance of
     *                                                      Selenium webdriver
     * @param elementLocator                                the locator of the
     *                                                      webElement under test
     *                                                      (By xpath, id, selector,
     *                                                      name ...etc)
     * @param customElementIdentificationTimeout            the desired timeout in
     *                                                      seconds that should be
     *                                                      respected while
     *                                                      attempting to locate an
     *                                                      element using the
     *                                                      provided elementLocator
     * @param retriesBeforeThrowingElementNotFoundException the number of
     *                                                      retries/attempts for
     *                                                      each of which the
     *                                                      customElementIdentificationTimeout
     *                                                      is honored, and after
     *                                                      all of which an
     *                                                      ElementNotFoundException
     *                                                      is thrown
     * @param waitForLazyLoading                            whether or not to wait
     *                                                      for lazy loading
     * @return an integer value that represents the number of found elements
     */
    @Deprecated
    public static int getElementsCount(WebDriver driver, By elementLocator, int customElementIdentificationTimeout,
	    int retriesBeforeThrowingElementNotFoundException, boolean waitForLazyLoading) {
	return getMatchingElementsCount(driver, elementLocator, retriesBeforeThrowingElementNotFoundException);
    }

    /**
     * Switches focus to a certain iFrame, is mainly used in coordination with
     * {@link #switchToDefaultContent(WebDriver)} to navigate inside any iFrame
     * layer and go back to the main page
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the iFrame webElement under test (By
     *                       xpath, id, selector, name ...etc)
     */
    public static void switchToIframe(WebDriver driver, By elementLocator) {
	if (getMatchingElementsCount(driver, elementLocator, attemptsBeforeThrowingElementNotFoundException) == 1) {
	    driver.switchTo().frame((WebElement) driver.findElement(elementLocator));
	    // note to self: remove elementLocator in case of bug in screenshot manager
	    Boolean discreetLoggingState = ReportManager.isDiscreteLogging();
	    ReportManager.setDiscreteLogging(true);
	    passAction(driver, "switchToIframe");
	    ReportManager.setDiscreteLogging(discreetLoggingState);
	} else {
	    failAction(driver, "switchToIframe");
	}
    }

    /**
     * Switches focus to default content, is mainly used in coordination with
     * {@link #switchToIframe(WebDriver, By)} to exit any iFrame layer and go back
     * to the main page
     * 
     * @param driver the current instance of Selenium webdriver
     */
    public static void switchToDefaultContent(WebDriver driver) {
	try {
	    driver.switchTo().defaultContent();
	    Boolean discreetLoggingState = ReportManager.isDiscreteLogging();
	    ReportManager.setDiscreteLogging(true);
	    passAction(driver, "switchToDefaultContent");
	    ReportManager.setDiscreteLogging(discreetLoggingState);
	} catch (Exception e) {
	    failAction(driver, "switchToDefaultContent");
	}
    }

    public static void switchToDefaultContent() {
	if (BrowserFactory.getActiveDriverSessions() > 0 && (lastUsedDriver != null)) {
	    try {
		lastUsedDriver.switchTo().defaultContent();
		Boolean discreetLoggingState = ReportManager.isDiscreteLogging();
		ReportManager.setDiscreteLogging(true);
		passAction(lastUsedDriver, "switchToDefaultContent");
		ReportManager.setDiscreteLogging(discreetLoggingState);
	    } catch (Exception e) {
		ReportManager.log(e);
	    }
	}
	// if there is no last used driver or no drivers in the drivers list, do
	// nothing...
    }

    /**
     * Attempts to Click on a certain web element using selenium webdriver, or using
     * javascript
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    public static void click(WebDriver driver, By elementLocator) {
	// Waits for the element to be clickable, and then clicks it.
	if (identifyUniqueElement(driver, elementLocator)) {
	    // adding hover before clicking an element to enable styles to show in the
	    // execution screenshots and to solve issues clicking on certain elements.
	    try {
		performHover(driver, elementLocator);
	    } catch (Exception e) {
		if (!(e.getMessage().contains("Unable to locate element")
			|| e.getMessage().contains("no such element"))) {
		    ReportManager.log(e);
		}
		// else ignore this issue
	    }

	    takeScreenshot(driver, elementLocator, "click", null, true);
	    // takes screenshot before clicking the element out of view

	    try {
		// wait for element to be clickable
		(new WebDriverWait(driver, defaultElementIdentificationTimeout))
			.until(ExpectedConditions.elementToBeClickable(elementLocator));
	    } catch (TimeoutException e) {
		ReportManager.log(e);
	    }

	    try {
		driver.findElement(elementLocator).click();
	    } catch (Exception e) {
		try {
		    ((JavascriptExecutor) driver).executeScript("arguments[arguments.length - 1].click();",
			    driver.findElement(elementLocator));
		} catch (Exception e2) {
		    ReportManager.log(e);
		    ReportManager.log(e2);
		    failAction(driver, "click");
		}
	    }
	    // issue: if performing a navigation after clicking on the login button,
	    // navigation is triggered immediately and hence it fails.
	    // solution: wait for any possible navigation that may be triggered by this
	    // click action to conclude

	    // removed to enhance performance, and replaced with a process to assert after
	    // every navigation
	    passAction(driver, elementLocator, "click");
	} else {
	    failAction(driver, "click");
	}
    }

    /**
     * Waits for the element to be clickable, and then clicks and holds it.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    public static void clickAndHold(WebDriver driver, By elementLocator) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    (new WebDriverWait(driver, 30)).until(ExpectedConditions.elementToBeClickable(elementLocator));
	    // wait for element to be clickable
	    passAction(driver, elementLocator, "clickAndHold");
	    (new Actions(driver)).clickAndHold(driver.findElement(elementLocator)).build().perform();

	    // takes screenshot before holding the element
	} else

	{
	    failAction(driver, "clickAndHold");
	}
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     */
    public static void type(WebDriver driver, By elementLocator, String text) {
	typeWrapper(driver, elementLocator, text, false);
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element. Obfuscates the written text in the ourput
     * report. This action should be used for writing passwords and secure text.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     */
    public static void typeSecure(WebDriver driver, By elementLocator, String text) {
	typeWrapper(driver, elementLocator, text, true);
    }

    /**
     * Types the required file path into an input[type='file'] button, to
     * successfully upload the target file.
     * 
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param absoluteFilePath the full path to the file that needs to be uploaded
     */
    public static void typeFileLocationForUpload(WebDriver driver, By elementLocator, String absoluteFilePath) {
	absoluteFilePath = absoluteFilePath.replace("/", FileSystems.getDefault().getSeparator());
	if (identifyUniqueElement(driver, elementLocator)) {
	    passAction(driver, elementLocator, "typeFileLocationForUpload", absoluteFilePath);
	    try {
		driver.findElement(elementLocator).sendKeys(absoluteFilePath);
	    } catch (ElementNotInteractableException e) {
		((JavascriptExecutor) driver).executeScript(
			"arguments[0].setAttribute('style', 'display:block !important;');",
			driver.findElement(elementLocator));
		try {
		    driver.findElement(elementLocator).sendKeys(absoluteFilePath);
		} catch (WebDriverException e2) {
		    ReportManager.log(e2);
		    // happened for the first time on MacOSX due to incorrect file path separator
		    failAction(driver, "typeFileLocationForUpload", absoluteFilePath);
		}
		try {
		    ((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:none');",
			    driver.findElement(elementLocator));
		} catch (NoSuchElementException | StaleElementReferenceException e2) {
		    // this exception is sometimes thrown on firefox after the upload has been
		    // successful, since we don't have to return the style to what it was, then it's
		    // okay to do nothing here.
		}
	    }
	} else {
	    failAction(driver, "typeFileLocationForUpload", absoluteFilePath);
	}
    }

    /**
     * Appends the required string into the target element, regardless of the
     * current text value.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be appended into the
     *                       target webElement
     */
    public static void typeAppend(WebDriver driver, By elementLocator, String text) {
	if (identifyUniqueElement(driver, elementLocator) && (text != null)) {
	    driver.findElement(elementLocator).sendKeys(text);
	    passAction(driver, elementLocator, "type", text);
	} else {
	    failAction(driver, "type", text);
	}
    }

    /**
     * Selects an element from a dropdown list using its displayed text
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the text of the choice that you need to select from the
     *                       target dropDown menu
     */
    public static void select(WebDriver driver, By elementLocator, String text) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    try {
		(new Select(driver.findElement(elementLocator))).selectByVisibleText(text);
	    } catch (NoSuchElementException e) {
		ReportManager.log(e);
		ReportManager.log("Value not found in the dropdown menu.");
		failAction(driver, "select", text);
	    }
	    passAction(driver, elementLocator, "select", text);
	} else {
	    failAction(driver, "select", text);
	}
    }

    /**
     * Sends a keypress to the target element. Supported keys are: ENTER, RETURN,
     * TAB
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param key            the key that should be pressed
     */
    public static void keyPress(WebDriver driver, By elementLocator, String key) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    switch (key.toLowerCase().trim()) {
	    case "enter":
		driver.findElement(elementLocator).sendKeys(Keys.ENTER);
		break;
	    case "return":
		driver.findElement(elementLocator).sendKeys(Keys.RETURN);
		break;
	    case "tab":
		driver.findElement(elementLocator).sendKeys(Keys.TAB);
		break;
	    default:
		ReportManager.log("Unsupported Key.");
		failAction(driver, "keyPress", key);
		break;
	    }
	} else {
	    failAction(driver, "keyPress", key);
	}
	passAction(driver, elementLocator, "keyPress", key);
    }

    /**
     * Hovers over target element. If you want to hover on a webElement to expose
     * another webElement and click on it, use hoverAndClick instead for a more
     * reliable result.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    public static void hover(WebDriver driver, By elementLocator) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    try {
		performHover(driver, elementLocator);
	    } catch (Exception e) {
		ReportManager.log(e);
		failAction(driver, "hover", "Unhandled Exception: " + e.getMessage());
	    }
	    passAction(driver, elementLocator, "hover");
	} else {
	    failAction(driver, "hover");
	}
    }

    /**
     * Hovers over the hoverElements in sequence then clicks the clickableElement
     * 
     * @param driver                  the current instance of Selenium webdriver
     * @param hoverElementLocators    the list of locators of the webElements under
     *                                test upon which the hover action will be
     *                                performed in sequence (By xpath, id, selector,
     *                                name ...etc)
     * @param clickableElementLocator the locator of the webElement under test upon
     *                                which the click action will be performed (By
     *                                xpath, id, selector, name ...etc)
     */
    public static void hoverAndClick(WebDriver driver, List<By> hoverElementLocators, By clickableElementLocator) {
	Actions chainedHoverAndClickAction = new Actions(driver);
	if (identifyUniqueElement(driver, hoverElementLocators.get(0))) {
	    hoverElementLocators.forEach(hoverElementLocator -> {
		chainedHoverAndClickAction.moveToElement(driver.findElement(hoverElementLocator));
	    });
	    chainedHoverAndClickAction.moveToElement(driver.findElement(clickableElementLocator))
		    .click(driver.findElement(clickableElementLocator)).perform();
	} else {
	    failAction(driver, "hoverAndClick");
	}
    }

    /**
     * Hovers over the hoverElement then clicks the clickableElement
     * 
     * @param driver                  the current instance of Selenium webdriver
     * @param hoverElementLocator     he locator of the webElement under test upon
     *                                which the hover action will be performed (By
     *                                xpath, id, selector, name ...etc)
     * @param clickableElementLocator the locator of the webElement under test upon
     *                                which the click action will be performed (By
     *                                xpath, id, selector, name ...etc)
     */
    public static void hoverAndClick(WebDriver driver, By hoverElementLocator, By clickableElementLocator) {
	hoverAndClick(driver, Arrays.asList(hoverElementLocator), clickableElementLocator);
    }

    private static void performHover(WebDriver driver, By elementLocator) {

//	String javaScript = "var evObj = document.createEvent('MouseEvent');"
//		+ "evObj.initMouseEvent('mousemove', true, false, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);"
//		+ "arguments[arguments.length -1].dispatchEvent(evObj);";
//	((JavascriptExecutor) driver).executeScript(javaScript, driver.findElement(elementLocator));
//
//	javaScript = "var evObj = document.createEvent('MouseEvents');"
//		+ "evObj.initMouseEvent('mouseenter',true, false, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);"
//		+ "arguments[arguments.length -1].dispatchEvent(evObj);";
//	((JavascriptExecutor) driver).executeScript(javaScript, driver.findElement(elementLocator));
//
//	javaScript = "var evObj = document.createEvent('MouseEvents');"
//		+ "evObj.initMouseEvent('mouseover',true, false, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);"
//		+ "arguments[arguments.length -1].dispatchEvent(evObj);";
//	((JavascriptExecutor) driver).executeScript(javaScript, driver.findElement(elementLocator));

	(new Actions(driver)).moveToElement(driver.findElement(elementLocator)).perform();
    }

    /**
     * Drags the source element and drops it onto the destination element using
     * javascript
     * 
     * @param driver                    the current instance of Selenium webdriver
     * @param sourceElementLocator      the locator of the source webElement that
     *                                  should be dragged under test (By xpath, id,
     *                                  selector, name ...etc)
     * @param destinationElementLocator the locator of the target webElement that
     *                                  should receive the dropped source element
     *                                  under test (By xpath, id, selector, name
     *                                  ...etc)
     */
    public static void dragAndDrop(WebDriver driver, By sourceElementLocator, By destinationElementLocator) {
	if (identifyUniqueElement(driver, sourceElementLocator) && getMatchingElementsCount(driver,
		destinationElementLocator, attemptsBeforeThrowingElementNotFoundException) == 1) {

	    // replaced canFindUniqueElementForInternalUse, with countFoundElements for
	    // destinationElement to bypass the check for element visibility

	    // define source and destination elements
	    WebElement sourceElement = driver.findElement(sourceElementLocator);
	    WebElement destinationElement = driver.findElement(destinationElementLocator);

	    // get source element start location
	    String startLocation = sourceElement.getLocation().toString();

	    // attempt to perform drag and drop
	    try {
		driver.manage().timeouts().setScriptTimeout(10, TimeUnit.SECONDS);
		JavascriptExecutor js = (JavascriptExecutor) driver;

		String jQueryLoader = new String(
			Files.readAllBytes(Paths.get("src/main/resources/scripts/jquery_load_helper.js")));
		js.executeAsyncScript(jQueryLoader /* , http://localhost:8080/jquery-1.7.2.js */);

		String dragAndDropHelper = new String(
			Files.readAllBytes(Paths.get("src/main/resources/scripts/drag_and_drop_helper.js")));

		dragAndDropHelper = dragAndDropHelper + "$(arguments[0]).simulateDragDrop({dropTarget:arguments[1]});";

		((JavascriptExecutor) driver).executeScript(dragAndDropHelper, sourceElement, destinationElement);
	    } catch (Exception e) {
		ReportManager.log(e);
		failAction(driver, "dragAndDrop");
	    }

	    // get source element end location
	    String endLocation = driver.findElement(sourceElementLocator).getLocation().toString();

	    if (!endLocation.equals(startLocation)) {
		passAction(driver, sourceElementLocator, "dragAndDrop",
			"Start point: " + startLocation + ", End point: " + endLocation);
	    } else {
		try {
		    (new Actions(driver)).dragAndDrop(driver.findElement(sourceElementLocator),
			    driver.findElement(destinationElementLocator)).build().perform();

		} catch (Exception e) {
		    ReportManager.log(e);
		    failAction(driver, "dragAndDrop");
		}
		// get source element end location
		endLocation = driver.findElement(sourceElementLocator).getLocation().toString();
		if (!endLocation.equals(startLocation)) {
		    passAction(driver, sourceElementLocator, "dragAndDrop",
			    "Start point: " + startLocation + ", End point: " + endLocation);
		} else {
		    failAction(driver, "dragAndDrop", "Start point = End point: " + endLocation);
		}
	    }
	} else {
	    failAction(driver, "dragAndDrop");
	}
    }

    /**
     * Drags the source element and drops it onto the determined offset
     * 
     * @param driver               the current instance of Selenium webdriver
     * @param sourceElementLocator the locator of the source webElement that should
     *                             be dragged under test (By xpath, id, selector,
     *                             name ...etc)
     * @param xOffset              the horizontal offset by which the element should
     *                             be moved
     * @param yOffset              the vertical offset by which the element should
     *                             be moved
     */
    public static void dragAndDropByOffset(WebDriver driver, By sourceElementLocator, int xOffset, int yOffset) {
	if (identifyUniqueElement(driver, sourceElementLocator)) {
	    WebElement sourceElement = driver.findElement(sourceElementLocator);
	    String startLocation = sourceElement.getLocation().toString();

	    // attempt to perform drag and drop
	    try {
		(new Actions(driver)).dragAndDropBy(driver.findElement(sourceElementLocator), xOffset, yOffset).build()
			.perform();
	    } catch (Exception e) {
		ReportManager.log(e);
		failAction(driver, "dragAndDropByOffset");
	    }

	    String endLocation = driver.findElement(sourceElementLocator).getLocation().toString();

	    if (!endLocation.equals(startLocation)) {
		passAction(driver, sourceElementLocator, "dragAndDropByOffset",
			"Start point: " + startLocation + ", End point: " + endLocation);
	    } else {
		failAction(driver, "dragAndDropByOffset", "Start point = End point: " + endLocation);
	    }
	} else {
	    failAction(driver, "dragAndDropByOffset");
	}
    }

    /**
     * Retrieves text from the target element and returns it as a string value.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the text value of the target webElement
     */
    public static String getText(WebDriver driver, By elementLocator) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    String elementText = driver.findElement(elementLocator).getText();
	    if (elementText.trim().equals("")) {
		elementText = driver.findElement(elementLocator).getAttribute("textContent");
	    }
	    if (elementText.trim().equals("")) {
		elementText = driver.findElement(elementLocator).getAttribute("value");
	    }
	    passAction(driver, elementLocator, "getText", elementText);
	    return elementText;
	} else {
	    failAction(driver, "getText");
	    return null;
	}
    }

    /**
     * Retrieves tag name from the target element and returns it as a string value.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the tag name of the webElement under test
     */
    public static String getTagName(WebDriver driver, By elementLocator) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    String elementTagName = driver.findElement(elementLocator).getTagName();
	    passAction(driver, elementLocator, "getTagName", elementTagName);
	    return elementTagName;
	} else {
	    failAction(driver, "getTagName");
	    return null;
	}
    }

    /**
     * Retrieves element size from the target element and returns it as a string
     * value.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the size of the webElement under test
     */
    public static String getSize(WebDriver driver, By elementLocator) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    String elementSize = driver.findElement(elementLocator).getSize().toString();
	    passAction(driver, elementLocator, "getSize", elementSize);
	    return elementSize;
	} else {
	    failAction(driver, "getSize");
	    return null;
	}
    }

    /**
     * Get the value of the given attribute of the element. Will return the current
     * value, even if this has been modified after the page has been loaded.
     * 
     * More exactly, this method will return the value of the property with the
     * given name, if it exists. If it does not, then the value of the attribute
     * with the given name is returned. If neither exists, null is returned.
     * 
     * The "style" attribute is converted as best can be to a text representation
     * with a trailing semi-colon.
     * 
     * The following are deemed to be "boolean" attributes, and will return either
     * "true" or null:
     * 
     * async, autofocus, autoplay, checked, compact, complete, controls, declare,
     * defaultchecked, defaultselected, defer, disabled, draggable, ended,
     * formnovalidate, hidden, indeterminate, iscontenteditable, ismap, itemscope,
     * loop, multiple, muted, nohref, noresize, noshade, novalidate, nowrap, open,
     * paused, pubdate, readonly, required, reversed, scoped, seamless, seeking,
     * selected, truespeed, willvalidate
     * 
     * Finally, the following commonly mis-capitalized attribute/property names are
     * evaluated as expected:
     * 
     * If the given name is "class", the "className" property is returned. If the
     * given name is "readonly", the "readOnly" property is returned. Note: The
     * reason for this behavior is that users frequently confuse attributes and
     * properties. If you need to do something more precise, e.g., refer to an
     * attribute even when a property of the same name exists, then you should
     * evaluate Javascript to obtain the result you desire.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param attributeName  the target attribute of the webElement under test
     * @return the value of the target attribute of the webElement under test
     */
    public static String getAttribute(WebDriver driver, By elementLocator, String attributeName) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    String elementAttribute = driver.findElement(elementLocator).getAttribute(attributeName);
	    passAction(driver, elementLocator, "getAttribute", elementAttribute);
	    return elementAttribute;
	} else {
	    failAction(driver, "getAttribute");
	    return null;
	}
    }

    /**
     * Get the value of a given CSS property. Color values should be returned as
     * rgba strings, so, for example if the "background-color" property is set as
     * "green" in the HTML source, the returned value will be "rgba(0, 255, 0, 1)".
     * Note that shorthand CSS properties (e.g. background, font, border,
     * border-top, margin, margin-top, padding, padding-top, list-style, outline,
     * pause, cue) are not returned, in accordance with the DOM CSS2 specification -
     * you should directly access the longhand properties (e.g. background-color) to
     * access the desired values.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param propertyName   the target CSS property of the webElement under test
     * @return the value of the target CSS property of the webElement under test
     */
    public static String getCSSProperty(WebDriver driver, By elementLocator, String propertyName) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    String elementCssProperty = driver.findElement(elementLocator).getCssValue(propertyName);
	    passAction(driver, elementLocator, "getCSSProperty", elementCssProperty);
	    return elementCssProperty;
	} else {
	    failAction(driver, "getCSSProperty");
	    return null;
	}
    }

    /**
     * Waits dynamically for a specific element's text to change from the initial
     * value to a new unknown value. Waits for a specific number of retries
     * multiplied by the default element identification timeout (in the POM.xml
     * file)
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param initialValue   the initial text value of the target webElement
     * @param numberOfTries  the number of times to try and wait for the element
     *                       text to change (default is 1)
     */
    public static void waitForTextToChange(WebDriver driver, By elementLocator, String initialValue,
	    int numberOfTries) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    try {
		(new WebDriverWait(driver, defaultElementIdentificationTimeout * numberOfTries))
			.until(ExpectedConditions.not(ExpectedConditions.textToBe(elementLocator, initialValue)));
	    } catch (Exception e) {
		ReportManager.log(e);
		failAction(driver, "waitForTextToChange",
			"waited for (" + defaultElementIdentificationTimeout * numberOfTries + ") seconds");
	    }
	    try {
		passAction(driver, elementLocator, "waitForTextToChange",
			"from: \"" + initialValue + "\", to: \"" + getText(driver, elementLocator) + "\"");
	    } catch (Exception e) {
		passAction(driver, elementLocator, "waitForTextToChange",
			"from: \"" + initialValue + "\", to a new value.");
	    }
	} else {
	    failAction(driver, "waitForTextToChange",
		    "Element with locator (" + elementLocator.toString() + ") was not found on this page.");
	}
    }

    /**
     * Waits dynamically for a specific element to achieve the desired
     * stateOfPresence on the current page. Waits for a specific number of retries
     * multiplied by the default element identification timeout (in the POM.xml
     * file)
     * 
     * @param driver          the current instance of Selenium webdriver
     * @param elementLocator  the locator of the webElement under test (By xpath,
     *                        id, selector, name ...etc)
     * @param numberOfTries   the number of times to try and wait for the element to
     *                        achieve the desired stateOfPresence (default is 1)
     * @param stateOfPresence the expected state of presence of the element; false
     *                        is not present, and true is present
     */
    public static void waitForElementToBePresent(WebDriver driver, By elementLocator, int numberOfTries,
	    boolean stateOfPresence) {
	int foundElementsCount = getMatchingElementsCount(driver, elementLocator, numberOfTries);

	if (foundElementsCount <= 1) {
	    try {
		if (stateOfPresence) {
		    passAction(driver, elementLocator, "waitForElementToBePresent",
			    "waited for (" + defaultElementIdentificationTimeout * numberOfTries
				    + ") seconds, for the element's state of presence to be (" + stateOfPresence
				    + "). Element locator (" + elementLocator.toString() + ")");
		} else {
		    passAction(driver, "waitForElementToBePresent",
			    "waited for (" + defaultElementIdentificationTimeout * numberOfTries
				    + ") seconds, for the element's state of presence to be (" + stateOfPresence
				    + "). Element locator (" + elementLocator.toString() + ")");
		}
	    } catch (Exception e) {
		ReportManager.log(e);
		failAction(driver, "waitForElementToBePresent",
			"waited for (" + defaultElementIdentificationTimeout * numberOfTries
				+ ") seconds, for the element's state of presence to be (" + stateOfPresence
				+ "). Element locator (" + elementLocator.toString() + ")");
	    }
	} else {
	    failAction(driver, "waitForElementToBePresent", "Element with locator (" + elementLocator.toString()
		    + "] was found [" + foundElementsCount + "] times on this page.");
	}
    }

    /**
     * Checks to see if an element is displayed
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return boolean value, true if the element is displayed, and false if the
     *         element is not displayed
     */
    public static boolean isElementDisplayed(WebDriver driver, By elementLocator) {
	if (identifyUniqueElement(driver, elementLocator, attemptsBeforeThrowingElementNotFoundException, false)) {
	    (new WebDriverWait(driver, defaultElementIdentificationTimeout))
		    .until(ExpectedConditions.visibilityOfElementLocated(elementLocator));
	    // wait for element to be visible
	    passAction(driver, elementLocator, "isElementDisplayed");
	    return true;
	} else {
	    failAction(driver, "isElementDisplayed");
	    return false;
	}
    }

    /**
     * Checks to see if an element is clickable
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return boolean value, true if the element is clickable, and false if the
     *         element is not clickable
     */
    public static boolean isElementClickable(WebDriver driver, By elementLocator) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    (new WebDriverWait(driver, defaultElementIdentificationTimeout))
		    .until(ExpectedConditions.elementToBeClickable(elementLocator));
	    // wait for element to be clickable
	    passAction(driver, elementLocator, "isElementClickable");
	    return true;
	} else {
	    failAction(driver, "isElementClickable");
	    return false;
	}
    }

    /**
     * Attempts to perform a native clipboard action on the text from a certain web
     * element, like copy/cut/paste
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param action         supports the following actions "copy", "paste", "cut",
     *                       "select all", "unselect"
     */
    public static void clipboardActions(WebDriver driver, By elementLocator, String action) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    if (!System.getProperty("targetOperatingSystem").equals("Mac-64")) {
		performClipboardActionsForMac(driver, elementLocator, action);
	    } else {
		performClipboardActions(driver, elementLocator, action);
	    }

	} else {
	    failAction(driver, "clipboardActions");
	}
    }
}
