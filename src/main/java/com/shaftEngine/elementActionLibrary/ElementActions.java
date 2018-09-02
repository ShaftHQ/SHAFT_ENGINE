package com.shaftEngine.elementActionLibrary;

import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;

import org.openqa.selenium.By;
import org.openqa.selenium.ElementNotInteractableException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.interactions.internal.Locatable;
import org.openqa.selenium.remote.UnreachableBrowserException;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.testng.Assert;

import com.shaftEngine.ioActionLibrary.ReportManager;
import com.shaftEngine.ioActionLibrary.ScreenshotManager;

public class ElementActions {
	static int defaultElementIdentificationTimeout = Integer
			.parseInt(System.getProperty("defaultElementIdentificationTimeout").trim());
	static int retriesBeforeThrowingElementNotFoundException = 10;

	private ElementActions() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Returns True if only one element matches the locator specified, and Returns
	 * false (with a descriptive error) if no elements were found, or if more than
	 * one element was found.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @return boolean value, true if the element is found and unique, and false if
	 *         the element is not found or is not unique
	 */
	public static boolean canFindUniqueElement(WebDriver driver, By elementLocator) {
		return internalCanFindUniqueElement(driver, elementLocator, defaultElementIdentificationTimeout, false);
	}

	/**
	 * Returns True if only one element matches the locator specified, and Returns
	 * false (with a descriptive error) if no elements were found, or if more than
	 * one element was found. You can manually set the timeout (in seconds) given
	 * that it will be multiplied by 4.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param timeout
	 *            controls the wait timeout until the target element is present on
	 *            the current page
	 * @return boolean value, true if the element is found and unique, and false if
	 *         the element is not found or is not unique
	 */
	public static boolean canFindUniqueElement(WebDriver driver, By elementLocator, int timeout) {
		return internalCanFindUniqueElement(driver, elementLocator, timeout, false);
	}

	/**
	 * For internal use ONLY inside the ElementActions library, Custom Validations
	 * package, and the Screenshooter class. Returns True if only one element
	 * matches the locator specified, and Returns false (with a descriptive error)
	 * if no elements were found, or if more than one element was found.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @return boolean value, true if the element is found and unique, and false if
	 *         the element is not found or is not unique
	 */
	public static boolean internalCanFindUniqueElement(WebDriver driver, By elementLocator) {
		return internalCanFindUniqueElement(driver, elementLocator, defaultElementIdentificationTimeout, true);
	}

	/**
	 * For internal use ONLY inside the Custom Validations package
	 * (AssertElementExists(false). Returns True if only one element matches the
	 * locator specified, and Returns false (with a descriptive error) if no
	 * elements were found, or if more than one element was found.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param timeout
	 *            controls the wait timeout until the target element is present on
	 *            the current page
	 * @return boolean value, true if the element is found and unique, and false if
	 *         the element is not found or is not unique
	 */
	public static boolean internalCanFindUniqueElement(WebDriver driver, By elementLocator, int timeout) {
		return internalCanFindUniqueElement(driver, elementLocator, timeout, true);
	}

	/**
	 * Returns True if only one element matches the locator specified, and Returns
	 * false (with a descriptive error) if no elements were found, or if more than
	 * one element was found. You can manually set the timeout (in seconds) given
	 * that it will be multiplied by 4. If not an internal call, it is treated as a
	 * regular element action and a pass/fail status is reported.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param timeout
	 *            controls the wait timeout until the target element is present on
	 *            the current page
	 * @return boolean value, true if the element is found and unique, and false if
	 *         the element is not found or is not unique
	 */
	private static boolean internalCanFindUniqueElement(WebDriver driver, By elementLocator, int timeout,
			Boolean isInternalCall) {
		int foundElementsCount = countFoundElements(driver, elementLocator, timeout);
		switch (foundElementsCount) {
		case 0: // no elements found
			ReportManager.log("Element with locator [" + elementLocator.toString() + "] was not found on this page.");
			failAction(driver, "canFindUniqueElement");
			return false;
		case 1: // one element found
			if (!isInternalCall) {
				passAction(driver, elementLocator, "canFindUniqueElement");
			}
			return true;
		default: // multiple elements found
			ReportManager.log("Element with locator [" + elementLocator.toString() + "] was found ["
					+ foundElementsCount + "] times on this page.");
			failAction(driver, "canFindUniqueElement");
			return false;
		}
	}

	private static int countFoundElements(WebDriver driver, By elementLocator) {
		return countFoundElements(driver, elementLocator, defaultElementIdentificationTimeout);
	}

	private static int countFoundElements(WebDriver driver, By elementLocator, int timeout) {
		if (!isWaitForLazyLoadingSuccessful()) {
			return 0;
		}
		// implementing loop to try and break out of the stale element exception issue
		int count = 0;
		int foundElementsCount = 0;
		while (count < retriesBeforeThrowingElementNotFoundException) {
			try {
				// attempt to find elements
				foundElementsCount = attemptToFindElements(driver, elementLocator, timeout);
				return foundElementsCount;
			} catch (StaleElementReferenceException | ElementNotInteractableException | UnreachableBrowserException
					| NoSuchElementException e) {
				if (count + 1 == retriesBeforeThrowingElementNotFoundException) {
					ReportManager.log(e);
					return 0;
				}
				count++;
			} catch (Exception e) {
				if (e.getMessage().contains("cannot focus element")) {
					count++;
				} else {
					ReportManager.log(e);
					ReportManager.log("Unhandled Exception: " + e.getMessage());
					return 0;
				}
			}
		}
		return foundElementsCount;
	}

	private static int attemptToFindElements(WebDriver driver, By elementLocator, int timeout) {
		int foundElements = 0;
		try {
			(new WebDriverWait(driver, timeout)).until(ExpectedConditions.presenceOfElementLocated(elementLocator));
			foundElements = driver.findElements(elementLocator).size();
			if (foundElements == 1) {
				moveToElement(driver, elementLocator);
				if (!elementLocator.toString().contains("input[@type='file']")) {
					(new WebDriverWait(driver, timeout))
							.until(ExpectedConditions.visibilityOfElementLocated(elementLocator));
				}
				return 1;
			}
		} catch (Exception e) {
			ReportManager.log(e);
		}
		return foundElements;
	}

	private static boolean isWaitForLazyLoadingSuccessful() {
		try {
			JSWaiter.waitForLazyLoading();
		} catch (Exception e) {
			if (e.getMessage().contains("jQuery is not defined")
					|| e.getMessage().contains("Error communicating with the remote browser. It may have died.")) {
				// do nothing
			} else {
				ReportManager.log(e);
				ReportManager.log("Unhandled Exception: " + e.getMessage());
				return false;
			}

		}
		return true;
	}

	private static void moveToElement(WebDriver driver, By elementLocator) {
		try {
			// ReportManager.log("Locatable");
			((Locatable) driver.findElement(elementLocator)).getCoordinates().inViewPort(); // main man
			// TO-DO: fix this
			// ReportManager.log("scrollIntoView(true)");
			// ((JavascriptExecutor)
			// driver).executeScript("arguments[0].scrollIntoView(true);",
			// driver.findElement(elementLocator));
		} catch (Exception e) {
			// ReportManager.log("moveToElement");
			// (new
			// Actions(driver)).moveToElement(driver.findElement(elementLocator)).perform();
			// ReportManager.log("scrollIntoView(false)");
			// ((JavascriptExecutor)
			// driver).executeScript("arguments[0].scrollIntoView(false);",
			// driver.findElement(elementLocator));
			// ReportManager.log("sendKeys");
			driver.findElement(elementLocator).sendKeys("");
			ReportManager.log(e);
		}
	}

	protected static void passAction(WebDriver driver, By elementLocator, String actionName) {
		passAction(driver, elementLocator, actionName, null);
	}

	private static void passAction(WebDriver driver, By elementLocator, String actionName, String testData) {
		String message = "[" + actionName + "] successfully performed.";
		if (testData != null) {
			message = message + " With the following test data [" + testData + "].";
		}
		try {
			// moveToElement(driver, elementLocator);
			ScreenshotManager.captureScreenShot(driver, elementLocator, actionName + "_performed", true);
			ReportManager.log(message);
		} catch (Exception e) {
			ReportManager.log(e);
			ReportManager.log(
					"Failed to take a screenshot of the element as it doesn't exist anymore. Taking a screenshot of the whole page.");
			ScreenshotManager.captureScreenShot(driver, actionName + "_performed", true);
			ReportManager.log(message);
		}
	}

	protected static void failAction(WebDriver driver, String actionName) {
		failAction(driver, actionName, null);
	}

	private static void failAction(WebDriver driver, String actionName, String testData) {
		String message = "[" + actionName + "] failed.";
		if (testData != null) {
			message = message + " With the following test data [" + testData + "].";
		}
		ScreenshotManager.captureScreenShot(driver, actionName + "_failed", false);
		ReportManager.log(message);
		Assert.fail(message);
	}

	/**
	 * Clicks Element
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 */
	// Waits for the element to be clickable, and then clicks it.
	public static void click(WebDriver driver, By elementLocator) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
			passAction(driver, elementLocator, "click"); // takes screenshot before clicking the element out of view
			try {
				(new WebDriverWait(driver, defaultElementIdentificationTimeout))
						.until(ExpectedConditions.elementToBeClickable(elementLocator));
				// wait for element to be clickable
				driver.findElement(elementLocator).click();
			} catch (Exception e) {
				if (e.getMessage().contains("Other element would receive the click")
						|| e.getMessage().contains("Expected condition failed: waiting for element to be clickable")
						|| e.getMessage().matches(
								"([\\s\\S]*Element.*is not clickable at point.*because another element.*obscures it\\s[\\s\\S]*)")) {
					((JavascriptExecutor) driver).executeScript("arguments[0].click();",
							driver.findElement(elementLocator));
					// attempting to click using javascript if the regular click fails due to a
					// webdriver error
				} else {
					ReportManager.log(e);
					failAction(driver, "click", "Unhandled Exception: " + e.getMessage());
				}
			}
		} else {
			failAction(driver, "click");
		}
	}

	/**
	 * Waits for the element to be clickable, and then clicks and holds it.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 */
	public static void clickAndHold(WebDriver driver, By elementLocator) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
			(new WebDriverWait(driver, 30)).until(ExpectedConditions.elementToBeClickable(elementLocator));
			// wait for element to be clickable
			passAction(driver, elementLocator, "clickAndHold");
			(new Actions(driver)).clickAndHold(driver.findElement(elementLocator)).build().perform();
			// takes screenshot before holding the element
		} else {
			failAction(driver, "clickAndHold");
		}
	}

	/**
	 * Checks if there is any text in an element, clears it, then types the required
	 * string into the target element.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param text
	 *            the target text that needs to be typed into the target webElement
	 */
	public static void type(WebDriver driver, By elementLocator, String text) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
			// attempt to type
			String successfulTextLocationStrategy;
			String elementText = driver.findElement(elementLocator).getText();
			successfulTextLocationStrategy = "text";
			if (elementText.trim().equals("")) {
				elementText = driver.findElement(elementLocator).getAttribute("textContent");
				successfulTextLocationStrategy = "textContent";
			}
			if (elementText.trim().equals("")) {
				elementText = driver.findElement(elementLocator).getAttribute("value");
				successfulTextLocationStrategy = "value";
			}
			if (!elementText.trim().equals("")) {
				// attempt to clear element then check text size
				clearBeforeTyping(driver, elementLocator, elementText, successfulTextLocationStrategy);
			}
			if (internalCanFindUniqueElement(driver, elementLocator) && (!text.equals(""))) {
				driver.findElement(elementLocator).sendKeys(text);
			}
			if (internalCanFindUniqueElement(driver, elementLocator) && (!text.equals(""))) {
				// to confirm that the text was written successfully
				confirmTypingWasSuccessful(driver, elementLocator, text, successfulTextLocationStrategy);
			}

		}
	}

	private static void clearBeforeTyping(WebDriver driver, By elementLocator, String elementText,
			String successfulTextLocationStrategy) {
		driver.findElement(elementLocator).clear();
		switch (successfulTextLocationStrategy) {
		case "text":
			elementText = driver.findElement(elementLocator).getText();
			break;
		case "textContent":
			elementText = driver.findElement(elementLocator).getAttribute("textContent");
			break;
		case "value":
			elementText = driver.findElement(elementLocator).getAttribute("value");
			break;
		default:
			break;
		}
		if (!elementText.trim().equals("")) {
			int counter = elementText.length(); // delete text manually if clear didn't work
			while (counter > 0) {
				driver.findElement(elementLocator).sendKeys(Keys.BACK_SPACE);
				counter--;
			}
		}
	}

	private static void confirmTypingWasSuccessful(WebDriver driver, By elementLocator, String text,
			String successfulTextLocationStrategy) {
		// to confirm that the text was written successfully
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
		if (actualText.equals(text)) {
			passAction(driver, elementLocator, "type", text);
		} else {
			failAction(driver, "type", "Expected to type: \"" + text + "\", but ended up with: \"" + actualText + "\"");
		}
	}

	/**
	 * Types the required file path into an input[type='file'] button, to
	 * successfully upload the target file.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param absoluteFilePath
	 *            the full path to the file that needs to be uploaded
	 */
	public static void typeFileLocationForUpload(WebDriver driver, By elementLocator, String absoluteFilePath) {
		absoluteFilePath = absoluteFilePath.replace("/", FileSystems.getDefault().getSeparator());
		if (internalCanFindUniqueElement(driver, elementLocator)) {
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
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param text
	 *            the target text that needs to be appended into the target
	 *            webElement
	 */
	public static void typeAppend(WebDriver driver, By elementLocator, String text) {
		if (internalCanFindUniqueElement(driver, elementLocator) && (text != null)) {
			driver.findElement(elementLocator).sendKeys(text);
			passAction(driver, elementLocator, "type", text);
		} else {
			failAction(driver, "type", text);
		}
	}

	/**
	 * Selects an element from a dropdown list using its displayed text
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param text
	 *            the text of the choice that you need to select from the target
	 *            dropDown menu
	 */
	public static void select(WebDriver driver, By elementLocator, String text) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
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
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param key
	 *            the key that should be pressed
	 */
	public static void keyPress(WebDriver driver, By elementLocator, String key) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
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
	 * Hovers over target element.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 */
	public static void hover(WebDriver driver, By elementLocator) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
			(new Actions(driver)).moveToElement(driver.findElement(elementLocator)).build().perform();
			passAction(driver, elementLocator, "hover");
		} else {
			failAction(driver, "hover");
		}
	}

	/**
	 * Drags the source element and drops it onto the destination element using
	 * javascript
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param sourceElementLocator
	 *            the locator of the source webElement that should be dragged under
	 *            test (By xpath, id, selector, name ...etc)
	 * @param destinationElementLocator
	 *            the locator of the target webElement that should receive the
	 *            dropped source element under test (By xpath, id, selector, name
	 *            ...etc)
	 */
	public static void dragAndDrop(WebDriver driver, By sourceElementLocator, By destinationElementLocator) {
		if (internalCanFindUniqueElement(driver, sourceElementLocator)
				&& internalCanFindUniqueElement(driver, destinationElementLocator)) {

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
				failAction(driver, "dragAndDrop", "Start point = End point: " + endLocation);
			}
		} else {
			failAction(driver, "dragAndDrop");
		}
	}

	/**
	 * Retrieves text from the target element and returns it as a string value.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @return the text value of the target webElement
	 */
	public static String getText(WebDriver driver, By elementLocator) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
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
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @return the tag name of the webElement under test
	 */
	public static String getTagName(WebDriver driver, By elementLocator) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
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
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @return the size of the webElement under test
	 */
	public static String getSize(WebDriver driver, By elementLocator) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
			String elementSize = driver.findElement(elementLocator).getSize().toString();
			passAction(driver, elementLocator, "getSize", elementSize);
			return elementSize;
		} else {
			failAction(driver, "getSize");
			return null;
		}
	}

	/**
	 * Retrieves a certain attribute's value from the target element and returns it
	 * as a string value.
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param attributeName
	 *            the target attribute of the webElement under test
	 * @return the value of the target attribute of the webElement under test
	 */
	public static String getAttribute(WebDriver driver, By elementLocator, String attributeName) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
			String elementAttribute = driver.findElement(elementLocator).getAttribute(attributeName);
			passAction(driver, elementLocator, "getAttribute", elementAttribute);
			return elementAttribute;
		} else {
			failAction(driver, "getAttribute");
			return null;
		}
	}

	/**
	 * Waits dynamically for a specific element's text to change from the initial
	 * value to a new unknown value for a specific number of retries multiplied by
	 * the default element identification timeout (in the POM.xml file)
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @param initialValue
	 *            the initial text value of the target webElement
	 * @param numberOfTries
	 *            the number of times to try and wait for the element text to change
	 *            (default is 1)
	 */
	public static void waitForTextToChange(WebDriver driver, By elementLocator, String initialValue,
			int numberOfTries) {

		if (internalCanFindUniqueElement(driver, elementLocator)) {
			try {
				(new WebDriverWait(driver, defaultElementIdentificationTimeout * numberOfTries))
						.until(ExpectedConditions.not(ExpectedConditions.textToBe(elementLocator, initialValue)));
			} catch (Exception e) {
				//
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
					"waited for [" + defaultElementIdentificationTimeout * numberOfTries + "] seconds");
		}
	}

	/**
	 * Checks to see if an element is displayed
	 * 
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @return boolean value, true if the element is displayed, and false if the
	 *         element is not displayed
	 */
	public static boolean isElementDisplayed(WebDriver driver, By elementLocator) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
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
	 * @param driver
	 *            the current instance of Selenium webdriver
	 * @param elementLocator
	 *            the locator of the webElement under test (By xpath, id, selector,
	 *            name ...etc)
	 * @return boolean value, true if the element is clickable, and false if the
	 *         element is not clickable
	 */
	public static boolean isElementClickable(WebDriver driver, By elementLocator) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
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

	public static void clipboardActions(WebDriver driver, By elementLocator, String action) {
		if (internalCanFindUniqueElement(driver, elementLocator)) {
			if (!System.getProperty("targetOperatingSystem").equals("Mac-64")) {
				performClipboardActionsForMac(driver, elementLocator, action);
			} else {
				performClipboardActions(driver, elementLocator, action);
			}

		} else {
			failAction(driver, "clipboardActions");
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
				// (new Actions(driver)).sendKeys(Keys.chord(Keys.CONTROL, "a")).perform();
				break;
			case "unselect":
				// (new Actions(driver)).sendKeys(Keys.ESCAPE).perform();
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

	public static int getElementsCount(WebDriver driver, By elementLocator) {
		return countFoundElements(driver, elementLocator);
	}

	public static int getElementsCount(WebDriver driver, By elementLocator, int customElementIdentificationTimeout) {
		return countFoundElements(driver, elementLocator, customElementIdentificationTimeout);
	}
}
