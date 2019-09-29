package com.shaft.gui.element;

import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.time.Duration;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.opencv.imgproc.Imgproc;
import org.openqa.selenium.By;
import org.openqa.selenium.ElementNotInteractableException;
import org.openqa.selenium.JavascriptException;
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

import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.image.ImageProcessingActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;

public class ElementActions {
    private static Duration defaultElementIdentificationTimeout = Duration
	    .ofSeconds(Integer.parseInt(System.getProperty("defaultElementIdentificationTimeout").trim()));
    private static int attemptsBeforeThrowingElementNotFoundException = Integer
	    .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static boolean forceCheckForElementVisibility = Boolean
	    .parseBoolean(System.getProperty("forceCheckForElementVisibility").trim());
    // this will only be used for switching back to default content
    static WebDriver lastUsedDriver = null;
    private static By aiGeneratedElementLocator = null;
    private static String aiReferenceFileName = "aiAidedElementIdentificationReferenceDB.properties";

    public static String getAiReferenceFileName() {
	return aiReferenceFileName;
    }

    public static By getAiGeneratedElementLocator() {
	return aiGeneratedElementLocator;
    }

    private ElementActions() {
	throw new IllegalStateException("Utility class");
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Reporting Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static void passAction(WebDriver driver, String actionName) {
	passAction(driver, null, actionName, null, null);
    }

    private static void passAction(WebDriver driver, By elementLocator, String actionName) {
	passAction(driver, elementLocator, actionName, null, null);
    }

    private static void passAction(WebDriver driver, By elementLocator, String actionName, List<Object> screenshot) {
	passAction(driver, elementLocator, actionName, null, screenshot);
    }

    private static void passAction(WebDriver driver, String actionName, String testData) {
	passAction(driver, null, actionName, testData, null);
    }

    private static void passAction(WebDriver driver, By elementLocator, String actionName, String testData) {
	passAction(driver, elementLocator, actionName, testData, null);
    }

    private static void passAction(WebDriver driver, By elementLocator, String actionName, String testData,
	    List<Object> screenshot) {
	String message = "Element Action [" + actionName + "] successfully performed.";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	if (screenshot != null) {
	    // screenshot taken before action (in case of click)
	    ReportManager.log(message, Arrays.asList(screenshot));
	} else {
	    ReportManager.log(message,
		    Arrays.asList(takeScreenshot(driver, elementLocator, actionName, testData, true)));
	}
    }

    private static void failAction(WebDriver driver, String actionName) {
	failAction(driver, actionName, null);
    }

    private static void failAction(WebDriver driver, String actionName, String testData) {
	String message = "[" + actionName + "] failed.";
	if (testData != null) {
	    message = message + " With the following test data [" + testData + "].";
	}
	ReportManager.log(message, Arrays.asList(takeScreenshot(driver, null, actionName, testData, false)));
	Assert.fail(message);
    }

    private static List<Object> takeScreenshot(WebDriver driver, By elementLocator, String actionName, String testData,
	    boolean passFailStatus) {
	if (passFailStatus) {
	    try {
		if ((elementLocator == null) && (testData == null)) {
		    // this only happens when switching to default content so there is no need to
		    // take a screenshot
		} else if (elementLocator != null) {
		    return ScreenshotManager.captureScreenShot(driver, elementLocator, actionName, true);
		} else {
		    return ScreenshotManager.captureScreenShot(driver, actionName, true);
		}
	    } catch (Exception e) {
		ReportManager.log(e);
		ReportManager.log(
			"Failed to take a screenshot of the element as it doesn't exist anymore. Taking a screenshot of the whole page.");
		return ScreenshotManager.captureScreenShot(driver, actionName, true);
	    }
	} else {
	    return ScreenshotManager.captureScreenShot(driver, actionName, false);
	}
	lastUsedDriver = driver;
	return null;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////// [private] Preparation and Support Actions
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private static Boolean attemptToFindElementUsingAI(WebDriver driver, By elementLocator) {
	if (ScreenshotManager.getAiSupportedElementIdentification()) {
	    aiGeneratedElementLocator = null; // reset static container

	    String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);

	    // if this is a new element that's failing for the first time
	    String aiFolderPath = ScreenshotManager.getAiAidedElementIdentificationFolderpath();
	    String referenceImagePath = aiFolderPath + hashedLocatorName + ".png";
	    List<Integer> point = ImageProcessingActions.findImageWithinCurrentPage(referenceImagePath,
		    ScreenshotManager.takeFullPageScreenshot(driver), Imgproc.TM_CCORR_NORMED); // TM_CCOEFF
	    if (point.size() == 2) {
		WebElement targetElement = (WebElement) ((JavascriptExecutor) driver).executeScript(
			"return document.elementFromPoint(arguments[0], arguments[1])", point.get(0), point.get(1));

		if (targetElement == null) {
		    // element may be outside viewport, attempt to scroll and find it using custom
		    // javascript
		    targetElement = (WebElement) ((JavascriptExecutor) driver).executeScript(
			    JSHelpers.SCROLL_TO_ELEMENT_OUTSIDE_VIEWPORT.getValue(), point.get(0), point.get(1));
		}
		Boolean initialLoggingState = ReportManager.isDiscreteLogging();
		ReportManager.setDiscreteLogging(false);
		ReportManager.log(
			"New Element found using AI... Kindly update your element locator [" + elementLocator + "].");
		ReportManager.setDiscreteLogging(initialLoggingState);

		String newXpath = suggestNewXpath(driver, targetElement, elementLocator);
		System.setProperty(hashedLocatorName, newXpath);

		if (FileActions.doesFileExist(aiFolderPath, aiReferenceFileName, 1)) {
		    // append to current file content if the file already exists
		    FileActions.writeToFile(aiFolderPath, aiReferenceFileName,
			    FileActions.readFromFile(aiFolderPath, aiReferenceFileName) + System.lineSeparator()
				    + hashedLocatorName + "=" + newXpath);
		} else {
		    // writing for the first time
		    FileActions.writeToFile(aiFolderPath, aiReferenceFileName, hashedLocatorName + "=" + newXpath);
		}
		setAiGeneratedXpath(newXpath);
		return true;
	    } else {
		return false;
	    }
	} else {
	    return false;
	}
    }

    private static void setAiGeneratedXpath(String newXpath) {
	if (newXpath == null) {
	    aiGeneratedElementLocator = null;
	} else {
	    aiGeneratedElementLocator = By.xpath(newXpath);
	}
	ScreenshotManager.setAiGeneratedElementLocator(aiGeneratedElementLocator);
    }

    private static String suggestNewXpath(WebDriver driver, WebElement targetElement, By deprecatedElementLocator) {
	// attempt to find an optimal xpath for the targerElement
	int maximumXpathNodes = 6;
	String newXpath = "";
	for (int i = 0; i < maximumXpathNodes; i++) {
	    String xpathFindingAlgorithm = JSHelpers.GET_XPATH.getValue();
	    /**
	     * $$GetIndex$$ $$GetId$$ $$GetName$$ $$GetType$$ $$GetClass$$ $$GetText$$
	     * $$MaxCount$$
	     */
	    String maxCount = String.valueOf(i);
	    String getId = String.valueOf(true);
	    String getIndex;
	    String getName;
	    String getType;
	    String getClass;
	    String getText;
	    getIndex = getName = getType = getClass = getText = String.valueOf(false);

	    if (i == 0) {
		maxCount = String.valueOf(1);
	    } else if (i == 1 || i == 2) {
		getName = String.valueOf(true);
		getType = String.valueOf(true);
		getText = String.valueOf(true);
	    } else if (i == 3 || i == 4) {
		getName = String.valueOf(true);
		getType = String.valueOf(true);
		getClass = String.valueOf(true);
		getText = String.valueOf(true);
	    } else {
		getIndex = String.valueOf(true);
		getName = String.valueOf(true);
		getType = String.valueOf(true);
		getText = String.valueOf(true);
		getClass = String.valueOf(true);
	    }

	    xpathFindingAlgorithm = xpathFindingAlgorithm.replaceAll("\\$\\$MaxCount\\$\\$", maxCount)
		    .replaceAll("\\$\\$GetId\\$\\$", getId).replaceAll("\\$\\$GetIndex\\$\\$", getIndex)
		    .replaceAll("\\$\\$GetName\\$\\$", getName).replaceAll("\\$\\$GetType\\$\\$", getType)
		    .replaceAll("\\$\\$GetClass\\$\\$", getClass).replaceAll("\\$\\$GetText\\$\\$", getText);

	    try {
		newXpath = (String) ((JavascriptExecutor) driver).executeScript(xpathFindingAlgorithm, targetElement);
		if (newXpath != null && driver.findElements(By.xpath(newXpath)).size() == 1) {
		    // if unique element was found, break, else keep iterating
		    break;
		}
	    } catch (JavascriptException e) {
		ReportManager.log(e);
		ReportManager.log("Failed to suggest a new XPath for the target element with this deprecated locator ["
			+ deprecatedElementLocator + "]");
	    }
	}
	if (newXpath != null) {
	    Boolean initialLoggingState = ReportManager.isDiscreteLogging();
	    ReportManager.setDiscreteLogging(false);
	    ReportManager.log("New AI-Suggested XPath [" + newXpath.replace("\"", "'") + "]");
	    ReportManager.setDiscreteLogging(initialLoggingState);
	    return newXpath;
	} else {
	    ReportManager.log("Failed to suggest a new XPath for the target element with this deprecated locator ["
		    + deprecatedElementLocator + "]");
	    return null;
	}
    }

    private static boolean identifyUniqueElement(WebDriver driver, By elementLocator) {
	return identifyUniqueElement(driver, elementLocator, attemptsBeforeThrowingElementNotFoundException, true);
    }

    private static By updateLocatorWithAIGenratedOne(By elementLocator) {
	// Override current locator with the aiGeneratedElementLocator
	if (ScreenshotManager.getAiSupportedElementIdentification() && aiGeneratedElementLocator != null
		&& elementLocator != null) {
	    return aiGeneratedElementLocator;
	}
	return elementLocator;
    }

    private static boolean identifyUniqueElement(WebDriver driver, By elementLocator, int numberOfAttempts,
	    boolean checkForVisibility) {
	int matchingElementsCount = getMatchingElementsCount(driver, elementLocator, numberOfAttempts);

	// Override current locator with the aiGeneratedElementLocator
	elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

	if (elementLocator != null) {
	    switch (matchingElementsCount) {
	    case 0:
		failAction(driver, "identifyUniqueElement",
			"zero elements found matching this locator \"" + elementLocator + "\".");
		break;
	    case 1:
		// unique element found
		if (checkForVisibility && !elementLocator.toString().contains("input[@type='file']")
			&& !elementLocator.equals(By.tagName("html"))) {
		    // scroll element into viewPort
		    ((Locatable) driver.findElement(elementLocator)).getCoordinates().inViewPort();

		    // check for visibility
		    checkForElementVisibility(driver, elementLocator);
		}
		return true;
	    default:
		failAction(driver, "identifyUniqueElement",
			"multiple elements found matching this locator \"" + elementLocator + "\".");
		break;
	    }
	} else {
	    failAction(driver, "identifyUniqueElement", "element locator is NULL.");
	}
	return false;
    }

    private static void checkForElementVisibility(WebDriver driver, By elementLocator) {
	if (forceCheckForElementVisibility) {
	    try {
		(new WebDriverWait(driver, defaultElementIdentificationTimeout))
			.until(ExpectedConditions.visibilityOfElementLocated(elementLocator));
	    } catch (TimeoutException e) {
		ReportManager.log(e);
		failAction(driver, "identifyUniqueElement",
			"unique element matching this locator \"" + elementLocator + "\" is not visible.");
	    }
	}
    }

    private static int getMatchingElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts) {
	return getMatchingElementsCount(driver, elementLocator, numberOfAttempts, true);
    }

    private static int getMatchingElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts,
	    boolean waitForLazyLoading) {
	if (waitForLazyLoading) {
	    JSWaiter.waitForLazyLoading();
	}

	if (elementLocator != null && !elementLocator.equals(By.tagName("html"))) {

	    // check to see if this element was already identified using AI, and if it's
	    // still unique, use that locator directly
	    String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
	    String previouslyIdentifiedXpath = System.getProperty(hashedLocatorName);

	    // wait for element presence
	    int matchingElementsCount = 0;
	    if (previouslyIdentifiedXpath != null) {
		setAiGeneratedXpath(previouslyIdentifiedXpath);
		matchingElementsCount = waitForElementPresence(driver, aiGeneratedElementLocator, numberOfAttempts);
	    } else {
		setAiGeneratedXpath(null);
		matchingElementsCount = waitForElementPresence(driver, elementLocator, numberOfAttempts);
	    }

	    if (matchingElementsCount == 1) {
		if (previouslyIdentifiedXpath != null) {
		    Boolean initialLoggingState = ReportManager.isDiscreteLogging();
		    ReportManager.setDiscreteLogging(false);
		    ReportManager
			    .log("Element was previously found using AI... Kindly update your element locator from ["
				    + elementLocator + "] to [" + aiGeneratedElementLocator + "].");
		    ReportManager.setDiscreteLogging(initialLoggingState);
		    elementLocator = aiGeneratedElementLocator;
		}
		ScreenshotManager.storeElementScreenshotForAISupportedElementIdentification(driver, elementLocator);
	    }
	    if (matchingElementsCount == 0 && attemptToFindElementUsingAI(driver, elementLocator)) {
		matchingElementsCount = 1;
	    }
	    return matchingElementsCount;
	} else if (elementLocator != null && elementLocator.equals(By.tagName("html"))
		&& waitForElementPresence(driver, elementLocator, numberOfAttempts) == 1) {
	    return 1;
	}

	else {
	    return 0;
	}
    }

    private static int waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts) {
	// TODO: Implement fluent wait
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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

	    // attempt to type
	    String successfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(driver, elementLocator);
	    String elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
		    successfulTextLocationStrategy);

	    if (elementText != null) {
		if (!elementText.trim().equals("")) {
		    // attempt to clear element then check text size
		    clearBeforeTyping(driver, elementLocator, successfulTextLocationStrategy);
		}
		if ((getMatchingElementsCount(driver, elementLocator,
			attemptsBeforeThrowingElementNotFoundException) == 1) && (!targetText.equals(""))) {
		    // Override current locator with the aiGeneratedElementLocator
		    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

		    performType(driver, elementLocator, targetText);
		}
		if ((getMatchingElementsCount(driver, elementLocator,
			attemptsBeforeThrowingElementNotFoundException) == 1) && (!targetText.equals(""))) {
		    // Override current locator with the aiGeneratedElementLocator
		    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);
		    // to confirm that the text was written successfully
		    confirmTypingWasSuccessful(driver, elementLocator, targetText, isSecureTyping,
			    successfulTextLocationStrategy);
		}
	    } else {
		// happens in case a wrong locator is used
		ReportManager.log("Current Element Text is NULL, this may be due to a wrong element locator ["
			+ elementLocator + "].");
	    }
	}
    }

    private static void confirmTypingWasSuccessful(WebDriver driver, By elementLocator, String targetText,
	    Boolean isSecureTyping, String successfulTextLocationStrategy) {
	if (targetText.equals(
		readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator, successfulTextLocationStrategy))) {
	    if (isSecureTyping) {
		passAction(driver, elementLocator, "type", targetText.replaceAll(".", "*"));
	    } else {
		passAction(driver, elementLocator, "type", targetText);
	    }
	} else {
	    // attempt once to type using javascript then confirm typing was successful
	    // again
	    attemptTypeUsingJavascript(driver, elementLocator, targetText, isSecureTyping,
		    successfulTextLocationStrategy);

	}
    }

    private static void attemptTypeUsingJavascript(WebDriver driver, By elementLocator, String targetText,
	    Boolean isSecureTyping, String successfulTextLocationStrategy) {
	clearBeforeTyping(driver, elementLocator, successfulTextLocationStrategy);
	setValueUsingJavaScript(driver, elementLocator, targetText, true);
	if (targetText.equals(
		readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator, successfulTextLocationStrategy))) {
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
		failAction(driver, "type",
			"Expected to type: \"" + targetText + "\", but ended up with: \"" + actualText + "\"");
	    } catch (Exception e) {
		failAction(driver, "type",
			"Expected to type: \"" + targetText + "\", but ended up with something else");
	    }
	}
    }

    private static void clearBeforeTyping(WebDriver driver, By elementLocator, String successfulTextLocationStrategy) {
	try {
	    // attempt clear using clear
	    driver.findElement(elementLocator).clear();

	    String elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
		    successfulTextLocationStrategy);

	    // attempt clear using sendKeys
	    if (!elementText.trim().equals("")) {
		driver.findElement(elementLocator).sendKeys("");
	    }
	    elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
		    successfulTextLocationStrategy);

	    // attempt clear using javascript
	    if (!elementText.trim().equals("")) {
		setValueUsingJavaScript(driver, elementLocator, "", true);
	    }

	    elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
		    successfulTextLocationStrategy);
	    // attempt clear using letter by letter backspace
	    if (!elementText.trim().equals("")) {
		driver.findElement(elementLocator).sendKeys("");
		for (int i = 0; i < elementText.length(); i++) {
		    driver.findElement(elementLocator).sendKeys(Keys.BACK_SPACE);
		}

	    }
	} catch (org.openqa.selenium.InvalidElementStateException e) {
	    // this was seen in case of attempting to type in an invalid element (an image)
	    ReportManager.log(e);
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
     * Used to force set the value of a certain element using javascript, bypassing
     * regular visibility and element uniqueness checks
     * 
     * @param driver
     * @param elementLocator
     * @param value
     */
    private static void setValueUsingJavaScript(WebDriver driver, By elementLocator, String value,
	    boolean isInternalCall) {
	try {
	    ((JavascriptExecutor) driver).executeScript("arguments[0].value='" + value + "';",
		    driver.findElement(elementLocator));
	    if (isInternalCall) {
		ReportManager.logDiscrete("Set Element Value to [" + value + "] using JavaScript");
	    } else {
		passAction(driver, elementLocator, "setValueUsingJavaScript", value);
	    }
	} catch (Exception e) {
	    ReportManager.log(e);
	    failAction(driver, "setValueUsingJavaScript");
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

    private static void performHover(WebDriver driver, By elementLocator) {
	String createMouseEvent = "var evObj = document.createEvent('MouseEvents');";
	String dispatchMouseEvent = "arguments[arguments.length -1].dispatchEvent(evObj);";

	String mouseEventFirstHalf = "evObj.initMouseEvent(\"";
	String mouseEventSecondHalf = "\", true, false, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);";

	String javaScript = createMouseEvent + mouseEventFirstHalf + "mousemove" + mouseEventSecondHalf
		+ dispatchMouseEvent;
	((JavascriptExecutor) driver).executeScript(javaScript, driver.findElement(elementLocator));

	javaScript = createMouseEvent + mouseEventFirstHalf + "mouseenter" + mouseEventSecondHalf + dispatchMouseEvent;
	((JavascriptExecutor) driver).executeScript(javaScript, driver.findElement(elementLocator));

	javaScript = createMouseEvent + mouseEventFirstHalf + "mouseover" + mouseEventSecondHalf + dispatchMouseEvent;
	((JavascriptExecutor) driver).executeScript(javaScript, driver.findElement(elementLocator));

	(new Actions(driver)).moveToElement(driver.findElement(elementLocator)).perform();
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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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

	    List<Object> screenshot = takeScreenshot(driver, elementLocator, "click", null, true);
	    // takes screenshot before clicking the element out of view
	    String elementText = "";
	    try {
		// wait for element to be clickable
		(new WebDriverWait(driver, defaultElementIdentificationTimeout))
			.until(ExpectedConditions.elementToBeClickable(elementLocator));
	    } catch (TimeoutException e) {
		ReportManager.log(e);
	    }

	    try {
		elementText = driver.findElement(elementLocator).getText();
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
	    if (elementText != null && !elementText.equals("")) {
		passAction(driver, elementLocator, "click", elementText, screenshot);
	    } else {
		passAction(driver, elementLocator, "click", screenshot);
	    }
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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

	    (new WebDriverWait(driver, defaultElementIdentificationTimeout))
		    .until(ExpectedConditions.elementToBeClickable(elementLocator));
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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
     * Sends a keypress to the target element.
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param key            the key that should be pressed
     */
    public static void keyPress(WebDriver driver, By elementLocator, Keys key) {
	if (identifyUniqueElement(driver, elementLocator)) {
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

	    driver.findElement(elementLocator).sendKeys(key);
	} else {
	    failAction(driver, "keyPress", key.name());
	}
	passAction(driver, elementLocator, "keyPress", key.name());
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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    hoverElementLocators.set(0, updateLocatorWithAIGenratedOne(hoverElementLocators.get(0)));

	    hoverElementLocators.forEach(hoverElementLocator -> chainedHoverAndClickAction
		    .moveToElement(driver.findElement(hoverElementLocator)));
	    try {
		chainedHoverAndClickAction.moveToElement(driver.findElement(clickableElementLocator))
			.click(driver.findElement(clickableElementLocator)).perform();
	    } catch (NoSuchElementException e) {
		ReportManager.log(e);
		failAction(driver, "hoverAndClick");
	    }
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
	    // Override current locator with the aiGeneratedElementLocator
	    sourceElementLocator = updateLocatorWithAIGenratedOne(sourceElementLocator);
	    destinationElementLocator = updateLocatorWithAIGenratedOne(destinationElementLocator);

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

		String jQueryLoader = JSHelpers.LOAD_JQUERY.getValue();

		js.executeAsyncScript(jQueryLoader /* , http://localhost:8080/jquery-1.7.2.js */);

		String dragAndDropHelper = JSHelpers.DRAG_AND_DROP.getValue();

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
	    // Override current locator with the aiGeneratedElementLocator
	    sourceElementLocator = updateLocatorWithAIGenratedOne(sourceElementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

	    try {
		(new WebDriverWait(driver,
			Duration.ofSeconds(defaultElementIdentificationTimeout.getSeconds() * numberOfTries))).until(
				ExpectedConditions.not(ExpectedConditions.textToBe(elementLocator, initialValue)));
	    } catch (Exception e) {
		ReportManager.log(e);
		failAction(driver, "waitForTextToChange", "waited for ("
			+ defaultElementIdentificationTimeout.getSeconds() * numberOfTries + ") seconds");
	    }
	    try {
		passAction(driver, elementLocator, "waitForTextToChange",
			"from: \"" + initialValue + "\", to: \"" + getText(driver, elementLocator) + "\"");
	    } catch (Exception e) {
		passAction(driver, elementLocator, "waitForTextToChange",
			"from: \"" + initialValue + "\", to a new value.");
	    }
	} else {
	    if (elementLocator != null) {
		failAction(driver, "waitForTextToChange",
			"Element with locator (" + elementLocator.toString() + ") was not found on this page.");
	    } else {
		// this code is unreachable it's just in place to satisfy SonarLint
		failAction(driver, "waitForTextToChange", "Element has Null locator.");
	    }
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
	// Override current locator with the aiGeneratedElementLocator
	elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

	if (foundElementsCount <= 1 && elementLocator != null) {
	    try {
		if (stateOfPresence) {
		    passAction(driver, elementLocator, "waitForElementToBePresent",
			    "waited for (" + defaultElementIdentificationTimeout.getSeconds() * numberOfTries
				    + ") seconds, for the element's state of presence to be (" + stateOfPresence
				    + "). Element locator (" + elementLocator.toString() + ")");
		} else {
		    passAction(driver, "waitForElementToBePresent",
			    "waited for (" + defaultElementIdentificationTimeout.getSeconds() * numberOfTries
				    + ") seconds, for the element's state of presence to be (" + stateOfPresence
				    + "). Element locator (" + elementLocator.toString() + ")");
		}
	    } catch (Exception e) {
		ReportManager.log(e);
		failAction(driver, "waitForElementToBePresent",
			"waited for (" + defaultElementIdentificationTimeout.getSeconds() * numberOfTries
				+ ") seconds, for the element's state of presence to be (" + stateOfPresence
				+ "). Element locator (" + elementLocator.toString() + ")");
	    }
	} else {
	    if (elementLocator != null) {
		failAction(driver, "waitForElementToBePresent", "Element with locator (" + elementLocator.toString()
			+ "] was found [" + foundElementsCount + "] times on this page.");
	    } else {
		failAction(driver, "waitForElementToBePresent", "Element locator is NULL.");
	    }
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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

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
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

	    if (!System.getProperty("targetOperatingSystem").equals("Mac-64")) {
		performClipboardActionsForMac(driver, elementLocator, action);
	    } else {
		performClipboardActions(driver, elementLocator, action);
	    }

	} else {
	    failAction(driver, "clipboardActions");
	}
    }

    /**
     * Used to set value for an element (hidden or visible) using javascript
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param value          the desired value that should be set for the target
     *                       element
     */
    public static void setValueUsingJavaScript(WebDriver driver, By elementLocator, String value) {
	if (identifyUniqueElement(driver, elementLocator, attemptsBeforeThrowingElementNotFoundException, false)) {
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

	    setValueUsingJavaScript(driver, elementLocator, value, false);
	} else {
	    failAction(driver, "setValueUsingJavaScript");
	}
    }

    /**
     * Used to submit a form using javascript
     * 
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    public static void submitFormUsingJavaScript(WebDriver driver, By elementLocator) {
	if (identifyUniqueElement(driver, elementLocator, attemptsBeforeThrowingElementNotFoundException, false)) {
	    // Override current locator with the aiGeneratedElementLocator
	    elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

	    try {
		((JavascriptExecutor) driver).executeScript("arguments[0].submit();",
			driver.findElement(elementLocator));
		passAction(driver, elementLocator, "submitFormUsingJavaScript");
	    } catch (Exception e) {
		ReportManager.log(e);
		failAction(driver, "submitFormUsingJavaScript");
	    }
	} else {
	    failAction(driver, "submitFormUsingJavaScript");
	}
    }
}
