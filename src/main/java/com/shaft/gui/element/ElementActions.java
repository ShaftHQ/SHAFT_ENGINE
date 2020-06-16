package com.shaft.gui.element;

import com.shaft.cli.FileActions;
import com.shaft.gui.browser.BrowserFactory;
import com.shaft.gui.image.ImageProcessingActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.gui.video.RecordManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.support.JSHelpers;
import io.appium.java_client.AppiumDriver;
import org.opencv.imgproc.Imgproc;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.interactions.Locatable;
import org.openqa.selenium.remote.UnreachableBrowserException;
import org.openqa.selenium.support.ui.*;
import org.sikuli.script.App;
import org.sikuli.script.Pattern;
import org.sikuli.script.Screen;
import org.testng.Assert;

import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class ElementActions {
    private static final int DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER = Integer
            .parseInt(System.getProperty("defaultElementIdentificationTimeout").trim());
    private static final Duration DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT = Duration
            .ofSeconds(DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER);
    private static final int ATTEMPTS_BEFORE_THROWING_ELEMENTNOTFOUNDEXCEPTION = Integer
            .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 1; // seconds
    private static final boolean FORCE_CHECK_FOR_ELEMENT_VISIBILITY = Boolean
            .parseBoolean(System.getProperty("forceCheckForElementVisibility").trim());
    private static final String AI_REFERENCE_FILE_NAME = "aiAidedElementIdentificationReferenceDB.properties";
    private static final String OBFUSCATED_STRING = "•";
    // this will only be used for switching back to default content
    private static WebDriver lastUsedDriver = null;
    private static By aiGeneratedElementLocator = null;

    public ElementActions(WebDriver driver) {
        setLastUsedDriver(driver);
    }

    public static void setLastUsedDriver(WebDriver driver) {
        lastUsedDriver = driver;
    }

    public static String getAiReferenceFileName() {
        return AI_REFERENCE_FILE_NAME;
    }

    public static By getAiGeneratedElementLocator() {
        return aiGeneratedElementLocator;
    }

    private static void passAction(WebDriver driver) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, null, actionName, null, null);
    }

    static void passAction(WebDriver driver, By elementLocator) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, elementLocator, actionName, null, null);
    }

    static void passAction(WebDriver driver, By elementLocator, List<Object> screenshot) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, elementLocator, actionName, null, screenshot);
    }

    private static void passAction(WebDriver driver, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, null, actionName, testData, null);
    }

    static void passAction(WebDriver driver, By elementLocator, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, elementLocator, actionName, testData, null);
    }

    static void passAction(WebDriver driver, By elementLocator, String testData, List<Object> screenshot) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, elementLocator, actionName, testData, screenshot);
    }

    static void passAction(Screen screen, App applicationWindow, Pattern element, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(null, null, actionName, testData, SikuliActions.prepareElementScreenshotAttachment(screen, applicationWindow, element, actionName, true));
    }

    private static void passAction(WebDriver driver, By elementLocator, String actionName, String testData,
                                   List<Object> screenshot) {
        reportActionResult(driver, actionName, testData, elementLocator, screenshot, true);
    }

    static void failAction(WebDriver driver, By elementLocator, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, null, elementLocator, null, rootCauseException);
    }

    static void failAction(WebDriver driver, String testData, By elementLocator, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, elementLocator, null, rootCauseException);
    }

    static void failAction(Screen screen, App applicationWindow, Pattern element, String testData, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(null, actionName, testData, null, SikuliActions.prepareElementScreenshotAttachment(screen, applicationWindow, element, actionName, false), rootCauseException);
    }

    private static void failAction(WebDriver driver, String actionName, String testData, By elementLocator, List<Object> screenshot,
                                   Exception... rootCauseException) {
        String message = reportActionResult(driver, actionName, testData, elementLocator, screenshot, false);

        if (rootCauseException != null && rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
    }

    private static String reportActionResult(WebDriver driver, String actionName, String testData, By elementLocator,
                                             List<Object> screenshot, Boolean passFailStatus) {
        String message = "";
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "Element Action [" + actionName + "] successfully performed.";
        } else {
            message = "Element Action [" + actionName + "] failed.";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && !testData.isEmpty() && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Element Action Test Data - " + actionName,
                    "Actual Value", testData);
            attachments.add(actualValueAttachment);
        } else if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data [" + testData + "].";
        }

        if (screenshot != null && screenshot != new ArrayList<>()) {
            // screenshot taken before action (in case of click)
            attachments.add(screenshot);
        } else if (driver != null && elementLocator != null) {
            List<Object> newScreenshot = takeScreenshot(driver, elementLocator, actionName, testData, passFailStatus);
            if (newScreenshot != null && newScreenshot != new ArrayList<>()) {
                attachments.add(newScreenshot);
            }
        }

        if (!attachments.equals(new ArrayList<>())) {
            ReportManager.log(message, attachments);
        } else {
            ReportManager.log(message);
        }
        return message;
    }

    static List<Object> takeScreenshot(WebDriver driver, By elementLocator, String actionName, String testData,
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
        setLastUsedDriver(driver);
        return new ArrayList<>();
    }

    public static Boolean attemptToFindElementUsingAI(WebDriver driver, By elementLocator) {
        if (Boolean.TRUE.equals(ScreenshotManager.getAiSupportedElementIdentification())) {
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
                    targetElement = (WebElement) ((JavascriptExecutor) driver)
                            .executeScript(JSHelpers.ELEMENT_SCROLL_TO_VIEWPORT.getValue(), point.get(0), point.get(1));
                }
                Boolean initialLoggingState = ReportManager.isDiscreteLogging();
                ReportManager.setDiscreteLogging(false);
                ReportManager.log(
                        "New Element found using AI... Kindly update your element locator [" + elementLocator + "].");
                ReportManager.setDiscreteLogging(initialLoggingState);

                String newXpath = suggestNewXpath(driver, targetElement, elementLocator);
                System.setProperty(hashedLocatorName, newXpath);

                if (FileActions.doesFileExist(aiFolderPath, AI_REFERENCE_FILE_NAME, 1)) {
                    // append to current file content if the file already exists
                    FileActions.writeToFile(aiFolderPath, AI_REFERENCE_FILE_NAME,
                            FileActions.readFromFile(aiFolderPath, AI_REFERENCE_FILE_NAME) + System.lineSeparator()
                                    + hashedLocatorName + "=" + newXpath);
                } else {
                    // writing for the first time
                    FileActions.writeToFile(aiFolderPath, AI_REFERENCE_FILE_NAME, hashedLocatorName + "=" + newXpath);
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
            String xpathFindingAlgorithm = JSHelpers.ELEMENT_GET_XPATH.getValue();
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

    static boolean identifyUniqueElement(WebDriver driver, By elementLocator) {
        return identifyUniqueElement(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENTNOTFOUNDEXCEPTION, true);
    }

    static By updateLocatorWithAIGenratedOne(By elementLocator) {
        // Override current locator with the aiGeneratedElementLocator
        if (Boolean.TRUE.equals(ScreenshotManager.getAiSupportedElementIdentification())
                && aiGeneratedElementLocator != null && elementLocator != null) {
            return aiGeneratedElementLocator;
        }
        return elementLocator;
    }

    private static boolean identifyUniqueElement(WebDriver driver, By elementLocator, int numberOfAttempts,
                                                 boolean checkForVisibility) {
        // Override current locator with the aiGeneratedElementLocator
        elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

        int matchingElementsCount = getMatchingElementsCount(driver, elementLocator, numberOfAttempts);
        if (elementLocator != null) {
            switch (matchingElementsCount) {
                case 0:
                    failAction(driver, "zero elements found matching this locator \"" + elementLocator + "\".", elementLocator);
                    break;
                case 1:
                    // unique element found
                    if (checkForVisibility && !elementLocator.toString().contains("input[@type='file']")
                            && !elementLocator.equals(By.tagName("html"))) {
                        try {
                            // scroll element into viewPort
                            ((Locatable) driver.findElement(elementLocator)).getCoordinates().inViewPort();
                        } catch (org.openqa.selenium.UnsupportedCommandException getElementLocationOnceScrolledIntoView) {
                            // appium -> do nothing
                            // TODO: scroll to elemnt using touchActions
                            ReportManager.logDiscrete(getElementLocationOnceScrolledIntoView);
                        }

                        // check for visibility
                        checkForElementVisibility(driver, elementLocator);
                    }
                    return true;
                default:
                    if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckElementLocatorIsUnique")))) {
                        failAction(driver, "multiple elements found matching this locator \"" + elementLocator + "\".",
                                elementLocator);
                    }
                    return true;
            }
        } else {
            failAction(driver, "element locator is NULL.", elementLocator);
        }
        return false;
    }

    private static void checkForElementVisibility(WebDriver driver, By elementLocator) {
        if (FORCE_CHECK_FOR_ELEMENT_VISIBILITY) {
            try {
                (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                        .until(ExpectedConditions.visibilityOfElementLocated(elementLocator));
            } catch (TimeoutException rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, "unique element matching this locator \"" + elementLocator + "\" is not visible.",
                        null, rootCauseException);
            }
        }
    }

    private static int getMatchingElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts) {
        return getMatchingElementsCount(driver, elementLocator, numberOfAttempts, true);
    }

    private static int getMatchingElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts,
                                                boolean waitForLazyLoading) {
        RecordManager.startVideoRecording();
        if (waitForLazyLoading) {
            JavaScriptWaitManager.waitForLazyLoading();
        }

        int matchingElementsCount = 0;
        if (elementLocator != null && elementLocator.equals(By.tagName("html"))) {
            matchingElementsCount = waitForElementPresence(driver, elementLocator, numberOfAttempts);
        } else if (elementLocator != null) {
            // check to see if this element was already identified using AI, and if it's
            // still unique, use that locator directly
            String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(elementLocator);
            String previouslyIdentifiedXpath = System.getProperty(hashedLocatorName);
            setAiGeneratedXpath(previouslyIdentifiedXpath);

            // wait for element presence
            if (previouslyIdentifiedXpath != null) {
                elementLocator = aiGeneratedElementLocator;
            }
            matchingElementsCount = waitForElementPresence(driver, elementLocator, numberOfAttempts);

            if (matchingElementsCount == 0
                    && Boolean.TRUE.equals(attemptToFindElementUsingAI(driver, elementLocator))) {
                matchingElementsCount = 1;
            } else if (matchingElementsCount == 1) {
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
        }
        return matchingElementsCount;
    }

    private static int waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts) {
        try {
            new FluentWait<WebDriver>(driver)
                    .withTimeout(Duration.ofSeconds(
                            (long) DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER * numberOfAttempts))
                    .pollingEvery(Duration.ofSeconds(ELEMENT_IDENTIFICATION_POLLING_DELAY))
                    .ignoring(NoSuchElementException.class).until(nestedDriver -> driver.findElement(elementLocator));
            return driver.findElements(elementLocator).size();
        } catch (TimeoutException e) {
            // In case the element was not found and the timeout expired
            ReportManager.logDiscrete(e);
            return 0;
        }
    }

    private static TextDetectionStrategy determineSuccessfulTextLocationStrategy(WebDriver driver, By elementLocator) {
        String text = driver.findElement(elementLocator).getText().trim();
        String content = "";
        String value = "";
        if (!BrowserFactory.isMobileNativeExecution()) {
            content = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.CONTENT.getValue()).trim();
            value = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.VALUE.getValue());
        }

        if (value != null) {
            value = value.trim();
        }

        TextDetectionStrategy successfulTextLocationStrategy;
        if (!text.equals("")) {
            successfulTextLocationStrategy = TextDetectionStrategy.TEXT;
        } else if (!content.equals("")) {
            successfulTextLocationStrategy = TextDetectionStrategy.CONTENT;
        } else if (value != null && !value.equals("")) {
            successfulTextLocationStrategy = TextDetectionStrategy.VALUE;
        } else {
            successfulTextLocationStrategy = TextDetectionStrategy.UNDEFINED;
        }
        return successfulTextLocationStrategy;
    }

    private static String readTextBasedOnSuccessfulLocationStrategy(WebDriver driver, By elementLocator,
                                                                    TextDetectionStrategy successfulTextLocationStrategy) {
        String actualText = "";
        switch (successfulTextLocationStrategy) {
            case TEXT:
                actualText = driver.findElement(elementLocator).getText();
                break;
            case CONTENT:
                actualText = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.CONTENT.getValue());
                break;
            case VALUE:
                actualText = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.VALUE.getValue());
                break;
            default:
                break;
        }
        return actualText;
    }

    private static String typeWrapper(WebDriver driver, By elementLocator, String targetText) {
        if (identifyUniqueElement(driver, elementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

            TextDetectionStrategy successfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(driver,
                    elementLocator);
            if (!successfulTextLocationStrategy.equals(TextDetectionStrategy.UNDEFINED)) {
                clearBeforeTyping(driver, elementLocator, successfulTextLocationStrategy);
            }
            if (!targetText.equals("")) {
                performType(driver, elementLocator, targetText);
            }
            if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckTextWasTypedCorrectly")))) {
                return confirmTypingWasSuccessful(driver, elementLocator, targetText, successfulTextLocationStrategy);
            } else {
                return targetText;
            }
        } else {
            ReportManager.log("Failed to identify Target element with locator [" + elementLocator + "].");
            return null;
        }
    }

    private static String confirmTypingWasSuccessful(WebDriver driver, By elementLocator, String expectedText,
                                                     TextDetectionStrategy successfulTextLocationStrategy) {
        if (successfulTextLocationStrategy.equals(TextDetectionStrategy.UNDEFINED)) {
            successfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(driver,
                    elementLocator);
        }
        String actualText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
                successfulTextLocationStrategy);

        if (expectedText.equals(actualText) || OBFUSCATED_STRING.repeat(expectedText.length()).equals(actualText)) {
            return expectedText;
        } else {
            // attempt once to type using javascript then confirm typing was successful
            // again
            internalSetValueUsingJavaScript(driver, elementLocator, expectedText);
            actualText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator, TextDetectionStrategy.VALUE);
            return actualText;
        }
    }

    private static void clearBeforeTyping(WebDriver driver, By elementLocator,
                                          TextDetectionStrategy successfulTextLocationStrategy) {
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
                internalSetValueUsingJavaScript(driver, elementLocator, "");
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
        } catch (InvalidElementStateException e) {
            // this was seen in case of attempting to type in an invalid element (an image)
            ReportManager.log(e);
        }
    }

    private static void performType(WebDriver driver, By elementLocator, String text) {
        // implementing loop to try and break out of the stale element exception issue
        for (int i = 0; i < ATTEMPTS_BEFORE_THROWING_ELEMENTNOTFOUNDEXCEPTION; i++) {
            try {
                // attempt to perform action
                driver.findElement(elementLocator).sendKeys(text);
                break;
            } catch (StaleElementReferenceException | ElementNotInteractableException | UnreachableBrowserException
                    | NoSuchElementException | TimeoutException e) {
                if (i + 1 == ATTEMPTS_BEFORE_THROWING_ELEMENTNOTFOUNDEXCEPTION) {
                    ReportManager.log(e);
                }
            } catch (Exception e) {
                if (e.getMessage().contains("cannot focus element")
                        && (i + 1 == ATTEMPTS_BEFORE_THROWING_ELEMENTNOTFOUNDEXCEPTION)) {
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
    private static boolean internalSetValueUsingJavaScript(WebDriver driver, By elementLocator, String value) {
        try {
            ((JavascriptExecutor) driver).executeScript("arguments[0].value='" + value + "';",
                    driver.findElement(elementLocator));
            return true;
        } catch (Exception e) {
            ReportManager.log(e);
            return false;
        }
    }

    private static boolean performClipboardActions(WebDriver driver, By elementLocator, String action) {

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
                    return false;
            }
            return true;
        } catch (HeadlessException e) {
            ReportManager.log(e);
            return false;
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

    private static Boolean performClipboardActionsForMac(WebDriver driver, String action) {
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
                return false;
        }
        return true;
    }

    private static void performHover(WebDriver driver, By elementLocator) {
        try {
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
        } catch (UnsupportedCommandException methodIsNotImplemented) {
            // appium -> do nothing
            ReportManager.log(methodIsNotImplemented);

        }
    }

    /**
     * Returns the number of elements that match a certain elementLocator
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return integer value that represents the number of elements that match the
     * desired elementLocator
     */
    public static int getElementsCount(WebDriver driver, By elementLocator) {
        return getMatchingElementsCount(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENTNOTFOUNDEXCEPTION);
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
     * desired elementLocator
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
     * desired elementLocator
     */
    public static int getElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts,
                                       boolean waitForLazyLoading) {
        return getMatchingElementsCount(driver, elementLocator, numberOfAttempts, waitForLazyLoading);
    }

    /**
     * Switches focus to another window
     *
     * @param driver       the current instance of Selenium webdriver
     * @param nameOrHandle The name of the window or the handle as returned by
     *                     ElementActions.getWindowHandle(WebDriver driver)
     */
    public static void switchToWindow(WebDriver driver, String nameOrHandle) {
        if (driver.getWindowHandles().contains(nameOrHandle)) {
            driver.switchTo().window(nameOrHandle);
            ElementActions.passAction(driver, nameOrHandle);
        } else {
            ElementActions.failAction(driver, nameOrHandle, null);
        }
    }

    /**
     * Returns the unique handle for currently active window. This can be used to
     * switch to this window at a later time.
     *
     * @param driver the current instance of Selenium webdriver
     * @return window handle
     */
    public static String getWindowHandle(WebDriver driver) {
        String nameOrHandle = driver.getWindowHandle();
        ElementActions.passAction(driver, nameOrHandle);
        return nameOrHandle;
    }

    /**
     * Returns a list of unique handles for all the currently open windows. This can
     * be used to switch to any of these windows at a later time.
     *
     * @param driver the current instance of Selenium webdriver
     * @return list of window handles
     */
    public static List<String> getWindowHandles(WebDriver driver) {
        List<String> windowHandles = new ArrayList<>();
        windowHandles.addAll(driver.getWindowHandles());
        ElementActions.passAction(driver, String.valueOf(windowHandles));
        return windowHandles;

    }

    /**
     * Returns the handle for currently active context. This can be used to switch
     * to this context at a later time.
     *
     * @param driver the current instance of Appium Driver
     * @return The current context handle
     */
    public static String getContext(WebDriver driver) {
        String context = "";
        if (driver instanceof AppiumDriver<?>) {
            context = ((AppiumDriver<?>) driver).getContext();
            ElementActions.passAction(driver);
        } else {
            ElementActions.failAction(driver, null);
        }
        return context;
    }

    /**
     * Returns a list of unique handles for all the currently open contexts. This
     * can be used to switch to any of these contexts at a later time.
     *
     * @param driver the current instance of Appium Driver
     * @return list of context handles
     */
    public static List<String> getContextHandles(WebDriver driver) {
        List<String> windowHandles = new ArrayList<>();
        if (driver instanceof AppiumDriver<?>) {
            windowHandles.addAll(((AppiumDriver<?>) driver).getContextHandles());
            ElementActions.passAction(driver);
        } else {
            ElementActions.failAction(driver, null);
        }
        return windowHandles;
    }

    /**
     * Switches focus to another context
     *
     * @param driver  the current instance of Appium Driver
     * @param context The name of the context or the handle as returned by
     *                ElementActions.getContext(WebDriver driver)
     */
    public static void setContext(WebDriver driver, String context) {
        if (driver instanceof AppiumDriver<?>) {
            ((AppiumDriver<?>) driver).context(context);
            ElementActions.passAction(driver, context);
        } else {
            ElementActions.failAction(driver, context, null);
        }
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
        if (identifyUniqueElement(driver, elementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

            driver.switchTo().frame(driver.findElement(elementLocator));
            // note to self: remove elementLocator in case of bug in screenshot manager
            Boolean discreetLoggingState = ReportManager.isDiscreteLogging();
            ReportManager.setDiscreteLogging(true);
            passAction(driver);
            ReportManager.setDiscreteLogging(discreetLoggingState);
        } else {
            failAction(driver, elementLocator);
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
            passAction(driver);
            ReportManager.setDiscreteLogging(discreetLoggingState);
        } catch (Exception rootCauseException) {
            failAction(driver, null, rootCauseException);
        }
    }

    public static void switchToDefaultContent() {
        if (BrowserFactory.getActiveDriverSessions() > 0 && (lastUsedDriver != null)) {
            try {
                lastUsedDriver.switchTo().defaultContent();
                Boolean discreetLoggingState = ReportManager.isDiscreteLogging();
                ReportManager.setDiscreteLogging(true);
                passAction(lastUsedDriver);
                ReportManager.setDiscreteLogging(discreetLoggingState);
            } catch (Exception e) {
                ReportManager.log(e);
            }
        }
        // if there is no last used driver or no drivers in the drivers list, do
        // nothing...
    }

    /**
     * Double-clicks on an element using Selenium WebDriver's Actions Library
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    public static void doubleClick(WebDriver driver, By elementLocator) {
        if (ElementActions.identifyUniqueElement(driver, elementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = ElementActions.updateLocatorWithAIGenratedOne(elementLocator);
            String elementText = "";
            try {
                // attempting to read element text
                elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
                        determineSuccessfulTextLocationStrategy(driver, elementLocator));
            } catch (Exception e) {
                // do nothing
                ReportManager.logDiscrete(e);
            }
            List<Object> screenshot = ElementActions.takeScreenshot(driver, elementLocator, "doubleClick", null, true);
            // takes screenshot before clicking the element out of view

            try {
                (new Actions(driver)).moveToElement(driver.findElement(elementLocator)).doubleClick().perform();
            } catch (Exception e) {
                ElementActions.failAction(driver, elementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                ElementActions.passAction(driver, elementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                ElementActions.passAction(driver, elementLocator, screenshot);
            }
        } else {
            ElementActions.failAction(driver, elementLocator);
        }
    }

    /**
     * Clicks on a certain element using Selenium WebDriver, or JavaScript
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

            String elementText = "";
            try {
                // attempting to read element text
                elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
                        determineSuccessfulTextLocationStrategy(driver, elementLocator));
                // adding hover before clicking an element to enable styles to show in the
                // execution screenshots and to solve issues clicking on certain elements.
                performHover(driver, elementLocator);
            } catch (Exception e) {
                ReportManager.logDiscrete(e);
            }

            List<Object> screenshot = takeScreenshot(driver, elementLocator, "click", null, true);
            // takes screenshot before clicking the element out of view
            try {
                // wait for element to be clickable
                (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                        .until(ExpectedConditions.elementToBeClickable(elementLocator));
            } catch (TimeoutException e) {
                ReportManager.logDiscrete(e);
            }

            try {
                driver.findElement(elementLocator).click();
            } catch (Exception exception1) {
                try {
                    ((JavascriptExecutor) driver).executeScript("arguments[arguments.length - 1].click();",
                            driver.findElement(elementLocator));
                } catch (Exception rootCauseException) {
                    rootCauseException.initCause(exception1);
                    ReportManager.log(exception1);
                    ReportManager.log(rootCauseException);
                    failAction(driver, elementLocator, rootCauseException);
                }
            }
            // issue: if performing a navigation after clicking on the login button,
            // navigation is triggered immediately and hence it fails.
            // solution: wait for any possible navigation that may be triggered by this
            // click action to conclude

            // removed to enhance performance, and replaced with a process to assert after
            // every navigation
            if (elementText != null && !elementText.equals("")) {
                passAction(driver, elementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                passAction(driver, elementLocator, screenshot);
            }
        } else {
            failAction(driver, elementLocator);
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

            (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                    .until(ExpectedConditions.elementToBeClickable(elementLocator));
            // wait for element to be clickable
            passAction(driver, elementLocator);
            (new Actions(driver)).clickAndHold(driver.findElement(elementLocator)).build().perform();

            // takes screenshot before holding the element
        } else {
            failAction(driver, elementLocator);
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
        String actualResult = typeWrapper(driver, elementLocator, text);

        if (actualResult != null && actualResult.equals(text)) {
            passAction(driver, elementLocator, text);
        } else if (actualResult == null) {
            failAction(driver, elementLocator);
        } else {
            failAction(driver, "Expected to type: \"" + text + "\", but ended up with: \"" + actualResult + "\"",
                    elementLocator);
        }
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element. Obfuscates the written text in the output
     * report. This action should be used for writing passwords and secure text.
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     */
    public static void typeSecure(WebDriver driver, By elementLocator, String text) {
        String actualResult = typeWrapper(driver, elementLocator, text);

        if (actualResult != null && actualResult.equals(text)) {
            passAction(driver, elementLocator, OBFUSCATED_STRING.repeat(text.length()));
        } else if (actualResult == null) {
            failAction(driver, elementLocator);
        } else {
            failAction(driver, "Expected to type: \"" + text + "\", but ended up with: \""
                    + actualResult + "\"", elementLocator);
        }

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
        if (identifyUniqueElement(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENTNOTFOUNDEXCEPTION, false)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

            List<Object> screenshot = takeScreenshot(driver, elementLocator, "typeFileLocationForUpload", null, true);
            // takes screenshot before clicking the element out of view

            try {
                driver.findElement(elementLocator).sendKeys(absoluteFilePath);
            } catch (InvalidArgumentException e) {
                //this happens when the file path doesn't exist
                failAction(driver, absoluteFilePath, elementLocator, e);

            } catch (ElementNotInteractableException exception1) {
                ((JavascriptExecutor) driver).executeScript(
                        "arguments[0].setAttribute('style', 'display:block !important;');",
                        driver.findElement(elementLocator));
                try {
                    driver.findElement(elementLocator).sendKeys(absoluteFilePath);
                } catch (WebDriverException rootCauseException) {
                    rootCauseException.initCause(exception1);
                    ReportManager.log(rootCauseException);
                    // happened for the first time on MacOSX due to incorrect file path separator
                    failAction(driver, absoluteFilePath, elementLocator, rootCauseException);
                }
                try {
                    ((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:none');",
                            driver.findElement(elementLocator));
                } catch (NoSuchElementException | StaleElementReferenceException e) {
                    // this exception is sometimes thrown on firefox after the upload has been
                    // successful, since we don't have to return the style to what it was, then it's
                    // okay to do nothing here.
                    ReportManager.logDiscrete(e);
                }
            }
            passAction(driver, elementLocator, absoluteFilePath, screenshot);
        } else {
            failAction(driver, absoluteFilePath, elementLocator);
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
            passAction(driver, elementLocator, text);
        } else {
            failAction(driver, text, elementLocator);
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

            //add forced check that the select element actually has options
            try {
                (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                        .until(ExpectedConditions.not(ExpectedConditions.textToBe(elementLocator, "")));
            } catch (Exception rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, "waited for (" + DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT.getSeconds() + ") seconds", elementLocator, rootCauseException);
            }
            Boolean isOptionFound = false;
            var availableOptionsList = (new Select(driver.findElement(elementLocator))).getOptions();
            for (int i = 0; i < availableOptionsList.size(); i++) {
                String visibleText = availableOptionsList.get(i).getText();
                String value = availableOptionsList.get(i).getAttribute("value");
                if (visibleText.trim().equals(text) || value.trim().equals(text)) {
                    (new Select(driver.findElement(elementLocator))).selectByIndex(i);
                    passAction(driver, elementLocator, text);
                    isOptionFound = true;
                    break;
                }
            }
            if (Boolean.FALSE.equals(isOptionFound)) {
                failAction(driver, text, elementLocator);
            }
        } else {
            failAction(driver, text, elementLocator);
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
                    failAction(driver, key, elementLocator);
                    break;
            }
        } else {
            failAction(driver, key, elementLocator);
        }
        passAction(driver, elementLocator, key);
    }

    /**
     * Sends a keypress to the target element.
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param keys           the key that should be pressed
     */
    public static void keyPress(WebDriver driver, By elementLocator, Keys keys) {
        if (identifyUniqueElement(driver, elementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

            driver.findElement(elementLocator).sendKeys(keys);
        } else {
            failAction(driver, keys.name(), elementLocator);
        }
        passAction(driver, elementLocator, keys.name());
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
            } catch (Exception rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, elementLocator, rootCauseException);
            }
            passAction(driver, elementLocator);
        } else {
            failAction(driver, elementLocator);
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
            } catch (NoSuchElementException rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, hoverElementLocators.get(0), rootCauseException);
            }
        } else {
            failAction(driver, hoverElementLocators.get(0));
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
        if (identifyUniqueElement(driver, sourceElementLocator)
                && identifyUniqueElement(driver, destinationElementLocator)) {
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

                String dragAndDropHelper = JSHelpers.ELEMENT_DRAG_AND_DROP.getValue();

                dragAndDropHelper = dragAndDropHelper + "$(arguments[0]).simulateDragDrop({dropTarget:arguments[1]});";

                ((JavascriptExecutor) driver).executeScript(dragAndDropHelper, sourceElement, destinationElement);
            } catch (Exception rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, sourceElementLocator, rootCauseException);
            }

            // get source element end location
            String endLocation = driver.findElement(sourceElementLocator).getLocation().toString();

            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;

            if (!endLocation.equals(startLocation)) {
                passAction(driver, sourceElementLocator, reportMessage);
            } else {
                try {
                    (new Actions(driver)).dragAndDrop(driver.findElement(sourceElementLocator),
                            driver.findElement(destinationElementLocator)).build().perform();

                } catch (Exception rootCauseException) {
                    ReportManager.log(rootCauseException);
                    failAction(driver, sourceElementLocator, rootCauseException);
                }
                // get source element end location
                endLocation = driver.findElement(sourceElementLocator).getLocation().toString();
                if (!endLocation.equals(startLocation)) {
                    passAction(driver, sourceElementLocator, reportMessage);
                } else {
                    failAction(driver, reportMessage, sourceElementLocator);
                }
            }
        } else {
            failAction(driver, sourceElementLocator);
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
            } catch (Exception rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, sourceElementLocator, rootCauseException);
            }

            String endLocation = driver.findElement(sourceElementLocator).getLocation().toString();

            if (!endLocation.equals(startLocation)) {
                passAction(driver, sourceElementLocator,
                        "Start point: " + startLocation + ", End point: " + endLocation);
            } else {
                failAction(driver, "Start point = End point: " + endLocation, sourceElementLocator);
            }
        } else {
            failAction(driver, sourceElementLocator);
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

            if (elementText.trim().equals("") && !BrowserFactory.isMobileNativeExecution()) {
                elementText = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.CONTENT.getValue());
            }
            if (elementText.trim().equals("") && !BrowserFactory.isMobileNativeExecution()) {
                elementText = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.VALUE.getValue());
            }
            passAction(driver, elementLocator, elementText);
            return elementText;
        } else {
            failAction(driver, elementLocator);
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
            passAction(driver, elementLocator, elementTagName);
            return elementTagName;
        } else {
            failAction(driver, elementLocator);
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
            passAction(driver, elementLocator, elementSize);
            return elementSize;
        } else {
            failAction(driver, elementLocator);
            return null;
        }
    }

    /**
     * Get the value of the given attribute of the element. Will return the current
     * value, even if this has been modified after the page has been loaded.
     * <p>
     * More exactly, this method will return the value of the property with the
     * given name, if it exists. If it does not, then the value of the attribute
     * with the given name is returned. If neither exists, null is returned.
     * <p>
     * The "style" attribute is converted as best can be to a text representation
     * with a trailing semi-colon.
     * <p>
     * The following are deemed to be "boolean" attributes, and will return either
     * "true" or null:
     * <p>
     * async, autofocus, autoplay, checked, compact, complete, controls, declare,
     * defaultchecked, defaultselected, defer, disabled, draggable, ended,
     * formnovalidate, hidden, indeterminate, iscontenteditable, ismap, itemscope,
     * loop, multiple, muted, nohref, noresize, noshade, novalidate, nowrap, open,
     * paused, pubdate, readonly, required, reversed, scoped, seamless, seeking,
     * selected, truespeed, willvalidate
     * <p>
     * Finally, the following commonly mis-capitalized attribute/property names are
     * evaluated as expected:
     * <p>
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
        ReportManager.logDiscrete("Attempting to getAttribute [" + attributeName + "] from elementLocator [" + elementLocator + "].");
        if (identifyUniqueElement(driver, elementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);
            try {
                String elementAttribute = driver.findElement(elementLocator).getAttribute(attributeName);
                passAction(driver, elementLocator, elementAttribute);
                return elementAttribute;
            } catch (org.openqa.selenium.UnsupportedCommandException rootCauseException) {
                failAction(driver, elementLocator, rootCauseException);
                return null;
            }
        } else {
            failAction(driver, elementLocator);
            return null;
        }
    }

    /**
     * Get the value of a given CSS property. Color values should be returned as
     * RGBA strings, so, for example if the "background-color" property is set as
     * "green" in the HTML source, the returned value will be "RGBA(0, 255, 0, 1)".
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
            passAction(driver, elementLocator, elementCssProperty);
            return elementCssProperty;
        } else {
            failAction(driver, elementLocator);
            return null;
        }
    }

    /**
     * Retrieves the selected text from the target drop-down list element and returns it as a string value.
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the selected text of the target webElement
     */
    public static String getSelectedText(WebDriver driver, By elementLocator) {
        if (identifyUniqueElement(driver, elementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);
            StringBuilder elementSelectedText = new StringBuilder();
            try {
                new Select(driver.findElement(elementLocator)).getAllSelectedOptions().forEach(selectedOption -> elementSelectedText.append(selectedOption.getText()));
                passAction(driver, elementLocator, elementSelectedText.toString().trim());
                return elementSelectedText.toString().trim();
            } catch (UnexpectedTagNameException rootCauseException) {
                failAction(driver, elementLocator, rootCauseException);
                return null;
            }
        } else {
            failAction(driver, elementLocator);
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
                (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER * (long) numberOfTries))
                        .until(ExpectedConditions.not(ExpectedConditions.textToBe(elementLocator, initialValue)));
            } catch (Exception rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, "waited for (" + DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT.getSeconds() * numberOfTries
                        + ") seconds", elementLocator, rootCauseException);
            }
            try {
                passAction(driver, elementLocator,
                        "from: \"" + initialValue + "\", to: \"" + getText(driver, elementLocator) + "\"");
            } catch (Exception e) {
                passAction(driver, elementLocator, "from: \"" + initialValue + "\", to a new value.");
            }
        } else {
            if (elementLocator != null) {
                failAction(driver,
                        "Element with locator (" + elementLocator.toString() + ") was not found on this page.",
                        elementLocator);
            } else {
                // this code is unreachable it's just in place to satisfy SonarLint
                failAction(driver, "Element has Null locator.", elementLocator);
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
        ReportManager.logDiscrete("Waiting for element to be present; elementLocator [" + elementLocator + "], numberOfTries[" + numberOfTries + "], stateOfPresence[" + stateOfPresence + "]...");
        // Override current locator with the aiGeneratedElementLocator
        elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

        int foundElementsCount = 0;
        Boolean isElementFound = false;
        int i = 1;
        do {
            foundElementsCount = getMatchingElementsCount(driver, elementLocator, 1);
            isElementFound = foundElementsCount >= 1;
            i++;
        } while (i < numberOfTries && Boolean.compare(stateOfPresence, isElementFound) != 0);

        String reportMessage = "waited up to (" + DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT.getSeconds() * numberOfTries
                + ") seconds, for the element's state of presence to be (" + stateOfPresence
                + "). Element locator (" + elementLocator.toString() + ")";
        if (Boolean.compare(stateOfPresence, isElementFound) == 0) {
            passAction(driver, elementLocator, reportMessage);
        } else {
            failAction(driver, reportMessage, elementLocator);
        }
    }

    /**
     * Checks to see if an element is displayed
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return boolean value, true if the element is displayed, and false if the
     * element is not displayed
     */
    public static boolean isElementDisplayed(WebDriver driver, By elementLocator) {
        if (identifyUniqueElement(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENTNOTFOUNDEXCEPTION, false)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

            Boolean isDisplayed = driver.findElement(elementLocator).isDisplayed();
            passAction(driver, elementLocator);
            return isDisplayed;
        } else {
            failAction(driver, elementLocator);
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
     * element is not clickable
     */
    public static boolean isElementClickable(WebDriver driver, By elementLocator) {
        if (identifyUniqueElement(driver, elementLocator) 
        		&& driver.findElement(elementLocator).isEnabled()) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

            (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                    .until(ExpectedConditions.elementToBeClickable(elementLocator));
            // wait for element to be clickable
            passAction(driver, elementLocator);
            return true;          
        } else if (identifyUniqueElement(driver, elementLocator) 
        		&& !(driver.findElement(elementLocator).isEnabled())) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);
            // wait for element to be clickable
            passAction(driver, elementLocator);
            return false;
        }else {
        	failAction(driver, elementLocator);
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
        // TODO: implement enum for list of possible actions
        if (identifyUniqueElement(driver, elementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

            Boolean wasActionPerformed = false;
            if (!System.getProperty("targetOperatingSystem").equals("Mac-64")) {
                wasActionPerformed = performClipboardActionsForMac(driver, action);
            } else {
                wasActionPerformed = performClipboardActions(driver, elementLocator, action);
            }

            if (Boolean.TRUE.equals(wasActionPerformed)) {
                passAction(driver, elementLocator, action);
            } else {
                failAction(driver, action, elementLocator);
            }

        } else {
            failAction(driver, elementLocator);
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
        if (identifyUniqueElement(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENTNOTFOUNDEXCEPTION, false)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

            Boolean valueSetSuccessfully = internalSetValueUsingJavaScript(driver, elementLocator, value);

            if (Boolean.TRUE.equals(valueSetSuccessfully)) {
                passAction(driver, elementLocator, value);
            } else {
                failAction(driver, elementLocator);
            }
        } else {
            failAction(driver, elementLocator);
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
        if (identifyUniqueElement(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENTNOTFOUNDEXCEPTION, false)) {
            // Override current locator with the aiGeneratedElementLocator
            elementLocator = updateLocatorWithAIGenratedOne(elementLocator);

            try {
                ((JavascriptExecutor) driver).executeScript("arguments[0].submit();",
                        driver.findElement(elementLocator));
                passAction(driver, elementLocator);
            } catch (Exception rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, elementLocator, rootCauseException);
            }
        } else {
            failAction(driver, elementLocator);
        }
    }

    /**
     * This is a generic method to enable the execution of any of the native mobile
     * commands found herein: http://appium.io/docs/en/commands/mobile-command/
     * <p>
     * Note: This method does no validation on the output of the executed JavaScript
     *
     * @param driver     the current instance of Selenium webdriver, which should
     *                   wrap around a native mobile object
     * @param command    the desired mobile command to be executed. e.g., "mobile:
     *                   scroll"
     * @param parameters a map of the key, value parameters for this command. e.g.,
     *                   ImmutableMap.of("direction", "down")
     */
    public static void executeNativeMobileCommand(WebDriver driver, String command, Map<String, String> parameters) {
        try {
            ((JavascriptExecutor) driver).executeScript(command, parameters);
        } catch (Exception rootCauseException) {
            failAction(driver, null, rootCauseException);
        }
    }

    /**
     * This is a convenience method to be able to call TouchActions Actions for
     * touch-enabled devices from within the regular Element Actions Class.
     * <p>
     * Sample use would look like this:
     * ElementActions.performTouchAction().tap(driver, loginButton);
     *
     * @param driver the current instance of Selenium webdriver, which should wrap
     *               around a native mobile object
     * @return a TouchActions object capable of performing actions on touch-enabled devices
     */
    public static TouchActions performTouchAction(WebDriver driver) {
        return new TouchActions(driver);
    }

    public static SikuliActions performSikuliAction() {
        return new SikuliActions();
    }

    public static SikuliActions performSikuliAction(App applicationWindow) {
        return new SikuliActions(applicationWindow);
    }

    /**
     * Types the required file path into an input[type='file'] button, to
     * successfully upload the target file.
     *
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param absoluteFilePath the full path to the file that needs to be uploaded
     * @return a self-reference to be used to chain actions
     */
    public ElementActions typeFileLocationForUpload(By elementLocator, String absoluteFilePath) {
        typeFileLocationForUpload(lastUsedDriver, elementLocator, absoluteFilePath);
        return this;
    }

    /**
     * Appends the required string into the target element, regardless of the
     * current text value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be appended into the
     *                       target webElement
     * @return a self-reference to be used to chain actions
     */
    public ElementActions typeAppend(By elementLocator, String text) {
        typeAppend(lastUsedDriver, elementLocator, text);
        return this;
    }

    /**
     * Selects an element from a dropdown list using its displayed text
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the text of the choice that you need to select from the
     *                       target dropDown menu
     * @return a self-reference to be used to chain actions
     */
    public ElementActions select(By elementLocator, String text) {
        select(lastUsedDriver, elementLocator, text);
        return this;
    }

    /**
     * Sends a keypress to the target element.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param keys           the key that should be pressed
     * @return a self-reference to be used to chain actions
     */
    public ElementActions keyPress(By elementLocator, Keys keys) {
        keyPress(lastUsedDriver, elementLocator, keys);
        return this;
    }

    /**
     * Hovers over target element. If you want to hover on a webElement to expose
     * another webElement and click on it, use hoverAndClick instead for a more
     * reliable result.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions hover(By elementLocator) {
        hover(lastUsedDriver, elementLocator);
        return this;
    }

    /**
     * Hovers over the hoverElements in sequence then clicks the clickableElement
     *
     * @param hoverElementLocators    the list of locators of the webElements under
     *                                test upon which the hover action will be
     *                                performed in sequence (By xpath, id, selector,
     *                                name ...etc)
     * @param clickableElementLocator the locator of the webElement under test upon
     *                                which the click action will be performed (By
     *                                xpath, id, selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions hoverAndClick(List<By> hoverElementLocators, By clickableElementLocator) {
        hoverAndClick(lastUsedDriver, hoverElementLocators, clickableElementLocator);
        return this;
    }

    /**
     * Drags the source element and drops it onto the destination element using
     * javascript
     *
     * @param sourceElementLocator      the locator of the source webElement that
     *                                  should be dragged under test (By xpath, id,
     *                                  selector, name ...etc)
     * @param destinationElementLocator the locator of the target webElement that
     *                                  should receive the dropped source element
     *                                  under test (By xpath, id, selector, name
     *                                  ...etc)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions dragAndDrop(By sourceElementLocator, By destinationElementLocator) {
        dragAndDrop(lastUsedDriver, sourceElementLocator, destinationElementLocator);
        return this;
    }

    /**
     * Drags the source element and drops it onto the determined offset
     *
     * @param sourceElementLocator the locator of the source webElement that should
     *                             be dragged under test (By xpath, id, selector,
     *                             name ...etc)
     * @param xOffset              the horizontal offset by which the element should
     *                             be moved
     * @param yOffset              the vertical offset by which the element should
     *                             be moved
     * @return a self-reference to be used to chain actions
     */
    public ElementActions dragAndDropByOffset(By sourceElementLocator, int xOffset, int yOffset) {
        dragAndDropByOffset(lastUsedDriver, sourceElementLocator, xOffset, yOffset);
        return this;
    }

    /**
     * Retrieves text from the target element and returns it as a string value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the text value of the target webElement
     */
    public String getText(By elementLocator) {
        return getText(lastUsedDriver, elementLocator);
    }

    /**
     * Get the value of the given attribute of the element. Will return the current
     * value, even if this has been modified after the page has been loaded.
     * <p>
     * More exactly, this method will return the value of the property with the
     * given name, if it exists. If it does not, then the value of the attribute
     * with the given name is returned. If neither exists, null is returned.
     * <p>
     * The "style" attribute is converted as best can be to a text representation
     * with a trailing semi-colon.
     * <p>
     * The following are deemed to be "boolean" attributes, and will return either
     * "true" or null:
     * <p>
     * async, autofocus, autoplay, checked, compact, complete, controls, declare,
     * defaultchecked, defaultselected, defer, disabled, draggable, ended,
     * formnovalidate, hidden, indeterminate, iscontenteditable, ismap, itemscope,
     * loop, multiple, muted, nohref, noresize, noshade, novalidate, nowrap, open,
     * paused, pubdate, readonly, required, reversed, scoped, seamless, seeking,
     * selected, truespeed, willvalidate
     * <p>
     * Finally, the following commonly mis-capitalized attribute/property names are
     * evaluated as expected:
     * <p>
     * If the given name is "class", the "className" property is returned. If the
     * given name is "readonly", the "readOnly" property is returned. Note: The
     * reason for this behavior is that users frequently confuse attributes and
     * properties. If you need to do something more precise, e.g., refer to an
     * attribute even when a property of the same name exists, then you should
     * evaluate Javascript to obtain the result you desire.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param attributeName  the target attribute of the webElement under test
     * @return the value of the target attribute of the webElement under test
     */
    public String getAttribute(By elementLocator, String attributeName) {
        return getAttribute(lastUsedDriver, elementLocator, attributeName);
    }

    /**
     * Get the value of a given CSS property. Color values should be returned as
     * RGBA strings, so, for example if the "background-color" property is set as
     * "green" in the HTML source, the returned value will be "RGBA(0, 255, 0, 1)".
     * Note that shorthand CSS properties (e.g. background, font, border,
     * border-top, margin, margin-top, padding, padding-top, list-style, outline,
     * pause, cue) are not returned, in accordance with the DOM CSS2 specification -
     * you should directly access the longhand properties (e.g. background-color) to
     * access the desired values.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param propertyName   the target CSS property of the webElement under test
     * @return the value of the target CSS property of the webElement under test
     */
    public String getCSSProperty(By elementLocator, String propertyName) {
        return getCSSProperty(lastUsedDriver, elementLocator, propertyName);

    }

    /**
     * Clicks on a certain element using Selenium WebDriver, or JavaScript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions click(By elementLocator) {
        click(lastUsedDriver, elementLocator);
        return this;
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required string into the target element.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     * @return a self-reference to be used to chain actions
     */
    public ElementActions type(By elementLocator, String text) {
        type(lastUsedDriver, elementLocator, text);
        return this;
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required string into the target element. Obfuscates the written text in the output report. This action should be used for writing passwords and secure text.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     * @return a self-reference to be used to chain actions
     */
    public ElementActions typeSecure(By elementLocator, String text) {
        typeSecure(lastUsedDriver, elementLocator, text);
        return this;
    }

    public enum TextDetectionStrategy {
        TEXT("text"), CONTENT("textContent"), VALUE("value"), UNDEFINED("undefined");

        private final String value;

        TextDetectionStrategy(String strategy) {
            this.value = strategy;
        }

        protected String getValue() {
            return value;
        }
    }
}
