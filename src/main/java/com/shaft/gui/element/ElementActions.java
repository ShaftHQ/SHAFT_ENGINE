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
import org.openqa.selenium.NoSuchElementException;
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
import java.util.List;
import java.util.*;
import java.util.concurrent.TimeUnit;

@SuppressWarnings("unused")
public class ElementActions {
    private static final int DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER = Integer
            .parseInt(System.getProperty("defaultElementIdentificationTimeout").trim());
    private static final Duration DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT = Duration
            .ofSeconds(DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER);
    private static final int ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION = Integer
            .parseInt(System.getProperty("attemptsBeforeThrowingElementNotFoundException").trim());
    private static final int ELEMENT_IDENTIFICATION_POLLING_DELAY = 1; // seconds
    private static final boolean FORCE_CHECK_FOR_ELEMENT_VISIBILITY = Boolean
            .parseBoolean(System.getProperty("forceCheckForElementVisibility").trim());
    private static final String AI_REFERENCE_FILE_NAME = "aiAidedElementIdentificationReferenceDB.properties";
    private static final String OBFUSCATED_STRING = "•";
    private static WebDriver lastUsedDriver = null;
    private static By aiGeneratedElementLocator = null;

    public ElementActions(WebDriver driver) {
        setLastUsedDriver(driver);
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
                boolean initialLoggingState = ReportManager.isDiscreteLogging();
                ReportManager.setDiscreteLogging(false);
                ReportManager.log(
                        "New Element found using AI... Kindly update your element locator [" + elementLocator + "].");
                ReportManager.setDiscreteLogging(initialLoggingState);

                String newXpath = suggestNewXpath(driver, targetElement, elementLocator);
                if (newXpath != null) {
                    System.setProperty(hashedLocatorName, newXpath);
                }

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

    /**
     * Clicks on a certain element using Selenium WebDriver, or JavaScript
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    public static void click(WebDriver driver, By elementLocator) {
        By internalElementLocator = elementLocator;
        // Waits for the element to be clickable, and then clicks it.
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            String elementText = "";
            try {
                // attempting to read element text
                elementText = readTextBasedOnSuccessfulLocationStrategy(driver, internalElementLocator,
                        determineSuccessfulTextLocationStrategy(driver, internalElementLocator));
                // adding hover before clicking an element to enable styles to show in the
                // execution screenshots and to solve issues clicking on certain elements.
                performHover(driver, internalElementLocator);
            } catch (Exception e) {
                ReportManager.logDiscrete(e);
            }

            List<Object> screenshot = takeScreenshot(driver, internalElementLocator, "click", null, true);
            // takes screenshot before clicking the element out of view
            try {
                // wait for element to be clickable
                (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                        .until(ExpectedConditions.elementToBeClickable(internalElementLocator));
            } catch (TimeoutException e) {
                ReportManager.logDiscrete(e);
            }

            try {
                driver.findElement(internalElementLocator).click();
            } catch (Exception exception1) {
                try {
                    ((JavascriptExecutor) driver).executeScript("arguments[arguments.length - 1].click();",
                            driver.findElement(internalElementLocator));
                } catch (Exception rootCauseException) {
                    rootCauseException.initCause(exception1);
                    ReportManager.log(exception1);
                    ReportManager.log(rootCauseException);
                    failAction(driver, internalElementLocator, rootCauseException);
                }
            }
            // issue: if performing a navigation after clicking on the login button,
            // navigation is triggered immediately and hence it fails.
            // solution: wait for any possible navigation that may be triggered by this
            // click action to conclude

            // removed to enhance performance, and replaced with a process to assert after
            // every navigation
            if (elementText != null && !elementText.equals("")) {
                passAction(driver, internalElementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                passAction(driver, internalElementLocator, screenshot);
            }
        } else {
            failAction(driver, internalElementLocator);
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);
            (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                    .until(ExpectedConditions.elementToBeClickable(internalElementLocator));
//            By finalInternalElementLocator = internalElementLocator;
//            new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT).until(waitDriver -> ExpectedConditions.elementToBeClickable(finalInternalElementLocator));
            // wait for element to be clickable
            passAction(driver, internalElementLocator);
            (new Actions(driver)).clickAndHold(driver.findElement(internalElementLocator)).build().perform();

            // takes screenshot before holding the element
        } else {
            failAction(driver, internalElementLocator);
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            boolean wasActionPerformed;
            if (!System.getProperty("targetOperatingSystem").equals("Mac-64")) {
                wasActionPerformed = performClipboardActionsForMac(driver, action);
            } else {
                wasActionPerformed = performClipboardActions(driver, internalElementLocator, action);
            }

            if (Boolean.TRUE.equals(wasActionPerformed)) {
                passAction(driver, internalElementLocator, action);
            } else {
                failAction(driver, action, internalElementLocator);
            }

        } else {
            failAction(driver, internalElementLocator);
        }
    }

    /**
     * Double-clicks on an element using Selenium WebDriver's Actions Library
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    public static void doubleClick(WebDriver driver, By elementLocator) {
        By internalElementLocator = elementLocator;
        if (ElementActions.identifyUniqueElement(driver, internalElementLocator)) {
// Override current locator with the aiGeneratedElementLocator
            internalElementLocator = ElementActions.updateLocatorWithAIGeneratedOne(internalElementLocator);
            String elementText = "";
            try {
                // attempting to read element text
                elementText = readTextBasedOnSuccessfulLocationStrategy(driver, internalElementLocator,
                        determineSuccessfulTextLocationStrategy(driver, internalElementLocator));
            } catch (Exception e) {
                // do nothing
                ReportManager.logDiscrete(e);
            }
            List<Object> screenshot = ElementActions.takeScreenshot(driver, internalElementLocator, "doubleClick", null, true);
            // takes screenshot before clicking the element out of view

            try {
                (new Actions(driver)).moveToElement(driver.findElement(internalElementLocator)).doubleClick().perform();
            } catch (Exception e) {
                ElementActions.failAction(driver, internalElementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                ElementActions.passAction(driver, internalElementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                ElementActions.passAction(driver, internalElementLocator, screenshot);
            }
        } else {
            ElementActions.failAction(driver, internalElementLocator);
        }
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
        By internalSourceElementLocator = sourceElementLocator;
        By internalDestinationElementLocator = destinationElementLocator;
        if (identifyUniqueElement(driver, internalSourceElementLocator)
                && identifyUniqueElement(driver, internalDestinationElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalSourceElementLocator = updateLocatorWithAIGeneratedOne(internalSourceElementLocator);
            internalDestinationElementLocator = updateLocatorWithAIGeneratedOne(internalDestinationElementLocator);

            // replaced canFindUniqueElementForInternalUse, with countFoundElements for
            // destinationElement to bypass the check for element visibility

            // define source and destination elements
            WebElement sourceElement = driver.findElement(internalSourceElementLocator);
            WebElement destinationElement = driver.findElement(internalDestinationElementLocator);

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
                failAction(driver, internalSourceElementLocator, rootCauseException);
            }

            // get source element end location
            String endLocation = driver.findElement(internalSourceElementLocator).getLocation().toString();

            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;

            if (!endLocation.equals(startLocation)) {
                passAction(driver, internalSourceElementLocator, reportMessage);
            } else {
                try {
                    (new Actions(driver)).dragAndDrop(driver.findElement(internalSourceElementLocator),
                            driver.findElement(internalDestinationElementLocator)).build().perform();

                } catch (Exception rootCauseException) {
                    ReportManager.log(rootCauseException);
                    failAction(driver, internalSourceElementLocator, rootCauseException);
                }
                // get source element end location
                endLocation = driver.findElement(internalSourceElementLocator).getLocation().toString();
                if (!endLocation.equals(startLocation)) {
                    passAction(driver, internalSourceElementLocator, reportMessage);
                } else {
                    failAction(driver, reportMessage, internalSourceElementLocator);
                }
            }
        } else {
            failAction(driver, internalSourceElementLocator);
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
        By internalSourceElementLocator = sourceElementLocator;
        if (identifyUniqueElement(driver, internalSourceElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalSourceElementLocator = updateLocatorWithAIGeneratedOne(internalSourceElementLocator);

            WebElement sourceElement = driver.findElement(internalSourceElementLocator);
            String startLocation = sourceElement.getLocation().toString();

            // attempt to perform drag and drop
            try {
                (new Actions(driver)).dragAndDropBy(driver.findElement(internalSourceElementLocator), xOffset, yOffset).build()
                        .perform();
            } catch (Exception rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, internalSourceElementLocator, rootCauseException);
            }

            String endLocation = driver.findElement(internalSourceElementLocator).getLocation().toString();

            if (!endLocation.equals(startLocation)) {
                passAction(driver, internalSourceElementLocator,
                        "Start point: " + startLocation + ", End point: " + endLocation);
            } else {
                failAction(driver, "Start point = End point: " + endLocation, internalSourceElementLocator);
            }
        } else {
            failAction(driver, internalSourceElementLocator);
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

    public static By getAiGeneratedElementLocator() {
        return aiGeneratedElementLocator;
    }

    public static String getAiReferenceFileName() {
        return AI_REFERENCE_FILE_NAME;
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
        By internalElementLocator = elementLocator;
        ReportManager.logDiscrete("Attempting to getAttribute [" + attributeName + "] from elementLocator [" + internalElementLocator + "].");
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);
            try {
                String elementAttribute = driver.findElement(internalElementLocator).getAttribute(attributeName);
                passAction(driver, internalElementLocator, elementAttribute);
                return elementAttribute;
            } catch (UnsupportedCommandException rootCauseException) {
                failAction(driver, internalElementLocator, rootCauseException);
                return null;
            }
        } else {
            failAction(driver, internalElementLocator);
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            String elementCssProperty = driver.findElement(internalElementLocator).getCssValue(propertyName);
            passAction(driver, internalElementLocator, elementCssProperty);
            return elementCssProperty;
        } else {
            failAction(driver, internalElementLocator);
            return null;
        }
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
     * Returns the number of elements that match a certain elementLocator
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return integer value that represents the number of elements that match the
     * desired elementLocator
     */
    public static int getElementsCount(WebDriver driver, By elementLocator) {
        return getMatchingElementsCount(driver, elementLocator, ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION);
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
     * Retrieves the selected text from the target drop-down list element and returns it as a string value.
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the selected text of the target webElement
     */
    public static String getSelectedText(WebDriver driver, By elementLocator) {
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);
            StringBuilder elementSelectedText = new StringBuilder();
            try {
                new Select(driver.findElement(internalElementLocator)).getAllSelectedOptions().forEach(selectedOption -> elementSelectedText.append(selectedOption.getText()));
                passAction(driver, internalElementLocator, elementSelectedText.toString().trim());
                return elementSelectedText.toString().trim();
            } catch (UnexpectedTagNameException rootCauseException) {
                failAction(driver, internalElementLocator, rootCauseException);
                return null;
            }
        } else {
            failAction(driver, internalElementLocator);
            return null;
        }
    }

    /**
     * Retrieves the selected text from the target drop-down list element and returns it as a string value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the selected text of the target webElement
     */
    public static String getSelectedText(By elementLocator) {
        return getSelectedText(lastUsedDriver, elementLocator);
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            String elementSize = driver.findElement(internalElementLocator).getSize().toString();
            passAction(driver, internalElementLocator, elementSize);
            return elementSize;
        } else {
            failAction(driver, internalElementLocator);
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            String elementTagName = driver.findElement(internalElementLocator).getTagName();
            passAction(driver, internalElementLocator, elementTagName);
            return elementTagName;
        } else {
            failAction(driver, internalElementLocator);
            return null;
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            String elementText = driver.findElement(internalElementLocator).getText();

            if (elementText.trim().equals("") && !BrowserFactory.isMobileNativeExecution()) {
                elementText = driver.findElement(internalElementLocator).getAttribute(TextDetectionStrategy.CONTENT.getValue());
            }
            if (elementText.trim().equals("") && !BrowserFactory.isMobileNativeExecution()) {
                elementText = driver.findElement(internalElementLocator).getAttribute(TextDetectionStrategy.VALUE.getValue());
            }
            passAction(driver, internalElementLocator, elementText);
            return elementText;
        } else {
            failAction(driver, internalElementLocator);
            return null;
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
        List<String> windowHandles = new ArrayList<>(driver.getWindowHandles());
        ElementActions.passAction(driver, String.valueOf(windowHandles));
        return windowHandles;

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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            try {
                performHover(driver, internalElementLocator);
            } catch (Exception rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, internalElementLocator, rootCauseException);
            }
            passAction(driver, internalElementLocator);
        } else {
            failAction(driver, internalElementLocator);
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
            hoverElementLocators.set(0, updateLocatorWithAIGeneratedOne(hoverElementLocators.get(0)));

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
        hoverAndClick(driver, Collections.singletonList(hoverElementLocator), clickableElementLocator);
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)

                && driver.findElement(internalElementLocator).isEnabled()) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);
            (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                    .until(ExpectedConditions.elementToBeClickable(internalElementLocator));
//            By finalInternalElementLocator = internalElementLocator;
//            new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT).until(waitDriver -> ExpectedConditions.elementToBeClickable(finalInternalElementLocator));
            // wait for element to be clickable
            passAction(driver, internalElementLocator);
            return true;
        } else if (identifyUniqueElement(driver, internalElementLocator)
                && !(driver.findElement(internalElementLocator).isEnabled())) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);
            // wait for element to be clickable
            passAction(driver, internalElementLocator);
            return false;
        } else {
            failAction(driver, internalElementLocator);
            return false;
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator
                , false)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            boolean isDisplayed = driver.findElement(internalElementLocator).isDisplayed();
            passAction(driver, internalElementLocator);
            return isDisplayed;
        } else {
            failAction(driver, internalElementLocator);
            return false;
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            switch (key.toLowerCase().trim()) {
                case "enter" -> driver.findElement(internalElementLocator).sendKeys(Keys.ENTER);
                case "return" -> driver.findElement(internalElementLocator).sendKeys(Keys.RETURN);
                case "tab" -> driver.findElement(internalElementLocator).sendKeys(Keys.TAB);
                default -> {
                    ReportManager.log("Unsupported Key.");
                    failAction(driver, key, internalElementLocator);
                }
            }
        } else {
            failAction(driver, key, internalElementLocator);
        }
        passAction(driver, internalElementLocator, key);
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            driver.findElement(internalElementLocator).sendKeys(keys);
        } else {
            failAction(driver, keys.name(), internalElementLocator);
        }
        passAction(driver, internalElementLocator, keys.name());
    }

    public static SikuliActions performSikuliAction() {
        return new SikuliActions();
    }

    public static SikuliActions performSikuliAction(App applicationWindow) {
        return new SikuliActions(applicationWindow);
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            //add forced check that the select element actually has options
            try {
                (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                        .until(ExpectedConditions.not(ExpectedConditions.textToBe(internalElementLocator, "")));
            } catch (Exception rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, "waited for (" + DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT.getSeconds() + ") seconds", internalElementLocator, rootCauseException);
            }
            boolean isOptionFound = false;
            var availableOptionsList = (new Select(driver.findElement(internalElementLocator))).getOptions();
            for (int i = 0; i < availableOptionsList.size(); i++) {
                String visibleText = availableOptionsList.get(i).getText();
                String value = availableOptionsList.get(i).getAttribute("value");
                if (visibleText.trim().equals(text) || value.trim().equals(text)) {
                    (new Select(driver.findElement(internalElementLocator))).selectByIndex(i);
                    passAction(driver, internalElementLocator, text);
                    isOptionFound = true;
                    break;
                }
            }
            if (Boolean.FALSE.equals(isOptionFound)) {
                failAction(driver, text, internalElementLocator);
            }
        } else {
            failAction(driver, text, internalElementLocator);
        }
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

    public static void setLastUsedDriver(WebDriver driver) {
        lastUsedDriver = driver;
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator
                , false)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            Boolean valueSetSuccessfully = internalSetValueUsingJavaScript(driver, internalElementLocator, value);

            if (Boolean.TRUE.equals(valueSetSuccessfully)) {
                passAction(driver, internalElementLocator, value);
            } else {
                failAction(driver, internalElementLocator);
            }
        } else {
            failAction(driver, internalElementLocator);
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator
                , false)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            try {
                ((JavascriptExecutor) driver).executeScript("arguments[0].submit();",
                        driver.findElement(internalElementLocator));
                passAction(driver, internalElementLocator);
            } catch (Exception rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, internalElementLocator, rootCauseException);
            }
        } else {
            failAction(driver, internalElementLocator);
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
            boolean discreetLoggingState = ReportManager.isDiscreteLogging();
            ReportManager.setDiscreteLogging(true);
            passAction(driver);
            ReportManager.setDiscreteLogging(discreetLoggingState);
        } catch (Exception rootCauseException) {
            failAction(driver, null, rootCauseException);
        }
    }

    /**
     * Switches focus to default content, is mainly used in coordination with
     * {@link #switchToIframe(WebDriver, By)} to exit any iFrame layer and go back
     * to the main page
     *
     * @return a self-reference to be used to chain actions
     */
    public static ElementActions switchToDefaultContent() {
        if (BrowserFactory.getActiveDriverSessions() > 0 && (lastUsedDriver != null)) {
            try {
                lastUsedDriver.switchTo().defaultContent();
                boolean discreetLoggingState = ReportManager.isDiscreteLogging();
                ReportManager.setDiscreteLogging(true);
                passAction(lastUsedDriver);
                ReportManager.setDiscreteLogging(discreetLoggingState);
            } catch (Exception e) {
                ReportManager.log(e);
            }
        }
        // if there is no last used driver or no drivers in the drivers list, do
        // nothing...
        return new ElementActions(lastUsedDriver);
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            driver.switchTo().frame(driver.findElement(internalElementLocator));
            // note to self: remove internalElementLocator in case of bug in screenshot manager
            boolean discreetLoggingState = ReportManager.isDiscreteLogging();
            ReportManager.setDiscreteLogging(true);
            passAction(driver);
            ReportManager.setDiscreteLogging(discreetLoggingState);
        } else {
            failAction(driver, internalElementLocator);
        }
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)
                && (text != null)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            driver.findElement(internalElementLocator).sendKeys(text);
            passAction(driver, internalElementLocator, text);
        } else {
            failAction(driver, text, internalElementLocator);
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
        String internalAbsoluteFilePath = absoluteFilePath.replace("/", FileSystems.getDefault().getSeparator());
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator
                , false)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            List<Object> screenshot = takeScreenshot(driver, internalElementLocator, "typeFileLocationForUpload", null, true);
            // takes screenshot before clicking the element out of view

            try {
                driver.findElement(internalElementLocator).sendKeys(internalAbsoluteFilePath);
            } catch (InvalidArgumentException e) {
                //this happens when the file path doesn't exist
                failAction(driver, internalAbsoluteFilePath, internalElementLocator, e);

            } catch (ElementNotInteractableException exception1) {
                ((JavascriptExecutor) driver).executeScript(
                        "arguments[0].setAttribute('style', 'display:block !important;');",
                        driver.findElement(internalElementLocator));
                try {
                    driver.findElement(internalElementLocator).sendKeys(internalAbsoluteFilePath);
                } catch (WebDriverException rootCauseException) {
                    rootCauseException.initCause(exception1);
                    ReportManager.log(rootCauseException);
                    // happened for the first time on MacOSX due to incorrect file path separator
                    failAction(driver, internalAbsoluteFilePath, internalElementLocator, rootCauseException);
                }
                try {
                    ((JavascriptExecutor) driver).executeScript("arguments[0].setAttribute('style', 'display:none');",
                            driver.findElement(internalElementLocator));
                } catch (NoSuchElementException | StaleElementReferenceException e) {
                    // this exception is sometimes thrown on firefox after the upload has been
                    // successful, since we don't have to return the style to what it was, then it's
                    // okay to do nothing here.
                    ReportManager.logDiscrete(e);
                }
            }
            passAction(driver, internalElementLocator, internalAbsoluteFilePath, screenshot);
        } else {
            failAction(driver, internalAbsoluteFilePath, internalElementLocator);
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
        By internalElementLocator = elementLocator;
        ReportManager.logDiscrete("Waiting for element to be present; elementLocator [" + internalElementLocator + "], numberOfTries[" + numberOfTries + "], stateOfPresence[" + stateOfPresence + "]...");
        // Override current locator with the aiGeneratedElementLocator
        internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

        int foundElementsCount;
        boolean isElementFound;
        int i = 1;
        do {
            foundElementsCount = getMatchingElementsCount(driver, internalElementLocator, 1);
            isElementFound = foundElementsCount >= 1;
            i++;
        } while (i < numberOfTries && Boolean.compare(stateOfPresence, isElementFound) != 0);

        String reportMessage = "waited up to (" + DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT.getSeconds() * numberOfTries
                + ") seconds, for the element's state of presence to be (" + stateOfPresence
                + "). Element locator (" + internalElementLocator.toString() + ")";
        if (Boolean.compare(stateOfPresence, isElementFound) == 0) {
            passAction(driver, internalElementLocator, reportMessage);
        } else {
            failAction(driver, reportMessage, internalElementLocator);
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
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            try {
                (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER * (long) numberOfTries))
                        .until(ExpectedConditions.not(ExpectedConditions.textToBe(internalElementLocator, initialValue)));
            } catch (Exception rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, "waited for (" + DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT.getSeconds() * numberOfTries
                        + ") seconds", internalElementLocator, rootCauseException);
            }
            try {
                passAction(driver, internalElementLocator,
                        "from: \"" + initialValue + "\", to: \"" + getText(driver, internalElementLocator) + "\"");
            } catch (Exception e) {
                passAction(driver, internalElementLocator, "from: \"" + initialValue + "\", to a new value.");
            }
        } else {
            if (internalElementLocator != null) {
                failAction(driver,
                        "Element with locator (" + internalElementLocator.toString() + ") was not found on this page.",
                        internalElementLocator);
            } else {
                // this code is unreachable it's just in place to satisfy SonarLint
                failAction(driver, "Element has Null locator.", null);
            }
        }
    }

    protected static void failAction(WebDriver driver, By elementLocator, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, null, elementLocator, null, rootCauseException);
    }

    protected static void failAction(WebDriver driver, String testData, By elementLocator, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, elementLocator, null, rootCauseException);
    }

    protected static void failAction(Screen screen, App applicationWindow, Pattern element, String testData, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(null, actionName, testData, null, SikuliActions.prepareElementScreenshotAttachment(screen, applicationWindow, element, actionName, false), rootCauseException);
    }

    protected static boolean identifyUniqueElement(WebDriver driver, By elementLocator) {
        return identifyUniqueElement(driver, elementLocator, true);
    }

    protected static void passAction(WebDriver driver, By elementLocator) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, elementLocator, actionName, null, null);
    }

    protected static void passAction(WebDriver driver, By elementLocator, List<Object> screenshot) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, elementLocator, actionName, null, screenshot);
    }

    protected static void passAction(WebDriver driver, By elementLocator, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, elementLocator, actionName, testData, null);
    }

    protected static void passAction(WebDriver driver, By elementLocator, String testData, List<Object> screenshot) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, elementLocator, actionName, testData, screenshot);
    }

    protected static void passAction(Screen screen, App applicationWindow, Pattern element, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(null, null, actionName, testData, SikuliActions.prepareElementScreenshotAttachment(screen, applicationWindow, element, actionName, true));
    }

    protected static List<Object> takeScreenshot(WebDriver driver, By elementLocator, String actionName, String testData,
                                                 boolean passFailStatus) {
        if (passFailStatus) {
            try {
                if (elementLocator != null) {
                    return ScreenshotManager.captureScreenShot(driver, elementLocator, actionName, true);
                } else if (testData != null) {
                    return ScreenshotManager.captureScreenShot(driver, actionName, true);
                }
                // else only happens when switching to default content so there is no need to
                // take a screenshot
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

    protected static By updateLocatorWithAIGeneratedOne(By elementLocator) {
        // Override current locator with the aiGeneratedElementLocator
        if (Boolean.TRUE.equals(ScreenshotManager.getAiSupportedElementIdentification())
                && aiGeneratedElementLocator != null && elementLocator != null) {
            return aiGeneratedElementLocator;
        }
        return elementLocator;
    }

    private static void checkForElementVisibility(WebDriver driver, By elementLocator) {
        if (FORCE_CHECK_FOR_ELEMENT_VISIBILITY) {
            try {
                (new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT_INTEGER))
                        .until(ExpectedConditions.visibilityOfElementLocated(elementLocator));
//                new WebDriverWait(driver, DEFAULT_ELEMENT_IDENTIFICATION_TIMEOUT).until(waitDriver -> ExpectedConditions.visibilityOfElementLocated(elementLocator));
            } catch (TimeoutException rootCauseException) {
                ReportManager.log(rootCauseException);
                failAction(driver, "unique element matching this locator \"" + elementLocator + "\" is not visible.",
                        null, rootCauseException);
            }
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

    private static String confirmTypingWasSuccessful(WebDriver driver, By elementLocator, String expectedText,
                                                     TextDetectionStrategy successfulTextLocationStrategy) {
        TextDetectionStrategy updatedSuccessfulTextLocationStrategy = successfulTextLocationStrategy;
        if (updatedSuccessfulTextLocationStrategy.equals(TextDetectionStrategy.UNDEFINED)) {
            updatedSuccessfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(driver,
                    elementLocator);
        }
        String actualText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
                updatedSuccessfulTextLocationStrategy);

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

    private static TextDetectionStrategy determineSuccessfulTextLocationStrategy(WebDriver driver, By elementLocator) {
        if (BrowserFactory.isMobileNativeExecution()) {
            return TextDetectionStrategy.TEXT;
        }
        String text = driver.findElement(elementLocator).getText().trim();
        String content = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.CONTENT.getValue()).trim();
        String value = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.VALUE.getValue());

        if (value != null) {
            value = value.trim();
        }

        TextDetectionStrategy successfulTextLocationStrategy;
        if (!"".equals(text)) {
            successfulTextLocationStrategy = TextDetectionStrategy.TEXT;
        } else if (!"".equals(content)) {
            successfulTextLocationStrategy = TextDetectionStrategy.CONTENT;
        } else if (value != null && !"".equals(value)) {
            successfulTextLocationStrategy = TextDetectionStrategy.VALUE;
        } else {
            successfulTextLocationStrategy = TextDetectionStrategy.UNDEFINED;
        }
        return successfulTextLocationStrategy;
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

    private static int getMatchingElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts) {
        return getMatchingElementsCount(driver, elementLocator, numberOfAttempts, true);
    }

    private static int getMatchingElementsCount(WebDriver driver, By elementLocator, int numberOfAttempts,
                                                boolean waitForLazyLoading) {
        By internalElementLocator = elementLocator;
        RecordManager.startVideoRecording(driver);
        if (waitForLazyLoading) {
            JavaScriptWaitManager.waitForLazyLoading();
        }

        int matchingElementsCount = 0;
        if (internalElementLocator != null && internalElementLocator.equals(By.tagName("html"))) {
            matchingElementsCount = waitForElementPresence(driver, internalElementLocator, numberOfAttempts);
        } else if (internalElementLocator != null) {
            // check to see if this element was already identified using AI, and if it's
            // still unique, use that locator directly
            String hashedLocatorName = ImageProcessingActions.formatElementLocatorToImagePath(internalElementLocator);
            String previouslyIdentifiedXpath = System.getProperty(hashedLocatorName);
            setAiGeneratedXpath(previouslyIdentifiedXpath);

            // wait for element presence
            if (previouslyIdentifiedXpath != null && Boolean.TRUE.equals(ScreenshotManager.getAiSupportedElementIdentification())) {
                internalElementLocator = aiGeneratedElementLocator;
            }
            matchingElementsCount = waitForElementPresence(driver, internalElementLocator, numberOfAttempts);

            if (matchingElementsCount == 0
                    && Boolean.TRUE.equals(attemptToFindElementUsingAI(driver, internalElementLocator))) {
                matchingElementsCount = 1;
            } else if (matchingElementsCount == 1) {
                if (previouslyIdentifiedXpath != null) {
                    boolean initialLoggingState = ReportManager.isDiscreteLogging();
                    ReportManager.setDiscreteLogging(false);
                    ReportManager
                            .log("Element was previously found using AI... Kindly update your element locator from ["
                                    + internalElementLocator + "] to [" + aiGeneratedElementLocator + "].");
                    ReportManager.setDiscreteLogging(initialLoggingState);
                    internalElementLocator = aiGeneratedElementLocator;
                }
                ScreenshotManager.storeElementScreenshotForAISupportedElementIdentification(driver, internalElementLocator);
            }
        }
        return matchingElementsCount;
    }

    private static boolean identifyUniqueElement(WebDriver driver, By elementLocator,
                                                 boolean checkForVisibility) {
        By internalElementLocator = elementLocator;
        // Override current locator with the aiGeneratedElementLocator
        internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

        int matchingElementsCount = getMatchingElementsCount(driver, internalElementLocator, ElementActions.ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION);
        if (internalElementLocator != null) {
            // unique element found
            switch (matchingElementsCount) {
                case 0 -> failAction(driver, "zero elements found matching this locator \"" + internalElementLocator + "\".", internalElementLocator);
                case 1 -> {
                    if (checkForVisibility && !internalElementLocator.toString().contains("input[@type='file']")
                            && !internalElementLocator.equals(By.tagName("html"))) {
                        try {
                            // scroll element into viewPort
                            ((Locatable) driver.findElement(internalElementLocator)).getCoordinates().inViewPort();
                        } catch (UnsupportedCommandException getElementLocationOnceScrolledIntoView) {
                            // TODO: appium -> swipe element into view
//                            if (BrowserFactory.isMobileNativeExecution()) {
                            //ElementActions.performTouchAction(driver).swipeElementIntoView(internalElementLocator, TouchActions.SwipeDirection.DOWN);
//                            }
                            //ReportManager.logDiscrete(getElementLocationOnceScrolledIntoView);
                        }

                        // check for visibility
                        checkForElementVisibility(driver, internalElementLocator);
                    }
                    return true;
                }
                default -> {
                    if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckElementLocatorIsUnique")))) {
                        failAction(driver, "multiple elements found matching this locator \"" + internalElementLocator + "\".",
                                internalElementLocator);
                    }
                    return true;
                }
            }
        } else {
            failAction(driver, "element locator is NULL.", null);
        }
        return false;
    }

    /**
     * Used to force set the value of a certain element using javascript, bypassing
     * regular visibility and element uniqueness checks
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param value          the desired value that will be forced
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

    private static void passAction(WebDriver driver) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, null, actionName, null, null);
    }

    private static void passAction(WebDriver driver, By elementLocator, String actionName, String testData,
                                   List<Object> screenshot) {
        reportActionResult(driver, actionName, testData, elementLocator, screenshot, true);
    }

    private static void passAction(WebDriver driver, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, null, actionName, testData, null);
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

    private static void performType(WebDriver driver, By elementLocator, String text) {
        // implementing loop to try and break out of the stale element exception issue
        for (int i = 0; i < ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION; i++) {
            try {
                // attempt to perform action
                driver.findElement(elementLocator).sendKeys(text);
                break;
            } catch (StaleElementReferenceException | ElementNotInteractableException | UnreachableBrowserException
                    | NoSuchElementException | TimeoutException e) {
                if (i + 1 == ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION) {
                    ReportManager.log(e);
                }
            } catch (Exception e) {
                if (e.getMessage().contains("cannot focus element")
                        && (i + 1 == ATTEMPTS_BEFORE_THROWING_ELEMENT_NOT_FOUND_EXCEPTION)) {
                    ReportManager.log(e);
                } else {
                    ReportManager.log(e);
                    ReportManager.log("Unhandled Exception: " + e.getMessage());
                }
            }
        }
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

    private static String reportActionResult(WebDriver driver, String actionName, String testData, By elementLocator,
                                             List<Object> screenshot, Boolean passFailStatus) {
        String message;
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

        if (screenshot != null && !screenshot.equals(new ArrayList<>())) {
            // screenshot taken before action (in case of click)
            attachments.add(screenshot);
        } else if (driver != null && elementLocator != null) {
            List<Object> newScreenshot = takeScreenshot(driver, elementLocator, actionName, testData, passFailStatus);
            if (newScreenshot != null && !newScreenshot.equals(new ArrayList<>())) {
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

    private static void setAiGeneratedXpath(String newXpath) {
        if (newXpath == null) {
            aiGeneratedElementLocator = null;
        } else {
            aiGeneratedElementLocator = By.xpath(newXpath);
        }
        ScreenshotManager.setAiGeneratedElementLocator(aiGeneratedElementLocator);
    }

    private static String suggestNewXpath(WebDriver driver, WebElement targetElement, By deprecatedElementLocator) {
        // attempt to find an optimal xpath for the targetElement
        int maximumXpathNodes = 6;
        String newXpath = "";
        for (int i = 0; i < maximumXpathNodes; i++) {
            String xpathFindingAlgorithm = JSHelpers.ELEMENT_GET_XPATH.getValue();
            /*
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
            boolean initialLoggingState = ReportManager.isDiscreteLogging();
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

    private static String typeWrapper(WebDriver driver, By elementLocator, String targetText) {
        By internalElementLocator = elementLocator;
        if (identifyUniqueElement(driver, internalElementLocator)) {
            // Override current locator with the aiGeneratedElementLocator
            internalElementLocator = updateLocatorWithAIGeneratedOne(internalElementLocator);

            TextDetectionStrategy successfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(driver,
                    internalElementLocator);
            if (!successfulTextLocationStrategy.equals(TextDetectionStrategy.UNDEFINED)) {
                clearBeforeTyping(driver, internalElementLocator, successfulTextLocationStrategy);
            }
            if (!"".equals(targetText)) {
                performType(driver, internalElementLocator, targetText);
            }
            if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckTextWasTypedCorrectly")))) {
                return confirmTypingWasSuccessful(driver, internalElementLocator, targetText, successfulTextLocationStrategy);
            } else {
                return targetText;
            }
        } else {
            ReportManager.log("Failed to identify Target element with locator [" + internalElementLocator + "].");
            return null;
        }
    }

    private static int waitForElementPresence(WebDriver driver, By elementLocator, int numberOfAttempts) {
        try {
            new FluentWait<>(driver)
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
     * Waits for the element to be clickable, and then clicks and holds it.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions clickAndHold(By elementLocator) {
        clickAndHold(lastUsedDriver, elementLocator);
        return this;
    }

    /**
     * Attempts to perform a native clipboard action on the text from a certain web
     * element, like copy/cut/paste
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param action         supports the following actions "copy", "paste", "cut",
     *                       "select all", "unselect"
     * @return a self-reference to be used to chain actions
     */
    public ElementActions clipboardActions(By elementLocator, String action) {
        clipboardActions(lastUsedDriver, elementLocator, action);
        return this;
    }

    /**
     * Double-clicks on an element using Selenium WebDriver's Actions Library
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions doubleClick(By elementLocator) {
        doubleClick(lastUsedDriver, elementLocator);
        return this;
    }

    /**
     * Drags the source element and drops it onto the destination element
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
     * Returns the handle for currently active context. This can be used to switch
     * to this context at a later time.
     *
     * @return The current context handle
     */
    public String getContext() {
        return getContext(lastUsedDriver);
    }

    /**
     * Switches focus to another context
     *
     * @param context The name of the context or the handle as returned by
     *                ElementActions.getContext(WebDriver driver)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions setContext(String context) {
        setContext(lastUsedDriver, context);
        return this;
    }

    /**
     * Returns a list of unique handles for all the currently open contexts. This
     * can be used to switch to any of these contexts at a later time.
     *
     * @return list of context handles
     */
    public List<String> getContextHandles() {
        return getContextHandles(lastUsedDriver);
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
     * Returns the unique handle for currently active window. This can be used to
     * switch to this window at a later time.
     *
     * @return window handle
     */
    public String getWindowHandle() {
        return getWindowHandle(lastUsedDriver);
    }

    /**
     * Returns a list of unique handles for all the currently open windows. This can
     * be used to switch to any of these windows at a later time.
     *
     * @return list of window handles
     */
    public List<String> getWindowHandles() {
        return getWindowHandles(lastUsedDriver);
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
     * Used to set value for an element (hidden or visible) using javascript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param value          the desired value that should be set for the target
     *                       element
     * @return a self-reference to be used to chain actions
     */
    public ElementActions setValueUsingJavaScript(By elementLocator, String value) {
        setValueUsingJavaScript(lastUsedDriver, elementLocator, value);
        return this;
    }

    /**
     * Used to submit a form using javascript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions submitFormUsingJavaScript(By elementLocator) {
        submitFormUsingJavaScript(lastUsedDriver, elementLocator);
        return this;
    }

    /**
     * Switches focus to a certain iFrame, is mainly used in coordination with
     * {@link #switchToDefaultContent(WebDriver)} to navigate inside any iFrame
     * layer and go back to the main page
     *
     * @param elementLocator the locator of the iFrame webElement under test (By
     *                       xpath, id, selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions switchToIframe(By elementLocator) {
        switchToIframe(lastUsedDriver, elementLocator);
        return this;
    }

    /**
     * Switches focus to another window
     *
     * @param nameOrHandle The name of the window or the handle as returned by
     *                     ElementActions.getWindowHandle(WebDriver driver)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions switchToWindow(String nameOrHandle) {
        switchToWindow(lastUsedDriver, nameOrHandle);
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

    /**
     * Waits dynamically for a specific element to achieve the desired
     * stateOfPresence on the current page. Waits for a specific number of retries
     * multiplied by the default element identification timeout (in the POM.xml
     * file)
     *
     * @param elementLocator  the locator of the webElement under test (By xpath,
     *                        id, selector, name ...etc)
     * @param numberOfTries   the number of times to try and wait for the element to
     *                        achieve the desired stateOfPresence (default is 1)
     * @param stateOfPresence the expected state of presence of the element; false
     *                        is not present, and true is present
     * @return a self-reference to be used to chain actions
     */
    public ElementActions waitForElementToBePresent(By elementLocator, int numberOfTries,
                                                    boolean stateOfPresence) {
        waitForElementToBePresent(lastUsedDriver, elementLocator, numberOfTries,
                stateOfPresence);
        return this;
    }

    /**
     * Waits dynamically for a specific element's text to change from the initial
     * value to a new unknown value. Waits for a specific number of retries
     * multiplied by the default element identification timeout (in the POM.xml
     * file)
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param initialValue   the initial text value of the target webElement
     * @param numberOfTries  the number of times to try and wait for the element
     *                       text to change (default is 1)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions waitForTextToChange(By elementLocator, String initialValue,
                                              int numberOfTries) {
        waitForTextToChange(lastUsedDriver, elementLocator, initialValue,
                numberOfTries);
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
