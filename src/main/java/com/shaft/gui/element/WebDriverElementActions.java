package com.shaft.gui.element;

import com.shaft.driver.DriverFactoryHelper;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import com.shaft.tools.support.JavaActions;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.remote.UnreachableBrowserException;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.openqa.selenium.support.ui.FluentWait;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.UnexpectedTagNameException;
import org.sikuli.script.App;
import org.sikuli.script.Pattern;
import org.sikuli.script.Screen;
import org.testng.Assert;

import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.time.Duration;
import java.util.List;
import java.util.*;
public class WebDriverElementActions {
    private static final String OBFUSCATED_STRING = "•";
    private static final boolean CLICK_USING_JAVASCRIPT_WHEN_WEB_DRIVER_CLICK_FAILS = Boolean.parseBoolean(System.getProperty("clickUsingJavascriptWhenWebDriverClickFails"));
    private static final boolean ATTEMPT_CLEAR_BEFORE_TYPING_USING_BACKSPACE = Boolean.parseBoolean(System.getProperty("attemptClearBeforeTypingUsingBackspace"));
    public WebDriverElementActions(WebDriver driver) {
        new WebDriverElementActions();
    }
    public WebDriverElementActions() {
    }
    /**
     * If the element is outside the viewport, scrolls the bottom of the element to the bottom of the viewport.
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    public static void scrollToElement(WebDriver driver, By elementLocator) {
        //identifyUniqueElement will internally scroll to find the target element so no extra action is needed
        if (identifyUniqueElement(driver, elementLocator)) {
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null,getElementName(driver,elementLocator));
        } else {
            failAction(driver, elementLocator);
        }
    }
    /**
     * Clicks on a certain element using Selenium WebDriver, or JavaScript
     * Clicks on a certain element using Selenium WebDriver, or JavaScript
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    public static void click(WebDriver driver, By elementLocator) {
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            new TouchActions(driver).tap(elementLocator);
        } else {
        // Waits for the element to be clickable, and then clicks it.
            if (identifyUniqueElement(driver, elementLocator)) {
                var elementName = getElementName(driver, elementLocator);
                try {
                    // adding hover before clicking an element to enable styles to show in the
                    // execution screenshots and to solve issues clicking on certain elements.
                    (new Actions(driver)).moveToElement(driver.findElement(elementLocator)).perform();
                } catch (Throwable t) {
//                    ReportManagerHelper.logDiscrete(t);
                }
                List<Object> screenshot = takeScreenshot(driver, elementLocator, "click", null, true);
                // takes screenshot before clicking the element out of view
                // wait for element to be clickable
                if (Boolean.FALSE.equals(ElementActionsHelper.waitForElementToBeClickable(driver, elementLocator))) {
                    failAction(driver, "element is not clickable", elementLocator);
                }
                try {
                    driver.findElement(elementLocator).click();
                } catch (Exception exception1) {
                    if (CLICK_USING_JAVASCRIPT_WHEN_WEB_DRIVER_CLICK_FAILS) {
                        try {
                            ElementActionsHelper.clickUsingJavascript(driver, elementLocator);
                        } catch (Exception rootCauseException) {
                            rootCauseException.initCause(exception1);
                            ReportManagerHelper.log(exception1);
                            ReportManagerHelper.log(rootCauseException);
                            failAction(driver, elementLocator, rootCauseException);
                        }
                    } else {
                        ReportManagerHelper.log(exception1);
                        failAction(driver, elementLocator, exception1);
                    }
                }
                // issue: if performing a navigation after clicking on the login button,
                // navigation is triggered immediately and hence it fails.
                // solution: wait for any possible navigation that may be triggered by this
                // click action to conclude
                // removed to enhance performance, and replaced with a process to assert after
                // every navigation
                passAction(driver, elementLocator,"", screenshot, elementName);
            } else {
                failAction(driver, elementLocator);
            }
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
            var elementName = getElementName(driver, elementLocator);
            if (Boolean.FALSE.equals(ElementActionsHelper.waitForElementToBeClickable(driver, elementLocator))) {
                failAction(driver, "element is not clickable", elementLocator);
            }
            // wait for element to be clickable
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
            (new Actions(driver)).clickAndHold(driver.findElement(elementLocator)).build().perform();
            // takes screenshot before holding the element
        } else {
            failAction(driver, elementLocator);
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
            var elementName = getElementName(driver, elementLocator);
            boolean wasActionPerformed = false;
            if (System.getProperty("targetOperatingSystem").contains("Mac")) {
                wasActionPerformed = performClipboardActions(driver, elementLocator, action, Keys.COMMAND);
            }else{
                wasActionPerformed = performClipboardActions(driver, elementLocator, action, Keys.CONTROL);
            }
            if (Boolean.TRUE.equals(wasActionPerformed)) {
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), action, null, elementName);
            } else {
                failAction(driver, action, elementLocator);
            }
        } else {
            failAction(driver, elementLocator);
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
        if (WebDriverElementActions.identifyUniqueElement(driver, elementLocator)) {
            var elementName = getElementName(driver, elementLocator);
            // takes screenshot before clicking the element out of view
            var screenshot = WebDriverElementActions.takeScreenshot(driver, elementLocator, "doubleClick", null, true);
            List<List<Object>> attachments = new LinkedList<>();
            attachments.add(screenshot);
            try {
                (new Actions(driver)).moveToElement(driver.findElement(elementLocator)).doubleClick().perform();
            } catch (Exception e) {
                WebDriverElementActions.failAction(driver, elementLocator, e);
            }
            WebDriverElementActions.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, attachments, elementName);
        } else {
            WebDriverElementActions.failAction(driver, elementLocator);
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
        if (identifyUniqueElement(driver, sourceElementLocator)
                && identifyUniqueElement(driver, destinationElementLocator)) {
            var elementName = getElementName(driver, sourceElementLocator);
            // replaced canFindUniqueElementForInternalUse, with countFoundElements for
            // destinationElement to bypass the check for element visibility
            // get source element start location
            String startLocation = driver.findElement(sourceElementLocator).getLocation().toString();
            // attempt to perform drag and drop
            try {
                ElementActionsHelper.dragAndDropUsingJavascript(driver, sourceElementLocator, destinationElementLocator);
            } catch (Exception rootCauseException) {
                ReportManagerHelper.log(rootCauseException);
                failAction(driver, sourceElementLocator, rootCauseException);
            }
            // get source element end location
            String endLocation = driver.findElement(sourceElementLocator).getLocation().toString();
            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;
            if (!endLocation.equals(startLocation)) {
                passAction(driver, sourceElementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, elementName);
            } else {
                try {
                    (new Actions(driver)).dragAndDrop(driver.findElement(sourceElementLocator),
                            driver.findElement(destinationElementLocator)).build().perform();
                } catch (Exception rootCauseException) {
                    ReportManagerHelper.log(rootCauseException);
                    failAction(driver, sourceElementLocator, rootCauseException);
                }
                // get source element end location
                endLocation = driver.findElement(sourceElementLocator).getLocation().toString();
                if (!endLocation.equals(startLocation)) {
                    passAction(driver, sourceElementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, elementName);
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
            var elementName = getElementName(driver, sourceElementLocator);
            WebElement sourceElement = driver.findElement(sourceElementLocator);
            String startLocation = sourceElement.getLocation().toString();
            // attempt to perform drag and drop
            try {
                (new Actions(driver)).dragAndDropBy(driver.findElement(sourceElementLocator), xOffset, yOffset).build()
                        .perform();
            } catch (Exception rootCauseException) {
                ReportManagerHelper.log(rootCauseException);
                failAction(driver, sourceElementLocator, rootCauseException);
            }
            String endLocation = driver.findElement(sourceElementLocator).getLocation().toString();
            if (!endLocation.equals(startLocation)) {
                passAction(driver, sourceElementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "Start point: " + startLocation + ", End point: " + endLocation, null, elementName);
            } else {
                failAction(driver, "Start point = End point: " + endLocation, sourceElementLocator);
            }
        } else {
            failAction(driver, sourceElementLocator);
        }
    }
    /**
     * This is a generic method to enable the execution of the native mobile
     * commands found herein: <a href="http://appium.io/docs/en/commands/mobile-command/">appium.io/mobile-command/</a>
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
            ElementActionsHelper.executeNativeMobileCommandUsingJavascript(driver, command, parameters);
            var testData = "Command: " + command + ", Parameters: " + parameters;
            passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), testData, null, null);
        } catch (Exception rootCauseException) {
            failAction(driver, null, rootCauseException);
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
     * with a trailing semicolon.
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
        ReportManager.logDiscrete("Attempting to getAttribute \"" + attributeName + "\" from elementLocator \"" + elementLocator + "\".");
        if (identifyUniqueElement(driver, elementLocator)) {
            var elementName = getElementName(driver, elementLocator);
            try {
                String elementAttribute = driver.findElement(elementLocator).getAttribute(attributeName);
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementAttribute, null, elementName);
                return elementAttribute;
            } catch (UnsupportedCommandException rootCauseException) {
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
            var elementName = getElementName(driver, elementLocator);
            String elementCssProperty = driver.findElement(elementLocator).getCssValue(propertyName);
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementCssProperty, null, elementName);
            return elementCssProperty;
        } else {
            failAction(driver, elementLocator);
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
        if (driver instanceof AndroidDriver androidDriver) {
            context = androidDriver.getContext();
        } else if (driver instanceof IOSDriver iosDriver) {
            context = iosDriver.getContext();
        } else {
            WebDriverElementActions.failAction(driver, null);
        }
        passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), context, null, null);
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
        if (driver instanceof AndroidDriver androidDriver) {
            windowHandles.addAll(androidDriver.getContextHandles());
        } else if (driver instanceof IOSDriver iosDriver) {
            windowHandles.addAll(iosDriver.getContextHandles());
        } else {
            WebDriverElementActions.failAction(driver, null);
        }
        passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(windowHandles), null, null);
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
        return Integer.parseInt(getMatchingElementsInformation(driver, elementLocator, Optional.empty(), Optional.empty()).get(0).toString());
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
        return Integer.parseInt(getMatchingElementsInformation(driver, elementLocator, Optional.of(numberOfAttempts), Optional.empty()).get(0).toString());
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
            var elementName = getElementName(driver, elementLocator);
            StringBuilder elementSelectedText = new StringBuilder();
            try {
                new Select(driver.findElement(elementLocator)).getAllSelectedOptions().forEach(selectedOption -> elementSelectedText.append(selectedOption.getText()));
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementSelectedText.toString().trim(), null, elementName);
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
     * Retrieves the selected text from the target drop-down list element and returns it as a string value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the selected text of the target webElement
     */
    public static String getSelectedText(By elementLocator) {
        return getSelectedText(DriverFactoryHelper.getDriver().get(), elementLocator);
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
            var elementName = getElementName(driver, elementLocator);
            String elementSize = driver.findElement(elementLocator).getSize().toString();
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementSize, null, elementName);
            return elementSize;
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
            var elementName = getElementName(driver, elementLocator);
            String elementTagName = driver.findElement(elementLocator).getTagName();
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementTagName, null, elementName);
            return elementTagName;
        } else {
            failAction(driver, elementLocator);
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
        if (identifyUniqueElement(driver, elementLocator)) {
            var elementName = getElementName(driver, elementLocator);
            String elementText = driver.findElement(elementLocator).getText();
            if ((elementText == null || elementText.trim().equals("")) && !DriverFactoryHelper.isMobileNativeExecution()) {
                elementText = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.CONTENT.getValue());
            }
            if ((elementText == null || elementText.trim().equals("")) && !DriverFactoryHelper.isMobileNativeExecution()) {
                elementText = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.VALUE.getValue());
            }
            if (elementText == null) {
                elementText = "";
            }
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementText, null, elementName);
            return elementText;
        } else {
            failAction(driver, elementLocator);
            return null;
        }
    }

    private static String getElementName(WebDriver driver, By elementLocator) {
        if (Boolean.TRUE.equals(Boolean.parseBoolean(System.getProperty("captureElementName"))) && identifyUniqueElement(driver, elementLocator)) {
            try {
                return driver.findElement(elementLocator).getAccessibleName();
            } catch (WebDriverException e){
                //happens on some elements that show unhandled inspector error
                //this exception is thrown on some older selenium grid instances, I saw it with firefox running over selenoid
            }
        }
        return null;
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
        passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), nameOrHandle, null, null);
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
        passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(windowHandles), null, null);
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
        if (identifyUniqueElement(driver, elementLocator)) {
            var elementName = getElementName(driver, elementLocator);
            try {
                (new Actions(driver)).moveToElement(driver.findElement(elementLocator)).perform();
            } catch (Exception rootCauseException) {
                ReportManagerHelper.log(rootCauseException);
                failAction(driver, elementLocator, rootCauseException);
            }
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
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
        hoverElementLocators.forEach(hoverElementLocator -> ElementActions.hover(driver, hoverElementLocator));
        ElementActions.click(driver, clickableElementLocator);
    }
    /**
     * Hovers over the hoverElement then clicks the clickableElement
     *
     * @param driver                  the current instance of Selenium webdriver
     * @param hoverElementLocator     the locator of the webElement under test upon
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
        if (identifyUniqueElement(driver, elementLocator)){
            var elementName = getElementName(driver, elementLocator);
            if (ElementActionsHelper.waitForElementToBeClickable(driver, elementLocator)){
                //element is clickable
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "element is clickable", null, elementName);
                return true;
            }else{
                //element is not clickable
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "element is not clickable", null, elementName);
                return false;
            }
        }else{
            //element is not unique
            failAction(driver, elementLocator);
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
        if (identifyUniqueElementIgnoringVisibility(driver, elementLocator)) {
            var elementName = getElementName(driver, elementLocator);
            boolean isDisplayed = driver.findElement(elementLocator).isDisplayed();
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
            return isDisplayed;
        } else {
            failAction(driver, elementLocator);
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
        if (identifyUniqueElement(driver, elementLocator)) {
            var elementName = getElementName(driver, elementLocator);
            switch (key.toLowerCase().trim()) {
                case "enter" -> driver.findElement(elementLocator).sendKeys(Keys.ENTER);
                case "return" -> driver.findElement(elementLocator).sendKeys(Keys.RETURN);
                case "tab" -> driver.findElement(elementLocator).sendKeys(Keys.TAB);
                default -> {
                    ReportManager.log("Unsupported Key.");
                    failAction(driver, key, elementLocator);
                }
            }
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), key, null, elementName);
        } else {
            failAction(driver, key, elementLocator);
        }
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
            var elementName = getElementName(driver, elementLocator);
            List<Object> screenshot = takeScreenshot(driver, elementLocator, "keyPress", null, true);
            // takes screenshot before moving the element out of view
            driver.findElement(elementLocator).sendKeys(key);
            passAction(driver, elementLocator, key.name(), screenshot, elementName);
        } else {
            failAction(driver, key.name(), elementLocator);
        }
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
     * Waits dynamically for a specific element's text to change from the initial
     * value to a new unknown value. Waits until the default element identification timeout
     *
     * @param driver         the current instance of Selenium webdriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param initialValue   the initial text value of the target webElement
     */
    public static void waitForTextToChange(WebDriver driver, By elementLocator, String initialValue) {
        if (identifyUniqueElement(driver, elementLocator)) {
            var elementName = getElementName(driver, elementLocator);
            if (!Boolean.TRUE.equals(ElementActionsHelper.waitForElementTextToBeNot(driver, elementLocator, initialValue))) {
                failAction(driver, initialValue, elementLocator);
            }
            try {
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "from: \"" + initialValue + "\", to: \"" + getText(driver, elementLocator) + "\"", null, elementName);
            } catch (Exception e) {
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "from: \"" + initialValue + "\", to a new value.", null, elementName);
            }
        } else {
            if (elementLocator != null) {
                failAction(driver,
                        "Element with locator (" + elementLocator + ") was not found on this page.",
                        elementLocator);
            } else {
                // this code is unreachable it's just in place to satisfy SonarLint
                failAction(driver, "Element has Null locator.", null);
            }
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
            var elementName = getElementName(driver, elementLocator);
            //add forced check that the select element actually has options and is not empty
            if (!Boolean.TRUE.equals(ElementActionsHelper.waitForElementTextToBeNot(driver, elementLocator, ""))) {
                failAction(driver, text, elementLocator);
            }
            boolean isOptionFound = false;
            var availableOptionsList = (new Select(driver.findElement(elementLocator))).getOptions();
            for (int i = 0; i < availableOptionsList.size(); i++) {
                String visibleText = availableOptionsList.get(i).getText();
                String value = availableOptionsList.get(i).getAttribute("value");
                if (visibleText.trim().equals(text) || value.trim().equals(text)) {
                    (new Select(driver.findElement(elementLocator))).selectByIndex(i);
                    passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), text, null, elementName);
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
     * Switches focus to another context
     *
     * @param driver  the current instance of Appium Driver
     * @param context The name of the context or the handle as returned by
     *                ElementActions.getContext(WebDriver driver)
     */
    public static void setContext(WebDriver driver, String context) {
        if (driver instanceof AndroidDriver androidDriver) {
            androidDriver.context(context);
        } else if (driver instanceof IOSDriver iosDriver) {
            iosDriver.context(context);
        } else {
            WebDriverElementActions.failAction(driver, context, null);
        }
        passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), context, null, null);
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
        if (identifyUniqueElementIgnoringVisibility(driver, elementLocator)) {
            var elementName = getElementName(driver, elementLocator);
            Boolean valueSetSuccessfully = ElementActionsHelper.setValueUsingJavascript(driver, elementLocator, value);
            if (Boolean.TRUE.equals(valueSetSuccessfully)) {
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), value, null, elementName);
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
        if (identifyUniqueElementIgnoringVisibility(driver, elementLocator)) {
            var elementName = getElementName(driver, elementLocator);
            try {
                ElementActionsHelper.submitFormUsingJavascript(driver, elementLocator);
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
            } catch (Exception rootCauseException) {
                ReportManagerHelper.log(rootCauseException);
                failAction(driver, elementLocator, rootCauseException);
            }
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
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
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
    public static WebDriverElementActions switchToDefaultContent() {
        if (DriverFactoryHelper.getDriver() !=null && (DriverFactoryHelper.getDriver().get() != null)) {
            try {
                DriverFactoryHelper.getDriver().get().switchTo().defaultContent();
                boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
                ReportManagerHelper.setDiscreteLogging(true);
                passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
                ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
            } catch (Exception e) {
                ReportManagerHelper.log(e);
            }
        }
        // if there is no last used driver or no drivers in the drivers list, do
        // nothing...
        return new WebDriverElementActions(DriverFactoryHelper.getDriver().get());
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
            driver.switchTo().frame(driver.findElement(elementLocator));
            // note to self: remove elementLocator in case of bug in screenshot manager
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(elementLocator), null, null);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
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
        var elementName = getElementName(driver, elementLocator);
        if (actualResult != null && actualResult.equals(text)) {
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), text, null, elementName);
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
        if (identifyUniqueElement(driver, elementLocator)
                && (text != null)) {
            var elementName = getElementName(driver, elementLocator);
            driver.findElement(elementLocator).sendKeys(text);
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), text, null, elementName);
        } else {
            failAction(driver, text, elementLocator);
        }
    }
    /**
     * ValidationEnums the required file path into an input[type='file'] button, to
     * successfully upload the target file.
     *
     * @param driver           the current instance of Selenium webdriver
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param absoluteFilePath the full path to the file that needs to be uploaded
     */
    public static void typeFileLocationForUpload(WebDriver driver, By elementLocator, String absoluteFilePath) {
        String internalAbsoluteFilePath = absoluteFilePath.replace("/", FileSystems.getDefault().getSeparator());
        if (identifyUniqueElementIgnoringVisibility(driver, elementLocator)) {
            var elementName = getElementName(driver, elementLocator);
            List<Object> screenshot = takeScreenshot(driver, elementLocator, "typeFileLocationForUpload", null, true);
            // takes screenshot before clicking the element out of view
            try {
                driver.findElement(elementLocator).sendKeys(internalAbsoluteFilePath);
            } catch (InvalidArgumentException e) {
                //this happens when the file path doesn't exist
                failAction(driver, internalAbsoluteFilePath, elementLocator, e);
            } catch (ElementNotInteractableException | NoSuchElementException exception1) {
                ElementActionsHelper.changeWebElementVisibilityUsingJavascript(driver, elementLocator, true);
                try {
                    driver.findElement(elementLocator).sendKeys(internalAbsoluteFilePath);
                } catch (WebDriverException rootCauseException) {
                    rootCauseException.initCause(exception1);
                    ReportManagerHelper.log(rootCauseException);
                    // happened for the first time on MacOSX due to incorrect file path separator
                    failAction(driver, internalAbsoluteFilePath, elementLocator, rootCauseException);
                }
                try {
                    ElementActionsHelper.changeWebElementVisibilityUsingJavascript(driver, elementLocator, false);
                } catch (NoSuchElementException | StaleElementReferenceException e) {
                    // this exception is sometimes thrown on firefox after the upload has been
                    // successful, since we don't have to return the style to what it was, then it's
                    // okay to do nothing here.
                    ReportManagerHelper.logDiscrete(e);
                }
            }
            passAction(driver, elementLocator, internalAbsoluteFilePath, screenshot, elementName);
        } else {
            failAction(driver, internalAbsoluteFilePath, elementLocator);
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
        var elementName = getElementName(driver, elementLocator);
        if (actualResult != null && actualResult.equals(text)) {
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), OBFUSCATED_STRING.repeat(text.length()), null, elementName);
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
        ReportManager.logDiscrete("Waiting for element to be present; elementLocator \"" + elementLocator + "\", numberOfTries\"" + numberOfTries + "\", stateOfPresence\"" + stateOfPresence + "\"...");
        String reportMessage = "waited for the element's state of presence to be (" + stateOfPresence
                + "). Element locator (" + elementLocator.toString() + ")";
        if (Boolean.compare(stateOfPresence, Integer.parseInt(getMatchingElementsInformation(driver, elementLocator, Optional.of(numberOfTries), Optional.empty()).get(0).toString()) >= 1) == 0) {
            if (Boolean.TRUE.equals(stateOfPresence)){
                //element is expected to be present
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, getElementName(driver, elementLocator));
            } else {
                //element is expected to be not present
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, null);
            }
        } else {
            failAction(driver, reportMessage, elementLocator);
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
    protected static void failAction(WebDriver driver, String testData, By elementLocator, List<List<Object>> attachments, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, elementLocator, attachments, rootCauseException);
    }
    protected static void failAction(Screen screen, App applicationWindow, Pattern element, String testData, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        List<List<Object>> attachments = new LinkedList<>();
        attachments.add(SikuliActions.prepareElementScreenshotAttachment(screen, applicationWindow, element, actionName, false));
        failAction(null, actionName, testData, null, attachments, rootCauseException);
    }
    protected static List<Object> identifyUniqueElement(WebDriver driver, By elementLocator) {
        return identifyUniqueElement(driver, elementLocator, true);
    }
    protected static List<Object> identifyUniqueElementIgnoringVisibility(WebDriver driver, By elementLocator) {
        return identifyUniqueElement(driver, elementLocator, false);
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
                ReportManagerHelper.log(e);
                ReportManager.log(
                        "Failed to take a screenshot of the element as it doesn't exist anymore. Taking a screenshot of the whole page.");
                return ScreenshotManager.captureScreenShot(driver, actionName, true);
            }
        } else {
            return ScreenshotManager.captureScreenShot(driver, actionName, false);
        }
        return new ArrayList<>();
    }
    private static void clearBeforeTyping(WebDriver driver, By elementLocator,
                                          TextDetectionStrategy successfulTextLocationStrategy) {
        try {
            // attempt clear using clear
            driver.findElement(elementLocator).clear();
//
//            // attempt clear using sendKeys
//            if (!elementText.trim().equals("")) {
//                driver.findElement(elementLocator).sendKeys("");
//            }
//            elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
//                    successfulTextLocationStrategy);
//
//            // attempt clear using javascript
//            if (!elementText.trim().equals("")) {
//            	ElementActionsHelper.setValueUsingJavascript(driver, elementLocator, "");
//            }
//
//            elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
//                    successfulTextLocationStrategy);
            // attempt clear using letter by letter backspace
            if (ATTEMPT_CLEAR_BEFORE_TYPING_USING_BACKSPACE) {
                String elementText = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
                        successfulTextLocationStrategy);
                for (var character : elementText.toCharArray()) {
                    driver.findElement(elementLocator).sendKeys(Keys.BACK_SPACE);
                }
            }
        } catch (InvalidElementStateException e) {
            // this was seen in case of attempting to type in an invalid element (an image)
            ReportManagerHelper.log(e);
        }
    }
    private static String confirmTypingWasSuccessful(WebDriver driver, By elementLocator,
                                                     TextDetectionStrategy successfulTextLocationStrategy) {
        TextDetectionStrategy updatedSuccessfulTextLocationStrategy = successfulTextLocationStrategy;
        if (updatedSuccessfulTextLocationStrategy.equals(TextDetectionStrategy.UNDEFINED)) {
            updatedSuccessfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(driver,
                    elementLocator);
        }
        return readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator,
                updatedSuccessfulTextLocationStrategy);
    }
    private static TextDetectionStrategy determineSuccessfulTextLocationStrategy(WebDriver driver, By elementLocator) {
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            return TextDetectionStrategy.TEXT;
        }
        String text = driver.findElement(elementLocator).getText();
        // fixing https://github.com/ShaftHQ/SHAFT_ENGINE/issues/533
        String content = "";
        try {
            content = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.CONTENT.getValue());
        } catch (Exception exception) {
            // ignore exception
        }
        String value = "";
        try {
            value = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.VALUE.getValue());
        } catch (Exception exception) {
            // ignore exception
        }
        TextDetectionStrategy successfulTextLocationStrategy;
        if (text != null && !"".equals(text.trim())) {
            successfulTextLocationStrategy = TextDetectionStrategy.TEXT;
        } else if (content != null && !"".equals(content.trim())) {
            successfulTextLocationStrategy = TextDetectionStrategy.CONTENT;
        } else if (value != null && !"".equals(value.trim())) {
            successfulTextLocationStrategy = TextDetectionStrategy.VALUE;
        } else {
            successfulTextLocationStrategy = TextDetectionStrategy.UNDEFINED;
        }
        return successfulTextLocationStrategy;
    }
    private static void failAction(WebDriver driver, String actionName, String testData, By elementLocator, List<List<Object>> screenshots,
                                   Exception... rootCauseException) {
        //TODO: merge all fail actions, make all methods call this one, get elementName where applicable instead of reporting null
        String message = reportActionResult(driver, actionName, testData, elementLocator, screenshots, null, false);
        if (rootCauseException != null && rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
    }
    private static List<Object> getMatchingElementsInformation(WebDriver driver, By elementLocator, Optional<Integer> numberOfAttempts, Optional<Boolean> checkForVisibility) {
        if (elementLocator == null) {
            var elementInformation = new ArrayList<>();
            elementInformation.add(0);
            elementInformation.add(null);
            return elementInformation;
        }
        JavaScriptWaitManager.waitForLazyLoading();
        if (!elementLocator.equals(By.tagName("html"))) {
            if (numberOfAttempts.isEmpty() && checkForVisibility.isEmpty()) {
                return ElementActionsHelper.waitForElementPresence(driver, elementLocator);
            } else if (numberOfAttempts.isPresent() && checkForVisibility.isEmpty()) {
                return ElementActionsHelper.waitForElementPresence(driver, elementLocator, numberOfAttempts.get());
            } else if (numberOfAttempts.isEmpty()) {
                return ElementActionsHelper.waitForElementPresence(driver, elementLocator, checkForVisibility.get());
            } else {
                return ElementActionsHelper.waitForElementPresence(driver, elementLocator, numberOfAttempts.get(), checkForVisibility.get());
            }
        }else{
            //if locator is just tagname html
            var elementInformation = new ArrayList<>();
            elementInformation.add(1);
            elementInformation.add(null);
            return elementInformation;
        }
    }
    private static List<Object> identifyUniqueElement(WebDriver driver, By elementLocator,
                                                 boolean checkForVisibility) {
        var matchingElementsInformation = getMatchingElementsInformation(driver, elementLocator, Optional.empty(), Optional.of(checkForVisibility));

            if (elementLocator != null) {
                if (!(elementLocator instanceof RelativeLocator.RelativeBy)) {
                    // in case of regular locator
                    switch (Integer.parseInt(matchingElementsInformation.get(0).toString())) {
                        case 0 -> failAction(driver, "zero elements found matching this locator \"" + elementLocator + "\".", elementLocator);
                        case 1 -> {
                            return matchingElementsInformation;
                        }
                        default -> {
                            if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckElementLocatorIsUnique")))) {
                                failAction(driver, "multiple elements found matching this locator \"" + elementLocator + "\".",
                                        elementLocator);
                            }
                            return matchingElementsInformation;
                        }
                    }
                }
                //in case of relativeLocator
                return matchingElementsInformation;
            } else {
                // in case locator is null
                failAction(driver, "element locator is NULL.", null);
            }
            //unreachable code
        return matchingElementsInformation;
    }
    protected static void passAction(WebDriver driver, By elementLocator, String testData, List<Object> screenshot, String elementName) {
        //TODO: open calling methods, and test if Appium can also fetch the element name instead of passing null
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        List<List<Object>> attachments = new LinkedList<>();
        attachments.add(screenshot);
        passAction(driver, elementLocator, actionName, testData, attachments, elementName);
    }
    protected static void passAction(Screen screen, App applicationWindow, Pattern element, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        List<List<Object>> attachments = new LinkedList<>();
        attachments.add(SikuliActions.prepareElementScreenshotAttachment(screen, applicationWindow, element, actionName, true));
        passAction(null, null, actionName, testData, attachments, null);
    }
    protected static void passAction(WebDriver driver, By elementLocator, String actionName, String testData,
                                   List<List<Object>> screenshots, String elementName) {
        reportActionResult(driver, actionName, testData, elementLocator, screenshots, elementName, true);
    }
    private static void pasteFromClipboard(WebDriver driver, By elementLocator) {
        try {
            typeAppend(driver, elementLocator,
                    (String) ((Toolkit.getDefaultToolkit().getSystemClipboard()).getContents(WebDriverElementActions.class))
                            .getTransferData(DataFlavor.stringFlavor));
        } catch (UnsupportedFlavorException e) {
            ReportManagerHelper.log(e);
            ReportManager.log("Unsupported Flavor Exception: " + e.getMessage());
        } catch (IOException e) {
            ReportManagerHelper.log(e);
            ReportManager.log("IO Exception: " + e.getMessage());
        }
    }
    private static boolean performClipboardActions(WebDriver driver, By elementLocator, String action, Keys CommandOrControl) {
        try {
            switch (action.toLowerCase()) {
                case "copy":
                    (new Actions(driver)).sendKeys(Keys.chord(CommandOrControl, "c")).perform();
                    break;
                case "paste":
                    (new Actions(driver)).sendKeys(Keys.chord(CommandOrControl, "v")).perform();
                    break;
                case "cut":
                    (new Actions(driver)).sendKeys(Keys.chord(CommandOrControl, "x")).perform();
//                    type(driver, elementLocator, "");
                    break;
                case "select all":
                    (new Actions(driver)).sendKeys(Keys.chord(CommandOrControl, "a")).perform();
                    break;
                case "unselect":
                    (new Actions(driver)).sendKeys(Keys.ESCAPE).perform();
                    break;
                default:
                    return false;
            }
            return true;
        } catch (HeadlessException e) {
            ReportManagerHelper.log(e);
            return false;
        }
    }
    private static void performType(WebDriver driver, By elementLocator, String text) {
        ArrayList<Class<? extends Exception>> expectedExceptions = new ArrayList<>();
        expectedExceptions.add(StaleElementReferenceException.class);
        expectedExceptions.add(ElementNotInteractableException.class);
        expectedExceptions.add(UnreachableBrowserException.class);
        expectedExceptions.add(NoSuchElementException.class);
        expectedExceptions.add(WebDriverException.class);
        try {
            new FluentWait<>(driver)
                    .withTimeout(Duration.ofSeconds(5))
                    .pollingEvery(Duration.ofSeconds(1))
                    .ignoreAll(expectedExceptions)
                    .until(nestedDriver -> {
                        nestedDriver.findElement(elementLocator).sendKeys(text);
                        return true;
                    });
        } catch (TimeoutException e) {
            // In case typing failed and the timeout expired
            ReportManagerHelper.log(e);
        }
    }
    private static String readTextBasedOnSuccessfulLocationStrategy(WebDriver driver, By elementLocator,
                                                                    TextDetectionStrategy successfulTextLocationStrategy) {
        String temp;
        switch (successfulTextLocationStrategy) {
            case TEXT -> {
                temp = driver.findElement(elementLocator).getText();
                return (temp == null) ? "" : temp;
            }
            case CONTENT -> {
                temp = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.CONTENT.getValue());
                return (temp == null) ? "" : temp;
            }
            case VALUE -> {
                temp = driver.findElement(elementLocator).getAttribute(TextDetectionStrategy.VALUE.getValue());
                return (temp == null) ? "" : temp;
            }
        }
        return "";
    }
    private static String reportActionResult(WebDriver driver, String actionName, String testData, By elementLocator,
                                             List<List<Object>> screenshots, String elementName, Boolean passFailStatus) {
        actionName = JavaActions.convertToSentenceCase(actionName);
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "Element Action: " + actionName;
        } else {
            message = "Element Action: " + actionName + " failed";
        }
        List<List<Object>> attachments = new ArrayList<>();

        if (testData != null && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Element Action Test Data - " + actionName,
                    "Actual Value", testData);
            attachments.add(actualValueAttachment);
        } else if ((testData != null && !testData.isEmpty())) {
            message = message + " \"" + testData.trim() + "\"";
        }

        if ((elementName != null && !elementName.isEmpty())) {
            var preposition = " ";
            if (actionName.toLowerCase().contains("type") || actionName.toLowerCase().contains("set value using javascript")){
                preposition = " into ";
            }else if (actionName.toLowerCase().contains("get") || actionName.toLowerCase().contains("select")){
                preposition = " from ";
            }else if (actionName.toLowerCase().contains("clipboard")){
                preposition = " on ";
            } else if (actionName.toLowerCase().contains("drag and drop") || actionName.toLowerCase().contains("key press") || actionName.toLowerCase().contains("wait") || actionName.toLowerCase().contains("submit")|| actionName.toLowerCase().contains("switch")){
                preposition = " against ";
            } else if (actionName.toLowerCase().contains("hover")){
                preposition = " over ";
            }
            message = message + preposition + " \"" + elementName.trim() + "\"";
        }

        message = message + ".";
        if (screenshots != null && !screenshots.equals(new ArrayList<>())) {
            // screenshot taken before action (in case of click)
            attachments.addAll(screenshots);
        } else if (driver != null) {
            List<Object> newScreenshot = takeScreenshot(driver, elementLocator, actionName, testData, passFailStatus);
            if (newScreenshot != null && !newScreenshot.equals(new ArrayList<>())) {
                attachments.add(newScreenshot);
            }
        }
//        ReportManager.logDiscrete(message);
        message = message.replace("Element Action: ", "");
        if (!attachments.equals(new ArrayList<>())) {
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.log(message);
        }
        return message;
    }
    private static String typeWrapper(WebDriver driver, By elementLocator, String targetText) {
        if (identifyUniqueElement(driver, elementLocator)) {
            TextDetectionStrategy successfulTextLocationStrategy = TextDetectionStrategy.UNDEFINED;
            if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckTextWasTypedCorrectly")))) {
                successfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(driver,
                        elementLocator);
            }
//            if (!successfulTextLocationStrategy.equals(TextDetectionStrategy.UNDEFINED)) {
            clearBeforeTyping(driver, elementLocator, successfulTextLocationStrategy);
//            }
            if (!"".equals(targetText)) {
                performType(driver, elementLocator, targetText);
            }
            if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckTextWasTypedCorrectly")))) {
                String actualText = confirmTypingWasSuccessful(driver, elementLocator, successfulTextLocationStrategy);
                if (targetText.equals(actualText) || OBFUSCATED_STRING.repeat(targetText.length()).equals(actualText)) {
                    return targetText;
                } else {
                    // attempt once to type using javascript then confirm typing was successful
                    // again
                    ElementActionsHelper.setValueUsingJavascript(driver, elementLocator, targetText);
                    var textAfterSettingValueUsingJavascript = readTextBasedOnSuccessfulLocationStrategy(driver, elementLocator, TextDetectionStrategy.VALUE);
                    if ("".equals(textAfterSettingValueUsingJavascript) && successfulTextLocationStrategy.equals(TextDetectionStrategy.UNDEFINED)) {
                        return targetText;
                    }
                    return textAfterSettingValueUsingJavascript;
                }
            } else {
                return targetText;
            }
        } else {
            ReportManager.log("Failed to identify Target element with locator \"" + elementLocator + "\".");
            return null;
        }
    }
    public enum TextDetectionStrategy {
        TEXT("text"), CONTENT("textContent"), VALUE("value"), UNDEFINED("undefined");
        private final String value;
        TextDetectionStrategy(String strategy) {
            this.value = strategy;
        }
        String getValue() {
            return value;
        }
    }
}
