package com.shaft.gui.element;

import com.shaft.cli.FileActions;
import com.shaft.tools.io.ReportManager;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.github.shafthq.shaft.driver.DriverFactoryHelper;
import io.github.shafthq.shaft.gui.browser.FluentBrowserActions;
import io.github.shafthq.shaft.gui.element.ElementActionsHelper;
import io.github.shafthq.shaft.gui.element.FluentElementActions;
import io.github.shafthq.shaft.tools.io.helpers.ReportManagerHelper;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.UnexpectedTagNameException;
import org.sikuli.script.App;

import java.nio.file.FileSystems;
import java.util.*;

import static io.github.shafthq.shaft.gui.element.ElementActionsHelper.*;

//TODO: Move body of implementation into the Fluent Actions class to fix internal "Deprecated member is still used" warnings
public class ElementActions extends FluentElementActions {

    public ElementActions() {
        new FluentElementActions();
    }

    public ElementActions(WebDriver driver) {
        new FluentElementActions();
    }

    @Deprecated
    public static FluentElementActions performElementAction(WebDriver driver) {
        return new FluentElementActions();
    }

    @Deprecated
    public static FluentBrowserActions performBrowserAction(WebDriver driver) {
        return new FluentBrowserActions();
    }

    @Deprecated
    public static SikuliActions performSikuliAction() {
        return new SikuliActions();
    }

    @Deprecated
    public static SikuliActions performSikuliAction(App applicationWindow) {
        return new SikuliActions(applicationWindow);
    }

    @Deprecated
    public static TouchActions performTouchAction(WebDriver driver) {
        return new TouchActions();
    }

    @Deprecated
    public static AlertActions performAlertAction(WebDriver driver) {
        return new AlertActions();
    }

    /**
     * Clicks on a certain element using Selenium WebDriver, or JavaScript
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    @Deprecated
    public static void click(WebDriver driver, By elementLocator) {
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            new TouchActions(driver).tap(elementLocator);
        } else {
            // Waits for the element to be clickable, and then clicks it.
            try {
                var elementName = getElementName(driver, elementLocator);
                try {
                    // adding hover before clicking an element to enable styles to show in the
                    // execution screenshots and to solve issues clicking on certain elements.
                    (new Actions(driver)).moveToElement(((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1))).perform();
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
                    ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).click();
                } catch (Exception exception1) {
                    var clickUsingJavascriptWhenWebDriverClickFails = Boolean.parseBoolean(System.getProperty("clickUsingJavascriptWhenWebDriverClickFails"));
                    if (clickUsingJavascriptWhenWebDriverClickFails) {
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
                passAction(driver, elementLocator, "", screenshot, elementName);
            } catch (Throwable throwable) {
                failAction(driver, elementLocator, throwable);
            }
        }
    }

    /**
     * If the element is outside the viewport, scrolls the bottom of the element to the bottom of the viewport.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    @Deprecated
    public static void scrollToElement(WebDriver driver, By elementLocator) {
        // if mobile, call swipeElementIntoView(null, targetElementLocator, swipeDirection); for convenience
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            performTouchAction(driver).swipeElementIntoView(elementLocator, TouchActions.SwipeDirection.DOWN);
        }
        try {
            ElementActionsHelper.scrollToFindElement(driver, elementLocator);
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, getElementName(driver, elementLocator));
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
        }
    }

    /**
     * Waits for the element to be clickable, and then clicks and holds it.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    @Deprecated
    public static void clickAndHold(WebDriver driver, By elementLocator) {
        try {
            var elementName = getElementName(driver, elementLocator);
            if (Boolean.FALSE.equals(ElementActionsHelper.waitForElementToBeClickable(driver, elementLocator))) {
                failAction(driver, "element is not clickable", elementLocator);
            }
            // wait for element to be clickable
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
            (new Actions(driver)).clickAndHold(((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1))).build().perform();
            // takes screenshot before holding the element
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
        }
    }

    /**
     * Attempts to perform a native clipboard action on the text from a certain web
     * element, like copy/cut/paste
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param action         supports the following actions "copy", "paste", "cut",
     *                       "select all", "unselect"
     */
    @Deprecated
    public static void clipboardActions(WebDriver driver, By elementLocator, String action) {
        // TODO: implement enum for list of possible actions
        try {
            var elementName = getElementName(driver, elementLocator);
            boolean wasActionPerformed;
            if (System.getProperty("targetOperatingSystem").contains("Mac")) {
                wasActionPerformed = ElementActionsHelper.performClipboardActions(driver, elementLocator, action, Keys.COMMAND);
            } else {
                wasActionPerformed = ElementActionsHelper.performClipboardActions(driver, elementLocator, action, Keys.CONTROL);
            }
            if (Boolean.TRUE.equals(wasActionPerformed)) {
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), action, null, elementName);
            } else {
                failAction(driver, action, elementLocator);
            }
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
        }
    }

    /**
     * Double-clicks on an element using Selenium WebDriver's Actions Library
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    @Deprecated
    public static void doubleClick(WebDriver driver, By elementLocator) {
        try {
            var elementName = getElementName(driver, elementLocator);
            // takes screenshot before clicking the element out of view
            var screenshot = ElementActionsHelper.takeScreenshot(driver, elementLocator, "doubleClick", null, true);
            List<List<Object>> attachments = new LinkedList<>();
            attachments.add(screenshot);
            try {
                (new Actions(driver)).moveToElement(((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1))).doubleClick().perform();
            } catch (Exception e) {
                failAction(driver, elementLocator, e);
            }
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, attachments, elementName);
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
        }
    }

    /**
     * Drags the source element and drops it onto the destination element using
     * javascript
     *
     * @param driver                    the current instance of Selenium WebDriver
     * @param sourceElementLocator      the locator of the source webElement that
     *                                  should be dragged under test (By xpath, id,
     *                                  selector, name ...etc)
     * @param destinationElementLocator the locator of the target webElement that
     *                                  should receive the dropped source element
     *                                  under test (By xpath, id, selector, name
     *                                  ...etc)
     */
    @Deprecated
    public static void dragAndDrop(WebDriver driver, By sourceElementLocator, By destinationElementLocator) {
        try {
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
        } catch (Throwable throwable) {
            failAction(driver, sourceElementLocator, throwable);
        }
    }

    /**
     * Drags the source element and drops it onto the determined offset
     *
     * @param driver               the current instance of Selenium WebDriver
     * @param sourceElementLocator the locator of the source webElement that should
     *                             be dragged under test (By xpath, id, selector,
     *                             name ...etc)
     * @param xOffset              the horizontal offset by which the element should
     *                             be moved
     * @param yOffset              the vertical offset by which the element should
     *                             be moved
     */
    @Deprecated
    public static void dragAndDropByOffset(WebDriver driver, By sourceElementLocator, int xOffset, int yOffset) {
        try {
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
        } catch (Throwable throwable) {
            failAction(driver, sourceElementLocator, throwable);
        }
    }

    /**
     * This is a generic method to enable the execution of the native mobile
     * commands found herein: <a href="http://appium.io/docs/en/commands/mobile-command/">appium.io</a>
     * <p>
     * Note: This method does no validation on the output of the executed JavaScript
     *
     * @param driver     the current instance of Selenium WebDriver, which should
     *                   wrap around a native mobile object
     * @param command    the desired mobile command to be executed. e.g., "mobile:
     *                   scroll"
     * @param parameters a map of the key, value parameters for this command. e.g.,
     *                   ImmutableMap.of("direction", "down")
     */
    @Deprecated
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
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param attributeName  the target attribute of the webElement under test
     * @return the value of the target attribute of the webElement under test
     */
    @Deprecated
    public static String getAttribute(WebDriver driver, By elementLocator, String attributeName) {
        ReportManager.logDiscrete("Attempting to getAttribute \"" + attributeName + "\" from elementLocator \"" + elementLocator + "\".");
        try {
            var elementName = getElementName(driver, elementLocator);
            try {
                String elementAttribute = ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).getAttribute(attributeName);
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementAttribute, null, elementName);
                return elementAttribute;
            } catch (UnsupportedCommandException rootCauseException) {
                failAction(driver, elementLocator, rootCauseException);
                return null;
            }
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
            //unreachable code
            return null;
        }
    }

    /**
     * Get the value of a given CSS property. Color values should be returned as
     * RGBA strings, so, for example if the "background-color" property is SetProperty as
     * "green" in the HTML source, the returned value will be "RGBA(0, 255, 0, 1)".
     * Note that shorthand CSS properties (e.g. background, font, border,
     * border-top, margin, margin-top, padding, padding-top, list-style, outline,
     * pause, cue) are not returned, in accordance with the DOM CSS2 specification -
     * you should directly access the longhand properties (e.g. background-color) to
     * access the desired values.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param propertyName   the target CSS property of the webElement under test
     * @return the value of the target CSS property of the webElement under test
     */
    @Deprecated
    public static String getCSSProperty(WebDriver driver, By elementLocator, String propertyName) {
        try {
            var elementName = getElementName(driver, elementLocator);
            String elementCssProperty = ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).getCssValue(propertyName);
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementCssProperty, null, elementName);
            return elementCssProperty;
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
            //unreachable code
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
    @Deprecated
    public static String getContext(WebDriver driver) {
        String context = "";
        if (driver instanceof AndroidDriver androidDriver) {
            context = androidDriver.getContext();
        } else if (driver instanceof IOSDriver iosDriver) {
            context = iosDriver.getContext();
        } else {
            failAction(driver, null);
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
    @Deprecated
    public static List<String> getContextHandles(WebDriver driver) {
        List<String> windowHandles = new ArrayList<>();
        if (driver instanceof AndroidDriver androidDriver) {
            windowHandles.addAll(androidDriver.getContextHandles());
        } else if (driver instanceof IOSDriver iosDriver) {
            windowHandles.addAll(iosDriver.getContextHandles());
        } else {
            failAction(driver, null);
        }
        passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(windowHandles), null, null);
        return windowHandles;
    }

    /**
     * Retrieves the selected text from the target drop-down list element and
     * returns it as a string value.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the selected text of the target webElement
     */
    @Deprecated
    public static String getSelectedText(WebDriver driver, By elementLocator) {
        try {
            var elementName = getElementName(driver, elementLocator);
            StringBuilder elementSelectedText = new StringBuilder();
            try {
                new Select(((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1))).getAllSelectedOptions().forEach(selectedOption -> elementSelectedText.append(selectedOption.getText()));
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementSelectedText.toString().trim(), null, elementName);
                return elementSelectedText.toString().trim();
            } catch (UnexpectedTagNameException rootCauseException) {
                failAction(driver, elementLocator, rootCauseException);
                return null;
            }
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
            //unreachable code
            return null;
        }
    }

    /**
     * Retrieves element size from the target element and returns it as a string
     * value.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the size of the webElement under test
     */
    @Deprecated
    public static String getSize(WebDriver driver, By elementLocator) {
        try {
            var elementName = getElementName(driver, elementLocator);
            String elementSize = ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).getSize().toString();
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementSize, null, elementName);
            return elementSize;
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
            //unreachable code
            return null;
        }
    }

    /**
     * Retrieves tag name from the target element and returns it as a string value.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the tag name of the webElement under test
     */
    @Deprecated
    public static String getTagName(WebDriver driver, By elementLocator) {
        try {
            var elementName = getElementName(driver, elementLocator);
            String elementTagName = ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).getTagName();
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementTagName, null, elementName);
            return elementTagName;
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
            //unreachable code
            return null;
        }
    }

    /**
     * Retrieves text from the target element and returns it as a string value.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the text value of the target webElement
     */
    @Deprecated
    public static String getText(WebDriver driver, By elementLocator) {
        try {
            var elementName = getElementName(driver, elementLocator);
            String elementText = ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).getText();
            if ((elementText == null || elementText.trim().equals("")) && !DriverFactoryHelper.isMobileNativeExecution()) {
                elementText = ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).getAttribute(ElementActionsHelper.TextDetectionStrategy.CONTENT.getValue());
            }
            if ((elementText == null || elementText.trim().equals("")) && !DriverFactoryHelper.isMobileNativeExecution()) {
                elementText = ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).getAttribute(ElementActionsHelper.TextDetectionStrategy.VALUE.getValue());
            }
            if (elementText == null) {
                elementText = "";
            }
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementText, null, elementName);
            return elementText;
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
            //unreachable code
            return null;
        }
    }

    /**
     * Returns the unique handle for currently active window. This can be used to
     * switch to this window at a later time.
     *
     * @param driver the current instance of Selenium WebDriver
     * @return window handle
     */
    @Deprecated
    public static String getWindowHandle(WebDriver driver) {
        String nameOrHandle = driver.getWindowHandle();
        passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), nameOrHandle, null, null);
        return nameOrHandle;
    }

    /**
     * Returns a list of unique handles for all the currently open windows. This can
     * be used to switch to any of these windows at a later time.
     *
     * @param driver the current instance of Selenium WebDriver
     * @return list of window handles
     */
    @Deprecated
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
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    @Deprecated
    public static void hover(WebDriver driver, By elementLocator) {
        try {
            var elementName = getElementName(driver, elementLocator);
            try {
                (new Actions(driver)).moveToElement(((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1))).perform();
            } catch (Exception rootCauseException) {
                ReportManagerHelper.log(rootCauseException);
                failAction(driver, elementLocator, rootCauseException);
            }
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
        }
    }

    /**
     * Hovers over the hoverElements in sequence then clicks the clickableElement
     *
     * @param driver                  the current instance of Selenium WebDriver
     * @param hoverElementLocators    the list of locators of the webElements under
     *                                test upon which the hover action will be
     *                                performed in sequence (By xpath, id, selector,
     *                                name ...etc)
     * @param clickableElementLocator the locator of the webElement under test upon
     *                                which the click action will be performed (By
     *                                xpath, id, selector, name ...etc)
     */
    @Deprecated
    public static void hoverAndClick(WebDriver driver, List<By> hoverElementLocators, By clickableElementLocator) {
        hoverElementLocators.forEach(hoverElementLocator -> ElementActions.hover(driver, hoverElementLocator));
        ElementActions.click(driver, clickableElementLocator);
    }

    /**
     * Hovers over the hoverElement then clicks the clickableElement
     *
     * @param driver                  the current instance of Selenium WebDriver
     * @param hoverElementLocator     the locator of the webElement under test upon
     *                                which the hover action will be performed (By
     *                                xpath, id, selector, name ...etc)
     * @param clickableElementLocator the locator of the webElement under test upon
     *                                which the click action will be performed (By
     *                                xpath, id, selector, name ...etc)
     */
    @Deprecated
    public static void hoverAndClick(WebDriver driver, By hoverElementLocator, By clickableElementLocator) {
        hoverAndClick(driver, Collections.singletonList(hoverElementLocator), clickableElementLocator);
    }

    /**
     * Checks to see if an element is clickable
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return boolean value, true if the element is clickable, and false if the
     * element is not clickable
     */
    @Deprecated
    public static boolean isElementClickable(WebDriver driver, By elementLocator) {
        try {
            var elementName = getElementName(driver, elementLocator);
            if (ElementActionsHelper.waitForElementToBeClickable(driver, elementLocator)) {
                //element is clickable
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "element is clickable", null, elementName);
                return true;
            } else {
                //element is not clickable
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "element is not clickable", null, elementName);
                return false;
            }
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
            //unreachable code
            return false;
        }
    }

    /**
     * Checks to see if an element is displayed
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return boolean value, true if the element is displayed, and false if the
     * element is not displayed
     */
    @Deprecated
    public static boolean isElementDisplayed(WebDriver driver, By elementLocator) {
        try {
            var elementName = getElementName(driver, elementLocator);
            boolean isDisplayed = ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).isDisplayed();
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
            return isDisplayed;
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
            //unreachable code
            return false;
        }
    }

    /**
     * Sends a key-press to the target element. Supported keys are: ENTER, RETURN,
     * TAB
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param key            the key that should be pressed
     */
    @Deprecated
    public static void keyPress(WebDriver driver, By elementLocator, String key) {
        try {
            var elementName = getElementName(driver, elementLocator);
            switch (key.toLowerCase().trim()) {
                case "enter" ->
                        ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).sendKeys(Keys.ENTER);
                case "return" ->
                        ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).sendKeys(Keys.RETURN);
                case "tab" ->
                        ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).sendKeys(Keys.TAB);
                default -> {
                    ReportManager.log("Unsupported Key.");
                    failAction(driver, key, elementLocator);
                }
            }
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), key, null, elementName);
        } catch (Throwable throwable) {
            failAction(driver, key, elementLocator, throwable);
        }
    }

    /**
     * Sends a key-press to the target element.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param key            the key that should be pressed
     */
    @Deprecated
    public static void keyPress(WebDriver driver, By elementLocator, Keys key) {
        try {
            var elementName = getElementName(driver, elementLocator);
            List<Object> screenshot = takeScreenshot(driver, elementLocator, "keyPress", null, true);
            // takes screenshot before moving the element out of view
            ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).sendKeys(key);
            passAction(driver, elementLocator, key.name(), screenshot, elementName);
        } catch (Throwable throwable) {
            failAction(driver, key.name(), elementLocator, throwable);
        }
    }

    /**
     * Waits dynamically for a specific element's text to change from the initial
     * value to a new unknown value. Waits until the default element identification timeout
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param initialValue   the initial text value of the target webElement
     */
    @Deprecated
    public static void waitForTextToChange(WebDriver driver, By elementLocator, String initialValue) {
        try {
            var elementName = getElementName(driver, elementLocator);
            if (!Boolean.TRUE.equals(ElementActionsHelper.waitForElementTextToBeNot(driver, elementLocator, initialValue))) {
                failAction(driver, initialValue, elementLocator);
            }
            try {
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "from: \"" + initialValue + "\", to: \"" + getText(driver, elementLocator) + "\"", null, elementName);
            } catch (Exception e) {
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "from: \"" + initialValue + "\", to a new value.", null, elementName);
            }
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
        }
    }

    /**
     * Selects an element from a dropdown list using its displayed text
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the text of the choice that you need to select from the
     *                       target dropDown menu
     */
    @Deprecated
    public static void select(WebDriver driver, By elementLocator, String text) {
        try {
            var elementName = getElementName(driver, elementLocator);
            //add forced check that the select element actually has options and is not empty
            if (!Boolean.TRUE.equals(ElementActionsHelper.waitForElementTextToBeNot(driver, elementLocator, ""))) {
                failAction(driver, text, elementLocator);
            }
            boolean isOptionFound = false;
            var availableOptionsList = (new Select(((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)))).getOptions();
            for (int i = 0; i < availableOptionsList.size(); i++) {
                String visibleText = availableOptionsList.get(i).getText();
                String value = availableOptionsList.get(i).getAttribute("value");
                if (visibleText.trim().equals(text) || value.trim().equals(text)) {
                    (new Select(((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)))).selectByIndex(i);
                    passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), text, null, elementName);
                    isOptionFound = true;
                    break;
                }
            }
            if (Boolean.FALSE.equals(isOptionFound)) {
                failAction(driver, text, elementLocator);
            }
        } catch (Throwable throwable) {
            failAction(driver, text, elementLocator, throwable);
        }
    }

    /**
     * Switches focus to another context
     *
     * @param driver  the current instance of Appium Driver
     * @param context The name of the context or the handle as returned by
     *                ElementActions.getContext(WebDriver driver)
     */
    @Deprecated
    public static void setContext(WebDriver driver, String context) {
        if (driver instanceof AndroidDriver androidDriver) {
            androidDriver.context(context);
        } else if (driver instanceof IOSDriver iosDriver) {
            iosDriver.context(context);
        } else {
            failAction(driver, context, null);
        }
        passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), context, null, null);
    }

    /**
     * Used to SetProperty value for an element (hidden or visible) using javascript
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param value          the desired value that should be SetProperty for the target
     *                       element
     */
    @Deprecated
    public static void setValueUsingJavaScript(WebDriver driver, By elementLocator, String value) {
        try {
            var elementName = getElementName(driver, elementLocator);
            Boolean valueSetSuccessfully = ElementActionsHelper.setValueUsingJavascript(driver, elementLocator, value);
            if (Boolean.TRUE.equals(valueSetSuccessfully)) {
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), value, null, elementName);
            } else {
                failAction(driver, elementLocator);
            }
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
        }
    }

    /**
     * Used to submit a form using javascript
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     */
    @Deprecated
    public static void submitFormUsingJavaScript(WebDriver driver, By elementLocator) {
        try {
            var elementName = getElementName(driver, elementLocator);
            try {
                ElementActionsHelper.submitFormUsingJavascript(driver, elementLocator);
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
            } catch (Exception rootCauseException) {
                ReportManagerHelper.log(rootCauseException);
                failAction(driver, elementLocator, rootCauseException);
            }
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
        }
    }

    /**
     * Switches focus to default content, is mainly used in coordination with
     * {@link #switchToIframe(WebDriver, By)} to exit any iFrame layer and go back
     * to the main page
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void switchToDefaultContent(WebDriver driver) {
        try {
            driver.switchTo().defaultContent();
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (Exception rootCauseException) {
//                ReportManagerHelper.log(rootCauseException);
            failAction(driver, null, rootCauseException);
        }
    }

    /**
     * Switches focus to a certain iFrame, is mainly used in coordination with
     * {@link #switchToDefaultContent(WebDriver)} to navigate inside any iFrame
     * layer and go back to the main page
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the iFrame webElement under test (By
     *                       xpath, id, selector, name ...etc)
     */
    @Deprecated
    public static void switchToIframe(WebDriver driver, By elementLocator) {
        try {
            driver.switchTo().frame(((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)));
            // note to self: remove elementLocator in case of bug in screenshot manager
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(elementLocator), null, null);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
        }
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     */
    @Deprecated
    public static void type(WebDriver driver, By elementLocator, String text) {
        try {
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
        } catch (Throwable throwable) {
            failAction(driver, elementLocator, throwable);
        }
    }

    /**
     * Appends the required string into the target element, regardless of the
     * current text value.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be appended into the
     *                       target webElement
     */
    @Deprecated
    public static void typeAppend(WebDriver driver, By elementLocator, String text) {
        try {
            if (text != null) {
                var elementName = getElementName(driver, elementLocator);
                ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).sendKeys(text);
                passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), text, null, elementName);
            }
        } catch (Throwable throwable) {
            failAction(driver, text, elementLocator, throwable);
        }
    }

    /**
     * ValidationEnums the required file path into an input[type='file'] button, to
     * successfully upload the target file.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc)
     * @param filePath       the full path to the file that needs to be uploaded, it can be absolute or relative
     *                       path, Engine will detect that
     */
    @Deprecated
    public static void typeFileLocationForUpload(WebDriver driver, By elementLocator, String filePath) {
        var absoluteFilePath = filePath;
        if (filePath.startsWith("src")) {
            absoluteFilePath = FileActions.getInstance().getAbsolutePath(filePath);
        }

        String internalAbsoluteFilePath = absoluteFilePath.replace("/", FileSystems.getDefault().getSeparator());
        try {
            var elementName = getElementName(driver, elementLocator);
            List<Object> screenshot = takeScreenshot(driver, elementLocator, "typeFileLocationForUpload", null, true);
            // takes screenshot before clicking the element out of view
            try {
                ((WebElement) ElementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).sendKeys(internalAbsoluteFilePath);
            } catch (InvalidArgumentException e) {
                //this happens when the file path doesn't exist
                failAction(driver, internalAbsoluteFilePath, elementLocator, e);
            } catch (ElementNotInteractableException | NoSuchElementException exception1) {
                ElementActionsHelper.changeWebElementVisibilityUsingJavascript(driver, elementLocator, true);
                try {
                    ((WebElement) ElementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).sendKeys(internalAbsoluteFilePath);
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
        } catch (Throwable throwable) {
            failAction(driver, internalAbsoluteFilePath, elementLocator, throwable);
        }
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element. Obfuscates the written text in the output
     * report. This action should be used for writing passwords and secure text.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     */
    @Deprecated
    public static void typeSecure(WebDriver driver, By elementLocator, String text) {
        String actualResult = typeWrapper(driver, elementLocator, text);
        var elementName = getElementName(driver, elementLocator);
        if (actualResult != null && actualResult.equals(text)) {
            passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), ElementActionsHelper.OBFUSCATED_STRING.repeat(text.length()), null, elementName);
        } else if (actualResult == null) {
            failAction(driver, elementLocator);
        } else {
            failAction(driver, "Expected to type: \"" + text + "\", but ended up with: \""
                    + actualResult + "\"", elementLocator);
        }
    }

    /**
     * Waits dynamically for a specific element to be present in DOM, and ready to interact with, on the current page.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc)
     */
    @Deprecated
    public static void waitForElementToBePresent(WebDriver driver, By elementLocator, int numberOfTries,
                                                 boolean stateOfPresence) {
        ReportManager.logDiscrete("Waiting for element to be present; elementLocator \"" + elementLocator + "\", numberOfTries\"" + numberOfTries + "\", stateOfPresence\"" + stateOfPresence + "\"...");
        String reportMessage = "waited for the element's state of presence to be (" + stateOfPresence
                + "). Element locator (" + elementLocator.toString() + ")";
        if (Boolean.compare(stateOfPresence, Integer.parseInt(getMatchingElementsInformation(driver, elementLocator, Optional.of(numberOfTries), Optional.empty()).get(0).toString()) >= 1) == 0) {
            if (Boolean.TRUE.equals(stateOfPresence)) {
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

    /**
     * Waits dynamically for a specific element to be detached from DOM, or hidden, on the current page.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc)
     */
    @Deprecated
    public static void waitForElementToBeInvisible(WebDriver driver, By elementLocator) {
        waitForElementToBePresent(driver, elementLocator, 1, false);
    }
}
