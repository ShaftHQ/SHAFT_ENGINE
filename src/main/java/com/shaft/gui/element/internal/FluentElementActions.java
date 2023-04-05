package com.shaft.gui.element.internal;

import com.google.common.base.Throwables;
import com.shaft.cli.FileActions;
import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.enums.internal.ClipboardAction;
import com.shaft.enums.internal.ElementAction;
import com.shaft.gui.browser.internal.FluentBrowserActions;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.SikuliActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.WebDriverElementValidationsBuilder;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.UnexpectedTagNameException;
import org.sikuli.script.App;

import java.nio.file.FileSystems;
import java.util.*;

@SuppressWarnings({"UnusedReturnValue", "unused"})
public class FluentElementActions {
    private static final ThreadLocal<FluentElementActions> INSTANCE = new ThreadLocal<>();

    protected FluentElementActions() {
    }

    public synchronized static FluentElementActions getInstance() {
        JavaScriptWaitManager.waitForLazyLoading();
        if (INSTANCE.get() == null) {
            INSTANCE.set(new FluentElementActions());
        }
        return INSTANCE.get();
    }

    /**
     * This is a convenience method to be able to call TouchActions Actions for
     * touch-enabled devices from within the regular Element Actions Class.
     * <p>
     * Sample use would look like this:
     * ElementActions.performTouchAction().tap(driver, loginButton);
     *
     * @return a TouchActions object capable of performing actions on touch-enabled devices
     */
    public TouchActions performTouchAction() {
        return new TouchActions();
    }

    public AlertActions performAlertAction() {
        return new AlertActions();
    }

    public FluentBrowserActions performBrowserAction() {
        return FluentBrowserActions.getInstance();
    }

    public SikuliActions performSikuliAction() {
        return new SikuliActions();
    }

    public SikuliActions performSikuliAction(App applicationWindow) {
        return new SikuliActions(applicationWindow);
    }

    public TouchActions touch() {
        return new TouchActions();
    }

    public AlertActions alert() {
        return new AlertActions();
    }

    public FluentBrowserActions browser() {
        return FluentBrowserActions.getInstance();
    }

    public SikuliActions sikulix() {
        return new SikuliActions();
    }

    public SikuliActions sikulix(App applicationWindow) {
        return new SikuliActions(applicationWindow);
    }

    public FluentElementActions and() {
        return this;
    }

    public WebDriverElementValidationsBuilder assertThat(By elementLocator) {
        return new WizardHelpers.WebDriverAssertions().element(elementLocator);
    }

    public WebDriverElementValidationsBuilder verifyThat(By elementLocator) {
        return new WizardHelpers.WebDriverVerifications().element(elementLocator);
    }

    public int getElementsCount(By elementLocator) {
        return ElementActionsHelper.getElementsCount(DriverFactoryHelper.getDriver().get(), elementLocator);
    }

    /**
     * Retrieves the selected text from the target drop-down list element and returns it as a string value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the selected text of the target webElement
     */
    public String getSelectedText(By elementLocator) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            StringBuilder elementSelectedText = new StringBuilder();
            try {
                new Select(((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1))).getAllSelectedOptions().forEach(selectedOption -> elementSelectedText.append(selectedOption.getText()));
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementSelectedText.toString().trim(), null, elementName);
                return elementSelectedText.toString().trim();
            } catch (UnexpectedTagNameException rootCauseException) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, rootCauseException);
                return null;
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
        return null;
    }

    /**
     * This is a generic method to enable the execution of the native mobile
     * commands found herein: <a href="http://appium.io/docs/en/commands/mobile-command/">appium.io</a>
     * <p>
     * Note: This method does no validation on the output of the executed JavaScript
     *
     * @param command    the desired mobile command to be executed. e.g., "mobile:
     *                   scroll"
     * @param parameters a map of the key, value parameters for this command. e.g.,
     *                   ImmutableMap.of("direction", "down")
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions executeNativeMobileCommand(String command, Map<String, String> parameters) {
        try {
            ElementActionsHelper.executeNativeMobileCommandUsingJavascript(DriverFactoryHelper.getDriver().get(), command, parameters);
            var testData = "Command: " + command + ", Parameters: " + parameters;
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), testData, null, null);
        } catch (Exception rootCauseException) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, rootCauseException);
        }
        return this;
    }

    /**
     * Clicks on a certain element using Selenium WebDriver, or JavaScript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions click(By elementLocator) {
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            new TouchActions(DriverFactoryHelper.getDriver().get()).tap(elementLocator);
        } else {
            // Waits for the element to be clickable, and then clicks it.
            try {
                var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
                try {
                    // adding hover before clicking an element to enable styles to show in the
                    // execution screenshots and to solve issues clicking on certain elements.
                    (new Actions(DriverFactoryHelper.getDriver().get())).moveToElement(((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1))).perform();
                } catch (Exception t) {
//                    ReportManagerHelper.logDiscrete(t);
                }
                List<Object> screenshot = ElementActionsHelper.takeScreenshot(DriverFactoryHelper.getDriver().get(), elementLocator, "click", null, true);
                // takes screenshot before clicking the element out of view
                // wait for element to be clickable
                try {
                    WebElement element = (WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1);
                    //noinspection ResultOfMethodCallIgnored
                    Boolean.FALSE.equals(ElementActionsHelper.waitForElementToBeClickable(DriverFactoryHelper.getDriver().get(), elementLocator, "click"));
                } catch (Exception exception) {
                    ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, exception);
                }
                // issue: if performing a navigation after clicking on the login button,
                // navigation is triggered immediately and hence it fails.
                // solution: wait for any possible navigation that may be triggered by this
                // click action to conclude
                // removed to enhance performance, and replaced with a process to assert after
                // every navigation
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, "", screenshot, elementName);
            } catch (Throwable throwable) {
                // has to be throwable to catch assertion errors in case element was not found
                if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                    ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
                } else {
                    ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
                }
            }
        }
        return this;
    }

    /**
     * Clicks on certain element using javaScript only
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions clickUsingJavascript(By elementLocator) {

        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            List<Object> screenshot = ElementActionsHelper.takeScreenshot(DriverFactoryHelper.getDriver().get(), elementLocator, "clickUsingJavascript", null, true);
            ElementActionsHelper.clickUsingJavascript(DriverFactoryHelper.getDriver().get(), elementLocator);
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, "", screenshot, elementName);
        } catch (Exception exception) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, exception);
        }

        return this;
    }

    /**
     * If the element is outside the viewport, scrolls the bottom of the element to the bottom of the viewport.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions scrollToElement(By elementLocator) {
        // if mobile, call swipeElementIntoView(null, targetElementLocator, swipeDirection); for convenience
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            performTouchAction().swipeElementIntoView(elementLocator, TouchActions.SwipeDirection.DOWN);
        }
        try {
            ElementActionsHelper.scrollToFindElement(DriverFactoryHelper.getDriver().get(), elementLocator);
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator));
        } catch (Exception throwable) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
        }
        return this;
    }

    /**
     * Waits for the element to be clickable, and then clicks and holds it.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions clickAndHold(By elementLocator) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            List<Object> screenshot = ElementActionsHelper.takeScreenshot(DriverFactoryHelper.getDriver().get(), elementLocator, "clickAndHold", null, true);
            WebElement element = (WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1);
            if (Boolean.FALSE.equals(ElementActionsHelper.waitForElementToBeClickable(DriverFactoryHelper.getDriver().get(), elementLocator, "clickAndHold"))) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), "element is not clickable", elementLocator);
            }
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, "", screenshot, elementName);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
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
    public FluentElementActions clipboardActions(By elementLocator, ClipboardAction action) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            boolean wasActionPerformed;
            if (System.getProperty("targetOperatingSystem").contains("Mac")) {
                wasActionPerformed = ElementActionsHelper.performClipboardActions(DriverFactoryHelper.getDriver().get(), action, Keys.COMMAND);
            } else {
                wasActionPerformed = ElementActionsHelper.performClipboardActions(DriverFactoryHelper.getDriver().get(), action, Keys.CONTROL);
            }
            if (Boolean.TRUE.equals(wasActionPerformed)) {
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), action.getValue(), null, elementName);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), action.getValue(), elementLocator);
            }
        } catch (Exception throwable) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
        }
        return this;
    }

    /**
     * Double-clicks on an element using Selenium WebDriver's Actions Library
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions doubleClick(By elementLocator) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            // takes screenshot before clicking the element out of view
            var screenshot = ElementActionsHelper.takeScreenshot(DriverFactoryHelper.getDriver().get(), elementLocator, "doubleClick", null, true);
            List<List<Object>> attachments = new LinkedList<>();
            attachments.add(screenshot);
            try {
                (new Actions(DriverFactoryHelper.getDriver().get())).moveToElement(((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1))).doubleClick().perform();
            } catch (Exception e) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, e);
            }
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, attachments, elementName);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
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
    public FluentElementActions dragAndDrop(By sourceElementLocator, By destinationElementLocator) {
        try {
            Exception exception = new Exception();
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), sourceElementLocator);
            // replaced canFindUniqueElementForInternalUse, with countFoundElements for
            // destinationElement to bypass the check for element visibility
            // get source element start location
            String startLocation = ((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), sourceElementLocator).get(1)).getLocation().toString();
            // attempt to perform drag and drop
            try {
                ElementActionsHelper.dragAndDropUsingJavascript(DriverFactoryHelper.getDriver().get(), sourceElementLocator, destinationElementLocator);
            } catch (Exception rootCauseException) {
                exception = rootCauseException;
                ReportManagerHelper.logDiscrete(rootCauseException);
            }
            // get source element end location
            String endLocation = ((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), sourceElementLocator).get(1)).getLocation().toString();
            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;
            if (!endLocation.equals(startLocation)) {
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), sourceElementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, elementName);
            } else {
                try {
                    ElementActionsHelper.dragAndDropUsingActions(DriverFactoryHelper.getDriver().get(), sourceElementLocator, destinationElementLocator);
                } catch (Exception rootCauseException) {
                    if (!exception.equals(new Exception())) {
                        rootCauseException.addSuppressed(exception);
                    }
                    ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), sourceElementLocator, rootCauseException);
                }
                // get source element end location
                endLocation = ((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), sourceElementLocator).get(1)).getLocation().toString();
                if (!endLocation.equals(startLocation)) {
                    ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), sourceElementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, elementName);
                } else {
                    ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), reportMessage, sourceElementLocator);
                }
            }
        } catch (Throwable throwable) {
            //has to be throwable to catch element not found exception
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), sourceElementLocator, throwable);
            }
        }
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
    public FluentElementActions dragAndDropByOffset(By sourceElementLocator, int xOffset, int yOffset) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), sourceElementLocator);
            WebElement sourceElement = ((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), sourceElementLocator).get(1));
            String startLocation = sourceElement.getLocation().toString();
            // attempt to perform drag and drop
            try {
                (new Actions(DriverFactoryHelper.getDriver().get())).dragAndDropBy(((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), sourceElementLocator).get(1)), xOffset, yOffset).build()
                        .perform();
            } catch (Exception rootCauseException) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), sourceElementLocator, rootCauseException);
            }
            String endLocation = ((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), sourceElementLocator).get(1)).getLocation().toString();
            if (!endLocation.equals(startLocation)) {
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), sourceElementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "Start point: " + startLocation + ", End point: " + endLocation, null, elementName);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), "Start point = End point: " + endLocation, sourceElementLocator);
            }
        } catch (Exception throwable) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), sourceElementLocator, throwable);
        }
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
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param attributeName  the target attribute of the webElement under test
     * @return the value of the target attribute of the webElement under test
     */
    @SuppressWarnings("SpellCheckingInspection")
    public String getAttribute(By elementLocator, String attributeName) {
        ReportManager.logDiscrete("Attempting to getAttribute \"" + attributeName + "\" from elementLocator \"" + elementLocator + "\".");
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            try {
                String elementAttribute = ((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1)).getAttribute(attributeName);
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementAttribute, null, elementName);
                return elementAttribute;
            } catch (UnsupportedCommandException rootCauseException) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, rootCauseException);
                return null;
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
        return null;
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
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param propertyName   the target CSS property of the webElement under test
     * @return the value of the target CSS property of the webElement under test
     */
    public String getCSSProperty(By elementLocator, String propertyName) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            String elementCssProperty = ((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1)).getCssValue(propertyName);
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementCssProperty, null, elementName);
            return elementCssProperty;
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
        return null;

    }

    /**
     * Returns the handle for currently active context. This can be used to switch
     * to this context at a later time.
     *
     * @return The current context handle
     */
    public String getContext() {
        String context = "";
        if (DriverFactoryHelper.getDriver().get() instanceof AndroidDriver androidDriver) {
            context = androidDriver.getContext();
        } else if (DriverFactoryHelper.getDriver().get() instanceof IOSDriver iosDriver) {
            context = iosDriver.getContext();
        } else {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null);
        }
        ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), context, null, null);
        return context;
    }

    /**
     * Switches focus to another context
     *
     * @param context The name of the context or the handle as returned by
     *                ElementActions.getContext(WebDriver driver)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions setContext(String context) {
        if (DriverFactoryHelper.getDriver().get() instanceof AndroidDriver androidDriver) {
            androidDriver.context(context);
        } else if (DriverFactoryHelper.getDriver().get() instanceof IOSDriver iosDriver) {
            iosDriver.context(context);
        } else {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), context, null);
        }
        ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), context, null, null);
        return this;
    }

    /**
     * Returns a list of unique handles for all the currently open contexts. This
     * can be used to switch to any of these contexts at a later time.
     *
     * @return list of context handles
     */
    public List<String> getContextHandles() {
        List<String> windowHandles = new ArrayList<>();
        if (DriverFactoryHelper.getDriver().get() instanceof AndroidDriver androidDriver) {
            windowHandles.addAll(androidDriver.getContextHandles());
        } else if (DriverFactoryHelper.getDriver().get() instanceof IOSDriver iosDriver) {
            windowHandles.addAll(iosDriver.getContextHandles());
        } else {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null);
        }
        ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(windowHandles), null, null);
        return windowHandles;
    }

    /**
     * Retrieves text from the target element and returns it as a string value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the text value of the target webElement
     */
    public String getText(By elementLocator) {
        try {
            var elementInformation = ElementInformation.fromList(ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator));
            var elementName = elementInformation.getElementName();
            String elementText;
            try {
                elementText = (elementInformation.getFirstElement()).getText();
            } catch (WebDriverException webDriverException) {
                elementText = ElementInformation.fromList(ElementActionsHelper.performActionAgainstUniqueElement(DriverFactoryHelper.getDriver().get(), elementInformation.getLocator(), ElementAction.GET_TEXT)).getActionResult();
            }
            if ((elementText == null || elementText.trim().equals("")) && !DriverFactoryHelper.isMobileNativeExecution()) {
                try {
                    elementText = (elementInformation.getFirstElement()).getAttribute(ElementActionsHelper.TextDetectionStrategy.CONTENT.getValue());
                } catch (WebDriverException webDriverException) {
                    elementText = ElementInformation.fromList(ElementActionsHelper.performActionAgainstUniqueElement(DriverFactoryHelper.getDriver().get(), elementInformation.getLocator(), ElementAction.GET_CONTENT)).getActionResult();
                }
            }
            if ((elementText == null || elementText.trim().equals("")) && !DriverFactoryHelper.isMobileNativeExecution()) {
                try {
                    elementText = (elementInformation.getFirstElement()).getAttribute(ElementActionsHelper.TextDetectionStrategy.VALUE.getValue());
                } catch (WebDriverException webDriverException) {
                    elementText = ElementInformation.fromList(ElementActionsHelper.performActionAgainstUniqueElement(DriverFactoryHelper.getDriver().get(), elementInformation.getLocator(), ElementAction.GET_VALUE)).getActionResult();
                }
            }
            if (elementText == null) {
                elementText = "";
            }
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementText, null, elementName);
            return elementText;
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            var rootCauseClassName = Throwables.getRootCause(throwable).getClass().getName();
            if (rootCauseClassName.equals(org.openqa.selenium.NoSuchElementException.class.getName())
                    || rootCauseClassName.equals(org.openqa.selenium.InvalidSelectorException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
        return null;
    }

    /**
     * Returns the unique handle for currently active window. This can be used to
     * switch to this window at a later time.
     *
     * @return window handle
     */
    public String getWindowHandle() {
        String nameOrHandle = DriverFactoryHelper.getDriver().get().getWindowHandle();
        ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), nameOrHandle, null, null);
        return nameOrHandle;
    }

    /**
     * Returns a list of unique handles for all the currently open windows. This can
     * be used to switch to any of these windows at a later time.
     *
     * @return list of window handles
     */
    public List<String> getWindowHandles() {
        List<String> windowHandles = new ArrayList<>(DriverFactoryHelper.getDriver().get().getWindowHandles());
        ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(windowHandles), null, null);
        return windowHandles;
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
    public FluentElementActions hover(By elementLocator) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            try {
                (new Actions(DriverFactoryHelper.getDriver().get())).moveToElement(((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1))).perform();
            } catch (Exception rootCauseException) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, rootCauseException);
            }
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
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
    public FluentElementActions hoverAndClick(List<By> hoverElementLocators, By clickableElementLocator) {
        hoverElementLocators.forEach(this::hover);
        click(clickableElementLocator);
        return this;
    }

    /**
     * Sends a key-press to the target element.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param key            the key that should be pressed
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions keyPress(By elementLocator, Keys key) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            List<Object> screenshot = ElementActionsHelper.takeScreenshot(DriverFactoryHelper.getDriver().get(), elementLocator, "keyPress", null, true);
            // takes screenshot before moving the element out of view
            ((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1)).sendKeys(key);
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, key.name(), screenshot, elementName);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), key.name(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), key.name(), elementLocator, throwable);
            }
        }
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
    public FluentElementActions select(By elementLocator, String text) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            //add forced check that the select element actually has options and is not empty
            if (!Boolean.TRUE.equals(ElementActionsHelper.waitForElementTextToBeNot(DriverFactoryHelper.getDriver().get(), elementLocator, ""))) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), text, elementLocator);
            }
            boolean isOptionFound = false;
            var availableOptionsList = (new Select(((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1)))).getOptions();
            for (int i = 0; i < availableOptionsList.size(); i++) {
                String visibleText = availableOptionsList.get(i).getText();
                String value = availableOptionsList.get(i).getAttribute("value");
                if (visibleText.trim().equals(text) || value.trim().equals(text)) {
                    (new Select(((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1)))).selectByIndex(i);
                    ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), text, null, elementName);
                    isOptionFound = true;
                    break;
                }
            }
            if (Boolean.FALSE.equals(isOptionFound)) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), text, elementLocator);
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), text, null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), text, elementLocator, throwable);
            }
        }
        return this;
    }

    /**
     * Used to SetProperty value for an element (hidden or visible) using javascript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param value          the desired value that should be SetProperty for the target
     *                       element
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions setValueUsingJavaScript(By elementLocator, String value) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            Boolean valueSetSuccessfully = ElementActionsHelper.setValueUsingJavascript(elementLocator, value);
            if (Boolean.TRUE.equals(valueSetSuccessfully)) {
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), value, null, elementName);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator);
            }
        } catch (Exception throwable) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
        }
        return this;
    }

    /**
     * Used to submit a form using javascript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions submitFormUsingJavaScript(By elementLocator) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            try {
                ElementActionsHelper.submitFormUsingJavascript(DriverFactoryHelper.getDriver().get(), elementLocator);
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
            } catch (Exception rootCauseException) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, rootCauseException);
            }
        } catch (Exception throwable) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
        }
        return this;
    }

    /**
     * Switches focus to a certain iFrame, is mainly used in coordination with
     * {@link #switchToDefaultContent()} to navigate inside any iFrame
     * layer and go back to the main page
     *
     * @param elementLocator the locator of the iFrame webElement under test (By
     *                       xpath, id, selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions switchToIframe(By elementLocator) {
        try {
            DriverFactoryHelper.getDriver().get().switchTo().frame(((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1)));
            // note to self: remove elementLocator in case of bug in screenshot manager
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(elementLocator), null, null);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
        return this;
    }

    /**
     * Switches focus to default content, is mainly used in coordination with
     * {@link #switchToIframe(By)} to exit any iFrame layer and go back
     * to the main page
     *
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("UnusedReturnValue")
    public FluentElementActions switchToDefaultContent() {
        try {
            DriverFactoryHelper.getDriver().get().switchTo().defaultContent();
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (Exception rootCauseException) {
//            failAction(DriverFactoryHelper.getDriver().get(), null, rootCauseException);
        }
        // if there is no last used driver or no drivers in the drivers list, do
        // nothing...
//        return new FluentElementActions(Objects.requireNonNull(DriverFactoryHelper.getDriver()).get());
        return this;
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions type(By elementLocator, String text) {
        try {
            var elementInformation = ElementInformation.fromList(ElementActionsHelper.identifyUniqueElementIgnoringVisibility(DriverFactoryHelper.getDriver().get(), elementLocator));
            String actualResult = ElementActionsHelper.typeWrapper(elementInformation, text);
            var elementName = elementInformation.getElementName();
            if (actualResult != null && actualResult.equals(text)) {
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), text, null, elementName);
            } else if (actualResult == null) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), "Expected to type: \"" + text + "\", but ended up with: \"" + actualResult + "\"",
                        elementLocator);
            }
        } catch (Throwable throwable) {
            // it has to be throwable so that it can catch any underlying assertion error
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
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
    public FluentElementActions typeAppend(By elementLocator, String text) {
        try {
            if (text != null) {
                var elementInformation = ElementInformation.fromList(ElementActionsHelper.performActionAgainstUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator, ElementAction.SEND_KEYS, text));
                var elementName = elementInformation.getElementName();
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), text, null, elementName);
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
        return this;
    }

    /**
     * ValidationEnums the required file path into an input[type='file'] button, to
     * successfully upload the target file.
     *
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc)
     * @param filePath       the full path to the file that needs to be uploaded, it can be absolute or relative
     *                       path, Engine will detect that
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions typeFileLocationForUpload(By elementLocator, String filePath) {
        var absoluteFilePath = filePath;
        if (filePath.startsWith("src")) {
            absoluteFilePath = FileActions.getInstance().getAbsolutePath(filePath);
        }

        String internalAbsoluteFilePath = absoluteFilePath.replace("/", FileSystems.getDefault().getSeparator());
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            List<Object> screenshot = ElementActionsHelper.takeScreenshot(DriverFactoryHelper.getDriver().get(), elementLocator, "typeFileLocationForUpload", null, true);
            // takes screenshot before clicking the element out of view
            try {
                ((WebElement) ElementActionsHelper.identifyUniqueElementIgnoringVisibility(DriverFactoryHelper.getDriver().get(), elementLocator).get(1)).sendKeys(internalAbsoluteFilePath);
            } catch (InvalidArgumentException e) {
                //this happens when the file path doesn't exist
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), internalAbsoluteFilePath, elementLocator, e);
            } catch (ElementNotInteractableException | NoSuchElementException exception1) {
                ElementActionsHelper.changeWebElementVisibilityUsingJavascript(DriverFactoryHelper.getDriver().get(), elementLocator, true);
                try {
                    ((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1)).sendKeys(internalAbsoluteFilePath);
                } catch (WebDriverException rootCauseException) {
                    rootCauseException.addSuppressed(exception1);
                    // happened for the first time on MacOSX due to incorrect file path separator
                    ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), internalAbsoluteFilePath, elementLocator, rootCauseException);
                }
                try {
                    ElementActionsHelper.changeWebElementVisibilityUsingJavascript(DriverFactoryHelper.getDriver().get(), elementLocator, false);
                } catch (NoSuchElementException | StaleElementReferenceException e) {
                    // this exception is sometimes thrown on firefox after the upload has been
                    // successful, since we don't have to return the style to what it was, then it's
                    // okay to do nothing here.
                    ReportManagerHelper.logDiscrete(e);
                }
            }
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, internalAbsoluteFilePath, screenshot, elementName);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
        return this;
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element. Obfuscates the written text in the output
     * report. This action should be used for writing passwords and secure text.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions typeSecure(By elementLocator, String text) {
        try {
            var elementInformation = ElementInformation.fromList(ElementActionsHelper.identifyUniqueElementIgnoringVisibility(DriverFactoryHelper.getDriver().get(), elementLocator));
            String actualResult = ElementActionsHelper.typeWrapper(elementInformation, text);
            var elementName = (String) elementInformation.getElementName();
            if (actualResult != null && actualResult.equals(text)) {
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), ElementActionsHelper.OBFUSCATED_STRING.repeat(text.length()), null, elementName);
            } else if (actualResult == null) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), "Expected to type: \"" + text + "\", but ended up with: \""
                        + actualResult + "\"", elementLocator);
            }
        } catch (Throwable throwable) {
            // it has to be throwable so that it can catch any underlying assertion error
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
        return this;
    }

    /**
     * Waits dynamically for a specific element to be present in DOM, and ready to interact with, on the current page.
     *
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions waitToBeReady(By elementLocator) {
        return waitToBeReady(elementLocator, true);
    }

    public FluentElementActions waitToBeReady(By elementLocator, boolean isExpectedToBeVisible) {
        ReportManager.logDiscrete("Waiting for element to be present; elementLocator \"" + elementLocator + "\", isExpectedToBeVisible\"" + isExpectedToBeVisible + "\"...");
        String reportMessage = "waited for the element's state of visibility to be (" + isExpectedToBeVisible
                + "). Element locator (" + ElementActionsHelper.formatLocatorToString(elementLocator) + ")";
        try {
            var elementInformation = ElementInformation.fromList(ElementActionsHelper.performActionAgainstUniqueElementIgnoringVisibility(DriverFactoryHelper.getDriver().get(), elementLocator, ElementAction.IS_DISPLAYED));
            boolean isDisplayed = Boolean.parseBoolean(elementInformation.getActionResult());
            //element is present
            if (isExpectedToBeVisible == isDisplayed) {
                //either expected to be visible and is displayed, or not expected to be visible and not displayed
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, elementInformation.getElementName());
            } else //noinspection ConstantValue
                if (!isExpectedToBeVisible && isDisplayed) {
                    // Element is displayed and needed to wait until it's invisible
                    if (ElementActionsHelper.waitForElementInvisibility(elementLocator)) {
                        ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, elementInformation.getElementName());
                    } else {
                        // Element still exists after timeout
                        ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), reportMessage, elementLocator);
                    }
                } else {
                    // Element is not displayed
                    ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), reportMessage, elementLocator);
                }
        } catch (AssertionError assertionError) {
            // in case element was not found
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), reportMessage, null, assertionError);
        }
        return this;
    }

    @Deprecated
    public FluentElementActions waitForElementToBePresent(By elementLocator) {
        return waitToBeReady(elementLocator, true);
    }

    /**
     * Waits dynamically for a specific element to be detached from DOM, or hidden, on the current page.
     *
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions waitToBeInvisible(By elementLocator) {
        return waitToBeReady(elementLocator, false);
    }

    /**
     * Waits dynamically for a specific element's text to change from the initial
     * value to a new unknown value. Waits until the default element identification timeout
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param initialValue   the initial text value of the target webElement
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("UnusedReturnValue")
    public FluentElementActions waitForTextToChange(By elementLocator, String initialValue) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            if (!Boolean.TRUE.equals(ElementActionsHelper.waitForElementTextToBeNot(DriverFactoryHelper.getDriver().get(), elementLocator, initialValue))) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), initialValue, elementLocator);
            }
            try {
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(),
                        "from: \"" + initialValue + "\", to: \"" + getText(elementLocator) + "\"", null, elementName);
            } catch (Exception e) {
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(),
                        "from: \"" + initialValue + "\", to a new value.", null, elementName);
            }
        } catch (Exception throwable) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
        }
        return this;
    }

    /**
     * Waits dynamically for a specific element's attribute to be a certain value.
     * Waits until the default element identification timeout
     *
     * @param elementLocator   the locator of the webElement under test (By xpath, id,
     *                         selector, name ...etc)
     * @param att              the attribute name of the target webElement
     * @param expectedAttValue the expected value of the attribute
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions waitToAttribute(By elementLocator, String att, String expectedAttValue) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            if (Boolean.FALSE.equals(ElementActionsHelper
                    .waitForElementAttributeToBe(DriverFactoryHelper.getDriver().get(), elementLocator,
                            att, expectedAttValue))) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator);
            }
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(),
                    "wait for element attribute \"" + att + "\" to be \"" + expectedAttValue + "\"", null, elementName);
        } catch (Exception throwable) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
        }
        return this;
    }

    /**
     * Checks to see if an element is displayed
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return boolean value, true if the element is displayed, and false if the
     * element is not displayed
     */
    public boolean isElementDisplayed(By elementLocator) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            boolean isDisplayed = ((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1)).isDisplayed();
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
            return isDisplayed;
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
        return false;
    }

    /**
     * Checks to see if an element is clickable
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return boolean value, true if the element is clickable, and false if the
     * element is not clickable
     */
    public boolean isElementClickable(By elementLocator) {
        try {
            var elementName = ElementActionsHelper.getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            if (ElementActionsHelper.waitForElementToBeClickable(DriverFactoryHelper.getDriver().get(), elementLocator, "")) {
                //element is clickable
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "element is clickable", null, elementName);
                return true;
            } else {
                //element is not clickable
                ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "element is not clickable", null, elementName);
                return false;
            }
        } catch (Exception throwable) {
            ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            //unreachable code
            return false;
        }
    }

    public FluentElementActions captureScreenshot(By elementLocator) {
        ReportManagerHelper.log("Capture element screenshot", Collections.singletonList(ScreenshotManager.prepareImageForReport(ScreenshotManager.takeElementScreenshot(DriverFactoryHelper.getDriver().get(), elementLocator), "captureScreenshot")));
        return this;
    }
}
