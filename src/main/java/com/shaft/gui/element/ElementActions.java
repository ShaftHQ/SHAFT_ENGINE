package com.shaft.gui.element;

import com.google.common.base.Throwables;
import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.driver.DriverFactoryHelper;
import io.github.shafthq.shaft.gui.browser.FluentBrowserActions;
import io.github.shafthq.shaft.gui.element.ElementActionsHelper;
import io.github.shafthq.shaft.gui.element.FluentElementActions;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import static io.github.shafthq.shaft.gui.element.ElementActionsHelper.*;

//TODO: Move body of implementation into the Fluent Actions class to fix internal "Deprecated member is still used" warnings
public class ElementActions extends FluentElementActions {

    public ElementActions() {
        super();
    }

    @SuppressWarnings("unused")
    public ElementActions(WebDriver driver) {
        super();
    }

    public static ElementActions getInstance() {
        return new ElementActions();
    }

    @Deprecated
    public static FluentBrowserActions performBrowserAction(WebDriver driver) {
        return FluentBrowserActions.getInstance();
    }

    @Deprecated
    public static FluentElementActions performElementAction(WebDriver driver) {
        return FluentElementActions.getInstance();
    }

    @Deprecated
    public static TouchActions performTouchAction(WebDriver driver) {
        return new TouchActions();
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
        FluentElementActions.getInstance().click(elementLocator);
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
        FluentElementActions.getInstance().scrollToElement(elementLocator);
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
        FluentElementActions.getInstance().clickAndHold(elementLocator);
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
        FluentElementActions.getInstance().doubleClick(elementLocator);
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
        FluentElementActions.getInstance().dragAndDrop(sourceElementLocator, destinationElementLocator);
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
        FluentElementActions.getInstance().dragAndDropByOffset(sourceElementLocator, xOffset, yOffset);
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
        FluentElementActions.getInstance().executeNativeMobileCommand(command, parameters);
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
    @SuppressWarnings("SpellCheckingInspection")
    @Deprecated
    public static String getAttribute(WebDriver driver, By elementLocator, String attributeName) {
        return FluentElementActions.getInstance().getAttribute(elementLocator, attributeName);
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
        return FluentElementActions.getInstance().getCSSProperty(elementLocator, propertyName);
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
        return FluentElementActions.getInstance().getContext();
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
        return FluentElementActions.getInstance().getContextHandles();
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
        return FluentElementActions.getInstance().getSelectedText(elementLocator);
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
     * Retrieves text from the target element and returns it as a string value.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the text value of the target webElement
     */
    @Deprecated
    public static String getText(WebDriver driver, By elementLocator) {
        return FluentElementActions.getInstance().getText(elementLocator);
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
        return FluentElementActions.getInstance().getWindowHandle();
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
        return FluentElementActions.getInstance().getWindowHandles();
    }

    /**
     * Returns the number of elements that match a certain elementLocator
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return integer value that represents the number of elements that match the
     * desired elementLocator
     */
    @Deprecated
    public static int getElementsCount(WebDriver driver, By elementLocator) {
        return Integer.parseInt(ElementActionsHelper.getMatchingElementsInformation(driver, elementLocator, 1, false).get(0).toString());
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
        FluentElementActions.getInstance().hover(elementLocator);
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
        FluentElementActions.getInstance().hoverAndClick(hoverElementLocators, clickableElementLocator);
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
        return FluentElementActions.getInstance().isElementClickable(elementLocator);
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
        return FluentElementActions.getInstance().isElementDisplayed(elementLocator);
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
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), key, null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), key, elementLocator, throwable);
            }
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
        FluentElementActions.getInstance().keyPress(elementLocator, key);
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
        FluentElementActions.getInstance().waitForTextToChange(elementLocator, initialValue);
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
        FluentElementActions.getInstance().select(elementLocator, text);
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
        FluentElementActions.getInstance().setContext(context);
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
        FluentElementActions.getInstance().setValueUsingJavaScript(elementLocator, value);
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
        FluentElementActions.getInstance().submitFormUsingJavaScript(elementLocator);
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
        FluentElementActions.getInstance().switchToDefaultContent();
    }

    /**
     * Switches focus to a certain iFrame, is mainly used to navigate inside any iFrame
     * layer and go back to the main page
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the iFrame webElement under test (By
     *                       xpath, id, selector, name ...etc)
     */
    @Deprecated
    public static void switchToIframe(WebDriver driver, By elementLocator) {
        FluentElementActions.getInstance().switchToIframe(elementLocator);
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
        FluentElementActions.getInstance().type(elementLocator, text);
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
        FluentElementActions.getInstance().typeAppend(elementLocator, text);
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
        FluentElementActions.getInstance().typeFileLocationForUpload(elementLocator, filePath);
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
        FluentElementActions.getInstance().typeSecure(elementLocator, text);
    }

    /**
     * Waits dynamically for a specific element to be present in DOM, and ready to interact with, on the current page.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc)
     */
    @Deprecated
    public static void waitForElementToBePresent(WebDriver driver, By elementLocator, boolean isExpectedToBeVisible) {
        if (isExpectedToBeVisible) {
            FluentElementActions.getInstance().waitToBeReady(elementLocator);
        } else {
            FluentElementActions.getInstance().waitToBeInvisible(elementLocator);
        }
    }

    /**
     * Waits dynamically for a specific element to be present in DOM, and ready to interact with, on the current page.
     *
     * @param driver          the current instance of Selenium WebDriver
     * @param elementLocator  the locator of the webElement under test (By xpath,
     *                        id, selector, name ...etc)
     */
    @Deprecated(forRemoval = true)
    public static void waitForElementToBeReady(WebDriver driver, By elementLocator) {
        FluentElementActions.getInstance().waitToBeReady(elementLocator);
    }

    /**
     * Waits dynamically for a specific element to be detached from DOM, or hidden, on the current page.
     *
     * @param driver         the current instance of Selenium WebDriver
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc)
     */
    @Deprecated(forRemoval = true)
    public static void waitForElementToBeInvisible(WebDriver driver, By elementLocator) {
        FluentElementActions.getInstance().waitToBeInvisible(elementLocator);
    }
}
