package io.github.shafthq.shaft.gui.element;

import com.google.common.base.Throwables;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import io.github.shafthq.shaft.driver.helpers.DriverFactoryHelper;
import io.github.shafthq.shaft.driver.helpers.WizardHelpers;
import io.github.shafthq.shaft.gui.browser.FluentBrowserActions;
import io.github.shafthq.shaft.gui.image.ScreenshotManager;
import io.github.shafthq.shaft.tools.io.helpers.ReportManagerHelper;
import io.github.shafthq.shaft.validations.helpers.WebDriverElementValidationsBuilder;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.UnexpectedTagNameException;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import static io.github.shafthq.shaft.gui.element.ElementActionsHelper.*;

public class FluentElementActions {
    public FluentElementActions(WebDriver driver) {
        new FluentElementActions();
    }

    public FluentElementActions() {
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
        return new FluentBrowserActions();
    }

    public TouchActions touch() {
        return new TouchActions();
    }

    public AlertActions alert() {
        return new AlertActions();
    }

    public FluentBrowserActions browser() {
        return new FluentBrowserActions();
    }

    public FluentElementActions and() {
        return this;
    }

    public WebDriverElementValidationsBuilder assertThat(By elementLocator) {
        return new WizardHelpers.WebDriverAssertions(DriverFactoryHelper.getDriver()).element(elementLocator);
    }

    public WebDriverElementValidationsBuilder verifyThat(By elementLocator) {
        return new WizardHelpers.WebDriverVerifications(DriverFactoryHelper.getDriver()).element(elementLocator);
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
            var elementName = getElementName(DriverFactoryHelper.getDriver().get(), elementLocator);
            StringBuilder elementSelectedText = new StringBuilder();
            try {
                new Select(((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1))).getAllSelectedOptions().forEach(selectedOption -> elementSelectedText.append(selectedOption.getText()));
                passAction(DriverFactoryHelper.getDriver().get(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementSelectedText.toString().trim(), null, elementName);
                return elementSelectedText.toString().trim();
            } catch (UnexpectedTagNameException rootCauseException) {
                failAction(DriverFactoryHelper.getDriver().get(), elementLocator, rootCauseException);
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
        ElementActions.executeNativeMobileCommand(DriverFactoryHelper.getDriver().get(), command,
                parameters);
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
        ElementActions.click(DriverFactoryHelper.getDriver().get(), elementLocator);
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
        ElementActions.scrollToElement(DriverFactoryHelper.getDriver().get(), elementLocator);
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
        ElementActions.clickAndHold(DriverFactoryHelper.getDriver().get(), elementLocator);
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
    public FluentElementActions clipboardActions(By elementLocator, String action) {
        ElementActions.clipboardActions(DriverFactoryHelper.getDriver().get(), elementLocator, action);
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
        ElementActions.doubleClick(DriverFactoryHelper.getDriver().get(), elementLocator);
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
        ElementActions.dragAndDrop(DriverFactoryHelper.getDriver().get(), sourceElementLocator, destinationElementLocator);
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
        ElementActions.dragAndDropByOffset(DriverFactoryHelper.getDriver().get(), sourceElementLocator, xOffset, yOffset);
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
    public String getAttribute(By elementLocator, String attributeName) {
        return ElementActions.getAttribute(DriverFactoryHelper.getDriver().get(), elementLocator, attributeName);
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
        return ElementActions.getCSSProperty(DriverFactoryHelper.getDriver().get(), elementLocator, propertyName);

    }

    /**
     * Returns the handle for currently active context. This can be used to switch
     * to this context at a later time.
     *
     * @return The current context handle
     */
    public String getContext() {
        return ElementActions.getContext(DriverFactoryHelper.getDriver().get());
    }

    /**
     * Switches focus to another context
     *
     * @param context The name of the context or the handle as returned by
     *                ElementActions.getContext(WebDriver driver)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions setContext(String context) {
        ElementActions.setContext(DriverFactoryHelper.getDriver().get(), context);
        return this;
    }

    /**
     * Returns a list of unique handles for all the currently open contexts. This
     * can be used to switch to any of these contexts at a later time.
     *
     * @return list of context handles
     */
    public List<String> getContextHandles() {
        return ElementActions.getContextHandles(DriverFactoryHelper.getDriver().get());
    }

    /**
     * Retrieves text from the target element and returns it as a string value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @return the text value of the target webElement
     */
    public String getText(By elementLocator) {
        return ElementActions.getText(DriverFactoryHelper.getDriver().get(), elementLocator);
    }

    /**
     * Returns the unique handle for currently active window. This can be used to
     * switch to this window at a later time.
     *
     * @return window handle
     */
    public String getWindowHandle() {
        return ElementActions.getWindowHandle(DriverFactoryHelper.getDriver().get());
    }

    /**
     * Returns a list of unique handles for all the currently open windows. This can
     * be used to switch to any of these windows at a later time.
     *
     * @return list of window handles
     */
    public List<String> getWindowHandles() {
        return ElementActions.getWindowHandles(DriverFactoryHelper.getDriver().get());
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
        ElementActions.hover(DriverFactoryHelper.getDriver().get(), elementLocator);
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
        ElementActions.hoverAndClick(DriverFactoryHelper.getDriver().get(), hoverElementLocators, clickableElementLocator);
        return this;
    }

    /**
     * Sends a key-press to the target element.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc)
     * @param keys           the key that should be pressed
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions keyPress(By elementLocator, Keys keys) {
        ElementActions.keyPress(DriverFactoryHelper.getDriver().get(), elementLocator, keys);
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
        ElementActions.select(DriverFactoryHelper.getDriver().get(), elementLocator, text);
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
        ElementActions.setValueUsingJavaScript(DriverFactoryHelper.getDriver().get(), elementLocator, value);
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
        ElementActions.submitFormUsingJavaScript(DriverFactoryHelper.getDriver().get(), elementLocator);
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
        ElementActions.switchToIframe(DriverFactoryHelper.getDriver().get(), elementLocator);
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
        ElementActions.switchToDefaultContent(DriverFactoryHelper.getDriver().get());
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
        ElementActions.type(DriverFactoryHelper.getDriver().get(), elementLocator, text);
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
        ElementActions.typeAppend(DriverFactoryHelper.getDriver().get(), elementLocator, text);
        return this;
    }

    /**
     * ValidationEnums the required file path into an input[type='file'] button, to
     * successfully upload the target file.
     *
     * @param elementLocator   the locator of the webElement under test (By xpath,
     *                         id, selector, name ...etc)
     * @param absoluteFilePath the full path to the file that needs to be uploaded
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions typeFileLocationForUpload(By elementLocator, String absoluteFilePath) {
        ElementActions.typeFileLocationForUpload(DriverFactoryHelper.getDriver().get(), elementLocator, absoluteFilePath);
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
        ElementActions.typeSecure(DriverFactoryHelper.getDriver().get(), elementLocator, text);
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
        ElementActions.waitForElementToBePresent(DriverFactoryHelper.getDriver().get(), elementLocator, true);
        return this;
    }

    @Deprecated
    public FluentElementActions waitForElementToBePresent(By elementLocator) {
        return waitToBeReady(elementLocator);
    }

    /**
     * Waits dynamically for a specific element to be detached from DOM, or hidden, on the current page.
     *
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public FluentElementActions waitToBeInvisible(By elementLocator) {
        ElementActions.waitForElementToBePresent(DriverFactoryHelper.getDriver().get(), elementLocator, false);
        return this;
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
        ElementActions.waitForTextToChange(DriverFactoryHelper.getDriver().get(), elementLocator, initialValue);
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
        return ElementActions.isElementDisplayed(DriverFactoryHelper.getDriver().get(), elementLocator);
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
        return ElementActions.isElementClickable(DriverFactoryHelper.getDriver().get(), elementLocator);
    }

    public FluentElementActions captureScreenshot(By elementLocator) {
        ReportManagerHelper.log("Capture element screenshot", Collections.singletonList(ScreenshotManager.prepareImageforReport(ScreenshotManager.takeElementScreenshot(DriverFactoryHelper.getDriver().get(), elementLocator), "captureScreenshot")));
        return this;
    }
}
