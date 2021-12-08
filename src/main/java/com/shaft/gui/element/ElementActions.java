package com.shaft.gui.element;

import com.microsoft.playwright.Page;
import com.shaft.cli.FileActions;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.sikuli.script.App;

import java.util.List;
import java.util.Map;

public class ElementActions {

	public ElementActions(WebDriver driver) {
		WebDriverElementActions.setLastUsedDriver(driver);
	}

	public ElementActions(Page page) {
		PlayWrightElementActions.setLastUsedPage(page);
	}
	
	public static WebDriverElementActions performElementAction(WebDriver driver) {
		return new WebDriverElementActions(driver);
	}
	
	public WebDriverElementActions performWebDriverElementAction() {
		return new WebDriverElementActions(WebDriverElementActions.getLastUsedDriver());
	}
	
	public static PlayWrightElementActions performElementAction(Page page) {
		return new PlayWrightElementActions(page);
	}
	
	public PlayWrightElementActions performPlayWrightElementAction() {
		return new PlayWrightElementActions(PlayWrightElementActions.getLastUsedPage());
	}
	
	public static SikuliActions performSikuliAction() {
		return new SikuliActions();
	}

	public static SikuliActions performSikuliAction(App applicationWindow) {
		return new SikuliActions(applicationWindow);
	}

	public static TouchActions performTouchAction(WebDriver driver) {
		return new TouchActions(driver);
	}

	public TouchActions performTouchAction() {
		return new TouchActions(WebDriverElementActions.getLastUsedDriver());
	}

	/**
	 * Clicks on a certain element using Selenium WebDriver, or JavaScript
	 *
	 * @param driver         the current instance of Selenium webdriver
	 * @param elementLocator the locator of the webElement under test (By xpath, id,
	 *                       selector, name ...etc)
	 */
	public static void click(WebDriver driver, By elementLocator) {
		WebDriverElementActions.click(driver, elementLocator);
	}

	/**
	 * Waits for the element to be clickable, and then clicks and holds it.
	 *
	 * @param driver         the current instance of Selenium webdriver
	 * @param elementLocator the locator of the webElement under test (By xpath, id,
	 *                       selector, name ...etc)
	 */
	public static void clickAndHold(WebDriver driver, By elementLocator) {
		WebDriverElementActions.clickAndHold(driver, elementLocator);
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
		WebDriverElementActions.clipboardActions(driver, elementLocator, action);
	}

	/**
	 * Double-clicks on an element using Selenium WebDriver's Actions Library
	 *
	 * @param driver         the current instance of Selenium webdriver
	 * @param elementLocator the locator of the webElement under test (By xpath, id,
	 *                       selector, name ...etc)
	 */
	public static void doubleClick(WebDriver driver, By elementLocator) {
		WebDriverElementActions.doubleClick(driver, elementLocator);
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
		WebDriverElementActions.dragAndDrop(driver, sourceElementLocator, destinationElementLocator);
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
		WebDriverElementActions.dragAndDropByOffset(driver, sourceElementLocator, xOffset, yOffset);
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
		WebDriverElementActions.executeNativeMobileCommand(driver, command, parameters);
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
		return WebDriverElementActions.getAttribute(driver, elementLocator, attributeName);
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
		return WebDriverElementActions.getCSSProperty(driver, elementLocator, propertyName);
	}

	/**
	 * Returns the handle for currently active context. This can be used to switch
	 * to this context at a later time.
	 *
	 * @param driver the current instance of Appium Driver
	 * @return The current context handle
	 */
	public static String getContext(WebDriver driver) {
		return WebDriverElementActions.getContext(driver);
	}

	/**
	 * Returns a list of unique handles for all the currently open contexts. This
	 * can be used to switch to any of these contexts at a later time.
	 *
	 * @param driver the current instance of Appium Driver
	 * @return list of context handles
	 */
	public static List<String> getContextHandles(WebDriver driver) {
		return WebDriverElementActions.getWindowHandles(driver);
	}

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
		return WebDriverElementActions.getElementsCount(driver, elementLocator);
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
		return WebDriverElementActions.getElementsCount(driver, elementLocator, numberOfAttempts);
	}

	/**
	 * Retrieves the selected text from the target drop-down list element and
	 * returns it as a string value.
	 *
	 * @param driver         the current instance of Selenium webdriver
	 * @param elementLocator the locator of the webElement under test (By xpath, id,
	 *                       selector, name ...etc)
	 * @return the selected text of the target webElement
	 */
	public static String getSelectedText(WebDriver driver, By elementLocator) {
		return WebDriverElementActions.getSelectedText(driver, elementLocator);
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
		return WebDriverElementActions.getSize(driver, elementLocator);
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
		return WebDriverElementActions.getTagName(driver, elementLocator);
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
		return WebDriverElementActions.getText(driver, elementLocator);
	}

	/**
	 * Returns the unique handle for currently active window. This can be used to
	 * switch to this window at a later time.
	 *
	 * @param driver the current instance of Selenium webdriver
	 * @return window handle
	 */
	public static String getWindowHandle(WebDriver driver) {
		return WebDriverElementActions.getWindowHandle(driver);
	}

	/**
	 * Returns a list of unique handles for all the currently open windows. This can
	 * be used to switch to any of these windows at a later time.
	 *
	 * @param driver the current instance of Selenium webdriver
	 * @return list of window handles
	 */
	public static List<String> getWindowHandles(WebDriver driver) {
		return WebDriverElementActions.getWindowHandles(driver);

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
		WebDriverElementActions.hover(driver, elementLocator);
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
		WebDriverElementActions.hoverAndClick(driver, hoverElementLocators, clickableElementLocator);
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
		WebDriverElementActions.hoverAndClick(driver, hoverElementLocator, clickableElementLocator);
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
		return WebDriverElementActions.isElementClickable(driver, elementLocator);
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
		return WebDriverElementActions.isElementDisplayed(driver, elementLocator);
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
		WebDriverElementActions.keyPress(driver, elementLocator, key);
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
		WebDriverElementActions.keyPress(driver, elementLocator, key);
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
		WebDriverElementActions.waitForTextToChange(driver, elementLocator, initialValue, numberOfTries);
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
		WebDriverElementActions.select(driver, elementLocator, text);
	}

	/**
	 * Switches focus to another context
	 *
	 * @param driver  the current instance of Appium Driver
	 * @param context The name of the context or the handle as returned by
	 *                ElementActions.getContext(WebDriver driver)
	 */
	public static void setContext(WebDriver driver, String context) {
		WebDriverElementActions.setContext(driver, context);
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
		WebDriverElementActions.setValueUsingJavaScript(driver, elementLocator, value);
	}

	/**
	 * Used to submit a form using javascript
	 *
	 * @param driver         the current instance of Selenium webdriver
	 * @param elementLocator the locator of the webElement under test (By xpath, id,
	 *                       selector, name ...etc)
	 */
	public static void submitFormUsingJavaScript(WebDriver driver, By elementLocator) {
		WebDriverElementActions.submitFormUsingJavaScript(driver, elementLocator);
	}

	/**
	 * Switches focus to default content, is mainly used in coordination with
	 * {@link #switchToIframe(WebDriver, By)} to exit any iFrame layer and go back
	 * to the main page
	 *
	 * @param driver the current instance of Selenium webdriver
	 */
	public static void switchToDefaultContent(WebDriver driver) {
		WebDriverElementActions.switchToDefaultContent(driver);
	}

	/**
	 * Switches focus to default content, is mainly used in coordination with
	 * {@link #switchToIframe(WebDriver, By)} to exit any iFrame layer and go back
	 * to the main page
	 *
	 * @return a self-reference to be used to chain actions
	 */
	public static WebDriverElementActions switchToDefaultContent() {
		return WebDriverElementActions.switchToDefaultContent();
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
		WebDriverElementActions.switchToIframe(driver, elementLocator);
	}

	/**
	 * Switches focus to another window
	 *
	 * @param driver       the current instance of Selenium webdriver
	 * @param nameOrHandle The name of the window or the handle as returned by
	 *                     ElementActions.getWindowHandle(WebDriver driver)
	 */
	public static void switchToWindow(WebDriver driver, String nameOrHandle) {
		WebDriverElementActions.switchToWindow(driver, nameOrHandle);
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
		WebDriverElementActions.type(driver, elementLocator, text);
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
		WebDriverElementActions.typeAppend(driver, elementLocator, text);
	}

	/**
	 * ValidationEnums the required file path into an input[type='file'] button, to
	 * successfully upload the target file.
	 *
	 * @param driver           the current instance of Selenium webdriver
	 * @param elementLocator   the locator of the webElement under test (By xpath,
	 *                         id, selector, name ...etc)
	 *@param filePath       the full path to the file that needs to be uploaded, it can be absolute or relative
     *                       path, Engine will detect that
	 */
	public static void typeFileLocationForUpload(WebDriver driver, By elementLocator, String filePath) {
        if (filePath.startsWith("src")) {
            WebDriverElementActions.typeFileLocationForUpload(driver, elementLocator,
                    FileActions.getAbsolutePath(filePath));
        } else {
            WebDriverElementActions.typeFileLocationForUpload(driver, elementLocator, filePath);
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
		WebDriverElementActions.typeSecure(driver, elementLocator, text);
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
		WebDriverElementActions.waitForElementToBePresent(driver, elementLocator, numberOfTries, stateOfPresence);
	}

	/**
	 * This is a generic method to enable the execution of any of the native mobile
	 * commands found herein: http://appium.io/docs/en/commands/mobile-command/
	 * <p>
	 * Note: This method does no validation on the output of the executed JavaScript
	 *
	 * @param command    the desired mobile command to be executed. e.g., "mobile:
	 *                   scroll"
	 * @param parameters a map of the key, value parameters for this command. e.g.,
	 *                   ImmutableMap.of("direction", "down")
	 * @return a self-reference to be used to chain actions
	 */
	public ElementActions executeNativeMobileCommand(String command, Map<String, String> parameters) {
		WebDriverElementActions.executeNativeMobileCommand(WebDriverElementActions.getLastUsedDriver(), command,
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
	public ElementActions click(By elementLocator) {
		click(WebDriverElementActions.getLastUsedDriver(), elementLocator);
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
		clickAndHold(WebDriverElementActions.getLastUsedDriver(), elementLocator);
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
		clipboardActions(WebDriverElementActions.getLastUsedDriver(), elementLocator, action);
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
		doubleClick(WebDriverElementActions.getLastUsedDriver(), elementLocator);
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
		dragAndDrop(WebDriverElementActions.getLastUsedDriver(), sourceElementLocator, destinationElementLocator);
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
		dragAndDropByOffset(WebDriverElementActions.getLastUsedDriver(), sourceElementLocator, xOffset, yOffset);
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
		return getAttribute(WebDriverElementActions.getLastUsedDriver(), elementLocator, attributeName);
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
		return getCSSProperty(WebDriverElementActions.getLastUsedDriver(), elementLocator, propertyName);

	}

	/**
	 * Returns the handle for currently active context. This can be used to switch
	 * to this context at a later time.
	 *
	 * @return The current context handle
	 */
	public String getContext() {
		return getContext(WebDriverElementActions.getLastUsedDriver());
	}

	/**
	 * Switches focus to another context
	 *
	 * @param context The name of the context or the handle as returned by
	 *                ElementActions.getContext(WebDriver driver)
	 * @return a self-reference to be used to chain actions
	 */
	public ElementActions setContext(String context) {
		setContext(WebDriverElementActions.getLastUsedDriver(), context);
		return this;
	}

	/**
	 * Returns a list of unique handles for all the currently open contexts. This
	 * can be used to switch to any of these contexts at a later time.
	 *
	 * @return list of context handles
	 */
	public List<String> getContextHandles() {
		return getContextHandles(WebDriverElementActions.getLastUsedDriver());
	}

	/**
	 * Retrieves text from the target element and returns it as a string value.
	 *
	 * @param elementLocator the locator of the webElement under test (By xpath, id,
	 *                       selector, name ...etc)
	 * @return the text value of the target webElement
	 */
	public String getText(By elementLocator) {
		return getText(WebDriverElementActions.getLastUsedDriver(), elementLocator);
	}

	/**
	 * Returns the unique handle for currently active window. This can be used to
	 * switch to this window at a later time.
	 *
	 * @return window handle
	 */
	public String getWindowHandle() {
		return getWindowHandle(WebDriverElementActions.getLastUsedDriver());
	}

	/**
	 * Returns a list of unique handles for all the currently open windows. This can
	 * be used to switch to any of these windows at a later time.
	 *
	 * @return list of window handles
	 */
	public List<String> getWindowHandles() {
		return getWindowHandles(WebDriverElementActions.getLastUsedDriver());
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
		hover(WebDriverElementActions.getLastUsedDriver(), elementLocator);
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
		hoverAndClick(WebDriverElementActions.getLastUsedDriver(), hoverElementLocators, clickableElementLocator);
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
		keyPress(WebDriverElementActions.getLastUsedDriver(), elementLocator, keys);
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
		select(WebDriverElementActions.getLastUsedDriver(), elementLocator, text);
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
		setValueUsingJavaScript(WebDriverElementActions.getLastUsedDriver(), elementLocator, value);
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
		submitFormUsingJavaScript(WebDriverElementActions.getLastUsedDriver(), elementLocator);
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
		switchToIframe(WebDriverElementActions.getLastUsedDriver(), elementLocator);
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
		switchToWindow(WebDriverElementActions.getLastUsedDriver(), nameOrHandle);
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
	public ElementActions type(By elementLocator, String text) {
		type(WebDriverElementActions.getLastUsedDriver(), elementLocator, text);
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
		typeAppend(WebDriverElementActions.getLastUsedDriver(), elementLocator, text);
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
	public ElementActions typeFileLocationForUpload(By elementLocator, String absoluteFilePath) {
		typeFileLocationForUpload(WebDriverElementActions.getLastUsedDriver(), elementLocator, absoluteFilePath);
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
	public ElementActions typeSecure(By elementLocator, String text) {
		typeSecure(WebDriverElementActions.getLastUsedDriver(), elementLocator, text);
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
	public ElementActions waitForElementToBePresent(By elementLocator, int numberOfTries, boolean stateOfPresence) {
		waitForElementToBePresent(WebDriverElementActions.getLastUsedDriver(), elementLocator, numberOfTries,
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
	public ElementActions waitForTextToChange(By elementLocator, String initialValue, int numberOfTries) {
		waitForTextToChange(WebDriverElementActions.getLastUsedDriver(), elementLocator, initialValue, numberOfTries);
		return this;
	}

}
