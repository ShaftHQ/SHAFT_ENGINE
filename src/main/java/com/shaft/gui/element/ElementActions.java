package com.shaft.gui.element;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.enums.internal.ClipboardAction;
import com.shaft.enums.internal.ElementAction;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.gui.element.internal.ElementInformation;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.waits.WaitActions;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.WebDriverElementValidationsBuilder;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.*;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.Select;
import org.openqa.selenium.support.ui.UnexpectedTagNameException;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.nio.file.FileSystems;
import java.time.Duration;
import java.util.*;

@SuppressWarnings({"unused", "UnusedReturnValue"})
public class ElementActions extends FluentWebDriverAction {
    public ElementActions() {
        initialize();
    }

    public ElementActions(WebDriver driver) {
        initialize(driver);
    }

    public ElementActions(WebDriver driver, boolean isSilent) {
        initialize(driver, isSilent);
    }

    public ElementActions(DriverFactoryHelper helper) {
        initialize(helper);
    }

    @Override public ElementActions and() {
        return this;
    }

    public WebDriverElementValidationsBuilder assertThat(By elementLocator) {
        return new WizardHelpers.WebDriverAssertions(driverFactoryHelper).element(elementLocator);
    }

    public WebDriverElementValidationsBuilder verifyThat(By elementLocator) {
        return new WizardHelpers.WebDriverVerifications(driverFactoryHelper).element(elementLocator);
    }

    public int getElementsCount(By elementLocator) {
        return elementActionsHelper.getElementsCount(driver, elementLocator);
    }

    /**
     * Retrieves the selected text from the target drop-down list element and returns it as a string value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return the selected text of the target webElement
     */
    public String getSelectedText(By elementLocator) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            StringBuilder elementSelectedText = new StringBuilder();
            try {
                new Select(((WebElement) elementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1))).getAllSelectedOptions().forEach(selectedOption -> elementSelectedText.append(selectedOption.getText()));
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementSelectedText.toString().trim(), null, elementName);
                return elementSelectedText.toString().trim();
            } catch (UnexpectedTagNameException rootCauseException) {
                elementActionsHelper.failAction(driver, elementLocator, rootCauseException);
                return null;
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
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
    public ElementActions executeNativeMobileCommand(String command, Map<String, String> parameters) {
        try {
            elementActionsHelper.executeNativeMobileCommandUsingJavascript(driver, command, parameters);
            var testData = "Command: " + command + ", Parameters: " + parameters;
            elementActionsHelper.passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), testData, null, null);
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driver, null, rootCauseException);
        }
        return this;
    }

    /**
     * Clicks on a certain element using Selenium WebDriver, or JavaScript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions click(By elementLocator) {
        new com.shaft.gui.element.internal.Actions(driverFactoryHelper).click(elementLocator);
//        if (DriverFactoryHelper.isMobileNativeExecution()) {
//            new TouchActions(driverFactoryHelper).tap(elementLocator);
//        } else {
//            //rewriting click logic to optimize performance and fully support shadowDom elements
//            // get screenshot before doing anything to be attached in animated GIF (if any) or if screenshots are set to always
//            List<Object> screenshot = elementActionsHelper.takeScreenshot(driver, elementLocator, "click", null, true);
//            ElementInformation elementInformation = new ElementInformation();
//            try {
//                // try performing move to element followed by click
//                elementInformation = ElementInformation.fromList(elementActionsHelper.performActionAgainstUniqueElementIgnoringVisibility(driver, elementLocator, ElementAction.CLICK));
//            } catch (Throwable throwable) {
//                elementActionsHelper.failAction(driver, elementLocator, throwable);
//            }
//            // get element name
//            var elementName = elementInformation.getElementName();
//            elementActionsHelper.passAction(driver, elementLocator, "", screenshot, elementName);
//        }
        return this;
    }

    /**
     * Clicks on certain element using javaScript only
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions clickUsingJavascript(By elementLocator) {

        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            List<Object> screenshot = elementActionsHelper.takeScreenshot(driver, elementLocator, "clickUsingJavascript", null, true);
            elementActionsHelper.clickUsingJavascript(driver, elementLocator);
            elementActionsHelper.passAction(driver, elementLocator, "", screenshot, elementName);
        } catch (Throwable throwable) {
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }

        return this;
    }

    /**
     * If the element is outside the viewport, scrolls the bottom of the element to the bottom of the viewport.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions scrollToElement(By elementLocator) {
        // if mobile, call swipeElementIntoView(null, targetElementLocator, swipeDirection); for convenience
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            performTouchAction().swipeElementIntoView(elementLocator, TouchActions.SwipeDirection.DOWN);
        }
        try {
            elementActionsHelper.scrollToFindElement(driver, elementLocator);
            elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementActionsHelper.getElementName(driver, elementLocator));
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * Waits for the element to be clickable, and then clicks and holds it.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions clickAndHold(By elementLocator) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            List<Object> screenshot = elementActionsHelper.takeScreenshot(driver, elementLocator, "clickAndHold", null, true);
            WebElement element = (WebElement) elementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1);
            if (Boolean.FALSE.equals(elementActionsHelper.waitForElementToBeClickable(driver, elementLocator, "clickAndHold"))) {
                elementActionsHelper.failAction(driver, "element is not clickable", elementLocator);
            }
            elementActionsHelper.passAction(driver, elementLocator, "", screenshot, elementName);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * Attempts to perform a native clipboard action on the text from a certain web
     * element, like copy/cut/paste
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param action         supports the following actions "copy", "paste", "cut",
     *                       "select all", "unselect"
     * @return a self-reference to be used to chain actions
     */
    public ElementActions clipboardActions(By elementLocator, ClipboardAction action) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            boolean wasActionPerformed = elementActionsHelper.performClipboardActions(driver, action);
            if (Boolean.TRUE.equals(wasActionPerformed)) {
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), action.getValue(), null, elementName);
            } else {
                elementActionsHelper.failAction(driver, action.getValue(), elementLocator);
            }
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * Double-clicks on an element using Selenium WebDriver's Actions Library
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions doubleClick(By elementLocator) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            // takes screenshot before clicking the element out of view
            var screenshot = elementActionsHelper.takeScreenshot(driver, elementLocator, "doubleClick", null, true);
            List<List<Object>> attachments = new LinkedList<>();
            attachments.add(screenshot);
            try {
                (new Actions(driver)).moveToElement(((WebElement) elementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1))).doubleClick().perform();
            } catch (Exception e) {
                elementActionsHelper.failAction(driver, elementLocator, e);
            }
            elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, attachments, elementName);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * Drags the source element and drops it onto the destination element
     *
     * @param sourceElementLocator      the locator of the source webElement that
     *                                  should be dragged under test (By xpath, id,
     *                                  selector, name ...etc.)
     * @param destinationElementLocator the locator of the target webElement that
     *                                  should receive the dropped source element
     *                                  under test (By xpath, id, selector, name
     *                                  ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions dragAndDrop(By sourceElementLocator, By destinationElementLocator) {
        new com.shaft.gui.element.internal.Actions(driverFactoryHelper).dragAndDrop(sourceElementLocator, destinationElementLocator);
//        try {
//            Exception exception = new Exception();
//            var elementName = elementActionsHelper.getElementName(driver, sourceElementLocator);
//            // replaced canFindUniqueElementForInternalUse, with countFoundElements for
//            // destinationElement to bypass the check for element visibility
//            // get source element start location
//            String startLocation = ((WebElement) elementActionsHelper.identifyUniqueElement(driver, sourceElementLocator).get(1)).getLocation().toString();
//            // attempt to perform drag and drop
//            try {
//                elementActionsHelper.dragAndDropUsingJavascript(driver, sourceElementLocator, destinationElementLocator);
//            } catch (Exception rootCauseException) {
//                exception = rootCauseException;
//                ReportManagerHelper.logDiscrete(rootCauseException);
//            }
//            // get source element end location
//            String endLocation = ((WebElement) elementActionsHelper.identifyUniqueElement(driver, sourceElementLocator).get(1)).getLocation().toString();
//            String reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;
//            if (!endLocation.equals(startLocation)) {
//                elementActionsHelper.passAction(driver, sourceElementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, elementName);
//            } else {
//                try {
//                    elementActionsHelper.dragAndDropUsingActions(driver, sourceElementLocator, destinationElementLocator);
//                } catch (Exception rootCauseException) {
//                    if (!exception.equals(new Exception())) {
//                        rootCauseException.addSuppressed(exception);
//                    }
//                    elementActionsHelper.failAction(driver, sourceElementLocator, rootCauseException);
//                }
//                // get source element end location
//                endLocation = ((WebElement) elementActionsHelper.identifyUniqueElement(driver, sourceElementLocator).get(1)).getLocation().toString();
//                if (!endLocation.equals(startLocation)) {
//                    elementActionsHelper.passAction(driver, sourceElementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, elementName);
//                } else {
//                    elementActionsHelper.failAction(driver, reportMessage, sourceElementLocator);
//                }
//            }
//        } catch (Throwable throwable) {
//            // has to be throwable to catch assertion errors in case element was not found
//            elementActionsHelper.failAction(driver, sourceElementLocator, throwable);
//        }
        return this;
    }

    /**
     * Drags the source element and drops it onto the determined offset
     *
     * @param sourceElementLocator the locator of the source webElement that should
     *                             be dragged under test (By xpath, id, selector,
     *                             name ...etc.)
     * @param xOffset              the horizontal offset by which the element should
     *                             be moved
     * @param yOffset              the vertical offset by which the element should
     *                             be moved
     * @return a self-reference to be used to chain actions
     */
    public ElementActions dragAndDropByOffset(By sourceElementLocator, int xOffset, int yOffset) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, sourceElementLocator);
            WebElement sourceElement = ((WebElement) elementActionsHelper.identifyUniqueElement(driver, sourceElementLocator).get(1));
            String startLocation = sourceElement.getLocation().toString();
            // attempt to perform drag and drop
            try {
                (new Actions(driver)).dragAndDropBy(((WebElement) elementActionsHelper.identifyUniqueElement(driver, sourceElementLocator).get(1)), xOffset, yOffset).build().perform();
            } catch (Exception rootCauseException) {
                elementActionsHelper.failAction(driver, sourceElementLocator, rootCauseException);
            }
            String endLocation = ((WebElement) elementActionsHelper.identifyUniqueElement(driver, sourceElementLocator).get(1)).getLocation().toString();
            if (!endLocation.equals(startLocation)) {
                elementActionsHelper.passAction(driver, sourceElementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "Start point: " + startLocation + ", End point: " + endLocation, null, elementName);
            } else {
                elementActionsHelper.failAction(driver, "Start point = End point: " + endLocation, sourceElementLocator);
            }
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driver, sourceElementLocator, throwable);
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
     *                       selector, name ...etc.)
     * @param attributeName  the target attribute of the webElement under test
     * @return the value of the target attribute of the webElement under test
     */
    @SuppressWarnings("SpellCheckingInspection")
    public String getAttribute(By elementLocator, String attributeName) {
//        ReportManager.logDiscrete("Attempting to getAttribute \"" + attributeName + "\" from elementLocator \"" + elementLocator + "\".");
        try {
            var elementInformation = ElementInformation.fromList(elementActionsHelper.performActionAgainstUniqueElementIgnoringVisibility(driver, elementLocator, ElementAction.GET_ATTRIBUTE, attributeName));
            try {
                String elementAttribute = elementInformation.getActionResult();
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementAttribute, null, elementInformation.getElementName());
                return elementAttribute;
            } catch (UnsupportedCommandException rootCauseException) {
                elementActionsHelper.failAction(driver, elementLocator, rootCauseException);
                return null;
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
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
     *                       selector, name ...etc.)
     * @param propertyName   the target CSS property of the webElement under test
     * @return the value of the target CSS property of the webElement under test
     */
    public String getCSSProperty(By elementLocator, String propertyName) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            String elementCssProperty = ((WebElement) elementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).getCssValue(propertyName);
            elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementCssProperty, null, elementName);
            return elementCssProperty;
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return null;

    }

    /**
     * Retrieves text from the target element and returns it as a string value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return the text value of the target webElement
     */
    public String getText(By elementLocator) {
        try {
            var elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator));
            var elementName = elementInformation.getElementName();
            String elementText;
            try {
                elementText = (elementInformation.getFirstElement()).getText();
            } catch (WebDriverException webDriverException) {
                elementText = ElementInformation.fromList(elementActionsHelper.performActionAgainstUniqueElementIgnoringVisibility(driver, elementInformation.getLocator(), ElementAction.GET_TEXT)).getActionResult();
            }
            if ((elementText == null || elementText.isBlank()) && !DriverFactoryHelper.isMobileNativeExecution()) {
                try {
                    elementText = (elementInformation.getFirstElement()).getAttribute(ElementActionsHelper.TextDetectionStrategy.CONTENT.getValue());
                } catch (WebDriverException webDriverException) {
                    elementText = ElementInformation.fromList(elementActionsHelper.performActionAgainstUniqueElementIgnoringVisibility(driver, elementInformation.getLocator(), ElementAction.GET_CONTENT)).getActionResult();
                }
            }
            if ((elementText == null || elementText.isBlank()) && !DriverFactoryHelper.isMobileNativeExecution()) {
                try {
                    elementText = (elementInformation.getFirstElement()).getAttribute(ElementActionsHelper.TextDetectionStrategy.VALUE.getValue());
                } catch (WebDriverException webDriverException) {
                    elementText = ElementInformation.fromList(elementActionsHelper.performActionAgainstUniqueElementIgnoringVisibility(driver, elementInformation.getLocator(), ElementAction.GET_VALUE)).getActionResult();
                }
            }
            if (elementText == null) {
                elementText = "";
            }
            elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), elementText, null, elementName);
            return elementText;
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return null;
    }

    /**
     * Hovers over target element. If you want to hover on a webElement to expose
     * another webElement and click on it, use hoverAndClick instead for a more
     * reliable result.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions hover(By elementLocator) {
        try {
            var elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator));
            var elementName = elementInformation.getElementName();
            ElementInformation.fromList(elementActionsHelper.performActionAgainstUniqueElement(driver, elementInformation.getLocator(), ElementAction.HOVER));
            elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * Hovers over the hoverElements in sequence then clicks the clickableElement
     *
     * @param hoverElementLocators    the list of locators of the webElements under
     *                                test upon which the hover action will be
     *                                performed in sequence (By xpath, id, selector,
     *                                name ...etc.)
     * @param clickableElementLocator the locator of the webElement under test upon
     *                                which the click action will be performed (By
     *                                xpath, id, selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions hoverAndClick(List<By> hoverElementLocators, By clickableElementLocator) {
        hoverElementLocators.forEach(this::hover);
        click(clickableElementLocator);
        return this;
    }

    /**
     * Sends a key-press to the target element.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param key            the key that should be pressed
     * @return a self-reference to be used to chain actions
     */
    public ElementActions keyPress(By elementLocator, Keys key) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            List<Object> screenshot = elementActionsHelper.takeScreenshot(driver, elementLocator, "keyPress", null, true);
            // takes screenshot before moving the element out of view
            ((WebElement) elementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).sendKeys(key);
            elementActionsHelper.passAction(driver, elementLocator, key.name(), screenshot, elementName);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, key.name(), elementLocator, throwable);
        }
        return this;
    }

    /**
     * Selects an element from a dropdown list using its displayed text or attribute Value
     *
     * @param elementLocator     the locator of the webElement under test (By xpath, id,
     *                           selector, name ...etc.)
     * @param valueOrVisibleText the text of the choice that you need to select from the
     *                           target dropDown menu or the string value of attribute "value"
     * @return a self-reference to be used to chain actions
     */
    public ElementActions select(By elementLocator, String valueOrVisibleText) {
        ElementInformation elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElement(driver, elementLocator));

        //Capture the Element Tag
        String elementTag = elementInformation.getElementTag();

        //The Logic to Handle non-Select dropDowns
        if (!elementTag.equals("select")) {
            if (SHAFT.Properties.flags.handleNonSelectDropDown()) {
                click(elementInformation.getLocator());
                elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElement(driver, elementLocator));
                try {
                    RelativeLocator.RelativeBy relativeBy = SHAFT.GUI.Locator.hasAnyTagName().and().containsText(valueOrVisibleText).relativeBy().below(elementInformation.getLocator());
                    elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElement(driver, relativeBy));
                } catch (Throwable var9) {
                    ReportManager.logDiscrete("Cannot Find Element with the following Locator in the DropDown Options: " + By.xpath("//*[text()='" + valueOrVisibleText + "']"));
                    elementActionsHelper.failAction(driver, By.xpath("//*[text()='" + valueOrVisibleText + "']").toString(), elementLocator, var9);
                }
                click(elementInformation.getLocator());
            } else {
                ReportManager.logDiscrete("Cannot Find Element with the following Locator in the DropDown Options: " + By.xpath("//*[text()='" + valueOrVisibleText + "']"));
                elementActionsHelper.failAction(driver, "Select: ", valueOrVisibleText + "\" from Element : " + " Tag should be <Select, yet it was found to be " + "<" + elementTag, elementLocator, null);
            }

            //End of non-select DropDowns Logic
            //================================================================//

        } else {

            try {
                String elementName = elementActionsHelper.getElementName(driver, elementLocator);
                if (!Boolean.TRUE.equals(elementActionsHelper.waitForElementTextToBeNot(driver, elementLocator, ""))) {
                    elementActionsHelper.failAction(driver, valueOrVisibleText, elementLocator);
                }

                boolean isOptionFound = false;
                List<WebElement> availableOptionsList = (new Select((WebElement) elementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1))).getOptions();

                for (int i = 0; i < availableOptionsList.size(); ++i) {
                    String visibleText = availableOptionsList.get(i).getText();
                    String value = availableOptionsList.get(i).getAttribute("value");
                    if (visibleText.trim().equals(valueOrVisibleText) || Objects.requireNonNull(value).trim().equals(valueOrVisibleText)) {
                        (new Select((WebElement) elementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1))).selectByIndex(i);
                        elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), valueOrVisibleText, null, elementName);
                        isOptionFound = true;
                        break;
                    }
                }

                if (Boolean.FALSE.equals(isOptionFound)) {
                    throw new NoSuchElementException("Cannot locate option with Value or Visible text =" + valueOrVisibleText);
                }
            } catch (Throwable var9) {
                elementActionsHelper.failAction(driver, valueOrVisibleText, elementLocator, var9);
            }

        }
        return this;
    }

    /**
     * Used to SetProperty value for an element (hidden or visible) using javascript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param value          the desired value that should be SetProperty for the target
     *                       element
     * @return a self-reference to be used to chain actions
     */
    public ElementActions setValueUsingJavaScript(By elementLocator, String value) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            Boolean valueSetSuccessfully = elementActionsHelper.setValueUsingJavascript(driver, elementLocator, value);
            if (Boolean.TRUE.equals(valueSetSuccessfully)) {
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), value, null, elementName);
            } else {
                elementActionsHelper.failAction(driver, elementLocator);
            }
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * Used to submit a form using javascript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions submitFormUsingJavaScript(By elementLocator) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            List<Object> screenshot = null;
            try {
                screenshot = elementActionsHelper.takeScreenshot(driver, elementLocator, "submitFormUsingJavaScript", null, true);
                elementActionsHelper.submitFormUsingJavascript(driver, elementLocator);
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, Collections.singletonList(screenshot), elementName);
            } catch (JavascriptException javascriptException) {
                if (screenshot == null)
                    screenshot = elementActionsHelper.takeScreenshot(driver, elementLocator, "submitFormUsingJavaScript", null, true);
                driver.findElement(elementLocator).submit();
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, Collections.singletonList(screenshot), elementName);
            } catch (Exception rootCauseException) {
                elementActionsHelper.failAction(driver, elementLocator, rootCauseException);
            }
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * Switches focus to a certain iFrame, is mainly used in coordination with
     * {@link #switchToDefaultContent()} to navigate inside any iFrame
     * layer and go back to the main page
     *
     * @param elementLocator the locator of the iFrame webElement under test (By
     *                       xpath, id, selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions switchToIframe(By elementLocator) {
        try {
            var elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElement(driver, elementLocator));
            LocatorBuilder.getIFrameLocator().set(elementInformation.getLocator());
            // note to self: remove elementLocator in case of bug in screenshot manager
            driver.switchTo().frame(elementInformation.getFirstElement());
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(elementLocator), null, elementInformation.getElementName());
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
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
    public ElementActions switchToDefaultContent() {
        try {
            driver.switchTo().defaultContent();
            LocatorBuilder.getIFrameLocator().remove();
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            elementActionsHelper.passAction(driver, null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (Exception rootCauseException) {
//            failAction(driver, null, rootCauseException);
        }
        // if there is no last used driver or no drivers in the drivers list, do
        // nothing...
//        return new ElementActions(Objects.requireNonNull(driver).get());
        return this;
    }

    /**
     * gets the current frame
     *
     * @return currentFrame the current frame name
     */
    public String getCurrentFrame() {
        String currentFrame = (String) ((JavascriptExecutor) driver).executeScript("return self.name");
        ReportManager.logDiscrete("Current frame name: \"" + currentFrame + "\"");
        return currentFrame;
    }

    public ElementActions type(By elementLocator, CharSequence text) {
        new com.shaft.gui.element.internal.Actions(driverFactoryHelper).type(elementLocator, text);
//        if (text instanceof String stringText) {
//            try {
//                ElementInformation elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator));
//                String actualTextAfterTyping = elementActionsHelper.typeWrapper(driver, elementInformation, stringText);
//                var elementName = elementInformation.getElementName();
//                if (actualTextAfterTyping.equals(stringText)) {
//                    elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), stringText, null, elementName);
//                } else {
//                    elementActionsHelper.failAction(driver, "Expected to type: \"" + stringText + "\", but ended up with: \"" + actualTextAfterTyping + "\"", elementLocator);
//                }
//            } catch (Throwable throwable) {
//                elementActionsHelper.failAction(driver, elementLocator, throwable);
//            }
//            return this;
//        } else {
//            return new com.shaft.gui.element.internal.Actions(driverFactoryHelper).type(elementLocator, text);
//        }
        return this;
    }

    public ElementActions clear(By elementLocator) {
        try {
            // try clearing text
            var elementInformation = ElementInformation.fromList(elementActionsHelper.performActionAgainstUniqueElement(driver, elementLocator, ElementAction.CLEAR));
            var elementName = elementInformation.getElementName();
            if (!SHAFT.Properties.flags.forceCheckTextWasTypedCorrectly()){
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "", null, elementName);
            }else {
                var currentText = getText(elementLocator);
                if (currentText.isBlank()) {
                    elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "", null, elementName);
                } else {
                    // try deleting letter by letter using backspaces
                    for (var ignored : currentText.toCharArray()) {
                        try {
                            (elementInformation.getFirstElement()).sendKeys(Keys.BACK_SPACE);
                        } catch (WebDriverException webDriverException) {
                            elementActionsHelper.performActionAgainstUniqueElement(driver, elementInformation.getLocator(), ElementAction.BACKSPACE);
                        }
                    }
                    var currentTextAfterClearingUsingBackSpace = getText(elementLocator);
                    if (currentText.isBlank()) {
                        elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "", null, elementName);
                    } else {
                        elementActionsHelper.failAction(driver, "Expected to clear existing text, but ended up with: \"" + currentText + "\"", elementLocator);
                    }
                }
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * Appends the required string into the target element, regardless of the
     * current text value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param text           the target text that needs to be appended into the
     *                       target webElement
     * @return a self-reference to be used to chain actions
     */
    public ElementActions typeAppend(By elementLocator, String text) {
        try {
            if (text != null) {
                var elementInformation = ElementInformation.fromList(elementActionsHelper.performActionAgainstUniqueElement(driver, elementLocator, ElementAction.SEND_KEYS, text));
                var elementName = elementInformation.getElementName();
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), text, null, elementName);
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * ValidationEnums the required file path into an input[type='file'] button, to
     * successfully upload the target file.
     *
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc.)
     * @param filePath       the full path to the file that needs to be uploaded, it can be absolute or relative
     *                       path, Engine will detect that.
     * @return a self-reference to be used to chain actions.
     */
    public ElementActions typeFileLocationForUpload(By elementLocator, String filePath) {
        var absoluteFilePath = filePath;
        if (filePath.startsWith("src")) {
            absoluteFilePath = FileActions.getInstance(true).getAbsolutePath(filePath);
        }

        String internalAbsoluteFilePath = absoluteFilePath.replace("/", FileSystems.getDefault().getSeparator());
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            List<Object> screenshot = elementActionsHelper.takeScreenshot(driver, elementLocator, "typeFileLocationForUpload", null, true);
            // takes screenshot before clicking the element out of view
            try {
                ((WebElement) elementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator).get(1)).sendKeys(internalAbsoluteFilePath);
            } catch (InvalidArgumentException e) {
                //this happens when the file path doesn't exist
                elementActionsHelper.failAction(driver, internalAbsoluteFilePath, elementLocator, e);
            } catch (ElementNotInteractableException | NoSuchElementException exception1) {
                elementActionsHelper.changeWebElementVisibilityUsingJavascript(driver, elementLocator, true);
                try {
                    ((WebElement) elementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).sendKeys(internalAbsoluteFilePath);
                } catch (WebDriverException rootCauseException) {
                    rootCauseException.addSuppressed(exception1);
                    // happened for the first time on MacOSX due to incorrect file path separator
                    elementActionsHelper.failAction(driver, internalAbsoluteFilePath, elementLocator, rootCauseException);
                }
                try {
                    elementActionsHelper.changeWebElementVisibilityUsingJavascript(driver, elementLocator, false);
                } catch (NoSuchElementException | StaleElementReferenceException e) {
                    // this exception is sometimes thrown on firefox after the upload has been
                    // successful, since we don't have to return the style to what it was, then it's
                    // okay to do nothing here.
                    ReportManagerHelper.logDiscrete(e);
                }
            }
            elementActionsHelper.passAction(driver, elementLocator, internalAbsoluteFilePath, screenshot, elementName);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element. Obfuscates the written text in the output
     * report. This action should be used for writing passwords and secure text.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     * @return a self-reference to be used to chain actions
     */
    public ElementActions typeSecure(By elementLocator, String text) {
        try {
            var elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElementIgnoringVisibility(driver, elementLocator));
            String actualResult = elementActionsHelper.typeWrapper(driver, elementInformation, text);
            var elementName = elementInformation.getElementName();
            if (actualResult.equals(text)) {
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), ElementActionsHelper.OBFUSCATED_STRING.repeat(text.length()), null, elementName);
            } else {
                elementActionsHelper.failAction(driver, "Expected to type: \"" + text + "\", but ended up with: \"" + actualResult + "\"", elementLocator);
            }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * Waits dynamically for a specific element to be present in DOM, and ready to interact with, on the current page.
     *
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions waitToBeReady(By elementLocator) {
        return waitToBeReady(elementLocator, true);
    }

    public ElementActions waitToBeReady(By elementLocator, boolean isExpectedToBeVisible) {
        ReportManager.logDiscrete("Waiting for element to be present; elementLocator \"" + elementLocator + "\", isExpectedToBeVisible\"" + isExpectedToBeVisible + "\"...");
        String reportMessage = "Waited for the element's state of visibility to be (" + isExpectedToBeVisible + "). Element locator (" + JavaHelper.formatLocatorToString(elementLocator) + ")";
        try {
            var elementInformation = ElementInformation.fromList(elementActionsHelper.performActionAgainstUniqueElementIgnoringVisibility(driver, elementLocator, ElementAction.IS_DISPLAYED));
            boolean isDisplayed = Boolean.parseBoolean(elementInformation.getActionResult());
            //element is present
            if (isExpectedToBeVisible == isDisplayed) {
                //either expected to be visible and is displayed, or not expected to be visible and not displayed
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, elementInformation.getElementName());
            } else //noinspection ConstantValue
                if (!isExpectedToBeVisible && isDisplayed) {
                    // Element is displayed and needed to wait until it's invisible
                    if (elementActionsHelper.waitForElementInvisibility(driver, elementLocator)) {
                        elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), reportMessage, null, elementInformation.getElementName());
                    } else {
                        // Element still exists after timeout
                        elementActionsHelper.failAction(driver, reportMessage, elementLocator);
                    }
                } else {
                    // Element is not displayed
                    elementActionsHelper.failAction(driver, reportMessage, elementLocator);
                }
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, reportMessage, null, throwable);
        }
        return this;
    }

    /**
     * Waits dynamically for a specific element to be detached from DOM, or hidden, on the current page.
     *
     * @param elementLocator the locator of the webElement under test (By xpath,
     *                       id, selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public ElementActions waitToBeInvisible(By elementLocator) {
        return waitToBeReady(elementLocator, false);
    }

    /**
     * Waits dynamically for a specific element's text to change from the initial
     * value to a new unknown value. Waits until the default element identification timeout
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param initialValue   the initial text value of the target webElement
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("UnusedReturnValue")
    public ElementActions waitForTextToChange(By elementLocator, String initialValue) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            if (!Boolean.TRUE.equals(elementActionsHelper.waitForElementTextToBeNot(driver, elementLocator, initialValue))) {
                elementActionsHelper.failAction(driver, initialValue, elementLocator);
            }
            try {
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "from: \"" + initialValue + "\", to: \"" + getText(elementLocator) + "\"", null, elementName);
            } catch (Exception e) {
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "from: \"" + initialValue + "\", to a new value.", null, elementName);
            }
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return this;
    }

    /**
     * Waits dynamically for a specific element's attribute to be a certain value.
     * Waits until the default element identification timeout
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param attribute      the attribute name of the target webElement
     * @param expectedValue  the expected value of the attribute
     * @return a self-reference to be used to chain actions
     */
    public ElementActions waitToAttribute(By elementLocator, String attribute, String expectedValue) {
        try {
            new SynchronizationManager(driver).fluentWait(false).until(f -> {
                var actualValue = new ElementActions(driver, true).getAttribute(elementLocator, attribute);
                return Objects.equals(expectedValue, actualValue);
            });
            elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "wait for element attribute \"" + attribute + "\" to be \"" + expectedValue + "\"", null, elementActionsHelper.getElementName(driver, elementLocator));
        } catch (TimeoutException timeoutException) {
            elementActionsHelper.failAction(driver, elementLocator, timeoutException);
        }
        return this;
    }

    /**
     * Checks to see if an element is displayed
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return boolean value, true if the element is displayed, and false if the
     * element is not displayed
     */
    public boolean isElementDisplayed(By elementLocator) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            boolean isDisplayed = ((WebElement) elementActionsHelper.identifyUniqueElement(driver, elementLocator).get(1)).isDisplayed();
            elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementName);
            return isDisplayed;
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driver, elementLocator, throwable);
        }
        return false;
    }

    /**
     * Checks to see if an element is clickable
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return boolean value, true if the element is clickable, and false if the
     * element is not clickable
     */
    public boolean isElementClickable(By elementLocator) {
        try {
            var elementName = elementActionsHelper.getElementName(driver, elementLocator);
            if (elementActionsHelper.waitForElementToBeClickable(driver, elementLocator, "")) {
                //element is clickable
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "element is clickable", null, elementName);
                return true;
            } else {
                //element is not clickable
                elementActionsHelper.passAction(driver, elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), "element is not clickable", null, elementName);
                return false;
            }
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driver, elementLocator, throwable);
            //unreachable code
            return false;
        }
    }

    /**
     * Get any simple table rows' data that has
     * thead which include all the column labels and tbody which includes all table data
     *
     * @param tableLocator the locator of the table which should be a table tag
     * @return List of Map format and each Map Object follows the following format (Key:column label, value: cell data)
     */
    public List<Map<String, String>> getTableRowsData(By tableLocator) {
        List<Map<String, String>> tableData = new ArrayList<>();
        // Wait for the table to be present and visible
        WebDriverWait wait = new WebDriverWait(driver, Duration.ofSeconds(10));

        try {
            wait.until(ExpectedConditions.visibilityOfElementLocated(tableLocator));
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driver, tableLocator, throwable);
            return null;
        }
        WebElement table = driver.findElement(tableLocator);
        try {
            //Wait until any row is loaded because some websites use lazy loading,
            //and you need to wait for rows to be loaded
            wait.until(ExpectedConditions.visibilityOfElementLocated(By.cssSelector("tbody tr")));
        } catch (Exception e) {
            ReportManager.logDiscrete("Table\"" + tableLocator + "\" is empty");
            //Will return empty list to be used in case you want to assert if the table is empty
            return new ArrayList<>();
        }
        List<WebElement> rows = table.findElement(By.tagName("tbody")).findElements(By.tagName("tr"));
        List<WebElement> headerCells = table.findElement(By.tagName("thead")).findElements(By.tagName("th"));

        //extract the data into a List of Maps
        for (WebElement row : rows) {
            WebElement currentRow = wait.until(ExpectedConditions.visibilityOf(row));
            List<WebElement> cells = row.findElements(By.tagName("td"));
            Map<String, String> rowData = new HashMap<>();
            for (int cellIndex = 0; cellIndex < cells.size(); cellIndex++) {
                String columnName = headerCells.get(cellIndex).getText();
                String cellValue = cells.get(cellIndex).getText();
                rowData.put(columnName, cellValue);
            }

            tableData.add(rowData);
        }

        return tableData;
    }

    public ElementActions captureScreenshot(By elementLocator) {
        var screenshotManager = new ScreenshotManager();
        ReportManagerHelper.log("Capture element screenshot", Collections.singletonList(screenshotManager.prepareImageForReport(screenshotManager.takeElementScreenshot(driver, elementLocator), "captureScreenshot")));
        return this;
    }

    public ElementActions waitUntilNumberOfElementsToBe(By elementLocator, int numberOfElements) {
        new WaitActions(driverFactoryHelper).explicitWaits(ExpectedConditions.numberOfElementsToBe(elementLocator, numberOfElements), (int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout());
        return this;
    }

    public ElementActions waitUntilNumberOfElementsToBeLessThan(By elementLocator, int numberOfElements) {
        new WaitActions(driverFactoryHelper).explicitWaits(ExpectedConditions.numberOfElementsToBeLessThan(elementLocator, numberOfElements), (int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout());
        return this;
    }

    public ElementActions waitUntilNumberOfElementsToBeMoreThan(By elementLocator, int numberOfElements) {
        new WaitActions(driverFactoryHelper).explicitWaits(ExpectedConditions.numberOfElementsToBeMoreThan(elementLocator, numberOfElements), (int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout());
        return this;
    }

    public ElementActions waitUntilAttributeContains(By elementLocator, String attribute, String attributeContainsValue) {
        new WaitActions(driverFactoryHelper).explicitWaits(ExpectedConditions.attributeContains(elementLocator, attribute, attributeContainsValue), (int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout());
        return this;
    }

    public ElementActions waitUntilElementTextToBe(By elementLocator, String text) {
        new WaitActions(driverFactoryHelper).explicitWaits(ExpectedConditions.textToBe(elementLocator, text), (int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout());
        return this;
    }

    public ElementActions waitUntilElementToBeSelected(By elementLocator) {
        new WaitActions(driverFactoryHelper).explicitWaits(ExpectedConditions.elementToBeSelected(elementLocator), (int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout());
        return this;
    }

    public ElementActions waitUntilPresenceOfAllElementsLocatedBy(By elementLocator) {
        new WaitActions(driverFactoryHelper).explicitWaits(ExpectedConditions.presenceOfAllElementsLocatedBy(elementLocator), (int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout());
        return this;
    }

}