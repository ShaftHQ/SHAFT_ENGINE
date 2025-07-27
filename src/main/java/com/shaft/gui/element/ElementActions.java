package com.shaft.gui.element;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.enums.internal.ClipboardAction;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.element.internal.ElementInformation;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.WebDriverElementValidationsBuilder;
import org.openqa.selenium.*;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.support.locators.RelativeLocator;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.Select;
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

    @Override
    public Actions and() {
        return new Actions(driverFactoryHelper);
    }

    public WebDriverElementValidationsBuilder assertThat(By elementLocator) {
        return new WizardHelpers.WebDriverAssertions(driverFactoryHelper).element(elementLocator);
    }

    public WebDriverElementValidationsBuilder verifyThat(By elementLocator) {
        return new WizardHelpers.WebDriverVerifications(driverFactoryHelper).element(elementLocator);
    }

    public int getElementsCount(By elementLocator) {
        return elementActionsHelper.getElementsCount(driverFactoryHelper.getDriver(), elementLocator);
    }

    /**
     * Deprecated use {@link Actions.GetElementInformation#selectedText(By)} instead
     * Retrieves the selected text from the target drop-down list element and returns it as a string value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return the selected text of the target webElement
     */
    @Deprecated
    public String getSelectedText(By elementLocator) {
        return new Actions(driverFactoryHelper).get().selectedText(elementLocator);
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
    public Actions executeNativeMobileCommand(String command, Map<String, String> parameters) {
        try {
            elementActionsHelper.executeNativeMobileCommandUsingJavascript(driverFactoryHelper.getDriver(), command, parameters);
            var testData = "Command: " + command + ", Parameters: " + parameters;
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), testData, null, null);
        } catch (Exception rootCauseException) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
        return new Actions(driverFactoryHelper);
    }

    /**
     * Clicks on a certain element using Selenium WebDriver, or JavaScript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public Actions click(By elementLocator) {
        return new Actions(driverFactoryHelper).click(elementLocator);
    }

    /**
     * Clicks on certain element using javaScript only
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public Actions clickUsingJavascript(By elementLocator) {
        return new Actions(driverFactoryHelper).clickUsingJavascript(elementLocator);
    }

    /**
     * If the element is outside the viewport, scrolls the bottom of the element to the bottom of the viewport.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public Actions scrollToElement(By elementLocator) {
        // if mobile, call swipeElementIntoView(null, targetElementLocator, swipeDirection); for convenience
        if (DriverFactoryHelper.isMobileNativeExecution()) {
            performTouchAction().swipeElementIntoView(elementLocator, TouchActions.SwipeDirection.DOWN);
        }
        try {
            elementActionsHelper.scrollToFindElement(driverFactoryHelper.getDriver(), elementLocator);
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, elementActionsHelper.getElementName(driverFactoryHelper.getDriver(), elementLocator));
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, throwable);
        }
        return new Actions(driverFactoryHelper);
    }

    /**
     * Waits for the element to be clickable, and then clicks and holds it.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public Actions clickAndHold(By elementLocator) {
        return new Actions(driverFactoryHelper).clickAndHold(elementLocator);
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
    public Actions clipboardActions(By elementLocator, ClipboardAction action) {
        try {
            var elementName = elementActionsHelper.getElementName(driverFactoryHelper.getDriver(), elementLocator);
            boolean wasActionPerformed = elementActionsHelper.performClipboardActions(driverFactoryHelper.getDriver(), action);
            if (Boolean.TRUE.equals(wasActionPerformed)) {
                elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), action.getValue(), null, elementName);
            } else {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), action.getValue(), elementLocator);
            }
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, throwable);
        }
        return new Actions(driverFactoryHelper);
    }

    /**
     * Double-clicks on an element using Selenium WebDriver's Actions Library
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public Actions doubleClick(By elementLocator) {
        return new Actions(driverFactoryHelper).doubleClick(elementLocator);
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
    public Actions dragAndDrop(By sourceElementLocator, By destinationElementLocator) {
        return new Actions(driverFactoryHelper).dragAndDrop(sourceElementLocator, destinationElementLocator);
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
    public Actions dragAndDropByOffset(By sourceElementLocator, int xOffset, int yOffset) {
        return new Actions(driverFactoryHelper).dragAndDropByOffset(sourceElementLocator, xOffset, yOffset);
    }

    /**
     * Deprecated use {@link Actions.GetElementInformation#domProperty(By, String)} or {@link Actions.GetElementInformation#domAttribute(By, String)} instead
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @param attributeName  the target DOM property of the webElement under test
     * @return the value of the target DOM property of the webElement under test
     */
    @Deprecated
    public String getAttribute(By elementLocator, String attributeName) {
        return new Actions(driverFactoryHelper).get().domProperty(elementLocator, attributeName);
    }

    /**
     * Deprecated use {@link Actions.GetElementInformation#cssValue(By, String)} instead
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
    @Deprecated
    public String getCSSProperty(By elementLocator, String propertyName) {
        return new Actions(driverFactoryHelper).get().cssValue(elementLocator, propertyName);


    }

    /**
     * Deprecated use {@link Actions.GetElementInformation#text(By)} instead
     * Retrieves text from the target element and returns it as a string value.
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return the text value of the target webElement
     */
    @Deprecated
    public String getText(By elementLocator) {
        return new Actions(driverFactoryHelper).get().text(elementLocator);
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
    public Actions hover(By elementLocator) {
        return new Actions(driverFactoryHelper).hover(elementLocator);
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
    public Actions hoverAndClick(List<By> hoverElementLocators, By clickableElementLocator) {
        hoverElementLocators.forEach(this::hover);
        return click(clickableElementLocator);
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
    public Actions select(By elementLocator, String valueOrVisibleText) {
        ElementInformation elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator));

        //Capture the Element Tag
        String elementTag = elementInformation.getElementTag();

        //The Logic to Handle non-Select dropDowns
        if (!elementTag.equals("select")) {
            if (SHAFT.Properties.flags.handleNonSelectDropDown()) {
                click(elementInformation.getLocator());
                elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator));
                try {
                    RelativeLocator.RelativeBy relativeBy = SHAFT.GUI.Locator.hasAnyTagName().and().containsText(valueOrVisibleText).byRelation().below(elementInformation.getLocator());
                    elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), relativeBy));
                } catch (Throwable var9) {
                    ReportManager.logDiscrete("Cannot Find Element with the following Locator in the DropDown Options: " + By.xpath("//*[text()='" + valueOrVisibleText + "']"));
                    elementActionsHelper.failAction(driverFactoryHelper.getDriver(), By.xpath("//*[text()='" + valueOrVisibleText + "']").toString(), elementLocator, var9);
                }
                click(elementInformation.getLocator());
            } else {
                ReportManager.logDiscrete("Cannot Find Element with the following Locator in the DropDown Options: " + By.xpath("//*[text()='" + valueOrVisibleText + "']"));
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), "Select: ", valueOrVisibleText + "\" from Element : " + " Tag should be <Select, yet it was found to be " + "<" + elementTag, elementLocator, null);
            }

            //End of non-select DropDowns Logic
            //================================================================//

        } else {

            try {
                String elementName = elementActionsHelper.getElementName(driverFactoryHelper.getDriver(), elementLocator);
                if (!Boolean.TRUE.equals(elementActionsHelper.waitForElementTextToBeNot(driverFactoryHelper.getDriver(), elementLocator, ""))) {
                    elementActionsHelper.failAction(driverFactoryHelper.getDriver(), valueOrVisibleText, elementLocator);
                }

                boolean isOptionFound = false;
                List<WebElement> availableOptionsList = (new Select((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1))).getOptions();

                for (int i = 0; i < availableOptionsList.size(); ++i) {
                    String visibleText = availableOptionsList.get(i).getText();
                    String value = availableOptionsList.get(i).getDomProperty("value");
                    if (visibleText.trim().equals(valueOrVisibleText) || Objects.requireNonNull(value).trim().equals(valueOrVisibleText)) {
                        (new Select((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1))).selectByIndex(i);
                        elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), valueOrVisibleText, null, elementName);
                        isOptionFound = true;
                        break;
                    }
                }

                if (Boolean.FALSE.equals(isOptionFound)) {
                    throw new NoSuchElementException("Cannot locate option with Value or Visible text =" + valueOrVisibleText);
                }
            } catch (Throwable var9) {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), valueOrVisibleText, elementLocator, var9);
            }

        }
        return new Actions(driverFactoryHelper);
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
    public Actions setValueUsingJavaScript(By elementLocator, String value) {
        return new Actions(driverFactoryHelper).setValueUsingJavaScript(elementLocator, value);
    }

    /**
     * Used to submit a form using javascript
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return a self-reference to be used to chain actions
     */
    public Actions submitFormUsingJavaScript(By elementLocator) {
        try {
            var elementName = elementActionsHelper.getElementName(driverFactoryHelper.getDriver(), elementLocator);
            List<Object> screenshot = null;
            try {
                screenshot = elementActionsHelper.takeScreenshot(driverFactoryHelper.getDriver(), elementLocator, "submitFormUsingJavaScript", null, true);
                elementActionsHelper.submitFormUsingJavascript(driverFactoryHelper.getDriver(), elementLocator);
                elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, Collections.singletonList(screenshot), elementName);
            } catch (JavascriptException javascriptException) {
                if (screenshot == null)
                    screenshot = elementActionsHelper.takeScreenshot(driverFactoryHelper.getDriver(), elementLocator, "submitFormUsingJavaScript", null, true);
                driverFactoryHelper.getDriver().findElement(elementLocator).submit();
                elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), null, Collections.singletonList(screenshot), elementName);
            } catch (Exception rootCauseException) {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, rootCauseException);
            }
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, throwable);
        }
        return new Actions(driverFactoryHelper);
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
    public Actions switchToIframe(By elementLocator) {
        try {
            var elementInformation = ElementInformation.fromList(elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator));
            LocatorBuilder.getIFrameLocator().set(elementInformation.getLocator());
            // note to self: remove elementLocator in case of bug in screenshot manager
            driverFactoryHelper.getDriver().switchTo().frame(elementInformation.getFirstElement());
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(elementLocator), null, elementInformation.getElementName());
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, throwable);
        }
        return new Actions(driverFactoryHelper);
    }

    /**
     * Switches focus to default content, is mainly used in coordination with
     * {@link #switchToIframe(By)} to exit any iFrame layer and go back
     * to the main page
     *
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("UnusedReturnValue")
    public Actions switchToDefaultContent() {
        try {
            driverFactoryHelper.getDriver().switchTo().defaultContent();
            LocatorBuilder.getIFrameLocator().remove();
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (Exception rootCauseException) {
//            failAction(driverFactoryHelper.getDriver(), null, rootCauseException);
        }
        // if there is no last used driver or no drivers in the drivers list, do
        // nothing...
//        return new ElementActions(Objects.requireNonNull(driver).get());
        return new Actions(driverFactoryHelper);
    }

    /**
     * gets the current frame
     *
     * @return currentFrame the current frame name
     */
    public String getCurrentFrame() {
        String currentFrame = (String) ((JavascriptExecutor) driverFactoryHelper.getDriver()).executeScript("return self.name");
        ReportManager.logDiscrete("Current frame name: \"" + currentFrame + "\"");
        return currentFrame;
    }

    public Actions type(By elementLocator, CharSequence... text) {
        return new Actions(driverFactoryHelper).type(elementLocator, text);
    }

    public Actions clear(By elementLocator) {
        return new Actions(driverFactoryHelper).clear(elementLocator);
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
    public Actions typeAppend(By elementLocator, CharSequence... text) {
        return new Actions(driverFactoryHelper).typeAppend(elementLocator, text);
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
    public Actions typeFileLocationForUpload(By elementLocator, String filePath) {
        var absoluteFilePath = filePath;
        if (filePath.startsWith("src")) {
            absoluteFilePath = FileActions.getInstance(true).getAbsolutePath(filePath);
        }

        String internalAbsoluteFilePath = absoluteFilePath.replace("/", FileSystems.getDefault().getSeparator());
        try {
            var elementName = elementActionsHelper.getElementName(driverFactoryHelper.getDriver(), elementLocator);
            List<Object> screenshot = elementActionsHelper.takeScreenshot(driverFactoryHelper.getDriver(), elementLocator, "typeFileLocationForUpload", null, true);
            // takes screenshot before clicking the element out of view
            try {
                ((WebElement) elementActionsHelper.identifyUniqueElementIgnoringVisibility(driverFactoryHelper.getDriver(), elementLocator).get(1)).sendKeys(internalAbsoluteFilePath);
            } catch (InvalidArgumentException e) {
                //this happens when the file path doesn't exist
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), internalAbsoluteFilePath, elementLocator, e);
            } catch (ElementNotInteractableException | NoSuchElementException exception1) {
                elementActionsHelper.changeWebElementVisibilityUsingJavascript(driverFactoryHelper.getDriver(), elementLocator, true);
                try {
                    ((WebElement) elementActionsHelper.identifyUniqueElement(driverFactoryHelper.getDriver(), elementLocator).get(1)).sendKeys(internalAbsoluteFilePath);
                } catch (WebDriverException rootCauseException) {
                    rootCauseException.addSuppressed(exception1);
                    // happened for the first time on MacOSX due to incorrect file path separator
                    elementActionsHelper.failAction(driverFactoryHelper.getDriver(), internalAbsoluteFilePath, elementLocator, rootCauseException);
                }
                try {
                    elementActionsHelper.changeWebElementVisibilityUsingJavascript(driverFactoryHelper.getDriver(), elementLocator, false);
                } catch (NoSuchElementException | StaleElementReferenceException e) {
                    // this exception is sometimes thrown on firefox after the upload has been
                    // successful, since we don't have to return the style to what it was, then it's
                    // okay to do nothing here.
                    ReportManagerHelper.logDiscrete(e);
                }
            }
            elementActionsHelper.passAction(driverFactoryHelper.getDriver(), elementLocator, internalAbsoluteFilePath, screenshot, elementName);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), elementLocator, throwable);
        }
        return new Actions(driverFactoryHelper);
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
    public Actions typeSecure(By elementLocator, CharSequence... text) {
        return new Actions(driverFactoryHelper).typeSecure(elementLocator, text);
    }

    /**
     * Deprecated use {@link Actions.GetElementInformation#isDisplayed(By)} instead
     * Checks to see if an element is displayed
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return boolean value, true if the element is displayed, and false if the
     * element is not displayed
     */
    @Deprecated
    public boolean isElementDisplayed(By elementLocator) {
        return new Actions(driverFactoryHelper).get().isDisplayed(elementLocator);
    }

    /**
     * Deprecated use {@link Actions.GetElementInformation#isEnabled(By)} instead
     * Checks to see if an element is clickable
     *
     * @param elementLocator the locator of the webElement under test (By xpath, id,
     *                       selector, name ...etc.)
     * @return boolean value, true if the element is clickable, and false if the
     * element is not clickable
     */
    @Deprecated
    public boolean isElementClickable(By elementLocator) {
        return new Actions(driverFactoryHelper).get().isEnabled(elementLocator);
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
        WebDriverWait wait = new WebDriverWait(driverFactoryHelper.getDriver(), Duration.ofSeconds(10));

        try {
            wait.until(ExpectedConditions.visibilityOfElementLocated(tableLocator));
        } catch (Exception throwable) {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), tableLocator, throwable);
            return null;
        }
        WebElement table = driverFactoryHelper.getDriver().findElement(tableLocator);
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

    public Actions captureScreenshot(By elementLocator) {
        var screenshotManager = new ScreenshotManager();
        ReportManagerHelper.log("Capture element screenshot", Collections.singletonList(screenshotManager.prepareImageForReport(screenshotManager.takeElementScreenshot(driverFactoryHelper.getDriver(), elementLocator), "captureScreenshot")));
        return new Actions(driverFactoryHelper);
    }

    @Deprecated
    public Actions waitUntilNumberOfElementsToBe(By elementLocator, int numberOfElements) {
        return new Actions(driverFactoryHelper).waitUntil(d -> d.findElements(elementLocator).size() == numberOfElements, Duration.ofSeconds((int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout()));
    }

    @Deprecated
    public Actions waitUntilNumberOfElementsToBeLessThan(By elementLocator, int numberOfElements) {
        return new Actions(driverFactoryHelper).waitUntil(d -> d.findElements(elementLocator).size() < numberOfElements, Duration.ofSeconds((int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout()));
    }

    @Deprecated
    public Actions waitUntilNumberOfElementsToBeMoreThan(By elementLocator, int numberOfElements) {
        return new Actions(driverFactoryHelper).waitUntil(d -> d.findElements(elementLocator).size() > numberOfElements, Duration.ofSeconds((int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout()));
    }

    @Deprecated
    public Actions waitUntilAttributeContains(By elementLocator, String attribute, String attributeContainsValue) {
        return new Actions(driverFactoryHelper).waitUntil(d -> {
            var currentValue = driverFactoryHelper.getDriver().findElement(elementLocator).getDomProperty(attribute);
            currentValue = currentValue != null ? currentValue : "";
            return currentValue.contains(attributeContainsValue);
        }, Duration.ofSeconds((int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout()));
    }

    @Deprecated
    public Actions waitUntilElementTextToBe(By elementLocator, String text) {
        return new Actions(driverFactoryHelper).waitUntil(d -> text.equals(driverFactoryHelper.getDriver().findElement(elementLocator).getText()), Duration.ofSeconds((int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout()));
    }

    @Deprecated
    public Actions waitUntilElementToBeSelected(By elementLocator) {
        return new Actions(driverFactoryHelper).waitUntil(d -> d.findElement(elementLocator).isSelected(), Duration.ofSeconds((int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout()));
    }

    @Deprecated
    public Actions waitUntilPresenceOfAllElementsLocatedBy(By elementLocator) {
        return new Actions(driverFactoryHelper).waitUntil(d -> !d.findElements(elementLocator).isEmpty(), Duration.ofSeconds((int) SHAFT.Properties.timeouts.defaultElementIdentificationTimeout()));
    }

}