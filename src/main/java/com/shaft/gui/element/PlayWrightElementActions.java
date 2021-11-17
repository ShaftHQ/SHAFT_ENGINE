package com.shaft.gui.element;

import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Mouse.DownOptions;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Page.ClickOptions;
import com.microsoft.playwright.Page.DblclickOptions;
import com.microsoft.playwright.Page.WaitForLoadStateOptions;
import com.microsoft.playwright.Page.WaitForSelectorOptions;
import com.microsoft.playwright.options.LoadState;
import com.microsoft.playwright.options.MouseButton;
import com.microsoft.playwright.options.WaitForSelectorState;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import org.sikuli.script.App;
import org.testng.Assert;

import java.nio.file.FileSystems;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class PlayWrightElementActions {
    private static final String OBFUSCATED_STRING = "•";
    private static Page lastUsedPage = null;
    
    public static void setLastUsedPage(Page page) {
    	PlayWrightElementActions.lastUsedPage=page;
    }
    
    protected static Page getLastUsedPage() {
    	return PlayWrightElementActions.lastUsedPage;
    }

    public PlayWrightElementActions(Page page) {
    	PlayWrightElementActions.lastUsedPage=page;
    }
    

  /**
     * Clicks on a certain element using PlayWright
     *
     * @param page         the current instance of PlayWright page
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     */
    public static void click(Page page, String elementLocator) {
            // Waits for the element to be clickable, and then clicks it.
            if (identifyUniqueElement(page, elementLocator)) {
                String elementText = "";
                try {
                    // attempting to read element text
                    elementText = readTextBasedOnSuccessfulLocationStrategy(page, elementLocator,
                            determineSuccessfulTextLocationStrategy(page, elementLocator));
                    // adding hover before clicking an element to enable styles to show in the
                    // execution screenshots and to solve issues clicking on certain elements.
                    page.hover(elementLocator);
                } catch (Exception e) {
                    ReportManagerHelper.logDiscrete(e);
                }

                List<Object> screenshot = takeScreenshot(page, elementLocator, "click", null, true);
                // takes screenshot before clicking the element out of view

                try {
                    page.click(elementLocator, new ClickOptions().setButton(MouseButton.LEFT));
                } catch (Exception rootCauseException) {
                        ReportManagerHelper.log(rootCauseException);
                        failAction(page, "", elementLocator, rootCauseException);
                }
                
                if (elementText != null && !elementText.equals("")) {
                    passAction(page, elementLocator, elementText.replaceAll("\n", " "), screenshot);
                } else {
                    passAction(page, elementLocator, screenshot);
                }
            } else {
                failAction(page, "", elementLocator);
            }
        }

    /**
     * Double-clicks on an element using PlayWright's Actions Library
     *
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     */
    public static void doubleClick(Page page, String elementLocator) {
        if (PlayWrightElementActions.identifyUniqueElement(page, elementLocator)) {
            String elementText = "";
            try {
                // attempting to read element text
                elementText = readTextBasedOnSuccessfulLocationStrategy(page, elementLocator,
                        determineSuccessfulTextLocationStrategy(page, elementLocator));
            } catch (Exception e) {
                // do nothing
            }
            List<Object> screenshot = PlayWrightElementActions.takeScreenshot(page, elementLocator, "doubleClick", null, true);
            // takes screenshot before clicking the element out of view

            try {
                page.dblclick(elementLocator, new DblclickOptions().setButton(MouseButton.LEFT));
            } catch (Exception e) {
                PlayWrightElementActions.failAction(page, "", elementLocator, e);
            }

            if (elementText != null && !elementText.equals("")) {
                PlayWrightElementActions.passAction(page, elementLocator, elementText.replaceAll("\n", " "), screenshot);
            } else {
                PlayWrightElementActions.passAction(page, elementLocator, screenshot);
            }
        } else {
            PlayWrightElementActions.failAction(page, "", elementLocator);
        }
    }

    /**
     * Drags the source element and drops it onto the destination element using
     * javascript
     *
     * @param page                    the current instance of PlayWright
     * @param sourceElementLocator      the locator of the source webElement that
     *                                  should be dragged under test (String xpath, id,
     *                                  selector, name ...etc)
     * @param destinationElementLocator the locator of the target webElement that
     *                                  should receive the dropped source element
     *                                  under test (String xpath, id, selector, name
     *                                  ...etc)
     */
    public static void dragAndDrop(Page page, String sourceElementLocator, String destinationElementLocator) {
        if (identifyUniqueElement(page, sourceElementLocator)
                && identifyUniqueElement(page, destinationElementLocator)) {
        	var startLocation = String.valueOf(page.querySelector(sourceElementLocator).boundingBox());
        	page.dispatchEvent(sourceElementLocator, "dragstart");
            page.dispatchEvent(destinationElementLocator, "dragenter");
            page.dispatchEvent(destinationElementLocator, "dragover");
            page.dispatchEvent(destinationElementLocator, "drop");
            page.dispatchEvent(destinationElementLocator, "dragend");
            // get source element end location
            var endLocation = String.valueOf(page.querySelector(sourceElementLocator).boundingBox());
            var reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;
            if (!endLocation.equals(startLocation)) {
                passAction(page, sourceElementLocator, reportMessage);
            } else {
                    failAction(page, reportMessage, sourceElementLocator);
            }
        } else {
            failAction(page, "", sourceElementLocator);
        }
    }

    /**
     * Drags the source element and drops it onto the determined offset
     *
     * @param page               the current instance of PlayWright
     * @param sourceElementLocator the locator of the source webElement that should
     *                             be dragged under test (String xpath, id, selector,
     *                             name ...etc)
     * @param xOffset              the horizontal offset String which the element should
     *                             be moved
     * @param yOffset              the vertical offset String which the element should
     *                             be moved
     */
    public static void dragAndDropByOffset(Page page, String sourceElementLocator, int xOffset, int yOffset) {
        if (identifyUniqueElement(page, sourceElementLocator)) {
            var sourceElementBox = page.querySelector(sourceElementLocator).boundingBox();
            var startLocation = String.valueOf(sourceElementBox);

            page.mouse().move(sourceElementBox.x + (sourceElementBox.width / 2), sourceElementBox.y + (sourceElementBox.height / 2));
            page.mouse().down(new DownOptions().setButton(MouseButton.LEFT));
            page.mouse().move(xOffset, yOffset);
            page.mouse().up();
            // get source element end location
            var endLocation = String.valueOf(page.querySelector(sourceElementLocator).boundingBox());
            var reportMessage = "Start point: " + startLocation + ", End point: " + endLocation;

            if (!endLocation.equals(startLocation)) {
                passAction(page, sourceElementLocator, reportMessage);
            } else {
                failAction(page, reportMessage, sourceElementLocator);
            }
        } else {
            failAction(page, "", sourceElementLocator);
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
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @param attributeName  the target attribute of the webElement under test
     * @return the value of the target attribute of the webElement under test
     */
    public static String getAttribute(Page page, String elementLocator, String attributeName) {
        ReportManager.logDiscrete("Attempting to getAttribute [" + attributeName + "] from elementLocator [" + elementLocator + "].");
        if (identifyUniqueElement(page, elementLocator)) {
                String elementAttribute = page.getAttribute(elementLocator, attributeName);
                passAction(page, elementLocator, elementAttribute);
                return elementAttribute;
        } else {
            failAction(page, "", elementLocator);
            return null;
        }
    }

    /**
     * Returns the number of elements that match a certain elementLocator
     *
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return integer value that represents the number of elements that match the
     * desired elementLocator
     */
    public static int getElementsCount(Page page, String elementLocator) {
        return getMatchingElementsCount(page, elementLocator);
    }
    
    /**
     * Returns the number of elements that match a certain elementLocator
     *
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return integer value that represents the number of elements that match the
     * desired elementLocator
     */
    public int getElementsCount(String elementLocator) {
        return getMatchingElementsCount(lastUsedPage, elementLocator);
    }


    /**
     * Retrieves element size from the target element and returns it as a string
     * value.
     *
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return the size of the webElement under test
     */
    public static String getSize(Page page, String elementLocator) {
        if (identifyUniqueElement(page, elementLocator)) {
            var boundingBox = page.querySelector(elementLocator).boundingBox();
            String elementSize = boundingBox.width +" * "+boundingBox.height;
            passAction(page, elementLocator, elementSize);
            return elementSize;
        } else {
            failAction(page, "", elementLocator);
            return null;
        }
    }
    
    /**
     * Retrieves element size from the target element and returns it as a string
     * value.
     *
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return the size of the webElement under test
     */
    public String getSize(String elementLocator) {
    	var page = lastUsedPage;
        if (identifyUniqueElement(page, elementLocator)) {
            var boundingBox = page.querySelector(elementLocator).boundingBox();
            String elementSize = boundingBox.width +" * "+boundingBox.height;
            passAction(page, elementLocator, elementSize);
            return elementSize;
        } else {
            failAction(page, "", elementLocator);
            return null;
        }
    }


    /**
     * Retrieves text from the target element and returns it as a string value.
     *
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return the text value of the target webElement
     */
    public static String getText(Page page, String elementLocator) {
        if (identifyUniqueElement(page, elementLocator)) {
            String elementText = readTextBasedOnSuccessfulLocationStrategy(page, elementLocator, determineSuccessfulTextLocationStrategy(page, elementLocator));
            passAction(page, elementLocator, elementText);
            return elementText;
        } else {
            failAction(page, "", elementLocator);
            return null;
        }
    }

    /**
     * Returns the BrowserContext for currently active page.
     *
     * @param page the current instance of PlayWright
     * @return window handle
     */
    public static BrowserContext getBrowserContext(Page page) {
        var context = page.context();
        PlayWrightElementActions.passAction(page, null, context.toString());
        return context;
    }

    /**
     * Hovers over target element. If you want to hover on a webElement to expose
     * another webElement and click on it, use hoverAndClick instead for a more
     * reliable result.
     *
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     */
    public static void hover(Page page, String elementLocator) {
        if (identifyUniqueElement(page, elementLocator)) {
            try {
            	page.hover(elementLocator);
            } catch (Exception rootCauseException) {
                ReportManagerHelper.log(rootCauseException);
                failAction(page, "", elementLocator, rootCauseException);
            }
            passAction(page, elementLocator,"");
        } else {
            failAction(page, "", elementLocator);
        }
    }

    /**
     * Hovers over the hoverElements in sequence then clicks the clickableElement
     *
     * @param page                  the current instance of PlayWright
     * @param hoverElementLocators    the list of locators of the webElements under
     *                                test upon which the hover action will be
     *                                performed in sequence (String xpath, id, selector,
     *                                name ...etc)
     * @param clickableElementLocator the locator of the webElement under test upon
     *                                which the click action will be performed (By
     *                                xpath, id, selector, name ...etc)
     */
    public static void hoverAndClick(Page page, List<String> hoverElementLocators, String clickableElementLocator) {
        if (identifyUniqueElement(page, hoverElementLocators.get(0))) {
            hoverElementLocators.forEach(page::hover);
            page.click(clickableElementLocator, new ClickOptions().setButton(MouseButton.LEFT));
            passAction(page, hoverElementLocators.get(0),"");
        } else {
            failAction(page, "", hoverElementLocators.get(0));
        }
    }

    /**
     * Hovers over the hoverElement then clicks the clickableElement
     *
     * @param page                  the current instance of PlayWright
     * @param hoverElementLocator     he locator of the webElement under test upon
     *                                which the hover action will be performed (By
     *                                xpath, id, selector, name ...etc)
     * @param clickableElementLocator the locator of the webElement under test upon
     *                                which the click action will be performed (By
     *                                xpath, id, selector, name ...etc)
     */
    public static void hoverAndClick(Page page, String hoverElementLocator, String clickableElementLocator) {
        hoverAndClick(page, Collections.singletonList(hoverElementLocator), clickableElementLocator);
    }

    /**
     * Checks to see if an element is clickable
     *
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return boolean value, true if the element is clickable, and false if the
     * element is not clickable
     */
    public static boolean isElementClickable(Page page, String elementLocator) {
        if (identifyUniqueElement(page, elementLocator)) {
            if (!page.querySelector(elementLocator).isVisible() || !page.querySelector(elementLocator).isEnabled()) {
                failAction(page, "element is not clickable", elementLocator);
            }
            // wait for element to be clickable
            passAction(page, elementLocator,"");
            return true;
        } else {
            failAction(page, "", elementLocator);
            return false;
        }
    }

    /**
     * Checks to see if an element is displayed
     *
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return boolean value, true if the element is displayed, and false if the
     * element is not displayed
     */
    public static boolean isElementDisplayed(Page page, String elementLocator) {
        if (identifyUniqueElement(page, elementLocator, false)) {
            boolean isDisplayed = page.querySelector(elementLocator).isVisible();
            passAction(page, elementLocator,"");
            return isDisplayed;
        } else {
            failAction(page, "", elementLocator);
            return false;
        }
    }

    /**
     * Sends a keypress to the target element. Supported keys are: ENTER, RETURN,
     * TAB
     *
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @param key            the key that should be pressed
     */
    public static void keyPress(Page page, String elementLocator, String key) {
        if (identifyUniqueElement(page, elementLocator)) {
            switch (key.toLowerCase().trim()) {
                case "enter" -> page.keyboard().press("Enter");
                case "return" -> page.keyboard().press("Return");
                case "tab" -> page.keyboard().press("Tab");
                default -> {
                    ReportManager.log("Unsupported Key.");
                    failAction(page, key, elementLocator);
                }
            }
        } else {
            failAction(page, key, elementLocator);
        }
        passAction(page, elementLocator, key);
    }

    public static SikuliActions performSikuliAction() {
        return new SikuliActions();
    }

    public static SikuliActions performSikuliAction(App applicationWindow) {
        return new SikuliActions(applicationWindow);
    }

    public static Page switchToIframe(Page page, String elementLocator) {
        if (identifyUniqueElement(page, elementLocator)) {
            var frame = page.frame(elementLocator).page();
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            passAction(page);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
            return frame;
        } else {
            failAction(page, "", elementLocator);
            return null;
        }
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element.
     *
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     */
    public static void type(Page page, String elementLocator, String text) {
        String actualResult = typeWrapper(page, elementLocator, text);
        if (actualResult != null && actualResult.equals(text)) {
            passAction(page, elementLocator, text);
        } else if (actualResult == null) {
            failAction(page, "", elementLocator);
        } else {
            failAction(page, "Expected to type: \"" + text + "\", but ended up with: \"" + actualResult + "\"",
                    elementLocator);
        }
    }

    /**
     * Appends the required string into the target element, regardless of the
     * current text value.
     *
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be appended into the
     *                       target webElement
     */
    public static void typeAppend(Page page, String elementLocator, String text) {
        if (identifyUniqueElement(page, elementLocator)
                && (text != null)) {
            var currentText = readTextBasedOnSuccessfulLocationStrategy(page, elementLocator, determineSuccessfulTextLocationStrategy(page, elementLocator));
            page.fill(elementLocator, currentText+text);
            passAction(page, elementLocator, text);
        } else {
            failAction(page, text, elementLocator);
        }
    }

    /**
     * ValidationEnums the required file path into an input[type='file'] button, to
     * successfully upload the target file.
     *
     * @param page             the current instance of PlayWright
     * @param elementLocator   the locator of the webElement under test (String xpath,
     *                         id, selector, name ...etc)
     * @param absoluteFilePath the full path to the file that needs to be uploaded
     */
    public static void typeFileLocationForUpload(Page page, String elementLocator, String absoluteFilePath) {
        String internalAbsoluteFilePath = absoluteFilePath.replace("/", FileSystems.getDefault().getSeparator());
        if (identifyUniqueElement(page, elementLocator, false)) {
            List<Object> screenshot = takeScreenshot(page, elementLocator, "typeFileLocationForUpload", null, true);
            // takes screenshot before clicking the element out of view
        	page.setInputFiles("input#upload", Paths.get(absoluteFilePath));
            passAction(page, elementLocator, internalAbsoluteFilePath, screenshot);
        } else {
            failAction(page, internalAbsoluteFilePath, elementLocator);
        }
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required
     * string into the target element. Obfuscates the written text in the output
     * report. This action should be used for writing passwords and secure text.
     *
     * @param page         the current instance of PlayWright
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     */
    public static void typeSecure(Page page, String elementLocator, String text) {
        String actualResult = typeWrapper(page, elementLocator, text);
        if (actualResult != null && actualResult.equals(text)) {
            passAction(page, elementLocator, OBFUSCATED_STRING.repeat(text.length()));
        } else if (actualResult == null) {
            failAction(page, "", elementLocator);
        } else {
            failAction(page, "Expected to type: \"" + text + "\", but ended up with: \""
                    + actualResult + "\"", elementLocator);
        }

    }

    /**
     * Waits dynamically for a specific element to achieve the desired
     * stateOfPresence on the current page. Waits for a specific number of retries
     * multiplied String the default element identification timeout (in the POM.xml
     * file)
     *
     * @param page          the current instance of PlayWright
     * @param elementLocator  the locator of the webElement under test (String xpath,
     *                        id, selector, name ...etc)
     * @param numberOfTries   the number of times to try and wait for the element to
     *                        achieve the desired stateOfPresence (default is 1)
     * @param stateOfPresence the expected state of presence of the element; false
     *                        is not present, and true is present
     */
    public static void waitForElementToBePresent(Page page, String elementLocator, int numberOfTries,
                                                 boolean stateOfPresence) {
        ReportManager.logDiscrete("Waiting for element to be present; elementLocator [" + elementLocator + "], numberOfTries[" + numberOfTries + "], stateOfPresence[" + stateOfPresence + "]...");
        String reportMessage = "waited for the element's state of presence to be (" + stateOfPresence
                + "). Element locator (" + elementLocator + ")";
        if (Boolean.compare(stateOfPresence, getMatchingElementsCount(page, elementLocator) >= 1) == 0) {
            passAction(page, elementLocator, reportMessage);
        } else {
            failAction(page, reportMessage, elementLocator);
        }
    }

    protected static boolean identifyUniqueElement(Page page, String elementLocator) {
        return identifyUniqueElement(page, elementLocator, true);
    }

    protected static List<Object> takeScreenshot(Page page, String elementLocator, String actionName, String testData,
                                                 boolean passFailStatus) {
        if (passFailStatus) {
            try {
                if (elementLocator != null) {
                    return ScreenshotManager.captureScreenShot(page, elementLocator, actionName, true);
                } else if (testData != null) {
                    return ScreenshotManager.captureScreenShot(page, null, actionName, true);
                }
                // else only happens when switching to default content so there is no need to
                // take a screenshot
            } catch (Exception e) {
                ReportManagerHelper.log(e);
                ReportManager.log(
                        "Failed to take a screenshot of the element as it doesn't exist anymore. Taking a screenshot of the whole page.");
                return ScreenshotManager.captureScreenShot(page, null, actionName, true);
            }
        } else {
            return ScreenshotManager.captureScreenShot(page, null, actionName, false);
        }
        return new ArrayList<>();
    }

    private static String confirmTypingWasSuccessful(Page page, String elementLocator, String expectedText,
                                                     TextDetectionStrategy successfulTextLocationStrategy) {
        TextDetectionStrategy updatedSuccessfulTextLocationStrategy = successfulTextLocationStrategy;
        if (updatedSuccessfulTextLocationStrategy.equals(TextDetectionStrategy.UNDEFINED)) {
            updatedSuccessfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(page,
                    elementLocator);
        }
        String actualText = readTextBasedOnSuccessfulLocationStrategy(page, elementLocator,
                updatedSuccessfulTextLocationStrategy);

        if (expectedText.equals(actualText) || OBFUSCATED_STRING.repeat(expectedText.length()).equals(actualText)) {
            return expectedText;
        } else {
            return actualText;
        }
    }

    private static TextDetectionStrategy determineSuccessfulTextLocationStrategy(Page page, String elementLocator) {
        String text = page.querySelector(elementLocator).innerText().trim();
        String content = page.querySelector(elementLocator).textContent().trim();
        Object valueObj = page.querySelector(elementLocator).evaluate("node => node.value");
        String value =null;
        
        if (valueObj !=null) {
            value = (String) valueObj;
        }
        
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
    
    private static void failAction(Page page, String testData, String elementLocator, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(page, actionName, testData, elementLocator, null, rootCauseException);
    }

    private static void failAction(Page page, String actionName, String testData, String elementLocator, List<Object> screenshot,
                                   Exception... rootCauseException) {
        String message = reportActionResult(page, actionName, testData, elementLocator, screenshot, false);

        if (rootCauseException != null && rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
    }

    private static int getMatchingElementsCount(Page page, String elementLocator) {
        if (elementLocator == null) {
            return 0;
        }
//        RecordManager.startVideoRecording(page);
        int matchingElementCount = 0;
        try {
            page.waitForLoadState(LoadState.NETWORKIDLE, new WaitForLoadStateOptions());
            page.waitForSelector(elementLocator, new WaitForSelectorOptions().setState(WaitForSelectorState.ATTACHED).setTimeout(Double
                    .parseDouble(System.getProperty("defaultElementIdentificationTimeout").trim())*1000));
        	matchingElementCount = page.querySelectorAll(elementLocator).size();
        }catch(Exception rootCauseException) {
        	failAction(page, "", elementLocator, rootCauseException);
        }
        return matchingElementCount;
    }

    private static boolean identifyUniqueElement(Page page, String elementLocator,
                                                 boolean checkForVisibility) {
        int matchingElementsCount = getMatchingElementsCount(page, elementLocator);
        if (elementLocator != null) {
            // unique element found
            switch (matchingElementsCount) {
                case 0 -> failAction(page, "zero elements found matching this locator \"" + elementLocator + "\".", elementLocator);
                case 1 -> {
                    if (checkForVisibility
                            && !elementLocator.contains("input[@type='file']")
                            && !elementLocator.contains("html")
                            && Boolean.FALSE.equals(page.isVisible(elementLocator))) {
                        failAction(page, "element is not visible.", elementLocator);
                    }
                    return true;
                }
                default -> {
                    if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckElementLocatorIsUnique")))) {
                        failAction(page, "multiple elements found matching this locator \"" + elementLocator + "\".",
                                elementLocator);
                    }
                    return true;
                }
            }
        } else {
            failAction(page, "element locator is NULL.", null);
        }
        return false;
    }
    
    private static void passAction(Page page, String elementLocator, List<Object> screenshot) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(page, elementLocator, actionName, null, screenshot);
    }

    private static void passAction(Page page, String elementLocator, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(page, elementLocator, actionName, testData, null);
    }

    private static void passAction(Page page, String elementLocator, String testData, List<Object> screenshot) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(page, elementLocator, actionName, testData, screenshot);
    }

    private static void passAction(Page page) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(page, null, actionName, null, null);
    }

    private static void passAction(Page page, String elementLocator, String actionName, String testData,
                                   List<Object> screenshot) {
        reportActionResult(page, actionName, testData, elementLocator, screenshot, true);
    }

    private static String readTextBasedOnSuccessfulLocationStrategy(Page page, String elementLocator,
                                                                    TextDetectionStrategy successfulTextLocationStrategy) {
        String actualText = "";
        switch (successfulTextLocationStrategy) {
            case TEXT -> actualText = page.querySelector(elementLocator).innerText();
            case CONTENT -> actualText = page.querySelector(elementLocator).textContent();
            case VALUE -> actualText = (String) page.querySelector(elementLocator).evaluate("node => node.value");
            default -> {
            }
        }
        return actualText;
    }

    private static String reportActionResult(Page page, String actionName, String testData, String elementLocator,
                                             List<Object> screenshot, Boolean passFailStatus) {
        actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "Element Action [" + actionName + "] successfully performed.";
        } else {
            message = "Element Action [" + actionName + "] failed.";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Element Action Test Data - " + actionName,
                    "Actual Value", testData);
            attachments.add(actualValueAttachment);
        } else if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data [" + testData + "].";
        }

        if (screenshot != null && !screenshot.equals(new ArrayList<>())) {
            // screenshot taken before action (in case of click)
            attachments.add(screenshot);
        } else if (page != null && elementLocator != null) {
            List<Object> newScreenshot = takeScreenshot(page, elementLocator, actionName, testData, passFailStatus);
            if (newScreenshot != null && !newScreenshot.equals(new ArrayList<>())) {
                attachments.add(newScreenshot);
            }
        }

        if (!attachments.equals(new ArrayList<>())) {
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.log(message);
        }
        return message;
    }

    private static String typeWrapper(Page page, String elementLocator, String targetText) {
        if (identifyUniqueElement(page, elementLocator)) {
            TextDetectionStrategy successfulTextLocationStrategy = determineSuccessfulTextLocationStrategy(page,
                    elementLocator);
            if (!successfulTextLocationStrategy.equals(TextDetectionStrategy.UNDEFINED)) {
                page.fill(elementLocator, "");
            }
            if (!"".equals(targetText)) {
                page.type(elementLocator, targetText);
            }
            if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckTextWasTypedCorrectly")))) {
                return confirmTypingWasSuccessful(page, elementLocator, targetText, successfulTextLocationStrategy);
            } else {
                return targetText;
            }
        } else {
            ReportManager.log("Failed to identify Target element with locator [" + elementLocator + "].");
            return null;
        }
    }

    /**
     * Clicks on a certain element using PlayWright, or JavaScript
     *
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions click(String elementLocator) {
        click(lastUsedPage, elementLocator);
        return this;
    }

    /**
     * Double-clicks on an element using PlayWright's Actions Library
     *
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions doubleClick(String elementLocator) {
        doubleClick(lastUsedPage, elementLocator);
        return this;
    }

    /**
     * Drags the source element and drops it onto the destination element
     *
     * @param sourceElementLocator      the locator of the source webElement that
     *                                  should be dragged under test (String xpath, id,
     *                                  selector, name ...etc)
     * @param destinationElementLocator the locator of the target webElement that
     *                                  should receive the dropped source element
     *                                  under test (String xpath, id, selector, name
     *                                  ...etc)
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions dragAndDrop(String sourceElementLocator, String destinationElementLocator) {
        dragAndDrop(lastUsedPage, sourceElementLocator, destinationElementLocator);
        return this;
    }

    /**
     * Drags the source element and drops it onto the determined offset
     *
     * @param sourceElementLocator the locator of the source webElement that should
     *                             be dragged under test (String xpath, id, selector,
     *                             name ...etc)
     * @param xOffset              the horizontal offset String which the element should
     *                             be moved
     * @param yOffset              the vertical offset String which the element should
     *                             be moved
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions dragAndDropByOffset(String sourceElementLocator, int xOffset, int yOffset) {
        dragAndDropByOffset(lastUsedPage, sourceElementLocator, xOffset, yOffset);
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
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @param attributeName  the target attribute of the webElement under test
     * @return the value of the target attribute of the webElement under test
     */
    public String getAttribute(String elementLocator, String attributeName) {
        return getAttribute(lastUsedPage, elementLocator, attributeName);
    }
    
    /**
     * Retrieves text from the target element and returns it as a string value.
     *
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return the text value of the target webElement
     */
    public String getText(String elementLocator) {
        return getText(lastUsedPage, elementLocator);
    }

    /**
     * Hovers over target element. If you want to hover on a webElement to expose
     * another webElement and click on it, use hoverAndClick instead for a more
     * reliable result.
     *
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions hover(String elementLocator) {
        hover(lastUsedPage, elementLocator);
        return this;
    }

    /**
     * Hovers over the hoverElements in sequence then clicks the clickableElement
     *
     * @param hoverElementLocators    the list of locators of the webElements under
     *                                test upon which the hover action will be
     *                                performed in sequence (String xpath, id, selector,
     *                                name ...etc)
     * @param clickableElementLocator the locator of the webElement under test upon
     *                                which the click action will be performed (By
     *                                xpath, id, selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions hoverAndClick(List<String> hoverElementLocators, String clickableElementLocator) {
        hoverAndClick(lastUsedPage, hoverElementLocators, clickableElementLocator);
        return this;
    }

    /**
     * Sends a keypress to the target element.
     *
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @param key           the key that should be pressed
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions keyPress(String elementLocator, String key) {
        keyPress(lastUsedPage, elementLocator, key);
        return this;
    }

    /**
     * Switches focus to a certain iFrame
     *
     * @param elementLocator the locator of the iFrame webElement under test (By
     *                       xpath, id, selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions switchToIframe(String elementLocator) {
        switchToIframe(lastUsedPage, elementLocator);
        return this;
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required string into the target element.
     *
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions type(String elementLocator, String text) {
        type(lastUsedPage, elementLocator, text);
        return this;
    }

    /**
     * Appends the required string into the target element, regardless of the
     * current text value.
     *
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be appended into the
     *                       target webElement
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions typeAppend(String elementLocator, String text) {
        typeAppend(lastUsedPage, elementLocator, text);
        return this;
    }

    /**
     * ValidationEnums the required file path into an input[type='file'] button, to
     * successfully upload the target file.
     *
     * @param elementLocator   the locator of the webElement under test (String xpath,
     *                         id, selector, name ...etc)
     * @param absoluteFilePath the full path to the file that needs to be uploaded
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions typeFileLocationForUpload(String elementLocator, String absoluteFilePath) {
        typeFileLocationForUpload(lastUsedPage, elementLocator, absoluteFilePath);
        return this;
    }

    /**
     * Checks if there is any text in an element, clears it, then types the required string into the target element. Obfuscates the written text in the output report. This action should be used for writing passwords and secure text.
     *
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @param text           the target text that needs to be typed into the target
     *                       webElement
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions typeSecure(String elementLocator, String text) {
        typeSecure(lastUsedPage, elementLocator, text);
        return this;
    }

    /**
     * Waits dynamically for a specific element to achieve the desired
     * stateOfPresence on the current page. Waits for a specific number of retries
     * multiplied String the default element identification timeout (in the POM.xml
     * file)
     *
     * @param elementLocator  the locator of the webElement under test (String xpath,
     *                        id, selector, name ...etc)
     * @param numberOfTries   the number of times to try and wait for the element to
     *                        achieve the desired stateOfPresence (default is 1)
     * @param stateOfPresence the expected state of presence of the element; false
     *                        is not present, and true is present
     * @return a self-reference to be used to chain actions
     */
    public PlayWrightElementActions waitForElementToBePresent(String elementLocator, int numberOfTries,
                                                    boolean stateOfPresence) {
        waitForElementToBePresent(lastUsedPage, elementLocator, numberOfTries,
                stateOfPresence);
        return this;
    }

    /**
     * Checks to see if an element is displayed
     *
     * @param elementLocator the locator of the webElement under test (String xpath, id,
     *                       selector, name ...etc)
     * @return boolean value, true if the element is displayed, and false if the
     * element is not displayed
     */
    public boolean isElementDisplayed(String elementLocator) {
        if (identifyUniqueElement(lastUsedPage, elementLocator, false)) {
            boolean isDisplayed = lastUsedPage.querySelector(elementLocator).isVisible();
            passAction(lastUsedPage, elementLocator,"");
            return isDisplayed;
        } else {
            failAction(lastUsedPage, "", elementLocator);
            return false;
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
