package com.shaft.gui.driver;

import com.google.common.annotations.Beta;
import com.shaft.gui.internal.locator.SmartLocators;
import com.shaft.gui.ocr.OcrTarget;
import org.openqa.selenium.By;

import java.util.List;
import java.util.Map;

/**
 * Public contract for element-level SHAFT actions.
 */
public interface ElementActionsContract {

    /**
     * Returns this instance to chain the next element action.
     */
    ElementActionsContract and();

    /**
     * Starts a hard assertion on the located element.
     */
    ElementAssertions assertThat(By elementLocator);

    /**
     * Starts a soft verification on the located element.
     */
    ElementAssertions verifyThat(By elementLocator);

    /**
     * Starts a hard assertion on the element found by the SHAFT locator.
     */
    default ElementAssertions assertThat(ShaftLocator elementLocator) {
        return assertThat(elementLocator.toBy());
    }

    /**
     * Starts a soft verification on the element found by the SHAFT locator.
     */
    default ElementAssertions verifyThat(ShaftLocator elementLocator) {
        return verifyThat(elementLocator.toBy());
    }

    /**
     * Starts hard assertions against a lazily composed portable element target.
     *
     * @param elementTarget portable element target
     * @return element assertions facade
     */
    default ElementAssertions assertThat(ElementTarget elementTarget) {
        return assertThat(elementTarget.toBy());
    }

    /**
     * Starts soft verifications against a lazily composed portable element target.
     *
     * @param elementTarget portable element target
     * @return element verifications facade
     */
    default ElementAssertions verifyThat(ElementTarget elementTarget) {
        return verifyThat(elementTarget.toBy());
    }

    /**
     * Returns how many elements match the locator.
     */
    int getElementsCount(By elementLocator);

    /**
     * Returns how many elements match the SHAFT locator.
     */
    default int getElementsCount(ShaftLocator elementLocator) {
        return getElementsCount(elementLocator.toBy());
    }

    /**
     * Runs a native mobile command with the given parameters.
     */
    ElementActionsContract executeNativeMobileCommand(String command, Map<String, String> parameters);

    /**
     * Clicks the located element.
     */
    ElementActionsContract click(By elementLocator);

    /** Clicks the center of visible text recognized in the current screenshot. */
    default ElementActionsContract click(OcrTarget target) {
        throw new UnsupportedOperationException("OCR click is not supported by this element actions implementation.");
    }

    /**
     * Clicks a clickable element resolved by visible text, label, or accessible name.
     *
     * @param elementName the visible text, label, or accessible name of the target element
     * @return a self-reference to be used to chain actions
     */
    @Beta
    default ElementActionsContract click(String elementName) {
        return click(SmartLocators.clickableField(elementName));
    }

    /**
     * Clicks the element found by the SHAFT locator.
     */
    default ElementActionsContract click(ShaftLocator elementLocator) {
        return click(elementLocator.toBy());
    }

    /**
     * Clicks the located element using JavaScript.
     */
    ElementActionsContract clickUsingJavascript(By elementLocator);

    /**
     * Clicks the element found by the SHAFT locator using JavaScript.
     */
    default ElementActionsContract clickUsingJavascript(ShaftLocator elementLocator) {
        return clickUsingJavascript(elementLocator.toBy());
    }

    /**
     * Scrolls the located element into view.
     */
    ElementActionsContract scrollToElement(By elementLocator);

    /**
     * Scrolls the element found by the SHAFT locator into view.
     */
    default ElementActionsContract scrollToElement(ShaftLocator elementLocator) {
        return scrollToElement(elementLocator.toBy());
    }

    /**
     * Clicks and holds the located element.
     */
    ElementActionsContract clickAndHold(By elementLocator);

    /**
     * Clicks and holds the element found by the SHAFT locator.
     */
    default ElementActionsContract clickAndHold(ShaftLocator elementLocator) {
        return clickAndHold(elementLocator.toBy());
    }

    /**
     * Double-clicks the located element.
     */
    ElementActionsContract doubleClick(By elementLocator);

    /** Double-clicks the center of visible text recognized in the current screenshot. */
    default ElementActionsContract doubleClick(OcrTarget target) {
        throw new UnsupportedOperationException("OCR double-click is not supported by this element actions implementation.");
    }

    /**
     * Double-clicks the element found by the SHAFT locator.
     */
    default ElementActionsContract doubleClick(ShaftLocator elementLocator) {
        return doubleClick(elementLocator.toBy());
    }

    /**
     * Drags the source element and drops it on the destination element.
     */
    ElementActionsContract dragAndDrop(By sourceElementLocator, By destinationElementLocator);

    /**
     * Drags the source element and drops it on the destination element, both found by SHAFT
     * locators.
     */
    default ElementActionsContract dragAndDrop(ShaftLocator sourceElementLocator, ShaftLocator destinationElementLocator) {
        return dragAndDrop(sourceElementLocator.toBy(), destinationElementLocator.toBy());
    }

    /**
     * Drags the source element by the given pixel offset.
     */
    ElementActionsContract dragAndDropByOffset(By sourceElementLocator, int xOffset, int yOffset);

    /**
     * Drags the element found by the SHAFT locator by the given pixel offset.
     */
    default ElementActionsContract dragAndDropByOffset(ShaftLocator sourceElementLocator, int xOffset, int yOffset) {
        return dragAndDropByOffset(sourceElementLocator.toBy(), xOffset, yOffset);
    }

    /**
     * Hovers over the located element.
     */
    ElementActionsContract hover(By elementLocator);

    /** Moves the pointer to visible text recognized in the current screenshot. */
    default ElementActionsContract hover(OcrTarget target) {
        throw new UnsupportedOperationException("OCR hover is not supported by this element actions implementation.");
    }

    /**
     * Hovers over the element found by the SHAFT locator.
     */
    default ElementActionsContract hover(ShaftLocator elementLocator) {
        return hover(elementLocator.toBy());
    }

    /**
     * Hovers over each element in order, then clicks the final element.
     */
    ElementActionsContract hoverAndClick(List<By> hoverElementLocators, By clickableElementLocator);

    /**
     * Selects the drop-down option whose value or visible text matches.
     */
    ElementActionsContract select(By elementLocator, String valueOrVisibleText);

    /**
     * Selects the drop-down option whose value or visible text matches, in the element found by the
     * SHAFT locator.
     */
    default ElementActionsContract select(ShaftLocator elementLocator, String valueOrVisibleText) {
        return select(elementLocator.toBy(), valueOrVisibleText);
    }

    /**
     * Sets the value of the located element using JavaScript.
     */
    ElementActionsContract setValueUsingJavaScript(By elementLocator, String value);

    /**
     * Sets the value of the element found by the SHAFT locator using JavaScript.
     */
    default ElementActionsContract setValueUsingJavaScript(ShaftLocator elementLocator, String value) {
        return setValueUsingJavaScript(elementLocator.toBy(), value);
    }

    /**
     * Submits the form of the located element using JavaScript.
     */
    ElementActionsContract submitFormUsingJavaScript(By elementLocator);

    /**
     * Submits the form of the element found by the SHAFT locator using JavaScript.
     */
    default ElementActionsContract submitFormUsingJavaScript(ShaftLocator elementLocator) {
        return submitFormUsingJavaScript(elementLocator.toBy());
    }

    /**
     * Switches into the located iframe.
     */
    ElementActionsContract switchToIframe(By elementLocator);

    /**
     * Switches into the iframe found by the SHAFT locator.
     */
    default ElementActionsContract switchToIframe(ShaftLocator elementLocator) {
        return switchToIframe(elementLocator.toBy());
    }

    /**
     * Switches back to the top-level page from any iframe.
     */
    ElementActionsContract switchToDefaultContent();

    /**
     * Switches focus from the current iframe to its parent frame.
     *
     * @return a self-reference to be used to chain actions
     */
    default ElementActionsContract switchToParentFrame() {
        throw new UnsupportedOperationException("switchToParentFrame is not supported by this element actions implementation.");
    }

    /**
     * Returns the name of the current frame.
     */
    String getCurrentFrame();

    /**
     * Replaces the value of the located element with the given text.
     */
    ElementActionsContract type(By elementLocator, CharSequence... text);

    /**
     * Types into an input resolved by visible label, placeholder, or accessible name.
     *
     * @param elementName the visible label, placeholder, or accessible name of the target input
     * @param text        one or more character sequences to type
     * @return a self-reference to be used to chain actions
     */
    @Beta
    default ElementActionsContract type(String elementName, CharSequence... text) {
        return type(SmartLocators.inputField(elementName), text);
    }

    /**
     * Replaces the value of the element found by the SHAFT locator with the given text.
     */
    default ElementActionsContract type(ShaftLocator elementLocator, CharSequence... text) {
        return type(elementLocator.toBy(), text);
    }

    /**
     * Clears the value of the located element.
     */
    ElementActionsContract clear(By elementLocator);

    /**
     * Clears the value of the element found by the SHAFT locator.
     */
    default ElementActionsContract clear(ShaftLocator elementLocator) {
        return clear(elementLocator.toBy());
    }

    /**
     * Appends the given text to the value of the located element.
     */
    ElementActionsContract typeAppend(By elementLocator, CharSequence... text);

    /**
     * Appends the given text to the value of the element found by the SHAFT locator.
     */
    default ElementActionsContract typeAppend(ShaftLocator elementLocator, CharSequence... text) {
        return typeAppend(elementLocator.toBy(), text);
    }

    /**
     * Types a file path into the located file input to upload it.
     */
    ElementActionsContract typeFileLocationForUpload(By elementLocator, String filePath);

    /**
     * Types a file path into the file input found by the SHAFT locator to upload it.
     */
    default ElementActionsContract typeFileLocationForUpload(ShaftLocator elementLocator, String filePath) {
        return typeFileLocationForUpload(elementLocator.toBy(), filePath);
    }

    /**
     * Types sensitive text into the located element and masks it in the report.
     */
    ElementActionsContract typeSecure(By elementLocator, CharSequence... text);

    /**
     * Types sensitive text into the element found by the SHAFT locator and masks it in the report.
     */
    default ElementActionsContract typeSecure(ShaftLocator elementLocator, CharSequence... text) {
        return typeSecure(elementLocator.toBy(), text);
    }

    /**
     * Reads the located table into a list of rows, each mapping a column header to its cell text.
     */
    List<Map<String, String>> getTableRowsData(By tableLocator);

    /**
     * Reads the table found by the SHAFT locator into a list of rows, each mapping a column header
     * to its cell text.
     */
    default List<Map<String, String>> getTableRowsData(ShaftLocator tableLocator) {
        return getTableRowsData(tableLocator.toBy());
    }

    /**
     * Attaches a screenshot of the located element to the report.
     */
    ElementActionsContract captureScreenshot(By elementLocator);

    /**
     * Attaches a screenshot of the element found by the SHAFT locator to the report.
     */
    default ElementActionsContract captureScreenshot(ShaftLocator elementLocator) {
        return captureScreenshot(elementLocator.toBy());
    }

    /**
     * Captures an accessible-name-tree ("aria") snapshot of the target element, serialized as YAML.
     *
     * @param elementLocator the locator of the element to snapshot
     * @return the captured snapshot serialized as YAML
     */
    String ariaSnapshot(By elementLocator);

    /**
     * Returns the ARIA snapshot of the element found by the SHAFT locator.
     */
    default String ariaSnapshot(ShaftLocator elementLocator) {
        return ariaSnapshot(elementLocator.toBy());
    }
}
