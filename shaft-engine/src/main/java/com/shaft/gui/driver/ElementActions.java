package com.shaft.gui.driver;

import org.openqa.selenium.By;

import java.util.List;
import java.util.Map;

/**
 * Public contract for element-level SHAFT actions.
 */
public interface ElementActions {

    ElementActions and();

    ElementAssertions assertThat(By elementLocator);

    ElementAssertions verifyThat(By elementLocator);

    default ElementAssertions assertThat(ShaftLocator elementLocator) {
        return assertThat(elementLocator.toBy());
    }

    default ElementAssertions verifyThat(ShaftLocator elementLocator) {
        return verifyThat(elementLocator.toBy());
    }

    int getElementsCount(By elementLocator);

    default int getElementsCount(ShaftLocator elementLocator) {
        return getElementsCount(elementLocator.toBy());
    }

    ElementActions executeNativeMobileCommand(String command, Map<String, String> parameters);

    ElementActions click(By elementLocator);

    default ElementActions click(ShaftLocator elementLocator) {
        return click(elementLocator.toBy());
    }

    ElementActions clickUsingJavascript(By elementLocator);

    default ElementActions clickUsingJavascript(ShaftLocator elementLocator) {
        return clickUsingJavascript(elementLocator.toBy());
    }

    ElementActions scrollToElement(By elementLocator);

    default ElementActions scrollToElement(ShaftLocator elementLocator) {
        return scrollToElement(elementLocator.toBy());
    }

    ElementActions clickAndHold(By elementLocator);

    default ElementActions clickAndHold(ShaftLocator elementLocator) {
        return clickAndHold(elementLocator.toBy());
    }

    ElementActions doubleClick(By elementLocator);

    default ElementActions doubleClick(ShaftLocator elementLocator) {
        return doubleClick(elementLocator.toBy());
    }

    ElementActions dragAndDrop(By sourceElementLocator, By destinationElementLocator);

    default ElementActions dragAndDrop(ShaftLocator sourceElementLocator, ShaftLocator destinationElementLocator) {
        return dragAndDrop(sourceElementLocator.toBy(), destinationElementLocator.toBy());
    }

    ElementActions dragAndDropByOffset(By sourceElementLocator, int xOffset, int yOffset);

    default ElementActions dragAndDropByOffset(ShaftLocator sourceElementLocator, int xOffset, int yOffset) {
        return dragAndDropByOffset(sourceElementLocator.toBy(), xOffset, yOffset);
    }

    ElementActions hover(By elementLocator);

    default ElementActions hover(ShaftLocator elementLocator) {
        return hover(elementLocator.toBy());
    }

    ElementActions hoverAndClick(List<By> hoverElementLocators, By clickableElementLocator);

    ElementActions select(By elementLocator, String valueOrVisibleText);

    default ElementActions select(ShaftLocator elementLocator, String valueOrVisibleText) {
        return select(elementLocator.toBy(), valueOrVisibleText);
    }

    ElementActions setValueUsingJavaScript(By elementLocator, String value);

    default ElementActions setValueUsingJavaScript(ShaftLocator elementLocator, String value) {
        return setValueUsingJavaScript(elementLocator.toBy(), value);
    }

    ElementActions submitFormUsingJavaScript(By elementLocator);

    default ElementActions submitFormUsingJavaScript(ShaftLocator elementLocator) {
        return submitFormUsingJavaScript(elementLocator.toBy());
    }

    ElementActions switchToIframe(By elementLocator);

    default ElementActions switchToIframe(ShaftLocator elementLocator) {
        return switchToIframe(elementLocator.toBy());
    }

    ElementActions switchToDefaultContent();

    String getCurrentFrame();

    ElementActions type(By elementLocator, CharSequence... text);

    default ElementActions type(ShaftLocator elementLocator, CharSequence... text) {
        return type(elementLocator.toBy(), text);
    }

    ElementActions clear(By elementLocator);

    default ElementActions clear(ShaftLocator elementLocator) {
        return clear(elementLocator.toBy());
    }

    ElementActions typeAppend(By elementLocator, CharSequence... text);

    default ElementActions typeAppend(ShaftLocator elementLocator, CharSequence... text) {
        return typeAppend(elementLocator.toBy(), text);
    }

    ElementActions typeFileLocationForUpload(By elementLocator, String filePath);

    default ElementActions typeFileLocationForUpload(ShaftLocator elementLocator, String filePath) {
        return typeFileLocationForUpload(elementLocator.toBy(), filePath);
    }

    ElementActions typeSecure(By elementLocator, CharSequence... text);

    default ElementActions typeSecure(ShaftLocator elementLocator, CharSequence... text) {
        return typeSecure(elementLocator.toBy(), text);
    }

    List<Map<String, String>> getTableRowsData(By tableLocator);

    default List<Map<String, String>> getTableRowsData(ShaftLocator tableLocator) {
        return getTableRowsData(tableLocator.toBy());
    }

    ElementActions captureScreenshot(By elementLocator);

    default ElementActions captureScreenshot(ShaftLocator elementLocator) {
        return captureScreenshot(elementLocator.toBy());
    }
}
