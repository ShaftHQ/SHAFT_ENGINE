package com.shaft.gui.driver;

import org.openqa.selenium.By;

import java.util.List;
import java.util.Map;

/**
 * Public contract for element-level SHAFT actions.
 */
public interface ElementActionsContract {

    ElementActionsContract and();

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

    ElementActionsContract executeNativeMobileCommand(String command, Map<String, String> parameters);

    ElementActionsContract click(By elementLocator);

    default ElementActionsContract click(ShaftLocator elementLocator) {
        return click(elementLocator.toBy());
    }

    ElementActionsContract clickUsingJavascript(By elementLocator);

    default ElementActionsContract clickUsingJavascript(ShaftLocator elementLocator) {
        return clickUsingJavascript(elementLocator.toBy());
    }

    ElementActionsContract scrollToElement(By elementLocator);

    default ElementActionsContract scrollToElement(ShaftLocator elementLocator) {
        return scrollToElement(elementLocator.toBy());
    }

    ElementActionsContract clickAndHold(By elementLocator);

    default ElementActionsContract clickAndHold(ShaftLocator elementLocator) {
        return clickAndHold(elementLocator.toBy());
    }

    ElementActionsContract doubleClick(By elementLocator);

    default ElementActionsContract doubleClick(ShaftLocator elementLocator) {
        return doubleClick(elementLocator.toBy());
    }

    ElementActionsContract dragAndDrop(By sourceElementLocator, By destinationElementLocator);

    default ElementActionsContract dragAndDrop(ShaftLocator sourceElementLocator, ShaftLocator destinationElementLocator) {
        return dragAndDrop(sourceElementLocator.toBy(), destinationElementLocator.toBy());
    }

    ElementActionsContract dragAndDropByOffset(By sourceElementLocator, int xOffset, int yOffset);

    default ElementActionsContract dragAndDropByOffset(ShaftLocator sourceElementLocator, int xOffset, int yOffset) {
        return dragAndDropByOffset(sourceElementLocator.toBy(), xOffset, yOffset);
    }

    ElementActionsContract hover(By elementLocator);

    default ElementActionsContract hover(ShaftLocator elementLocator) {
        return hover(elementLocator.toBy());
    }

    ElementActionsContract hoverAndClick(List<By> hoverElementLocators, By clickableElementLocator);

    ElementActionsContract select(By elementLocator, String valueOrVisibleText);

    default ElementActionsContract select(ShaftLocator elementLocator, String valueOrVisibleText) {
        return select(elementLocator.toBy(), valueOrVisibleText);
    }

    ElementActionsContract setValueUsingJavaScript(By elementLocator, String value);

    default ElementActionsContract setValueUsingJavaScript(ShaftLocator elementLocator, String value) {
        return setValueUsingJavaScript(elementLocator.toBy(), value);
    }

    ElementActionsContract submitFormUsingJavaScript(By elementLocator);

    default ElementActionsContract submitFormUsingJavaScript(ShaftLocator elementLocator) {
        return submitFormUsingJavaScript(elementLocator.toBy());
    }

    ElementActionsContract switchToIframe(By elementLocator);

    default ElementActionsContract switchToIframe(ShaftLocator elementLocator) {
        return switchToIframe(elementLocator.toBy());
    }

    ElementActionsContract switchToDefaultContent();

    String getCurrentFrame();

    ElementActionsContract type(By elementLocator, CharSequence... text);

    default ElementActionsContract type(ShaftLocator elementLocator, CharSequence... text) {
        return type(elementLocator.toBy(), text);
    }

    ElementActionsContract clear(By elementLocator);

    default ElementActionsContract clear(ShaftLocator elementLocator) {
        return clear(elementLocator.toBy());
    }

    ElementActionsContract typeAppend(By elementLocator, CharSequence... text);

    default ElementActionsContract typeAppend(ShaftLocator elementLocator, CharSequence... text) {
        return typeAppend(elementLocator.toBy(), text);
    }

    ElementActionsContract typeFileLocationForUpload(By elementLocator, String filePath);

    default ElementActionsContract typeFileLocationForUpload(ShaftLocator elementLocator, String filePath) {
        return typeFileLocationForUpload(elementLocator.toBy(), filePath);
    }

    ElementActionsContract typeSecure(By elementLocator, CharSequence... text);

    default ElementActionsContract typeSecure(ShaftLocator elementLocator, CharSequence... text) {
        return typeSecure(elementLocator.toBy(), text);
    }

    List<Map<String, String>> getTableRowsData(By tableLocator);

    default List<Map<String, String>> getTableRowsData(ShaftLocator tableLocator) {
        return getTableRowsData(tableLocator.toBy());
    }

    ElementActionsContract captureScreenshot(By elementLocator);

    default ElementActionsContract captureScreenshot(ShaftLocator elementLocator) {
        return captureScreenshot(elementLocator.toBy());
    }
}
