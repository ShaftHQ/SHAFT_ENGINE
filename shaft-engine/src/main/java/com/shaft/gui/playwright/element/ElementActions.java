package com.shaft.gui.playwright.element;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.options.BoundingBox;
import com.shaft.gui.driver.ElementAssertions;
import com.shaft.gui.driver.ShaftLocator;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.playwright.validation.PlaywrightElementValidationsBuilder;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;

import java.io.ByteArrayInputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class ElementActions implements com.shaft.gui.driver.ElementActionsContract {
    private final PlaywrightSession session;

    public ElementActions(PlaywrightSession session) {
        this.session = session;
    }

    @Override
    public ElementActions and() {
        return this;
    }

    @Override
    public ElementAssertions assertThat(By elementLocator) {
        return assertThat(ShaftLocator.from(elementLocator));
    }

    @Override
    public ElementAssertions assertThat(ShaftLocator elementLocator) {
        return assertThat(elementLocator.toPlaywrightLocator(session.page()));
    }

    public ElementAssertions assertThat(Locator elementLocator) {
        return new PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT, session,
                elementLocator);
    }

    @Override
    public ElementAssertions verifyThat(By elementLocator) {
        return verifyThat(ShaftLocator.from(elementLocator));
    }

    @Override
    public ElementAssertions verifyThat(ShaftLocator elementLocator) {
        return verifyThat(elementLocator.toPlaywrightLocator(session.page()));
    }

    public ElementAssertions verifyThat(Locator elementLocator) {
        return new PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT, session,
                elementLocator);
    }

    @Override
    public int getElementsCount(By elementLocator) {
        return resolve(elementLocator).count();
    }

    @Override
    public int getElementsCount(ShaftLocator elementLocator) {
        return resolve(elementLocator).count();
    }

    public int getElementsCount(Locator elementLocator) {
        return elementLocator.count();
    }

    @Override
    public ElementActions executeNativeMobileCommand(String command, Map<String, String> parameters) {
        throw unsupported("native Appium mobile commands");
    }

    @Override
    public ElementActions click(By elementLocator) {
        return click(resolve(elementLocator));
    }

    @Override
    public ElementActions click(ShaftLocator elementLocator) {
        return click(resolve(elementLocator));
    }

    public ElementActions click(Locator elementLocator) {
        elementLocator.click();
        ReportManager.log("Clicked Playwright element.");
        return this;
    }

    @Override
    public ElementActions clickUsingJavascript(By elementLocator) {
        return clickUsingJavascript(resolve(elementLocator));
    }

    @Override
    public ElementActions clickUsingJavascript(ShaftLocator elementLocator) {
        return clickUsingJavascript(resolve(elementLocator));
    }

    public ElementActions clickUsingJavascript(Locator elementLocator) {
        elementLocator.evaluate("element => element.click()");
        return this;
    }

    @Override
    public ElementActions scrollToElement(By elementLocator) {
        return scrollToElement(resolve(elementLocator));
    }

    @Override
    public ElementActions scrollToElement(ShaftLocator elementLocator) {
        return scrollToElement(resolve(elementLocator));
    }

    public ElementActions scrollToElement(Locator elementLocator) {
        elementLocator.scrollIntoViewIfNeeded();
        return this;
    }

    @Override
    public ElementActions clickAndHold(By elementLocator) {
        return clickAndHold(resolve(elementLocator));
    }

    @Override
    public ElementActions clickAndHold(ShaftLocator elementLocator) {
        return clickAndHold(resolve(elementLocator));
    }

    public ElementActions clickAndHold(Locator elementLocator) {
        BoundingBox box = elementLocator.boundingBox();
        session.page().mouse().move(box.x + box.width / 2, box.y + box.height / 2);
        session.page().mouse().down();
        return this;
    }

    @Override
    public ElementActions doubleClick(By elementLocator) {
        return doubleClick(resolve(elementLocator));
    }

    @Override
    public ElementActions doubleClick(ShaftLocator elementLocator) {
        return doubleClick(resolve(elementLocator));
    }

    public ElementActions doubleClick(Locator elementLocator) {
        elementLocator.dblclick();
        return this;
    }

    @Override
    public ElementActions dragAndDrop(By sourceElementLocator, By destinationElementLocator) {
        return dragAndDrop(resolve(sourceElementLocator), resolve(destinationElementLocator));
    }

    @Override
    public ElementActions dragAndDrop(ShaftLocator sourceElementLocator, ShaftLocator destinationElementLocator) {
        return dragAndDrop(resolve(sourceElementLocator), resolve(destinationElementLocator));
    }

    public ElementActions dragAndDrop(Locator sourceElementLocator, Locator destinationElementLocator) {
        sourceElementLocator.dragTo(destinationElementLocator);
        return this;
    }

    @Override
    public ElementActions dragAndDropByOffset(By sourceElementLocator, int xOffset, int yOffset) {
        return dragAndDropByOffset(resolve(sourceElementLocator), xOffset, yOffset);
    }

    @Override
    public ElementActions dragAndDropByOffset(ShaftLocator sourceElementLocator, int xOffset, int yOffset) {
        return dragAndDropByOffset(resolve(sourceElementLocator), xOffset, yOffset);
    }

    public ElementActions dragAndDropByOffset(Locator sourceElementLocator, int xOffset, int yOffset) {
        BoundingBox box = sourceElementLocator.boundingBox();
        double x = box.x + box.width / 2;
        double y = box.y + box.height / 2;
        session.page().mouse().move(x, y);
        session.page().mouse().down();
        session.page().mouse().move(x + xOffset, y + yOffset);
        session.page().mouse().up();
        return this;
    }

    @Override
    public ElementActions hover(By elementLocator) {
        return hover(resolve(elementLocator));
    }

    @Override
    public ElementActions hover(ShaftLocator elementLocator) {
        return hover(resolve(elementLocator));
    }

    public ElementActions hover(Locator elementLocator) {
        elementLocator.hover();
        return this;
    }

    @Override
    public ElementActions hoverAndClick(List<By> hoverElementLocators, By clickableElementLocator) {
        hoverElementLocators.forEach(this::hover);
        return click(clickableElementLocator);
    }

    @Override
    public ElementActions select(By elementLocator, String valueOrVisibleText) {
        return select(resolve(elementLocator), valueOrVisibleText);
    }

    @Override
    public ElementActions select(ShaftLocator elementLocator, String valueOrVisibleText) {
        return select(resolve(elementLocator), valueOrVisibleText);
    }

    public ElementActions select(Locator elementLocator, String valueOrVisibleText) {
        elementLocator.selectOption(valueOrVisibleText);
        return this;
    }

    @Override
    public ElementActions setValueUsingJavaScript(By elementLocator, String value) {
        return setValueUsingJavaScript(resolve(elementLocator), value);
    }

    @Override
    public ElementActions setValueUsingJavaScript(ShaftLocator elementLocator, String value) {
        return setValueUsingJavaScript(resolve(elementLocator), value);
    }

    public ElementActions setValueUsingJavaScript(Locator elementLocator, String value) {
        elementLocator.evaluate("(element, value) => { element.value = value; element.dispatchEvent(new Event('input', {bubbles: true})); element.dispatchEvent(new Event('change', {bubbles: true})); }",
                value);
        return this;
    }

    @Override
    public ElementActions submitFormUsingJavaScript(By elementLocator) {
        return submitFormUsingJavaScript(resolve(elementLocator));
    }

    @Override
    public ElementActions submitFormUsingJavaScript(ShaftLocator elementLocator) {
        return submitFormUsingJavaScript(resolve(elementLocator));
    }

    public ElementActions submitFormUsingJavaScript(Locator elementLocator) {
        elementLocator.evaluate("element => element.form ? element.form.submit() : element.submit()");
        return this;
    }

    @Override
    public ElementActions switchToIframe(By elementLocator) {
        throw unsupported("WebDriver iframe switching");
    }

    @Override
    public ElementActions switchToDefaultContent() {
        return this;
    }

    @Override
    public String getCurrentFrame() {
        return "PLAYWRIGHT_PAGE";
    }

    @Override
    public ElementActions type(By elementLocator, CharSequence... text) {
        return type(resolve(elementLocator), text);
    }

    @Override
    public ElementActions type(ShaftLocator elementLocator, CharSequence... text) {
        return type(resolve(elementLocator), text);
    }

    public ElementActions type(Locator elementLocator, CharSequence... text) {
        elementLocator.fill(join(text));
        return this;
    }

    @Override
    public ElementActions clear(By elementLocator) {
        return clear(resolve(elementLocator));
    }

    @Override
    public ElementActions clear(ShaftLocator elementLocator) {
        return clear(resolve(elementLocator));
    }

    public ElementActions clear(Locator elementLocator) {
        elementLocator.clear();
        return this;
    }

    @Override
    public ElementActions typeAppend(By elementLocator, CharSequence... text) {
        return typeAppend(resolve(elementLocator), text);
    }

    @Override
    public ElementActions typeAppend(ShaftLocator elementLocator, CharSequence... text) {
        return typeAppend(resolve(elementLocator), text);
    }

    public ElementActions typeAppend(Locator elementLocator, CharSequence... text) {
        elementLocator.pressSequentially(join(text));
        return this;
    }

    @Override
    public ElementActions typeFileLocationForUpload(By elementLocator, String filePath) {
        return typeFileLocationForUpload(resolve(elementLocator), filePath);
    }

    @Override
    public ElementActions typeFileLocationForUpload(ShaftLocator elementLocator, String filePath) {
        return typeFileLocationForUpload(resolve(elementLocator), filePath);
    }

    public ElementActions typeFileLocationForUpload(Locator elementLocator, String filePath) {
        elementLocator.setInputFiles(Path.of(filePath));
        return this;
    }

    @Override
    public ElementActions typeSecure(By elementLocator, CharSequence... text) {
        return typeSecure(resolve(elementLocator), text);
    }

    @Override
    public ElementActions typeSecure(ShaftLocator elementLocator, CharSequence... text) {
        return typeSecure(resolve(elementLocator), text);
    }

    public ElementActions typeSecure(Locator elementLocator, CharSequence... text) {
        elementLocator.fill(join(text));
        ReportManager.log("Typed secure text into Playwright element.");
        return this;
    }

    @Override
    public List<Map<String, String>> getTableRowsData(By tableLocator) {
        return getTableRowsData(resolve(tableLocator));
    }

    @Override
    public List<Map<String, String>> getTableRowsData(ShaftLocator tableLocator) {
        return getTableRowsData(resolve(tableLocator));
    }

    @SuppressWarnings("unchecked")
    public List<Map<String, String>> getTableRowsData(Locator tableLocator) {
        List<List<String>> rows = (List<List<String>>) tableLocator.evaluate(
                "table => Array.from(table.querySelectorAll('tr')).map(tr => Array.from(tr.querySelectorAll('th,td')).map(cell => cell.innerText))");
        List<Map<String, String>> mappedRows = new ArrayList<>();
        if (rows == null || rows.isEmpty()) {
            return mappedRows;
        }
        List<String> headers = rows.getFirst();
        for (int rowIndex = 1; rowIndex < rows.size(); rowIndex++) {
            Map<String, String> row = new LinkedHashMap<>();
            List<String> cells = rows.get(rowIndex);
            for (int cellIndex = 0; cellIndex < cells.size(); cellIndex++) {
                String key = cellIndex < headers.size() && !headers.get(cellIndex).isBlank()
                        ? headers.get(cellIndex)
                        : "column" + (cellIndex + 1);
                row.put(key, cells.get(cellIndex));
            }
            mappedRows.add(row);
        }
        return mappedRows;
    }

    @Override
    public ElementActions captureScreenshot(By elementLocator) {
        return captureScreenshot(resolve(elementLocator));
    }

    @Override
    public ElementActions captureScreenshot(ShaftLocator elementLocator) {
        return captureScreenshot(resolve(elementLocator));
    }

    public ElementActions captureScreenshot(Locator elementLocator) {
        byte[] screenshot = elementLocator.screenshot();
        ReportManagerHelper.attach("Playwright Screenshot", "element.png", new ByteArrayInputStream(screenshot));
        ReportManager.log("Captured Playwright element screenshot.");
        return this;
    }

    private Locator resolve(By locator) {
        return ShaftLocator.from(locator).toPlaywrightLocator(session.page());
    }

    private Locator resolve(ShaftLocator locator) {
        return locator.toPlaywrightLocator(session.page());
    }

    private String join(CharSequence... text) {
        StringBuilder builder = new StringBuilder();
        for (CharSequence value : text) {
            builder.append(value);
        }
        return builder.toString();
    }

    private UnsupportedOperationException unsupported(String capability) {
        return new UnsupportedOperationException(capability + " is WebDriver-specific in SHAFT and is not available through the Playwright backend.");
    }
}
