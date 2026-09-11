package com.shaft.gui.playwright.element;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.options.BoundingBox;
import com.google.common.annotations.Beta;
import com.shaft.gui.driver.ElementAssertions;
import com.shaft.gui.driver.ElementTarget;
import com.shaft.gui.driver.ShaftLocator;
import com.shaft.gui.element.internal.interaction.ElementClassifier;
import com.shaft.gui.element.internal.interaction.ElementKind;
import com.shaft.gui.element.internal.interaction.ElementSignals;
import com.shaft.gui.element.internal.interaction.TypeStrategies;
import com.shaft.gui.internal.aria.AriaSnapshotHelper;
import com.shaft.gui.internal.ocr.OcrCoordinateMapper;
import com.shaft.gui.internal.ocr.OcrPoint;
import com.shaft.gui.internal.ocr.OcrProcessingActions;
import com.shaft.gui.ocr.OcrTarget;
import com.shaft.gui.internal.locator.CompositeLocator;
import com.shaft.gui.internal.locator.SmartLocators;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.playwright.validation.PlaywrightElementValidationsBuilder;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.BrowserPerformanceExecutionReport;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.tools.io.internal.TraceEventRecorder;
import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;

import java.io.ByteArrayInputStream;
import javax.imageio.ImageIO;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Playwright element actions. Click uses Playwright actionability waits (visible, stable,
 * enabled, receives events). {@code force}/{@code trial} click options are intentionally
 * not exposed on the public API (default off); use {@link #clickUsingJavascript} only when
 * an intentional bypass is required.
 * <p>
 * Open Shadow DOM is reachable via Playwright locator pierce (CSS/role locators). Closed
 * shadow roots are unsupported for pierce — interact through the host's public API or
 * accessibility tree instead.
 * <p>
 * {@link #type} chooses {@code fill} vs {@code pressSequentially} by element kind
 * (epic #5732 Wave C): plain inputs stay {@code fill}; contenteditable / masked inputs
 * use sequential key presses.
 */
public class ElementActions implements com.shaft.gui.driver.ElementActionsContract {

    /**
     * Single evaluate payload for Wave C classify + masked heuristic (keep in sync with
     * {@link ElementClassifier#fromEvaluateMap}).
     */
    private static final String ELEMENT_SIGNALS_SCRIPT = """
            el => ({
              tagName: el.tagName || '',
              type: el.getAttribute('type') || el.type || '',
              role: el.getAttribute('role') || '',
              contentEditable: el.getAttribute('contenteditable'),
              isContentEditable: el.isContentEditable === true ? 'true' : 'false',
              disabled: el.disabled === true ? 'true'
                : (el.getAttribute('disabled') === null ? null : (el.getAttribute('disabled') || 'disabled')),
              readonly: el.readOnly === true ? 'true'
                : (el.getAttribute('readonly') === null ? null : (el.getAttribute('readonly') || 'readonly')),
              ariaDisabled: el.getAttribute('aria-disabled'),
              dataMask: el.getAttribute('data-mask'),
              dataInputmask: el.getAttribute('data-inputmask') || el.getAttribute('data-input-mask'),
              mask: el.getAttribute('mask'),
              className: typeof el.className === 'string' ? el.className : (el.getAttribute('class') || ''),
              autocomplete: el.getAttribute('autocomplete')
            })
            """;

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
        return assertThat(ElementTarget.located(elementLocator));
    }

    @Override
    public ElementAssertions assertThat(ElementTarget elementTarget) {
        return new PlaywrightElementValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT, session, elementTarget);
    }

    public ElementAssertions assertThat(Locator elementLocator) {
        return new PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT, session,
                elementLocator, String.valueOf(elementLocator));
    }

    @Override
    public ElementAssertions verifyThat(By elementLocator) {
        return verifyThat(ShaftLocator.from(elementLocator));
    }

    @Override
    public ElementAssertions verifyThat(ShaftLocator elementLocator) {
        return verifyThat(ElementTarget.located(elementLocator));
    }

    @Override
    public ElementAssertions verifyThat(ElementTarget elementTarget) {
        return new PlaywrightElementValidationsBuilder(
                ValidationEnums.ValidationCategory.SOFT_ASSERT, session, elementTarget);
    }

    public ElementAssertions verifyThat(Locator elementLocator) {
        return new PlaywrightElementValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT, session,
                elementLocator, String.valueOf(elementLocator));
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

    @Override
    public ElementActions click(OcrTarget target) {
        return ocrPointerAction(target, OcrPointerGesture.CLICK);
    }

    /**
     * Clicks a clickable element resolved by visible text, label, or accessible name.
     *
     * @param elementName the visible text, label, or accessible name of the target element
     * @return a self-reference to be used to chain actions
     */
    @Beta
    @Override
    public ElementActions click(String elementName) {
        return click(SmartLocators.clickableField(elementName));
    }

    /**
     * Clicks using Playwright actionability (waits until actionable). Does not set
     * {@code force} or {@code trial} — both stay default-off and are not part of the
     * public SHAFT API. Open shadow pierce works via the locator engine; closed shadow
     * is unsupported (use host public API / a11y).
     */
    public ElementActions click(Locator elementLocator) {
        return timed("playwright.element.click", elementLocator, () -> {
            elementLocator.click();
            ReportManager.log("Clicked Playwright element.");
        });
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
        return timed("playwright.element.clickUsingJavascript", elementLocator,
                () -> elementLocator.evaluate("element => element.click()"));
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
        return timed("playwright.element.scrollToElement", elementLocator, elementLocator::scrollIntoViewIfNeeded);
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
        return timed("playwright.element.clickAndHold", elementLocator, () -> {
            BoundingBox box = elementLocator.boundingBox();
            session.page().mouse().move(box.x + box.width / 2, box.y + box.height / 2);
            session.page().mouse().down();
        });
    }

    @Override
    public ElementActions doubleClick(By elementLocator) {
        return doubleClick(resolve(elementLocator));
    }

    @Override
    public ElementActions doubleClick(ShaftLocator elementLocator) {
        return doubleClick(resolve(elementLocator));
    }

    @Override
    public ElementActions doubleClick(OcrTarget target) {
        return ocrPointerAction(target, OcrPointerGesture.DOUBLE_CLICK);
    }

    public ElementActions doubleClick(Locator elementLocator) {
        return timed("playwright.element.doubleClick", elementLocator, elementLocator::dblclick);
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
        return timed("playwright.element.dragAndDrop", sourceElementLocator,
                () -> sourceElementLocator.dragTo(destinationElementLocator));
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
        return timed("playwright.element.dragAndDropByOffset", sourceElementLocator, () -> {
            BoundingBox box = sourceElementLocator.boundingBox();
            double x = box.x + box.width / 2;
            double y = box.y + box.height / 2;
            session.page().mouse().move(x, y);
            session.page().mouse().down();
            session.page().mouse().move(x + xOffset, y + yOffset);
            session.page().mouse().up();
        });
    }

    @Override
    public ElementActions hover(By elementLocator) {
        return hover(resolve(elementLocator));
    }

    @Override
    public ElementActions hover(ShaftLocator elementLocator) {
        return hover(resolve(elementLocator));
    }

    @Override
    public ElementActions hover(OcrTarget target) {
        return ocrPointerAction(target, OcrPointerGesture.HOVER);
    }

    public ElementActions hover(Locator elementLocator) {
        return timed("playwright.element.hover", elementLocator, elementLocator::hover);
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
        return timed("playwright.element.select", elementLocator, () -> elementLocator.selectOption(valueOrVisibleText));
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
        return timed("playwright.element.setValueUsingJavaScript", elementLocator,
                () -> elementLocator.evaluate("(element, value) => { element.value = value; element.dispatchEvent(new Event('input', {bubbles: true})); element.dispatchEvent(new Event('change', {bubbles: true})); }",
                        value));
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
        return timed("playwright.element.submitFormUsingJavaScript", elementLocator,
                () -> elementLocator.evaluate("element => element.form ? element.form.submit() : element.submit()"));
    }

    @Override
    public ElementActions switchToIframe(By elementLocator) {
        throw unsupported("WebDriver iframe switching");
    }

    @Override
    public ElementActions switchToDefaultContent() {
        return this;
    }

    /**
     * Switches focus from the current iframe to its parent frame.
     *
     * @return a self-reference to be used to chain actions
     */
    @Override
    public ElementActions switchToParentFrame() {
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

    /**
     * Types into an input resolved by visible label, placeholder, or accessible name.
     *
     * @param elementName the visible label, placeholder, or accessible name of the target input
     * @param text        one or more character sequences to type
     * @return a self-reference to be used to chain actions
     */
    @Beta
    @Override
    public ElementActions type(String elementName, CharSequence... text) {
        return type(SmartLocators.inputField(elementName), text);
    }

    public ElementActions type(Locator elementLocator, CharSequence... text) {
        return timed("playwright.element.type", elementLocator, () -> typeByKind(elementLocator, join(text), false));
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
        return timed("playwright.element.clear", elementLocator, elementLocator::clear);
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
        return timed("playwright.element.typeAppend", elementLocator, () -> {
            ElementSignals signals = readSignals(elementLocator);
            ElementKind kind = ElementClassifier.classify(signals);
            boolean masked = ElementClassifier.looksMasked(signals);
            TypeStrategies.PlaywrightTypeRoute route = TypeStrategies.playwrightRouteFor(kind, masked);
            if (route == TypeStrategies.PlaywrightTypeRoute.PRESS_SEQUENTIALLY) {
                typeByKind(elementLocator, join(text), true);
                return;
            }
            if (route == TypeStrategies.PlaywrightTypeRoute.REJECT
                    || route == TypeStrategies.PlaywrightTypeRoute.TOGGLE_CLICK
                    || route == TypeStrategies.PlaywrightTypeRoute.SELECT_OPTION
                    || route == TypeStrategies.PlaywrightTypeRoute.SET_FILES) {
                typeByKind(elementLocator, join(text), false);
                return;
            }
            elementLocator.fill(readTextForAppend(elementLocator) + join(text));
        });
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
        return timed("playwright.element.typeFileLocationForUpload", elementLocator,
                () -> elementLocator.setInputFiles(Path.of(filePath)));
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
        return timed("playwright.element.typeSecure", elementLocator, () -> {
            typeByKind(elementLocator, join(text), false);
            ReportManager.log("Typed secure text into Playwright element.");
        });
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
        return timed("playwright.element.captureScreenshot", elementLocator, () -> {
            byte[] screenshot = elementLocator.screenshot();
            ReportManagerHelper.attach("Playwright Screenshot", "element.png", new ByteArrayInputStream(screenshot));
            ReportManager.log("Captured Playwright element screenshot.");
        });
    }

    @Override
    public String ariaSnapshot(By elementLocator) {
        return ariaSnapshot(resolve(elementLocator));
    }

    @Override
    public String ariaSnapshot(ShaftLocator elementLocator) {
        return ariaSnapshot(resolve(elementLocator));
    }

    public String ariaSnapshot(Locator elementLocator) {
        return AriaSnapshotHelper.captureAriaSnapshot(elementLocator);
    }

    private Locator resolve(By locator) {
        if (locator instanceof CompositeLocator compositeLocator) {
            return resolve(compositeLocator, locator);
        }
        return ShaftLocator.from(locator).toPlaywrightLocator(session.page());
    }

    private ElementActions ocrPointerAction(OcrTarget target, OcrPointerGesture gesture) {
        return timed("playwright.element.ocr." + gesture.name().toLowerCase(), null, () -> {
            byte[] screenshot = session.page().screenshot();
            java.awt.image.BufferedImage image;
            try {
                image = ImageIO.read(new ByteArrayInputStream(screenshot));
            } catch (IOException exception) {
                throw new IllegalArgumentException("Playwright returned an unreadable OCR screenshot.", exception);
            }
            if (image == null) {
                throw new IllegalArgumentException("Playwright returned an unreadable OCR screenshot.");
            }
            var viewport = session.page().viewportSize();
            int targetWidth = viewport == null ? image.getWidth() : viewport.width;
            int targetHeight = viewport == null ? image.getHeight() : viewport.height;
            OcrPoint point = OcrCoordinateMapper.toPointerCenter(OcrProcessingActions.find(screenshot, target),
                    image.getWidth(), image.getHeight(), targetWidth, targetHeight, 0, 0);
            switch (gesture) {
                case CLICK -> session.page().mouse().click(point.x(), point.y());
                case DOUBLE_CLICK -> session.page().mouse().dblclick(point.x(), point.y());
                case HOVER -> session.page().mouse().move(point.x(), point.y());
                default -> throw new IllegalStateException("Unsupported OCR pointer gesture: " + gesture);
            }
        });
    }

    private enum OcrPointerGesture {
        CLICK,
        DOUBLE_CLICK,
        HOVER
    }

    private Locator resolve(CompositeLocator compositeLocator, By originalLocator) {
        for (By alternative : compositeLocator.alternatives()) {
            try {
                Locator candidate = ShaftLocator.from(alternative).toPlaywrightLocator(session.page());
                if (candidate.count() == 1) {
                    return candidate;
                }
            } catch (RuntimeException ignored) {
                // Unsupported or non-unique alternatives do not make later portable candidates invalid.
            }
        }
        throw new IllegalArgumentException("No unique Playwright element matched locator: " + originalLocator);
    }

    private Locator resolve(ShaftLocator locator) {
        return locator.toPlaywrightLocator(session.page());
    }

    private void typeByKind(Locator locator, String text, boolean append) {
        ElementSignals signals = readSignals(locator);
        ElementKind kind = ElementClassifier.classify(signals);
        boolean masked = ElementClassifier.looksMasked(signals);
        TypeStrategies.PlaywrightTypeRoute route = TypeStrategies.playwrightRouteFor(kind, masked);
        switch (route) {
            case REJECT -> throw new IllegalStateException(
                    "type() is not supported for element kind " + kind
                            + "; use click/select/upload APIs as appropriate.");
            case TOGGLE_CLICK -> {
                ReportManager.logDiscrete("type() on " + kind + " redirected to click (toggle); fill skipped.");
                locator.click();
            }
            case SELECT_OPTION -> locator.selectOption(text);
            case SET_FILES -> locator.setInputFiles(Path.of(text));
            case PRESS_SEQUENTIALLY -> pressSequentially(locator, text, append, kind);
            case FILL -> locator.fill(text == null ? "" : text);
        }
    }

    private void pressSequentially(Locator locator, String text, boolean append, ElementKind kind) {
        String typed = text == null ? "" : text;
        if (!append) {
            if (kind == ElementKind.CONTENTEDITABLE) {
                try {
                    locator.click();
                } catch (RuntimeException ignored) {
                    // Focus best-effort before select-all / sequential keys.
                }
                locator.press("ControlOrMeta+A");
            } else {
                try {
                    locator.clear();
                } catch (RuntimeException clearFailed) {
                    try {
                        locator.click();
                        locator.press("ControlOrMeta+A");
                    } catch (RuntimeException ignored) {
                        // Best-effort replace; sequential keys still attempt to land.
                    }
                }
            }
        }
        if (!typed.isEmpty()) {
            locator.pressSequentially(typed);
        }
    }

    @SuppressWarnings("unchecked")
    private ElementSignals readSignals(Locator locator) {
        try {
            Object raw = locator.evaluate(ELEMENT_SIGNALS_SCRIPT);
            if (raw instanceof Map<?, ?> map) {
                return ElementClassifier.fromEvaluateMap(map);
            }
        } catch (RuntimeException ignored) {
            // Conservative: unknown → fill path via TEXT_LIKE/UNKNOWN routing.
        }
        return ElementSignals.of(null, null, null, null, null, null, null, null);
    }

    private String readTextForAppend(Locator locator) {
        try {
            String value = locator.inputValue();
            if (value != null) {
                return value;
            }
        } catch (RuntimeException ignored) {
            // Non-input elements do not expose inputValue; append to text content instead.
        }
        String text = locator.textContent();
        return text == null ? "" : text;
    }

    private String join(CharSequence... text) {
        StringBuilder builder = new StringBuilder();
        for (CharSequence value : text) {
            builder.append(value);
        }
        return builder.toString();
    }

    private ElementActions timed(String actionName, Locator locator, Runnable action) {
        long start = System.nanoTime();
        String operation = actionName.substring(actionName.lastIndexOf('.') + 1);
        var event = TraceEventRecorder.startForBackend("element", operation,
                locator == null ? "" : String.valueOf(locator), AutomationBackend.MICROSOFT_PLAYWRIGHT);
        try {
            action.run();
            TraceEventRecorder.finish(event, "passed", "element " + operation + " completed.", null,
                    Map.of(), List.of());
            return this;
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", "element " + operation + " failed.", exception,
                    Map.of(), List.of());
            throw exception;
        } finally {
            BrowserPerformanceExecutionReport.recordBrowserAction(actionName, System.nanoTime() - start);
        }
    }

    private UnsupportedOperationException unsupported(String capability) {
        return new UnsupportedOperationException(capability + " is WebDriver-specific in SHAFT and is not available through the Playwright backend.");
    }
}
