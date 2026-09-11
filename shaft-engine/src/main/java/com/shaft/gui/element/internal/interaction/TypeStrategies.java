package com.shaft.gui.element.internal.interaction;

import com.shaft.tools.io.ReportManager;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

/**
 * Routes {@code type} by {@link ElementKind} so checkbox/radio/select/file/date/contenteditable
 * never take the blind clear+sendKeys path. Unknown kinds stay on the legacy caller path.
 */
public final class TypeStrategies {

    private static final String JS_SET_VALUE_WITH_EVENTS = """
            arguments[0].value = arguments[1];
            arguments[0].dispatchEvent(new Event('input', {bubbles: true}));
            arguments[0].dispatchEvent(new Event('change', {bubbles: true}));
            """;

    /**
     * Contenteditable clear+insert via execCommand when available; falls back to select-all + keys.
     * Avoids {@link WebElement#clear()} which targets value-backed controls and can wipe the wrong node.
     */
    private static final String JS_CONTENTEDITABLE_SET_TEXT = """
            var el = arguments[0];
            var text = arguments[1];
            el.focus();
            try {
              document.execCommand('selectAll', false, null);
              document.execCommand('insertText', false, text);
            } catch (e) {
              el.textContent = text;
              el.dispatchEvent(new Event('input', {bubbles: true}));
            }
            """;

    public enum TypeRoute {
        LEGACY_SEND_KEYS,
        TOGGLE_CLICK,
        SELECT_OPTION,
        SET_FILES,
        SET_VALUE_WITH_EVENTS,
        CONTENTEDITABLE,
        REJECT
    }

    private TypeStrategies() {
    }

    public static TypeRoute routeFor(ElementKind kind) {
        return switch (kind) {
            case CHECKBOX, RADIO -> TypeRoute.TOGGLE_CLICK;
            case SELECT -> TypeRoute.SELECT_OPTION;
            case FILE -> TypeRoute.SET_FILES;
            case DATE_LIKE, RANGE, COLOR -> TypeRoute.SET_VALUE_WITH_EVENTS;
            case CONTENTEDITABLE -> TypeRoute.CONTENTEDITABLE;
            // Readonly/disabled/iframe: refuse type. Button/link stay legacy (conservative).
            case DISABLED, READONLY, IFRAME -> TypeRoute.REJECT;
            case TEXT_LIKE, COMBOBOX, BUTTON, LINK, UNKNOWN -> TypeRoute.LEGACY_SEND_KEYS;
        };
    }

    public static void rejectUnsupported(ElementKind kind) {
        throw new InvalidElementStateException(
                "type() is not supported for element kind " + kind
                        + "; use click/select/upload APIs as appropriate.");
    }

    public static void toggleInsteadOfTyping(WebElement element, ElementKind kind) {
        ReportManager.logDiscrete("type() on " + kind + " redirected to click (toggle); sendKeys skipped.");
        element.click();
    }

    public static void setValueWithEvents(WebDriver driver, WebElement element, String text) {
        if (!(driver instanceof JavascriptExecutor executor)) {
            throw new InvalidElementStateException(
                    "Cannot set value with events: driver does not execute JavaScript.");
        }
        executor.executeScript(JS_SET_VALUE_WITH_EVENTS, element, text == null ? "" : text);
    }

    /**
     * Focus then insert text without {@link WebElement#clear()}. When {@code clearBefore} is true,
     * replaces existing content via documented JS insert; otherwise appends with sendKeys.
     */
    public static void typeContentEditable(WebDriver driver, WebElement element, CharSequence[] text,
                                           boolean clearBefore) {
        String typed = stringify(text);
        if (clearBefore && driver instanceof JavascriptExecutor executor) {
            executor.executeScript(JS_CONTENTEDITABLE_SET_TEXT, element, typed);
            return;
        }
        try {
            element.click();
        } catch (RuntimeException ignored) {
            // Focus best-effort; keys may still land on the active editable.
        }
        if (clearBefore) {
            element.sendKeys(Keys.chord(Keys.CONTROL, "a"));
            element.sendKeys(Keys.BACK_SPACE);
        }
        if (!typed.isEmpty()) {
            // Sequential keys (not clear()+value): preserves key handlers / editor contracts.
            for (int i = 0; i < typed.length(); i++) {
                element.sendKeys(String.valueOf(typed.charAt(i)));
            }
        }
    }

    public static void sendKeysFilePath(WebElement element, String absoluteFilePath) {
        element.sendKeys(absoluteFilePath);
    }

    private static String stringify(CharSequence[] text) {
        if (text == null) {
            return "";
        }
        StringBuilder value = new StringBuilder();
        for (CharSequence sequence : text) {
            value.append(sequence == null ? "" : sequence);
        }
        return value.toString();
    }
}
