package com.shaft.gui.element.internal.interaction;

import com.shaft.tools.io.ReportManager;
import io.appium.java_client.HidesKeyboard;
import io.appium.java_client.android.CanReplaceElementValue;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.RemoteWebElement;

import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Routes {@code type} by {@link ElementKind} so checkbox/radio/select/file/date/contenteditable
 * never take the blind clear+sendKeys path. Unknown kinds stay on the legacy caller path.
 * Mobile native (#5732 Wave D): focus → setValue / sendKeys / {@code mobile: type} → optional hideKeyboard.
 *
 * <p><b>Compose / Flutter notes:</b> Jetpack Compose TextFields often reject setValue/sendKeys until
 * focused (enable {@code testTagsAsResourceId} and tap first). Flutter TextFields need ValueKey /
 * Semantics identifiers; after focus prefer driver sendKeys or {@code flutter:*} enter-text helpers.
 * When setValue is rejected without focus, SHAFT throws an actionable {@link InvalidElementStateException}.
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

    /**
     * Playwright {@code type()} routes (Wave C): plain inputs stay {@link PlaywrightTypeRoute#FILL};
     * contenteditable / masked use {@link PlaywrightTypeRoute#PRESS_SEQUENTIALLY}.
     */
    public enum PlaywrightTypeRoute {
        FILL,
        PRESS_SEQUENTIALLY,
        TOGGLE_CLICK,
        SELECT_OPTION,
        SET_FILES,
        REJECT
    }

    /** Mobile-native type routes (Wave D). */
    public enum MobileTypeRoute {
        TOGGLE_CLICK,
        MOBILE_TEXT,
        REJECT,
        /** Unknown / button / link — keep sendKeys after focus for conservative compatibility. */
        LEGACY_SEND_KEYS
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

    public static MobileTypeRoute mobileRouteFor(ElementKind kind) {
        return switch (kind) {
            case CHECKBOX, RADIO -> MobileTypeRoute.TOGGLE_CLICK;
            // SeekBar/slider/date/color need platform value APIs — do not pretend sendKeys works.
            case DISABLED, READONLY, IFRAME, FILE, SELECT, RANGE, DATE_LIKE, COLOR -> MobileTypeRoute.REJECT;
            case TEXT_LIKE, COMBOBOX, CONTENTEDITABLE, UNKNOWN -> MobileTypeRoute.MOBILE_TEXT;
            case BUTTON, LINK -> MobileTypeRoute.LEGACY_SEND_KEYS;
        };
    }

    /**
     * Playwright type routing. {@code masked} only affects text-like / combobox / unknown /
     * button / link paths (contenteditable always sequential).
     */
    public static PlaywrightTypeRoute playwrightRouteFor(ElementKind kind, boolean masked) {
        return switch (kind) {
            case CHECKBOX, RADIO -> PlaywrightTypeRoute.TOGGLE_CLICK;
            case SELECT -> PlaywrightTypeRoute.SELECT_OPTION;
            case FILE -> PlaywrightTypeRoute.SET_FILES;
            case CONTENTEDITABLE -> PlaywrightTypeRoute.PRESS_SEQUENTIALLY;
            case DISABLED, READONLY, IFRAME -> PlaywrightTypeRoute.REJECT;
            // date/range/color: Playwright fill is the fast, event-friendly default.
            case DATE_LIKE, RANGE, COLOR -> PlaywrightTypeRoute.FILL;
            case TEXT_LIKE, COMBOBOX, BUTTON, LINK, UNKNOWN ->
                    masked ? PlaywrightTypeRoute.PRESS_SEQUENTIALLY : PlaywrightTypeRoute.FILL;
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

    /**
     * Mobile checkbox/switch/radio: toggle via click/tap, never sendKeys.
     */
    public static void toggleMobileInsteadOfTyping(WebDriver driver, WebElement element, ElementKind kind) {
        ReportManager.logDiscrete("type() on mobile " + kind + " redirected to tap (toggle); sendKeys skipped.");
        ClickStrategies.focusTap(driver, element);
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

    /**
     * Mobile native text entry after the caller has focused (and optionally cleared) the field.
     * Ladder: {@code sendKeys} → Android {@code mobile: replaceElementValue} (replace/clear modes only)
     * → {@code mobile: type}. Compose/Flutter rejections without focus surface an actionable error.
     *
     * @param replaceAllowed when false ({@code clearBeforeTypingMode=off} / append), skip
     *                       {@code replaceElementValue} so existing text is not wiped
     */
    public static void typeMobileText(WebDriver driver, WebElement element, CharSequence[] text) {
        typeMobileText(driver, element, text, true);
    }

    public static void typeMobileText(WebDriver driver, WebElement element, CharSequence[] text,
                                      boolean replaceAllowed) {
        String typed = stringify(text);
        RuntimeException lastFailure = null;

        try {
            element.sendKeys(text);
            return;
        } catch (RuntimeException sendKeysFailure) {
            lastFailure = sendKeysFailure;
            ReportManager.logDiscrete("mobile sendKeys failed; trying platform setValue / mobile: type.");
        }

        if (replaceAllowed
                && driver instanceof CanReplaceElementValue replacer
                && element instanceof RemoteWebElement remote) {
            try {
                replacer.replaceElementValue(remote, typed);
                return;
            } catch (RuntimeException replaceFailure) {
                lastFailure = replaceFailure;
            }
        }

        if (driver instanceof JavascriptExecutor executor) {
            try {
                Map<String, Object> params = new LinkedHashMap<>();
                params.put("text", typed);
                executor.executeScript("mobile: type", params);
                return;
            } catch (RuntimeException mobileTypeFailure) {
                lastFailure = mobileTypeFailure;
            }
        }

        throw actionableMobileTypeFailure(element, lastFailure);
    }

    public static void hideKeyboardIfConfigured(WebDriver driver, boolean hideKeyboardAfterTyping) {
        if (!hideKeyboardAfterTyping) {
            return;
        }
        if (driver instanceof HidesKeyboard hidesKeyboard) {
            try {
                hidesKeyboard.hideKeyboard();
                ReportManager.logDiscrete("hideKeyboard after mobile type (hideKeyboardAfterTyping=true).");
            } catch (RuntimeException ignored) {
                ReportManager.logDiscrete("hideKeyboard after typing was requested but failed; continuing.");
            }
        }
    }

    static InvalidElementStateException actionableMobileTypeFailure(WebElement element, RuntimeException cause) {
        String hint = composeFlutterHint(element);
        String message = "Mobile type/setValue was rejected"
                + (hint.isEmpty() ? "" : " (" + hint + ")")
                + ". Focus the field first (tap/click), then type. "
                + "Jetpack Compose: enable testTagsAsResourceId and tap the TextField before type — "
                + "setValue/sendKeys often throws InvalidElementState until focused. "
                + "Flutter: use ValueKey/Semantics locators; after focus use sendKeys or flutter enter-text helpers.";
        if (cause == null) {
            return new InvalidElementStateException(message);
        }
        return new InvalidElementStateException(message, cause);
    }

    private static String composeFlutterHint(WebElement element) {
        try {
            String tag = element.getTagName();
            String className = element.getDomAttribute("class");
            if (className == null) {
                className = element.getAttribute("class");
            }
            String blob = ((tag == null ? "" : tag) + " " + (className == null ? "" : className))
                    .toLowerCase(Locale.ROOT);
            if (blob.contains("compose")) {
                return "Compose-like control";
            }
            if (blob.contains("flutter") || blob.contains("editabletext") || "textfield".equals(blob.trim())) {
                return "Flutter-like control";
            }
        } catch (RuntimeException ignored) {
            // Hint is best-effort only.
        }
        return "";
    }

    public static String stringify(CharSequence[] text) {
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
