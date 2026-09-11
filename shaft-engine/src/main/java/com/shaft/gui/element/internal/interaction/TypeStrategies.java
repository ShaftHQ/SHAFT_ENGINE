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
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Routes {@code type} by {@link ElementKind} so checkbox/radio/select/file/date/contenteditable
 * never take the blind clear+sendKeys path. Unknown kinds stay on the legacy caller path.
 * Mobile native (#5732 Wave D): focus → setValue / sendKeys / {@code mobile: type} → optional hideKeyboard.
 * Windows desktop (#5732 Wave E): focus → clear → sendKeys / {@code windows: keys}; CheckBox/Radio toggle
 * (not sendKeys); ComboBox expand then type-to-filter + Enter.
 *
 * <p><b>Compose / Flutter notes:</b> Jetpack Compose TextFields often reject setValue/sendKeys until
 * focused (enable {@code testTagsAsResourceId} and tap first). Flutter TextFields need ValueKey /
 * Semantics identifiers; after focus prefer driver sendKeys or {@code flutter:*} enter-text helpers.
 * When setValue is rejected without focus, SHAFT throws an actionable {@link InvalidElementStateException}.
 *
 * <p><b>WinAppDriver / UIA notes:</b> Prefer WebElement click/clear/sendKeys. On flake use
 * {@code windows: click} / {@code windows: keys}. Value pattern is read via {@code Value.Value} when
 * present; writing still goes through clear+keys (classic WinAppDriver has no setValue script).
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

    /** Windows desktop / UIA type routes (Wave E). */
    public enum DesktopTypeRoute {
        TOGGLE_CLICK,
        DESKTOP_TEXT,
        COMBOBOX,
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

    public static DesktopTypeRoute desktopRouteFor(ElementKind kind) {
        return switch (kind) {
            case CHECKBOX, RADIO -> DesktopTypeRoute.TOGGLE_CLICK;
            case TEXT_LIKE, CONTENTEDITABLE -> DesktopTypeRoute.DESKTOP_TEXT;
            case COMBOBOX -> DesktopTypeRoute.COMBOBOX;
            case DISABLED, READONLY, IFRAME, FILE, SELECT, RANGE, DATE_LIKE, COLOR -> DesktopTypeRoute.REJECT;
            case BUTTON, LINK, UNKNOWN -> DesktopTypeRoute.LEGACY_SEND_KEYS;
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

    /**
     * Windows CheckBox / RadioButton: toggle via click (Invoke/Toggle), never sendKeys.
     * Logs {@link WebElement#isSelected()} and {@code Toggle.ToggleState} when available.
     */
    public static void toggleDesktopInsteadOfTyping(WebDriver driver, WebElement element, ElementKind kind) {
        ReportManager.logDiscrete("type() on desktop " + kind + " redirected to click (toggle); sendKeys skipped.");
        boolean before = safeIsSelected(element);
        ClickStrategies.focusWindows(driver, element);
        boolean after = safeIsSelected(element);
        String toggleState = safeAttribute(element, "Toggle.ToggleState");
        ReportManager.logDiscrete("desktop toggle state selected=" + after
                + (before == after ? " (unchanged from " + before + ")" : " (was " + before + ")")
                + (toggleState == null ? "" : " Toggle.ToggleState=" + toggleState));
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
     * Defaults to replace-allowed ({@code replaceAllowed=true}).
     */
    public static void typeMobileText(WebDriver driver, WebElement element, CharSequence[] text) {
        typeMobileText(driver, element, text, true);
    }

    /**
     * Mobile native text entry after the caller has focused (and optionally cleared) the field.
     * Ladder: {@code sendKeys} → Android {@code mobile: replaceElementValue} (replace/clear modes only)
     * → {@code mobile: type}. Compose/Flutter rejections without focus surface an actionable error.
     *
     * @param replaceAllowed when false ({@code clearBeforeTypingMode=off} / append), skip
     *                       {@code replaceElementValue} so existing text is not wiped
     */
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

    /**
     * Windows Edit / Document text entry after focus: {@code sendKeys} → {@code windows: keys}.
     * Value pattern ({@code Value.Value}) is observed when present; writing uses keys.
     */
    public static void typeDesktopText(WebDriver driver, WebElement element, CharSequence[] text) {
        String typed = stringify(text);
        try {
            element.sendKeys(text);
            logValuePatternIfPresent(element);
            return;
        } catch (RuntimeException sendKeysFailure) {
            ReportManager.logDiscrete("desktop sendKeys failed; trying windows: keys.");
            windowsKeys(driver, typed);
            logValuePatternIfPresent(element);
        }
    }

    /**
     * Windows ComboBox: click to expand (best-effort), type-to-filter, then Enter to commit.
     */
    public static void typeDesktopCombobox(WebDriver driver, WebElement element, CharSequence[] text) {
        ClickStrategies.focusWindows(driver, element);
        String typed = stringify(text);
        try {
            if (!typed.isEmpty()) {
                element.sendKeys(text);
            }
            element.sendKeys(Keys.ENTER);
        } catch (RuntimeException sendKeysFailure) {
            ReportManager.logDiscrete("desktop ComboBox sendKeys failed; trying windows: keys + Enter.");
            if (!typed.isEmpty()) {
                windowsKeys(driver, typed);
            }
            Map<String, Object> enter = new LinkedHashMap<>();
            enter.put("virtualKeyCode", 0x0D);
            windowsKeys(driver, List.of(enter));
        }
    }

    static void windowsKeys(WebDriver driver, String text) {
        windowsKeys(driver, List.of(Map.of("text", text == null ? "" : text)));
    }

    static void windowsKeys(WebDriver driver, List<Map<String, Object>> actions) {
        if (!(driver instanceof JavascriptExecutor executor)) {
            throw new InvalidElementStateException(
                    "windows: keys requires a driver that executes scripts.");
        }
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("actions", actions);
        executor.executeScript("windows: keys", params);
    }

    private static void logValuePatternIfPresent(WebElement element) {
        String value = safeAttribute(element, "Value.Value");
        if (value != null) {
            ReportManager.logDiscrete("UIA Value.Value after type: " + value);
        }
    }

    private static boolean safeIsSelected(WebElement element) {
        try {
            return element.isSelected();
        } catch (RuntimeException ignored) {
            return false;
        }
    }

    private static String safeAttribute(WebElement element, String name) {
        try {
            String dom = element.getDomAttribute(name);
            if (dom != null && !dom.isBlank()) {
                return dom;
            }
        } catch (RuntimeException ignored) {
            // Fall through to getAttribute.
        }
        try {
            return element.getAttribute(name);
        } catch (RuntimeException ignored) {
            return null;
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
