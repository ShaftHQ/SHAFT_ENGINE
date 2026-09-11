package com.shaft.gui.element.internal.interaction;

import org.openqa.selenium.WebElement;

import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * Cheap classifier: tagName, type, role/ARIA, contenteditable, disabled flags.
 * Conservative by design — ambiguous custom widgets return {@link ElementKind#UNKNOWN}.
 * Shared by Selenium Wave B and Playwright Wave C (#5732).
 */
public final class ElementClassifier {

    private static final Set<String> TEXT_INPUT_TYPES = Set.of(
            "text", "password", "email", "search", "tel", "url", "number", "");
    private static final Set<String> DATE_INPUT_TYPES = Set.of(
            "date", "time", "month", "week", "datetime-local");
    private static final Set<String> BUTTON_INPUT_TYPES = Set.of(
            "button", "submit", "reset", "image");
    private static final Set<String> TEXT_ROLES = Set.of(
            "textbox", "searchbox", "spinbutton");

    private ElementClassifier() {
    }

    public static ElementKind classify(WebElement element) {
        if (element == null) {
            return ElementKind.UNKNOWN;
        }
        return classify(fromWebElement(element));
    }

    /**
     * Classify from pre-read DOM signals (Playwright evaluate map or Selenium attribute reads).
     */
    public static ElementKind classify(ElementSignals signals) {
        if (signals == null) {
            return ElementKind.UNKNOWN;
        }

        if (isTruthyFlagValue(signals.disabled(), "disabled")
                || "true".equals(safeLower(signals.ariaDisabled()))) {
            return ElementKind.DISABLED;
        }
        // Readonly before contenteditable/input kinds so type() can refuse
        // while click() still focuses (HTML readonly controls remain clickable).
        if (isTruthyFlagValue(signals.readonly(), "readonly")) {
            return ElementKind.READONLY;
        }

        String tag = safeLower(signals.tagName());
        String type = safeLower(signals.type());
        String role = safeLower(signals.role());
        String className = safeLower(signals.className());

        if (isContentEditable(signals)) {
            return ElementKind.CONTENTEDITABLE;
        }

        ElementKind byMobile = classifyMobileNative(tag, className, role);
        if (byMobile != null) {
            return byMobile;
        }

        ElementKind byTag = classifyByTag(tag, role);
        if (byTag != null) {
            return byTag;
        }

        if ("input".equals(tag)) {
            return classifyInput(type, role);
        }

        return classifyByRole(role);
    }

    /**
     * Appium native class / XCUI / Flutter-ish tags. Returns null when not a known mobile control.
     * Conservative: only exact / well-known suffixes so custom views stay {@link ElementKind#UNKNOWN}.
     */
    static ElementKind classifyMobileNative(String tag, String className, String role) {
        String token = firstNonBlank(tag, className);
        if (token == null) {
            return null;
        }
        String simple = simpleClassName(token);

        if (isMobileToggle(simple, role)) {
            return ElementKind.CHECKBOX;
        }
        if (isMobileRadio(simple, role)) {
            return ElementKind.RADIO;
        }
        if (isMobileTextField(simple, role)) {
            return ElementKind.TEXT_LIKE;
        }
        if (isMobileButton(simple, role)) {
            return ElementKind.BUTTON;
        }
        if (isMobileLink(simple, role)) {
            return ElementKind.LINK;
        }
        if ("seekbar".equals(simple) || "slider".equals(simple) || "xcuielementtypeslider".equals(simple)) {
            return ElementKind.RANGE;
        }
        return null;
    }

    private static boolean isMobileToggle(String simple, String role) {
        return "checkbox".equals(role)
                || "switch".equals(role)
                || "checkbox".equals(simple)
                || "switch".equals(simple)
                || "togglebutton".equals(simple)
                || "xcuielementtypeswitch".equals(simple)
                || "xcuielementtypecheckbox".equals(simple);
    }

    private static boolean isMobileRadio(String simple, String role) {
        return "radio".equals(role)
                || "radiobutton".equals(simple)
                || "xcuielementtyperadiobutton".equals(simple);
    }

    private static boolean isMobileTextField(String simple, String role) {
        if (role != null && TEXT_ROLES.contains(role)) {
            return true;
        }
        return "edittext".equals(simple)
                || "textfield".equals(simple)
                || "autocompletetextview".equals(simple)
                || "multiautocompletetextview".equals(simple)
                || "xcuielementtypetextfield".equals(simple)
                || "xcuielementtypesecuretextfield".equals(simple)
                || "xcuielementtypetextview".equals(simple)
                // Flutter integration driver type names (ValueKey / Semantics still required by apps).
                || "editabletext".equals(simple);
    }

    private static boolean isMobileButton(String simple, String role) {
        return "button".equals(role)
                || "button".equals(simple)
                || "imagebutton".equals(simple)
                || "xcuielementtypebutton".equals(simple);
    }

    private static boolean isMobileLink(String simple, String role) {
        return "link".equals(role) || "xcuielementtypelink".equals(simple);
    }

    private static String simpleClassName(String raw) {
        String lower = raw.toLowerCase(Locale.ROOT);
        int slash = Math.max(lower.lastIndexOf('.'), lower.lastIndexOf('/'));
        return slash >= 0 && slash + 1 < lower.length() ? lower.substring(slash + 1) : lower;
    }

    /**
     * Build signals from a Playwright-style evaluate result map (string keys, scalar values).
     */
    public static ElementSignals fromEvaluateMap(Map<?, ?> raw) {
        if (raw == null) {
            return null;
        }
        return new ElementSignals(
                asString(raw.get("tagName")),
                asString(raw.get("type")),
                asString(raw.get("role")),
                asString(raw.get("contentEditable")),
                asString(raw.get("isContentEditable")),
                asString(raw.get("disabled")),
                asString(raw.get("readonly")),
                asString(raw.get("ariaDisabled")),
                asString(raw.get("dataMask")),
                asString(raw.get("dataInputmask")),
                asString(raw.get("mask")),
                asString(raw.get("className")),
                asString(raw.get("autocomplete")));
    }

    /**
     * Conservative masked / OTP / input-mask heuristic for Playwright sequential typing.
     * Prefer explicit data-* / mask attributes and known library class tokens over broad "mask" substrings.
     */
    public static boolean looksMasked(ElementSignals signals) {
        if (signals == null) {
            return false;
        }
        if (nonBlank(signals.dataMask())
                || nonBlank(signals.dataInputmask())
                || nonBlank(signals.mask())) {
            return true;
        }
        String autocomplete = safeLower(signals.autocomplete());
        if ("one-time-code".equals(autocomplete)) {
            return true;
        }
        String className = safeLower(signals.className());
        if (className == null) {
            return false;
        }
        return className.contains("inputmask")
                || className.contains("imask")
                || className.contains("mask-input")
                || className.contains("masked-input");
    }

    private static ElementSignals fromWebElement(WebElement element) {
        return new ElementSignals(
                safeTagName(element),
                firstNonBlank(safeDom(element, "type"), safeProperty(element, "type")),
                safeDom(element, "role"),
                safeDom(element, "contenteditable"),
                safeProperty(element, "isContentEditable"),
                safeDom(element, "disabled"),
                safeDom(element, "readonly"),
                safeDom(element, "aria-disabled"),
                safeDom(element, "data-mask"),
                firstNonBlank(safeDom(element, "data-inputmask"), safeDom(element, "data-input-mask")),
                safeDom(element, "mask"),
                firstNonBlank(safeDom(element, "class"), safeProperty(element, "className")),
                safeDom(element, "autocomplete"));
    }

    /**
     * Tag-driven kinds (and tag/role pairs that are decided before input handling).
     * Returns null when the tag does not decide the kind.
     */
    private static ElementKind classifyByTag(String tag, String role) {
        if ("select".equals(tag)) {
            return ElementKind.SELECT;
        }
        if ("textarea".equals(tag)) {
            return ElementKind.TEXT_LIKE;
        }
        if ("iframe".equals(tag) || "frame".equals(tag)) {
            return ElementKind.IFRAME;
        }
        if ("a".equals(tag) || "link".equals(role)) {
            return ElementKind.LINK;
        }
        if ("button".equals(tag) || "button".equals(role)) {
            return ElementKind.BUTTON;
        }
        return null;
    }

    private static ElementKind classifyInput(String type, String role) {
        ElementKind toggle = classifyToggleInput(type, role);
        if (toggle != null) {
            return toggle;
        }
        ElementKind special = classifySpecialInput(type, role);
        if (special != null) {
            return special;
        }
        return classifyButtonOrTextInput(type);
    }

    /** Checkbox / radio input type or role. Returns null when not a toggle. */
    private static ElementKind classifyToggleInput(String type, String role) {
        if ("checkbox".equals(type) || "checkbox".equals(role)) {
            return ElementKind.CHECKBOX;
        }
        if ("radio".equals(type) || "radio".equals(role)) {
            return ElementKind.RADIO;
        }
        return null;
    }

    /** File / date / range / color. Returns null when not a special input. */
    private static ElementKind classifySpecialInput(String type, String role) {
        if ("file".equals(type)) {
            return ElementKind.FILE;
        }
        if (DATE_INPUT_TYPES.contains(type)) {
            return ElementKind.DATE_LIKE;
        }
        if ("range".equals(type) || "slider".equals(role)) {
            return ElementKind.RANGE;
        }
        if ("color".equals(type)) {
            return ElementKind.COLOR;
        }
        return null;
    }

    /** Button-like or text-like input types; unknown types stay UNKNOWN. */
    private static ElementKind classifyButtonOrTextInput(String type) {
        if (BUTTON_INPUT_TYPES.contains(type)) {
            return ElementKind.BUTTON;
        }
        if (type == null || TEXT_INPUT_TYPES.contains(type)) {
            return ElementKind.TEXT_LIKE;
        }
        // Unknown input type (e.g. custom) — do not guess.
        return ElementKind.UNKNOWN;
    }

    private static ElementKind classifyByRole(String role) {
        if ("checkbox".equals(role) || "switch".equals(role)) {
            return ElementKind.CHECKBOX;
        }
        if ("radio".equals(role)) {
            return ElementKind.RADIO;
        }
        if ("slider".equals(role)) {
            return ElementKind.RANGE;
        }
        if ("combobox".equals(role) || "listbox".equals(role)) {
            return ElementKind.COMBOBOX;
        }
        if (role != null && TEXT_ROLES.contains(role)) {
            return ElementKind.TEXT_LIKE;
        }
        return ElementKind.UNKNOWN;
    }

    private static boolean isContentEditable(ElementSignals signals) {
        if (isContentEditableAttributeValue(signals.contentEditable())) {
            return true;
        }
        // Property path: only the boolean true string — mock junk like "dom-isContentEditable" must not match.
        return "true".equalsIgnoreCase(signals.isContentEditableProperty());
    }

    /**
     * HTML contenteditable is on for "", "true", "plaintext-only", or the attribute name itself.
     * Reject arbitrary non-empty strings so mocked getDomAttribute defaults cannot false-positive.
     */
    static boolean isContentEditableAttributeValue(String raw) {
        if (raw == null) {
            return false;
        }
        String trimmed = raw.trim();
        if (trimmed.isEmpty()) {
            return true;
        }
        String lower = trimmed.toLowerCase(Locale.ROOT);
        return "true".equals(lower)
                || "plaintext-only".equals(lower)
                || "contenteditable".equals(lower);
    }

    /**
     * HTML boolean attributes are present as "" or the attribute name; ARIA uses "true"/"false".
     * Reject arbitrary non-empty strings so mocked defaults cannot false-positive.
     */
    static boolean isTruthyFlag(WebElement element, String attributeName) {
        return isTruthyFlagValue(safeDom(element, attributeName), attributeName);
    }

    static boolean isTruthyFlagValue(String value, String attributeName) {
        if (value == null) {
            return false;
        }
        String trimmed = value.trim();
        if (trimmed.isEmpty()) {
            return true;
        }
        String lower = trimmed.toLowerCase(Locale.ROOT);
        return "true".equals(lower) || attributeName.equalsIgnoreCase(trimmed);
    }

    private static String safeTagName(WebElement element) {
        try {
            return element.getTagName();
        } catch (RuntimeException ignored) {
            return null;
        }
    }

    private static String safeDom(WebElement element, String name) {
        try {
            return element.getDomAttribute(name);
        } catch (RuntimeException ignored) {
            try {
                return element.getAttribute(name);
            } catch (RuntimeException ignoredAgain) {
                return null;
            }
        }
    }

    private static String safeProperty(WebElement element, String name) {
        try {
            return element.getDomProperty(name);
        } catch (RuntimeException ignored) {
            return null;
        }
    }

    private static String safeLower(String value) {
        return value == null ? null : value.trim().toLowerCase(Locale.ROOT);
    }

    private static String firstNonBlank(String first, String second) {
        if (first != null && !first.isBlank()) {
            return first;
        }
        if (second != null && !second.isBlank()) {
            return second;
        }
        return first != null ? first : second;
    }

    private static boolean nonBlank(String value) {
        return value != null && !value.isBlank();
    }

    private static String asString(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof Boolean bool) {
            return bool ? "true" : "false";
        }
        String text = String.valueOf(value);
        return "null".equals(text) ? null : text;
    }
}
