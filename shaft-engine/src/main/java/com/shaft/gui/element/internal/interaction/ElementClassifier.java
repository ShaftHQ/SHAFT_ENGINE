package com.shaft.gui.element.internal.interaction;

import org.openqa.selenium.WebElement;

import java.util.Locale;
import java.util.Set;

/**
 * Cheap Selenium-path classifier: tagName, type, role/ARIA, contenteditable, disabled flags.
 * Conservative by design — ambiguous custom widgets return {@link ElementKind#UNKNOWN}.
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

        if (isDisabledOrReadonly(element)) {
            return ElementKind.DISABLED;
        }

        String tag = safeLower(safeTagName(element));
        String type = safeLower(firstNonBlank(safeDom(element, "type"), safeProperty(element, "type")));
        String role = safeLower(safeDom(element, "role"));

        if (isTruthyFlag(element, "contenteditable") || "true".equalsIgnoreCase(safeProperty(element, "isContentEditable"))) {
            return ElementKind.CONTENTEDITABLE;
        }

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

        if ("input".equals(tag)) {
            if ("checkbox".equals(type) || "checkbox".equals(role)) {
                return ElementKind.CHECKBOX;
            }
            if ("radio".equals(type) || "radio".equals(role)) {
                return ElementKind.RADIO;
            }
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
            if (BUTTON_INPUT_TYPES.contains(type)) {
                return ElementKind.BUTTON;
            }
            if (TEXT_INPUT_TYPES.contains(type) || type == null) {
                return ElementKind.TEXT_LIKE;
            }
            // Unknown input type (e.g. custom) — do not guess.
            return ElementKind.UNKNOWN;
        }

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

    private static boolean isDisabledOrReadonly(WebElement element) {
        if (isTruthyFlag(element, "disabled") || isTruthyFlag(element, "readonly")) {
            return true;
        }
        String ariaDisabled = safeLower(safeDom(element, "aria-disabled"));
        return "true".equals(ariaDisabled);
    }

    /**
     * HTML boolean attributes are present as "" or the attribute name; ARIA uses "true"/"false".
     * Reject arbitrary non-empty strings so mocked defaults cannot false-positive.
     */
    static boolean isTruthyFlag(WebElement element, String attributeName) {
        String value = safeDom(element, attributeName);
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
}
