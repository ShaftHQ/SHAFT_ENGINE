package com.shaft.capture.control;

import com.shaft.capture.model.LocatorCandidate;

/**
 * Renders a single copy-paste SHAFT locator Java expression for one picked locator candidate.
 * Matches {@code LocatorPolicy}'s three-tier emission: builder id, builder role/text, then
 * native {@code By.xpath}. Never emits the banned {@code SHAFT.GUI.Locator.id/name/cssSelector/xpath}
 * factories.
 */
public final class PickedLocatorSnippetBuilder {
    private PickedLocatorSnippetBuilder() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Renders one candidate as a copy-paste Java locator expression.
     *
     * @param candidate picked locator candidate
     * @return SHAFT locator-builder or {@code By.xpath} Java expression
     */
    public static String snippet(LocatorCandidate candidate) {
        String expression = candidate.expression();
        return switch (candidate.strategy()) {
            case ID -> idBuilder(expression);
            case NAME -> attributeBuilder("name", expression);
            case TEST_ID -> testIdBuilder(expression);
            case CSS -> cssBuilder(expression);
            case XPATH -> "By.xpath(\"" + javaString(expression) + "\")";
            case ROLE, ACCESSIBLE_NAME, LABEL -> "SHAFT.GUI.Locator.hasAnyTagName().containsText(\""
                    + javaString(expression) + "\").build()";
        };
    }

    private static String idBuilder(String id) {
        if (id.indexOf('"') >= 0) {
            return "By.id(\"" + javaString(id) + "\")";
        }
        return "SHAFT.GUI.Locator.hasAnyTagName().hasId(\"" + javaString(id) + "\").build()";
    }

    private static String attributeBuilder(String attribute, String value) {
        return "SHAFT.GUI.Locator.hasAnyTagName().hasAttribute(\"" + javaString(attribute)
                + "\", \"" + javaString(value) + "\").build()";
    }

    private static String testIdBuilder(String expression) {
        if (expression.startsWith("#") && isSimpleIdentifier(expression.substring(1))) {
            return idBuilder(expression.substring(1));
        }
        String[] attribute = cssAttributeSelector(expression);
        if (attribute != null) {
            return attributeBuilder(attribute[0], attribute[1]);
        }
        return attributeBuilder("data-testid", expression);
    }

    /**
     * Parses {@code [data-testid="login"]} / {@code [data-qa='x']} into name and value.
     *
     * @return {attributeName, attributeValue}, or null when not an attribute selector
     */
    private static String[] cssAttributeSelector(String css) {
        String trimmed = css.trim();
        if (trimmed.length() < 5 || trimmed.charAt(0) != '[' || trimmed.charAt(trimmed.length() - 1) != ']') {
            return null;
        }
        int equals = trimmed.indexOf('=');
        if (equals <= 1) {
            return null;
        }
        int valueStart = equals + 1;
        int valueEnd = trimmed.length() - 1;
        if (valueStart >= valueEnd) {
            return null;
        }
        char quote = trimmed.charAt(valueStart);
        if (quote == '"' || quote == '\'') {
            if (trimmed.charAt(valueEnd - 1) != quote) {
                return null;
            }
            valueStart++;
            valueEnd--;
        }
        String name = trimmed.substring(1, equals);
        if (name.isBlank() || !name.chars().allMatch(ch -> Character.isLetterOrDigit(ch) || ch == '_' || ch == '-')) {
            return null;
        }
        return new String[] {name, trimmed.substring(valueStart, valueEnd)};
    }

    private static String cssBuilder(String css) {
        if (css.startsWith("#") && isSimpleIdentifier(css.substring(1))) {
            return idBuilder(css.substring(1));
        }
        if (css.startsWith(".") && isSimpleIdentifier(css.substring(1))) {
            return "SHAFT.GUI.Locator.hasAnyTagName().hasClass(\"" + javaString(css.substring(1)) + "\").build()";
        }
        return "By.cssSelector(\"" + javaString(css) + "\")";
    }

    private static boolean isSimpleIdentifier(String value) {
        return !value.isBlank() && value.chars().allMatch(ch -> Character.isLetterOrDigit(ch) || ch == '_' || ch == '-');
    }

    private static String javaString(String value) {
        return value.replace("\\", "\\\\").replace("\"", "\\\"");
    }
}
