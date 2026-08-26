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
        return attributeBuilder("data-testid", expression);
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
