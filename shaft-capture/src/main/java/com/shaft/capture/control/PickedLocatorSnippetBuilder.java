package com.shaft.capture.control;

import com.shaft.capture.model.LocatorCandidate;

/**
 * Renders a single copy-paste {@code SHAFT.GUI.Locator...} (or {@code By...}) Java expression for
 * one picked locator candidate, for the live "Pick Locator" flow (see
 * {@code CaptureControlServer#pickLocator}). Deliberately simpler than
 * {@code com.shaft.capture.generate.CaptureGenerator}'s codegen-time locator rendering (which also
 * has access to the target's semantic role/label/input-vs-clickable classification to prefer
 * {@code inputField(...)}/{@code clickableField(...)}): this builder only sees the picked
 * candidate itself, since a live pick has no surrounding recorded scenario to classify against.
 */
public final class PickedLocatorSnippetBuilder {
    private PickedLocatorSnippetBuilder() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Renders one candidate as a copy-paste Java locator expression.
     *
     * @param candidate picked locator candidate
     * @return {@code SHAFT.GUI.Locator...} or {@code By...} Java expression
     */
    public static String snippet(LocatorCandidate candidate) {
        return switch (candidate.strategy()) {
            case TEST_ID, CSS -> "SHAFT.GUI.Locator.cssSelector(\"" + javaString(candidate.expression()) + "\")";
            case ID -> "SHAFT.GUI.Locator.id(\"" + javaString(candidate.expression()) + "\")";
            case NAME -> "SHAFT.GUI.Locator.name(\"" + javaString(candidate.expression()) + "\")";
            case XPATH -> "By.xpath(\"" + javaString(candidate.expression()) + "\")";
            case ROLE, ACCESSIBLE_NAME, LABEL -> "SHAFT.GUI.Locator.hasAnyTagName().containsText(\""
                    + javaString(candidate.expression()) + "\").build()";
        };
    }

    private static String javaString(String value) {
        return value.replace("\\", "\\\\").replace("\"", "\\\"");
    }
}
