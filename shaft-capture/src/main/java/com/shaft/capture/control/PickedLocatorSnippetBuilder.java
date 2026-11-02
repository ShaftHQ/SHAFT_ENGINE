package com.shaft.capture.control;

import com.shaft.capture.generate.LocatorPolicy;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.gui.internal.locator.Role;

/**
 * Renders a single copy-paste {@code SHAFT.GUI.Locator...} (or {@code By...}) Java expression for
 * one picked locator candidate, for the live "Pick Locator" flow (see
 * {@code CaptureControlServer#pickLocator}). Deliberately simpler than
 * {@code com.shaft.capture.generate.CaptureGenerator}'s codegen-time locator rendering (which also
 * has access to the target's semantic role/label/input-vs-clickable classification to prefer
 * {@code inputField(...)}/{@code clickableField(...)}): this builder only sees the picked
 * candidate itself, since a live pick has no surrounding recorded scenario to classify against.
 *
 * <p>Policy (issue #5959): prefer SHAFT builder / role expressions; never emit
 * {@code SHAFT.GUI.Locator.xpath}; raw {@code By.xpath} is last-resort only.
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
            case ROLE -> roleSnippet(candidate.expression());
            case ACCESSIBLE_NAME, LABEL -> "SHAFT.GUI.Locator.hasAnyTagName().containsText(\""
                    + javaString(candidate.expression()) + "\").build()";
        };
    }

    /**
     * Turns a recorder ROLE expression ({@code role} or {@code role:accessibleName}) into a SHAFT
     * {@code hasRole} builder chain. Falls back to containsText when the role token is unmapped.
     */
    static String roleSnippet(String expression) {
        String raw = expression == null ? "" : expression.trim();
        String roleToken;
        String name;
        int colon = raw.indexOf(':');
        if (colon >= 0) {
            roleToken = raw.substring(0, colon).trim();
            name = raw.substring(colon + 1).trim();
        } else {
            roleToken = raw;
            name = "";
        }
        Role role = LocatorPolicy.ariaRole(roleToken);
        if (role == null) {
            return "SHAFT.GUI.Locator.hasAnyTagName().containsText(\"" + javaString(raw) + "\").build()";
        }
        String prefix = "SHAFT.GUI.Locator.hasRole(Role." + role.name() + ")";
        if (name.isBlank()) {
            return prefix + ".build()";
        }
        return prefix + ".hasNormalizedText(\"" + javaString(name) + "\").build()";
    }

    private static String javaString(String value) {
        return value.replace("\\", "\\\\").replace("\"", "\\\"");
    }
}
