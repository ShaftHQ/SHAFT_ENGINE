package com.shaft.intellij.ui.theme;

import com.intellij.ui.JBColor;
import org.jetbrains.annotations.NotNull;

import java.awt.Color;

/**
 * Dual-leg (light/dark) status and chrome colors for the SHAFT plugin (W1.3 / Carl F05).
 *
 * <p>Every public color is a {@link JBColor} with explicit light and dark legs. Callers must not
 * introduce single-leg {@code new Color(0x…)} fallbacks for status chrome.
 */
public final class ShaftUiColors {
    private ShaftUiColors() {
        throw new IllegalStateException("Utility class");
    }

    /** Success / verified (light 0x0A7F26, dark 0x81C784 — WCAG AA on white / Darcula panel). */
    public static @NotNull Color success() {
        return JBColor.namedColor(
                "ValidationTooltip.successForeground",
                new JBColor(new Color(0x0A7F26), new Color(0x81C784)));
    }

    /** Pending / idle. */
    public static @NotNull Color pending() {
        return JBColor.namedColor(
                "Label.disabledForeground",
                new JBColor(new Color(0x6B7280), new Color(0x9AA0A6)));
    }

    /** In-progress / focus. */
    public static @NotNull Color progress() {
        return JBColor.namedColor(
                "Component.focusColor",
                new JBColor(new Color(0x0550AE), new Color(0x82B1FF)));
    }

    /** Error / failed verification. */
    public static @NotNull Color error() {
        return JBColor.namedColor(
                "ValidationTooltip.errorForeground",
                new JBColor(new Color(0xB42318), new Color(0xFF8A80)));
    }

    /** Warning / disconnected. */
    public static @NotNull Color warning() {
        return JBColor.namedColor(
                "ValidationTooltip.warningForeground",
                new JBColor(new Color(0xB54708), new Color(0xFFB74D)));
    }
}
