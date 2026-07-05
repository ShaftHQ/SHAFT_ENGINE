package com.shaft.intellij.ui;

import java.awt.Color;

/**
 * Centralized status colors and glyph constants for SHAFT Assistant UI.
 */
public final class ShaftStatusPresentation {
    public static final String SUCCESS_ICON = "✅";
    public static final String PENDING_ICON = "⏳";
    public static final String ERROR_ICON = "❌";
    public static final String WARNING_ICON = "⚠️";

    private ShaftStatusPresentation() {
        throw new IllegalStateException("Utility class");
    }

    public static Color success() {
        return new Color(0x0A7F26);
    }

    public static Color pending() {
        Color foreground = javax.swing.UIManager.getColor("Label.disabledForeground");
        return foreground == null ? Color.GRAY : foreground;
    }

    public static Color progress() {
        Color foreground = javax.swing.UIManager.getColor("Component.focusColor");
        return foreground == null ? new Color(0x0550AE) : foreground;
    }

    public static Color error() {
        Color foreground = javax.swing.UIManager.getColor("ValidationTooltip.errorForeground");
        return foreground == null ? new Color(0xB42318) : foreground;
    }
}
