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
    public static final String DISCONNECTED_ICON = "⚡";

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

    public static Color disconnected() {
        Color foreground = javax.swing.UIManager.getColor("ValidationTooltip.warningForeground");
        return foreground == null ? new Color(0xFF9800) : foreground;
    }

    public static Color tint(Color base, Color overlay, double overlayWeight) {
        double bounded = Math.max(0.0D, Math.min(1.0D, overlayWeight));
        double baseWeight = 1.0D - bounded;
        return new Color(
                channel(base.getRed(), overlay.getRed(), baseWeight, bounded),
                channel(base.getGreen(), overlay.getGreen(), baseWeight, bounded),
                channel(base.getBlue(), overlay.getBlue(), baseWeight, bounded));
    }

    private static int channel(int base, int overlay, double baseWeight, double overlayWeight) {
        return Math.max(0, Math.min(255, (int) Math.round(base * baseWeight + overlay * overlayWeight)));
    }
}
