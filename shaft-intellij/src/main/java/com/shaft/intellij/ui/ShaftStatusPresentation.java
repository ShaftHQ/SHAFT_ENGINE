package com.shaft.intellij.ui;

import com.shaft.intellij.ui.theme.ShaftUiColors;
import org.jetbrains.annotations.NotNull;

import java.awt.Color;

/**
 * Centralized status colors and non-emoji glyphs for SHAFT UI (W1.3 / Carl F07).
 *
 * <p>Status strings use short ASCII markers so Swing labels stay readable in New UI without emoji
 * glyphs. Colors always come from {@link ShaftUiColors} dual-leg tokens.
 */
public final class ShaftStatusPresentation {
    public static final String SUCCESS_ICON = "OK";
    public static final String PENDING_ICON = "...";
    public static final String ERROR_ICON = "ERR";
    public static final String WARNING_ICON = "WARN";
    public static final String DISCONNECTED_ICON = "OFF";

    private ShaftStatusPresentation() {
        throw new IllegalStateException("Utility class");
    }

    /** Non-negative elapsed milliseconds for status text. */
    public static String formatElapsed(long elapsedMillis) {
        return Math.max(0L, elapsedMillis) + " ms";
    }

    public static @NotNull Color success() {
        return ShaftUiColors.success();
    }

    public static @NotNull Color pending() {
        return ShaftUiColors.pending();
    }

    public static @NotNull Color progress() {
        return ShaftUiColors.progress();
    }

    public static @NotNull Color error() {
        return ShaftUiColors.error();
    }

    public static @NotNull Color disconnected() {
        return ShaftUiColors.warning();
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
