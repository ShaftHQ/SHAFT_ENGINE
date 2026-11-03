package com.shaft.intellij.ui.theme;

import com.intellij.util.ui.JBFont;
import com.intellij.util.ui.JBUI;
import org.jetbrains.annotations.NotNull;

import javax.swing.border.Border;
import java.awt.Font;
import java.awt.Insets;

/**
 * Spacing and typography tokens that respect Appearance font scaling (W1.4 / Carl F08–F09).
 */
public final class ShaftUiMetrics {
    public static final int PAD_SM = 4;
    public static final int PAD_MD = 8;
    public static final int PAD_LG = 16;

    private ShaftUiMetrics() {
        throw new IllegalStateException("Utility class");
    }

    public static @NotNull Insets insetsSm() {
        return JBUI.insets(PAD_SM);
    }

    public static @NotNull Insets insetsMd() {
        return JBUI.insets(PAD_MD);
    }

    public static @NotNull Border emptyMd() {
        return JBUI.Borders.empty(PAD_MD);
    }

    public static @NotNull Border emptyLg() {
        return JBUI.Borders.empty(PAD_LG);
    }

    public static @NotNull Font headerFont() {
        return JBFont.h3().asBold();
    }

    public static @NotNull Font sectionFont() {
        return JBFont.label().asBold();
    }

    public static @NotNull Font bodyFont() {
        return JBFont.label();
    }

    public static @NotNull Font smallFont() {
        return JBFont.small();
    }
}
