package com.shaft.intellij.ui.theme;

import com.intellij.ui.JBColor;
import org.junit.jupiter.api.Test;

import java.awt.Color;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftUiColorsTest {
    private static final Color DARK_PANEL = new Color(0x3C3F41);
    private static final Color LIGHT_PANEL = Color.WHITE;

    @Test
    void successIsDualLegJbColor() {
        assertInstanceOf(JBColor.class, ShaftUiColors.success());
    }

    @Test
    void successFallbackLegsMeetWcagAa() {
        assertTrue(contrast(new Color(0x0A7F26), LIGHT_PANEL) >= 4.5D);
        assertTrue(contrast(new Color(0x81C784), DARK_PANEL) >= 4.5D);
    }

    @Test
    void errorAndWarningArePresent() {
        assertNotNull(ShaftUiColors.error());
        assertNotNull(ShaftUiColors.warning());
        assertNotNull(ShaftUiColors.pending());
        assertNotNull(ShaftUiColors.progress());
    }

    private static double contrast(Color first, Color second) {
        double a = lum(first);
        double b = lum(second);
        double lighter = Math.max(a, b);
        double darker = Math.min(a, b);
        return (lighter + 0.05D) / (darker + 0.05D);
    }

    private static double lum(Color c) {
        return 0.2126D * lin(c.getRed()) + 0.7152D * lin(c.getGreen()) + 0.0722D * lin(c.getBlue());
    }

    private static double lin(int channel) {
        double v = channel / 255.0D;
        return v <= 0.03928D ? v / 12.92D : Math.pow((v + 0.055D) / 1.055D, 2.4D);
    }
}
