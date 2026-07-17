package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.awt.Color;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Guards the WCAG contrast of {@link ShaftStatusPresentation#success()}'s dark-theme fallback.
 *
 * <p>Issue #3634: {@code success()} used to return a hardcoded {@code 0x0A7F26} with no theme
 * awareness, computing to ~2.06:1 contrast against the Darcula panel background (~{@code 0x3C3F41}
 * ) -- well below WCAG's 4.5:1 minimum for normal text. This test computes the WCAG relative
 * luminance / contrast ratio math directly (no external dependency) so a future regression to an
 * unreadable dark-mode fallback fails the build.
 */
class ShaftStatusPresentationTest {
    /** Darcula panel background used elsewhere in the plugin's screenshot/theme fixtures. */
    private static final Color DARK_PANEL_BACKGROUND = new Color(0x3C3F41);
    private static final Color LIGHT_PANEL_BACKGROUND = Color.WHITE;
    private static final double WCAG_AA_MINIMUM_CONTRAST = 4.5D;

    @Test
    void successDarkLegMeetsWcagAaContrastAgainstDarculaPanel() {
        Color darkLeg = new Color(0x81C784);

        double contrast = contrastRatio(darkLeg, DARK_PANEL_BACKGROUND);

        assertTrue(contrast >= WCAG_AA_MINIMUM_CONTRAST,
                "success() dark-theme fallback must meet WCAG AA 4.5:1 against the Darcula panel "
                        + "background, but was " + contrast);
    }

    @Test
    void successLightLegMeetsWcagAaContrastAgainstLightPanel() {
        Color lightLeg = new Color(0x0A7F26);

        double contrast = contrastRatio(lightLeg, LIGHT_PANEL_BACKGROUND);

        assertTrue(contrast >= WCAG_AA_MINIMUM_CONTRAST,
                "success() light-theme fallback must meet WCAG AA 4.5:1 against a light panel "
                        + "background, but was " + contrast);
    }

    /** WCAG 2.x contrast ratio: (L_lighter + 0.05) / (L_darker + 0.05). */
    private static double contrastRatio(Color first, Color second) {
        double firstLuminance = relativeLuminance(first);
        double secondLuminance = relativeLuminance(second);
        double lighter = Math.max(firstLuminance, secondLuminance);
        double darker = Math.min(firstLuminance, secondLuminance);
        return (lighter + 0.05D) / (darker + 0.05D);
    }

    /** WCAG 2.x relative luminance: 0.2126*R + 0.7152*G + 0.0722*B over linearized sRGB channels. */
    private static double relativeLuminance(Color color) {
        double red = linearizeChannel(color.getRed());
        double green = linearizeChannel(color.getGreen());
        double blue = linearizeChannel(color.getBlue());
        return 0.2126D * red + 0.7152D * green + 0.0722D * blue;
    }

    private static double linearizeChannel(int channel8Bit) {
        double channel = channel8Bit / 255.0D;
        return channel <= 0.03928D ? channel / 12.92D : Math.pow((channel + 0.055D) / 1.055D, 2.4D);
    }
}
