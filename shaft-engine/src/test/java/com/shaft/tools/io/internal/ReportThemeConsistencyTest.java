package com.shaft.tools.io.internal;

import com.shaft.tools.internal.support.ReportHtmlTheme;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Enforces the single SHAFT brand palette across both report theme surfaces (issue #3504 R3):
 * {@link ReportHtmlTheme#style()} (the {@code --shaft-*} tokens used by SHAFT's own offline HTML
 * reports) and {@link AllureManager#allureThemeColorsStyle()} (the {@code --color-*} overrides
 * injected into the generated Allure report). Both historically hardcoded the same hex palette
 * independently; this test is the drift-guard so the two can never diverge silently.
 */
public class ReportThemeConsistencyTest {

    /** Core brand hexes that must appear in BOTH theme surfaces. */
    private static final String[] SHARED_BRAND_HEXES = {
            "#006ec0", // primary (light)
            "#4cc2ff", // primary (dark)
            "#102a31", // deep (light)
            "#07111f", // deep (dark)
            "#c8d6e7", // muted (light)
            "#dff5f4", // muted (dark)
            "#f5fdff", // on-dark surface text
    };

    /** Status hexes owned by the SHAFT report tokens (not part of the Allure structural block). */
    private static final String[] STATUS_HEXES = {"#14804a", "#b7791f", "#c53030"};

    @Test(description = "The SHAFT report tokens and the Allure theme override must share one brand palette")
    public void reportHtmlThemeAndAllureThemeShareTheBrandPalette() {
        String shaftTheme = ReportHtmlTheme.style().toLowerCase();
        String allureTheme = AllureManager.allureThemeColorsStyle().toLowerCase();

        for (String hex : SHARED_BRAND_HEXES) {
            Assert.assertTrue(shaftTheme.contains(hex),
                    "ReportHtmlTheme.style() must carry the shared brand hex " + hex);
            Assert.assertTrue(allureTheme.contains(hex),
                    "The Allure theme override must carry the shared brand hex " + hex
                            + " (palette drift between the two report surfaces).");
        }
        for (String hex : STATUS_HEXES) {
            Assert.assertTrue(shaftTheme.contains(hex),
                    "ReportHtmlTheme.style() must carry the status hex " + hex);
        }
        // Both surfaces theme light and dark; guard the dark hooks so a future edit can't drop one.
        Assert.assertTrue(shaftTheme.contains("prefers-color-scheme: dark"),
                "ReportHtmlTheme must keep its dark-mode block.");
        Assert.assertTrue(allureTheme.contains("[data-theme=\"dark\"]"),
                "The Allure theme override must keep its dark-mode block.");
    }
}
