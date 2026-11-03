package com.shaft.intellij.ui.theme;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

class ShaftUiMetricsTest {
    @Test
    void fontsAndInsetsAreNonNull() {
        assertNotNull(ShaftUiMetrics.headerFont());
        assertNotNull(ShaftUiMetrics.sectionFont());
        assertNotNull(ShaftUiMetrics.bodyFont());
        assertNotNull(ShaftUiMetrics.smallFont());
        assertNotNull(ShaftUiMetrics.emptyMd());
        assertNotNull(ShaftUiMetrics.emptyLg());
        assertNotNull(ShaftUiMetrics.insetsSm());
        assertNotNull(ShaftUiMetrics.insetsMd());
        assertEquals(4, ShaftUiMetrics.PAD_SM);
        assertEquals(8, ShaftUiMetrics.PAD_MD);
        assertEquals(16, ShaftUiMetrics.PAD_LG);
    }
}
