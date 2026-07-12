package com.shaft.intellij.ui;

import com.shaft.intellij.testindex.ShaftTestIndex;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

class ShaftTestsPanelTest {
    @Test
    void isFailRowIsTrueOnlyForFailStatus() {
        ShaftTestIndex.TestRowState passRow = new ShaftTestIndex.TestRowState(
                "SignInTest", ShaftTestIndex.Status.PASS, 1_000L, 0);
        ShaftTestIndex.TestRowState failRow = new ShaftTestIndex.TestRowState(
                "CheckoutTest", ShaftTestIndex.Status.FAIL, 1_000L, 1);

        assertFalse(ShaftTestsPanel.isFailRow(passRow));
        assertTrue(ShaftTestsPanel.isFailRow(failRow));
    }

    @Test
    void isFailRowIsFalseForNull() {
        assertFalse(ShaftTestsPanel.isFailRow(null));
    }

    @Test
    void formatRowLabelIncludesStatusAndTestId() {
        ShaftTestIndex.TestRowState row = new ShaftTestIndex.TestRowState(
                "com.example.CheckoutTest", ShaftTestIndex.Status.FAIL, 1_000L, 1);

        String label = ShaftTestsPanel.formatRowLabel(row);

        assertTrue(label.startsWith("FAIL"));
        assertTrue(label.contains("com.example.CheckoutTest"));
    }

    @Test
    void formatRowLabelUsesPassForZeroExitCode() {
        ShaftTestIndex.TestRowState row = new ShaftTestIndex.TestRowState(
                "com.example.SignInTest", ShaftTestIndex.Status.PASS, 1_000L, 0);

        assertTrue(ShaftTestsPanel.formatRowLabel(row).startsWith("PASS"));
    }

    @Test
    void formatRowLabelOmitsTimeForNonPositiveTimestamp() {
        ShaftTestIndex.TestRowState row = new ShaftTestIndex.TestRowState(
                "com.example.SignInTest", ShaftTestIndex.Status.PASS, 0L, 0);

        assertEquals("PASS  com.example.SignInTest  ", ShaftTestsPanel.formatRowLabel(row));
    }
}
