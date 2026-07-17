package com.shaft.intellij.ui;

import com.shaft.intellij.testindex.ShaftTestIndex;
import org.junit.jupiter.api.Test;

import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * {@code isFailRow}/{@code formatRowLabel} tests below need no {@link ShaftTestsPanel} instance.
 * The Run/Navigate wiring tests do construct one, with a {@code null} project -- mirroring
 * {@code VisualBaselinesPanelTest}'s {@code new VisualBaselinesPanel(null)} pattern -- since
 * enablement and the no-selection guard never dereference the project. Clicking Run/Navigate
 * *with* a row selected would call into {@code ShaftRunConfigurationResolver}'s real
 * {@code RunManager}/PSI glue, which needs a live IntelliJ platform Project this plain JUnit
 * environment does not have (see {@code ShaftRunConfigurationResolverTest}'s javadoc); those tests
 * therefore only exercise the no-selection no-op path and the selection-driven enablement.
 */
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

    // ------------------------------------------------------------------
    // Run/Navigate wiring
    // ------------------------------------------------------------------

    @Test
    void runAndNavigateAreDisabledWithNoRowSelected() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex);

        assertAll(
                () -> assertFalse(panel.runButtonForTest().isEnabled()),
                () -> assertFalse(panel.navigateMenuItemForTest().isEnabled()));
    }

    @Test
    void runAndNavigateAreEnabledForAPassingRowUnlikeDiagnoseAndHeal() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex);

        panel.rowListForTest().setSelectedIndex(0);

        assertAll(
                () -> assertTrue(panel.runButtonForTest().isEnabled(), "Run enables for any status"),
                () -> assertTrue(panel.navigateMenuItemForTest().isEnabled(), "Navigate enables for any status"),
                () -> assertFalse(panel.diagnoseButtonForTest().isEnabled(), "Diagnose stays PASS-row disabled"),
                () -> assertFalse(panel.healButtonForTest().isEnabled(), "Heal stays PASS-row disabled"));
    }

    @Test
    void runAndNavigateAreDisabledAgainAfterClearingSelection() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex);
        panel.rowListForTest().setSelectedIndex(0);

        panel.rowListForTest().clearSelection();

        assertAll(
                () -> assertFalse(panel.runButtonForTest().isEnabled()),
                () -> assertFalse(panel.navigateMenuItemForTest().isEnabled()));
    }

    @Test
    void doubleClickAndCtrlDoubleClickWithNoSelectionAreSafeNoOps() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex);
        MouseListener[] listeners = panel.rowListForTest().getMouseListeners();
        assertTrue(listeners.length > 0, "a mouse listener should be registered for double-click run/navigate");

        assertAll(
                () -> dispatchDoubleClick(panel, listeners, false),
                () -> dispatchDoubleClick(panel, listeners, true));
    }

    @Test
    void navigateContextMenuItemWithNoSelectionIsASafeNoOp() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex);
        ActionListener[] listeners = panel.navigateMenuItemForTest().getActionListeners();
        assertTrue(listeners.length > 0, "the context-menu Navigate item should have an action listener wired");

        for (ActionListener listener : listeners) {
            listener.actionPerformed(null);
        }
    }

    private static void dispatchDoubleClick(ShaftTestsPanel panel, MouseListener[] listeners, boolean ctrlDown) {
        int modifiers = ctrlDown ? InputEvent.CTRL_DOWN_MASK : 0;
        MouseEvent doubleClick = new MouseEvent(panel.rowListForTest(), MouseEvent.MOUSE_CLICKED,
                System.currentTimeMillis(), modifiers, 0, 0, 2, false);
        for (MouseListener listener : listeners) {
            listener.mouseClicked(doubleClick);
        }
    }
}
