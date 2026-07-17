package com.shaft.intellij.ui;

import com.shaft.intellij.testindex.ShaftTestDiscovery;
import com.shaft.intellij.testindex.ShaftTestIndex;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * {@code isFailRow}/{@code formatRowLabel} tests below need no {@link ShaftTestsPanel} instance.
 * The Run/Navigate wiring tests do construct one, with a {@code null} project -- mirroring
 * {@code VisualBaselinesPanelTest}'s {@code new VisualBaselinesPanel(null)} pattern -- since
 * enablement and the no-selection guard never dereference the project. They drive the tree through
 * the test-only 3-arg constructor that injects a fixed {@link ShaftTestDiscovery.DiscoveredTestClass}
 * list instead of running real PSI discovery, which needs a live IntelliJ platform Project this
 * plain JUnit environment does not have (see {@code ShaftRunConfigurationResolverTest}'s javadoc).
 * Clicking Run/Navigate *with* a node selected would call into
 * {@code ShaftRunConfigurationResolver}'s real {@code RunManager}/PSI glue, needing that same live
 * Project; those tests therefore only exercise the no-selection no-op path and selection-driven
 * enablement.
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
    void isFailRowIsFalseForARunningInFlightRow() {
        ShaftTestIndex.TestRowState runningRow = new ShaftTestIndex.TestRowState(
                "SignInTest", ShaftTestIndex.Status.RUNNING, 1_000L, 0);

        assertFalse(ShaftTestsPanel.isFailRow(runningRow),
                "Diagnose/Heal must stay disabled while a test is still running, not just for a pass");
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
    void formatRowLabelUsesRunningForALiveInFlightRow() {
        ShaftTestIndex.TestRowState row = new ShaftTestIndex.TestRowState(
                "com.example.SignInTest", ShaftTestIndex.Status.RUNNING, 1_000L, 0);

        assertTrue(ShaftTestsPanel.formatRowLabel(row).startsWith("RUNNING"));
    }

    @Test
    void formatRowLabelOmitsTimeForNonPositiveTimestamp() {
        ShaftTestIndex.TestRowState row = new ShaftTestIndex.TestRowState(
                "com.example.SignInTest", ShaftTestIndex.Status.PASS, 0L, 0);

        assertEquals("PASS  com.example.SignInTest  ", ShaftTestsPanel.formatRowLabel(row));
    }

    // ------------------------------------------------------------------
    // Tree-node label logic
    // ------------------------------------------------------------------

    @Test
    void formatNodeLabelIsPlainDisplayNameWhenNeverRun() {
        assertEquals("SearchTest", ShaftTestsPanel.formatNodeLabel("SearchTest", null));
    }

    @Test
    void formatNodeLabelIncludesStatusAndDisplayNameWhenRun() {
        ShaftTestIndex.TestRowState row = new ShaftTestIndex.TestRowState(
                "com.example.CheckoutTest", ShaftTestIndex.Status.FAIL, 1_000L, 1);

        String label = ShaftTestsPanel.formatNodeLabel("CheckoutTest", row);

        assertTrue(label.startsWith("FAIL"));
        assertTrue(label.contains("CheckoutTest"));
    }

    @Test
    void formatNodeLabelUsesRunningForALiveInFlightRow() {
        ShaftTestIndex.TestRowState row = new ShaftTestIndex.TestRowState(
                "com.example.SignInTest", ShaftTestIndex.Status.RUNNING, 1_000L, 0);

        assertTrue(ShaftTestsPanel.formatNodeLabel("SignInTest", row).startsWith("RUNNING"));
    }

    // ------------------------------------------------------------------
    // Discovery tree structure
    // ------------------------------------------------------------------

    @Test
    void refreshBuildsPackageClassMethodTreeFromDiscoveryAndDecoratesMatchingRuns() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("com.example.SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex, () -> List.of(
                new ShaftTestDiscovery.DiscoveredTestClass(
                        "com.example.SignInTest", "com.example", "SignInTest", List.of("testSignIn")),
                new ShaftTestDiscovery.DiscoveredTestClass(
                        "com.example.SearchTest", "com.example", "SearchTest", List.of("testSearch"))));

        DefaultMutableTreeNode root = (DefaultMutableTreeNode) panel.treeForTest().getModel().getRoot();
        DefaultMutableTreeNode packageNode = (DefaultMutableTreeNode) root.getFirstChild();
        // Classes sort alphabetically by simple name: "SearchTest" < "SignInTest".
        DefaultMutableTreeNode searchNode = (DefaultMutableTreeNode) packageNode.getChildAt(0);
        DefaultMutableTreeNode signInNode = (DefaultMutableTreeNode) packageNode.getChildAt(1);

        assertAll(
                () -> assertEquals(1, root.getChildCount(), "one package node"),
                () -> assertEquals(2, packageNode.getChildCount(), "two class nodes"),
                () -> assertEquals(1, signInNode.getChildCount(), "one method node under SignInTest"),
                () -> assertFalse(ShaftTestsPanel.isFailRow(
                        ((ShaftTestsPanel.TestTreeNode) searchNode.getUserObject()).runState()),
                        "SearchTest has no recorded run"),
                () -> assertEquals(ShaftTestIndex.Status.PASS,
                        ((ShaftTestsPanel.TestTreeNode) signInNode.getUserObject()).runState().status(),
                        "SignInTest is decorated with its matching recorded run"),
                () -> assertEquals("2 test(s) discovered, 1 with recorded runs.",
                        panel.statusLabelForTest().getText()));
    }

    @Test
    void refreshWithNoDiscoveredClassesShowsEmptyStatusMessage() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex, List::of);

        assertEquals("No SHAFT tests found in this project.", panel.statusLabelForTest().getText());
    }

    @Test
    void clearResetsDecorationsButKeepsDiscoveredTreeStructure() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("com.example.SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex, () -> List.of(
                new ShaftTestDiscovery.DiscoveredTestClass(
                        "com.example.SignInTest", "com.example", "SignInTest", List.of("testSignIn"))));

        ((JButton) panel.clearButtonForTest()).doClick();

        DefaultMutableTreeNode root = (DefaultMutableTreeNode) panel.treeForTest().getModel().getRoot();
        DefaultMutableTreeNode packageNode = (DefaultMutableTreeNode) root.getFirstChild();
        DefaultMutableTreeNode classNode = (DefaultMutableTreeNode) packageNode.getFirstChild();

        assertAll(
                () -> assertEquals(1, root.getChildCount(), "the discovered structure survives Clear"),
                () -> assertNull(((ShaftTestsPanel.TestTreeNode) classNode.getUserObject()).runState(),
                        "the run-history decoration is reset to not-yet-run"));
    }

    // ------------------------------------------------------------------
    // Per-method live status (issue #3688): a method node prefers its own recorded row over the
    // class-level fallback every other node under the class shares.
    // ------------------------------------------------------------------

    @Test
    void methodNodePrefersItsOwnRecordedRowOverTheClassLevelFallback() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("com.example.SignInTest", 0, 1_000L);
        testIndex.recordStatus("com.example.SignInTest#testSignIn", ShaftTestIndex.Status.RUNNING, 2_000L);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex, () -> List.of(
                new ShaftTestDiscovery.DiscoveredTestClass(
                        "com.example.SignInTest", "com.example", "SignInTest", List.of("testSignIn"))));

        DefaultMutableTreeNode root = (DefaultMutableTreeNode) panel.treeForTest().getModel().getRoot();
        DefaultMutableTreeNode packageNode = (DefaultMutableTreeNode) root.getFirstChild();
        DefaultMutableTreeNode classNode = (DefaultMutableTreeNode) packageNode.getFirstChild();
        DefaultMutableTreeNode methodNode = (DefaultMutableTreeNode) classNode.getFirstChild();

        assertAll(
                () -> assertEquals(ShaftTestIndex.Status.PASS,
                        ((ShaftTestsPanel.TestTreeNode) classNode.getUserObject()).runState().status(),
                        "the class node keeps showing the class-level (process-terminated) row"),
                () -> assertEquals(ShaftTestIndex.Status.RUNNING,
                        ((ShaftTestsPanel.TestTreeNode) methodNode.getUserObject()).runState().status(),
                        "the method node prefers its own live per-method row over the class-level fallback"));
    }

    @Test
    void methodNodeFallsBackToTheClassLevelRowWhenItHasNoOwnRecordedRow() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("com.example.SignInTest", 1, 1_000L);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex, () -> List.of(
                new ShaftTestDiscovery.DiscoveredTestClass(
                        "com.example.SignInTest", "com.example", "SignInTest", List.of("testSignIn"))));

        DefaultMutableTreeNode root = (DefaultMutableTreeNode) panel.treeForTest().getModel().getRoot();
        DefaultMutableTreeNode packageNode = (DefaultMutableTreeNode) root.getFirstChild();
        DefaultMutableTreeNode classNode = (DefaultMutableTreeNode) packageNode.getFirstChild();
        DefaultMutableTreeNode methodNode = (DefaultMutableTreeNode) classNode.getFirstChild();

        assertEquals(ShaftTestIndex.Status.FAIL,
                ((ShaftTestsPanel.TestTreeNode) methodNode.getUserObject()).runState().status(),
                "no per-method row was recorded, so the method node inherits the class-level row "
                        + "(same v1 behavior as before issue #3688)");
    }

    @Test
    void methodNodeIsUndecoratedWhenNeitherItsOwnNorTheClassRowExists() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex, () -> List.of(
                new ShaftTestDiscovery.DiscoveredTestClass(
                        "com.example.SignInTest", "com.example", "SignInTest", List.of("testSignIn"))));

        DefaultMutableTreeNode root = (DefaultMutableTreeNode) panel.treeForTest().getModel().getRoot();
        DefaultMutableTreeNode packageNode = (DefaultMutableTreeNode) root.getFirstChild();
        DefaultMutableTreeNode classNode = (DefaultMutableTreeNode) packageNode.getFirstChild();
        DefaultMutableTreeNode methodNode = (DefaultMutableTreeNode) classNode.getFirstChild();

        assertNull(((ShaftTestsPanel.TestTreeNode) methodNode.getUserObject()).runState());
    }

    // ------------------------------------------------------------------
    // Live refresh (issue #3688): the panel re-reads the index as soon as it changes while
    // attached, instead of only on a manual Refresh click.
    //
    // NOTE: addNotify()/removeNotify() themselves cannot be called end-to-end here -- see
    // ShaftTestsPanel#indexListenerForTest's javadoc: super.addNotify() cascades into the platform's
    // own Tree widget, which unconditionally touches ApplicationManager.getApplication() the first
    // time it creates a real Swing peer, an NPE in this headless, no-platform-fixture Gradle test
    // JVM unrelated to this change. These tests instead drive the exact captured listener reference
    // addNotify()/removeNotify() would register/deregister, via indexListenerForTest().
    // ------------------------------------------------------------------

    @Test
    void indexListenerTriggersARefreshReflectingTheChange() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex, () -> List.of(
                new ShaftTestDiscovery.DiscoveredTestClass(
                        "com.example.SignInTest", "com.example", "SignInTest", List.of("testSignIn"))));
        testIndex.addListener(panel.indexListenerForTest());

        testIndex.recordStatus("com.example.SignInTest", ShaftTestIndex.Status.RUNNING, 1_000L);

        DefaultMutableTreeNode root = (DefaultMutableTreeNode) panel.treeForTest().getModel().getRoot();
        DefaultMutableTreeNode packageNode = (DefaultMutableTreeNode) root.getFirstChild();
        DefaultMutableTreeNode classNode = (DefaultMutableTreeNode) packageNode.getFirstChild();
        assertEquals(ShaftTestIndex.Status.RUNNING,
                ((ShaftTestsPanel.TestTreeNode) classNode.getUserObject()).runState().status(),
                "the tree must reflect the index change without a manual Refresh click");

        testIndex.removeListener(panel.indexListenerForTest());
    }

    @Test
    void indexListenerForTestReturnsTheSameReferenceEveryCall() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex, List::of);

        // The #3621-class regression this guards against: addNotify()/removeNotify() must pass the
        // exact same Runnable reference to add/removeListener, or removeListener's plain
        // List.remove(Object) silently fails to find and remove the one addNotify() registered.
        assertTrue(panel.indexListenerForTest() == panel.indexListenerForTest(),
                "the listener field must be captured once, not re-evaluated per call");

        testIndex.addListener(panel.indexListenerForTest());
        assertEquals(1, testIndex.listenerCountForTest());

        testIndex.removeListener(panel.indexListenerForTest());
        assertEquals(0, testIndex.listenerCountForTest(),
                "removeListener must find and remove the exact reference addListener registered");
    }

    @Test
    void repeatedAddRemoveListenerCyclesWithTheCapturedReferenceDoNotLeak() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        ShaftTestsPanel panel = new ShaftTestsPanel(null, testIndex, List::of);

        for (int cycle = 0; cycle < 5; cycle++) {
            testIndex.addListener(panel.indexListenerForTest());
            testIndex.removeListener(panel.indexListenerForTest());
        }

        assertEquals(0, testIndex.listenerCountForTest());
    }

    // ------------------------------------------------------------------
    // Run/Navigate wiring
    // ------------------------------------------------------------------

    private static ShaftTestsPanel panelWithOneDiscoveredClass(ShaftTestIndex testIndex) {
        return new ShaftTestsPanel(null, testIndex, () -> List.of(
                new ShaftTestDiscovery.DiscoveredTestClass(
                        "SignInTest", "", "SignInTest", List.of("testSignIn"))));
    }

    private static void selectFirstClassNode(ShaftTestsPanel panel) {
        DefaultMutableTreeNode root = (DefaultMutableTreeNode) panel.treeForTest().getModel().getRoot();
        DefaultMutableTreeNode packageNode = (DefaultMutableTreeNode) root.getFirstChild();
        DefaultMutableTreeNode classNode = (DefaultMutableTreeNode) packageNode.getFirstChild();
        panel.treeForTest().setSelectionPath(new TreePath(classNode.getPath()));
    }

    @Test
    void runAndNavigateAreDisabledWithNoRowSelected() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = panelWithOneDiscoveredClass(testIndex);

        assertAll(
                () -> assertFalse(panel.runButtonForTest().isEnabled()),
                () -> assertFalse(panel.navigateMenuItemForTest().isEnabled()));
    }

    @Test
    void runAndNavigateAreEnabledForAPassingRowUnlikeDiagnoseAndHeal() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = panelWithOneDiscoveredClass(testIndex);

        selectFirstClassNode(panel);

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
        ShaftTestsPanel panel = panelWithOneDiscoveredClass(testIndex);
        selectFirstClassNode(panel);

        panel.treeForTest().clearSelection();

        assertAll(
                () -> assertFalse(panel.runButtonForTest().isEnabled()),
                () -> assertFalse(panel.navigateMenuItemForTest().isEnabled()));
    }

    @Test
    void doubleClickAndCtrlDoubleClickWithNoSelectionAreSafeNoOps() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = panelWithOneDiscoveredClass(testIndex);
        MouseListener[] listeners = panel.treeForTest().getMouseListeners();
        assertTrue(listeners.length > 0, "a mouse listener should be registered for double-click run/navigate");

        assertAll(
                () -> dispatchDoubleClick(panel, listeners, false),
                () -> dispatchDoubleClick(panel, listeners, true));
    }

    @Test
    void navigateContextMenuItemWithNoSelectionIsASafeNoOp() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = panelWithOneDiscoveredClass(testIndex);
        ActionListener[] listeners = panel.navigateMenuItemForTest().getActionListeners();
        assertTrue(listeners.length > 0, "the context-menu Navigate item should have an action listener wired");

        for (ActionListener listener : listeners) {
            listener.actionPerformed(null);
        }
    }

    // ------------------------------------------------------------------
    // Debug wiring (mirrors Run wiring above)
    // ------------------------------------------------------------------

    @Test
    void debugIsDisabledWithNoRowSelected() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = panelWithOneDiscoveredClass(testIndex);

        assertAll(
                () -> assertFalse(panel.debugButtonForTest().isEnabled()),
                () -> assertFalse(panel.debugMenuItemForTest().isEnabled()));
    }

    @Test
    void debugIsEnabledForAPassingRowJustLikeRun() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = panelWithOneDiscoveredClass(testIndex);

        selectFirstClassNode(panel);

        assertAll(
                () -> assertTrue(panel.debugButtonForTest().isEnabled(), "Debug enables for any status"),
                () -> assertTrue(panel.debugMenuItemForTest().isEnabled(), "Debug menu item enables for any status"));
    }

    @Test
    void debugIsDisabledAgainAfterClearingSelection() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = panelWithOneDiscoveredClass(testIndex);
        selectFirstClassNode(panel);

        panel.treeForTest().clearSelection();

        assertAll(
                () -> assertFalse(panel.debugButtonForTest().isEnabled()),
                () -> assertFalse(panel.debugMenuItemForTest().isEnabled()));
    }

    @Test
    void debugButtonWithNoSelectionIsASafeNoOp() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = panelWithOneDiscoveredClass(testIndex);

        assertDoesNotThrow(() -> ((JButton) panel.debugButtonForTest()).doClick(),
                "Debug with no row selected must be a safe no-op, not throw");
    }

    @Test
    void debugContextMenuItemWithNoSelectionIsASafeNoOp() {
        ShaftTestIndex testIndex = ShaftTestIndex.getInstance(null);
        testIndex.recordRun("SignInTest", 0, 1_000L);
        ShaftTestsPanel panel = panelWithOneDiscoveredClass(testIndex);
        ActionListener[] listeners = panel.debugMenuItemForTest().getActionListeners();
        assertTrue(listeners.length > 0, "the context-menu Debug item should have an action listener wired");

        for (ActionListener listener : listeners) {
            listener.actionPerformed(null);
        }
    }

    // ------------------------------------------------------------------
    // Per-node Run/Debug icon hit-testing (pure math, see ShaftTestsPanel#iconZoneAt)
    // ------------------------------------------------------------------

    @Test
    void iconZoneAtIsRunAcrossTheRunIconWidth() {
        assertEquals(ShaftTestsPanel.IconZone.RUN, ShaftTestsPanel.iconZoneAt(0));
        assertEquals(ShaftTestsPanel.IconZone.RUN, ShaftTestsPanel.iconZoneAt(15));
    }

    @Test
    void iconZoneAtIsDebugAcrossTheDebugIconWidth() {
        assertEquals(ShaftTestsPanel.IconZone.DEBUG, ShaftTestsPanel.iconZoneAt(18));
        assertEquals(ShaftTestsPanel.IconZone.DEBUG, ShaftTestsPanel.iconZoneAt(33));
    }

    @Test
    void iconZoneAtIsNoneBeforeZeroInTheGapAndAfterTheDebugIcon() {
        assertAll(
                () -> assertEquals(ShaftTestsPanel.IconZone.NONE, ShaftTestsPanel.iconZoneAt(-1)),
                () -> assertEquals(ShaftTestsPanel.IconZone.NONE, ShaftTestsPanel.iconZoneAt(16), "gap"),
                () -> assertEquals(ShaftTestsPanel.IconZone.NONE, ShaftTestsPanel.iconZoneAt(17), "gap"),
                () -> assertEquals(ShaftTestsPanel.IconZone.NONE, ShaftTestsPanel.iconZoneAt(34), "text area"));
    }

    private static void dispatchDoubleClick(ShaftTestsPanel panel, MouseListener[] listeners, boolean ctrlDown) {
        int modifiers = ctrlDown ? InputEvent.CTRL_DOWN_MASK : 0;
        MouseEvent doubleClick = new MouseEvent(panel.treeForTest(), MouseEvent.MOUSE_CLICKED,
                System.currentTimeMillis(), modifiers, 0, 0, 2, false);
        for (MouseListener listener : listeners) {
            listener.mouseClicked(doubleClick);
        }
    }
}
