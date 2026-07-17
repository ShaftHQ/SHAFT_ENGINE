package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.treeStructure.Tree;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.notifications.FailedRunDoctorNotifier;
import com.shaft.intellij.notifications.ShaftToolWorkflowLauncher;
import com.shaft.intellij.testindex.ShaftRunConfigurationResolver;
import com.shaft.intellij.testindex.ShaftTestDiscovery;
import com.shaft.intellij.testindex.ShaftTestIndex;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Supplier;

/**
 * "SHAFT Tests" tool-window tab: a package/class/method tree of every SHAFT-runnable {@code @Test}
 * discovered by {@link ShaftTestDiscovery} (project-wide, PSI-based -- so tests that have never run
 * are still visible), decorated with recorded pass/fail/last-run-time from {@link ShaftTestIndex}
 * (run-configuration granularity -- see that class's javadoc) where a matching run exists. Nodes
 * with no matching run render as "not yet run" rather than being hidden.
 * <p>
 * Toolbar Refresh re-runs discovery and re-reads the index (rebuilds the tree); Clear empties only
 * the run-history index (the discovered structure stays, decorations reset). For a selected failed
 * run, one-click "Diagnose with SHAFT Doctor" / "Heal failed test" buttons reuse the same MCP
 * prefill flow as {@code FailedRunDoctorNotifier}'s failed-run balloon.
 * <p>
 * A selected class or method node can also be run, debugged, or navigated to via
 * {@link ShaftRunConfigurationResolver}: the "Run"/"Debug" buttons (or a plain double-click for
 * Run) act on it, while Ctrl+double-click or the row's right-click context menu navigates to its
 * resolved class. Package nodes are not selectable for Run/Debug/Navigate/Diagnose/Heal. Run and
 * Debug are method-granular -- a method node runs or debugs only itself, not its whole containing
 * class (see {@link ShaftRunConfigurationResolver}'s javadoc); Navigate stays class-granular. Each
 * CLASS/METHOD row also renders a pair of small Run/Debug icons ({@link #iconZoneAt}) that fire the
 * same actions on a single click, without needing to select the row first.
 * <p>
 * <b>Deferred (tracked as a follow-up, not implemented here):</b> status still only appears after a
 * run terminates ({@link ShaftTestIndex} is populated from exit code on process end). Live
 * grey/running/pass/fail progression during a run would need wiring this tree to IntelliJ's SM Test
 * Runner protocol ({@code SMTRunnerEventsListener}/{@code SMTestProxy}) -- {@link ShaftTestIndex}'s
 * own javadoc already flags that as deliberately out of scope for this lightweight module today.
 */
final class ShaftTestsPanel extends JPanel {
    private static final DateTimeFormatter TIME_FORMAT = DateTimeFormatter.ofPattern("HH:mm:ss")
            .withZone(ZoneId.systemDefault());

    /** Kind of node in the discovery tree; only {@link #CLASS} and {@link #METHOD} are selectable
     * for Run/Debug/Navigate/Diagnose/Heal. */
    enum NodeKind { PACKAGE, CLASS, METHOD }

    /** Which per-node icon (if any) a tree click landed on; see {@link #iconZoneAt}. */
    enum IconZone { NONE, RUN, DEBUG }

    /** Pixel width of one row icon; matches the platform's standard 16x16 action icons. */
    private static final int ICON_SIZE = 16;
    /** Pixel gap between the two row icons (and between the icon pair and the row text). */
    private static final int ICON_GAP = 2;

    /**
     * Tree-node payload. {@code qualifiedName} is the owning class's fully-qualified name for both
     * {@link NodeKind#CLASS} and {@link NodeKind#METHOD} nodes (a method node's Run/Navigate targets
     * its containing class -- see class javadoc), and {@code null} for {@link NodeKind#PACKAGE}
     * nodes. {@code runState} is the matched run-history row, or {@code null} for "not yet run".
     */
    record TestTreeNode(NodeKind kind, String qualifiedName, String displayName, ShaftTestIndex.TestRowState runState) {
    }

    private final Project project;
    private final ShaftTestIndex testIndex;
    private final Supplier<List<ShaftTestDiscovery.DiscoveredTestClass>> discoverySource;
    private final DefaultTreeModel treeModel = new DefaultTreeModel(new DefaultMutableTreeNode());
    private final Tree tree = new Tree(treeModel);
    private final JButton refreshButton;
    private final JButton clearButton;
    private final JButton runButton;
    private final JButton debugButton;
    private final JButton diagnoseButton;
    private final JButton healButton;
    private final JMenuItem navigateMenuItem;
    private final JMenuItem debugMenuItem;
    private final JPopupMenu rowContextMenu = new JPopupMenu();
    private final JLabel statusLabel = new JLabel(" ");

    ShaftTestsPanel(Project project) {
        this(project, ShaftTestIndex.getInstance(project));
    }

    ShaftTestsPanel(Project project, ShaftTestIndex testIndex) {
        this(project, testIndex, () -> ShaftTestDiscovery.discover(project));
    }

    /**
     * Test-only seam: injects a fixed discovery result instead of running real PSI discovery, which
     * needs a live IntelliJ platform {@link Project} this plain JUnit environment does not have.
     * Mirrors the existing {@code testIndex} injection seam above.
     */
    ShaftTestsPanel(Project project, ShaftTestIndex testIndex,
                     Supplier<List<ShaftTestDiscovery.DiscoveredTestClass>> discoverySource) {
        super(new BorderLayout(8, 8));
        this.project = project;
        this.testIndex = testIndex;
        this.discoverySource = discoverySource;
        setBorder(JBUI.Borders.empty(8));

        refreshButton = button("Refresh", "Re-discover SHAFT tests and reload run history",
                ShaftIcons.RERUN, this::refresh);
        clearButton = button("Clear", "Clear all recorded SHAFT test-run history",
                ShaftIcons.CLEAR, this::clearRows);
        JPanel toolbar = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        toolbar.add(refreshButton);
        toolbar.add(clearButton);

        tree.getAccessibleContext().setAccessibleName("SHAFT tests");
        tree.setRootVisible(false);
        tree.setShowsRootHandles(true);
        tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        tree.setCellRenderer(new DefaultTreeCellRenderer() {
            @Override
            public Component getTreeCellRendererComponent(JTree treeComponent, Object value, boolean selected,
                                                            boolean expanded, boolean leaf, int row, boolean hasFocus) {
                Component component = super.getTreeCellRendererComponent(
                        treeComponent, value, selected, expanded, leaf, row, hasFocus);
                if (component instanceof JLabel label && value instanceof DefaultMutableTreeNode node
                        && node.getUserObject() instanceof TestTreeNode treeNode) {
                    label.setText(formatNodeLabel(treeNode.displayName(), treeNode.runState()));
                    if (treeNode.kind() != NodeKind.PACKAGE) {
                        label.setIcon(new RunDebugIcon());
                        label.setIconTextGap(ICON_GAP + 2);
                    }
                }
                return component;
            }
        });
        tree.addTreeSelectionListener(event -> onSelectionChanged());
        tree.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent event) {
                IconZone zone = iconZoneForEvent(event);
                if (zone != IconZone.NONE) {
                    if (event.getClickCount() == 1) {
                        if (zone == IconZone.RUN) {
                            runSelected();
                        } else {
                            debugSelected();
                        }
                    }
                    // A double-click landing on either icon already ran/debugged on its first
                    // click above -- swallow the second click rather than firing the double-click
                    // Run/Navigate handling below a second time.
                    return;
                }
                if (event.getClickCount() != 2) {
                    return;
                }
                if (event.isControlDown()) {
                    navigateSelected();
                } else {
                    runSelected();
                }
            }

            @Override
            public void mousePressed(MouseEvent event) {
                maybeShowContextMenu(event);
            }

            @Override
            public void mouseReleased(MouseEvent event) {
                maybeShowContextMenu(event);
            }
        });
        navigateMenuItem = new JMenuItem("Navigate to test");
        navigateMenuItem.addActionListener(event -> navigateSelected());
        debugMenuItem = new JMenuItem("Debug test");
        debugMenuItem.addActionListener(event -> debugSelected());
        rowContextMenu.add(navigateMenuItem);
        rowContextMenu.add(debugMenuItem);
        JBScrollPane treeScroll = new JBScrollPane(tree);
        treeScroll.setPreferredSize(JBUI.size(400, 260));

        runButton = button("Run", "Run the selected SHAFT test.", ShaftIcons.RERUN, this::runSelected);
        debugButton = button("Debug", "Debug the selected SHAFT test.",
                ShaftIcons.DEBUG, this::debugSelected);
        diagnoseButton = button("Diagnose with SHAFT Doctor",
                "Prefill a doctor_analyze_failed_allure request for the selected failed run.",
                ShaftIcons.SEARCH, this::diagnoseSelected);
        healButton = button("Heal failed test",
                "Prefill a healer_run_failed_test request for the selected failed run.",
                ShaftIcons.CHECK, this::healSelected);
        runButton.setEnabled(false);
        debugButton.setEnabled(false);
        diagnoseButton.setEnabled(false);
        healButton.setEnabled(false);
        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        actions.add(runButton);
        actions.add(debugButton);
        actions.add(diagnoseButton);
        actions.add(healButton);

        statusLabel.getAccessibleContext().setAccessibleName("SHAFT test runs status");
        statusLabel.setBorder(JBUI.Borders.emptyTop(4));
        JPanel south = new JPanel(new BorderLayout(4, 4));
        south.add(actions, BorderLayout.NORTH);
        south.add(statusLabel, BorderLayout.SOUTH);

        add(toolbar, BorderLayout.NORTH);
        add(treeScroll, BorderLayout.CENTER);
        add(south, BorderLayout.SOUTH);

        refresh();
    }

    private void refresh() {
        List<ShaftTestDiscovery.DiscoveredTestClass> discoveredClasses = discoverySource.get();
        Map<String, ShaftTestIndex.TestRowState> rowsByTestId = new HashMap<>();
        testIndex.snapshot().forEach(row -> rowsByTestId.put(row.testId(), row));

        List<ShaftTestDiscovery.DiscoveredTestClass> sorted = new ArrayList<>(discoveredClasses);
        sorted.sort(Comparator.comparing(ShaftTestDiscovery.DiscoveredTestClass::packageName)
                .thenComparing(ShaftTestDiscovery.DiscoveredTestClass::simpleName));

        DefaultMutableTreeNode newRoot = new DefaultMutableTreeNode();
        Map<String, DefaultMutableTreeNode> packageNodes = new TreeMap<>();
        int decoratedCount = 0;
        for (ShaftTestDiscovery.DiscoveredTestClass discoveredClass : sorted) {
            ShaftTestIndex.TestRowState runState = matchRunState(rowsByTestId, discoveredClass);
            if (runState != null) {
                decoratedCount++;
            }
            DefaultMutableTreeNode packageNode = packageNodes.computeIfAbsent(discoveredClass.packageName(),
                    pkg -> new DefaultMutableTreeNode(new TestTreeNode(
                            NodeKind.PACKAGE, null, pkg.isEmpty() ? "(default package)" : pkg, null)));
            DefaultMutableTreeNode classNode = new DefaultMutableTreeNode(new TestTreeNode(
                    NodeKind.CLASS, discoveredClass.qualifiedName(), discoveredClass.simpleName(), runState));
            for (String methodName : discoveredClass.methodNames()) {
                classNode.add(new DefaultMutableTreeNode(new TestTreeNode(
                        NodeKind.METHOD, discoveredClass.qualifiedName(), methodName, runState)));
            }
            packageNode.add(classNode);
        }
        packageNodes.values().forEach(newRoot::add);
        treeModel.setRoot(newRoot);
        expandAll();
        statusLabel.setText(statusText(discoveredClasses.size(), decoratedCount));
        onSelectionChanged();
    }

    private void expandAll() {
        for (int row = 0; row < tree.getRowCount(); row++) {
            tree.expandRow(row);
        }
    }

    private void clearRows() {
        testIndex.clear();
        refresh();
    }

    private void onSelectionChanged() {
        TestTreeNode selected = selectedNode();
        boolean selectable = selected != null && selected.kind() != NodeKind.PACKAGE;
        boolean failed = selectable && isFailRow(selected.runState());
        runButton.setEnabled(selectable);
        debugButton.setEnabled(selectable);
        navigateMenuItem.setEnabled(selectable);
        debugMenuItem.setEnabled(selectable);
        diagnoseButton.setEnabled(failed);
        healButton.setEnabled(failed);
    }

    private TestTreeNode selectedNode() {
        Object component = tree.getLastSelectedPathComponent();
        if (!(component instanceof DefaultMutableTreeNode node)) {
            return null;
        }
        return node.getUserObject() instanceof TestTreeNode treeNode ? treeNode : null;
    }

    private void maybeShowContextMenu(MouseEvent event) {
        if (!event.isPopupTrigger()) {
            return;
        }
        TreePath path = tree.getPathForLocation(event.getX(), event.getY());
        if (path != null) {
            tree.setSelectionPath(path);
        }
        if (selectedNode() != null) {
            rowContextMenu.show(tree, event.getX(), event.getY());
        }
    }

    /**
     * Determines which per-node icon (if any) a click landed on, by comparing the click's x
     * position against the clicked row's rendered bounds -- see {@link #iconZoneAt} for the actual
     * zone math, kept pure and separately unit tested since it needs no live tree/PSI/project.
     *
     * @param event the mouse click to test
     * @return the icon zone the click landed in, or {@link IconZone#NONE}
     */
    private IconZone iconZoneForEvent(MouseEvent event) {
        TreePath path = tree.getPathForLocation(event.getX(), event.getY());
        if (path == null || !(path.getLastPathComponent() instanceof DefaultMutableTreeNode node)
                || !(node.getUserObject() instanceof TestTreeNode treeNode) || treeNode.kind() == NodeKind.PACKAGE) {
            return IconZone.NONE;
        }
        Rectangle bounds = tree.getPathBounds(path);
        if (bounds == null) {
            return IconZone.NONE;
        }
        return iconZoneAt(event.getX() - bounds.x);
    }

    /**
     * Pure zone math for the two row icons a {@link RunDebugIcon} paints: Run occupies
     * {@code [0, ICON_SIZE)}, a gap follows, then Debug occupies the next {@code ICON_SIZE} pixels,
     * then a final gap before the row's text label. Kept as a standalone static method so the
     * layout math is unit-testable without a rendered {@link JTree}.
     *
     * @param localX x offset from the start of the row's rendered cell (i.e. {@code event.getX()}
     *               minus {@link JTree#getPathBounds}'s x)
     * @return which icon {@code localX} falls within, or {@link IconZone#NONE} for the gaps/text
     */
    static IconZone iconZoneAt(int localX) {
        if (localX < 0 || localX >= ICON_SIZE) {
            int debugStart = ICON_SIZE + ICON_GAP;
            return (localX >= debugStart && localX < debugStart + ICON_SIZE) ? IconZone.DEBUG : IconZone.NONE;
        }
        return IconZone.RUN;
    }

    /** Paints the Run and Debug row icons side by side; see {@link #iconZoneAt} for the matching
     * hit-test zones. */
    private static final class RunDebugIcon implements Icon {
        @Override
        public void paintIcon(Component component, Graphics graphics, int x, int y) {
            ShaftIcons.RERUN.paintIcon(component, graphics, x, y);
            ShaftIcons.DEBUG.paintIcon(component, graphics, x + ICON_SIZE + ICON_GAP, y);
        }

        @Override
        public int getIconWidth() {
            return ICON_SIZE + ICON_GAP + ICON_SIZE;
        }

        @Override
        public int getIconHeight() {
            return ICON_SIZE;
        }
    }

    private void runSelected() {
        TestTreeNode selected = selectedNode();
        if (selected == null || selected.kind() == NodeKind.PACKAGE) {
            return;
        }
        ShaftRunConfigurationResolver.run(project, selected.qualifiedName(), methodNameOf(selected));
    }

    private void debugSelected() {
        TestTreeNode selected = selectedNode();
        if (selected == null || selected.kind() == NodeKind.PACKAGE) {
            return;
        }
        ShaftRunConfigurationResolver.debug(project, selected.qualifiedName(), methodNameOf(selected));
    }

    /**
     * A method node's {@code displayName} is the bare method name (see {@link #refresh}'s tree
     * construction); Run/Debug pass that through so {@link ShaftRunConfigurationResolver} can scope
     * to just that method instead of its whole containing class.
     *
     * @param node the selected node
     * @return the method name for a {@link NodeKind#METHOD} node, or {@code null} for a class node
     */
    private static String methodNameOf(TestTreeNode node) {
        return node.kind() == NodeKind.METHOD ? node.displayName() : null;
    }

    private void navigateSelected() {
        TestTreeNode selected = selectedNode();
        if (selected == null || selected.kind() == NodeKind.PACKAGE) {
            return;
        }
        ShaftRunConfigurationResolver.navigate(project, selected.qualifiedName());
    }

    private void diagnoseSelected() {
        TestTreeNode selected = selectedNode();
        if (selected == null || !isFailRow(selected.runState())) {
            return;
        }
        // No resolved Allure directory is known from this tab's node-level data (unlike
        // FailedRunDoctorNotifier, which resolves it from the just-failed run's project root): pass
        // null so the server auto-discovers the newest evidence instead of guessing a path.
        ShaftToolWorkflowLauncher.open(project, "doctor_analyze_failed_allure",
                FailedRunDoctorNotifier.doctorArguments(null));
    }

    private void healSelected() {
        TestTreeNode selected = selectedNode();
        if (selected == null || !isFailRow(selected.runState())) {
            return;
        }
        ShaftToolWorkflowLauncher.open(project, "healer_run_failed_test",
                FailedRunDoctorNotifier.healerArguments(selected.qualifiedName()));
    }

    // ------------------------------------------------------------------
    // Pure node/row logic (unit tested)
    // ------------------------------------------------------------------

    /**
     * Returns whether a run-history row is a failed run whose Doctor/Heal actions should be enabled.
     *
     * @param row matched run-history row for a node, or {@code null} when the node has never run
     * @return {@code true} only for a non-null {@link ShaftTestIndex.Status#FAIL} row
     */
    static boolean isFailRow(ShaftTestIndex.TestRowState row) {
        return row != null && row.status() == ShaftTestIndex.Status.FAIL;
    }

    /**
     * Formats a run-history row for display: status, test id, and last-run time.
     *
     * @param row row to format
     * @return display label, for example {@code "FAIL  CheckoutTest  14:32:10"}
     */
    static String formatRowLabel(ShaftTestIndex.TestRowState row) {
        String statusText = row.status() == ShaftTestIndex.Status.PASS ? "PASS" : "FAIL";
        return statusText + "  " + row.testId() + "  " + formatTimestamp(row.lastRunAtMillis());
    }

    /**
     * Formats a tree node's label: its display name (simple class or method name), decorated with
     * status and last-run time when a matching run-history row exists, or plain when it has never
     * run -- the whole point of this tree is that undiscovered/never-run tests stay visible rather
     * than being hidden.
     *
     * @param displayName the node's simple class or method name (or package name)
     * @param runState    matched run-history row, or {@code null} for "not yet run"
     * @return display label, for example {@code "FAIL  CheckoutTest  14:32:10"} or plain
     *         {@code "SearchTest"} when never run
     */
    static String formatNodeLabel(String displayName, ShaftTestIndex.TestRowState runState) {
        if (runState == null) {
            return displayName;
        }
        String statusText = runState.status() == ShaftTestIndex.Status.PASS ? "PASS" : "FAIL";
        return statusText + "  " + displayName + "  " + formatTimestamp(runState.lastRunAtMillis());
    }

    /**
     * Matches a discovered class to its run-history row, best-effort by qualified name then simple
     * name -- the same "no exhaustive disambiguation" tradeoff as
     * {@link ShaftRunConfigurationResolver}'s short-name class resolution (see that class's javadoc).
     *
     * @param rowsByTestId    recorded run-history rows, keyed by {@code testId}
     * @param discoveredClass the discovered class to match
     * @return the matched row, or {@code null} when the class has never been run
     */
    private static ShaftTestIndex.TestRowState matchRunState(
            Map<String, ShaftTestIndex.TestRowState> rowsByTestId,
            ShaftTestDiscovery.DiscoveredTestClass discoveredClass) {
        ShaftTestIndex.TestRowState exact = rowsByTestId.get(discoveredClass.qualifiedName());
        return exact != null ? exact : rowsByTestId.get(discoveredClass.simpleName());
    }

    private static String statusText(int discoveredCount, int decoratedCount) {
        return discoveredCount == 0
                ? "No SHAFT tests found in this project."
                : discoveredCount + " test(s) discovered, " + decoratedCount + " with recorded runs.";
    }

    private static String formatTimestamp(long millis) {
        if (millis <= 0) {
            return "";
        }
        return TIME_FORMAT.format(Instant.ofEpochMilli(millis));
    }

    // ------------------------------------------------------------------
    // Layout helpers (mirrors VisualBaselinesPanel's conventions)
    // ------------------------------------------------------------------

    private static JButton button(String text, String description, Icon icon, Runnable action) {
        JButton buttonComponent = new JButton();
        ShaftIconButtons.apply(buttonComponent, description, text, icon);
        buttonComponent.getAccessibleContext().setAccessibleDescription(description);
        buttonComponent.addActionListener(event -> action.run());
        return buttonComponent;
    }

    // ------------------------------------------------------------------
    // Test-only accessors
    // ------------------------------------------------------------------

    JComponent refreshButtonForTest() {
        return refreshButton;
    }

    JComponent clearButtonForTest() {
        return clearButton;
    }

    Tree treeForTest() {
        return tree;
    }

    JComponent diagnoseButtonForTest() {
        return diagnoseButton;
    }

    JComponent healButtonForTest() {
        return healButton;
    }

    JButton runButtonForTest() {
        return runButton;
    }

    JButton debugButtonForTest() {
        return debugButton;
    }

    JMenuItem navigateMenuItemForTest() {
        return navigateMenuItem;
    }

    JMenuItem debugMenuItemForTest() {
        return debugMenuItem;
    }

    JLabel statusLabelForTest() {
        return statusLabel;
    }
}
