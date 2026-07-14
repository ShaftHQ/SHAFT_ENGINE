package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBList;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.notifications.FailedRunDoctorNotifier;
import com.shaft.intellij.notifications.ShaftToolWorkflowLauncher;
import com.shaft.intellij.testindex.ShaftTestIndex;

import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.List;

/**
 * "SHAFT Tests" tool-window tab: lists recent SHAFT test-run results from {@link ShaftTestIndex}
 * (run-configuration granularity -- see that class's javadoc) with Refresh/Clear actions and, for
 * a selected failed run, one-click "Diagnose with SHAFT Doctor" / "Heal failed test" buttons that
 * reuse the same MCP prefill flow as {@code FailedRunDoctorNotifier}'s failed-run balloon.
 */
final class ShaftTestsPanel extends JPanel {
    private static final DateTimeFormatter TIME_FORMAT = DateTimeFormatter.ofPattern("HH:mm:ss")
            .withZone(ZoneId.systemDefault());

    private final Project project;
    private final ShaftTestIndex testIndex;
    private final DefaultListModel<ShaftTestIndex.TestRowState> listModel = new DefaultListModel<>();
    private final JBList<ShaftTestIndex.TestRowState> rowList = new JBList<>(listModel);
    private final JButton refreshButton;
    private final JButton clearButton;
    private final JButton diagnoseButton;
    private final JButton healButton;
    private final JLabel statusLabel = new JLabel(" ");

    ShaftTestsPanel(Project project) {
        this(project, ShaftTestIndex.getInstance(project));
    }

    ShaftTestsPanel(Project project, ShaftTestIndex testIndex) {
        super(new BorderLayout(8, 8));
        this.project = project;
        this.testIndex = testIndex;
        setBorder(JBUI.Borders.empty(8));

        refreshButton = button("Refresh", "Reload rows from the SHAFT test run index",
                ShaftIcons.RERUN, this::refresh);
        clearButton = button("Clear", "Clear all recorded SHAFT test-run rows",
                ShaftIcons.CLEAR, this::clearRows);
        JPanel toolbar = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        toolbar.add(refreshButton);
        toolbar.add(clearButton);

        rowList.getAccessibleContext().setAccessibleName("SHAFT test runs");
        rowList.setCellRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list, Object value, int index,
                                                           boolean isSelected, boolean cellHasFocus) {
                JLabel label = (JLabel) super.getListCellRendererComponent(
                        list, value, index, isSelected, cellHasFocus);
                if (value instanceof ShaftTestIndex.TestRowState row) {
                    label.setText(formatRowLabel(row));
                }
                return label;
            }
        });
        rowList.addListSelectionListener(event -> {
            if (!event.getValueIsAdjusting()) {
                onSelectionChanged();
            }
        });
        JBScrollPane listScroll = new JBScrollPane(rowList);
        listScroll.setPreferredSize(JBUI.size(400, 260));

        diagnoseButton = button("Diagnose with SHAFT Doctor",
                "Prefill a doctor_analyze_failed_allure request for the selected failed run.",
                ShaftIcons.SEARCH, this::diagnoseSelected);
        healButton = button("Heal failed test",
                "Prefill a healer_run_failed_test request for the selected failed run.",
                ShaftIcons.CHECK, this::healSelected);
        diagnoseButton.setEnabled(false);
        healButton.setEnabled(false);
        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        actions.add(diagnoseButton);
        actions.add(healButton);

        statusLabel.getAccessibleContext().setAccessibleName("SHAFT test runs status");
        statusLabel.setBorder(JBUI.Borders.emptyTop(4));
        JPanel south = new JPanel(new BorderLayout(4, 4));
        south.add(actions, BorderLayout.NORTH);
        south.add(statusLabel, BorderLayout.SOUTH);

        add(toolbar, BorderLayout.NORTH);
        add(listScroll, BorderLayout.CENTER);
        add(south, BorderLayout.SOUTH);

        refresh();
    }

    private void refresh() {
        listModel.clear();
        List<ShaftTestIndex.TestRowState> rows = testIndex.snapshot();
        rows.forEach(listModel::addElement);
        statusLabel.setText(rows.isEmpty()
                ? "No test runs recorded yet. Run a SHAFT test to populate this tab."
                : rows.size() + " test run(s) recorded.");
        onSelectionChanged();
    }

    private void clearRows() {
        testIndex.clear();
        refresh();
    }

    private void onSelectionChanged() {
        boolean failed = isFailRow(rowList.getSelectedValue());
        diagnoseButton.setEnabled(failed);
        healButton.setEnabled(failed);
    }

    private void diagnoseSelected() {
        ShaftTestIndex.TestRowState selected = rowList.getSelectedValue();
        if (!isFailRow(selected)) {
            return;
        }
        // No resolved Allure directory is known from this tab's row-level data (unlike
        // FailedRunDoctorNotifier, which resolves it from the just-failed run's project root): pass
        // null so the server auto-discovers the newest evidence instead of guessing a path.
        ShaftToolWorkflowLauncher.open(project, "doctor_analyze_failed_allure",
                FailedRunDoctorNotifier.doctorArguments(null));
    }

    private void healSelected() {
        ShaftTestIndex.TestRowState selected = rowList.getSelectedValue();
        if (!isFailRow(selected)) {
            return;
        }
        ShaftToolWorkflowLauncher.open(project, "healer_run_failed_test",
                FailedRunDoctorNotifier.healerArguments(selected.testId()));
    }

    // ------------------------------------------------------------------
    // Pure row logic (unit tested)
    // ------------------------------------------------------------------

    /**
     * Returns whether a row is a failed run whose Doctor/Heal actions should be enabled.
     *
     * @param row selected row, or {@code null} when nothing is selected
     * @return {@code true} only for a non-null {@link ShaftTestIndex.Status#FAIL} row
     */
    static boolean isFailRow(ShaftTestIndex.TestRowState row) {
        return row != null && row.status() == ShaftTestIndex.Status.FAIL;
    }

    /**
     * Formats a row for display: status, test id, and last-run time.
     *
     * @param row row to format
     * @return display label, for example {@code "FAIL  CheckoutTest  14:32:10"}
     */
    static String formatRowLabel(ShaftTestIndex.TestRowState row) {
        String statusText = row.status() == ShaftTestIndex.Status.PASS ? "PASS" : "FAIL";
        return statusText + "  " + row.testId() + "  " + formatTimestamp(row.lastRunAtMillis());
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

    JBList<ShaftTestIndex.TestRowState> rowListForTest() {
        return rowList;
    }

    JComponent diagnoseButtonForTest() {
        return diagnoseButton;
    }

    JComponent healButtonForTest() {
        return healButton;
    }

    JLabel statusLabelForTest() {
        return statusLabel;
    }
}
