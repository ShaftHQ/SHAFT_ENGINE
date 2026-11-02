package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.jetbrains.annotations.Nullable;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;

/**
 * Reporting canvas: cross-run Allure history (issue #5967 / S3-01).
 *
 * <p>Clients MCP {@code report_history} and CLI {@code shaft report history}. Never replaces the
 * allure-results root; missing history.jsonl shows an empty state. HISTORY rows are cross-launch;
 * RETRY rows are intra-launch.
 */
final class ReportingHistoryPanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT reporting history";
    static final String TAB_TITLE = "History";

    private static final String[] COLUMNS = {
            "historyId", "name", "kind", "launch", "status", "timestamp", "doctor"
    };

    private final @Nullable Project project;
    private final JBTextField historyPath;
    private final JBTextField doctorPath;
    private final JBTextField resultsPath;
    private final JBLabel status;
    private final DefaultTableModel tableModel;
    private final JTable table;
    private final JButton refresh;
    private boolean busy;

    ReportingHistoryPanel() {
        this(null);
    }

    ReportingHistoryPanel(@Nullable Project project) {
        super(new BorderLayout(0, JBUI.scale(8)));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Cross-run Allure history and Doctor joins; retries stay separate from history");

        JBLabel title = new JBLabel("Cross-run Allure history");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>Ingests <code>target/history.jsonl</code> (append-only when "
                        + "<code>allure.accumulateHistory=true</code>) plus optional Doctor JSON. "
                        + "Never replaces the allure-results root. HISTORY = cross-launch; "
                        + "RETRY = intra-launch.</html>");
        hint.setAllowAutoWrapping(true);

        historyPath = newField("History.jsonl path", "target/history.jsonl");
        doctorPath = newField("Doctor JSON path", "");
        resultsPath = newField("allure-results path", "target/allure-results");

        JPanel paths = new JPanel(new GridLayout(0, 1, 0, JBUI.scale(4)));
        paths.setOpaque(false);
        paths.add(labeled("History", historyPath));
        paths.add(labeled("Doctor", doctorPath));
        paths.add(labeled("Results", resultsPath));

        refresh = new JButton("Refresh history");
        refresh.getAccessibleContext().setAccessibleName("Refresh history");
        refresh.addActionListener(event -> loadHistory());
        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, JBUI.scale(6), 0));
        actions.setOpaque(false);
        actions.add(refresh);

        status = new JBLabel("No Allure history loaded yet.");
        status.getAccessibleContext().setAccessibleName("History status");

        tableModel = new DefaultTableModel(COLUMNS, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };
        table = new JTable(tableModel);
        table.getAccessibleContext().setAccessibleName("Allure history table");
        table.setAutoCreateRowSorter(true);

        JPanel north = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        north.setOpaque(false);
        north.add(title, BorderLayout.NORTH);
        north.add(hint, BorderLayout.CENTER);
        JPanel controls = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        controls.setOpaque(false);
        controls.add(paths, BorderLayout.CENTER);
        controls.add(actions, BorderLayout.SOUTH);
        north.add(controls, BorderLayout.SOUTH);

        add(north, BorderLayout.NORTH);
        add(new JBScrollPane(table), BorderLayout.CENTER);
        add(status, BorderLayout.SOUTH);
        updateEnabled();
    }

    JButton refreshButton() {
        return refresh;
    }

    JTable historyTable() {
        return table;
    }

    JBLabel statusLabel() {
        return status;
    }

    void applyHistoryJson(String raw) {
        tableModel.setRowCount(0);
        if (raw == null || raw.isBlank()) {
            status.setText("No Allure history yet.");
            return;
        }
        try {
            JsonElement parsed = JsonParser.parseString(raw);
            if (!parsed.isJsonObject()) {
                status.setText(truncate(raw));
                return;
            }
            JsonObject root = parsed.getAsJsonObject();
            if (root.has("empty") && root.get("empty").getAsBoolean()) {
                String message = text(root, "emptyMessage");
                status.setText(message.isBlank() ? "No Allure history yet." : message);
                return;
            }
            int rows = 0;
            for (JsonElement element : array(root, "tests")) {
                if (!element.isJsonObject()) {
                    continue;
                }
                JsonObject series = element.getAsJsonObject();
                String historyId = text(series, "historyId");
                String name = text(series, "name");
                String doctor = firstNonBlank(text(series, "doctorCause"), text(series, "doctorSummary"));
                for (JsonElement launchElement : array(series, "launches")) {
                    if (!launchElement.isJsonObject()) {
                        continue;
                    }
                    JsonObject launch = launchElement.getAsJsonObject();
                    tableModel.addRow(new Object[]{
                            historyId,
                            name,
                            firstNonBlank(text(launch, "kind"), "HISTORY"),
                            firstNonBlank(text(launch, "launchName"), text(launch, "launchUuid")),
                            text(launch, "status"),
                            String.valueOf(longValue(launch, "timestamp")),
                            doctor
                    });
                    rows++;
                }
            }
            for (JsonElement element : array(root, "retries")) {
                if (!element.isJsonObject()) {
                    continue;
                }
                JsonObject group = element.getAsJsonObject();
                String historyId = text(group, "historyId");
                String name = text(group, "name");
                for (JsonElement attemptElement : array(group, "attempts")) {
                    if (!attemptElement.isJsonObject()) {
                        continue;
                    }
                    JsonObject attempt = attemptElement.getAsJsonObject();
                    tableModel.addRow(new Object[]{
                            historyId,
                            name,
                            "RETRY",
                            text(attempt, "resultUuid"),
                            text(attempt, "status"),
                            String.valueOf(longValue(attempt, "start")),
                            ""
                    });
                    rows++;
                }
            }
            status.setText(rows == 0
                    ? "No Allure history rows."
                    : ("Showing " + rows + " history/retry rows (allure-results root never replaced)."));
        } catch (RuntimeException exception) {
            status.setText(truncate(raw));
        }
    }

    private void loadHistory() {
        if (project == null) {
            applyHistoryJson(
                    "{\"empty\":true,\"emptyMessage\":\"MCP is not available in this context.\"}");
            return;
        }
        busy = true;
        updateEnabled();
        JsonObject arguments = new JsonObject();
        arguments.addProperty("historyPath", historyPath.getText().trim());
        arguments.addProperty("doctorReportPath", doctorPath.getText().trim());
        arguments.addProperty("allureResultsPath", resultsPath.getText().trim());
        arguments.addProperty("limitPerHistoryId", 10);
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("report_history", arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    busy = false;
                    updateEnabled();
                    applyToolResult(result, error);
                }));
    }

    private void applyToolResult(@Nullable ShaftMcpToolResult result, @Nullable Throwable error) {
        if (error != null) {
            applyHistoryJson(error.getMessage() == null ? error.toString() : error.getMessage());
            return;
        }
        if (result == null || !result.success()) {
            String output = result == null || result.output() == null ? "" : result.output();
            applyHistoryJson(output.isBlank()
                    ? "{\"empty\":true,\"emptyMessage\":\"report_history failed.\"}"
                    : output);
            return;
        }
        applyHistoryJson(result.output() == null ? "" : result.output());
    }

    private void updateEnabled() {
        refresh.setEnabled(project != null && !busy);
    }

    private static JBTextField newField(String accessibleName, String initial) {
        JBTextField field = new JBTextField(initial);
        field.getAccessibleContext().setAccessibleName(accessibleName);
        return field;
    }

    private static JPanel labeled(String label, JBTextField field) {
        JPanel row = new JPanel(new BorderLayout(JBUI.scale(6), 0));
        row.setOpaque(false);
        JBLabel caption = new JBLabel(label);
        caption.setPreferredSize(new Dimension(JBUI.scale(72), caption.getPreferredSize().height));
        row.add(caption, BorderLayout.WEST);
        row.add(field, BorderLayout.CENTER);
        return row;
    }

    private static JsonArray array(JsonObject object, String field) {
        return object.has(field) && object.get(field).isJsonArray()
                ? object.getAsJsonArray(field)
                : new JsonArray();
    }

    private static String text(JsonObject object, String field) {
        return object.has(field) && !object.get(field).isJsonNull()
                ? object.get(field).getAsString()
                : "";
    }

    private static long longValue(JsonObject object, String field) {
        return object.has(field) && object.get(field).isJsonPrimitive()
                ? object.get(field).getAsLong()
                : 0L;
    }

    private static String firstNonBlank(String left, String right) {
        if (left != null && !left.isBlank()) {
            return left;
        }
        return right == null ? "" : right;
    }

    private static String truncate(String raw) {
        return raw.length() > 240 ? raw.substring(0, 240) + "…" : raw;
    }
}
