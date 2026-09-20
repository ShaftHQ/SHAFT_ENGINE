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
 * Reporting canvas: smart tags New / Always-failing / Flaky / Regressed / Fixed (issue #5975 / S3-09).
 *
 * <p>Clients MCP {@code report_smart_tags} and CLI {@code shaft report tags}. First-seen failure is
 * New, not Regressed. Insufficient history never invents Flaky. Duration-anomaly is optional.
 */
final class ReportingSmartTagsPanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT reporting smart tags";
    static final String TAB_TITLE = "Smart tags";

    private static final String[] COLUMNS = {
            "historyId", "name", "primary", "tags", "launches", "transitions", "newest", "previous", "durationAnomaly"
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

    ReportingSmartTagsPanel() {
        this(null);
    }

    ReportingSmartTagsPanel(@Nullable Project project) {
        super(new BorderLayout(0, JBUI.scale(8)));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Smart tags from Allure history: New, Always-failing, Flaky, Regressed, Fixed");

        JBLabel title = new JBLabel("Smart tags");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>Tags from cross-run history: <b>New</b>, <b>Always-failing</b>, <b>Flaky</b>, "
                        + "<b>Regressed</b>, <b>Fixed</b>. First-seen failure is New (not Regressed). "
                        + "Insufficient history never invents Flaky. Duration-anomaly is optional when "
                        + "timings exist.</html>");
        hint.setAllowAutoWrapping(true);

        historyPath = newField("History.jsonl path", "target/history.jsonl");
        doctorPath = newField("Doctor JSON path", "");
        resultsPath = newField("allure-results path", "target/allure-results");

        JPanel paths = new JPanel(new GridLayout(0, 1, 0, JBUI.scale(4)));
        paths.setOpaque(false);
        paths.add(labeled("History", historyPath));
        paths.add(labeled("Doctor", doctorPath));
        paths.add(labeled("Results", resultsPath));

        refresh = new JButton("Refresh smart tags");
        refresh.getAccessibleContext().setAccessibleName("Refresh smart tags");
        refresh.addActionListener(event -> loadTags());
        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, JBUI.scale(6), 0));
        actions.setOpaque(false);
        actions.add(refresh);

        status = new JBLabel("No smart tags loaded yet.");
        status.getAccessibleContext().setAccessibleName("Smart tags status");

        tableModel = new DefaultTableModel(COLUMNS, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };
        table = new JTable(tableModel);
        table.getAccessibleContext().setAccessibleName("Smart tags table");
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

    JTable tagsTable() {
        return table;
    }

    JBLabel statusLabel() {
        return status;
    }

    void applyTagsJson(String raw) {
        tableModel.setRowCount(0);
        if (raw == null || raw.isBlank()) {
            status.setText("No smart tags yet.");
            return;
        }
        try {
            JsonObject root = parseObject(raw);
            applyParsedTags(root, raw);
        } catch (RuntimeException exception) {
            status.setText(truncate(raw));
        }
    }

    private static JsonObject parseObject(String raw) {
        JsonElement parsed = JsonParser.parseString(raw);
        return parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
    }

    private void applyParsedTags(@Nullable JsonObject root, String raw) {
        if (root == null) {
            status.setText(truncate(raw));
            return;
        }
        if (root.has("empty") && root.get("empty").getAsBoolean()) {
            applyEmptyState(root);
            return;
        }
        int rows = appendRows(root);
        status.setText(rows == 0
                ? "No smart tag rows."
                : ("Showing " + rows + " smart-tag rows (insufficient history never invents Flaky)."));
    }

    private void applyEmptyState(JsonObject root) {
        String message = text(root, "emptyMessage");
        status.setText(message.isBlank() ? "No smart tags yet." : message);
    }

    private int appendRows(JsonObject root) {
        int rows = 0;
        for (JsonElement element : array(root, "rows")) {
            if (element.isJsonObject()) {
                tableModel.addRow(toTableRow(element.getAsJsonObject()));
                rows++;
            }
        }
        return rows;
    }

    private static Object[] toTableRow(JsonObject row) {
        return new Object[]{
                text(row, "historyId"),
                text(row, "name"),
                text(row, "primaryTag"),
                joinTags(row),
                String.valueOf(intValue(row, "launchCount")),
                transitionCountCell(row),
                text(row, "newestStatus"),
                text(row, "previousStatus"),
                durationAnomalyCell(row)
        };
    }

    private static String durationAnomalyCell(JsonObject row) {
        return row.has("durationAnomaly") && row.get("durationAnomaly").getAsBoolean()
                ? "yes"
                : "";
    }

    private void loadTags() {
        if (project == null) {
            applyTagsJson(
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
        arguments.addProperty("flakyTransitionThreshold", 3);
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("report_smart_tags", arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    busy = false;
                    updateEnabled();
                    applyToolResult(result, error);
                }));
    }

    private void applyToolResult(@Nullable ShaftMcpToolResult result, @Nullable Throwable error) {
        if (error != null) {
            applyTagsJson(error.getMessage() == null ? error.toString() : error.getMessage());
            return;
        }
        if (result == null || !result.success()) {
            String output = result == null || result.output() == null ? "" : result.output();
            applyTagsJson(output.isBlank()
                    ? "{\"empty\":true,\"emptyMessage\":\"report_smart_tags failed.\"}"
                    : output);
            return;
        }
        applyTagsJson(result.output() == null ? "" : result.output());
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

    private static int intValue(JsonObject object, String field) {
        return object.has(field) && object.get(field).isJsonPrimitive()
                ? object.get(field).getAsInt()
                : 0;
    }

    private static String transitionCountCell(JsonObject row) {
        if (row.has("transitionCount") && !row.get("transitionCount").isJsonNull()) {
            return String.valueOf(row.get("transitionCount").getAsInt());
        }
        return "—";
    }

    private static String joinTags(JsonObject row) {
        JsonArray tags = array(row, "tags");
        if (tags.isEmpty()) {
            return "";
        }
        StringBuilder builder = new StringBuilder();
        for (JsonElement element : tags) {
            if (!builder.isEmpty()) {
                builder.append(", ");
            }
            builder.append(element.getAsString());
        }
        return builder.toString();
    }

    private static String truncate(String raw) {
        return raw.length() > 240 ? raw.substring(0, 240) + "…" : raw;
    }
}
