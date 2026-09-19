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
 * Reporting canvas: dual flake definitions (issue #5968 / S3-02).
 *
 * <p>Clients MCP {@code report_flake} and CLI {@code shaft report flake}. Two columns/tags —
 * {@code retry-hidden} (intra-run) and {@code transitions} (cross-launch) — are never collapsed
 * into one score. Unknown history is explicit; CI commit metadata is optional.
 */
final class ReportingFlakePanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT reporting flake table";
    static final String TAB_TITLE = "Flake";

    private static final String[] COLUMNS = {
            "historyId", "name", "retry-hidden", "transitions", "transitionCount", "launches", "tags", "sameSha"
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

    ReportingFlakePanel() {
        this(null);
    }

    ReportingFlakePanel(@Nullable Project project) {
        super(new BorderLayout(0, JBUI.scale(8)));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Dual flake table: retry-hidden and transitions as separate columns");

        JBLabel title = new JBLabel("Dual flake definitions");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>Separate <b>retry-hidden</b> (intra-run fail then pass) from "
                        + "<b>transitions</b> (cross-launch flips, ≥3 in a 10-launch window). "
                        + "Never a single combined score. Insufficient history is "
                        + "<code>unknown</code>, not 0%. 100% failing is not flaky. "
                        + "Same-SHA is optional when CI metadata exists.</html>");
        hint.setAllowAutoWrapping(true);

        historyPath = newField("History.jsonl path", "target/history.jsonl");
        doctorPath = newField("Doctor JSON path", "");
        resultsPath = newField("allure-results path", "target/allure-results");

        JPanel paths = new JPanel(new GridLayout(0, 1, 0, JBUI.scale(4)));
        paths.setOpaque(false);
        paths.add(labeled("History", historyPath));
        paths.add(labeled("Doctor", doctorPath));
        paths.add(labeled("Results", resultsPath));

        refresh = new JButton("Refresh flake table");
        refresh.getAccessibleContext().setAccessibleName("Refresh flake table");
        refresh.addActionListener(event -> loadFlake());
        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, JBUI.scale(6), 0));
        actions.setOpaque(false);
        actions.add(refresh);

        status = new JBLabel("No flake table loaded yet.");
        status.getAccessibleContext().setAccessibleName("Flake status");

        tableModel = new DefaultTableModel(COLUMNS, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };
        table = new JTable(tableModel);
        table.getAccessibleContext().setAccessibleName("Dual flake table");
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

    JTable flakeTable() {
        return table;
    }

    JBLabel statusLabel() {
        return status;
    }

    void applyFlakeJson(String raw) {
        tableModel.setRowCount(0);
        if (raw == null || raw.isBlank()) {
            status.setText("No flake rows yet.");
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
                status.setText(message.isBlank() ? "No flake rows yet." : message);
                return;
            }
            int rows = 0;
            for (JsonElement element : array(root, "rows")) {
                if (!element.isJsonObject()) {
                    continue;
                }
                JsonObject row = element.getAsJsonObject();
                String retryHidden = row.has("retryHidden") && row.get("retryHidden").getAsBoolean()
                        ? firstNonBlank(text(row, "retryHiddenTag"), "retry-hidden")
                        : "";
                String transitions = text(row, "transitionAssessment");
                String transitionCount = row.has("transitionCount") && !row.get("transitionCount").isJsonNull()
                        ? String.valueOf(row.get("transitionCount").getAsInt())
                        : "—";
                String sameSha;
                if (row.has("sameShaAvailable") && row.get("sameShaAvailable").getAsBoolean()) {
                    sameSha = row.has("sameShaTransitionCount") && !row.get("sameShaTransitionCount").isJsonNull()
                            ? String.valueOf(row.get("sameShaTransitionCount").getAsInt())
                            : "available";
                } else {
                    sameSha = "n/a";
                }
                tableModel.addRow(new Object[]{
                        text(row, "historyId"),
                        text(row, "name"),
                        retryHidden,
                        transitions,
                        transitionCount,
                        String.valueOf(intValue(row, "launchCount")),
                        joinTags(row),
                        sameSha
                });
                rows++;
            }
            status.setText(rows == 0
                    ? "No flake rows."
                    : ("Showing " + rows
                    + " dual-definition rows (retry-hidden ⊥ transitions; no combined score)."));
        } catch (RuntimeException exception) {
            status.setText(truncate(raw));
        }
    }

    private void loadFlake() {
        if (project == null) {
            applyFlakeJson(
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
        arguments.addProperty("transitionThreshold", 3);
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("report_flake", arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    busy = false;
                    updateEnabled();
                    applyToolResult(result, error);
                }));
    }

    private void applyToolResult(@Nullable ShaftMcpToolResult result, @Nullable Throwable error) {
        if (error != null) {
            applyFlakeJson(error.getMessage() == null ? error.toString() : error.getMessage());
            return;
        }
        if (result == null || !result.success()) {
            String output = result == null || result.output() == null ? "" : result.output();
            applyFlakeJson(output.isBlank()
                    ? "{\"empty\":true,\"emptyMessage\":\"report_flake failed.\"}"
                    : output);
            return;
        }
        applyFlakeJson(result.output() == null ? "" : result.output());
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
