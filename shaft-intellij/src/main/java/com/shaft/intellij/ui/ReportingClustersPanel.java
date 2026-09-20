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
 * Reporting canvas: unique-error clusters from Doctor signatures (issue #5969 / S3-03).
 *
 * <p>Clients MCP {@code report_clusters} and CLI {@code shaft report clusters}. UI shows
 * error/signature → impacted tests. Deterministic historical-signature keys; no cloud ML.
 */
final class ReportingClustersPanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT reporting unique-error clusters";
    static final String TAB_TITLE = "Clusters";

    private static final String[] COLUMNS = {
            "signature", "error", "impactedCount", "impactedTests"
    };

    private final @Nullable Project project;
    private final JBTextField resultsPath;
    private final JBTextField doctorPath;
    private final JBLabel status;
    private final DefaultTableModel tableModel;
    private final JTable table;
    private final JButton refresh;
    private boolean busy;

    ReportingClustersPanel() {
        this(null);
    }

    ReportingClustersPanel(@Nullable Project project) {
        super(new BorderLayout(0, JBUI.scale(8)));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Unique-error clusters: Doctor signature to impacted tests");

        JBLabel title = new JBLabel("Unique-error clusters");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>Group failures by Doctor <b>historical-signature</b> keys "
                        + "(<code>clusterFingerprint</code> preferred, else normalized signature). "
                        + "Each row is one error → impacted tests. No cloud ML. "
                        + "Shared signatures merge; distinct signatures stay separate.</html>");
        hint.setAllowAutoWrapping(true);

        resultsPath = newField("allure-results path", "target/allure-results");
        doctorPath = newField("Doctor JSON path", "");

        JPanel paths = new JPanel(new GridLayout(0, 1, 0, JBUI.scale(4)));
        paths.setOpaque(false);
        paths.add(labeled("Results", resultsPath));
        paths.add(labeled("Doctor", doctorPath));

        refresh = new JButton("Refresh clusters");
        refresh.getAccessibleContext().setAccessibleName("Refresh unique-error clusters");
        refresh.addActionListener(event -> loadClusters());
        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, JBUI.scale(6), 0));
        actions.setOpaque(false);
        actions.add(refresh);

        status = new JBLabel("No clusters loaded yet.");
        status.getAccessibleContext().setAccessibleName("Clusters status");

        tableModel = new DefaultTableModel(COLUMNS, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };
        table = new JTable(tableModel);
        table.getAccessibleContext().setAccessibleName("Unique-error cluster table");
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

    JTable clustersTable() {
        return table;
    }

    JBLabel statusLabel() {
        return status;
    }

    void applyClustersJson(String raw) {
        tableModel.setRowCount(0);
        if (raw == null || raw.isBlank()) {
            status.setText("No unique-error clusters yet.");
            return;
        }
        try {
            JsonObject root = parseObject(raw);
            if (root == null) {
                status.setText(truncate(raw));
                return;
            }
            if (isEmptyView(root)) {
                String message = text(root, "emptyMessage");
                status.setText(message.isBlank() ? "No unique-error clusters yet." : message);
                return;
            }
            int rows = appendClusterRows(root);
            status.setText(rows == 0
                    ? "No unique-error clusters."
                    : ("Showing " + rows + " signature cluster(s); "
                    + intValue(root, "impactedTestCount") + " impacted test(s)."));
        } catch (RuntimeException exception) {
            status.setText(truncate(raw));
        }
    }

    private static JsonObject parseObject(String raw) {
        JsonElement parsed = JsonParser.parseString(raw);
        return parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
    }

    private static boolean isEmptyView(JsonObject root) {
        return root.has("empty") && root.get("empty").getAsBoolean();
    }

    private int appendClusterRows(JsonObject root) {
        int rows = 0;
        for (JsonElement element : array(root, "clusters")) {
            if (!element.isJsonObject()) {
                continue;
            }
            tableModel.addRow(toTableRow(element.getAsJsonObject()));
            rows++;
        }
        return rows;
    }

    private static Object[] toTableRow(JsonObject cluster) {
        return new Object[]{
                text(cluster, "signatureKey"),
                text(cluster, "displayError"),
                String.valueOf(intValue(cluster, "impactedCount")),
                joinImpactedNames(cluster)
        };
    }

    private static String joinImpactedNames(JsonObject cluster) {
        JsonArray tests = array(cluster, "impactedTests");
        if (tests.isEmpty()) {
            return "";
        }
        StringBuilder builder = new StringBuilder();
        for (JsonElement element : tests) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject test = element.getAsJsonObject();
            String name = firstNonBlank(text(test, "name"), text(test, "historyId"), text(test, "uuid"));
            if (name.isBlank()) {
                continue;
            }
            if (!builder.isEmpty()) {
                builder.append(", ");
            }
            builder.append(name);
        }
        return builder.toString();
    }

    private void loadClusters() {
        if (project == null) {
            applyClustersJson(
                    "{\"empty\":true,\"emptyMessage\":\"MCP is not available in this context.\"}");
            return;
        }
        busy = true;
        updateEnabled();
        JsonObject arguments = new JsonObject();
        arguments.addProperty("allureResultsPath", resultsPath.getText().trim());
        arguments.addProperty("doctorReportPath", doctorPath.getText().trim());
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("report_clusters", arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    busy = false;
                    updateEnabled();
                    applyToolResult(result, error);
                }));
    }

    private void applyToolResult(@Nullable ShaftMcpToolResult result, @Nullable Throwable error) {
        if (error != null) {
            applyClustersJson(error.getMessage() == null ? error.toString() : error.getMessage());
            return;
        }
        if (result == null || !result.success()) {
            String output = result == null || result.output() == null ? "" : result.output();
            applyClustersJson(output.isBlank()
                    ? "{\"empty\":true,\"emptyMessage\":\"report_clusters failed.\"}"
                    : output);
            return;
        }
        applyClustersJson(result.output() == null ? "" : result.output());
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

    private static String firstNonBlank(String... values) {
        if (values == null) {
            return "";
        }
        for (String value : values) {
            if (value != null && !value.isBlank()) {
                return value;
            }
        }
        return "";
    }

    private static String truncate(String raw) {
        return raw.length() > 240 ? raw.substring(0, 240) + "…" : raw;
    }
}
