package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.jetbrains.annotations.Nullable;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.DefaultTableModel;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.List;

/**
 * Reporting canvas: heal insights with persist-on-pass review gate (issue #5972 / S3-06).
 *
 * <p>Clients MCP {@code report_heal} and CLI {@code shaft report heal}. Counts by
 * {@code HealingDecision} status. Only RECOVERED + passing replay offers a reviewable patch via
 * {@code doctor_propose_healed_locator}. AMBIGUOUS has no apply-to-source primary action.
 * NO_CANDIDATES is shown. Never auto-writes locators. Reuse {@code healer_run_failed_test} for
 * guarded replay. Policy: locator-healing.md / #5454.
 */
final class ReportingHealPanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT reporting heal insights";
    static final String TAB_TITLE = "Heal";

    private static final String[] COLUMNS = {
            "status", "attemptId", "originalLocator", "proposedLocator", "outcome",
            "confidence", "primaryAction"
    };

    private final @Nullable Project project;
    private final JBTextField reportsPath;
    private final JBTextField proposalsPath;
    private final JBTextField sourcePath;
    private final JBLabel status;
    private final JBLabel countsLabel;
    private final JBTextArea reviewDiff;
    private final DefaultTableModel tableModel;
    private final JTable table;
    private final JButton refresh;
    private final JButton reviewPatch;
    private final List<JsonObject> insightRows = new ArrayList<>();
    private boolean busy;

    ReportingHealPanel() {
        this(null);
    }

    ReportingHealPanel(@Nullable Project project) {
        super(new BorderLayout(0, JBUI.scale(8)));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Heal insights by HealingDecision with persist-on-pass review gate");

        JBLabel title = new JBLabel("Heal insights");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>Counts by <b>HealingDecision</b> status from "
                        + "<code>target/shaft-heal/reports</code>. "
                        + "Propose locator patches only after a <b>passing replay</b> "
                        + "(reuse <code>healer_run_failed_test</code>). "
                        + "<b>Never auto-land</b> — RECOVERED + pass offers a reviewable diff; "
                        + "AMBIGUOUS has no apply-to-source action; NO_CANDIDATES is shown. "
                        + "Policy: locator-healing.md / #5454.</html>");
        hint.setAllowAutoWrapping(true);

        reportsPath = newField("Heal reports path", "target/shaft-heal/reports");
        proposalsPath = newField("Proposal manifests path", "target/shaft-doctor/healing-proposals");
        sourcePath = newField("Source path for review patch", "");

        JPanel paths = new JPanel(new GridLayout(0, 1, 0, JBUI.scale(4)));
        paths.setOpaque(false);
        paths.add(labeled("Reports", reportsPath));
        paths.add(labeled("Proposals", proposalsPath));
        paths.add(labeled("Source", sourcePath));

        refresh = new JButton("Refresh heal insights");
        refresh.getAccessibleContext().setAccessibleName("Refresh heal insights");
        refresh.addActionListener(event -> loadInsights());
        reviewPatch = new JButton("Review patch");
        reviewPatch.getAccessibleContext().setAccessibleName("Review heal locator patch");
        reviewPatch.addActionListener(event -> proposeSelected());
        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, JBUI.scale(6), 0));
        actions.setOpaque(false);
        actions.add(refresh);
        actions.add(reviewPatch);

        countsLabel = new JBLabel("Statuses: —");
        countsLabel.getAccessibleContext().setAccessibleName("Heal status counts");
        status = new JBLabel("No heal insights loaded yet.");
        status.getAccessibleContext().setAccessibleName("Heal insights status");

        tableModel = new DefaultTableModel(COLUMNS, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };
        table = new JTable(tableModel);
        table.getAccessibleContext().setAccessibleName("Heal insights table");
        table.setAutoCreateRowSorter(true);
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        table.getSelectionModel().addListSelectionListener(event -> updateReviewEnabled());

        reviewDiff = new JBTextArea(4, 40);
        reviewDiff.setEditable(false);
        reviewDiff.setLineWrap(true);
        reviewDiff.setWrapStyleWord(true);
        reviewDiff.getAccessibleContext().setAccessibleName("Heal review diff");
        reviewDiff.setText("Select a RECOVERED + pass row and click Review patch. "
                + "AMBIGUOUS rows never offer apply-to-source.");

        JPanel north = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        north.setOpaque(false);
        north.add(title, BorderLayout.NORTH);
        north.add(hint, BorderLayout.CENTER);
        JPanel controls = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        controls.setOpaque(false);
        controls.add(paths, BorderLayout.CENTER);
        controls.add(actions, BorderLayout.SOUTH);
        north.add(controls, BorderLayout.SOUTH);

        JPanel center = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        center.setOpaque(false);
        center.add(countsLabel, BorderLayout.NORTH);
        center.add(new JBScrollPane(table), BorderLayout.CENTER);
        center.add(new JBScrollPane(reviewDiff), BorderLayout.SOUTH);

        add(north, BorderLayout.NORTH);
        add(center, BorderLayout.CENTER);
        add(status, BorderLayout.SOUTH);
        updateEnabled();
    }

    JButton refreshButton() {
        return refresh;
    }

    JButton reviewPatchButton() {
        return reviewPatch;
    }

    JTable insightsTable() {
        return table;
    }

    JBLabel statusLabel() {
        return status;
    }

    JBLabel countsLabel() {
        return countsLabel;
    }

    JBTextArea reviewDiffArea() {
        return reviewDiff;
    }

    void applyInsightsJson(String raw) {
        tableModel.setRowCount(0);
        insightRows.clear();
        if (raw == null || raw.isBlank()) {
            countsLabel.setText("Statuses: —");
            status.setText("No heal insights yet.");
            return;
        }
        try {
            JsonObject root = parseObject(raw);
            if (root == null) {
                status.setText(truncate(raw));
                return;
            }
            countsLabel.setText(formatCounts(root));
            if (isEmptyView(root)) {
                String message = text(root, "emptyMessage");
                status.setText(message.isBlank() ? "No heal insights yet." : message);
                return;
            }
            int rows = appendInsightRows(root);
            status.setText(rows == 0
                    ? "No heal insights."
                    : ("Showing " + rows + " heal report(s); "
                    + intValue(root, "totalReports") + " total. "
                    + "Review patch only for RECOVERED + pass."));
        } catch (RuntimeException exception) {
            status.setText(truncate(raw));
        }
        updateReviewEnabled();
    }

    void applyProposalJson(String raw) {
        if (raw == null || raw.isBlank()) {
            reviewDiff.setText("No proposal returned.");
            return;
        }
        try {
            JsonObject root = parseObject(raw);
            if (root == null) {
                reviewDiff.setText(truncate(raw));
                return;
            }
            String original = text(root, "originalExpression");
            String proposed = text(root, "proposedExpression");
            String manifest = text(root, "manifestPath");
            StringBuilder builder = new StringBuilder();
            builder.append("Reviewable diff (not applied to source):\n");
            builder.append("- ").append(original.isBlank() ? "?" : original).append('\n');
            builder.append("+ ").append(proposed.isBlank() ? "?" : proposed).append('\n');
            if (!manifest.isBlank()) {
                builder.append("Manifest: ").append(manifest);
            }
            reviewDiff.setText(builder.toString());
            status.setText("Proposal persisted for review only — source unchanged (FR-003).");
        } catch (RuntimeException exception) {
            reviewDiff.setText(truncate(raw));
        }
    }

    private int appendInsightRows(JsonObject root) {
        int rows = 0;
        for (JsonElement element : array(root, "insights")) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject insight = element.getAsJsonObject();
            insightRows.add(insight);
            tableModel.addRow(toTableRow(insight));
            rows++;
        }
        return rows;
    }

    private static Object[] toTableRow(JsonObject insight) {
        String primary = text(insight, "primaryActionLabel");
        if (primary.isBlank()) {
            primary = text(insight, "primaryAction");
            if ("NONE".equals(primary) || primary.isBlank()) {
                primary = "—";
            }
        }
        return new Object[]{
                text(insight, "status"),
                text(insight, "attemptId"),
                text(insight, "originalLocator"),
                text(insight, "proposedLocator"),
                text(insight, "actionOutcome"),
                String.format(java.util.Locale.ROOT, "%.2f", doubleValue(insight, "confidence")),
                primary
        };
    }

    private static String formatCounts(JsonObject root) {
        JsonArray counts = array(root, "statusCounts");
        if (counts.isEmpty()) {
            return "Statuses: —";
        }
        StringBuilder builder = new StringBuilder("Statuses:");
        for (JsonElement element : counts) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject count = element.getAsJsonObject();
            int value = intValue(count, "count");
            if (value <= 0) {
                continue;
            }
            builder.append(' ').append(text(count, "status")).append('=').append(value);
        }
        return builder.length() <= "Statuses:".length() ? "Statuses: (all zero)" : builder.toString();
    }

    private void loadInsights() {
        if (project == null) {
            applyInsightsJson(
                    "{\"empty\":true,\"emptyMessage\":\"MCP is not available in this context.\"}");
            return;
        }
        busy = true;
        updateEnabled();
        JsonObject arguments = new JsonObject();
        arguments.addProperty("reportsPath", reportsPath.getText().trim());
        arguments.addProperty("proposalsPath", proposalsPath.getText().trim());
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("report_heal", arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    busy = false;
                    updateEnabled();
                    applyToolResult(result, error);
                }));
    }

    private void proposeSelected() {
        int viewRow = table.getSelectedRow();
        if (viewRow < 0) {
            reviewDiff.setText("Select a RECOVERED + pass insight first.");
            return;
        }
        int modelRow = table.convertRowIndexToModel(viewRow);
        if (modelRow < 0 || modelRow >= insightRows.size()) {
            reviewDiff.setText("Selected insight is out of range.");
            return;
        }
        JsonObject insight = insightRows.get(modelRow);
        if (!booleanFlag(insight, "canProposeSourcePatch")
                || !"REVIEW_DIFF".equals(text(insight, "primaryAction"))) {
            reviewDiff.setText("This insight cannot apply a source patch. "
                    + "AMBIGUOUS / failed replay / NO_CANDIDATES never auto-land (SC-001 / FR-002).");
            return;
        }
        String source = sourcePath.getText().trim();
        if (source.isBlank()) {
            reviewDiff.setText("Set a repository-relative Java source path to build the reviewable diff.");
            return;
        }
        if (project == null) {
            reviewDiff.setText("MCP is not available in this context.");
            return;
        }
        busy = true;
        updateEnabled();
        JsonObject arguments = new JsonObject();
        arguments.addProperty("repositoryRoot", ".");
        arguments.addProperty("healingReportPath", text(insight, "reportPath"));
        arguments.addProperty("sourcePath", source);
        arguments.addProperty("sourcePatchConsent", true);
        arguments.addProperty("outputDirectory", proposalsPath.getText().trim());
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("doctor_propose_healed_locator", arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    busy = false;
                    updateEnabled();
                    applyProposalResult(result, error);
                }));
    }

    private void applyToolResult(@Nullable ShaftMcpToolResult result, @Nullable Throwable error) {
        if (error != null) {
            applyInsightsJson(error.getMessage() == null ? error.toString() : error.getMessage());
            return;
        }
        if (result == null || !result.success()) {
            String output = result == null || result.output() == null ? "" : result.output();
            applyInsightsJson(output.isBlank()
                    ? "{\"empty\":true,\"emptyMessage\":\"report_heal failed.\"}"
                    : output);
            return;
        }
        applyInsightsJson(result.output() == null ? "" : result.output());
    }

    private void applyProposalResult(@Nullable ShaftMcpToolResult result, @Nullable Throwable error) {
        if (error != null) {
            reviewDiff.setText(error.getMessage() == null ? error.toString() : error.getMessage());
            return;
        }
        if (result == null || !result.success()) {
            String output = result == null || result.output() == null ? "" : result.output();
            reviewDiff.setText(output.isBlank() ? "doctor_propose_healed_locator failed." : truncate(output));
            return;
        }
        applyProposalJson(result.output() == null ? "" : result.output());
    }

    private void updateEnabled() {
        refresh.setEnabled(project != null && !busy);
        updateReviewEnabled();
    }

    private void updateReviewEnabled() {
        boolean eligible = false;
        int viewRow = table.getSelectedRow();
        if (!busy && project != null && viewRow >= 0) {
            int modelRow = table.convertRowIndexToModel(viewRow);
            if (modelRow >= 0 && modelRow < insightRows.size()) {
                JsonObject insight = insightRows.get(modelRow);
                eligible = booleanFlag(insight, "canProposeSourcePatch")
                        && "REVIEW_DIFF".equals(text(insight, "primaryAction"));
            }
        }
        reviewPatch.setEnabled(eligible);
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

    private static JsonObject parseObject(String raw) {
        JsonElement parsed = JsonParser.parseString(raw);
        return parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
    }

    private static boolean isEmptyView(JsonObject root) {
        return root.has("empty") && root.get("empty").getAsBoolean();
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

    private static double doubleValue(JsonObject object, String field) {
        return object.has(field) && object.get(field).isJsonPrimitive()
                ? object.get(field).getAsDouble()
                : 0.0;
    }

    private static boolean booleanFlag(JsonObject object, String field) {
        return object.has(field) && object.get(field).isJsonPrimitive()
                && object.get(field).getAsBoolean();
    }

    private static String truncate(String raw) {
        return raw.length() > 240 ? raw.substring(0, 240) + "…" : raw;
    }
}
