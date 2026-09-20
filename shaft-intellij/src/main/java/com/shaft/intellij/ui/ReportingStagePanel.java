package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.ide.BrowserUtil;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTabbedPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.datatransfer.StringSelection;
import java.util.List;
import java.util.stream.Stream;

/**
 * Unified Reporting canvas (issue #5976 / S3-10): one Overview composing Open Allure, Doctor,
 * flake, heal, and copyable engineer/stakeholder summaries. Secondary tabs keep History / Smart
 * tags / Clusters / Labels / Triage / Visual Baselines / Evidence without five primary stages.
 *
 * <p>CLI/MCP parity: {@code report_open}, {@code report_summary}. Does not rewrite Allure HTML.
 * User-cancelled runs stay suppressed by {@link com.shaft.intellij.notifications.FailedRunDoctorNotifier}.
 */
final class ReportingStagePanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT reporting";
    static final String OVERVIEW_TAB = "Overview";

    private final @Nullable Project project;
    private final JBTabbedPane surfaces;
    private final ReportingDoctorPanel doctor;
    private final ReportingFlakePanel flake;
    private final ReportingHealPanel heal;
    private final JBLabel allureStatus;
    private final JBTextArea engineerSummary;
    private final JBTextArea stakeholderSummary;
    private final JButton openAllure;
    private final JButton generateReport;
    private final JButton refreshSummaries;
    private final JButton copyEngineer;
    private final JButton copyStakeholder;
    private boolean busy;

    ReportingStagePanel(
            @Nullable Project project,
            @NotNull GuidedWorkflowPanel.ToolPrefill prefill,
            @NotNull ShaftSettingsState.Settings settings) {
        super(new BorderLayout(0, JBUI.scale(8)));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Unified Reporting: Open Allure, Doctor, flake, heal, and copyable summaries");

        JBLabel title = new JBLabel("Reporting & Analytics");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>One Reporting canvas: open Allure, review Doctor / flake / heal, and copy "
                        + "engineer or stakeholder summaries. Allure HTML stays the rich report — "
                        + "we do not rewrite it. Missing Allure offers <code>generate_test_report</code>. "
                        + "User-cancelled runs do not spam Doctor.</html>");
        hint.setAllowAutoWrapping(true);

        JPanel header = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        header.setOpaque(false);
        header.add(title, BorderLayout.NORTH);
        header.add(hint, BorderLayout.CENTER);

        doctor = new ReportingDoctorPanel(project);
        flake = new ReportingFlakePanel(project);
        heal = new ReportingHealPanel(project);
        ReportingHistoryPanel history = new ReportingHistoryPanel(project);
        ReportingSmartTagsPanel smartTags = new ReportingSmartTagsPanel(project);
        ReportingClustersPanel clusters = new ReportingClustersPanel(project);
        ReportingLabelsPanel labels = new ReportingLabelsPanel(project);
        EvidenceTriagePanel triage = new EvidenceTriagePanel(project, prefill);
        VisualBaselinesPanel visualBaselines = new VisualBaselinesPanel(project);
        ShaftFeaturePanel evidenceTools = new ShaftFeaturePanel(project, settings,
                List.of(new ToolCategory("Evidence",
                        Stream.concat(ToolTemplates.doctor().stream(), ToolTemplates.healer().stream()).toList())));

        engineerSummary = summaryArea("Engineer summary");
        stakeholderSummary = summaryArea("Stakeholder summary");
        engineerSummary.setText("Refresh summaries to load the engineer playbook text "
                + "(shaft-execution-reporting). Counts only — no secrets.");
        stakeholderSummary.setText("Refresh summaries to load the stakeholder playbook text "
                + "(shaft-stakeholder-reporting). Counts only — no secrets.");

        openAllure = new JButton("Open Allure");
        openAllure.getAccessibleContext().setAccessibleName("Open Allure report");
        openAllure.addActionListener(event -> runOpenAllure(false));
        generateReport = new JButton("Generate report");
        generateReport.getAccessibleContext().setAccessibleName("Generate test report");
        generateReport.addActionListener(event -> runGenerateReport());
        refreshSummaries = new JButton("Refresh summaries");
        refreshSummaries.getAccessibleContext().setAccessibleName("Refresh reporting summaries");
        refreshSummaries.addActionListener(event -> runSummaryRefresh());
        copyEngineer = new JButton("Copy engineer summary");
        copyEngineer.getAccessibleContext().setAccessibleName("Copy engineer summary");
        copyEngineer.addActionListener(event -> copyText(engineerSummary.getText(), "Engineer summary copied."));
        copyStakeholder = new JButton("Copy stakeholder summary");
        copyStakeholder.getAccessibleContext().setAccessibleName("Copy stakeholder summary");
        copyStakeholder.addActionListener(event ->
                copyText(stakeholderSummary.getText(), "Stakeholder summary copied."));

        allureStatus = new JBLabel("Allure: not checked yet. Use Open Allure or Generate report.");
        allureStatus.getAccessibleContext().setAccessibleName("Allure report status");

        JPanel allureActions = new JPanel(new FlowLayout(FlowLayout.LEFT, JBUI.scale(6), 0));
        allureActions.setOpaque(false);
        allureActions.add(openAllure);
        allureActions.add(generateReport);
        allureActions.add(refreshSummaries);
        allureActions.add(copyEngineer);
        allureActions.add(copyStakeholder);

        JPanel summaries = new JPanel(new GridLayout(1, 2, JBUI.scale(8), 0));
        summaries.setOpaque(false);
        summaries.add(wrapSummary("Engineer summary", engineerSummary));
        summaries.add(wrapSummary("Stakeholder summary", stakeholderSummary));

        JPanel overviewNorth = new JPanel(new BorderLayout(0, JBUI.scale(6)));
        overviewNorth.setOpaque(false);
        overviewNorth.add(allureActions, BorderLayout.NORTH);
        overviewNorth.add(allureStatus, BorderLayout.CENTER);

        JPanel composed = new JPanel(new BorderLayout(0, JBUI.scale(8)));
        composed.setOpaque(false);
        composed.add(overviewNorth, BorderLayout.NORTH);
        composed.add(summaries, BorderLayout.CENTER);

        JSplitPane mid = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, doctor, flake);
        mid.setResizeWeight(0.5);
        mid.setBorder(null);
        JSplitPane lower = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, mid, heal);
        lower.setResizeWeight(0.66);
        lower.setBorder(null);

        JPanel overviewBody = new JPanel(new BorderLayout(0, JBUI.scale(8)));
        overviewBody.setOpaque(false);
        overviewBody.add(composed, BorderLayout.NORTH);
        overviewBody.add(lower, BorderLayout.CENTER);

        JBScrollPane overviewScroll = new JBScrollPane(overviewBody);
        overviewScroll.setBorder(null);
        overviewScroll.getVerticalScrollBar().setUnitIncrement(16);

        surfaces = new JBTabbedPane();
        surfaces.getAccessibleContext().setAccessibleName("SHAFT reporting surfaces");
        surfaces.addTab(OVERVIEW_TAB, ShaftIcons.CHECK, overviewScroll);
        surfaces.addTab(ReportingHistoryPanel.TAB_TITLE, ShaftIcons.CHECK, history);
        surfaces.addTab(ReportingSmartTagsPanel.TAB_TITLE, ShaftIcons.VIEW, smartTags);
        surfaces.addTab(ReportingClustersPanel.TAB_TITLE, ShaftIcons.VIEW, clusters);
        surfaces.addTab(ReportingLabelsPanel.TAB_TITLE, ShaftIcons.EDIT, labels);
        surfaces.addTab("Triage", ShaftIcons.VIEW, triage);
        surfaces.addTab("Visual Baselines", ShaftIcons.VIEW, visualBaselines);
        surfaces.addTab("Evidence", ShaftIcons.EDIT, evidenceTools);

        add(header, BorderLayout.NORTH);
        add(surfaces, BorderLayout.CENTER);
        updateEnabled();
    }

    JBTabbedPane surfaces() {
        return surfaces;
    }

    ReportingDoctorPanel doctorPanel() {
        return doctor;
    }

    ReportingFlakePanel flakePanel() {
        return flake;
    }

    ReportingHealPanel healPanel() {
        return heal;
    }

    JButton openAllureButton() {
        return openAllure;
    }

    JButton generateReportButton() {
        return generateReport;
    }

    JBLabel allureStatusLabel() {
        return allureStatus;
    }

    JBTextArea engineerSummaryArea() {
        return engineerSummary;
    }

    JBTextArea stakeholderSummaryArea() {
        return stakeholderSummary;
    }

    void showOverview() {
        surfaces.setSelectedIndex(0);
    }

    private void runOpenAllure(boolean afterGenerate) {
        if (project == null) {
            allureStatus.setText(
                    "Allure: MCP is not available in this context. Call generate_test_report from CLI/MCP.");
            return;
        }
        busy = true;
        updateEnabled();
        JsonObject args = new JsonObject();
        args.addProperty("openInBrowser", true);
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("report_open", args)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    busy = false;
                    updateEnabled();
                    applyOpenResult(result, error, afterGenerate);
                }));
    }

    private void runGenerateReport() {
        if (project == null) {
            allureStatus.setText("Generate report: MCP is not available in this context.");
            return;
        }
        busy = true;
        updateEnabled();
        allureStatus.setText("Generating Allure via generate_test_report…");
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("generate_test_report", new JsonObject())
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    if (error != null || result == null || !result.success()) {
                        busy = false;
                        updateEnabled();
                        String message = error != null
                                ? error.getMessage()
                                : (result == null ? "generate_test_report failed." : result.output());
                        allureStatus.setText("Generate report failed: " + truncate(message));
                        return;
                    }
                    runOpenAllure(true);
                }));
    }

    private void runSummaryRefresh() {
        if (project == null) {
            engineerSummary.setText("MCP is not available in this context.");
            stakeholderSummary.setText("MCP is not available in this context.");
            return;
        }
        busy = true;
        updateEnabled();
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("report_summary", new JsonObject())
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    busy = false;
                    updateEnabled();
                    applySummaryResult(result, error);
                }));
    }

    private void applyOpenResult(
            @Nullable ShaftMcpToolResult result, @Nullable Throwable error, boolean afterGenerate) {
        if (error != null) {
            allureStatus.setText("Open Allure failed: " + truncate(error.getMessage()));
            return;
        }
        if (result == null || !result.success()) {
            String output = result == null || result.output() == null ? "" : result.output();
            allureStatus.setText(output.isBlank() ? "report_open failed." : truncate(output));
            return;
        }
        applyOpenPayload(result.output() == null ? "" : result.output(), afterGenerate);
    }

    private void applyOpenPayload(String raw, boolean afterGenerate) {
        try {
            JsonObject root = JsonParser.parseString(raw).getAsJsonObject();
            if (isEmptyPayload(root)) {
                showMissingAllure(root);
                return;
            }
            openReportPath(text(root, "reportPath"), afterGenerate);
        } catch (RuntimeException exception) {
            allureStatus.setText(truncate(raw));
        }
    }

    private static boolean isEmptyPayload(JsonObject root) {
        return root.has("empty") && root.get("empty").getAsBoolean();
    }

    private void showMissingAllure(JsonObject root) {
        String message = text(root, "emptyMessage");
        String cta = text(root, "ctaTool");
        if (message.isBlank()) {
            allureStatus.setText("Allure missing. Use Generate report (generate_test_report).");
            return;
        }
        allureStatus.setText(cta.isBlank() ? message : message + " CTA: " + cta);
    }

    private void openReportPath(String reportPath, boolean afterGenerate) {
        if (!reportPath.isBlank()) {
            try {
                BrowserUtil.browse(java.nio.file.Path.of(reportPath).toUri());
            } catch (RuntimeException ignored) {
                // MCP may already have opened the report.
            }
        }
        allureStatus.setText(afterGenerate
                ? "Allure generated and opened: " + blankDash(reportPath)
                : "Allure opened: " + blankDash(reportPath));
    }

    private void applySummaryResult(@Nullable ShaftMcpToolResult result, @Nullable Throwable error) {
        if (error != null) {
            String message = truncate(error.getMessage());
            engineerSummary.setText(message);
            stakeholderSummary.setText(message);
            return;
        }
        if (result == null || !result.success()) {
            String output = result == null || result.output() == null ? "report_summary failed." : result.output();
            engineerSummary.setText(truncate(output));
            stakeholderSummary.setText(truncate(output));
            return;
        }
        String raw = result.output() == null ? "" : result.output();
        try {
            JsonObject root = JsonParser.parseString(raw).getAsJsonObject();
            if (root.has("empty") && root.get("empty").getAsBoolean()) {
                String message = text(root, "emptyMessage");
                engineerSummary.setText(message);
                stakeholderSummary.setText(message);
                allureStatus.setText(message);
                return;
            }
            engineerSummary.setText(text(root, "engineerSummary"));
            stakeholderSummary.setText(text(root, "stakeholderSummary"));
            allureStatus.setText("Summaries refreshed (reconciled counts; no secrets).");
        } catch (RuntimeException exception) {
            engineerSummary.setText(truncate(raw));
            stakeholderSummary.setText(truncate(raw));
        }
    }

    private void copyText(String value, String statusMessage) {
        String text = value == null ? "" : value;
        try {
            CopyPasteManager.getInstance().setContents(new StringSelection(text));
            allureStatus.setText(statusMessage);
        } catch (RuntimeException exception) {
            allureStatus.setText("Copy unavailable in this context.");
        }
    }

    private void updateEnabled() {
        openAllure.setEnabled(!busy);
        generateReport.setEnabled(!busy);
        refreshSummaries.setEnabled(!busy);
        copyEngineer.setEnabled(!busy);
        copyStakeholder.setEnabled(!busy);
    }

    private static JBTextArea summaryArea(String accessibleName) {
        JBTextArea area = new JBTextArea(8, 40);
        area.setEditable(false);
        area.setLineWrap(true);
        area.setWrapStyleWord(true);
        area.getAccessibleContext().setAccessibleName(accessibleName);
        area.setMinimumSize(new Dimension(120, JBUI.scale(80)));
        return area;
    }

    private static JPanel wrapSummary(String title, JBTextArea area) {
        JPanel panel = new JPanel(new BorderLayout(0, JBUI.scale(4)));
        panel.setOpaque(false);
        JBLabel label = new JBLabel(title);
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        panel.add(label, BorderLayout.NORTH);
        panel.add(new JBScrollPane(area), BorderLayout.CENTER);
        return panel;
    }

    private static String text(JsonObject object, String key) {
        if (object == null || !object.has(key) || object.get(key).isJsonNull()) {
            return "";
        }
        try {
            return object.get(key).getAsString();
        } catch (RuntimeException ignored) {
            return "";
        }
    }

    private static String blankDash(String value) {
        return value == null || value.isBlank() ? "—" : value;
    }

    private static String truncate(String value) {
        if (value == null) {
            return "";
        }
        String trimmed = value.trim();
        return trimmed.length() <= 400 ? trimmed : trimmed.substring(0, 400) + "…";
    }
}
