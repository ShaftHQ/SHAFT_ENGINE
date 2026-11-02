package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.notifications.FailedRunDoctorNotifier;
import org.jetbrains.annotations.Nullable;

import javax.swing.JButton;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;

/**
 * Reporting canvas: Doctor diagnosis card for retries vs history vs timing vs locator
 * (issue #5973 / S3-07).
 *
 * <p>Reuses MCP {@code doctor_analyze_failed_allure} and {@link AssistantMarkdown} — Doctor JSON
 * schema is the API. Does not add a parallel detector. Cancel/throttle remain in
 * {@link FailedRunDoctorNotifier}.
 */
final class ReportingDoctorPanel extends JPanel {
    static final String ACCESSIBLE_NAME = "SHAFT reporting Doctor diagnosis card";
    static final String TAB_TITLE = "Doctor";

    private final @Nullable Project project;
    private final JBTextField resultsPath;
    private final JBTextField historicalBundlesPath;
    private final JBLabel status;
    private final JBTextArea card;
    private final JButton analyze;
    private boolean busy;

    ReportingDoctorPanel() {
        this(null);
    }

    ReportingDoctorPanel(@Nullable Project project) {
        super(new BorderLayout(0, JBUI.scale(8)));
        this.project = project;
        setBorder(JBUI.Borders.empty(8));
        getAccessibleContext().setAccessibleName(ACCESSIBLE_NAME);
        getAccessibleContext().setAccessibleDescription(
                "Doctor diagnosis card: retries vs history vs timing vs locator from Doctor fields");

        JBLabel title = new JBLabel("Doctor diagnosis card");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JBLabel hint = new JBLabel(
                "<html>Surfaces existing Doctor <b>retry-correlation</b>, "
                        + "<b>historical-signature</b>, and <b>timing</b> vs <b>locator</b> fields. "
                        + "Uses <code>doctor_analyze_failed_allure</code> — no second detector. "
                        + "User-cancelled runs stay suppressed by FailedRunDoctorNotifier.</html>");
        hint.setAllowAutoWrapping(true);

        resultsPath = newField("allure-results path", "target/allure-results");
        historicalBundlesPath = newField("Historical Doctor bundle paths (comma-separated)", "");

        JPanel paths = new JPanel(new GridLayout(0, 1, 0, JBUI.scale(4)));
        paths.setOpaque(false);
        paths.add(labeled("Results", resultsPath));
        paths.add(labeled("History bundles", historicalBundlesPath));

        analyze = new JButton("Analyze with Doctor");
        analyze.getAccessibleContext().setAccessibleName("Analyze with Doctor");
        analyze.addActionListener(event -> runAnalyze());
        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, JBUI.scale(6), 0));
        actions.setOpaque(false);
        actions.add(analyze);

        status = new JBLabel("No Doctor diagnosis loaded yet.");
        status.getAccessibleContext().setAccessibleName("Doctor diagnosis status");

        card = new JBTextArea(12, 40);
        card.setEditable(false);
        card.setLineWrap(true);
        card.setWrapStyleWord(true);
        card.getAccessibleContext().setAccessibleName("Doctor diagnosis card");
        card.setText("Run Analyze with Doctor to render retries vs history vs timing vs locator "
                + "from existing Doctor fields.");

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
        add(new JBScrollPane(card), BorderLayout.CENTER);
        add(status, BorderLayout.SOUTH);
        updateEnabled();
    }

    JButton analyzeButton() {
        return analyze;
    }

    JBTextArea cardArea() {
        return card;
    }

    JBLabel statusLabel() {
        return status;
    }

    /**
     * Applies raw Doctor MCP / doctor-report JSON onto the card via {@link AssistantMarkdown}
     * (package-visible for fixture tests — SC-001 / SC-002).
     */
    void applyDoctorJson(String raw) {
        if (raw == null || raw.isBlank()) {
            card.setText("No Doctor diagnosis yet.");
            status.setText("Empty Doctor payload.");
            return;
        }
        try {
            String markdown = AssistantMarkdown.fromMcpOutput("doctor_analyze_failed_allure", raw);
            if (markdown == null || markdown.isBlank()) {
                // Bare diagnosis / doctor-report without MCP text envelope.
                markdown = AssistantMarkdown.fromMcpOutput(raw);
            }
            if (markdown == null || markdown.isBlank()) {
                card.setText(truncate(raw));
                status.setText("Doctor payload could not be humanized.");
                return;
            }
            card.setText(markdown);
            status.setText(statusForCard(markdown));
        } catch (RuntimeException exception) {
            card.setText(truncate(raw));
            status.setText("Doctor payload parse failed.");
        }
    }

    private static String statusForCard(String markdown) {
        if (markdown.contains("Retry correlation") || markdown.contains("TIMING_SYNCHRONIZATION")) {
            if (markdown.contains("Historical signature")) {
                return "Diagnosis card: timing + historical signature dimensions rendered.";
            }
            return "Diagnosis card: timing / retry-correlation rendered.";
        }
        if (markdown.contains("Historical signature")) {
            return "Diagnosis card: historical signature cluster key rendered.";
        }
        if (markdown.contains("Locator")) {
            return "Diagnosis card: locator dimension rendered.";
        }
        return "Diagnosis card rendered from Doctor fields.";
    }

    private void runAnalyze() {
        if (project == null) {
            applyDoctorJson("{\"empty\":true}");
            status.setText("MCP is not available in this context.");
            card.setText("MCP is not available in this context.");
            return;
        }
        busy = true;
        updateEnabled();
        JsonObject arguments = FailedRunDoctorNotifier.doctorArguments(resultsPath.getText().trim());
        JsonArray historical = new JsonArray();
        String historicalRaw = historicalBundlesPath.getText().trim();
        if (!historicalRaw.isBlank()) {
            for (String part : historicalRaw.split(",")) {
                String path = part.trim();
                if (!path.isBlank()) {
                    historical.add(path);
                }
            }
        }
        arguments.add("historicalBundlePaths", historical);
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("doctor_analyze_failed_allure", arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    busy = false;
                    updateEnabled();
                    applyToolResult(result, error);
                }));
    }

    private void applyToolResult(@Nullable ShaftMcpToolResult result, @Nullable Throwable error) {
        if (error != null) {
            String message = error.getMessage() == null ? error.toString() : error.getMessage();
            status.setText("Doctor analyze failed: " + truncate(message));
            card.setText(truncate(message));
            return;
        }
        if (result == null || !result.success()) {
            String output = result == null || result.output() == null ? "" : result.output();
            status.setText(output.isBlank() ? "doctor_analyze_failed_allure failed." : truncate(output));
            card.setText(output.isBlank() ? "doctor_analyze_failed_allure failed." : truncate(output));
            return;
        }
        applyDoctorJson(result.output() == null ? "" : result.output());
    }

    private void updateEnabled() {
        analyze.setEnabled(!busy);
    }

    private static JBTextField newField(String accessibleName, String value) {
        JBTextField field = new JBTextField(value);
        field.getAccessibleContext().setAccessibleName(accessibleName);
        field.setToolTipText(accessibleName);
        Dimension preferred = field.getPreferredSize();
        preferred.width = Math.max(preferred.width, JBUI.scale(320));
        field.setPreferredSize(preferred);
        return field;
    }

    private static JPanel labeled(String labelText, JBTextField field) {
        JPanel row = new JPanel(new BorderLayout(JBUI.scale(6), 0));
        row.setOpaque(false);
        JBLabel label = new JBLabel(labelText);
        label.setLabelFor(field);
        row.add(label, BorderLayout.WEST);
        row.add(field, BorderLayout.CENTER);
        return row;
    }

    private static String truncate(String value) {
        if (value == null) {
            return "";
        }
        String trimmed = value.trim();
        return trimmed.length() <= 400 ? trimmed : trimmed.substring(0, 400) + "…";
    }
}
