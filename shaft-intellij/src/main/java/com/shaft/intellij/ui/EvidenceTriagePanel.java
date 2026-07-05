package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftCommandLine;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.Arrays;

/**
 * Guided failure-triage entry points backed by Doctor, Trace, and Healer MCP tools.
 */
final class EvidenceTriagePanel extends JPanel {
    private final JBTextField allurePath;
    private final JBTextField tracePath;
    private final JBTextField repositoryRoot;
    private final JBTextField sourceAllowlist;
    private final JBTextField sourcePath;
    private final JBTextField testCommand;
    private final GuidedWorkflowPanel.ToolPrefill prefill;

    EvidenceTriagePanel(Project project, GuidedWorkflowPanel.ToolPrefill prefill) {
        super(new BorderLayout(8, 8));
        this.prefill = prefill;
        setBorder(JBUI.Borders.empty(8));

        String basePath = project == null || project.getBasePath() == null || project.getBasePath().isBlank()
                ? "."
                : project.getBasePath();
        allurePath = field("Allure results path", "allure-results");
        tracePath = field("SHAFT trace path", "target/shaft-traces/latest.json");
        repositoryRoot = field("Repository root", basePath);
        sourceAllowlist = field("Approved source paths", "src/test/java");
        sourcePath = field("Locator source path", "src/test/java/example/Page.java");
        testCommand = field("Failed test Maven command", "mvn -q -Dtest=ExampleTest test");

        JPanel fields = new JPanel(new GridBagLayout());
        addFieldRow(fields, 0, row("Allure", 'A', allurePath));
        addFieldRow(fields, 1, row("Trace", 'T', tracePath));
        addFieldRow(fields, 2, row("Repository", 'R', repositoryRoot));
        addFieldRow(fields, 3, row("Sources", 'S', sourceAllowlist));
        addFieldRow(fields, 4, row("Locator source", 'L', sourcePath));
        addFieldRow(fields, 5, row("Command", 'C', testCommand));
        addFieldFiller(fields, 6);

        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        actions.add(button("Fix Failing Test", "Plan a repair from failure evidence via the coding partner",
                this::repairPlan));
        actions.add(button("Analyze Allure", "Analyze failed Allure evidence", this::analyzeAllure));
        actions.add(button("Analyze Trace", "Analyze the latest SHAFT trace", this::analyzeTrace));
        actions.add(button("Suggest Fix", "Build Doctor remediation snippets", this::suggestFix));
        actions.add(button("Run Healer", "Run a guarded failed-test healer loop", this::runHealer));
        actions.add(button("Propose Locator", "Create a review-only locator repair proposal", this::proposeLocator));

        add(introLabel("Evidence triage prepares Doctor, Trace, and Healer MCP requests."), BorderLayout.NORTH);
        add(fields, BorderLayout.CENTER);
        add(actions, BorderLayout.SOUTH);
    }

    private static JLabel introLabel(String text) {
        JLabel label = new JLabel(text);
        label.setFont(label.getFont().deriveFont(Font.BOLD, label.getFont().getSize2D() + 1f));
        label.setBorder(JBUI.Borders.emptyBottom(8));
        label.getAccessibleContext().setAccessibleName(text);
        return label;
    }

    private static JBTextField field(String accessibleName, String value) {
        JBTextField field = new JBTextField(value);
        field.getAccessibleContext().setAccessibleName(accessibleName);
        field.setToolTipText(accessibleName);
        return field;
    }

    private static JPanel row(String labelText, char mnemonic, JComponent component) {
        JPanel row = new JPanel(new BorderLayout(4, 4));
        JLabel label = new JLabel(labelText);
        label.setDisplayedMnemonic(mnemonic);
        label.setLabelFor(component);
        row.add(label, BorderLayout.WEST);
        row.add(component, BorderLayout.CENTER);
        return row;
    }

    private static void addFieldRow(JPanel panel, int rowIndex, JPanel row) {
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = rowIndex;
        constraints.weightx = 1.0;
        constraints.fill = GridBagConstraints.HORIZONTAL;
        constraints.insets = new Insets(0, 0, 6, 0);
        panel.add(row, constraints);
    }

    private static void addFieldFiller(JPanel panel, int rowIndex) {
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = rowIndex;
        constraints.weighty = 1.0;
        constraints.fill = GridBagConstraints.BOTH;
        panel.add(new JPanel(), constraints);
    }

    private static JButton button(String text, String description, Runnable action) {
        JButton button = new JButton();
        ShaftIconButtons.apply(button, description, text, iconFor(text));
        button.getAccessibleContext().setAccessibleDescription(description);
        button.addActionListener(event -> action.run());
        return button;
    }

    private static Icon iconFor(String text) {
        return switch (text) {
            case "Analyze Allure", "Analyze Trace" -> ShaftIcons.VIEW;
            case "Fix Failing Test", "Suggest Fix" -> ShaftIcons.EDIT;
            case "Run Healer" -> ShaftIcons.SEND;
            case "Propose Locator" -> ShaftIcons.SEARCH;
            default -> ShaftIcons.HELP;
        };
    }

    private void repairPlan() {
        JsonObject arguments = new JsonObject();
        String repository = repositoryRoot.getText().trim();
        arguments.addProperty("repositoryPath", repository.isBlank() ? "." : repository);
        arguments.addProperty("intent", "Repair the failing test using existing page objects and tests, then verify");
        arguments.addProperty("backend", "webdriver");
        arguments.addProperty("currentSourcePath", sourcePath.getText().trim());
        arguments.addProperty("selectedText", "");
        JsonArray artifacts = new JsonArray();
        addEvidence(artifacts, allurePath.getText());
        addEvidence(artifacts, tracePath.getText());
        arguments.add("artifactPaths", artifacts);
        arguments.addProperty("maxResults", 10);
        prefill.prefill("shaft_coding_partner_plan", arguments);
    }

    private static void addEvidence(JsonArray artifacts, String value) {
        String trimmed = value == null ? "" : value.trim();
        if (!trimmed.isBlank()) {
            artifacts.add(trimmed);
        }
    }

    private void analyzeAllure() {
        JsonObject arguments = doctorArguments();
        prefill.prefill("doctor_analyze_failed_allure", arguments);
    }

    private void analyzeTrace() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("tracePath", tracePath.getText().trim());
        arguments.addProperty("backend", "webdriver");
        prefill.prefill("doctor_analyze_trace", arguments);
    }

    private void suggestFix() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("jsonReportPath", "target/shaft-doctor/doctor-report.json");
        arguments.addProperty("repositoryRoot", repositoryRoot.getText().trim());
        arguments.add("allowedSourcePaths", stringArray(sourceAllowlist.getText()));
        arguments.addProperty("useAi", false);
        arguments.addProperty("allowLocalAi", false);
        arguments.addProperty("allowRemoteAi", false);
        arguments.addProperty("driverVariableName", "driver");
        prefill.prefill("doctor_suggest_fix", arguments);
    }

    private void runHealer() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("repositoryRoot", repositoryRoot.getText().trim());
        arguments.add("testCommand", commandArray(testCommand.getText()));
        arguments.addProperty("outputDirectory", "target/shaft-healer");
        arguments.addProperty("maxAttempts", 2);
        arguments.addProperty("includeScreenshots", true);
        arguments.addProperty("includePageSnapshots", false);
        arguments.add("allowedSourcePaths", stringArray(sourceAllowlist.getText()));
        arguments.addProperty("networkValidationApproved", false);
        arguments.addProperty("useConfiguredAi", false);
        arguments.addProperty("allowLocalAi", false);
        arguments.addProperty("allowRemoteAi", false);
        arguments.addProperty("driverVariableName", "driver");
        prefill.prefill("healer_run_failed_test", arguments);
    }

    private void proposeLocator() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("repositoryRoot", repositoryRoot.getText().trim());
        arguments.addProperty("healingReportPath", "target/shaft-heal/healing-report.json");
        arguments.addProperty("sourcePath", sourcePath.getText().trim());
        arguments.addProperty("sourcePatchConsent", false);
        arguments.addProperty("outputDirectory", "target/shaft-doctor/healing-proposals");
        prefill.prefill("doctor_propose_healed_locator", arguments);
    }

    private JsonObject doctorArguments() {
        JsonObject arguments = new JsonObject();
        arguments.add("allureResultPaths", stringArray(allurePath.getText()));
        arguments.add("historicalBundlePaths", new JsonArray());
        arguments.addProperty("outputDirectory", "target/shaft-doctor");
        arguments.addProperty("includeScreenshots", true);
        arguments.addProperty("includePageSnapshots", true);
        arguments.addProperty("minimumAllureResults", 1);
        arguments.addProperty("repositoryRoot", repositoryRoot.getText().trim());
        arguments.add("allowedSourcePaths", stringArray(sourceAllowlist.getText()));
        arguments.addProperty("useAi", false);
        arguments.addProperty("allowLocalAi", false);
        arguments.addProperty("allowRemoteAi", false);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    private static JsonArray stringArray(String values) {
        JsonArray array = new JsonArray();
        Arrays.stream((values == null ? "" : values).split("[,\\s]+"))
                .map(String::trim)
                .filter(value -> !value.isBlank())
                .forEach(array::add);
        return array;
    }

    private static JsonArray commandArray(String value) {
        JsonArray array = new JsonArray();
        ShaftCommandLine.parse(value == null ? "" : value).forEach(array::add);
        return array;
    }
}
