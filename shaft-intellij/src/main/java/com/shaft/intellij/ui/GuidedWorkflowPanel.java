package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.Icon;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;

/**
 * Guided recorder, locator, and code-generation entry points backed by existing MCP tools.
 */
final class GuidedWorkflowPanel extends JPanel {
    private final Project project;
    private final JComboBox<String> backend;
    private final JComboBox<WorkflowTemplate> templateSelector;
    private final JBTextField targetUrl;
    private final JBTextField intent;
    private final JBTextField currentSourcePath;
    private final JBTextField artifactPaths;
    private final JBTextField sessionPath;
    private final JBTextArea codeSnippet;
    private final ToolPrefill prefill;

    GuidedWorkflowPanel(Project project, ToolPrefill prefill) {
        super(new BorderLayout(8, 8));
        this.project = project;
        this.prefill = prefill;
        setBorder(JBUI.Borders.empty(8));

        backend = new JComboBox<>(new String[]{"WebDriver", "Playwright"});
        backend.getAccessibleContext().setAccessibleName("Guided workflow backend");
        templateSelector = new JComboBox<>(WorkflowTemplate.values());
        templateSelector.setPrototypeDisplayValue(WorkflowTemplate.CREATE_NEW_SHAFT_PROJECT);
        templateSelector.getAccessibleContext().setAccessibleName("Workflow template");
        templateSelector.addActionListener(event -> updateTemplateDescription());
        targetUrl = field("Target URL", "");
        intent = field("Intent", "Log in as a valid user");
        currentSourcePath = field("Current source path", "");
        artifactPaths = field("Evidence paths", "");
        sessionPath = field("Session path", "recordings/intellij-capture.json");
        codeSnippet = new JBTextArea(6, 32);
        codeSnippet.getAccessibleContext().setAccessibleName("Generated code or guardrail input");
        codeSnippet.getAccessibleContext().setAccessibleDescription(
                "Paste Java code for review-only guardrail checks.");
        codeSnippet.setLineWrap(true);
        codeSnippet.setWrapStyleWord(true);
        updateTemplateDescription();

        JPanel fields = new JPanel(new GridLayout(0, 1, 4, 4));
        fields.add(row("Backend", 'B', backend));
        fields.add(row("Template", 'T', templateControls(), templateSelector));
        fields.add(row("Target URL", 'U', targetUrl));
        fields.add(row("Intent", 'I', intent));
        fields.add(row("Current source", 'R', currentSourcePath));
        fields.add(row("Evidence paths", 'E', artifactPaths));
        fields.add(row("Session path", 'S', sessionPath));

        JPanel partner = section("Coding Partner",
                button("Plan coding partner", "Plan repository-aware SHAFT reuse, missing code, proof, and validation",
                        this::planPartnerWork),
                button("Find reuse", "Find existing Java test and page-object anchors before generating code",
                        this::findReuse));
        JPanel recorder = section("Recorder",
                button("Start recording", "Start a SHAFT recording", this::startRecording),
                button("Stop recording", "Stop the active SHAFT recording", this::stopRecording),
                button("Review code", "Generate reviewed SHAFT code blocks from a recording", this::generateCode));
        JPanel locator = section("Locator",
                button("Inspect locator", "Inspect the page and propose locator candidates", this::inspectLocator),
                button("Guardrail check", "Check generated SHAFT code for automation anti-patterns", this::guardrailCheck));

        JPanel center = new JPanel(new BorderLayout(6, 6));
        center.add(fields, BorderLayout.NORTH);
        center.add(row("Code", 'C', codeSnippet), BorderLayout.CENTER);

        JPanel actions = new JPanel(new GridLayout(1, 3, 8, 8));
        actions.add(partner);
        actions.add(recorder);
        actions.add(locator);

        add(new JLabel("Guided workflows prepare reviewed SHAFT MCP requests."), BorderLayout.NORTH);
        add(center, BorderLayout.CENTER);
        add(actions, BorderLayout.SOUTH);
    }

    private static JBTextField field(String accessibleName, String value) {
        JBTextField field = new JBTextField(value);
        field.getAccessibleContext().setAccessibleName(accessibleName);
        return field;
    }

    private static JPanel row(String labelText, char mnemonic, JComponent component) {
        return row(labelText, mnemonic, component, component);
    }

    private static JPanel row(String labelText, char mnemonic, JComponent component, JComponent labelFor) {
        JPanel row = new JPanel(new BorderLayout(4, 4));
        JLabel label = new JLabel(labelText);
        label.setDisplayedMnemonic(mnemonic);
        label.setLabelFor(labelFor);
        row.add(label, BorderLayout.WEST);
        row.add(component, BorderLayout.CENTER);
        return row;
    }

    private JPanel templateControls() {
        JPanel controls = new JPanel(new BorderLayout(4, 0));
        controls.add(templateSelector, BorderLayout.CENTER);
        controls.add(button("Use template", "Prefill the selected workflow template", this::applyTemplate),
                BorderLayout.EAST);
        return controls;
    }

    private static JPanel section(String title, JButton... buttons) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        panel.add(new JLabel(title));
        for (JButton button : buttons) {
            panel.add(button);
        }
        return panel;
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
            case "Start recording" -> ShaftIcons.SEND;
            case "Stop recording" -> ShaftIcons.CANCEL;
            case "Review code" -> ShaftIcons.CODE;
            case "Use template" -> ShaftIcons.SEND;
            case "Plan coding partner" -> ShaftIcons.CODE;
            case "Find reuse" -> ShaftIcons.SEARCH;
            case "Inspect locator" -> ShaftIcons.SEARCH;
            case "Guardrail check" -> ShaftIcons.CHECK;
            default -> ShaftIcons.HELP;
        };
    }

    private void updateTemplateDescription() {
        WorkflowTemplate template = selectedTemplate();
        if (template != null) {
            templateSelector.getAccessibleContext().setAccessibleDescription(template.description);
        }
    }

    private void applyTemplate() {
        WorkflowTemplate template = selectedTemplate();
        if (template == null) {
            return;
        }
        switch (template) {
            case RECORD_BROWSER_FLOW -> prefill.prefill("test_automation_scenarios", capturePageObjectWorkflow());
            case ANALYZE_FAILED_ALLURE -> prefill.prefill("doctor_analyze_failed_allure", failedAllureAnalysis());
            case CONVERT_SELENIUM_SNIPPET -> prefill.prefill("test_automation_scenarios", seleniumConversionWorkflow());
            case CREATE_NEW_SHAFT_PROJECT -> prefill.prefill("shaft_project_create", newShaftProject());
            case INSPECT_CURRENT_PAGE_LOCATORS -> prefill.prefill("browser_get_page_dom", currentPageDomInspection());
            default -> throw new IllegalStateException("Unsupported workflow template: " + template);
        }
    }

    private WorkflowTemplate selectedTemplate() {
        Object selected = templateSelector.getSelectedItem();
        return selected instanceof WorkflowTemplate template ? template : null;
    }

    private static JsonObject capturePageObjectWorkflow() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("area", "capture");
        arguments.addProperty("intent",
                "Record a browser flow and generate review-only Page Object SHAFT code from the capture. "
                        + "Do not write source files before explicit approval.");
        arguments.addProperty("maxResults", 5);
        return arguments;
    }

    private static JsonObject seleniumConversionWorkflow() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("area", "web");
        arguments.addProperty("intent",
                "Convert a pasted Selenium WebDriver Java snippet to SHAFT syntax using SHAFT.GUI.WebDriver, "
                        + "driver.browser(), driver.element(), driver.element().touch(), and SHAFT.GUI.Locator. "
                        + "Return review-only code and do not write source files.");
        arguments.addProperty("maxResults", 5);
        return arguments;
    }

    private static JsonObject failedAllureAnalysis() {
        JsonObject arguments = new JsonObject();
        arguments.add("allureResultPaths", array("allure-results"));
        arguments.add("historicalBundlePaths", array());
        arguments.addProperty("outputDirectory", "target/shaft-doctor");
        arguments.addProperty("includeScreenshots", true);
        arguments.addProperty("includePageSnapshots", true);
        arguments.addProperty("minimumAllureResults", 1);
        arguments.addProperty("repositoryRoot", ".");
        arguments.add("allowedSourcePaths", array());
        arguments.addProperty("useAi", false);
        arguments.addProperty("allowLocalAi", false);
        arguments.addProperty("allowRemoteAi", false);
        arguments.addProperty("driverVariableName", "driver");
        return arguments;
    }

    private static JsonObject newShaftProject() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputDirectory", "shaft-web-testng");
        arguments.addProperty("runner", "TestNG");
        arguments.addProperty("platform", "web");
        arguments.addProperty("groupId", "io.github.yourUsername");
        arguments.addProperty("artifactId", "shaft-web-testng");
        arguments.addProperty("version", "1.0.0");
        arguments.add("optionalModules", array());
        arguments.addProperty("includeGithubActions", true);
        arguments.addProperty("includeDependabot", true);
        arguments.addProperty("overwrite", false);
        return arguments;
    }

    private static JsonObject currentPageDomInspection() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("maxCharacters", 12_000);
        return arguments;
    }

    private static JsonArray array(String... values) {
        JsonArray array = new JsonArray();
        for (String value : values) {
            array.add(value);
        }
        return array;
    }

    private void startRecording() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("outputPath", sessionPath.getText().trim());
        if (playwright()) {
            arguments.addProperty("mode", "default");
            arguments.addProperty("includeSensitiveValues", false);
            prefill.prefill("playwright_record_start", arguments);
        } else {
            arguments.addProperty("targetUrl", targetUrl.getText().trim());
            arguments.addProperty("browser", "Chrome");
            arguments.addProperty("headless", false);
            prefill.prefill("capture_start", arguments);
        }
    }

    private void planPartnerWork() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("repositoryPath", ".");
        arguments.addProperty("intent", intent.getText().trim());
        arguments.addProperty("backend", backend.getSelectedItem().toString());
        arguments.addProperty("currentSourcePath", currentSourcePath());
        arguments.addProperty("selectedText", selectedText());
        arguments.add("artifactPaths", delimitedArray(artifactPaths.getText()));
        arguments.addProperty("maxResults", 10);
        prefill.prefill("shaft_coding_partner_plan", arguments);
    }

    private void findReuse() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("repositoryPath", ".");
        arguments.addProperty("maxResults", 10);
        prefill.prefill("capture_target_candidates", arguments);
    }

    private void stopRecording() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("discard", false);
        prefill.prefill(playwright() ? "playwright_record_stop" : "capture_stop", arguments);
    }

    private void generateCode() {
        JsonObject arguments = new JsonObject();
        if (playwright()) {
            arguments.addProperty("recordingPath", sessionPath.getText().trim());
            arguments.addProperty("driverVariableName", "driver");
            prefill.prefill("playwright_recording_code_blocks", arguments);
        } else {
            arguments.addProperty("sessionPath", sessionPath.getText().trim());
            arguments.addProperty("outputDirectory", ".");
            arguments.addProperty("packageName", "tests.generated");
            arguments.addProperty("className", "GeneratedShaftTest");
            arguments.addProperty("overwrite", false);
            arguments.addProperty("driverVariableName", "driver");
            prefill.prefill("capture_code_blocks", arguments);
        }
    }

    private void inspectLocator() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", targetUrl.getText().trim());
        arguments.addProperty("userIntent", intent.getText().trim());
        arguments.addProperty("maxCharacters", 12_000);
        arguments.addProperty("maxElements", 10);
        prefill.prefill("browser_open_intent", arguments);
    }

    private void guardrailCheck() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("language", "java");
        arguments.addProperty("code", codeSnippet.getText());
        prefill.prefill("test_code_guardrails_check", arguments);
    }

    private boolean playwright() {
        return "Playwright".equals(backend.getSelectedItem());
    }

    private String currentSourcePath() {
        String typedPath = currentSourcePath.getText().trim();
        if (!typedPath.isBlank() || project == null) {
            return typedPath;
        }
        VirtualFile[] selectedFiles = FileEditorManager.getInstance(project).getSelectedFiles();
        return selectedFiles.length == 0 ? "" : selectedFiles[0].getPath();
    }

    private String selectedText() {
        String pastedText = codeSnippet.getText().trim();
        if (!pastedText.isBlank() || project == null) {
            return pastedText;
        }
        Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
        return editor == null || editor.getSelectionModel().getSelectedText() == null
                ? ""
                : editor.getSelectionModel().getSelectedText();
    }

    private static JsonArray delimitedArray(String values) {
        JsonArray array = new JsonArray();
        if (values == null || values.isBlank()) {
            return array;
        }
        for (String value : values.split("[,;\\n]+")) {
            String trimmed = value.trim();
            if (!trimmed.isBlank()) {
                array.add(trimmed);
            }
        }
        return array;
    }

    @FunctionalInterface
    interface ToolPrefill {
        void prefill(String toolName, JsonObject arguments);
    }

    private enum WorkflowTemplate {
        RECORD_BROWSER_FLOW(
                "Record browser flow and generate Page Object code",
                "Prefills a review-only capture workflow plan for Page Object code generation."),
        ANALYZE_FAILED_ALLURE(
                "Analyze failed Allure results",
                "Prefills deterministic Doctor analysis with AI and source edits disabled."),
        CONVERT_SELENIUM_SNIPPET(
                "Convert Selenium snippet to SHAFT syntax",
                "Prefills a review-only SHAFT syntax conversion workflow."),
        CREATE_NEW_SHAFT_PROJECT(
                "Create a new SHAFT project",
                "Prefills a new project request with overwrite disabled."),
        INSPECT_CURRENT_PAGE_LOCATORS(
                "Inspect current page locators",
                "Prefills a bounded current-page DOM inspection.");

        private final String label;
        private final String description;

        WorkflowTemplate(String label, String description) {
            this.label = label;
            this.description = description;
        }

        @Override
        public String toString() {
            return label;
        }
    }
}
