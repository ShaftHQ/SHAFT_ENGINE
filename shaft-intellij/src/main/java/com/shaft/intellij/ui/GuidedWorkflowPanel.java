package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
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
    private final JComboBox<String> backend;
    private final JBTextField targetUrl;
    private final JBTextField intent;
    private final JBTextField sessionPath;
    private final JBTextArea codeSnippet;
    private final ToolPrefill prefill;

    GuidedWorkflowPanel(Project project, ToolPrefill prefill) {
        super(new BorderLayout(8, 8));
        this.prefill = prefill;
        setBorder(JBUI.Borders.empty(8));

        backend = new JComboBox<>(new String[]{"WebDriver", "Playwright"});
        backend.getAccessibleContext().setAccessibleName("Guided workflow backend");
        targetUrl = field("Target URL", "");
        intent = field("Intent", "Log in as a valid user");
        sessionPath = field("Session path", "recordings/intellij-capture.json");
        codeSnippet = new JBTextArea(6, 32);
        codeSnippet.getAccessibleContext().setAccessibleName("Generated code or guardrail input");
        codeSnippet.setLineWrap(true);
        codeSnippet.setWrapStyleWord(true);

        JPanel fields = new JPanel(new GridLayout(0, 1, 4, 4));
        fields.add(row("Backend", 'B', backend));
        fields.add(row("Target URL", 'U', targetUrl));
        fields.add(row("Intent", 'I', intent));
        fields.add(row("Session path", 'S', sessionPath));

        JPanel recorder = section("Recorder",
                button("Start recording", "Start a SHAFT recording", this::startRecording),
                button("Stop recording", "Stop the active SHAFT recording", this::stopRecording),
                button("Generate code", "Generate SHAFT code blocks from a recording", this::generateCode));
        JPanel locator = section("Locator",
                button("Inspect locator", "Inspect the page and propose locator candidates", this::inspectLocator),
                button("Guardrail check", "Check generated SHAFT code for automation anti-patterns", this::guardrailCheck));

        JPanel center = new JPanel(new BorderLayout(6, 6));
        center.add(fields, BorderLayout.NORTH);
        center.add(row("Code", 'C', codeSnippet), BorderLayout.CENTER);

        JPanel actions = new JPanel(new GridLayout(1, 2, 8, 8));
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
        JPanel row = new JPanel(new BorderLayout(4, 4));
        JLabel label = new JLabel(labelText);
        label.setDisplayedMnemonic(mnemonic);
        label.setLabelFor(component);
        row.add(label, BorderLayout.WEST);
        row.add(component, BorderLayout.CENTER);
        return row;
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
            case "Generate code" -> ShaftIcons.CODE;
            case "Inspect locator" -> ShaftIcons.SEARCH;
            case "Guardrail check" -> ShaftIcons.CHECK;
            default -> ShaftIcons.HELP;
        };
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

    @FunctionalInterface
    interface ToolPrefill {
        void prefill(String toolName, JsonObject arguments);
    }
}
