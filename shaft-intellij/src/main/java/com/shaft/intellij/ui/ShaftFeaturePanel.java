package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * MCP tools panel with editable JSON arguments.
 */
final class ShaftFeaturePanel extends JPanel {
    private final JComboBox<ToolCategory> categorySelector;
    private final JComboBox<ToolTemplate> toolSelector;
    private final JBTextArea argumentsArea;
    private final JBTextArea outputArea;
    private final JButton runButton;
    private final JLabel status;
    private final ShaftSettingsState.Settings settings;
    private final Map<String, String> argumentDrafts = new LinkedHashMap<>();
    private ToolTemplate activeTemplate;
    private boolean updatingTools;

    ShaftFeaturePanel(@NotNull Project project) {
        this(project, ShaftSettingsState.getInstance().getState());
    }

    ShaftFeaturePanel(Project project, @NotNull ShaftSettingsState.Settings settings) {
        super(new BorderLayout(8, 8));
        this.settings = settings;
        setBorder(JBUI.Borders.empty(8));
        categorySelector = new JComboBox<>(ToolTemplates.categories().toArray(ToolCategory[]::new));
        toolSelector = new JComboBox<>();
        argumentsArea = new JBTextArea(14, 56);
        argumentsArea.setLineWrap(true);
        argumentsArea.setWrapStyleWord(true);
        outputArea = new JBTextArea(10, 56);
        outputArea.setEditable(false);
        outputArea.setLineWrap(true);
        outputArea.setWrapStyleWord(true);
        status = new JLabel("Ready");

        runButton = new JButton("Run");
        runButton.addActionListener(event -> run(project));
        categorySelector.addActionListener(event -> refreshTools());
        toolSelector.addActionListener(event -> {
            if (!updatingTools) {
                loadSelectedTemplate();
            }
        });
        refreshTools();

        JPanel header = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        JLabel categoryLabel = label("Category", 'C', categorySelector);
        JLabel toolLabel = label("Tool", 'T', toolSelector);
        header.add(categoryLabel);
        header.add(categorySelector);
        header.add(toolLabel);
        header.add(toolSelector);
        header.add(runButton);
        header.add(status);

        JPanel north = new JPanel(new BorderLayout(6, 6));
        north.add(header, BorderLayout.NORTH);
        north.add(setupNotice(project, settings), BorderLayout.SOUTH);

        JPanel center = new JPanel(new BorderLayout(6, 6));
        JLabel argumentsLabel = label("Arguments", 'A', argumentsArea);
        center.add(argumentsLabel, BorderLayout.NORTH);
        center.add(new JBScrollPane(argumentsArea), BorderLayout.CENTER);

        JPanel output = new JPanel(new BorderLayout(6, 6));
        JLabel outputLabel = label("Output", 'O', outputArea);
        output.add(outputLabel, BorderLayout.NORTH);
        output.add(new JBScrollPane(outputArea), BorderLayout.CENTER);

        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, center, output);
        splitPane.setResizeWeight(0.66);
        splitPane.setBorder(JBUI.Borders.empty());
        add(north, BorderLayout.NORTH);
        add(splitPane, BorderLayout.CENTER);
    }

    JComponent preferredFocusComponent() {
        return argumentsArea;
    }

    private void run(Project project) {
        ToolTemplate template = selectedTemplate();
        if (template == null) {
            return;
        }
        if (!mcpConfigured()) {
            status.setText("Configure MCP");
            outputArea.setText("Configure SHAFT MCP in Settings before running Tools requests.");
            return;
        }
        JsonObject arguments;
        try {
            arguments = JsonParser.parseString(argumentsArea.getText().isBlank() ? "{}" : argumentsArea.getText())
                    .getAsJsonObject();
        } catch (RuntimeException exception) {
            status.setText("Invalid JSON");
            outputArea.setText("Invalid JSON: " + exception.getMessage());
            return;
        }
        setRunning(true, "Running " + template.toolName() + "...");
        outputArea.setText("");
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool(template.toolName(), arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                        () -> showResult(result, error)));
    }

    private void showResult(ShaftMcpToolResult result, Throwable error) {
        setRunning(false, error == null && result != null && result.success() ? "Finished" : "Failed");
        if (error != null) {
            outputArea.setText(error.getMessage());
        } else if (result == null) {
            outputArea.setText("No result returned.");
        } else {
            outputArea.setText(result.output());
        }
    }

    private void refreshTools() {
        saveActiveDraft();
        ToolCategory category = (ToolCategory) categorySelector.getSelectedItem();
        updatingTools = true;
        toolSelector.removeAllItems();
        if (category == null) {
            updatingTools = false;
            activeTemplate = null;
            argumentsArea.setText("");
            return;
        }
        for (ToolTemplate template : category.templates()) {
            toolSelector.addItem(template);
        }
        updatingTools = false;
        loadSelectedTemplate();
    }

    private void loadSelectedTemplate() {
        saveActiveDraft();
        ToolTemplate selected = selectedTemplate();
        activeTemplate = selected;
        if (selected != null) {
            argumentsArea.setText(argumentDrafts.getOrDefault(draftKey(selected), selected.arguments()));
        }
    }

    private void saveActiveDraft() {
        if (activeTemplate != null) {
            argumentDrafts.put(draftKey(activeTemplate), argumentsArea.getText());
        }
    }

    private void setRunning(boolean running, String message) {
        runButton.setEnabled(!running);
        categorySelector.setEnabled(!running);
        toolSelector.setEnabled(!running);
        status.setText(message);
    }

    private ToolTemplate selectedTemplate() {
        return (ToolTemplate) toolSelector.getSelectedItem();
    }

    private static JLabel label(String text, char mnemonic, JComponent target) {
        JLabel label = new JLabel(text);
        label.setDisplayedMnemonic(mnemonic);
        label.setLabelFor(target);
        return label;
    }

    private boolean mcpConfigured() {
        return settings.mcpCommand != null && !settings.mcpCommand.isBlank();
    }

    private static JPanel setupNotice(Project project, ShaftSettingsState.Settings settings) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        panel.add(new JLabel("Configure SHAFT MCP to run Assistant and Tools."));
        JButton openSettings = new JButton("Open Settings");
        openSettings.addActionListener(event -> {
            if (project != null) {
                ShowSettingsUtil.getInstance().showSettingsDialog(project, "SHAFT");
            }
        });
        panel.add(openSettings);
        panel.setVisible(settings.mcpCommand == null || settings.mcpCommand.isBlank());
        return panel;
    }

    private static String draftKey(ToolTemplate template) {
        return template.toolName() + "\n" + template.label();
    }
}
