package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.jetbrains.annotations.NotNull;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.util.List;

/**
 * Generic MCP tool tab with editable JSON arguments.
 */
final class ShaftFeaturePanel extends JPanel {
    private final JComboBox<ToolTemplate> toolSelector;
    private final JBTextArea argumentsArea;
    private final JBTextArea outputArea;

    ShaftFeaturePanel(@NotNull Project project, List<ToolTemplate> templates) {
        super(new BorderLayout(8, 8));
        toolSelector = new JComboBox<>(templates.toArray(ToolTemplate[]::new));
        argumentsArea = new JBTextArea(14, 56);
        outputArea = new JBTextArea(10, 56);
        outputArea.setEditable(false);
        if (!templates.isEmpty()) {
            argumentsArea.setText(templates.get(0).arguments());
        }

        JButton runButton = new JButton("Run");
        runButton.addActionListener(event -> run(project));
        toolSelector.addActionListener(event -> {
            ToolTemplate selected = selectedTemplate();
            if (selected != null) {
                argumentsArea.setText(selected.arguments());
            }
        });

        JPanel header = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        header.add(new JLabel("Tool"));
        header.add(toolSelector);
        header.add(runButton);

        JPanel center = new JPanel(new BorderLayout(6, 6));
        center.add(new JLabel("Arguments"), BorderLayout.NORTH);
        center.add(new JBScrollPane(argumentsArea), BorderLayout.CENTER);

        JPanel output = new JPanel(new BorderLayout(6, 6));
        output.add(new JLabel("Output"), BorderLayout.NORTH);
        output.add(new JBScrollPane(outputArea), BorderLayout.CENTER);

        add(header, BorderLayout.NORTH);
        add(center, BorderLayout.CENTER);
        add(output, BorderLayout.SOUTH);
    }

    JComponent preferredFocusComponent() {
        return argumentsArea;
    }

    private void run(Project project) {
        ToolTemplate template = selectedTemplate();
        if (template == null) {
            return;
        }
        JsonObject arguments;
        try {
            arguments = JsonParser.parseString(argumentsArea.getText().isBlank() ? "{}" : argumentsArea.getText())
                    .getAsJsonObject();
        } catch (RuntimeException exception) {
            outputArea.setText("Invalid JSON: " + exception.getMessage());
            return;
        }
        outputArea.setText("Running " + template.toolName() + "...");
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool(template.toolName(), arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                        () -> showResult(result, error)));
    }

    private void showResult(ShaftMcpToolResult result, Throwable error) {
        if (error != null) {
            outputArea.setText(error.getMessage());
        } else {
            outputArea.setText(result.output());
        }
    }

    private ToolTemplate selectedTemplate() {
        return (ToolTemplate) toolSelector.getSelectedItem();
    }
}
