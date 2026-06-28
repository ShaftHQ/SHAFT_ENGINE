package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBTabbedPane;
import com.google.gson.JsonObject;
import org.jetbrains.annotations.NotNull;

import javax.swing.JComponent;
import javax.swing.JPanel;
import java.awt.BorderLayout;

/**
 * Top-level SHAFT IntelliJ tool window content.
 */
public final class ShaftToolWindowPanel extends JPanel {
    private final JComponent preferredFocusComponent;
    private final JBTabbedPane tabs;
    private final ShaftFeaturePanel tools;

    public ShaftToolWindowPanel(@NotNull Project project) {
        super(new BorderLayout());
        tabs = new JBTabbedPane();
        ShaftAssistantPanel assistant = new ShaftAssistantPanel(project);
        tools = new ShaftFeaturePanel(project);
        preferredFocusComponent = assistant.preferredFocusComponent();
        tabs.addTab("Assistant", assistant);
        tabs.addTab("Tools", tools);
        add(tabs, BorderLayout.CENTER);
    }

    /**
     * Returns the default focus target.
     *
     * @return focus target
     */
    public JComponent preferredFocusComponent() {
        return preferredFocusComponent;
    }

    /**
     * Selects the Tools tab and pre-fills an MCP tool template.
     *
     * @param toolName MCP tool name
     * @param arguments JSON arguments
     */
    public void prefillTool(@NotNull String toolName, @NotNull JsonObject arguments) {
        tabs.setSelectedComponent(tools);
        tools.prefillTool(toolName, arguments);
    }
}
