package com.shaft.intellij.ui;

import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBTabbedPane;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.JComponent;
import javax.swing.JPanel;
import java.awt.BorderLayout;

/**
 * Top-level SHAFT IntelliJ tool window content.
 */
public final class ShaftToolWindowPanel extends JPanel {
    private final JComponent preferredFocusComponent;

    public ShaftToolWindowPanel(@NotNull Project project) {
        super(new BorderLayout());
        JBTabbedPane tabs = new JBTabbedPane();
        ShaftFeaturePanel recorder = new ShaftFeaturePanel(project, ToolTemplates.recorder());
        preferredFocusComponent = recorder.preferredFocusComponent();
        tabs.addTab("Recorder", recorder);
        tabs.addTab("Playback", new ShaftFeaturePanel(project, ToolTemplates.playback()));
        tabs.addTab("Doctor", new ShaftFeaturePanel(project, ToolTemplates.doctor()));
        tabs.addTab("Healer", new ShaftFeaturePanel(project, ToolTemplates.healer()));
        tabs.addTab("Inspector", new ShaftFeaturePanel(project, ToolTemplates.inspector()));
        tabs.addTab("MCP", new ShaftFeaturePanel(project, ToolTemplates.mcp()));
        tabs.addTab("Guide", new ShaftFeaturePanel(project, ToolTemplates.guide()));
        if (ShaftSettingsState.getInstance().getState().autobotEnabled) {
            tabs.addTab("Autobot", new ShaftAutobotPanel(project));
        }
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
}
