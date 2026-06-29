package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBTabbedPane;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import java.awt.BorderLayout;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

/**
 * Top-level SHAFT IntelliJ tool window content.
 */
public final class ShaftToolWindowPanel extends JPanel {
    private final JComponent preferredFocusComponent;
    private final JBTabbedPane tabs;
    private final ShaftFeaturePanel advancedTools;
    private final List<ShaftFeaturePanel> featurePanels;

    public ShaftToolWindowPanel(@NotNull Project project) {
        this(project, ShaftSettingsState.getInstance().getState());
    }

    ShaftToolWindowPanel(Project project, @NotNull ShaftSettingsState.Settings settings) {
        super(new BorderLayout());
        tabs = new JBTabbedPane();
        tabs.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
        tabs.getAccessibleContext().setAccessibleName("SHAFT workflow tabs");
        ShaftAssistantPanel assistant = new ShaftAssistantPanel(project, settings);
        GuidedWorkflowPanel guided = new GuidedWorkflowPanel(project, this::prefillTool);
        EvidenceTriagePanel triage = new EvidenceTriagePanel(project, this::prefillTool);
        ShaftFeaturePanel recorderTools = new ShaftFeaturePanel(project, settings,
                List.of(new ToolCategory("Recorder", ToolTemplates.recorder())));
        ShaftFeaturePanel inspectorTools = new ShaftFeaturePanel(project, settings,
                List.of(new ToolCategory("Inspector", ToolTemplates.inspector())));
        ShaftFeaturePanel evidenceTools = new ShaftFeaturePanel(project, settings,
                List.of(new ToolCategory("Evidence", Stream.concat(
                        ToolTemplates.doctor().stream(), ToolTemplates.healer().stream()).toList())));
        ShaftFeaturePanel projectsTools = new ShaftFeaturePanel(project, settings,
                List.of(new ToolCategory("Projects", ToolTemplates.projects())));
        advancedTools = new ShaftFeaturePanel(project, settings);
        featurePanels = new ArrayList<>();
        featurePanels.add(recorderTools);
        featurePanels.add(inspectorTools);
        featurePanels.add(evidenceTools);
        featurePanels.add(projectsTools);
        featurePanels.add(advancedTools);
        preferredFocusComponent = assistant.preferredFocusComponent();
        tabs.addTab("Assistant", assistant);
        tabs.addTab("Guided", guided);
        tabs.addTab("Recorder", recorderTools);
        tabs.addTab("Inspector", inspectorTools);
        tabs.addTab("Triage", triage);
        tabs.addTab("Evidence Tools", evidenceTools);
        tabs.addTab("Projects", projectsTools);
        tabs.addTab("Advanced Tools", advancedTools);
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

    JBTabbedPane tabbedPane() {
        return tabs;
    }

    /**
     * Selects the workflow tab that owns the MCP tool template and pre-fills the request.
     *
     * @param toolName MCP tool name
     * @param arguments JSON arguments
     */
    public void prefillTool(@NotNull String toolName, @NotNull JsonObject arguments) {
        for (ShaftFeaturePanel panel : featurePanels) {
            if (panel.prefillTool(toolName, arguments)) {
                tabs.setSelectedComponent(panel);
                return;
            }
        }
        tabs.setSelectedComponent(advancedTools);
    }
}
