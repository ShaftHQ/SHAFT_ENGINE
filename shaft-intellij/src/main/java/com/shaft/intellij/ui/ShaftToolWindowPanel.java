package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

/**
 * Top-level SHAFT IntelliJ tool window content.
 */
public final class ShaftToolWindowPanel extends JPanel {
    private final Project project;
    private final ShaftSettingsState.Settings settings;
    private final ShaftAssistantChatState assistantChatState;
    private JComponent preferredFocusComponent;
    private JComboBox<WorkflowView> workflowSelector;
    private JPanel workflowCards;
    private CardLayout workflowLayout;
    private final ShaftMcpSetupPanel.AgentReadinessProbe readinessProbe;
    private ShaftFeaturePanel advancedTools;
    private List<ShaftFeaturePanel> featurePanels = List.of();
    private List<WorkflowView> workflowViews = List.of();

    public ShaftToolWindowPanel(@NotNull Project project) {
        this(project, ShaftSettingsState.getInstance().getState());
    }

    ShaftToolWindowPanel(Project project, @NotNull ShaftSettingsState.Settings settings) {
        this(project, settings, AssistantLocalAgentRunner::readiness);
    }

    ShaftToolWindowPanel(Project project, @NotNull ShaftSettingsState.Settings settings,
                         @NotNull ShaftMcpSetupPanel.AgentReadinessProbe readinessProbe) {
        this(project, settings, readinessProbe, ShaftAssistantChatState.getInstance(project));
    }

    ShaftToolWindowPanel(Project project,
                         @NotNull ShaftSettingsState.Settings settings,
                         @NotNull ShaftMcpSetupPanel.AgentReadinessProbe readinessProbe,
                         @NotNull ShaftAssistantChatState assistantChatState) {
        super(new BorderLayout());
        this.project = project;
        this.settings = settings;
        this.readinessProbe = readinessProbe;
        this.assistantChatState = assistantChatState;
        if (mcpReady(settings)) {
            showMainView();
        } else {
            showSetupView();
        }
    }

    private void showSetupView() {
        removeAll();
        ShaftMcpSetupPanel setup = new ShaftMcpSetupPanel(project, settings, this::showMainView, readinessProbe);
        preferredFocusComponent = setup.preferredFocusComponent();
        workflowSelector = null;
        workflowCards = null;
        workflowLayout = null;
        advancedTools = null;
        featurePanels = List.of();
        workflowViews = List.of();
        add(setup, BorderLayout.CENTER);
        revalidate();
        repaint();
    }

    private void showMainView() {
        removeAll();
        ShaftAssistantPanel assistant = new ShaftAssistantPanel(project, settings,
                assistantChatState, this::showSetupView);
        preferredFocusComponent = assistant.preferredFocusComponent();
        if (!settings.advancedUiEnabled) {
            workflowSelector = null;
            workflowCards = null;
            workflowLayout = null;
            advancedTools = null;
            featurePanels = List.of();
            workflowViews = List.of(new WorkflowView("Assistant", assistant, ShaftIcons.SEND));
            add(assistant, BorderLayout.CENTER);
            revalidate();
            repaint();
            return;
        }

        workflowLayout = new CardLayout();
        workflowCards = new JPanel(workflowLayout);
        workflowCards.getAccessibleContext().setAccessibleName("SHAFT workflow content");
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
        workflowViews = List.of(
                new WorkflowView("Assistant", assistant, ShaftIcons.SEND),
                new WorkflowView("Guided", guided, ShaftIcons.CODE),
                new WorkflowView("Recorder", recorderTools, ShaftIcons.VIEW),
                new WorkflowView("Inspector", inspectorTools, ShaftIcons.SEARCH),
                new WorkflowView("Triage", triage, ShaftIcons.CHECK),
                new WorkflowView("Evidence", evidenceTools, ShaftIcons.EDIT),
                new WorkflowView("Projects", projectsTools, ShaftIcons.SETTINGS),
                new WorkflowView("Advanced", advancedTools, ShaftIcons.HELP));
        for (WorkflowView view : workflowViews) {
            workflowCards.add(view.component(), view.label());
        }
        workflowSelector = new JComboBox<>(new DefaultComboBoxModel<>(
                workflowViews.toArray(new WorkflowView[0])));
        workflowSelector.getAccessibleContext().setAccessibleName("SHAFT workflow selector");
        workflowSelector.setRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list,
                                                          Object value,
                                                          int index,
                                                          boolean isSelected,
                                                          boolean cellHasFocus) {
                JLabel label = (JLabel) super.getListCellRendererComponent(
                        list, value, index, isSelected, cellHasFocus);
                label.setBorder(JBUI.Borders.empty(2, 6));
                if (value instanceof WorkflowView workflow) {
                    label.setIcon(workflow.icon());
                    label.setIconTextGap(6);
                }
                return label;
            }
        });
        workflowSelector.setPrototypeDisplayValue(new WorkflowView("Assistant", assistant, ShaftIcons.SEND));
        Dimension selectorSize = workflowSelector.getPreferredSize();
        int selectorHeight = Math.max(30, selectorSize.height);
        workflowSelector.setPreferredSize(JBUI.size(Math.max(150, selectorSize.width), selectorHeight));
        workflowSelector.setMinimumSize(JBUI.size(140, selectorHeight));
        workflowSelector.addActionListener(event -> showSelectedWorkflow());
        JPanel header = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 2));
        header.setBorder(JBUI.Borders.empty(6, 8, 4, 8));
        JLabel label = new JLabel("Workflow");
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        label.setLabelFor(workflowSelector);
        header.add(label);
        header.add(workflowSelector);
        add(header, BorderLayout.NORTH);
        add(workflowCards, BorderLayout.CENTER);
        revalidate();
        repaint();
    }

    /**
     * Returns the default focus target.
     *
     * @return focus target
     */
    public JComponent preferredFocusComponent() {
        return preferredFocusComponent;
    }

    JComboBox<WorkflowView> workflowSelector() {
        return workflowSelector;
    }

    /**
     * Selects the workflow tab that owns the MCP tool template and pre-fills the request.
     *
     * @param toolName MCP tool name
     * @param arguments JSON arguments
     */
    public void prefillTool(@NotNull String toolName, @NotNull JsonObject arguments) {
        if (workflowSelector == null || !settings.advancedUiEnabled) {
            return;
        }
        for (ShaftFeaturePanel panel : featurePanels) {
            if (panel.prefillTool(toolName, arguments)) {
                selectWorkflow(panel);
                return;
            }
        }
        selectWorkflow(advancedTools);
    }

    private void showSelectedWorkflow() {
        WorkflowView view = workflowSelector == null ? null : (WorkflowView) workflowSelector.getSelectedItem();
        if (view != null && workflowLayout != null && workflowCards != null) {
            workflowLayout.show(workflowCards, view.label());
        }
    }

    private void selectWorkflow(JComponent component) {
        for (WorkflowView view : workflowViews) {
            if (view.component() == component) {
                workflowSelector.setSelectedItem(view);
                workflowLayout.show(workflowCards, view.label());
                return;
            }
        }
    }

    private static boolean mcpReady(ShaftSettingsState.Settings settings) {
        return settings != null && settings.mcpReady();
    }

    record WorkflowView(String label, JComponent component, Icon icon) {
        @Override
        public String toString() {
            return label;
        }
    }
}
