package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
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
    private final ShaftMcpSetupPanel.AgentReadinessProbe deepReadinessProbe;
    private ShaftFeaturePanel advancedTools;
    private List<ShaftFeaturePanel> featurePanels = List.of();
    private List<WorkflowView> workflowViews = List.of();
    private ApiRecordingSessionPanel apiRecordingPanel;
    private GuidedWorkflowPanel guidedWorkflowPanel;

    public ShaftToolWindowPanel(@NotNull Project project) {
        this(project, ShaftSettingsState.getInstance().getState());
    }

    ShaftToolWindowPanel(Project project, @NotNull ShaftSettingsState.Settings settings) {
        this(project, settings, AssistantLocalAgentRunner::readiness,
                AssistantLocalAgentRunner::connectionReadiness, ShaftAssistantChatState.getInstance(project));
    }

    ShaftToolWindowPanel(Project project, @NotNull ShaftSettingsState.Settings settings,
                         @NotNull ShaftMcpSetupPanel.AgentReadinessProbe readinessProbe) {
        this(project, settings, readinessProbe, readinessProbe, ShaftAssistantChatState.getInstance(project));
    }

    ShaftToolWindowPanel(Project project,
                         @NotNull ShaftSettingsState.Settings settings,
                         @NotNull ShaftMcpSetupPanel.AgentReadinessProbe readinessProbe,
                         @NotNull ShaftAssistantChatState assistantChatState) {
        this(project, settings, readinessProbe, readinessProbe, assistantChatState);
    }

    ShaftToolWindowPanel(Project project,
                         @NotNull ShaftSettingsState.Settings settings,
                         @NotNull ShaftMcpSetupPanel.AgentReadinessProbe readinessProbe,
                         @NotNull ShaftMcpSetupPanel.AgentReadinessProbe deepReadinessProbe,
                         @NotNull ShaftAssistantChatState assistantChatState) {
        super(new BorderLayout());
        this.project = project;
        this.settings = settings;
        this.readinessProbe = readinessProbe;
        this.deepReadinessProbe = deepReadinessProbe;
        this.assistantChatState = assistantChatState;
        if (mcpReady(settings)) {
            showMainView();
        } else {
            showSetupView();
        }
    }

    private void showSetupView() {
        disposeApiRecordingPanel();
        disposeGuidedWorkflowPanel();
        removeAll();
        ShaftMcpSetupPanel setup = new ShaftMcpSetupPanel(project, settings, this::onSetupComplete,
                readinessProbe, deepReadinessProbe);
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

    private void onSetupComplete() {
        assistantChatState.newSession();
        showMainView();
    }

    private void showMainView() {
        disposeApiRecordingPanel();
        disposeGuidedWorkflowPanel();
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
        GuidedWorkflowPanel guided = new GuidedWorkflowPanel(project, this::prefillTool, settings);
        guidedWorkflowPanel = guided;
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

    /**
     * Re-renders this panel back to the initial setup view, discarding any in-progress workflow
     * state. Used by {@code ShaftPluginResetService} after a factory reset. Callers are responsible
     * for marshaling this onto the EDT.
     */
    public void resetToSetupView() {
        showSetupView();
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

    /**
     * Opens (or reuses) the API Recording tab for the given target URL and MCP
     * {@code capture_api_start} arguments, starting a new polling session.
     *
     * @param targetUrl the URL the recording session targets
     * @param startArguments arguments for the {@code capture_api_start} MCP call
     */
    public void showApiRecordingTab(@NotNull String targetUrl, @NotNull JsonObject startArguments) {
        if (workflowCards == null || workflowLayout == null || !settings.advancedUiEnabled) {
            return;
        }
        disposeApiRecordingPanel();
        apiRecordingPanel = new ApiRecordingSessionPanel(project, targetUrl, null);
        WorkflowView apiRecordingView = new WorkflowView("API Recording", apiRecordingPanel, ShaftIcons.VIEW);
        List<WorkflowView> updated = new ArrayList<>(workflowViews);
        updated.removeIf(view -> "API Recording".equals(view.label()));
        updated.add(apiRecordingView);
        workflowViews = updated;
        workflowCards.add(apiRecordingPanel, apiRecordingView.label());
        workflowSelector.setModel(new DefaultComboBoxModel<>(workflowViews.toArray(new WorkflowView[0])));
        selectWorkflow(apiRecordingPanel);

        ShaftMcpInvocationService.getInstance(project)
                .startTool("capture_api_start", startArguments)
                .future()
                .whenComplete((result, error) -> com.intellij.openapi.application.ApplicationManager.getApplication()
                        .invokeLater(() -> {
                            if (apiRecordingPanel == null) {
                                return;
                            }
                            if (error != null || result == null || !result.success()) {
                                apiRecordingPanel.statusLabel().setText(
                                        "Failed to start recording: "
                                                + (result != null ? result.output() : String.valueOf(error)));
                            }
                        }));
    }

    /**
     * Disposes the current API Recording panel, if any, cancelling its poller.
     */
    private void disposeApiRecordingPanel() {
        if (apiRecordingPanel != null) {
            Disposer.dispose(apiRecordingPanel);
            apiRecordingPanel = null;
        }
    }

    /**
     * Disposes the current Guided workflow panel, if any, cancelling its recorder status poller.
     */
    private void disposeGuidedWorkflowPanel() {
        if (guidedWorkflowPanel != null) {
            Disposer.dispose(guidedWorkflowPanel);
            guidedWorkflowPanel = null;
        }
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
