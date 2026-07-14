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
    private ShaftAssistantPanel assistantPanel;
    private List<ShaftFeaturePanel> featurePanels = List.of();
    private List<WorkflowView> workflowViews = List.of();
    private ApiRecordingSessionPanel apiRecordingPanel;
    private GuidedWorkflowPanel guidedWorkflowPanel;
    private JLabel workflowSelectorLabel;
    private ShaftReadinessSummary readinessSummary;
    private javax.swing.JButton recheckHealth;

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
        workflowSelectorLabel = null;
        workflowCards = null;
        workflowLayout = null;
        advancedTools = null;
        assistantPanel = null;
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
        assistantPanel = assistant;
        preferredFocusComponent = assistant.preferredFocusComponent();
        workflowLayout = new CardLayout();
        workflowCards = new JPanel(workflowLayout);
        workflowCards.getAccessibleContext().setAccessibleName("SHAFT workflow content");
        featurePanels = new ArrayList<>();
        List<WorkflowView> views = new ArrayList<>();
        views.add(new WorkflowView("Assistant", assistant, ShaftIcons.SEND));
        if (settings.advancedUiEnabled) {
            GuidedWorkflowPanel guided = new GuidedWorkflowPanel(project, this::prefillTool, settings);
            guidedWorkflowPanel = guided;
            views.add(new WorkflowView("Guided", guided, ShaftIcons.CODE));
            EvidenceTriagePanel triage = new EvidenceTriagePanel(project, this::prefillTool);
            ShaftTestsPanel shaftTests = new ShaftTestsPanel(project);
            VisualBaselinesPanel visualBaselines = new VisualBaselinesPanel(project);
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
            featurePanels.add(recorderTools);
            featurePanels.add(inspectorTools);
            featurePanels.add(evidenceTools);
            featurePanels.add(projectsTools);
            featurePanels.add(advancedTools);
            views.add(new WorkflowView("Recorder", recorderTools, ShaftIcons.VIEW));
            views.add(new WorkflowView("Inspector", inspectorTools, ShaftIcons.SEARCH));
            views.add(new WorkflowView("Triage", triage, ShaftIcons.CHECK));
            views.add(new WorkflowView("SHAFT Tests", shaftTests, ShaftIcons.RERUN));
            views.add(new WorkflowView("Visual Baselines", visualBaselines, ShaftIcons.VIEW));
            views.add(new WorkflowView("Evidence", evidenceTools, ShaftIcons.EDIT));
            views.add(new WorkflowView("Projects", projectsTools, ShaftIcons.SETTINGS));
            views.add(new WorkflowView("Advanced", advancedTools, ShaftIcons.HELP));
        } else {
            // The Assistant is the product for regular users: it understands recording, code
            // generation, diagnosis, and upgrade intents in plain language. Every specialist
            // view stays behind the explicit expert-mode opt-in because those raw-tool surfaces
            // are unusable without MCP tool knowledge and only dilute first contact.
            advancedTools = null;
        }
        workflowViews = List.copyOf(views);
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
                    // Screen readers announce what each workflow does, not just its short name
                    // (issue #3538 G4).
                    String description = workflowDescription(workflow.label());
                    if (!description.isBlank()) {
                        label.getAccessibleContext().setAccessibleDescription(description);
                    }
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
        workflowSelectorLabel = label;
        header.add(label);
        header.add(workflowSelector);
        header.add(buildHealthChip());
        refreshWorkflowSelectorVisibility();
        add(header, BorderLayout.NORTH);
        add(workflowCards, BorderLayout.CENTER);
        revalidate();
        repaint();
    }

    /**
     * Persistent setup-health chip (issue #3425 A6): always visible in the main-view header, it
     * reflects the last verified MCP state and offers a one-click live re-check. A failed re-check
     * offers reconnecting through the setup view instead of leaving a dead tool window.
     */
    private JComponent buildHealthChip() {
        // Shared readiness summary (issue #3500 O4/A4): MCP, workspace, agent lane, and live
        // recording activity answered by one component with the same words as the setup ready row.
        readinessSummary = new ShaftReadinessSummary(settings, projectBasePath());
        recheckHealth = new javax.swing.JButton("Recheck");
        recheckHealth.getAccessibleContext().setAccessibleName("Recheck SHAFT MCP health");
        recheckHealth.setToolTipText("Run a live SHAFT MCP connection check now");
        recheckHealth.setMargin(JBUI.insets(1, 6));
        recheckHealth.addActionListener(event -> recheckMcpHealth());
        applyHealthState(settings.mcpReady() ? HealthState.VERIFIED : HealthState.UNKNOWN, "");
        JPanel chip = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 0));
        chip.setOpaque(false);
        chip.add(readinessSummary);
        chip.add(recheckHealth);
        return chip;
    }

    private java.nio.file.Path projectBasePath() {
        return project == null || project.getBasePath() == null
                ? null
                : java.nio.file.Path.of(project.getBasePath());
    }

    private void recheckMcpHealth() {
        String command = settings.mcpCommand == null ? "" : settings.mcpCommand.trim();
        if (command.isBlank()) {
            applyHealthState(HealthState.FAILED, "No MCP command configured");
            showSetupView();
            return;
        }
        recheckHealth.setEnabled(false);
        readinessSummary.applyMcpState(ShaftReadinessSummary.McpState.CHECKING, "");
        java.nio.file.Path root = project == null || project.getBasePath() == null
                ? java.nio.file.Path.of(".")
                : java.nio.file.Path.of(project.getBasePath());
        com.shaft.intellij.mcp.ShaftMcpConnectionProbe.test(command, settings, root)
                .whenComplete((result, error) -> com.intellij.openapi.application.ApplicationManager
                        .getApplication().invokeLater(() -> {
                            recheckHealth.setEnabled(true);
                            boolean healthy = error == null && result != null && result.success();
                            applyHealthState(healthy ? HealthState.VERIFIED : HealthState.FAILED,
                                    healthy ? "" : error != null
                                            ? String.valueOf(error.getMessage())
                                            : result == null ? "no result" : result.output());
                        }));
    }

    private void applyHealthState(HealthState state, String detail) {
        readinessSummary.applyMcpState(switch (state) {
            case VERIFIED -> ShaftReadinessSummary.McpState.VERIFIED;
            case FAILED -> ShaftReadinessSummary.McpState.FAILED;
            default -> ShaftReadinessSummary.McpState.UNKNOWN;
        }, detail);
        readinessSummary.applyAgentLane(settings.agentLaneReady);
        readinessSummary.applyWorkspace(projectBasePath());
    }

    private enum HealthState { UNKNOWN, VERIFIED, FAILED }

    private boolean projectArtifactExists(String relativePath) {
        if (project == null || project.getBasePath() == null || project.getBasePath().isBlank()) {
            return false;
        }
        try {
            return java.nio.file.Files.exists(java.nio.file.Path.of(project.getBasePath(), relativePath));
        } catch (RuntimeException invalidPath) {
            return false;
        }
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

    /** Package-private test accessor: the retained Assistant panel, or {@code null} before setup. */
    ShaftAssistantPanel assistantPanel() {
        return assistantPanel;
    }

    /**
     * Selects the workflow tab that owns the MCP tool template and pre-fills the request.
     *
     * @param toolName MCP tool name
     * @param arguments JSON arguments
     */
    public void prefillTool(@NotNull String toolName, @NotNull JsonObject arguments) {
        if (workflowSelector == null) {
            return;
        }
        for (ShaftFeaturePanel panel : featurePanels) {
            if (panel.prefillTool(toolName, arguments)) {
                selectWorkflow(panel);
                return;
            }
        }
        // No existing tab owns this tool: surface the Advanced tools tab on demand — the
        // progressive-disclosure default hides it until a workflow actually needs it (#3425 A4).
        if (advancedTools == null) {
            advancedTools = new ShaftFeaturePanel(project, settings);
            featurePanels = new ArrayList<>(featurePanels);
            featurePanels.add(advancedTools);
            WorkflowView advancedView = new WorkflowView("Advanced", advancedTools, ShaftIcons.HELP);
            List<WorkflowView> updated = new ArrayList<>(workflowViews);
            updated.add(advancedView);
            workflowViews = updated;
            workflowCards.add(advancedTools, advancedView.label());
            workflowSelector.setModel(new DefaultComboBoxModel<>(workflowViews.toArray(new WorkflowView[0])));
            refreshWorkflowSelectorVisibility();
        }
        advancedTools.prefillTool(toolName, arguments);
        selectWorkflow(advancedTools);
    }

    /**
     * Selects the Assistant tab and fills its composer with {@code text} for the user to review and
     * send themselves (issue #3552). The Assistant is the product for regular users, so this is the
     * "act" half of the advancedUiEnabled gate audit: entry points that used to silently no-op or
     * dead-end in a warning while advanced workflows are off now route here instead, landing a
     * ready-to-send plain-language request rather than leaving the user to retype it. A no-op here
     * (main view not yet built, e.g. the setup view is showing) is not a silent dead end: the tool
     * window itself already surfaces the setup panel explaining what to do next.
     *
     * @param text plain-language prompt to prefill
     */
    public void prefillAssistantPrompt(@NotNull String text) {
        if (assistantPanel == null) {
            return;
        }
        assistantPanel.prefillPrompt(text);
        if (workflowSelector != null) {
            selectWorkflow(assistantPanel);
        }
    }

    /**
     * Selects the Assistant tab, runs {@code toolName} against the live MCP connection, and renders
     * the result into the transcript as a read-only diagnosis card -- unlike {@link
     * #prefillAssistantPrompt}, this always executes rather than waiting for the user to review and
     * send (issue #3547 failure-recovery: an automatic post-failure diagnosis, or an explicit
     * "Diagnose"/"Heal" click, must actually produce a diagnosis in default mode, not a prefilled
     * request). A no-op when the main view has not been built yet (setup view still showing), same
     * rationale as {@link #prefillAssistantPrompt}.
     *
     * @param toolName MCP tool name to run
     * @param arguments MCP tool arguments
     */
    public void runAssistantTool(@NotNull String toolName, @NotNull JsonObject arguments) {
        if (assistantPanel == null) {
            return;
        }
        assistantPanel.runToolAndRenderCard(toolName, arguments);
        if (workflowSelector != null) {
            selectWorkflow(assistantPanel);
        }
    }

    /**
     * Opens (or reuses) the API Recording tab for the given target URL and MCP
     * {@code capture_api_start} arguments, starting a new polling session.
     *
     * @param targetUrl the URL the recording session targets
     * @param startArguments arguments for the {@code capture_api_start} MCP call
     */
    public void showApiRecordingTab(@NotNull String targetUrl, @NotNull JsonObject startArguments) {
        if (workflowCards == null || workflowLayout == null) {
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
        refreshWorkflowSelectorVisibility();
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

    /**
     * A selector with one entry is noise: regular users see just the Assistant plus the health
     * chip, and the workflow picker appears only when expert mode or a runtime flow adds real
     * choices.
     */
    private void refreshWorkflowSelectorVisibility() {
        boolean multipleViews = workflowViews.size() > 1;
        if (workflowSelector != null) {
            workflowSelector.setVisible(multipleViews);
        }
        if (workflowSelectorLabel != null) {
            workflowSelectorLabel.setVisible(multipleViews);
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

    /**
     * One-line description of what each workflow selector entry does, announced by screen readers
     * alongside the short label (issue #3538 G4). Blank for any future label added here without a
     * matching case, so a missing entry degrades to "no description" rather than a crash.
     */
    private static String workflowDescription(String label) {
        return switch (label) {
            case "Assistant" -> "Chat with the SHAFT Assistant to record, generate, and diagnose tests";
            case "Guided" -> "Step-by-step guided workflow across recording, code generation, and diagnosis";
            case "Recorder" -> "Record browser or mobile actions into SHAFT test code";
            case "Inspector" -> "Inspect page or app elements and pick resilient locators";
            case "Triage" -> "Review failed test evidence and get suggested fixes";
            case "SHAFT Tests" -> "Run and review SHAFT Engine test suites";
            case "Visual Baselines" -> "Manage and compare visual regression baselines";
            case "Evidence" -> "Doctor and healer tools for failure evidence";
            case "Projects" -> "Create or upgrade SHAFT projects";
            case "Advanced" -> "Raw SHAFT MCP tool catalog for advanced users";
            default -> "";
        };
    }

    record WorkflowView(String label, JComponent component, Icon icon) {
        @Override
        public String toString() {
            return label;
        }
    }
}
