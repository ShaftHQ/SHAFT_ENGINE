package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.ui.JBSplitter;
import com.intellij.ui.components.JBTabbedPane;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.java.JavaTargetContext;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
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
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.stream.Stream;

/**
 * Top-level SHAFT IntelliJ tool window content: three stages (Design, Automation, Reporting)
 * with a docked Assistant. Issue #5942.
 */
public final class ShaftToolWindowPanel extends JPanel implements Disposable {
    static final String STAGE_DESIGN = "Analysis & Design";
    static final String STAGE_AUTOMATION = "Automation";
    static final String STAGE_REPORTING = "Reporting & Analytics";
    private static final Set<ShaftToolWindowPanel> LIVE_PANELS =
            Collections.synchronizedSet(Collections.newSetFromMap(new WeakHashMap<>()));

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
    private ShaftMcpSetupPanel setupPanel;
    private ShaftAssistantPanel assistantPanel;
    private RecorderToolPanel recorderPanel;
    private List<ShaftFeaturePanel> featurePanels = List.of();
    private List<WorkflowView> workflowViews = List.of();
    private ApiRecordingSessionPanel apiRecordingPanel;
    private GuidedWorkflowPanel guidedWorkflowPanel;
    private JLabel workflowSelectorLabel;
    private DesignStagePanel designStagePanel;
    private AutomationStagePanel automationStagePanel;
    private JBTabbedPane automationTabs;
    private JBTabbedPane reportingTabs;
    private JPanel moreToolsPanel;

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
        LIVE_PANELS.add(this);
        if (mcpReady(settings)) {
            showMainView();
        } else {
            showSetupView();
        }
    }

    private void showSetupView() {
        disposeActiveChildren();
        removeAll();
        ShaftMcpSetupPanel setup = new ShaftMcpSetupPanel(project, settings, this::onSetupComplete,
                readinessProbe, deepReadinessProbe);
        setupPanel = setup;
        preferredFocusComponent = setup.preferredFocusComponent();
        workflowSelector = null;
        workflowSelectorLabel = null;
        workflowCards = null;
        workflowLayout = null;
        advancedTools = null;
        assistantPanel = null;
        recorderPanel = null;
        designStagePanel = null;
        automationStagePanel = null;
        automationTabs = null;
        reportingTabs = null;
        moreToolsPanel = null;
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
        disposeActiveChildren();
        removeAll();
        ShaftAssistantPanel assistant = new ShaftAssistantPanel(project, settings,
                assistantChatState, this::showSetupView);
        assistantPanel = assistant;
        preferredFocusComponent = assistant.preferredFocusComponent();
        workflowLayout = new CardLayout();
        workflowCards = new JPanel(workflowLayout);
        workflowCards.getAccessibleContext().setAccessibleName("SHAFT stage content");
        featurePanels = new ArrayList<>();

        DesignStagePanel design = new DesignStagePanel(project, json -> {
            if (automationStagePanel != null) {
                automationStagePanel.applyHandoffPrefillJson(json);
            }
        });
        designStagePanel = design;

        AutomationStagePanel automation = new AutomationStagePanel(project, this::prefillTool, settings);
        automationStagePanel = automation;
        GuidedWorkflowPanel guided = automation.guidedWorkflowPanel();
        guidedWorkflowPanel = guided;
        RecorderToolPanel recorder = new RecorderToolPanel(project, settings);
        recorderPanel = recorder;
        ShaftFeaturePanel inspectorTools = new ShaftFeaturePanel(project, settings,
                List.of(new ToolCategory("Inspector", ToolTemplates.inspector())));
        ShaftTestsPanel shaftTests = new ShaftTestsPanel(project);
        featurePanels.add(recorder.featurePanel());
        featurePanels.add(inspectorTools);

        // Issue #5957: Live record is the Automation canvas default. Secondary surfaces stay on
        // the same stage tab strip for API recording / Inspector, but Guided+Recorder are folded
        // into Live record; expert raw MCP goes under More when advancedUiEnabled.
        automationTabs = automation.surfaces();
        automationTabs.addChangeListener(event -> persistSelectedWorkflowView());
        if (settings.advancedUiEnabled) {
            automationTabs.addTab("Inspector", ShaftIcons.SEARCH, inspectorTools);
            automationTabs.addTab("SHAFT Tests", ShaftIcons.RERUN, shaftTests);
        }

        ReportingHistoryPanel history = new ReportingHistoryPanel(project);
        ReportingFlakePanel flake = new ReportingFlakePanel(project);
        ReportingSmartTagsPanel smartTags = new ReportingSmartTagsPanel(project);
        ReportingClustersPanel clusters = new ReportingClustersPanel(project);
        ReportingLabelsPanel labels = new ReportingLabelsPanel(project);
        ReportingDoctorPanel doctor = new ReportingDoctorPanel(project);
        ReportingHealPanel heal = new ReportingHealPanel(project);
        EvidenceTriagePanel triage = new EvidenceTriagePanel(project, this::prefillTool);
        VisualBaselinesPanel visualBaselines = new VisualBaselinesPanel(project);
        ShaftFeaturePanel evidenceTools = new ShaftFeaturePanel(project, settings,
                List.of(new ToolCategory("Evidence", Stream.concat(
                        ToolTemplates.doctor().stream(), ToolTemplates.healer().stream()).toList())));
        featurePanels.add(evidenceTools);
        reportingTabs = new JBTabbedPane();
        reportingTabs.getAccessibleContext().setAccessibleName("SHAFT reporting surfaces");
        reportingTabs.addTab(ReportingHistoryPanel.TAB_TITLE, ShaftIcons.CHECK, history);
        reportingTabs.addTab(ReportingFlakePanel.TAB_TITLE, ShaftIcons.VIEW, flake);
        reportingTabs.addTab(ReportingSmartTagsPanel.TAB_TITLE, ShaftIcons.VIEW, smartTags);
        reportingTabs.addTab(ReportingClustersPanel.TAB_TITLE, ShaftIcons.VIEW, clusters);
        reportingTabs.addTab(ReportingLabelsPanel.TAB_TITLE, ShaftIcons.EDIT, labels);
        reportingTabs.addTab(ReportingDoctorPanel.TAB_TITLE, ShaftIcons.VIEW, doctor);
        reportingTabs.addTab(ReportingHealPanel.TAB_TITLE, ShaftIcons.VIEW, heal);
        reportingTabs.addTab("Triage", ShaftIcons.VIEW, triage);
        reportingTabs.addTab("Visual Baselines", ShaftIcons.VIEW, visualBaselines);
        reportingTabs.addTab("Evidence", ShaftIcons.EDIT, evidenceTools);
        reportingTabs.addChangeListener(event -> persistSelectedWorkflowView());

        moreToolsPanel = new JPanel(new BorderLayout());
        moreToolsPanel.getAccessibleContext().setAccessibleName("SHAFT more tools");
        if (settings.advancedUiEnabled) {
            ShaftFeaturePanel projectsTools = new ShaftFeaturePanel(project, settings,
                    List.of(new ToolCategory("Projects", ToolTemplates.projects())));
            advancedTools = new ShaftFeaturePanel(project, settings);
            featurePanels.add(projectsTools);
            featurePanels.add(advancedTools);
            JBTabbedPane moreTabs = new JBTabbedPane();
            moreTabs.addTab("Recorder", ShaftIcons.VIEW, recorder);
            moreTabs.addTab("Projects", ShaftIcons.SETTINGS, projectsTools);
            moreTabs.addTab("Advanced", ShaftIcons.HELP, advancedTools);
            moreToolsPanel.add(moreTabs, BorderLayout.CENTER);
        } else {
            advancedTools = null;
        }

        List<WorkflowView> views = new ArrayList<>();
        views.add(new WorkflowView(STAGE_DESIGN, design, ShaftIcons.EDIT));
        views.add(new WorkflowView(STAGE_AUTOMATION, automation, ShaftIcons.CODE));
        views.add(new WorkflowView(STAGE_REPORTING, reportingTabs, ShaftIcons.CHECK));
        if (settings.advancedUiEnabled) {
            views.add(new WorkflowView("More", moreToolsPanel, ShaftIcons.SETTINGS));
        }
        workflowViews = List.copyOf(views);
        for (WorkflowView view : workflowViews) {
            workflowCards.add(view.component(), view.label());
        }
        workflowSelector = new JComboBox<>(new DefaultComboBoxModel<>(
                workflowViews.toArray(new WorkflowView[0])));
        workflowSelector.getAccessibleContext().setAccessibleName("SHAFT stage selector");
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
                    String description = workflowDescription(workflow.label());
                    if (!description.isBlank()) {
                        label.getAccessibleContext().setAccessibleDescription(description);
                    }
                }
                return label;
            }
        });
        workflowSelector.setPrototypeDisplayValue(new WorkflowView(STAGE_REPORTING, reportingTabs, ShaftIcons.CHECK));
        Dimension selectorSize = workflowSelector.getPreferredSize();
        int selectorHeight = Math.max(30, selectorSize.height);
        workflowSelector.setPreferredSize(JBUI.size(Math.max(180, selectorSize.width), selectorHeight));
        workflowSelector.setMinimumSize(JBUI.size(160, selectorHeight));
        restoreSelectedWorkflowView();
        workflowSelector.addActionListener(event -> {
            showSelectedWorkflow();
            persistSelectedWorkflowView();
        });
        JPanel header = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 2));
        header.setBorder(JBUI.Borders.empty(6, 8, 4, 8));
        JLabel label = new JLabel("Stage");
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        label.setLabelFor(workflowSelector);
        workflowSelectorLabel = label;
        header.add(label);
        header.add(workflowSelector);

        JBSplitter split = new JBSplitter(true, 0.48f);
        split.setFirstComponent(workflowCards);
        split.setSecondComponent(assistant);
        split.getAccessibleContext().setAccessibleName("SHAFT stage and assistant");

        add(header, BorderLayout.NORTH);
        add(split, BorderLayout.CENTER);
        revalidate();
        repaint();
    }

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
     * Package-private test accessor: the retained Guided live-record panel on the Automation
     * canvas, or {@code null} before setup.
     */
    GuidedWorkflowPanel guidedWorkflowPanel() {
        return guidedWorkflowPanel;
    }

    /**
     * Package-private test accessor: the retained Recorder surface, or {@code null} before setup.
     */
    RecorderToolPanel recorderPanel() {
        return recorderPanel;
    }

    DesignStagePanel designStagePanel() {
        return designStagePanel;
    }

    AutomationStagePanel automationStagePanel() {
        return automationStagePanel;
    }

    /**
     * Selects Automation/Recorder and starts a live {@code capture_start} recording anchored at a
     * resolved Java caret target (issue #3661 / #5942). A no-op only when the main view has not
     * been built yet (setup overlay still showing).
     *
     * @param context resolved Java caret target the generated code will be anchored at
     */
    public void startRecordingAtTarget(@NotNull JavaTargetContext context) {
        if (recorderPanel == null && automationStagePanel == null) {
            return;
        }
        String readyIntent = "";
        String readyUrl = "";
        if (automationStagePanel != null) {
            readyIntent = automationStagePanel.readyPackIntent();
            readyUrl = automationStagePanel.readyPackUrl();
            automationStagePanel.showLiveRecord();
        }
        if (recorderPanel != null) {
            recorderPanel.applyReadyPackUrl(readyUrl);
            recorderPanel.startRecordingAtTarget(context, readyIntent);
        }
        showSurface(STAGE_AUTOMATION, AutomationStagePanel.LIVE_RECORD_TAB, guidedWorkflowPanel);
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
            // The Recorder tab's visible WorkflowView component is the composite RecorderToolPanel,
            // not its embedded ShaftFeaturePanel held here for tool-name lookup (issue #3665 part B)
            // -- routing through RecorderToolPanel#prefillTool both keeps that identity match working
            // for selectWorkflow() below and expands its Advanced section for a tool the curated
            // Quick Start section does not surface.
            boolean isRecorderFeaturePanel = recorderPanel != null
                    && Objects.equals(panel, recorderPanel.featurePanel());
            boolean matched = isRecorderFeaturePanel
                    ? recorderPanel.prefillTool(toolName, arguments)
                    : panel.prefillTool(toolName, arguments);
            if (matched) {
                showSurfaceForComponent(isRecorderFeaturePanel ? recorderPanel : panel);
                return;
            }
        }
        ensureMoreTools();
        if (advancedTools != null) {
            advancedTools.prefillTool(toolName, arguments);
            showSurface("More", "Advanced", advancedTools);
        }
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
        addAutomationTab("API Recording", apiRecordingPanel);
        showSurface(STAGE_AUTOMATION, "API Recording", apiRecordingPanel);

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
     * Opens (or reuses) the API Recording tab for a no-browser pure-API session, starting
     * {@code capture_api_start} and populating the pairing panel (proxy port + CA
     * certificate) once it returns (issue #3530 A2).
     *
     * @param headerText title shown above the transactions table (e.g. the target platform)
     * @param startArguments arguments for the {@code capture_api_start} MCP call
     */
    public void showPureApiRecordingTab(@NotNull String headerText, @NotNull JsonObject startArguments) {
        if (workflowCards == null || workflowLayout == null) {
            return;
        }
        disposeApiRecordingPanel();
        apiRecordingPanel = new ApiRecordingSessionPanel(
                project, ApiRecordingSessionPanel.CaptureMode.PURE_API, headerText, null);
        addAutomationTab("API Recording", apiRecordingPanel);
        showSurface(STAGE_AUTOMATION, "API Recording", apiRecordingPanel);

        ShaftMcpInvocationService.getInstance(project)
                .startTool("capture_api_start", startArguments)
                .future()
                .whenComplete((result, error) -> com.intellij.openapi.application.ApplicationManager.getApplication()
                        .invokeLater(() -> applyMobileApiRecordStartResult(result, error)));
    }

    /**
     * Applies the {@code capture_api_start} result to the current API Recording panel: an
     * error/failure surfaces as a status message, otherwise the pairing panel is populated.
     * Split from {@link #showPureApiRecordingTab} (and further split below) to keep each method's
     * branching within PMD's NPath complexity threshold.
     */
    private void applyMobileApiRecordStartResult(ShaftMcpToolResult result, Throwable error) {
        if (apiRecordingPanel == null) {
            return;
        }
        if (error != null || result == null || !result.success()) {
            apiRecordingPanel.statusLabel().setText(
                    "Failed to start recording: " + (result != null ? result.output() : String.valueOf(error)));
            return;
        }
        JsonObject status = AssistantMarkdown.jsonObjectFromMcpOutput(result.output());
        if (status != null) {
            applyPairingInfo(apiRecordingPanel, status);
        }
    }

    /**
     * Extracts the proxy port, CA certificate, and warnings from a {@code MobileApiCaptureStatus}
     * JSON object and hands them to the panel's pairing display.
     */
    private static void applyPairingInfo(ApiRecordingSessionPanel panel, JsonObject status) {
        int proxyPort = status.has("proxyPort") ? status.get("proxyPort").getAsInt() : 0;
        String caCertificatePem = status.has("caCertificatePem") ? status.get("caCertificatePem").getAsString() : "";
        panel.showPairingInfo(proxyPort, caCertificatePem, warningsOf(status));
    }

    /**
     * Reads the {@code warnings} JSON array off a {@code MobileApiCaptureStatus} object.
     */
    private static List<String> warningsOf(JsonObject status) {
        List<String> warnings = new ArrayList<>();
        if (status.has("warnings") && status.get("warnings").isJsonArray()) {
            status.get("warnings").getAsJsonArray().forEach(warning -> warnings.add(warning.getAsString()));
        }
        return warnings;
    }

    /**
     * Disposes the current API Recording panel, if any, cancelling its poller.
     */
    private void disposeApiRecordingPanel() {
        if (apiRecordingPanel != null) {
            if (automationTabs != null) {
                int index = automationTabs.indexOfComponent(apiRecordingPanel);
                if (index >= 0) {
                    automationTabs.removeTabAt(index);
                }
            }
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
     * Disposes every currently active child panel that owns background polling (the API Recording
     * session and the Guided workflow's recorder status poller). Called both on internal view
     * switches ({@link #showSetupView()}/{@link #showMainView()}) and from {@link #dispose()} so
     * that real platform teardown (project close, tool-window content rebuild) tears these down
     * too, instead of only ever running via a manual view switch that real teardown never makes
     * (issue #3619).
     */
    private void disposeActiveChildren() {
        disposeSetupPanel();
        disposeApiRecordingPanel();
        disposeGuidedWorkflowPanel();
        disposeAssistantPanel();
    }

    /** Stops setup-only work before the setup view is replaced or the tool window closes. */
    private void disposeSetupPanel() {
        if (setupPanel != null) {
            Disposer.dispose(setupPanel);
            setupPanel = null;
        }
    }

    /**
     * Disposes the current Assistant panel, if any, killing the local-agent CLI process it may still
     * be running and stopping its output-flush timer (issue #4500). The assistant panel used to be
     * the one child this method dropped without disposing -- {@link #showSetupView()} simply nulls
     * the field -- so a run in flight at project close kept a real Codex/Claude process and one of
     * {@code ShaftPluginExecutor}'s bounded worker threads alive until its own timeout.
     */
    private void disposeAssistantPanel() {
        if (assistantPanel != null) {
            Disposer.dispose(assistantPanel);
            assistantPanel = null;
        }
    }

    /**
     * Wires this panel into the platform's {@code Disposer} tree ({@code
     * ShaftToolWindowFactory#createToolWindowContent} registers it via {@code
     * Content#setDisposer}), so closing the project or rebuilding the tool window's content
     * reliably cancels any in-progress child pollers instead of leaking them (issue #3619).
     */
    @Override
    public void dispose() {
        LIVE_PANELS.remove(this);
        disposeActiveChildren();
    }

    /**
     * Test seam: dispose every tool window still live in this JVM so Guided/Recorder children
     * cannot leak Swing timers across tests (issue #5942, same pattern as
     * {@link ShaftAssistantPanel#disposeLivePanels()}).
     */
    static void disposeLivePanels() {
        List<ShaftToolWindowPanel> panels;
        synchronized (LIVE_PANELS) {
            panels = new ArrayList<>(LIVE_PANELS);
        }
        panels.forEach(ShaftToolWindowPanel::dispose);
    }

    private void showSelectedWorkflow() {
        WorkflowView view = workflowSelector == null ? null : (WorkflowView) workflowSelector.getSelectedItem();
        if (view != null && workflowLayout != null && workflowCards != null) {
            workflowLayout.show(workflowCards, view.label());
        }
    }

    private void addAutomationTab(String title, JComponent component) {
        if (automationTabs == null) {
            return;
        }
        for (int index = 0; index < automationTabs.getTabCount(); index++) {
            if (title.equals(automationTabs.getTitleAt(index))) {
                automationTabs.setComponentAt(index, component);
                return;
            }
        }
        automationTabs.addTab(title, ShaftIcons.VIEW, component);
    }

    private void ensureMoreTools() {
        if (moreToolsPanel == null || workflowCards == null || workflowSelector == null) {
            return;
        }
        if (advancedTools == null) {
            advancedTools = new ShaftFeaturePanel(project, settings);
            featurePanels = new ArrayList<>(featurePanels);
            featurePanels.add(advancedTools);
            moreToolsPanel.removeAll();
            JBTabbedPane moreTabs = new JBTabbedPane();
            moreTabs.addTab("Advanced", ShaftIcons.HELP, advancedTools);
            moreToolsPanel.add(moreTabs, BorderLayout.CENTER);
        }
        boolean hasMore = false;
        for (WorkflowView view : workflowViews) {
            if ("More".equals(view.label())) {
                hasMore = true;
                break;
            }
        }
        if (!hasMore) {
            WorkflowView moreView = new WorkflowView("More", moreToolsPanel, ShaftIcons.SETTINGS);
            List<WorkflowView> updated = new ArrayList<>(workflowViews);
            updated.add(moreView);
            workflowViews = updated;
            workflowCards.add(moreToolsPanel, moreView.label());
            workflowSelector.setModel(new DefaultComboBoxModel<>(workflowViews.toArray(new WorkflowView[0])));
        }
    }

    private void showSurfaceForComponent(JComponent component) {
        if (showKnownAutomationSurface(component)
                || showTabbedSurface(automationTabs, STAGE_AUTOMATION, component)
                || showTabbedSurface(reportingTabs, STAGE_REPORTING, component)
                || showMoreSurface(component)) {
            return;
        }
        selectWorkflow(component);
    }

    private boolean showKnownAutomationSurface(JComponent component) {
        if (isRecorderComponent(component)) {
            if (settings.advancedUiEnabled && moreToolsPanel != null) {
                ensureMoreTools();
                showSurface("More", "Recorder", recorderPanel);
            } else {
                showSurface(STAGE_AUTOMATION, AutomationStagePanel.LIVE_RECORD_TAB, guidedWorkflowPanel);
            }
            return true;
        }
        if (automationStagePanel != null
                && Objects.equals(component, automationStagePanel.locatorPlaygroundPanel())) {
            showSurface(STAGE_AUTOMATION, AutomationStagePanel.LOCATOR_PICKER_TAB,
                    automationStagePanel.locatorPlaygroundPanel());
            return true;
        }
        if (Objects.equals(component, guidedWorkflowPanel)
                || (automationStagePanel != null && Objects.equals(component, automationStagePanel))) {
            showSurface(STAGE_AUTOMATION, AutomationStagePanel.LIVE_RECORD_TAB, guidedWorkflowPanel);
            return true;
        }
        if (Objects.equals(component, apiRecordingPanel)) {
            showSurface(STAGE_AUTOMATION, "API Recording", apiRecordingPanel);
            return true;
        }
        return false;
    }

    private boolean isRecorderComponent(JComponent component) {
        return Objects.equals(component, recorderPanel)
                || (recorderPanel != null && Objects.equals(component, recorderPanel.featurePanel()));
    }

    private boolean showTabbedSurface(JBTabbedPane tabs, String stage, JComponent component) {
        String title = tabTitleOf(tabs, component);
        if (title == null) {
            return false;
        }
        showSurface(stage, title, component);
        return true;
    }

    private boolean showMoreSurface(JComponent component) {
        if (moreToolsPanel == null) {
            return false;
        }
        if (!Objects.equals(component, moreToolsPanel) && !moreToolsPanel.isAncestorOf(component)) {
            return false;
        }
        String more = tabTitleOf(firstTabbedPane(moreToolsPanel), component);
        showSurface("More", more == null ? "Advanced" : more, component);
        return true;
    }

    private void showSurface(String stage, String tabTitle, JComponent component) {
        selectStage(stage);
        JBTabbedPane tabs = tabsForStage(stage);
        if (tabs != null && tabTitle != null) {
            for (int index = 0; index < tabs.getTabCount(); index++) {
                if (tabTitle.equals(tabs.getTitleAt(index))) {
                    tabs.setSelectedIndex(index);
                    persistSelectedWorkflowView();
                    return;
                }
            }
        }
        if (component != null) {
            selectWorkflow(component);
        }
        persistSelectedWorkflowView();
    }

    private void selectStage(String stage) {
        if (workflowSelector == null) {
            return;
        }
        for (WorkflowView view : workflowViews) {
            if (view.label().equals(stage)) {
                workflowSelector.setSelectedItem(view);
                workflowLayout.show(workflowCards, view.label());
                return;
            }
        }
    }

    private JBTabbedPane tabsForStage(String stage) {
        if (STAGE_AUTOMATION.equals(stage)) {
            return automationTabs;
        }
        if (STAGE_REPORTING.equals(stage)) {
            return reportingTabs;
        }
        if ("More".equals(stage)) {
            return firstTabbedPane(moreToolsPanel);
        }
        return null;
    }

    private static JBTabbedPane firstTabbedPane(JComponent root) {
        if (root instanceof JBTabbedPane tabs) {
            return tabs;
        }
        if (root != null) {
            for (Component child : root.getComponents()) {
                if (child instanceof JBTabbedPane tabs) {
                    return tabs;
                }
            }
        }
        return null;
    }

    private static String tabTitleOf(JBTabbedPane tabs, JComponent component) {
        if (tabs == null || component == null) {
            return null;
        }
        for (int index = 0; index < tabs.getTabCount(); index++) {
            Component page = tabs.getComponentAt(index);
            if (Objects.equals(page, component)
                    || (page instanceof JComponent parent && parent.isAncestorOf(component))) {
                return tabs.getTitleAt(index);
            }
        }
        return null;
    }

    String selectedStageLabel() {
        Object selected = workflowSelector == null ? null : workflowSelector.getSelectedItem();
        return selected instanceof WorkflowView view ? view.label() : "";
    }

    String selectedSurfaceLabel() {
        String stage = selectedStageLabel();
        JBTabbedPane tabs = tabsForStage(stage);
        if (tabs != null && tabs.getSelectedIndex() >= 0) {
            return tabs.getTitleAt(tabs.getSelectedIndex());
        }
        return STAGE_DESIGN.equals(stage) ? STAGE_DESIGN : stage;
    }

    /**
     * Restores the last-selected workflow view across IDE restarts (issue #3636), keyed by
     * {@link WorkflowView#label()} -- the same stable string already used as the {@code CardLayout}
     * key for each view. Falls back silently to the existing default (the first item, already
     * selected by the combo's model) when nothing is stored yet, or when the stored key no longer
     * matches any current view (e.g. after a plugin update changes the available workflows). Runs
     * before {@code workflowSelector}'s action listener is attached, so this never double-persists
     * the value it just read; the CardLayout is switched here explicitly since a plain
     * {@code setSelectedItem} call would not do that on its own without the listener.
     */
    private void restoreSelectedWorkflowView() {
        String savedKey = ShaftUiState.getInstance(project).workflowView();
        if (savedKey == null) {
            return;
        }
        SurfaceTarget target = surfaceTarget(savedKey);
        showSurface(target.stage, target.tab, target.component);
    }

    /** Persists the currently selected surface so {@link #restoreSelectedWorkflowView()} can find it next time. */
    private void persistSelectedWorkflowView() {
        String surface = selectedSurfaceLabel();
        if (surface.isBlank()) {
            return;
        }
        // Issue #6014: never re-write the retired Guided key; canonicalize to Live record.
        if ("Guided".equals(surface)) {
            surface = AutomationStagePanel.LIVE_RECORD_TAB;
        }
        ShaftUiState.getInstance(project).setWorkflowView(surface);
    }

    private SurfaceTarget surfaceTarget(String savedKey) {
        return switch (savedKey) {
            case "Assistant", STAGE_DESIGN -> new SurfaceTarget(STAGE_DESIGN, STAGE_DESIGN, designStagePanel);
            case "Guided", AutomationStagePanel.LIVE_RECORD_TAB ->
                    new SurfaceTarget(STAGE_AUTOMATION, AutomationStagePanel.LIVE_RECORD_TAB, guidedWorkflowPanel);
            case AutomationStagePanel.LOCATOR_PICKER_TAB ->
                    new SurfaceTarget(
                            STAGE_AUTOMATION,
                            AutomationStagePanel.LOCATOR_PICKER_TAB,
                            automationStagePanel == null ? null : automationStagePanel.locatorPlaygroundPanel());
            case "Recorder" -> settings.advancedUiEnabled
                    ? new SurfaceTarget("More", "Recorder", recorderPanel)
                    : new SurfaceTarget(STAGE_AUTOMATION, AutomationStagePanel.LIVE_RECORD_TAB, guidedWorkflowPanel);
            case "Inspector" -> new SurfaceTarget(STAGE_AUTOMATION, "Inspector", null);
            case "SHAFT Tests" -> new SurfaceTarget(STAGE_AUTOMATION, "SHAFT Tests", null);
            case "API Recording" -> new SurfaceTarget(STAGE_AUTOMATION, "API Recording", apiRecordingPanel);
            case ReportingHistoryPanel.TAB_TITLE -> new SurfaceTarget(STAGE_REPORTING, ReportingHistoryPanel.TAB_TITLE, null);
            case ReportingFlakePanel.TAB_TITLE -> new SurfaceTarget(STAGE_REPORTING, ReportingFlakePanel.TAB_TITLE, null);
            case ReportingSmartTagsPanel.TAB_TITLE -> new SurfaceTarget(STAGE_REPORTING, ReportingSmartTagsPanel.TAB_TITLE, null);
            case ReportingClustersPanel.TAB_TITLE -> new SurfaceTarget(STAGE_REPORTING, ReportingClustersPanel.TAB_TITLE, null);
            case ReportingLabelsPanel.TAB_TITLE -> new SurfaceTarget(STAGE_REPORTING, ReportingLabelsPanel.TAB_TITLE, null);
            case ReportingDoctorPanel.TAB_TITLE -> new SurfaceTarget(STAGE_REPORTING, ReportingDoctorPanel.TAB_TITLE, null);
            case ReportingHealPanel.TAB_TITLE -> new SurfaceTarget(STAGE_REPORTING, ReportingHealPanel.TAB_TITLE, null);
            case "Triage" -> new SurfaceTarget(STAGE_REPORTING, "Triage", null);
            case "Visual Baselines" -> new SurfaceTarget(STAGE_REPORTING, "Visual Baselines", null);
            case "Evidence" -> new SurfaceTarget(STAGE_REPORTING, "Evidence", null);
            case "Projects", "Advanced", "More" -> new SurfaceTarget("More", savedKey, null);
            case STAGE_AUTOMATION -> new SurfaceTarget(
                    STAGE_AUTOMATION, AutomationStagePanel.LIVE_RECORD_TAB, guidedWorkflowPanel);
            case STAGE_REPORTING -> new SurfaceTarget(STAGE_REPORTING, "Triage", null);
            default -> new SurfaceTarget(STAGE_DESIGN, STAGE_DESIGN, designStagePanel);
        };
    }

    private record SurfaceTarget(String stage, String tab, JComponent component) {
    }

    private void selectWorkflow(JComponent component) {
        for (WorkflowView view : workflowViews) {
            if (Objects.equals(view.component(), component)) {
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
            case STAGE_DESIGN -> "Turn a user story or requirements into reviewable Gherkin, then hand off to Automation";
            case STAGE_AUTOMATION -> "Live record, inspect, run, and generate SHAFT fluent Java";
            case STAGE_REPORTING -> "Analyze Allure, Doctor diagnosis card, flake, unique-error clusters, and heal evidence";
            case "More" -> "Project setup and raw MCP tools";
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
