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
import com.shaft.intellij.settings.ShaftCredentialService;
import com.shaft.intellij.settings.ShaftSettingsState;
import com.shaft.intellij.ui.firstrun.FirstRunWizardPanel;
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

/**
 * Top-level SHAFT IntelliJ tool window: Agent, Workflow, and Execution log.
 */
public final class ShaftToolWindowPanel extends JPanel implements Disposable {
    static final String SURFACE_AGENT = "Agent";
    static final String SURFACE_WORKFLOW = "Workflow";
    static final String SURFACE_LOG = "Execution log";
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
    private FirstRunWizardPanel wizardPanel;
    private boolean rerunWizard;
    private ShaftAssistantPanel assistantPanel;
    private RecorderToolPanel recorderPanel;
    private List<ShaftFeaturePanel> featurePanels = List.of();
    private List<WorkflowView> workflowViews = List.of();
    private ApiRecordingSessionPanel apiRecordingPanel;
    private GuidedWorkflowPanel guidedWorkflowPanel;
    private JLabel workflowSelectorLabel;
    private DesignStagePanel designStagePanel;
    private AutomationStagePanel automationStagePanel;
    private ReportingStagePanel reportingStagePanel;
    private JBTabbedPane automationTabs;
    private JBTabbedPane reportingTabs;
    private JPanel moreToolsPanel;
    private ExecutionLogPanel executionLogPanel;
    private JPanel workflowBody;
    private final PluginCommandConsole commandConsole = new PluginCommandConsole();

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
        if (this.deepReadinessProbe == null) {
            throw new IllegalArgumentException("deep readiness probe");
        }
        this.assistantChatState = assistantChatState;
        LIVE_PANELS.add(this);
        if (opensWizard(settings, false)) {
            int saved = settings.firstRunWizardStep;
            showWizard(saved < 1 || saved > 5 ? 1 : saved);
        } else {
            showMainView();
        }
    }

    /**
     * Incomplete setup shows the wizard. A verified MCP install is migrated to wizard-complete.
     * Re-run ({@code rerun}) shows the wizard again without clearing a completed flag.
     */
    public static boolean opensWizard(ShaftSettingsState.Settings settings, boolean rerun) {
        if (settings == null || rerun) {
            return true;
        }
        if (settings.firstRunWizardCompleted) {
            return false;
        }
        if (settings.mcpReady()) {
            settings.firstRunWizardCompleted = true;
            return false;
        }
        return true;
    }

    private void showSetupView() {
        rerunWizard = true;
        if (opensWizard(settings, rerunWizard)) {
            showWizard(1);
        }
    }

    private void showWizard(int step) {
        disposeActiveChildren();
        removeAll();
        settings.firstRunWizardStep = step;
        FirstRunWizardPanel wizard = new FirstRunWizardPanel(project, settings, this::onSetupComplete,
                (client, runtime) -> readinessProbe.test(client, runtime), HostToolVersion::snapshot);
        wizard.setSecretStore(this::storeProviderKey);
        wizard.setTerminalOpener((tab, command) ->
                ShaftTerminalCommands.openWithPreparedCommand(project, null, tab, command));
        wizardPanel = wizard;
        preferredFocusComponent = wizard.preferredFocusComponent();
        workflowSelector = null;
        workflowSelectorLabel = null;
        workflowCards = null;
        workflowLayout = null;
        advancedTools = null;
        assistantPanel = null;
        recorderPanel = null;
        designStagePanel = null;
        automationStagePanel = null;
        reportingStagePanel = null;
        automationTabs = null;
        reportingTabs = null;
        moreToolsPanel = null;
        featurePanels = List.of();
        workflowViews = List.of();
        add(wizard, BorderLayout.CENTER);
        revalidate();
        repaint();
    }

    private void storeProviderKey(String name, char[] value) {
        try {
            ShaftCredentialService.getInstance().setApiKeyAsync(name, value);
        } catch (Throwable ignored) {
            // Password Safe is unavailable outside a running IDE.
        }
    }

    private void onSetupComplete() {
        rerunWizard = false;
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
        workflowCards.getAccessibleContext().setAccessibleName("SHAFT view content");
        featurePanels = List.of();
        designStagePanel = null;
        automationStagePanel = null;
        reportingStagePanel = null;
        automationTabs = null;
        reportingTabs = null;
        moreToolsPanel = null;
        advancedTools = null;
        recorderPanel = new RecorderToolPanel(project, settings);
        GuidedWorkflowPanel guided = new GuidedWorkflowPanel(project, this::prefillTool, settings);
        guidedWorkflowPanel = guided;
        executionLogPanel = new ExecutionLogPanel();
        workflowBody = new JPanel(new CardLayout());
        workflowBody.add(guided, "guided");
        workflowBody.add(recorderPanel, "recorder");
        JPanel workflowCard = new JPanel(new BorderLayout(0, JBUI.scale(6)));
        workflowCard.setBorder(JBUI.Borders.empty(8));
        javax.swing.JButton runCommand = new javax.swing.JButton("Run in console");
        runCommand.getAccessibleContext().setAccessibleName("Run workflow command in console");
        runCommand.addActionListener(event -> placeCommand(guided.workflowCommand()));
        workflowCard.add(runCommand, BorderLayout.NORTH);
        workflowCard.add(workflowBody, BorderLayout.CENTER);
        JPanel logCard = new JPanel(new BorderLayout(0, JBUI.scale(6)));
        logCard.add(commandConsole, BorderLayout.NORTH);
        logCard.add(executionLogPanel, BorderLayout.CENTER);
        List<WorkflowView> views = new ArrayList<>();
        views.add(new WorkflowView(SURFACE_AGENT, assistant, ShaftIcons.EDIT));
        views.add(new WorkflowView(SURFACE_WORKFLOW, workflowCard, ShaftIcons.CODE));
        views.add(new WorkflowView(SURFACE_LOG, logCard, ShaftIcons.CHECK));
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
        workflowSelector.setPrototypeDisplayValue(new WorkflowView(SURFACE_LOG, executionLogPanel, ShaftIcons.CHECK));
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
        JLabel label = new JLabel("View");
        label.setFont(label.getFont().deriveFont(Font.BOLD));
        label.setLabelFor(workflowSelector);
        workflowSelectorLabel = label;
        header.add(label);
        header.add(workflowSelector);

        add(header, BorderLayout.NORTH);
        add(workflowCards, BorderLayout.CENTER);
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

    ReportingStagePanel reportingStagePanel() {
        return reportingStagePanel;
    }

    /**
     * Selects Automation/Recorder and starts a live {@code capture_start} recording anchored at a
     * resolved Java caret target (issue #3661 / #5942). A no-op only when the main view has not
     * been built yet (setup overlay still showing).
     *
     * @param context resolved Java caret target the generated code will be anchored at
     */
    public void startRecordingAtTarget(@NotNull JavaTargetContext context) {
        if (guidedWorkflowPanel == null && recorderPanel == null) {
            return;
        }
        selectStage(SURFACE_WORKFLOW);
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
            showRecorderSurface();
        }
        selectStage(SURFACE_WORKFLOW);
    }

    private void showRecorderSurface() {
        if (workflowBody == null || recorderPanel == null) {
            return;
        }
        if (recorderPanel.getParent() != workflowBody) {
            workflowBody.add(recorderPanel, "recorder");
        }
        ((CardLayout) workflowBody.getLayout()).show(workflowBody, "recorder");
    }

    /**
     * Selects the workflow tab that owns the MCP tool template and pre-fills the request.
     *
     * @param toolName MCP tool name
     * @param arguments JSON arguments
     */
    public void placeCommand(@NotNull String command) {
        commandConsole.place(command);
        String directory = project == null ? null : project.getBasePath();
        ShaftTerminalCommands.openWithPreparedCommand(project, directory, "SHAFT", command);
        if (executionLogPanel != null) {
            executionLogPanel.note("$ " + command);
        }
        selectStage(SURFACE_LOG);
    }
    public String placedCommand() { return commandConsole.placed(); }
    ExecutionLogPanel executionLogPanel() { return executionLogPanel; }
    public void showLogChunk(String chunk, boolean failure) {
        if (executionLogPanel != null) {
            executionLogPanel.showChunk(chunk, failure);
            selectStage(SURFACE_LOG);
        }
    }
    public void prefillTool(@NotNull String toolName, @NotNull JsonObject arguments) {
        if (workflowSelector == null) {
            return;
        }
        if (toolName.startsWith("capture_")) {
            selectStage(SURFACE_WORKFLOW);
            return;
        }
        prefillAssistantPrompt("Run " + toolName);
        selectStage(SURFACE_AGENT);
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
        selectStage(SURFACE_WORKFLOW);

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
        selectStage(SURFACE_WORKFLOW);

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
        if (wizardPanel != null) {
            Disposer.dispose(wizardPanel);
            wizardPanel = null;
        }
    }

    /** Upgrade leaves a completed wizard on the main view. */
    public boolean returnToSetupAfterUpgrade() {
        return settings == null || !settings.firstRunWizardCompleted;
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

    private void showSurfaceForComponent(JComponent component) {
        if (showKnownAutomationSurface(component)
                || showTabbedSurface(automationTabs, SURFACE_WORKFLOW, component)
                || showTabbedSurface(reportingTabs, SURFACE_AGENT, component)
                || showMoreSurface(component)) {
            return;
        }
        selectWorkflow(component);
    }

    private boolean showKnownAutomationSurface(JComponent component) {
        if (isRecorderComponent(component)
                || (automationStagePanel != null
                    && Objects.equals(component, automationStagePanel.locatorPlaygroundPanel()))
                || Objects.equals(component, guidedWorkflowPanel)
                || (automationStagePanel != null && Objects.equals(component, automationStagePanel))
                || Objects.equals(component, apiRecordingPanel)) {
            selectStage(SURFACE_WORKFLOW);
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
        String resolved = switch (stage) {
            case SURFACE_WORKFLOW, "More", "Recorder" -> SURFACE_WORKFLOW;
            case SURFACE_LOG -> SURFACE_LOG;
            default -> SURFACE_AGENT;
        };
        for (WorkflowView view : workflowViews) {
            if (view.label().equals(resolved)) {
                workflowSelector.setSelectedItem(view);
                workflowLayout.show(workflowCards, view.label());
                return;
            }
        }
    }

    private JBTabbedPane tabsForStage(String stage) {
        if (SURFACE_WORKFLOW.equals(stage)) {
            return automationTabs;
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
        return stage;
    }

    /** Label of the persisted workflow, or {@code default} when unset. */
    String restoredWorkflowLabel() {
        return ShaftUiState.restoredWorkflowLabel(ShaftUiState.getInstance(project).workflowView());
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
            case SURFACE_WORKFLOW, "Guided", "Recorder", "Automation", "Inspector", "API Recording",
                    "SHAFT Tests", "More" ->
                    new SurfaceTarget(SURFACE_WORKFLOW, SURFACE_WORKFLOW, guidedWorkflowPanel);
            case SURFACE_LOG -> new SurfaceTarget(SURFACE_LOG, SURFACE_LOG, executionLogPanel);
            default -> new SurfaceTarget(SURFACE_AGENT, SURFACE_AGENT, assistantPanel);
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
            case SURFACE_AGENT -> "Ask the agent and review its transcript";
            case SURFACE_WORKFLOW -> "Record a flow and run its command in the console";
            case SURFACE_LOG -> "Read the command placed in the console and its output";
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
