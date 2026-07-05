package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.AnimatedIcon;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
import com.intellij.util.ui.WrapLayout;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftCredentialService;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JProgressBar;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Component;
import java.awt.FontMetrics;
import java.awt.datatransfer.StringSelection;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.CancellationException;

/**
 * SHAFT Assistant chat-style panel.
 */
final class ShaftAssistantPanel extends JPanel {
    private static final int TRANSIENT_STATUS_MILLIS = 2300;
    private static final int MAX_AGENT_CONTEXT_CHARACTERS = 16_000;
    private static final String READY_STATUS = "Try asking me to do something...";
    private static final String SEND_TOOLTIP = "Send assistant prompt (Ctrl+Enter, Command+Enter, or Ctrl+click)";
    private static final String LOCAL_AGENT_STREAMING_HEADER = "_Running local assistant..._";
    private final Project project;
    private final ShaftAssistantChatState chatState;
    private final JComboBox<ShaftAssistantChatState.Session> chatSelector;
    private final JButton newChat;
    private final JComboBox<String> commandAutocomplete;
    private final JButton commandInfo;
    private final JButton contextInfo;
    private final JComboBox<String> mode;
    private final JComboBox<String> providerType;
    private final JComboBox<String> assistantFamily;
    private final JComboBox<String> assistantRuntime;
    private final JComboBox<String> cloudProvider;
    private final JBTextField cloudModel;
    private final JBTextField customCommand;
    private final JPanel cloudKeyPanel;
    private final JPasswordField cloudApiKey;
    private final JButton saveCloudApiKey;
    private final JLabel cloudKeyStatus;
    private final JBCheckBox allowSourceMutation;
    private final JBCheckBox verboseAgentOutput;
    private final JBTextArea prompt;
    private final AssistantTranscriptView transcript;
    private final JButton send;
    private final JButton cancel;
    private final JButton copyLastResponse;
    private final JButton copyRawResponse;
    private final JButton copyTranscript;
    private final JPanel captureReviewPanel;
    private final JLabel captureReviewStatus;
    private final JButton approveCaptureReview;
    private final JButton copyCaptureReview;
    private final JButton dismissCaptureReview;
    private final DefaultListModel<String> timelineModel;
    private final JList<String> timeline;
    private final JPanel timelinePanel;
    private final JButton clearTranscript;
    private final JButton rerunLastPrompt;
    private final JLabel currentAgentConfiguration;
    private final JButton configure;
    private final JProgressBar progress;
    private final JLabel status;
    private final ShaftSettingsState.Settings settings;
    private final Runnable configureFlow;
    private String lastResponse = "";
    private String lastRawResponse = "";
    private String lastPrompt = "";
    private ShaftMcpInvocation currentInvocation;
    private Timer transientStatusTimer;
    private Timer captureStartDiagnosticTimer;
    private boolean running;
    private boolean sendCancelHover;
    private boolean cancelRequested;
    private boolean refreshingChats;
    private boolean updatingCommandAutocomplete;
    private int localAgentStreamToken;
    private int activeLocalAgentStreamToken = -1;
    private int killedLocalAgentStreamToken = -1;
    private StringBuilder localAgentOutput;
    private final List<ToolEvidence> toolEvidence = new ArrayList<>();
    private String activeCaptureRecordingPath = AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH;
    private String activePlaywrightRecordingPath = AssistantCommand.DEFAULT_PLAYWRIGHT_RECORDING_PATH;
    private RecordingBackend activeRecordingBackend = RecordingBackend.WEBDRIVER;
    private CaptureReview pendingCaptureReview;
    private boolean generateCaptureReviewAfterStop;
    private boolean captureReviewGenerationRunning;
    private boolean captureIntegrationRunning;
    private List<AssistantCommand.ToolCall> currentToolSequence = List.of();
    private StringBuilder sequenceMarkdown;
    private StringBuilder sequenceRawOutput;
    private JPopupMenu contextPopup;

    ShaftAssistantPanel(Project project) {
        this(project, ShaftSettingsState.getInstance().getState());
    }

    ShaftAssistantPanel(Project project, @NotNull ShaftSettingsState.Settings settings) {
        this(project, settings, chatState(project));
    }

    ShaftAssistantPanel(Project project,
                        @NotNull ShaftSettingsState.Settings settings,
                        @NotNull ShaftAssistantChatState chatState) {
        this(project, settings, chatState, null);
    }

    ShaftAssistantPanel(Project project,
                        @NotNull ShaftSettingsState.Settings settings,
                        @NotNull ShaftAssistantChatState chatState,
                        Runnable setupFlow) {
        super(new BorderLayout(6, 6));
        this.project = project;
        this.settings = settings;
        this.chatState = chatState;
        this.configureFlow = setupFlow;
        setBorder(JBUI.Borders.empty(8));

        chatSelector = new JComboBox<>();
        chatSelector.getAccessibleContext().setAccessibleName("Assistant chat");
        chatSelector.setRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list,
                                                          Object value,
                                                          int index,
                                                          boolean isSelected,
                                                          boolean cellHasFocus) {
                JLabel label = (JLabel) super.getListCellRendererComponent(
                        list, value, index, isSelected, cellHasFocus);
                String title = value instanceof ShaftAssistantChatState.Session session
                        ? session.toString()
                        : String.valueOf(value == null ? "" : value);
                int availableWidth = index >= 0 ? list.getWidth() : chatSelector.getWidth();
                if (availableWidth <= 0) {
                    availableWidth = JBUI.scale(240);
                }
                label.setText(trimChatTitleForWidth(title, label.getFontMetrics(label.getFont()),
                        Math.max(32, availableWidth - JBUI.scale(38))));
                label.setToolTipText(title);
                label.setBorder(JBUI.Borders.empty(2, 6));
                return label;
            }
        });
        chatSelector.addActionListener(event -> switchChat());
        newChat = button("New chat", "Start a new Assistant chat", event -> newChat());
        ShaftIconButtons.apply(newChat, ShaftIcons.ADD);
        commandAutocomplete = new JComboBox<>(new DefaultComboBoxModel<>(commandItems()));
        commandAutocomplete.setEditable(true);
        commandAutocomplete.setSelectedItem("");
        commandAutocomplete.setPrototypeDisplayValue("/record-mobile inspector Android recordings/inspector.json");
        commandAutocomplete.setRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list,
                                                          Object value,
                                                          int index,
                                                          boolean isSelected,
                                                          boolean cellHasFocus) {
                JLabel label = (JLabel) super.getListCellRendererComponent(
                        list, value, index, isSelected, cellHasFocus);
                label.setText(commandPickerText(String.valueOf(value == null ? "" : value)));
                label.setBorder(JBUI.Borders.empty(3, 6));
                return label;
            }
        });
        commandAutocomplete.setPreferredSize(JBUI.size(220, ShaftIconButtons.SIZE));
        commandAutocomplete.setMinimumSize(JBUI.size(150, ShaftIconButtons.SIZE));
        commandAutocomplete.getAccessibleContext().setAccessibleName("Assistant command autocomplete");
        commandAutocomplete.setToolTipText("Insert /guide, /browser, /record, /doctor, and other tested commands");
        if (commandAutocomplete.getEditor().getEditorComponent() instanceof JTextComponent editor) {
            editor.getAccessibleContext().setAccessibleName("Assistant command autocomplete text");
            editor.setToolTipText("Insert a tested SHAFT command");
            editor.getDocument().addDocumentListener(new DocumentListener() {
                @Override
                public void insertUpdate(DocumentEvent event) {
                    scheduleCommandFilter(editor);
                }

                @Override
                public void removeUpdate(DocumentEvent event) {
                    scheduleCommandFilter(editor);
                }

                @Override
                public void changedUpdate(DocumentEvent event) {
                    scheduleCommandFilter(editor);
                }
            });
        }
        commandAutocomplete.addActionListener(event -> insertSelectedCommand());
        updatingCommandAutocomplete = true;
        try {
            commandAutocomplete.setSelectedItem("/");
        } finally {
            updatingCommandAutocomplete = false;
        }
        commandInfo = button("Commands", "SHAFT command hints",
                event -> showLocalResponse(AssistantCommand.commandHelp()));
        commandInfo.getAccessibleContext().setAccessibleDescription(
                "Shows the supported SHAFT Assistant command families in the command menu.");
        ShaftIconButtons.apply(commandInfo, ShaftIcons.HELP);
        commandInfo.setToolTipText(AssistantCommand.commandTooltip());
        contextInfo = button("Context", "Assistant context suggestions",
                event -> showContextSuggestions('@'));
        contextInfo.getAccessibleContext().setAccessibleDescription(
                "Shows workflow and project context insertions for the Assistant prompt.");
        ShaftIconButtons.apply(contextInfo, ShaftIcons.ADD);
        contextInfo.setToolTipText("Insert @workflow and #project context");
        mode = combo("Assistant mode", "ASK", "PLAN", "AGENT");
        mode.setSelectedItem(normalize(settings.defaultAutobotMode, "ASK"));
        mode.setToolTipText("Ask answers, Plan outlines steps, Agent can run local CLI tasks");
        providerType = combo("Assistant provider type", "LOCAL", "CLOUD");
        providerType.setSelectedItem(normalize(settings.assistantProviderType, "LOCAL"));
        providerType.setToolTipText("Use Local for CLI agents; Cloud for provider Ask and Plan");
        assistantFamily = combo("Assistant family", "CODEX", "CLAUDE", "COPILOT");
        assistantFamily.setSelectedItem(resolveFamily(settings));
        assistantFamily.setToolTipText("Local assistant client");
        assistantRuntime = combo("Assistant runtime", "CLI", "IDE_PLUGIN", "DESKTOP_APP");
        assistantRuntime.setSelectedItem(normalize(settings.assistantRuntime, "CLI"));
        assistantRuntime.setToolTipText("How the selected local assistant is installed");
        currentAgentConfiguration = new JLabel();
        currentAgentConfiguration.getAccessibleContext().setAccessibleName("Current agent configuration");
        currentAgentConfiguration.getAccessibleContext().setAccessibleDescription(
                "Read-only assistant agent configuration from the completed MCP setup flow.");
        cloudProvider = combo("Assistant cloud provider", "openai", "anthropic", "gemini", "github");
        cloudProvider.setSelectedItem(normalizeLower(settings.cloudProvider, "openai"));
        cloudModel = new JBTextField();
        cloudModel.setColumns(16);
        cloudModel.getEmptyText().setText("model");
        cloudModel.getAccessibleContext().setAccessibleName("Assistant cloud model");
        cloudModel.setToolTipText("Optional provider model override");
        cloudModel.setText(settings.cloudModel == null ? "" : settings.cloudModel);
        customCommand = new JBTextField();
        customCommand.setColumns(18);
        customCommand.getEmptyText().setText("Optional local agent command");
        customCommand.getAccessibleContext().setAccessibleName("Optional local agent command");
        customCommand.setToolTipText("Use only when the selected local CLI needs a custom command");

        cloudApiKey = new JPasswordField(16);
        cloudApiKey.getAccessibleContext().setAccessibleName("Assistant cloud API key");
        saveCloudApiKey = new JButton("Save key");
        saveCloudApiKey.getAccessibleContext().setAccessibleName("Save Assistant cloud API key");
        ShaftIconButtons.apply(saveCloudApiKey, ShaftIcons.CHECK);
        saveCloudApiKey.addActionListener(event -> saveCloudApiKey());
        cloudKeyStatus = new JLabel();
        cloudKeyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        cloudKeyPanel.add(cloudKeyStatus);
        cloudKeyPanel.add(cloudApiKey);
        cloudKeyPanel.add(saveCloudApiKey);

        allowSourceMutation = new JBCheckBox("Allow source edits");
        allowSourceMutation.getAccessibleContext().setAccessibleName("Approve source mutation for Agent mode");
        allowSourceMutation.setToolTipText("Enable only when Agent mode should edit local source files");
        verboseAgentOutput = new JBCheckBox("Verbose");
        verboseAgentOutput.getAccessibleContext().setAccessibleName("Show verbose agent output");
        verboseAgentOutput.setToolTipText("Show live local agent output instead of only the final result");
        prompt = new JBTextArea(6, 40);
        prompt.getAccessibleContext().setAccessibleName("Assistant prompt");
        prompt.getAccessibleContext().setAccessibleDescription(
                "Ask for help, choose a tested command, or request guarded local Agent work.");
        prompt.getEmptyText().setText("Ask SHAFT, or type / for commands");
        prompt.setLineWrap(true);
        prompt.setWrapStyleWord(true);
        transcript = new AssistantTranscriptView(project);
        if (!chatState.activeMarkdown().isBlank()) {
            transcript.setMessages(chatState.activeMessages());
            lastPrompt = latestUserPrompt();
        }
        status = new JLabel(READY_STATUS);
        status.getAccessibleContext().setAccessibleName("Assistant status");
        status.getAccessibleContext().setAccessibleDescription(READY_STATUS);
        status.setToolTipText(READY_STATUS);
        status.setFont(status.getFont().deriveFont(Math.max(10.0F, status.getFont().getSize2D() - 1.0F)));
        status.setPreferredSize(JBUI.size(260, status.getPreferredSize().height));
        status.setMinimumSize(JBUI.size(220, status.getPreferredSize().height));
        status.setVisible(false);
        progress = new JProgressBar();
        progress.setIndeterminate(true);
        progress.getAccessibleContext().setAccessibleName("Assistant thinking spinner");
        progress.setPreferredSize(JBUI.size(88, 12));
        progress.setVisible(false);

        send = button("Send", "Send assistant prompt", event -> {
            if (running) {
                cancelOrKillCurrent();
            } else {
                send(project);
            }
        });
        ShaftIconButtons.apply(send, ShaftIcons.SEND);
        ShaftIconButtons.widen(send, 64);
        send.setToolTipText(SEND_TOOLTIP);
        bindSendHover();
        cancel = button("Cancel", "Cancel assistant request", event -> cancelOrKillCurrent());
        ShaftIconButtons.apply(cancel, ShaftIcons.CANCEL);
        cancel.setEnabled(false);
        copyLastResponse = button("Copy response", "Copy last assistant response", event -> copyLastResponse());
        ShaftIconButtons.apply(copyLastResponse, ShaftIcons.COPY);
        copyLastResponse.setEnabled(false);
        copyRawResponse = button("Copy raw", "Copy last raw assistant response", event -> copyRawResponse());
        ShaftIconButtons.apply(copyRawResponse, ShaftIcons.CODE);
        copyRawResponse.setEnabled(false);
        copyTranscript = button("Copy all", "Copy assistant transcript",
                event -> copy(exportTranscriptWithEvidence(), "Copied transcript"));
        ShaftIconButtons.apply(copyTranscript, ShaftIcons.COPY);
        captureReviewStatus = new JLabel("Capture review ready");
        captureReviewStatus.getAccessibleContext().setAccessibleName("Capture review status");
        approveCaptureReview = button("Approve", "Approve Capture review", event -> approvePendingCaptureReview());
        ShaftIconButtons.apply(approveCaptureReview, ShaftIcons.CHECK);
        copyCaptureReview = button("Copy review", "Copy Capture review", event -> copyPendingCaptureReview());
        ShaftIconButtons.apply(copyCaptureReview, ShaftIcons.COPY);
        dismissCaptureReview = button("Dismiss", "Dismiss Capture review", event -> dismissPendingCaptureReview());
        ShaftIconButtons.apply(dismissCaptureReview, ShaftIcons.CANCEL);
        JPanel captureReviewActions = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 0));
        captureReviewActions.add(approveCaptureReview);
        captureReviewActions.add(copyCaptureReview);
        captureReviewActions.add(dismissCaptureReview);
        captureReviewPanel = new JPanel(new BorderLayout(8, 0));
        captureReviewPanel.getAccessibleContext().setAccessibleName("Capture review approval");
        captureReviewPanel.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createEtchedBorder(),
                JBUI.Borders.empty(6)));
        captureReviewPanel.add(captureReviewStatus, BorderLayout.CENTER);
        captureReviewPanel.add(captureReviewActions, BorderLayout.EAST);
        captureReviewPanel.setVisible(false);
        timelineModel = new DefaultListModel<>();
        timeline = new JList<>(timelineModel);
        timeline.getAccessibleContext().setAccessibleName("Assistant execution timeline");
        timeline.getAccessibleContext().setAccessibleDescription(
                "Status timeline for the current Assistant or MCP request.");
        timeline.setFocusable(false);
        timeline.setVisibleRowCount(3);
        addTimeline("Ready");
        clearTranscript = button("Clear", "Clear assistant transcript", event -> clearTranscript());
        ShaftIconButtons.apply(clearTranscript, ShaftIcons.CLEAR);
        rerunLastPrompt = button("Rerun", "Rerun last assistant prompt", event -> rerun(project));
        ShaftIconButtons.apply(rerunLastPrompt, ShaftIcons.RERUN);
        rerunLastPrompt.setEnabled(false);
        this.configure = button("Configure", "Open SHAFT MCP setup", event -> openSetup());
        ShaftIconButtons.apply(this.configure, ShaftIcons.SETTINGS);

        mode.addActionListener(event -> updateControlVisibility());
        providerType.addActionListener(event -> updateControlVisibility());
        assistantRuntime.addActionListener(event -> updateControlVisibility());
        cloudProvider.addActionListener(event -> updateControlVisibility());
        bindKeyboard(project);
        bindContextInsertion();

        JPanel transcriptPanel = new JPanel(new BorderLayout(4, 4));
        transcriptPanel.add(transcript, BorderLayout.CENTER);
        JPanel transcriptStatus = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        transcriptStatus.add(progress);
        transcriptStatus.add(status);
        JPanel transcriptBottom = new JPanel(new BorderLayout(4, 4));
        transcriptBottom.add(captureReviewPanel, BorderLayout.NORTH);
        timelinePanel = new JPanel(new BorderLayout(2, 2));
        JLabel timelineLabel = new JLabel("Run timeline");
        timelineLabel.setLabelFor(timeline);
        timelinePanel.add(timelineLabel, BorderLayout.NORTH);
        JBScrollPane timelineScroll = new JBScrollPane(timeline);
        timelineScroll.setPreferredSize(JBUI.size(240, 58));
        timelinePanel.add(timelineScroll, BorderLayout.CENTER);
        JPanel transcriptCenterChrome = new JPanel(new BorderLayout(4, 4));
        transcriptCenterChrome.add(timelinePanel, BorderLayout.CENTER);
        transcriptBottom.add(transcriptCenterChrome, BorderLayout.CENTER);
        transcriptBottom.add(transcriptStatus, BorderLayout.SOUTH);
        transcriptPanel.add(transcriptBottom, BorderLayout.SOUTH);

        JPanel chatRow = new JPanel(new BorderLayout(6, 0));
        chatRow.add(chatSelector, BorderLayout.CENTER);
        chatRow.add(newChat, BorderLayout.EAST);
        JPanel header = new JPanel(new BorderLayout(4, 4));
        header.getAccessibleContext().setAccessibleName("Assistant chat header");
        header.add(new JLabel("SHAFT"), BorderLayout.NORTH);
        header.add(chatRow, BorderLayout.CENTER);

        JPanel actionRow = wrapRow();
        actionRow.add(copyLastResponse);
        actionRow.add(copyRawResponse);
        actionRow.add(copyTranscript);
        actionRow.add(clearTranscript);
        actionRow.add(rerunLastPrompt);
        actionRow.add(cancel);

        JPanel routeRow = wrapRow();
        routeRow.add(mode);
        routeRow.add(providerType);
        routeRow.add(assistantFamily);
        routeRow.add(assistantRuntime);
        routeRow.add(currentAgentConfiguration);
        routeRow.add(configure);
        routeRow.add(customCommand);
        routeRow.add(cloudProvider);
        routeRow.add(cloudModel);
        routeRow.add(allowSourceMutation);
        routeRow.add(verboseAgentOutput);

        JPanel commandActions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        commandActions.add(commandAutocomplete);
        commandActions.add(commandInfo);
        commandActions.add(contextInfo);
        JPanel sendActions = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0));
        sendActions.add(send);
        JPanel promptActions = new JPanel(new BorderLayout(6, 0));
        promptActions.add(commandActions, BorderLayout.CENTER);
        promptActions.add(sendActions, BorderLayout.EAST);

        JPanel composerFooter = new JPanel(new BorderLayout(4, 4));
        composerFooter.add(routeRow, BorderLayout.CENTER);
        composerFooter.add(promptActions, BorderLayout.SOUTH);

        JPanel composer = new JPanel(new BorderLayout(4, 4));
        composer.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createEtchedBorder(),
                JBUI.Borders.empty(6)));
        JBScrollPane promptScroll = new JBScrollPane(prompt);
        promptScroll.setMinimumSize(JBUI.size(320, 108));
        promptScroll.setPreferredSize(JBUI.size(560, 120));
        composer.add(promptScroll, BorderLayout.CENTER);
        composer.add(cloudKeyPanel, BorderLayout.WEST);
        composer.add(composerFooter, BorderLayout.SOUTH);

        JPanel south = new JPanel(new BorderLayout(4, 4));
        south.add(actionRow, BorderLayout.NORTH);
        south.add(composer, BorderLayout.CENTER);

        JPanel north = new JPanel(new BorderLayout(4, 4));
        north.add(setupNotice(project, settings), BorderLayout.NORTH);
        north.add(header, BorderLayout.CENTER);
        add(north, BorderLayout.NORTH);
        add(transcriptPanel, BorderLayout.CENTER);
        add(south, BorderLayout.SOUTH);
        refreshChatSelector();
        showPendingAgentGuidanceOptimizationPrompt();
        updateControlVisibility();
    }

    JComponent preferredFocusComponent() {
        return prompt;
    }

    static boolean requiresMcpSetup(AssistantCommand.Invocation invocation, boolean mcpConfigured) {
        return invocation != null && invocation.requiresMcpConfiguration() && !mcpConfigured;
    }

    static String agentGuidanceOptimizationPrompt(ShaftSettingsState.Settings settings) {
        String family = resolveFamily(settings);
        String surfaces = switch (family) {
            case "CLAUDE" -> "CLAUDE.md, AGENTS.md, .agents/skills/**, .memory/**";
            case "COPILOT" -> ".github/copilot-instructions.md, AGENTS.md, .github/instructions/**, .github/skills/**, .memory/**";
            default -> "AGENTS.md, .codex/config.toml, .agents/skills/**, .memory/**";
        };
        return """
                Audit and optimize this using shaft-engine test automation project checkout's agent guidance for %s.

                Use SHAFT MCP tools before proposing guidance edits:
                - shaft_guide_search for official user-guide facts and URLs.
                - test_automation_scenarios for workflow expectations.
                - test_code_guardrails_check for code-generation guardrails.

                Review only these guidance and memory surfaces: %s.
                Keep AGENTS.md canonical, host adapters thin, and memories durable, evidence-backed, and non-duplicative.
                Do not edit product code, tests, workflows, manifests, dependencies, generated reports, binaries, or secrets without explicit user approval.

                If source edits are not enabled, return a concise patch plan only. If edits are enabled, make the smallest guidance or memory updates and rerun `py -3 scripts/ci/validate_agent_setup.py --skip-external` on Windows or `python3 scripts/ci/validate_agent_setup.py --skip-external` elsewhere.
                """.formatted(ShaftUiLabels.friendly(family), surfaces).strip();
    }

    private void showPendingAgentGuidanceOptimizationPrompt() {
        if (!settings.agentGuidanceOptimizationPromptPending || !mcpReady(settings)) {
            return;
        }
        settings.agentGuidanceOptimizationPromptPending = false;
        mode.setSelectedItem("PLAN");
        allowSourceMutation.setSelected(false);
        prompt.setText(agentGuidanceOptimizationPrompt(settings));
        prompt.setCaretPosition(0);
        setStatus("Review setup optimization prompt");
        updateControlVisibility();
    }

    private void resetTimeline(String firstStep) {
        timelineModel.clear();
        addTimeline(firstStep);
    }

    private void addTimeline(String step) {
        if (step == null || step.isBlank()) {
            return;
        }
        if (!timelineModel.isEmpty() && step.equals(timelineModel.lastElement())) {
            return;
        }
        timelineModel.addElement(step);
        while (timelineModel.size() > 8) {
            timelineModel.remove(0);
        }
        timeline.ensureIndexIsVisible(timelineModel.size() - 1);
        updateActionChrome();
    }

    List<ContextSuggestion> contextSuggestionsForTest(char trigger) {
        return contextSuggestions(trigger, project, openFileContext(project));
    }

    private List<ContextSuggestion> contextSuggestions(
            char trigger,
            Project project,
            AssistantCommand.OpenFileContext openFileContext) {
        if (trigger == '@') {
            return workflowContextSuggestions();
        }
        if (trigger == '/') {
            return commandContextSuggestions();
        }
        if (trigger == '#') {
            return projectContextSuggestions(project, openFileContext);
        }
        return List.of();
    }

    private static List<ContextSuggestion> commandContextSuggestions() {
        return AssistantCommand.commandHints().stream()
                .map(hint -> new ContextSuggestion(hint.canonical(), hint.example()))
                .toList();
    }

    private static List<ContextSuggestion> workflowContextSuggestions() {
        return List.of(
                new ContextSuggestion("@workflow:record-web", "/record-web https://example.com"),
                new ContextSuggestion("@workflow:record-mobile",
                        "/record-mobile inspector Android recordings/inspector.json"),
                new ContextSuggestion("@workflow:partner", "/partner "),
                new ContextSuggestion("@workflow:codegen", "/codegen "),
                new ContextSuggestion("@workflow:doctor", "/doctor "),
                new ContextSuggestion("@tool:guide-search", "/guide "),
                new ContextSuggestion("@tool:guardrails", "/guardrails "),
                new ContextSuggestion("@project:create-or-upgrade", "/project "));
    }

    private static List<ContextSuggestion> projectContextSuggestions(
            Project project,
            AssistantCommand.OpenFileContext openFileContext) {
        List<ContextSuggestion> suggestions = new ArrayList<>();
        if (openFileContext != null && openFileContext.present()) {
            suggestions.add(new ContextSuggestion("#file:" + fileName(openFileContext.path()),
                    "#file:" + openFileContext.path() + " "));
        }
        addProjectArtifact(suggestions, project, "#allure-results", "target/allure-results ",
                "target", "allure-results");
        addProjectArtifact(suggestions, project, "#shaft-traces", "target/shaft-traces ",
                "target", "shaft-traces");
        addProjectArtifact(suggestions, project, "#recordings", "recordings ",
                "recordings");
        return suggestions;
    }

    private void bindContextInsertion() {
        prompt.addKeyListener(new KeyAdapter() {
            @Override
            public void keyTyped(KeyEvent event) {
                char trigger = event.getKeyChar();
                if (trigger == '@' || trigger == '#' || trigger == '/') {
                    SwingUtilities.invokeLater(() -> showContextSuggestions(trigger));
                }
            }
        });
    }

    private void showContextSuggestions(char trigger) {
        hideContextPopup();
        List<ContextSuggestion> suggestions = contextSuggestionsForTest(trigger);
        if (suggestions.isEmpty()) {
            setStatus(trigger == '#' ? "No project context available"
                    : trigger == '/' ? "No SHAFT commands available"
                    : "No Assistant context available");
            return;
        }
        if (!prompt.isShowing()) {
            return;
        }
        contextPopup = new JPopupMenu("Assistant context suggestions");
        contextPopup.getAccessibleContext().setAccessibleName("Assistant context suggestions");
        for (ContextSuggestion suggestion : suggestions) {
            JMenuItem item = new JMenuItem(suggestion.label());
            item.getAccessibleContext().setAccessibleName("Insert " + suggestion.label());
            item.addActionListener(event -> insertContextSuggestion(trigger, suggestion));
            contextPopup.add(item);
        }
        contextPopup.show(prompt, JBUI.scale(8), Math.max(JBUI.scale(18), prompt.getHeight() - JBUI.scale(4)));
    }

    private void insertContextSuggestion(char trigger, ContextSuggestion suggestion) {
        hideContextPopup();
        int caret = prompt.getCaretPosition();
        String text = prompt.getText();
        int start = caret > 0 && caret <= text.length() && text.charAt(caret - 1) == trigger ? caret - 1 : caret;
        if (start < caret) {
            prompt.replaceRange(suggestion.insertion(), start, caret);
        } else {
            prompt.insert(suggestion.insertion(), caret);
        }
        prompt.setCaretPosition(start + suggestion.insertion().length());
        prompt.requestFocusInWindow();
        setStatus("Inserted " + suggestion.label());
    }

    private void hideContextPopup() {
        if (contextPopup != null) {
            contextPopup.setVisible(false);
            contextPopup = null;
        }
    }

    private static void addProjectArtifact(
            List<ContextSuggestion> suggestions,
            Project project,
            String label,
            String insertion,
            String firstSegment,
            String... moreSegments) {
        if (project == null || project.getBasePath() == null || project.getBasePath().isBlank()) {
            return;
        }
        Path candidate = Path.of(project.getBasePath(), firstSegment).resolve(Path.of("", moreSegments));
        if (Files.exists(candidate)) {
            suggestions.add(new ContextSuggestion(label, insertion));
        }
    }

    private static String fileName(String path) {
        if (path == null || path.isBlank()) {
            return "current";
        }
        int slash = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
        return slash >= 0 && slash + 1 < path.length() ? path.substring(slash + 1) : path;
    }

    private void send(Project project) {
        String text = prompt.getText().trim();
        if (text.isBlank()) {
            setStatus("Enter a prompt");
            return;
        }
        if (usesCloud() && !hasSelectedCloudKey()) {
            setStatus("Enter " + ShaftUiLabels.friendly(cloudProvider.getSelectedItem()) + " key");
            updateCloudKeyStatus();
            return;
        }
        lastPrompt = text;
        rerunLastPrompt.setEnabled(true);
        resetTimeline("Prompt received");
        AssistantCommand.Selection route = selectedRoute();
        boolean agentMode = "AGENT".equals(String.valueOf(mode.getSelectedItem()));
        String selectedMode = String.valueOf(mode.getSelectedItem());
        String workingDirectory = project == null || project.getBasePath() == null ? "" : project.getBasePath();
        String conversationContext = conversationContextForPrompt();
        boolean approvingCaptureReview = pendingCaptureReview != null && AssistantCommand.isCaptureApproval(text);
        AssistantCommand.Invocation invocation = approvingCaptureReview
                ? AssistantCommand.approvedCaptureIntegration(
                route,
                workingDirectory,
                customCommand.getText(),
                pendingCaptureReview.markdown(),
                pendingCaptureReview.rawResult())
                : AssistantCommand.fromPrompt(
                text,
                route,
                selectedMode,
                workingDirectory,
                customCommand.getText(),
                allowSourceMutation.isSelected(),
                openFileContext(project),
                conversationContext);
        invocation = routeNaturalStopToActiveRecorder(text, invocation);
        append("user", AssistantMarkdown.normalizeMarkdown(text), "");
        if (!approvingCaptureReview && AssistantCommand.requiresAgentModeForMcp(text, selectedMode, invocation)) {
            showResponse("This request needs MCP tool access. Switch to **Agent** mode, then send it again.",
                    "");
            addTimeline("Failed");
            setRunning(false, "Switch to Agent mode");
            return;
        }
        if (agentMode
                && !approvingCaptureReview
                && requiresSourceEditApprovalBeforeSend(
                        agentMode,
                        route.cloud(),
                        allowSourceMutation.isSelected(),
                        customCommand.getText(),
                        text,
                        conversationContext)) {
            showResponse("To let the agent make source edits, please enable **Allow source edits** before sending.",
                    "");
            addTimeline("Failed");
            setRunning(false, "Approve source edits");
            return;
        }
        prompt.setText("");
        if (invocation.isLocal()) {
            addTimeline("Completed");
            showLocalResponse(invocation.localResponse());
            return;
        }
        if (requiresMcpSetup(invocation, mcpConfigured())) {
            addTimeline("Failed");
            showLocalResponse("Configure SHAFT MCP in Settings before running this Assistant feature command.");
            setStatus("Configure MCP");
            return;
        }
        if (AssistantLocalAgentRunner.supports(invocation)) {
            captureIntegrationRunning = approvingCaptureReview;
            int streamToken = ++localAgentStreamToken;
            addTimeline("Tool selected: local assistant");
            addTimeline("Running");
            setRunning(true, "Thinking...");
            appendStreamingLocalAgentBubble(streamToken);
            currentInvocation = AssistantLocalAgentRunner.start(invocation, output -> ApplicationManager.getApplication().invokeLater(
                    () -> appendLocalAgentOutput(streamToken, output)));
            currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                    () -> showAgentResult(streamToken, result, error)));
            return;
        }
        rememberCaptureInvocation(text, invocation);
        startMcpInvocation(invocation);
    }

    private void startMcpInvocation(AssistantCommand.Invocation invocation) {
        if (invocation.isSequence()) {
            addTimeline("Tool selected: sequence");
            startToolSequence(invocation.toolCalls());
            return;
        }
        addTimeline("Tool selected: " + invocation.toolName());
        addTimeline("Running");
        setRunning(true, "Running " + invocation.toolName() + "...");
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startTool(invocation.toolName(), invocation.arguments());
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showResult(invocation.toolName(), result, error)));
    }

    private void startToolSequence(List<AssistantCommand.ToolCall> toolCalls) {
        currentToolSequence = List.copyOf(toolCalls);
        sequenceMarkdown = new StringBuilder();
        sequenceRawOutput = new StringBuilder();
        runNextSequenceCall(0);
    }

    private void runNextSequenceCall(int index) {
        if (index >= currentToolSequence.size()) {
            addTimeline("Completed");
            setRunning(false, READY_STATUS);
            showResponse("**SHAFT Assistant sequence OK**\n\n" + sequenceMarkdown, sequenceRawOutput.toString());
            clearSequenceState();
            return;
        }
        AssistantCommand.ToolCall toolCall = currentToolSequence.get(index);
        setRunning(true, "Running " + toolCall.toolName() + " (" + (index + 1) + "/" + currentToolSequence.size() + ")...");
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startTool(toolCall.toolName(), toolCall.arguments());
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showSequenceResult(index, toolCall, result, error)));
    }

    private void showSequenceResult(
            int index,
            AssistantCommand.ToolCall toolCall,
            ShaftMcpToolResult result,
            Throwable error) {
        boolean cancelled = error instanceof CancellationException;
        boolean success = error == null && result != null && result.success();
        String output = error != null ? error.getMessage()
                : result == null ? "No result returned."
                : result.output();
        boolean rejectedGeneratedJava = AssistantMarkdown.containsRejectedGeneratedJava(output);
        if (rejectedGeneratedJava) {
            sequenceMarkdown.append("### ")
                    .append(toolCall.toolName())
                    .append(" rejected")
                    .append("\n\n")
                    .append(AssistantMarkdown.fromMcpOutput(toolCall.toolName(), output))
                    .append("\n\n");
            setRunning(false, "Rejected generated code");
            addTimeline("Failed");
            showResponse("**SHAFT Assistant sequence rejected**\n\n" + sequenceMarkdown,
                    sequenceRawOutput.toString());
            clearSequenceState();
            return;
        }
        if (!output.isBlank()) {
            appendToolEvidence(toolCall.toolName(), output);
        }
        String statusText = cancelled ? "cancelled" : success ? "OK" : "failed";
        sequenceMarkdown.append("### ")
                .append(toolCall.toolName())
                .append(" ")
                .append(statusText)
                .append("\n\n")
                .append(AssistantMarkdown.fromMcpOutput(toolCall.toolName(), output))
                .append("\n\n");
        sequenceRawOutput.append("### ")
                .append(toolCall.toolName())
                .append("\n")
                .append(output)
                .append("\n\n");
        if (cancelled || !success) {
            setRunning(false, cancelled ? "Cancelled" : "Failed");
            addTimeline(cancelled ? "Cancelled" : "Failed");
            showResponse("**SHAFT Assistant sequence " + statusText + "**\n\n" + sequenceMarkdown,
                    sequenceRawOutput.toString());
            clearSequenceState();
            return;
        }
        runNextSequenceCall(index + 1);
    }

    private void clearSequenceState() {
        currentToolSequence = List.of();
        sequenceMarkdown = null;
        sequenceRawOutput = null;
    }

    static boolean requiresSourceEditApprovalBeforeSend(
            boolean agentMode,
            boolean cloudRoute,
            boolean allowSourceMutation,
            String customCommand,
            String prompt,
            String conversationContext) {
        return agentMode
                && !cloudRoute
                && !allowSourceMutation
                && customCommand != null
                && !customCommand.isBlank()
                && (promptRequiresSourceMutation(prompt) || continuationRequiresSourceMutation(prompt, conversationContext));
    }

    private static boolean promptRequiresSourceMutation(String text) {
        String lower = text == null ? "" : text.toLowerCase(Locale.ROOT);
        boolean sourceArtifact = containsAny(lower,
                "source", "code", "file", "class", "method", "test", "java", "pom", "gradle",
                "package", "module", "import", "dependency", "readme", "docs", "documentation");
        boolean browserInvestigation = containsAny(lower,
                "browser", "page", "url", "http://", "https://", "open ", "navigate", "visit", "click",
                "type", "inspect", "search", "form", "duckduckgo", "locator", "xpath", "css selector");
        if (browserInvestigation && !sourceArtifact) {
            return false;
        }
        boolean mutationVerb = containsAny(lower,
                "edit", "modify", "refactor", "fix", "implement", "rewrite", "rename", "update", "change");
        return lower.contains("apply patch")
                || lower.contains("write file")
                || lower.contains("source edit")
                || lower.contains("change source")
                || (sourceArtifact && (mutationVerb || lower.contains("add")));
    }

    private static boolean continuationRequiresSourceMutation(String prompt, String conversationContext) {
        return isContinuationPrompt(prompt) && promptRequiresSourceMutation(conversationContext);
    }

    private static boolean isContinuationPrompt(String prompt) {
        String lower = prompt == null ? "" : prompt.trim().toLowerCase(Locale.ROOT);
        return containsAny(lower,
                "try again",
                "retry",
                "continue",
                "go ahead",
                "do it",
                "apply",
                "proceed",
                "make the change",
                "yes");
    }

    private static boolean containsAny(String value, String... needles) {
        for (String needle : needles) {
            if (value.contains(needle)) {
                return true;
            }
        }
        return false;
    }

    private AssistantCommand.Selection selectedRoute() {
        settings.defaultAutobotMode = String.valueOf(mode.getSelectedItem());
        settings.assistantProviderType = String.valueOf(providerType.getSelectedItem());
        settings.assistantFamily = String.valueOf(assistantFamily.getSelectedItem());
        settings.assistantRuntime = String.valueOf(assistantRuntime.getSelectedItem());
        settings.cloudProvider = String.valueOf(cloudProvider.getSelectedItem());
        settings.cloudModel = cloudModel.getText().trim();
        settings.defaultAutobotClient = clientFromFamily(settings.assistantFamily);
        if (usesCloud()) {
            settings.pilotAiProvider = settings.cloudProvider;
            settings.pilotAiModel = settings.cloudModel;
            return AssistantCommand.Selection.cloud(settings.cloudProvider, settings.cloudModel);
        }
        return AssistantCommand.Selection.local(settings.assistantFamily, settings.assistantRuntime);
    }

    private void showResult(String toolName, ShaftMcpToolResult result, Throwable error) {
        boolean cancelled = error instanceof CancellationException;
        boolean success = error == null && result != null && result.success();
        boolean isMcpConnectionCheck = "mcp initialize".equals(toolName);
        setRunning(false, success ? (isMcpConnectionCheck ? "MCP test passed" : READY_STATUS) : "Failed");
        if (isMcpConnectionCheck && success) {
            showTransientStatus("MCP test passed. Ready to chat.");
        }
        if (cancelled) {
            addTimeline("Cancelled");
            if (isRecordingCodeReviewTool(toolName) && captureReviewGenerationRunning) {
                captureReviewGenerationRunning = false;
                clearPendingCaptureReview();
            }
            showResponse("**SHAFT Assistant (" + toolName + " cancelled)**", "");
            setStatus("Cancelled");
            return;
        }
        String output = error != null ? error.getMessage()
                : result == null ? "No result returned."
                : result.output();
        boolean rejectedGeneratedJava = AssistantMarkdown.containsRejectedGeneratedJava(output);
        if (!output.isBlank() && !rejectedGeneratedJava) {
            appendToolEvidence(toolName, output);
        }
        String markdown = AssistantMarkdown.fromMcpOutput(toolName, output);
        if (rejectedGeneratedJava) {
            if (captureReviewGenerationRunning && isRecordingCodeReviewTool(toolName)) {
                captureReviewGenerationRunning = false;
            }
            showResponse("**SHAFT Assistant (" + toolName + " rejected)**\n\n" + markdown, "");
            setStatus("Rejected generated code");
            addTimeline("Failed");
            return;
        }
        if (!success && captureReviewGenerationRunning && isRecordingCodeReviewTool(toolName)) {
            captureReviewGenerationRunning = false;
        }
        if (success && captureReviewGenerationRunning && isRecordingCodeReviewTool(toolName)) {
            captureReviewGenerationRunning = false;
            pendingCaptureReview = new CaptureReview(markdown, output);
            showPendingCaptureReview();
            showResponse("**SHAFT Assistant (" + toolName + " OK)**\n\n"
                    + markdown
                    + "\n\n**Review before writing files.** Send `approve`, `okay`, or `generate` to let the Agent create the actual Page Object Model files.",
                    output);
            setStatus("Awaiting approval");
            addTimeline("Waiting for approval");
            return;
        }
        if (success && generateCaptureReviewAfterStop
                && ("capture_stop".equals(toolName) || "playwright_record_stop".equals(toolName))) {
            stopCaptureStartDiagnostic();
            showResponse("**SHAFT Assistant (" + toolName + " OK)**\n\n" + markdown, output);
            startCaptureCodeReview();
            return;
        }
        if (success && formatUnknownResponse(toolName, output, markdown)) {
            addTimeline("Completed");
            return;
        }
        showResponse("**SHAFT Assistant (" + toolName + (success ? " OK" : " failed") + ")**\n\n"
                + markdown, output);
        addTimeline(success ? "Completed" : "Failed");
        if (success && "capture_start".equals(toolName)) {
            scheduleCaptureStartDiagnostic(output);
        }
    }

    private void showAgentResult(ShaftMcpToolResult result, Throwable error) {
        showAgentResult(-1, result, error);
    }

    private void showAgentResult(int streamToken, ShaftMcpToolResult result, Throwable error) {
        boolean cancelled = error instanceof CancellationException;
        boolean success = error == null && result != null && result.success();
        boolean currentStream = streamToken == activeLocalAgentStreamToken;
        if (streamToken > 0 && streamToken == killedLocalAgentStreamToken) {
            killedLocalAgentStreamToken = -1;
            setRunning(false, "Killed");
            return;
        }
        if (streamToken > 0 && !currentStream && activeLocalAgentStreamToken != -1) {
            return;
        }
        localAgentOutput = null;
        if (currentStream) {
            activeLocalAgentStreamToken = -1;
        }
        setRunning(false, success ? READY_STATUS : "Failed");
        if (captureIntegrationRunning) {
            if (success) {
                clearPendingCaptureReview();
            }
            captureIntegrationRunning = false;
        }
        if (cancelled) {
            addTimeline("Cancelled");
            showAgentCancelled(streamToken, currentStream);
            setStatus("Cancelled");
            return;
        }
        String output = error != null ? error.getMessage()
                : result == null ? "No response returned."
                : result.output();
        boolean rejectedGeneratedJava = AssistantMarkdown.containsRejectedGeneratedJava(output);
        if (!output.isBlank() && !rejectedGeneratedJava) {
            appendToolEvidence("autobot_local_agent_run", output);
        }
        String response = rejectedGeneratedJava
                ? AssistantMarkdown.nativeSeleniumRejectionMarkdown()
                : AssistantMarkdown.normalizeMarkdown(output);
        if (rejectedGeneratedJava) {
            setStatus("Rejected generated code");
            addTimeline("Failed");
        }
        showAgentResponse(streamToken, currentStream, response, rejectedGeneratedJava ? "" : output);
        if (!rejectedGeneratedJava) {
            addTimeline(success ? "Completed" : "Failed");
        }
    }

    private void showAgentCancelled(int streamToken, boolean currentStream) {
        String canceledResponse = "_Cancelled._";
        showAgentResponse(streamToken, currentStream, canceledResponse, "");
    }

    private void showAgentResponse(int streamToken, boolean currentStream, String response, String output) {
        if (currentStream) {
            finishLocalAgentResponse(streamToken, response, output);
        } else {
            showResponse(response, output);
        }
    }

    private void appendStreamingLocalAgentBubble(int streamToken) {
        activeLocalAgentStreamToken = streamToken;
        localAgentOutput = new StringBuilder();
        if (verboseLocalAgentOutput()) {
            append("assistant", LOCAL_AGENT_STREAMING_HEADER, "");
        }
    }

    private void appendLocalAgentOutput(int streamToken, String line) {
        if (streamToken != activeLocalAgentStreamToken || localAgentOutput == null) {
            return;
        }
        if (localAgentOutput.length() > 0) {
            localAgentOutput.append("\n");
        }
        localAgentOutput.append(line == null ? "" : line);
        if (verboseLocalAgentOutput()) {
            replaceLastTranscriptAndChatState("assistant", formatLocalAgentStreamingResponse(localAgentOutput.toString()));
        }
    }

    private void finishLocalAgentResponse(int streamToken, String response, String rawResponse) {
        if (streamToken != activeLocalAgentStreamToken && activeLocalAgentStreamToken != -1) {
            return;
        }
        String displayResponse = withTokenUsage(response, rawResponse);
        if (verboseLocalAgentOutput()) {
            replaceLastTranscriptAndChatState("assistant", displayResponse);
        } else {
            append("assistant", displayResponse, rawResponse);
        }
        lastResponse = displayResponse;
        lastRawResponse = rawResponse == null ? "" : rawResponse;
        copyLastResponse.setEnabled(true);
        copyRawResponse.setEnabled(!lastRawResponse.isBlank());
        updateActionChrome();
    }

    private boolean verboseLocalAgentOutput() {
        return verboseAgentOutput != null && verboseAgentOutput.isSelected();
    }

    private void stopLocalAgentStreaming() {
        if (activeLocalAgentStreamToken > 0) {
            killedLocalAgentStreamToken = activeLocalAgentStreamToken;
        }
        activeLocalAgentStreamToken = -1;
        localAgentOutput = null;
    }

    private void showLocalResponse(String response) {
        setStatus(READY_STATUS);
        showResponse("**SHAFT Assistant**\n\n" + AssistantMarkdown.normalizeMarkdown(response), response);
    }

    private void showResponse(String response, String rawResponse) {
        String displayResponse = withTokenUsage(response, rawResponse);
        lastResponse = displayResponse;
        lastRawResponse = rawResponse == null ? "" : rawResponse;
        copyLastResponse.setEnabled(true);
        copyRawResponse.setEnabled(!lastRawResponse.isBlank());
        append("assistant", displayResponse, rawResponse);
    }

    private String withTokenUsage(String response, String rawResponse) {
        String markdown = response == null ? "" : response.stripTrailing();
        if (markdown.isBlank()
                || LOCAL_AGENT_STREAMING_HEADER.equals(markdown)
                || markdown.toLowerCase(Locale.ROOT).contains("tokens consumed:")) {
            return markdown;
        }
        int tokens = estimatedTokenCount(lastPrompt) + estimatedTokenCount(rawResponse == null || rawResponse.isBlank()
                ? markdown
                : rawResponse);
        return markdown + "\n\n**Tokens consumed:** `" + Math.max(1, tokens) + "` (estimated)";
    }

    private static int estimatedTokenCount(String value) {
        String text = value == null ? "" : value.strip();
        if (text.isBlank()) {
            return 0;
        }
        int characters = text.codePointCount(0, text.length());
        int words = text.split("\\s+").length;
        return Math.max(words, (characters + 3) / 4);
    }

    private void append(String role, String text, String rawResponse) {
        transcript.append(role, text);
        chatState.append(role, text, rawResponse);
        refreshChatSelector();
        updateActionChrome();
    }

    void setRunning(boolean running, String message) {
        boolean wasRunning = this.running;
        this.running = running;
        if (running && !wasRunning) {
            cancelRequested = false;
        }
        send.setEnabled(true);
        chatSelector.setEnabled(!running);
        newChat.setEnabled(!running);
        configure.setEnabled(!running);
        rerunLastPrompt.setEnabled(!running && !lastPrompt.isBlank());
        mode.setEnabled(!running);
        providerType.setEnabled(!running);
        assistantFamily.setEnabled(!running);
        assistantRuntime.setEnabled(!running);
        cloudProvider.setEnabled(!running);
        cloudModel.setEnabled(!running);
        customCommand.setEnabled(!running);
        commandAutocomplete.setEnabled(!running);
        allowSourceMutation.setEnabled(!running);
        verboseAgentOutput.setEnabled(!running);
        saveCloudApiKey.setEnabled(!running);
        approveCaptureReview.setEnabled(!running && pendingCaptureReview != null);
        copyCaptureReview.setEnabled(!running && pendingCaptureReview != null);
        dismissCaptureReview.setEnabled(!running && pendingCaptureReview != null);
        commandInfo.setEnabled(!running);
        cancel.setEnabled(running);
        progress.setVisible(running);
        setStatus(message);
        updateSendButtonState();
        updateCancelButtonState();
        stopTransientStatus();
        updateControlVisibility();
        if (!running) {
            currentInvocation = null;
            cancelRequested = false;
            updateCancelButtonState();
        }
    }

    private void bindSendHover() {
        send.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent event) {
                if (running) {
                    sendCancelHover = true;
                    updateSendButtonState();
                }
            }

            @Override
            public void mouseExited(MouseEvent event) {
                if (running) {
                    sendCancelHover = false;
                    updateSendButtonState();
                }
            }
        });
    }

    private void updateSendButtonState() {
        if (!running) {
            sendCancelHover = false;
            send.setIcon(ShaftIcons.SEND);
            send.setToolTipText(SEND_TOOLTIP);
            return;
        }
        if (cancelRequested) {
            send.setIcon(ShaftIcons.CANCEL);
            send.setToolTipText("Kill assistant session");
            return;
        }
        send.setIcon(sendCancelHover ? ShaftIcons.CANCEL : AnimatedIcon.Default.INSTANCE);
        send.setToolTipText(sendCancelHover ? "Cancel assistant request" : "Assistant request running");
    }

    private void updateCancelButtonState() {
        String label = cancelRequested ? "Kill assistant session" : "Cancel assistant request";
        cancel.setToolTipText(label);
        cancel.getAccessibleContext().setAccessibleName(label);
    }

    private void showTransientStatus(String message) {
        stopTransientStatus();
        setStatus(message);
        transientStatusTimer = new Timer(TRANSIENT_STATUS_MILLIS, event -> {
            setStatus(READY_STATUS);
            stopTransientStatus();
        });
        transientStatusTimer.setRepeats(false);
        transientStatusTimer.start();
    }

    private void stopTransientStatus() {
        if (transientStatusTimer != null) {
            transientStatusTimer.stop();
            transientStatusTimer = null;
        }
    }

    private void setStatus(String message) {
        String value = message == null || message.isBlank() ? READY_STATUS : message;
        status.setText(value);
        status.setToolTipText(value);
        status.getAccessibleContext().setAccessibleDescription(value);
        status.setVisible(!READY_STATUS.equals(value));
    }

    private void updateControlVisibility() {
        boolean advanced = settings.advancedUiEnabled;
        if (!advanced && usesCloud()) {
            providerType.setSelectedItem("LOCAL");
        }
        boolean cloud = usesCloud();
        if (cloud && "AGENT".equals(mode.getSelectedItem())) {
            mode.setSelectedItem("PLAN");
        }
        boolean localAgent = !cloud;
        boolean localCli = localAgent && "CLI".equals(assistantRuntime.getSelectedItem());
        boolean lockedRoute = configureFlow != null && mcpConfigured();
        boolean controlsEnabled = !running;
        mode.setVisible(true);
        mode.setEnabled(controlsEnabled);
        providerType.setVisible(advanced && !lockedRoute);
        providerType.setEnabled(controlsEnabled && advanced && !lockedRoute);
        assistantFamily.setVisible(advanced && !lockedRoute && !cloud);
        assistantRuntime.setVisible(advanced && !lockedRoute && !cloud);
        assistantFamily.setEnabled(controlsEnabled && advanced && !lockedRoute);
        assistantRuntime.setEnabled(controlsEnabled && advanced && !lockedRoute);
        currentAgentConfiguration.setText(currentAgentConfigurationText());
        currentAgentConfiguration.setToolTipText(currentAgentConfigurationTooltip());
        currentAgentConfiguration.setVisible(lockedRoute);
        customCommand.setVisible(advanced && !lockedRoute && localCli);
        customCommand.setEnabled(controlsEnabled && advanced && !lockedRoute && localCli);
        cloudProvider.setVisible(advanced && !lockedRoute && cloud);
        cloudProvider.setEnabled(controlsEnabled && advanced && !lockedRoute && cloud);
        cloudModel.setVisible(advanced && !lockedRoute && cloud);
        cloudModel.setEnabled(controlsEnabled && advanced && !lockedRoute && cloud);
        cloudKeyPanel.setVisible(advanced && !lockedRoute && cloud);
        cloudApiKey.setEnabled(controlsEnabled && advanced && !lockedRoute && cloud);
        saveCloudApiKey.setEnabled(controlsEnabled && advanced && !lockedRoute && cloud);
        boolean agentMode = "AGENT".equals(mode.getSelectedItem());
        allowSourceMutation.setVisible(agentMode && localAgent);
        allowSourceMutation.setEnabled(controlsEnabled && agentMode && localAgent);
        verboseAgentOutput.setVisible(localAgent && localCli);
        verboseAgentOutput.setEnabled(controlsEnabled && localAgent && localCli);
        configure.setVisible(lockedRoute);
        configure.setEnabled(controlsEnabled && lockedRoute);
        if (!agentMode || !localAgent) {
            allowSourceMutation.setSelected(false);
        }
        if (!localAgent || !localCli) {
            verboseAgentOutput.setSelected(false);
        }
        if (cloud) {
            updateCloudKeyStatus();
        }
        updateActionChrome();
    }

    private void updateActionChrome() {
        if (copyLastResponse == null || copyRawResponse == null || copyTranscript == null
                || clearTranscript == null || rerunLastPrompt == null || cancel == null
                || timelinePanel == null) {
            return;
        }
        boolean hasResponse = !lastResponse.isBlank();
        boolean hasRawResponse = !lastRawResponse.isBlank();
        boolean hasTranscript = !transcript.markdown().isBlank() || !toolEvidence.isEmpty();
        boolean canRerun = !lastPrompt.isBlank();
        copyLastResponse.setVisible(hasResponse);
        copyLastResponse.setEnabled(hasResponse);
        copyRawResponse.setVisible(hasRawResponse);
        copyRawResponse.setEnabled(hasRawResponse);
        copyTranscript.setVisible(hasTranscript);
        copyTranscript.setEnabled(hasTranscript);
        clearTranscript.setVisible(hasTranscript);
        clearTranscript.setEnabled(hasTranscript && !running);
        rerunLastPrompt.setVisible(canRerun);
        rerunLastPrompt.setEnabled(canRerun && !running);
        cancel.setVisible(running);
        cancel.setEnabled(running);
        timelinePanel.setVisible(running || timelineModel.size() > 1);
        timelinePanel.revalidate();
    }

    private void insertSelectedCommand() {
        if (updatingCommandAutocomplete) {
            return;
        }
        String selected = String.valueOf(commandAutocomplete.getSelectedItem());
        String command = commandInsertion(selected);
        if (command.isBlank()) {
            filterCommandItems(selected);
            return;
        }
        prompt.replaceSelection(command + " ");
        prompt.requestFocusInWindow();
        updatingCommandAutocomplete = true;
        try {
            commandAutocomplete.setModel(new DefaultComboBoxModel<>(commandItems()));
            commandAutocomplete.setSelectedItem("/");
        } finally {
            updatingCommandAutocomplete = false;
        }
    }

    private void filterCommandItems(String prefix) {
        String typed = prefix == null ? "" : prefix.trim();
        String lower = typed.toLowerCase(Locale.ROOT);
        String[] items = AssistantCommand.commandHints().stream()
                .map(AssistantCommand.CommandHint::canonical)
                .filter(command -> lower.isBlank()
                        || "/".equals(lower)
                        || command.toLowerCase(Locale.ROOT).startsWith(lower))
                .toArray(String[]::new);
        updatingCommandAutocomplete = true;
        try {
            commandAutocomplete.setModel(new DefaultComboBoxModel<>(items.length == 0 ? commandItems() : items));
            commandAutocomplete.getEditor().setItem(typed.isBlank() ? "/" : typed);
        } finally {
            updatingCommandAutocomplete = false;
        }
    }

    private void scheduleCommandFilter(JTextComponent editor) {
        if (updatingCommandAutocomplete) {
            return;
        }
        SwingUtilities.invokeLater(() -> {
            if (!updatingCommandAutocomplete) {
                filterCommandItems(editor.getText());
            }
        });
    }

    private void updateCloudKeyStatus() {
        String provider = String.valueOf(cloudProvider.getSelectedItem());
        String keyName = providerKeyName(provider);
        boolean stored = !keyName.isBlank() && ShaftCredentialService.getInstance().hasApiKey(keyName);
        String providerLabel = ShaftUiLabels.friendly(provider);
        cloudKeyStatus.setText(stored ? providerLabel + " key stored" : "Enter " + providerLabel + " key");
        cloudApiKey.setVisible(!stored);
        saveCloudApiKey.setVisible(!stored);
    }

    private void saveCloudApiKey() {
        String keyName = providerKeyName(String.valueOf(cloudProvider.getSelectedItem()));
        if (keyName.isBlank() || cloudApiKey.getPassword().length == 0) {
            setStatus("Enter provider key");
            return;
        }
        ShaftCredentialService.getInstance().setApiKey(keyName, cloudApiKey.getPassword());
        cloudApiKey.setText("");
        settings.passProviderApiKeysToMcp = true;
        updateCloudKeyStatus();
        setStatus("Saved key");
    }

    private boolean hasSelectedCloudKey() {
        String keyName = providerKeyName(String.valueOf(cloudProvider.getSelectedItem()));
        return !keyName.isBlank() && ShaftCredentialService.getInstance().hasApiKey(keyName);
    }

    private void bindKeyboard(Project project) {
        Action cancelAction = new AbstractAction() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent event) {
                if (running) {
                    cancelOrKillCurrent();
                }
            }
        };
        prompt.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK), "send");
        prompt.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.META_DOWN_MASK), "send");
        prompt.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "cancel");
        getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
                .put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "cancel");
        prompt.getActionMap().put("send", new AbstractAction() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent event) {
                send(project);
            }
        });
        prompt.getActionMap().put("cancel", cancelAction);
        getActionMap().put("cancel", cancelAction);
        prompt.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent event) {
                if (event.getButton() == MouseEvent.BUTTON1
                        && (event.getModifiersEx() & InputEvent.CTRL_DOWN_MASK) != 0) {
                    send(project);
                }
            }
        });
    }

    private void copyLastResponse() {
        if (!lastResponse.isBlank()) {
            copy(lastResponse, "Copied response");
        }
    }

    private void copyRawResponse() {
        if (!lastRawResponse.isBlank()) {
            copy(lastRawResponse, "Copied raw response");
        }
    }

    private String exportTranscriptWithEvidence() {
        String transcriptText = transcript.markdown();
        String evidenceText = exportToolEvidence();
        if (evidenceText == null || evidenceText.isBlank()) {
            return transcriptText;
        }
        return transcriptText + "\n\n" + evidenceText;
    }

    private void appendToolEvidence(String toolName, String evidence) {
        if (evidence == null || evidence.isBlank()) {
            return;
        }
        toolEvidence.add(new ToolEvidence(
                toolName == null || toolName.isBlank() ? "tool" : toolName.trim(),
                evidence.strip(),
                Instant.now().toString()));
    }

    private String exportToolEvidence() {
        if (toolEvidence.isEmpty()) {
            return "";
        }
        StringBuilder export = new StringBuilder("## Tool evidence\n\n");
        for (ToolEvidence evidence : toolEvidence) {
            export.append("### ").append(evidence.toolName()).append(" (").append(evidence.createdAt()).append(")\n\n")
                    .append(fencedCodeBlock(evidence.payload()))
                    .append("\n\n");
        }
        return export.toString().trim();
    }

    private void clearTranscript() {
        chatState.clearActiveSession();
        toolEvidence.clear();
        clearPendingCaptureReview();
        stopCaptureStartDiagnostic();
        generateCaptureReviewAfterStop = false;
        captureReviewGenerationRunning = false;
        captureIntegrationRunning = false;
        transcript.clear();
        lastResponse = "";
        lastRawResponse = "";
        lastPrompt = "";
        copyLastResponse.setEnabled(false);
        copyRawResponse.setEnabled(false);
        rerunLastPrompt.setEnabled(false);
        refreshChatSelector();
        setStatus("Cleared");
        updateActionChrome();
    }

    private void newChat() {
        chatState.newSession();
        toolEvidence.clear();
        clearPendingCaptureReview();
        stopCaptureStartDiagnostic();
        generateCaptureReviewAfterStop = false;
        captureReviewGenerationRunning = false;
        captureIntegrationRunning = false;
        refreshChatSelector();
        transcript.clear();
        prompt.setText("");
        lastResponse = "";
        lastRawResponse = "";
        lastPrompt = "";
        copyLastResponse.setEnabled(false);
        copyRawResponse.setEnabled(false);
        rerunLastPrompt.setEnabled(false);
        setStatus("New chat");
        updateActionChrome();
    }

    private void switchChat() {
        if (refreshingChats) {
            return;
        }
        Object selected = chatSelector.getSelectedItem();
        if (selected instanceof ShaftAssistantChatState.Session session) {
            chatState.activate(session.id);
            toolEvidence.clear();
            clearPendingCaptureReview();
            stopCaptureStartDiagnostic();
            generateCaptureReviewAfterStop = false;
            captureReviewGenerationRunning = false;
            captureIntegrationRunning = false;
            restoreTranscript();
            setStatus("Chat loaded");
        }
    }

    private AssistantCommand.Invocation routeNaturalStopToActiveRecorder(String promptText, AssistantCommand.Invocation invocation) {
        if (activeRecordingBackend == RecordingBackend.PLAYWRIGHT
                && "capture_stop".equals(invocation.toolName())
                && AssistantCommand.isStopRecording(promptText)) {
            return AssistantCommand.stopPlaywrightRecording();
        }
        return invocation;
    }

    private void rememberCaptureInvocation(String promptText, AssistantCommand.Invocation invocation) {
        if ("capture_start".equals(invocation.toolName())) {
            activeRecordingBackend = RecordingBackend.WEBDRIVER;
            activeCaptureRecordingPath = string(invocation.arguments(), "outputPath",
                    AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH);
            clearPendingCaptureReview();
            stopCaptureStartDiagnostic();
            generateCaptureReviewAfterStop = false;
            captureReviewGenerationRunning = false;
            return;
        }
        if ("playwright_record_start".equals(invocation.toolName())) {
            activeRecordingBackend = RecordingBackend.PLAYWRIGHT;
            activePlaywrightRecordingPath = string(
                    invocation.arguments(), "outputPath", AssistantCommand.DEFAULT_PLAYWRIGHT_RECORDING_PATH);
            clearPendingCaptureReview();
            generateCaptureReviewAfterStop = false;
            captureReviewGenerationRunning = false;
            return;
        }
        if (("capture_stop".equals(invocation.toolName()) || "playwright_record_stop".equals(invocation.toolName()))
                && AssistantCommand.isStopRecording(promptText)) {
            if ("playwright_record_stop".equals(invocation.toolName())) {
                activeRecordingBackend = RecordingBackend.PLAYWRIGHT;
            }
            stopCaptureStartDiagnostic();
            generateCaptureReviewAfterStop = true;
        }
    }

    private void startCaptureCodeReview() {
        generateCaptureReviewAfterStop = false;
        captureReviewGenerationRunning = true;
        setRunning(true, "Generating review code...");
        RecordingBackend reviewBackend = activeRecordingBackend;
        AssistantCommand.Invocation invocation = recordingCodeReviewInvocation(reviewBackend);
        activeRecordingBackend = RecordingBackend.WEBDRIVER;
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startTool(invocation.toolName(), invocation.arguments());
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showResult(invocation.toolName(), result, error)));
    }

    private AssistantCommand.Invocation recordingCodeReviewInvocation(RecordingBackend backend) {
        boolean playwright = backend == RecordingBackend.PLAYWRIGHT;
        return AssistantCommand.Invocation.tool(
                playwright ? "playwright_recording_code_blocks" : "capture_code_blocks",
                playwright
                        ? AssistantCommand.playwrightCodeReview(activePlaywrightRecordingPath)
                        : AssistantCommand.captureCodeReview(activeCaptureRecordingPath));
    }

    private static boolean isRecordingCodeReviewTool(String toolName) {
        return "capture_code_blocks".equals(toolName) || "playwright_recording_code_blocks".equals(toolName);
    }

    private void showPendingCaptureReview() {
        if (pendingCaptureReview == null) {
            captureReviewPanel.setVisible(false);
            return;
        }
        captureReviewStatus.setText(captureReviewSummary(pendingCaptureReview.markdown()));
        approveCaptureReview.setEnabled(!running);
        copyCaptureReview.setEnabled(!running);
        dismissCaptureReview.setEnabled(!running);
        captureReviewPanel.setVisible(true);
        revalidate();
        repaint();
    }

    private void approvePendingCaptureReview() {
        if (pendingCaptureReview == null || running) {
            return;
        }
        prompt.setText("approve");
        send(project);
    }

    private void copyPendingCaptureReview() {
        if (pendingCaptureReview != null) {
            copy(pendingCaptureReview.markdown(), "Copied capture review");
        }
    }

    private void dismissPendingCaptureReview() {
        clearPendingCaptureReview();
        setStatus(READY_STATUS);
    }

    private void clearPendingCaptureReview() {
        pendingCaptureReview = null;
        if (captureReviewPanel != null) {
            approveCaptureReview.setEnabled(false);
            copyCaptureReview.setEnabled(false);
            dismissCaptureReview.setEnabled(false);
            captureReviewPanel.setVisible(false);
            revalidate();
            repaint();
        }
    }

    private void scheduleCaptureStartDiagnostic(String startOutput) {
        stopCaptureStartDiagnostic();
        if (project == null || !mcpConfigured()) {
            return;
        }
        String expectedOutputPath = activeCaptureRecordingPath;
        captureStartDiagnosticTimer = new Timer(1500, event -> {
            stopCaptureStartDiagnostic();
            if (running || !expectedOutputPath.equals(activeCaptureRecordingPath)) {
                return;
            }
            ShaftMcpInvocation invocation = ShaftMcpInvocationService.getInstance(project)
                    .startTool("capture_status", new JsonObject());
            invocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                    () -> showCaptureStartDiagnostic(expectedOutputPath, startOutput, result, error)));
        });
        captureStartDiagnosticTimer.setRepeats(false);
        captureStartDiagnosticTimer.start();
    }

    private void stopCaptureStartDiagnostic() {
        if (captureStartDiagnosticTimer != null) {
            captureStartDiagnosticTimer.stop();
            captureStartDiagnosticTimer = null;
        }
    }

    private void showCaptureStartDiagnostic(
            String expectedOutputPath,
            String startOutput,
            ShaftMcpToolResult result,
            Throwable error) {
        if (error != null || result == null || !result.success()) {
            return;
        }
        JsonObject statusJson = AssistantMarkdown.jsonObjectFromMcpOutput(result.output());
        if (statusJson == null || activeCaptureState(string(statusJson, "state", ""))) {
            return;
        }
        JsonObject startJson = AssistantMarkdown.jsonObjectFromMcpOutput(startOutput);
        String outputPath = string(statusJson, "outputPath", expectedOutputPath);
        if (outputPath.isBlank()) {
            outputPath = expectedOutputPath;
        }
        String markdown = captureStartDiagnosticMarkdown(statusJson, startJson, outputPath);
        appendToolEvidence("capture_status", result.output());
        showResponse("**Capture diagnostic**\n\n" + markdown, result.output());
        setStatus("Capture stopped");
    }

    private void rerun(Project project) {
        if (!lastPrompt.isBlank()) {
            prompt.setText(lastPrompt);
            send(project);
        }
    }

    private void cancelOrKillCurrent() {
        if (currentInvocation != null) {
            if (cancelRequested) {
                stopLocalAgentStreaming();
                currentInvocation.kill();
                setStatus("Killing...");
                addTimeline("Killed");
            } else {
                currentInvocation.cancel();
                cancelRequested = true;
                setStatus("Cancelling...");
                addTimeline("Cancelled");
            }
            updateSendButtonState();
            updateCancelButtonState();
        }
    }

    private static AssistantCommand.OpenFileContext openFileContext(Project project) {
        if (project == null) {
            return AssistantCommand.OpenFileContext.empty();
        }
        FileEditorManager manager = FileEditorManager.getInstance(project);
        Editor editor = manager.getSelectedTextEditor();
        if (editor == null) {
            return AssistantCommand.OpenFileContext.empty();
        }
        VirtualFile[] selectedFiles = manager.getSelectedFiles();
        String path = selectedFiles.length == 0 || selectedFiles[0] == null ? "" : selectedFiles[0].getPath();
        String selectedText = editor.getSelectionModel().getSelectedText();
        return new AssistantCommand.OpenFileContext(
                path,
                editor.getDocument().getText(),
                selectedText == null ? "" : selectedText);
    }

    private void openSetup() {
        if (configureFlow != null) {
            configureFlow.run();
        }
    }

    private boolean formatUnknownResponse(String toolName, String output, String fallbackMarkdown) {
        if (!AssistantMarkdown.shouldFormatWithAgent(toolName, output) || project == null || !mcpConfigured()) {
            return false;
        }
        if (usesCloud() && !hasSelectedCloudKey()) {
            return false;
        }
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                AssistantMarkdown.formatterPrompt(toolName, output),
                selectedRoute(),
                "ASK",
                project.getBasePath() == null ? "" : project.getBasePath(),
                customCommand.getText(),
                false);
        if (invocation.isLocal()) {
            return false;
        }
        setRunning(true, "Formatting response...");
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startTool(invocation.toolName(), invocation.arguments());
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showFormattedUnknownResponse(toolName, output, fallbackMarkdown, invocation.toolName(), result, error)));
        return true;
    }

    private void showFormattedUnknownResponse(String originalToolName,
                                              String rawOutput,
                                              String fallbackMarkdown,
                                              String formatterToolName,
                                              ShaftMcpToolResult result,
                                              Throwable error) {
        boolean success = error == null && result != null && result.success();
        setRunning(false, success ? "Formatted" : READY_STATUS);
        String markdown = fallbackMarkdown;
        if (success) {
            String formatted = AssistantMarkdown.fromMcpOutput(formatterToolName, result.output());
            if (!formatted.isBlank()) {
                markdown = formatted;
            }
        }
        showResponse("**SHAFT Assistant (" + originalToolName + " OK)**\n\n" + markdown, rawOutput);
    }

    private void refreshChatSelector() {
        refreshingChats = true;
        try {
            DefaultComboBoxModel<ShaftAssistantChatState.Session> model = new DefaultComboBoxModel<>();
            for (ShaftAssistantChatState.Session session : chatState.sessions()) {
                model.addElement(session);
            }
            chatSelector.setModel(model);
            chatSelector.setSelectedItem(chatState.activeSession());
        } finally {
            refreshingChats = false;
        }
    }

    private void restoreTranscript() {
        if (!chatState.activeMarkdown().isBlank()) {
            transcript.setMessages(chatState.activeMessages());
        } else {
            transcript.clear();
        }
        lastResponse = "";
        lastRawResponse = "";
        lastPrompt = latestUserPrompt();
        copyLastResponse.setEnabled(false);
        copyRawResponse.setEnabled(false);
        rerunLastPrompt.setEnabled(false);
        updateActionChrome();
    }

    private String latestUserPrompt() {
        List<ShaftAssistantChatState.Message> messages = chatState.activeMessages();
        for (int index = messages.size() - 1; index >= 0; index--) {
            ShaftAssistantChatState.Message message = messages.get(index);
            if (message != null && "user".equals(message.role)
                    && message.markdown != null && !message.markdown.isBlank()) {
                return message.markdown.trim();
            }
        }
        return "";
    }

    private String conversationContextForPrompt() {
        List<ShaftAssistantChatState.Message> messages = chatState.activeMessages();
        if (messages.isEmpty()) {
            return "";
        }
        List<String> entries = new ArrayList<>();
        int total = 0;
        for (int index = messages.size() - 1; index >= 0; index--) {
            ShaftAssistantChatState.Message message = messages.get(index);
            if (message == null || message.markdown == null || message.markdown.isBlank()) {
                continue;
            }
            String entry = contextRole(message.role) + ": " + message.markdown.trim();
            int nextTotal = total + entry.length() + 2;
            if (nextTotal > MAX_AGENT_CONTEXT_CHARACTERS && !entries.isEmpty()) {
                break;
            }
            entries.add(0, clipContextEntry(entry, MAX_AGENT_CONTEXT_CHARACTERS));
            total = nextTotal;
        }
        return String.join("\n\n", entries);
    }

    private static String contextRole(String role) {
        return "user".equals(role) ? "User" : "Assistant";
    }

    private static String clipContextEntry(String entry, int maxCharacters) {
        if (entry.length() <= maxCharacters) {
            return entry;
        }
        return entry.substring(0, maxCharacters) + "\n... truncated ...";
    }

    private void copy(String value, String message) {
        if (!value.isBlank()) {
            CopyPasteManager.getInstance().setContents(new StringSelection(value));
            setStatus(message);
        }
    }

    private void replaceLastTranscriptAndChatState(String role, String message) {
        if (message == null || message.isBlank()) {
            return;
        }
        ShaftAssistantChatState.Session active = chatState.activeSession();
        if (active != null && active.messages != null && !active.messages.isEmpty()) {
            ShaftAssistantChatState.Message last = active.messages.get(active.messages.size() - 1);
            last.role = role == null || role.isBlank() ? "assistant" : role.trim().toLowerCase(Locale.ROOT);
            last.markdown = message;
            transcript.replaceLast(last.role, message);
            return;
        }
        append(role, message, "");
    }

    private static String formatLocalAgentStreamingResponse(String output) {
        if (output == null || output.isBlank()) {
            return LOCAL_AGENT_STREAMING_HEADER;
        }
        return LOCAL_AGENT_STREAMING_HEADER + "\n\n" + fencedCodeBlock(output);
    }

    private static String fencedCodeBlock(String content) {
        String text = content == null ? "" : content.stripTrailing();
        String fence = "```";
        while (text.contains(fence)) {
            fence += "`";
        }
        return fence + "text\n" + text + "\n" + fence;
    }

    private static String captureReviewSummary(String markdown) {
        int codeBlocks = count(markdown, "```") / 2;
        boolean warnings = markdown != null && markdown.contains("**Warnings**");
        StringBuilder summary = new StringBuilder("Capture review ready");
        if (codeBlocks > 0) {
            summary.append(": ").append(codeBlocks).append(codeBlocks == 1 ? " code block" : " code blocks");
        }
        if (warnings) {
            summary.append(", warnings included");
        }
        return summary.toString();
    }

    private static String captureStartDiagnosticMarkdown(
            JsonObject statusJson,
            JsonObject startJson,
            String outputPath) {
        String startProcess = startJson == null ? "" : string(startJson, "processId", "");
        String latestProcess = string(statusJson, "processId", "");
        String process = startProcess.isBlank() ? latestProcess : startProcess;
        StringBuilder markdown = new StringBuilder();
        markdown.append("Managed browser capture stopped after start.")
                .append("\n\n")
                .append("- State: `").append(string(statusJson, "state", "unknown")).append("`")
                .append("\n")
                .append("- Recorder process: `").append(process.isBlank() ? "unknown" : process).append("`")
                .append("\n")
                .append("- Output: `").append(outputPath).append("`");
        if (!latestProcess.isBlank() && !latestProcess.equals(process)) {
            markdown.append("\n- Status check process: `").append(latestProcess).append("`");
        }
        JsonElement warnings = statusJson.get("warnings");
        if (warnings != null && warnings.isJsonArray() && !warnings.getAsJsonArray().isEmpty()) {
            markdown.append("\n\n**Warnings**");
            for (JsonElement warning : warnings.getAsJsonArray()) {
                if (warning.isJsonPrimitive()) {
                    markdown.append("\n- ").append(warning.getAsString());
                }
            }
        }
        return markdown.toString();
    }

    private static boolean activeCaptureState(String state) {
        String normalized = state == null ? "" : state.trim().toUpperCase(Locale.ROOT);
        return normalized.equals("STARTING") || normalized.equals("ACTIVE") || normalized.equals("STOPPING");
    }

    private static int count(String value, String needle) {
        if (value == null || value.isBlank() || needle == null || needle.isEmpty()) {
            return 0;
        }
        int matches = 0;
        int index = value.indexOf(needle);
        while (index >= 0) {
            matches++;
            index = value.indexOf(needle, index + needle.length());
        }
        return matches;
    }

    private boolean mcpConfigured() {
        return mcpReady(settings);
    }

    private boolean usesCloud() {
        return "CLOUD".equals(providerType.getSelectedItem());
    }

    private static JPanel setupNotice(Project project, ShaftSettingsState.Settings settings) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        panel.add(new JLabel("Configure SHAFT MCP to run Assistant feature commands."));
        JButton openSettings = new JButton("Open Settings");
        openSettings.getAccessibleContext().setAccessibleName("Open SHAFT settings");
        ShaftIconButtons.apply(openSettings, ShaftIcons.SETTINGS);
        openSettings.addActionListener(event -> {
            if (project != null) {
                ShowSettingsUtil.getInstance().showSettingsDialog(project, "SHAFT");
            }
        });
        panel.add(openSettings);
        panel.setVisible(!mcpReady(settings));
        return panel;
    }

    private static JComboBox<String> combo(String accessibleName, String... values) {
        JComboBox<String> combo = new JComboBox<>(values);
        ShaftUiLabels.applyFriendlyRenderer(combo);
        combo.getAccessibleContext().setAccessibleName(accessibleName);
        return combo;
    }

    private static String[] commandItems() {
        return AssistantCommand.commandHints().stream()
                .map(AssistantCommand.CommandHint::canonical)
                .toArray(String[]::new);
    }

    private static String commandPickerText(String command) {
        if (command == null || command.isBlank()) {
            return "";
        }
        for (AssistantCommand.CommandHint hint : AssistantCommand.commandHints()) {
            if (hint.canonical().equals(command)) {
                String aliases = hint.synonyms().isEmpty()
                        ? ""
                        : "<br><span style='color:#6A737D'>Aliases: "
                        + escapeHtml(String.join(", ", hint.synonyms()))
                        + "</span>";
                return "<html><b>" + escapeHtml(command) + "</b> - "
                        + escapeHtml(hint.summary())
                        + aliases
                        + "<br><span style='color:#6A737D'>"
                        + escapeHtml(hint.example())
                        + "</span></html>";
            }
        }
        return escapeHtml(command);
    }

    private static String escapeHtml(String value) {
        if (value == null || value.isBlank()) {
            return "";
        }
        return value.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
    }

    private static String commandInsertion(String value) {
        String normalized = value == null ? "" : value.trim();
        if (normalized.isBlank()) {
            return "";
        }
        String firstToken = normalized.split("\\s+", 2)[0];
        for (AssistantCommand.CommandHint hint : AssistantCommand.commandHints()) {
            if (hint.canonical().equalsIgnoreCase(firstToken)) {
                return hint.example();
            }
            if (hint.synonyms().stream().anyMatch(alias -> alias.equalsIgnoreCase(firstToken))) {
                return hint.example();
            }
        }
        return "";
    }

    private static JPanel wrapRow() {
        return new JPanel(new WrapLayout(FlowLayout.LEFT, 6, 4));
    }

    static String trimChatTitleForWidth(String title, FontMetrics metrics, int maxWidth) {
        String value = title == null || title.isBlank() ? "New chat" : title.strip();
        if (metrics == null || maxWidth <= 0 || metrics.stringWidth(value) <= maxWidth) {
            return value;
        }
        String ellipsis = "...";
        if (metrics.stringWidth(ellipsis) >= maxWidth) {
            return ellipsis;
        }
        int end = value.length();
        while (end > 0) {
            String candidate = value.substring(0, end).stripTrailing() + ellipsis;
            if (metrics.stringWidth(candidate) <= maxWidth) {
                return candidate;
            }
            end--;
        }
        return ellipsis;
    }

    private static JButton button(String text, String accessibleName, java.awt.event.ActionListener action) {
        JButton button = new JButton(text);
        button.getAccessibleContext().setAccessibleName(accessibleName);
        button.addActionListener(action);
        return button;
    }

    private static ShaftAssistantChatState chatState(Project project) {
        return ShaftAssistantChatState.getInstance(project);
    }

    private static String resolveFamily(ShaftSettingsState.Settings settings) {
        String family = normalize(settings.assistantFamily, "");
        if (!family.isBlank()) {
            return family;
        }
        return switch (normalize(settings.defaultAutobotClient, "CODEX")) {
            case "CLAUDE_CODE" -> "CLAUDE";
            case "COPILOT_CLI" -> "COPILOT";
            default -> "CODEX";
        };
    }

    private static String clientFromFamily(String family) {
        return switch (normalize(family, "CODEX")) {
            case "CLAUDE" -> "CLAUDE_CODE";
            case "COPILOT" -> "COPILOT_CLI";
            default -> "CODEX";
        };
    }

    private static String routeLabel(AssistantCommand.Selection route) {
        if (route.cloud()) {
            return ShaftUiLabels.friendly(route.cloudProvider());
        }
        return route.displayName();
    }

    private static boolean mcpReady(ShaftSettingsState.Settings settings) {
        return settings != null && settings.mcpReady();
    }

    private String currentAgentConfigurationText() {
        if (settings.advancedUiEnabled && "CLOUD".equals(normalize(settings.assistantProviderType, "LOCAL"))) {
            String model = settings.cloudModel == null || settings.cloudModel.isBlank() ? "" : " " + settings.cloudModel.trim();
            return ShaftUiLabels.friendly(normalizeLower(settings.cloudProvider, "openai")) + model;
        }
        return ShaftUiLabels.friendly(resolveFamily(settings)) + " "
                + ShaftUiLabels.friendly(normalize(settings.assistantRuntime, "CLI"));
    }

    private String currentAgentConfigurationTooltip() {
        if (settings.advancedUiEnabled && "CLOUD".equals(normalize(settings.assistantProviderType, "LOCAL"))) {
            String model = settings.cloudModel == null || settings.cloudModel.isBlank() ? "" : " / " + settings.cloudModel.trim();
            return "Agent: Cloud / " + ShaftUiLabels.friendly(normalizeLower(settings.cloudProvider, "openai")) + model;
        }
        return "Agent: Local / " + ShaftUiLabels.friendly(resolveFamily(settings))
                + " / " + ShaftUiLabels.friendly(normalize(settings.assistantRuntime, "CLI"));
    }

    private static String providerKeyName(String provider) {
        return switch (normalizeLower(provider, "")) {
            case "openai" -> "OPENAI_API_KEY";
            case "anthropic" -> "ANTHROPIC_API_KEY";
            case "gemini" -> "GEMINI_API_KEY";
            case "github" -> "GITHUB_TOKEN";
            default -> "";
        };
    }

    private static String normalize(String value, String fallback) {
        String normalized = value == null || value.isBlank() ? fallback : value.trim();
        return normalized.toUpperCase(Locale.ROOT).replace('-', '_').replace(' ', '_');
    }

    private static String normalizeLower(String value, String fallback) {
        String normalized = value == null || value.isBlank() ? fallback : value.trim();
        return normalized.toLowerCase(Locale.ROOT);
    }

    private static String string(JsonObject object, String key, String fallback) {
        JsonElement value = object == null ? null : object.get(key);
        return value != null && value.isJsonPrimitive() ? value.getAsString() : fallback;
    }

    private enum RecordingBackend {
        WEBDRIVER,
        PLAYWRIGHT
    }

    private record ToolEvidence(String toolName, String payload, String createdAt) {
    }

    record ContextSuggestion(String label, String insertion) {
    }

    private record CaptureReview(String markdown, String rawResult) {
    }
}
