package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
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
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JProgressBar;
import javax.swing.KeyStroke;
import javax.swing.Timer;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.datatransfer.StringSelection;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
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
    private static final String READY_STATUS = "ready";
    private static final String LOCAL_AGENT_STREAMING_HEADER = "_Running local assistant..._";
    private final Project project;
    private final ShaftAssistantChatState chatState;
    private final JComboBox<ShaftAssistantChatState.Session> chatSelector;
    private final JButton newChat;
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
    private boolean refreshingChats;
    private int localAgentStreamToken;
    private int activeLocalAgentStreamToken = -1;
    private StringBuilder localAgentOutput;
    private final List<ToolEvidence> toolEvidence = new ArrayList<>();
    private String activeCaptureRecordingPath = AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH;
    private CaptureReview pendingCaptureReview;
    private boolean generateCaptureReviewAfterStop;
    private boolean captureReviewGenerationRunning;
    private boolean captureIntegrationRunning;
    private List<AssistantCommand.ToolCall> currentToolSequence = List.of();
    private StringBuilder sequenceMarkdown;
    private StringBuilder sequenceRawOutput;

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
        chatSelector.addActionListener(event -> switchChat());
        newChat = button("New chat", "Start a new Assistant chat", event -> newChat());
        ShaftIconButtons.apply(newChat, ShaftIcons.ADD);
        mode = combo("Assistant mode", "ASK", "PLAN", "AGENT");
        mode.setSelectedItem(normalize(settings.defaultAutobotMode, "ASK"));
        providerType = combo("Assistant provider type", "LOCAL", "CLOUD");
        providerType.setSelectedItem(normalize(settings.assistantProviderType, "LOCAL"));
        assistantFamily = combo("Assistant family", "CODEX", "CLAUDE", "COPILOT");
        assistantFamily.setSelectedItem(resolveFamily(settings));
        assistantRuntime = combo("Assistant runtime", "CLI", "IDE_PLUGIN", "DESKTOP_APP");
        assistantRuntime.setSelectedItem(normalize(settings.assistantRuntime, "CLI"));
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
        cloudModel.setText(settings.cloudModel == null ? "" : settings.cloudModel);
        customCommand = new JBTextField();
        customCommand.setColumns(18);
        customCommand.getEmptyText().setText("Optional local agent command");
        customCommand.getAccessibleContext().setAccessibleName("Optional local agent command");

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
        prompt = new JBTextArea(5, 32);
        prompt.getAccessibleContext().setAccessibleName("Assistant prompt");
        prompt.setLineWrap(true);
        prompt.setWrapStyleWord(true);
        transcript = new AssistantTranscriptView();
        if (!chatState.activeMarkdown().isBlank()) {
            transcript.setMessages(chatState.activeMessages());
        }
        status = new JLabel(READY_STATUS);
        status.setFont(status.getFont().deriveFont(Math.max(10.0F, status.getFont().getSize2D() - 1.0F)));
        progress = new JProgressBar();
        progress.setIndeterminate(true);
        progress.getAccessibleContext().setAccessibleName("Assistant thinking spinner");
        progress.setPreferredSize(JBUI.size(88, 12));
        progress.setVisible(false);

        send = button("Send", "Send assistant prompt", event -> {
            if (running) {
                cancelCurrent();
            } else {
                send(project);
            }
        });
        ShaftIconButtons.apply(send, ShaftIcons.SEND);
        bindSendHover();
        cancel = button("Cancel", "Cancel assistant request", event -> cancelCurrent());
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
        updateControlVisibility();
        bindKeyboard(project);

        JPanel transcriptPanel = new JPanel(new BorderLayout(4, 4));
        transcriptPanel.add(transcript, BorderLayout.CENTER);
        JPanel transcriptStatus = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        transcriptStatus.add(progress);
        transcriptStatus.add(status);
        JPanel transcriptBottom = new JPanel(new BorderLayout(4, 4));
        transcriptBottom.add(captureReviewPanel, BorderLayout.NORTH);
        transcriptBottom.add(transcriptStatus, BorderLayout.SOUTH);
        transcriptPanel.add(transcriptBottom, BorderLayout.SOUTH);

        JPanel actionRow = wrapRow();
        actionRow.add(chatSelector);
        actionRow.add(newChat);
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

        JPanel promptActions = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 0));
        JButton commandHint = button("/commands", "SHAFT command hints",
                event -> status.setText("Hover /commands for command aliases"));
        ShaftIconButtons.apply(commandHint, ShaftIcons.HELP);
        commandHint.setToolTipText(AssistantCommand.commandTooltip());
        promptActions.add(commandHint);
        promptActions.add(send);

        JPanel composerFooter = new JPanel(new BorderLayout(4, 4));
        composerFooter.add(routeRow, BorderLayout.CENTER);
        composerFooter.add(promptActions, BorderLayout.SOUTH);

        JPanel composer = new JPanel(new BorderLayout(4, 4));
        composer.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createEtchedBorder(),
                JBUI.Borders.empty(6)));
        composer.add(new JLabel("Add context (#), extensions (@), commands (/commands)"), BorderLayout.NORTH);
        composer.add(new JBScrollPane(prompt), BorderLayout.CENTER);
        composer.add(cloudKeyPanel, BorderLayout.WEST);
        composer.add(composerFooter, BorderLayout.SOUTH);

        JPanel south = new JPanel(new BorderLayout(4, 4));
        south.add(actionRow, BorderLayout.NORTH);
        south.add(composer, BorderLayout.CENTER);

        add(setupNotice(project, settings), BorderLayout.NORTH);
        add(transcriptPanel, BorderLayout.CENTER);
        add(south, BorderLayout.SOUTH);
        refreshChatSelector();
    }

    JComponent preferredFocusComponent() {
        return prompt;
    }

    static boolean requiresMcpSetup(AssistantCommand.Invocation invocation, boolean mcpConfigured) {
        return invocation != null && invocation.requiresMcpConfiguration() && !mcpConfigured;
    }

    private void send(Project project) {
        String text = prompt.getText().trim();
        if (text.isBlank()) {
            status.setText("Enter a prompt");
            return;
        }
        if (usesCloud() && !hasSelectedCloudKey()) {
            status.setText("Enter " + ShaftUiLabels.friendly(cloudProvider.getSelectedItem()) + " key");
            updateCloudKeyStatus();
            return;
        }
        lastPrompt = text;
        rerunLastPrompt.setEnabled(true);
        AssistantCommand.Selection route = selectedRoute();
        boolean agentMode = "AGENT".equals(String.valueOf(mode.getSelectedItem()));
        String workingDirectory = project == null || project.getBasePath() == null ? "" : project.getBasePath();
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
                String.valueOf(mode.getSelectedItem()),
                workingDirectory,
                customCommand.getText(),
                allowSourceMutation.isSelected());
        append("user", AssistantMarkdown.normalizeMarkdown(text), "");
        if (agentMode
                && !approvingCaptureReview
                && !route.cloud()
                && !allowSourceMutation.isSelected()
                && promptRequiresSourceMutation(text)) {
            append("assistant", "To let the agent make source edits, please tick **Allow source edits** before sending.",
                    "");
        }
        prompt.setText("");
        if (invocation.isLocal()) {
            showLocalResponse(invocation.localResponse());
            return;
        }
        if (requiresMcpSetup(invocation, mcpConfigured())) {
            showLocalResponse("Configure SHAFT MCP in Settings before running this Assistant feature command.");
            status.setText("Configure MCP");
            return;
        }
        if (AssistantLocalAgentRunner.supports(invocation)) {
            captureIntegrationRunning = approvingCaptureReview;
            int streamToken = ++localAgentStreamToken;
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
            startToolSequence(invocation.toolCalls());
            return;
        }
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
            if ("capture_code_blocks".equals(toolName) && captureReviewGenerationRunning) {
                captureReviewGenerationRunning = false;
                clearPendingCaptureReview();
            }
            showResponse("**SHAFT Assistant (" + toolName + " cancelled)**", "");
            status.setText("Cancelled");
            return;
        }
        String output = error != null ? error.getMessage()
                : result == null ? "No result returned."
                : result.output();
        if (!output.isBlank()) {
            appendToolEvidence(toolName, output);
        }
        String markdown = AssistantMarkdown.fromMcpOutput(toolName, output);
        if (!success && captureReviewGenerationRunning && "capture_code_blocks".equals(toolName)) {
            captureReviewGenerationRunning = false;
        }
        if (success && captureReviewGenerationRunning && "capture_code_blocks".equals(toolName)) {
            captureReviewGenerationRunning = false;
            pendingCaptureReview = new CaptureReview(markdown, output);
            showPendingCaptureReview();
            showResponse("**SHAFT Assistant (" + toolName + " OK)**\n\n"
                    + markdown
                    + "\n\n**Review before writing files.** Send `approve`, `okay`, or `generate` to let the Agent create the actual Page Object Model files.",
                    output);
            status.setText("Awaiting approval");
            return;
        }
        if (success && generateCaptureReviewAfterStop && "capture_stop".equals(toolName)) {
            stopCaptureStartDiagnostic();
            showResponse("**SHAFT Assistant (" + toolName + " OK)**\n\n" + markdown, output);
            startCaptureCodeReview();
            return;
        }
        if (success && formatUnknownResponse(toolName, output, markdown)) {
            return;
        }
        showResponse("**SHAFT Assistant (" + toolName + (success ? " OK" : " failed") + ")**\n\n"
                + markdown, output);
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
        if (streamToken > 0 && !currentStream) {
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
            showAgentCancelled(streamToken, currentStream);
            status.setText("Cancelled");
            return;
        }
        String output = error != null ? error.getMessage()
                : result == null ? "No response returned."
                : result.output();
        if (!output.isBlank()) {
            appendToolEvidence("autobot_local_agent_run", output);
        }
        String response = AssistantMarkdown.normalizeMarkdown(output);
        showAgentResponse(streamToken, currentStream, response, output);
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
        append("assistant", LOCAL_AGENT_STREAMING_HEADER, "");
    }

    private void appendLocalAgentOutput(int streamToken, String line) {
        if (streamToken != activeLocalAgentStreamToken || localAgentOutput == null) {
            return;
        }
        if (localAgentOutput.length() > 0) {
            localAgentOutput.append("\n");
        }
        localAgentOutput.append(line == null ? "" : line);
        replaceLastTranscriptAndChatState("assistant", formatLocalAgentStreamingResponse(localAgentOutput.toString()));
    }

    private void finishLocalAgentResponse(int streamToken, String response, String rawResponse) {
        if (streamToken != activeLocalAgentStreamToken && activeLocalAgentStreamToken != -1) {
            return;
        }
        replaceLastTranscriptAndChatState("assistant", response);
        lastResponse = response;
        lastRawResponse = rawResponse == null ? "" : rawResponse;
        copyLastResponse.setEnabled(true);
        copyRawResponse.setEnabled(!lastRawResponse.isBlank());
    }

    private void showLocalResponse(String response) {
        status.setText(READY_STATUS);
        showResponse("**SHAFT Assistant**\n\n" + AssistantMarkdown.normalizeMarkdown(response), response);
    }

    private void showResponse(String response, String rawResponse) {
        lastResponse = response;
        lastRawResponse = rawResponse == null ? "" : rawResponse;
        copyLastResponse.setEnabled(true);
        copyRawResponse.setEnabled(!lastRawResponse.isBlank());
        append("assistant", response, rawResponse);
    }

    private void append(String role, String text, String rawResponse) {
        transcript.append(role, text);
        chatState.append(role, text, rawResponse);
        refreshChatSelector();
    }

    void setRunning(boolean running, String message) {
        this.running = running;
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
        allowSourceMutation.setEnabled(!running);
        saveCloudApiKey.setEnabled(!running);
        approveCaptureReview.setEnabled(!running && pendingCaptureReview != null);
        copyCaptureReview.setEnabled(!running && pendingCaptureReview != null);
        dismissCaptureReview.setEnabled(!running && pendingCaptureReview != null);
        cancel.setEnabled(running);
        progress.setVisible(running);
        status.setText(message);
        updateSendButtonState();
        stopTransientStatus();
        updateControlVisibility();
        if (!running) {
            currentInvocation = null;
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
            send.setToolTipText("Send assistant prompt");
            return;
        }
        send.setIcon(sendCancelHover ? ShaftIcons.CANCEL : AnimatedIcon.Default.INSTANCE);
        send.setToolTipText(sendCancelHover ? "Cancel assistant request" : "Assistant request running");
    }

    private void showTransientStatus(String message) {
        stopTransientStatus();
        status.setText(message);
        transientStatusTimer = new Timer(TRANSIENT_STATUS_MILLIS, event -> {
            status.setText(READY_STATUS);
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

    private void updateControlVisibility() {
        boolean advanced = settings.advancedUiEnabled;
        if (!advanced && usesCloud()) {
            providerType.setSelectedItem("LOCAL");
        }
        boolean cloud = usesCloud();
        if (cloud && "AGENT".equals(mode.getSelectedItem())) {
            mode.setSelectedItem("PLAN");
        }
        boolean localCli = !cloud && "CLI".equals(assistantRuntime.getSelectedItem());
        boolean lockedRoute = configureFlow != null && mcpConfigured();
        boolean controlsEnabled = !running;
        mode.setVisible(advanced);
        mode.setEnabled(controlsEnabled && advanced);
        providerType.setVisible(advanced && !lockedRoute);
        providerType.setEnabled(controlsEnabled && advanced && !lockedRoute);
        assistantFamily.setVisible(advanced && !lockedRoute && !cloud);
        assistantRuntime.setVisible(advanced && !lockedRoute && !cloud);
        assistantFamily.setEnabled(controlsEnabled && advanced && !lockedRoute);
        assistantRuntime.setEnabled(controlsEnabled && advanced && !lockedRoute);
        currentAgentConfiguration.setText(currentAgentConfigurationText());
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
        allowSourceMutation.setVisible(advanced && agentMode && localCli);
        allowSourceMutation.setEnabled(controlsEnabled && advanced && agentMode && localCli);
        configure.setVisible(lockedRoute);
        configure.setEnabled(controlsEnabled && lockedRoute);
        if (!agentMode || !localCli) {
            allowSourceMutation.setSelected(false);
        }
        if (cloud) {
            updateCloudKeyStatus();
        }
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
            status.setText("Enter provider key");
            return;
        }
        ShaftCredentialService.getInstance().setApiKey(keyName, cloudApiKey.getPassword());
        cloudApiKey.setText("");
        settings.passProviderApiKeysToMcp = true;
        updateCloudKeyStatus();
        status.setText("Saved key");
    }

    private boolean hasSelectedCloudKey() {
        String keyName = providerKeyName(String.valueOf(cloudProvider.getSelectedItem()));
        return !keyName.isBlank() && ShaftCredentialService.getInstance().hasApiKey(keyName);
    }

    private void bindKeyboard(Project project) {
        prompt.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, InputEvent.CTRL_DOWN_MASK), "send");
        prompt.getActionMap().put("send", new AbstractAction() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent event) {
                send(project);
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
        status.setText("Cleared");
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
        status.setText("New chat");
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
            status.setText("Chat loaded");
        }
    }

    private void rememberCaptureInvocation(String promptText, AssistantCommand.Invocation invocation) {
        if ("capture_start".equals(invocation.toolName())) {
            activeCaptureRecordingPath = string(invocation.arguments(), "outputPath",
                    AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH);
            clearPendingCaptureReview();
            stopCaptureStartDiagnostic();
            generateCaptureReviewAfterStop = false;
            captureReviewGenerationRunning = false;
            return;
        }
        if ("capture_stop".equals(invocation.toolName()) && AssistantCommand.isStopRecording(promptText)) {
            stopCaptureStartDiagnostic();
            generateCaptureReviewAfterStop = true;
        }
    }

    private void startCaptureCodeReview() {
        generateCaptureReviewAfterStop = false;
        captureReviewGenerationRunning = true;
        setRunning(true, "Generating review code...");
        AssistantCommand.Invocation invocation = AssistantCommand.Invocation.tool(
                "capture_code_blocks",
                AssistantCommand.captureCodeReview(activeCaptureRecordingPath));
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startTool(invocation.toolName(), invocation.arguments());
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showResult(invocation.toolName(), result, error)));
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
        status.setText(READY_STATUS);
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
        status.setText("Capture stopped");
    }

    private void rerun(Project project) {
        if (!lastPrompt.isBlank()) {
            prompt.setText(lastPrompt);
            send(project);
        }
    }

    private void cancelCurrent() {
        if (currentInvocation != null) {
            currentInvocation.cancel();
            status.setText("Cancelling...");
        }
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
        lastPrompt = "";
        copyLastResponse.setEnabled(false);
        copyRawResponse.setEnabled(false);
        rerunLastPrompt.setEnabled(false);
    }

    private void copy(String value, String message) {
        if (!value.isBlank()) {
            CopyPasteManager.getInstance().setContents(new StringSelection(value));
            status.setText(message);
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

    private static JPanel wrapRow() {
        return new JPanel(new WrapLayout(FlowLayout.LEFT, 6, 4));
    }

    private static JButton button(String text, String accessibleName, java.awt.event.ActionListener action) {
        JButton button = new JButton(text);
        button.getAccessibleContext().setAccessibleName(accessibleName);
        button.addActionListener(action);
        return button;
    }

    private static ShaftAssistantChatState chatState(Project project) {
        if (project == null) {
            return new ShaftAssistantChatState();
        }
        ShaftAssistantChatState state = ShaftAssistantChatState.getInstance(project);
        return state == null ? new ShaftAssistantChatState() : state;
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
        return settings != null
                && settings.mcpSetupComplete
                && settings.mcpCommand != null
                && !settings.mcpCommand.isBlank();
    }

    private String currentAgentConfigurationText() {
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

    private record ToolEvidence(String toolName, String payload, String createdAt) {
    }

    private record CaptureReview(String markdown, String rawResult) {
    }
}
