package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.intellij.ide.util.PropertiesComponent;
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
import com.shaft.intellij.approval.LocalAgentApprovalBridge;
import com.shaft.intellij.approval.ToolApprovalDecision;
import com.shaft.intellij.approval.ToolApprovalService;
import com.shaft.intellij.java.InsertionAnchor;
import com.shaft.intellij.java.InsertionAnchorResolver;
import com.shaft.intellij.project.ShaftProjectDetector;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpProgress;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.mcp.ShaftPluginExecutor;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.settings.ShaftCredentialService;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
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
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.text.JTextComponent;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.Component;
import java.awt.FontMetrics;
import java.awt.datatransfer.StringSelection;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * SHAFT Assistant chat-style panel.
 */
final class ShaftAssistantPanel extends JPanel {
    private static final int TRANSIENT_STATUS_MILLIS = 2300;
    private static final int MAX_AGENT_CONTEXT_CHARACTERS = 16_000;
    static final String PROMPT_PLACEHOLDER =
            "Tell SHAFT what you need — record, generate a test, diagnose failures, upgrade "
                    + "(# adds project context, @ inserts a workflow)";
    private static final String READY_STATUS = "Try asking me to do something...";
    private static final String SEND_TOOLTIP = "Send assistant prompt (Ctrl+Enter, Command+Enter, or Ctrl+click)";
    private static final String LOCAL_AGENT_STREAMING_HEADER = "_Running local assistant..._";
    private static final String LOCAL_MODEL_TOOLTIP_LIVE = "Model reported by the connected agent CLI";
    private static final String LOCAL_MODEL_TOOLTIP_FALLBACK =
            "Fallback model list (CLI did not report models) — click refresh to try again";
    /**
     * Prefix applied to a local-agent CLI's own tool names (e.g. {@code "Bash"}, {@code "Write"})
     * before recording/checking approval decisions, so they can never collide with SHAFT MCP tool
     * names (e.g. {@code "capture_start"}) in the shared {@link ToolApprovalService} namespace.
     */
    private static final String LOCAL_AGENT_APPROVAL_KEY_PREFIX = "local-agent:";
    /**
     * Sentinel key recorded when the user clicks "Approve all tools" on a local-agent approval
     * prompt. Deliberately NOT {@link ToolApprovalService}'s shared {@code approveAllTools} flag,
     * which would also silently auto-approve every unrelated SHAFT MCP tool call.
     */
    private static final String LOCAL_AGENT_APPROVE_ALL_KEY = "local-agent:*";
    private static final String NO_CODE_GENERATED_NOTE =
            "_No generated code was returned for this recording. The capture session may have no "
                    + "recorded actions (for example, if the browser was closed by a different process "
                    + "before any actions were captured) or code generation may have failed silently. "
                    + "Record the journey again, confirm the recording actually captured actions, then "
                    + "ask for a test generated from that recording._";
    /**
     * First-run welcome (issue #3500 O1, follow-up #3540): the Assistant's own first message,
     * shown once via {@link #showFirstRunWelcomeIfNeeded()} until dismissed. The numbered steps
     * read as plain text, not the five-emoji list this used to be -- this was the one place emoji
     * appeared anywhere in the plugin, breaking from the wizard/Settings/rest-of-panel voice
     * (#3601 B3.3); the opening "Hi!" keeps its own emoji as the one deliberate warm-greeting
     * accent. The closing line is a first-session tooltip coach for the icon-only toolbar (#3601
     * A3): every button in this panel is icon-only via {@code ShaftIconButtons.apply(...)} with a
     * hover tooltip and an accessible name, but a first-time user sees a row of unlabeled icons
     * with no hover yet. No codebase precedent for a Balloon/JBPopupFactory coach-mark exists to
     * point at a specific control, so this reuses the same show-once welcome mechanism instead of
     * introducing a new popup widget, naming only the two or three controls needed first.
     */
    private static final String FIRST_RUN_WELCOME_MARKDOWN =
            "👋 Hi! I'm the SHAFT Assistant — I turn what you do in your app into real tests.\n\n"
                    + "**Let's get started**\n\n"
                    + "1. Check your setup in the status strip up top.\n"
                    + "2. Record a sample flow — just click around your app.\n"
                    + "3. Review code to turn it into a real test.\n"
                    + "4. Or just tell me what you need below.\n\n"
                    + "Every button around this panel is an icon with a tooltip — hover any of them "
                    + "for its name; the ones you'll reach for first are New chat, Send, and Copy response.";
    private final Project project;
    // Stable per-instance identity so overlapping recordings across surfaces don't collapse onto
    // one process-wide flag (issue #3591 item 3).
    private final String recordingKey = "assistant#" + Integer.toHexString(System.identityHashCode(this));
    private final ShaftAssistantChatState chatState;
    private final JComboBox<ShaftAssistantChatState.Session> chatSelector;
    private final JButton newChat;
    private final JComboBox<String> mode;
    private final JComboBox<String> providerType;
    private final JComboBox<String> assistantFamily;
    private final JComboBox<String> assistantRuntime;
    private final JComboBox<String> cloudProvider;
    private final JComboBox<String> cloudModel;
    private final JComboBox<String> localModel;
    private final JButton refreshLocalModels;
    private final JComboBox<String> effort;
    private final JBTextField customCommand;
    private final JPanel cloudKeyPanel;
    private final JPasswordField cloudApiKey;
    private final JButton saveCloudApiKey;
    private final JLabel cloudKeyStatus;
    private final JBCheckBox allowSourceMutation;
    /**
     * Bordered chip wrapping {@link #allowSourceMutation} so the highest-stakes toggle in
     * {@code routeRow} (it lets Agent mode mutate the user's project source) reads with distinct
     * visual weight from the neutral {@link #verboseAgentOutput}/{@link #autoCompact} checkboxes
     * beside it (#3601 B3.4). Purely presentational: its own visibility must mirror
     * {@link #allowSourceMutation}'s in {@link #updateControlVisibility()} so it never renders as
     * an empty colored box, but its selection/enable/listener logic stays entirely on the checkbox.
     */
    private final JPanel allowSourceMutationChip;
    private final JBCheckBox verboseAgentOutput;
    private final JBCheckBox autoCompact;
    private final JBTextArea prompt;
    private final AssistantTranscriptView transcript;
    private final JButton send;
    private final JButton attach;
    private final JButton cancel;
    private final JButton copyLastResponse;
    private final JButton copyRawResponse;
    private final JButton copyTranscript;
    private final JButton saveTranscript;
    private final JPanel captureReviewPanel;
    private final JLabel captureReviewStatus;
    private final JButton approveCaptureReview;
    private final JButton copyCaptureReview;
    private final JButton dismissCaptureReview;
    private final JButton createTestClassFromReview;
    private final JButton insertReviewAtOpenFile;
    private final JButton openCaptureReview;
    private final JButton captureEvidencePack;
    private final JButton compareCaptureBackends;
    /** Session path behind the pending Capture review, for insert/compare/evidence actions. */
    private String lastReviewSessionPath = "";
    /** True once a terminal step (Completed/Failed/Cancelled/Killed) has been recorded for the current
     * run, so a duplicate terminal signal for the same run does not append a second terminal
     * milestone bubble. */
    private boolean terminalRecorded;
    /** Wall-clock start of the current run, for the terminal milestone bubble's elapsed-time suffix. */
    private long runStartNanos;
    /** Last agent-milestone bubble text appended for the current run (issue #3695), so an
     * immediately repeated milestone (e.g. two identical streamed progress notifications) is not
     * appended twice -- mirrors the old "Run timeline" list's consecutive-duplicate guard. */
    private String lastAgentMilestone = "";
    private final JPanel actionRow;
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
    /** True once the user's second cancel click escalates to a kill for the current invocation. */
    private boolean killRequested;
    private boolean refreshingChats;
    /** True when {@link #localModel} shows the curated catalog because the CLI reported no models. */
    private boolean localModelListIsFallback;
    private int localAgentStreamToken;
    private int activeLocalAgentStreamToken = -1;
    private int killedLocalAgentStreamToken = -1;
    /** Index (into the active chat session's message list) of the current local-agent run's
     * streaming placeholder bubble, so {@link #replaceLocalAgentStreamPlaceholder} can update that
     * exact bubble even after agent-milestone bubbles (issue #3695) have been appended after it --
     * unlike a plain "replace the last message", which those milestone bubbles would otherwise
     * shadow. -1 while no local-agent stream is active. */
    private int localAgentStreamPlaceholderMessageIndex = -1;
    private StringBuilder localAgentOutput;
    private boolean localAgentBubbleRendersContent;
    // issue #3751 part 2, HIGH finding 2: readAsync's stdout/stderr reader threads used to invoke one
    // ApplicationManager#invokeLater per output line, flooding the EDT queue on a chatty CLI stream.
    // This coalesces per-run into throttled ~100ms batch flushes -- see the field's flush() call sites
    // in showAgentResult/stopLocalAgentStreaming, which drain any not-yet-flushed lines before a
    // terminal render reads localAgentOutput, so a run's very last streamed lines are never lost or
    // silently delayed past the terminal state.
    private LocalAgentOutputCoalescer localAgentOutputCoalescer;
    private final Deque<Runnable> queuedLocalAgentApprovalPrompts = new ArrayDeque<>();
    private boolean localAgentApprovalPromptShowing;
    private final List<ToolEvidence> toolEvidence = new ArrayList<>();
    private String activeCaptureRecordingPath = AssistantCommand.DEFAULT_CAPTURE_RECORDING_PATH;
    private String activePlaywrightRecordingPath = AssistantCommand.DEFAULT_PLAYWRIGHT_RECORDING_PATH;
    /**
     * Session JSON path for the active {@code capture_api_*} recording (issue #3739). Unlike
     * {@link #activeCaptureRecordingPath}/{@link #activePlaywrightRecordingPath}, {@code
     * capture_api_start} returns no start-time output path -- the persisted session path only
     * arrives in the {@code capture_api_stop} result's {@code CaptureStatus.outputPath}, extracted
     * in {@link #showCaptureStopDiagnosticIfPending}.
     */
    private String activeApiRecordingPath = "";
    private RecordingBackend activeRecordingBackend = RecordingBackend.WEBDRIVER;
    private javax.swing.JPanel emptyStateChips;
    /**
     * Files/images attached to the next prompt (issue #3727): removable chips rendered by {@link
     * #refreshAttachmentsChipRow()}, folded into the outbound prompt text via {@link
     * AssistantAttachments#outboundBlock} from {@link #send(Project)}. Cleared alongside the prompt
     * text itself once a send actually goes through (mirrors the existing {@code prompt.setText("")}
     * reset point), but preserved across a pre-flight gate bounce so a resend keeps them.
     */
    private List<AssistantAttachment> attachments = new ArrayList<>();
    private javax.swing.JPanel attachmentsChipRow;
    /**
     * Resolves the current editor's content for "Add current file" (issue #3727). Defaults to the
     * existing {@link #openFileContext(Project)} static helper so production behavior is unchanged;
     * overridden via reflection in headless tests (no real {@code FileEditorManager} without a live
     * IDE project) to simulate an open editor, per the "current-file... actions with simulated
     * editors" TDD requirement.
     */
    private java.util.function.Function<Project, AssistantCommand.OpenFileContext> currentEditorFileProvider =
            ShaftAssistantPanel::openFileContext;
    /**
     * Resolves every open editor's content for "Add all open files" (issue #3727). Same
     * test-injection rationale as {@link #currentEditorFileProvider}.
     */
    private java.util.function.Function<Project, List<AssistantCommand.OpenFileContext>> openEditorFilesProvider =
            ShaftAssistantPanel::realOpenEditorFiles;
    private CaptureReview pendingCaptureReview;
    private boolean generateCaptureReviewAfterStop;
    private boolean captureReviewGenerationRunning;
    private boolean captureIntegrationRunning;
    private List<AssistantCommand.ToolCall> currentToolSequence = List.of();
    private StringBuilder sequenceMarkdown;
    private StringBuilder sequenceRawOutput;
    private final Set<String> approvedToolsThisRun = new HashSet<>();
    private ToolApprovalService approvalServiceOverride;
    private JPopupMenu contextPopup;
    private JButton convertSeleniumHint;
    private char contextPopupTrigger;
    private int contextTriggerOffset = -1;
    private int contextTruncationBoundaryIndex = -1;
    private String modelListFamily = "";
    private boolean modelListRefreshing;
    /**
     * Banner strip (setup notice + fresh-project hint) above the chat header. A field, not a
     * constructor-local, purely so {@code ShaftAssistantPanelLayoutTest} can assert its collapsed
     * preferred height via reflection (issue #3694): it must use a layout manager that skips
     * invisible children -- see the constructor for why {@link javax.swing.BoxLayout} replaced
     * {@link java.awt.GridLayout} here.
     */
    private final JPanel notices;

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
        super(new BorderLayout(8, 8));
        this.project = project;
        this.settings = settings;
        this.chatState = chatState;
        this.configureFlow = setupFlow;
        setBorder(JBUI.Borders.empty(12));

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
        mode = combo("Assistant mode", "ASK", "PLAN", "AGENT");
        mode.setSelectedItem(normalize(settings.defaultAutobotMode, "AGENT"));
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
        cloudProvider = combo("Assistant cloud provider", "gemini", "openai", "anthropic", "github");
        cloudProvider.setSelectedItem(normalizeLower(settings.cloudProvider, "gemini"));
        cloudModel = new JComboBox<>();
        cloudModel.setEditable(true);
        cloudModel.getAccessibleContext().setAccessibleName("Assistant cloud model");
        cloudModel.setToolTipText("Models available for the selected cloud provider; type any other model name");
        if (cloudModel.getEditor().getEditorComponent() instanceof JTextComponent cloudModelEditor) {
            cloudModelEditor.getAccessibleContext().setAccessibleName("Assistant cloud model text");
            cloudModelEditor.setToolTipText("Models available for the selected cloud provider; type any other model name");
        }
        applyCloudModelChoices(settings.cloudModel);
        localModel = new JComboBox<>();
        localModel.setEditable(true);
        localModel.getAccessibleContext().setAccessibleName("Assistant local agent model");
        localModel.setToolTipText(LOCAL_MODEL_TOOLTIP_LIVE);
        if (localModel.getEditor().getEditorComponent() instanceof JTextComponent localModelEditor) {
            localModelEditor.getAccessibleContext().setAccessibleName("Assistant local agent model text");
            localModelEditor.setToolTipText(LOCAL_MODEL_TOOLTIP_LIVE);
        }
        // Seed the selector from the curated catalog so it is never empty; the async CLI listing
        // replaces these entries when the connected agent can report its own models.
        applyLocalModels(resolveFamily(settings), List.of());
        modelListFamily = "";
        refreshLocalModels = button("Refresh", "Refresh local agent models", event -> {
            // Bypass refreshLocalModelsIfNeeded's already-fetched guard so a manual click always
            // asks the CLI again, even when it already reported (or failed to report) for this family.
            modelListFamily = "";
            refreshLocalModelsIfNeeded();
        });
        ShaftIconButtons.apply(refreshLocalModels, ShaftIcons.RERUN);
        effort = combo("Assistant effort", AssistantModelCatalog.effortLevels().toArray(new String[0]));
        effort.setSelectedItem(normalize(settings.assistantEffort, AssistantModelCatalog.DEFAULT_EFFORT));
        effort.setToolTipText("Reasoning effort requested from the selected model");
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
        allowSourceMutation.setToolTipText("Let Agent mode write generated tests and fixes into this project; "
                + "uncheck for suggestion-only runs");
        // Checked by default: a first-time user asking for a generated test expects it to land in
        // the project, and the per-send approval gate still confirms before the first mutation.
        allowSourceMutation.setSelected(true);
        // Warning-tinted chip so this reads as higher-stakes than the plain verboseAgentOutput/
        // autoCompact checkboxes beside it in routeRow, reusing the same
        // ShaftStatusPresentation.tint(...)+JBUI.Borders.customLine(...) pairing this file already
        // uses for the transient status label (#3601 B3.4).
        allowSourceMutationChip = new JPanel(new BorderLayout());
        allowSourceMutationChip.setOpaque(true);
        allowSourceMutationChip.setBackground(ShaftStatusPresentation.tint(
                javax.swing.UIManager.getColor("Panel.background") == null
                        ? java.awt.Color.WHITE
                        : javax.swing.UIManager.getColor("Panel.background"),
                ShaftStatusPresentation.disconnected(), 0.12D));
        allowSourceMutationChip.setBorder(JBUI.Borders.compound(
                JBUI.Borders.customLine(ShaftStatusPresentation.disconnected(), 1),
                JBUI.Borders.empty(2, 6)));
        allowSourceMutationChip.add(allowSourceMutation, BorderLayout.CENTER);
        verboseAgentOutput = new JBCheckBox("Verbose");
        verboseAgentOutput.getAccessibleContext().setAccessibleName("Show verbose agent output");
        verboseAgentOutput.setToolTipText("Forward everything as-is: live local agent output "
                + "(including its thinking and every tool call) plus the exact request and raw "
                + "response of each SHAFT MCP tool run. GitHub Copilot CLI and custom agent "
                + "commands cannot stream live output; their response is buffered until the "
                + "command completes.");
        autoCompact = new JBCheckBox("Auto-compact");
        autoCompact.getAccessibleContext().setAccessibleName("Compact agent context before each request");
        autoCompact.setToolTipText("Send the agent CLI's compact/compress command before each new prompt, when supported");
        autoCompact.setSelected(settings.autoCompactEnabled);
        autoCompact.addActionListener(event -> settings.autoCompactEnabled = autoCompact.isSelected());
        // Custom-painted placeholder: IntelliJ's StatusText never wraps and clips long lines even
        // in wide tool windows, so the hint is measured against the real component width and
        // continues onto the next line instead of being cut off.
        prompt = new PlaceholderTextArea(PROMPT_PLACEHOLDER);
        prompt.getAccessibleContext().setAccessibleName("Assistant prompt");
        prompt.getAccessibleContext().setAccessibleDescription(
                "Describe what you need in plain language or request guarded local Agent work.");
        prompt.setLineWrap(true);
        prompt.setWrapStyleWord(true);
        transcript = new AssistantTranscriptView(project);
        transcript.setCopyFullTranscriptAction(this::copyFullTranscript);
        if (!chatState.activeMarkdown().isBlank()) {
            transcript.setMessages(chatState.activeMessages());
            lastPrompt = latestUserPrompt();
            updateContextTruncationBoundary();
        } else {
            showFirstRunWelcomeIfNeeded();
        }
        status = new JLabel(READY_STATUS);
        status.getAccessibleContext().setAccessibleName("Assistant status");
        status.getAccessibleContext().setAccessibleDescription(READY_STATUS);
        status.setToolTipText(READY_STATUS);
        status.setFont(status.getFont().deriveFont(Math.max(10.0F, status.getFont().getSize2D() - 1.0F)));
        status.setOpaque(true);
        status.setBackground(ShaftStatusPresentation.tint(
                javax.swing.UIManager.getColor("Panel.background") == null
                        ? java.awt.Color.WHITE
                        : javax.swing.UIManager.getColor("Panel.background"),
                ShaftStatusPresentation.progress(), 0.08D));
        status.setBorder(JBUI.Borders.compound(
                JBUI.Borders.customLine(ShaftStatusPresentation.progress(), 1),
                JBUI.Borders.empty(4, 8)));
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
        attach = button("Attach", "Attach files or images to the prompt", event -> showAttachMenu());
        ShaftIconButtons.apply(attach, ShaftIcons.ATTACH);
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
                event -> copyFullTranscript());
        ShaftIconButtons.apply(copyTranscript, ShaftIcons.COPY);
        saveTranscript = button("Save transcript", "Save assistant transcript to file",
                event -> saveTranscript());
        saveTranscript.setMnemonic(java.awt.event.KeyEvent.VK_S);
        ShaftIconButtons.apply(saveTranscript, ShaftIcons.DOWNLOAD);
        captureReviewStatus = new JLabel("Capture review ready");
        captureReviewStatus.getAccessibleContext().setAccessibleName("Capture review status");
        // Accessible description mirrors the live status text (issue #3603): see
        // showPendingCaptureReview(), the choke point every later update runs through.
        captureReviewStatus.getAccessibleContext().setAccessibleDescription(captureReviewStatus.getText());
        approveCaptureReview = button("Approve", "Approve Capture review", event -> approvePendingCaptureReview());
        ShaftIconButtons.apply(approveCaptureReview, ShaftIcons.CHECK);
        copyCaptureReview = button("Copy review", "Copy Capture review", event -> copyPendingCaptureReview());
        ShaftIconButtons.apply(copyCaptureReview, ShaftIcons.COPY);
        dismissCaptureReview = button("Dismiss", "Dismiss Capture review", event -> dismissPendingCaptureReview());
        ShaftIconButtons.apply(dismissCaptureReview, ShaftIcons.CANCEL);
        // Record -> Review -> Insert headline loop plus evidence/differentiation actions
        // (issue #3425 B1/B6/C2): everything a reviewed recording can become is one click away.
        createTestClassFromReview = button("Create test class",
                "Create test class from Capture review", event -> createTestClassFromReview());
        ShaftIconButtons.apply(createTestClassFromReview, ShaftIcons.CODE);
        insertReviewAtOpenFile = button("Insert into open class",
                "Insert reviewed Capture steps into the open class", event -> insertReviewIntoOpenFile());
        ShaftIconButtons.apply(insertReviewAtOpenFile, ShaftIcons.EDIT);
        openCaptureReview = button("Open review file",
                "Open generated Capture review file", event -> openCaptureReviewFile());
        ShaftIconButtons.apply(openCaptureReview, ShaftIcons.VIEW);
        captureEvidencePack = button("Evidence pack",
                "Collect Capture evidence pack", event -> collectCaptureEvidencePack());
        ShaftIconButtons.apply(captureEvidencePack, ShaftIcons.COPY);
        compareCaptureBackends = button("Compare backends",
                "Compare WebDriver and Playwright generation", event -> compareCaptureBackends());
        ShaftIconButtons.apply(compareCaptureBackends, ShaftIcons.SEARCH);
        JPanel captureReviewActions = new JPanel(new WrapLayout(FlowLayout.RIGHT, 6, 2));
        captureReviewActions.add(approveCaptureReview);
        captureReviewActions.add(createTestClassFromReview);
        captureReviewActions.add(insertReviewAtOpenFile);
        captureReviewActions.add(openCaptureReview);
        captureReviewActions.add(captureEvidencePack);
        captureReviewActions.add(compareCaptureBackends);
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

        mode.addActionListener(event -> onModeOrRouteSelectionChanged());
        providerType.addActionListener(event -> onModeOrRouteSelectionChanged());
        assistantFamily.addActionListener(event -> updateControlVisibility());
        assistantRuntime.addActionListener(event -> updateControlVisibility());
        cloudProvider.addActionListener(event -> {
            applyCloudModelChoices("");
            updateControlVisibility();
        });
        bindKeyboard(project);
        bindContextInsertion();

        JPanel transcriptPanel = new JPanel(new BorderLayout(4, 4));
        transcriptPanel.add(transcript, BorderLayout.CENTER);
        // Single-line-only current-status strip (issue #3695): the separately-scrollable "Run
        // timeline" list that used to live above this row is gone -- every milestone it used to show
        // (tool selections, "Running", streamed progress, terminal Completed/Failed/...) now renders
        // as its own chat bubble in the transcript above instead (see appendAgentMilestone), so this
        // spinner + single-line, unlabeled status JLabel (see setStatus()) is the only run-status
        // surface left below the chat window.
        JPanel transcriptStatus = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        transcriptStatus.add(progress);
        transcriptStatus.add(status);
        JPanel transcriptBottom = new JPanel(new BorderLayout(4, 4));
        transcriptBottom.add(captureReviewPanel, BorderLayout.NORTH);
        transcriptBottom.add(transcriptStatus, BorderLayout.SOUTH);
        transcriptPanel.add(transcriptBottom, BorderLayout.SOUTH);

        JPanel chatRow = new JPanel(new BorderLayout(6, 0));
        chatRow.add(chatSelector, BorderLayout.CENTER);
        chatRow.add(newChat, BorderLayout.EAST);
        JPanel header = new JPanel(new BorderLayout(4, 2));
        header.getAccessibleContext().setAccessibleName("Assistant chat header");
        header.add(chatRow, BorderLayout.CENTER);

        actionRow = wrapRow();
        actionRow.add(copyLastResponse);
        actionRow.add(copyRawResponse);
        actionRow.add(copyTranscript);
        actionRow.add(saveTranscript);
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
        routeRow.add(localModel);
        routeRow.add(refreshLocalModels);
        routeRow.add(effort);
        routeRow.add(allowSourceMutationChip);
        routeRow.add(verboseAgentOutput);
        routeRow.add(autoCompact);

        JPanel sendActions = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 0));
        sendActions.add(attach);
        sendActions.add(send);
        JPanel promptActions = new JPanel(new BorderLayout(6, 0));
        promptActions.add(sendActions, BorderLayout.EAST);

        JPanel composerFooter = new JPanel(new BorderLayout(4, 4));
        convertSeleniumHint = new JButton("Selenium detected — convert to SHAFT + guardrails");
        convertSeleniumHint.getAccessibleContext().setAccessibleName("Convert pasted Selenium to SHAFT");
        convertSeleniumHint.setToolTipText("Wrap the pasted code in a convert-to-SHAFT request that also runs "
                + "the SHAFT guardrail check on the result");
        convertSeleniumHint.setVisible(false);
        convertSeleniumHint.addActionListener(event -> wrapPromptAsSeleniumConversion());
        composerFooter.add(convertSeleniumHint, BorderLayout.NORTH);
        composerFooter.add(routeRow, BorderLayout.CENTER);
        composerFooter.add(promptActions, BorderLayout.SOUTH);

        JPanel composer = new JPanel(new BorderLayout(4, 4));
        composer.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createEtchedBorder(),
                JBUI.Borders.empty(6)));
        JBScrollPane promptScroll = new JBScrollPane(prompt);
        promptScroll.setMinimumSize(JBUI.size(320, 108));
        promptScroll.setPreferredSize(JBUI.size(560, 120));
        // BoxLayout (not a plain BorderLayout NORTH slot), matching the {@code notices} panel
        // precedent above: the attachments chip row is hidden by default (no attachments yet) and
        // must collapse to zero height rather than reserve blank space (issue #3727).
        JPanel composerTop = new JPanel();
        composerTop.setLayout(new BoxLayout(composerTop, BoxLayout.Y_AXIS));
        composerTop.add(buildAttachmentsChipRow());
        composerTop.add(buildEmptyStateChips());
        composer.add(composerTop, BorderLayout.NORTH);
        composer.add(promptScroll, BorderLayout.CENTER);
        composer.add(cloudKeyPanel, BorderLayout.WEST);
        composer.add(composerFooter, BorderLayout.SOUTH);

        JPanel south = new JPanel(new BorderLayout(4, 4));
        south.add(actionRow, BorderLayout.NORTH);
        south.add(composer, BorderLayout.CENTER);

        // BoxLayout, not GridLayout(0, 1) (issue #3694): GridLayout reserves a full row's height for
        // every child regardless of visibility, so with both banners hidden (the common case -- MCP
        // already configured, project not "fresh") it still reserved two blank rows above the chat
        // header, reading as a large empty gap between the panel header and the "New chat" dropdown.
        // BoxLayout, like BorderLayout, collapses an invisible child's contribution to zero.
        notices = new JPanel();
        notices.setLayout(new BoxLayout(notices, BoxLayout.Y_AXIS));
        notices.add(setupNotice(project, settings));
        // Separate signal from showFirstRunWelcomeIfNeeded() (#3601 O3): keyed off the project's
        // actual pom.xml state via ShaftProjectVersionCheck, not firstRunCoachDismissed, so it
        // keeps helping a returning user whose project is still fresh after the welcome bubble is
        // long gone -- and stays quiet for an already-adopted project even on a first run.
        notices.add(freshProjectNotice(project));

        JPanel north = new JPanel(new BorderLayout(4, 4));
        north.add(notices, BorderLayout.NORTH);
        north.add(header, BorderLayout.CENTER);
        add(north, BorderLayout.NORTH);
        add(transcriptPanel, BorderLayout.CENTER);
        add(south, BorderLayout.SOUTH);
        refreshChatSelector();
        showPendingAgentGuidanceOptimizationPrompt();
        updateControlVisibility();
        warmToolsCache();
    }

    /**
     * Warms {@code ShaftMcpInvocationService}'s shared {@code tools/list} cache on first chat-panel
     * construction (issue #3870/#3866 T4 goal 3): every {@code startTool} dispatch in this class
     * routes through that service's unknown-tool fail-fast + did-you-mean guard, but the guard only
     * activates once {@code knownToolNames()} has been populated by a successful
     * {@code startListTools()} call -- and, before this change, nothing in this class ever made one
     * (only the separate {@link ShaftFeaturePanel}'s {@code autoPopulateCatalog} did, via the same
     * shared per-project service instance). Mirrors that method exactly: best-effort and silent, so
     * an unconfigured MCP command, a disconnected server, or a test double project must never break
     * panel construction or block ordinary chat usability.
     */
    private void warmToolsCache() {
        if (project == null || !mcpConfigured()) {
            return;
        }
        try {
            ShaftMcpInvocationService.getInstance(project).startListTools();
        } catch (RuntimeException ignored) {
            // Best-effort warm-up only.
        }
    }

    JComponent preferredFocusComponent() {
        return prompt;
    }

    /**
     * Fills the composer with {@code text} for the user to review and send themselves — never
     * auto-sends (issue #3552, mirrors {@link #emptyStateChip(String, String)}). Used by
     * {@link ShaftToolWindowPanel#prefillAssistantPrompt(String)} so action/notification entry
     * points that used to silently no-op while advanced workflows are off can instead route the
     * user straight to the Assistant with a ready-to-send plain-language request.
     *
     * @param text plain-language prompt to prefill
     */
    void prefillPrompt(String text) {
        if (text == null || text.isBlank()) {
            return;
        }
        prompt.setText(text);
        prompt.setCaretPosition(text.length());
        prompt.requestFocusInWindow();
        setStatus("Review the prefilled request, then send it");
    }

    /**
     * Runs {@code toolName} against the live MCP connection and renders its result into the
     * transcript as an assistant message, without touching the composer, run-milestone, or capture-
     * review state -- unlike {@link #dispatchApprovedTool}, this is not a user "send": it backs
     * failure-recovery entry points (issue #3547) that trigger a read-only Doctor/Healer diagnosis
     * from outside the composer, either automatically on a failed test run or from a notification's
     * "Diagnose"/"Heal" action. The MCP callback runs on a background thread, so rendering is
     * marshaled onto the EDT.
     *
     * @param toolName MCP tool name to run
     * @param arguments MCP tool arguments
     */
    void runToolAndRenderCard(String toolName, JsonObject arguments) {
        ShaftMcpInvocationService.getInstance(project).startTool(toolName, arguments).future()
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                        () -> append("assistant", toolCardMarkdown(toolName, result, error),
                                result == null ? "" : result.output())));
    }

    /**
     * Pure formatting for {@link #runToolAndRenderCard}: renders a tool result (or failure) as
     * plain-language Markdown, reusing {@link AssistantMarkdown#fromMcpOutput} for the known Doctor
     * card shape so a manual doctor run and an auto-triggered one read identically. Never returns
     * raw JSON or an exception's fully-qualified class name -- a thrown error (for example
     * {@code doctor_analyze_failed_allure} finding no Allure evidence at all) degrades to a plain
     * "couldn't finish" card instead. Package-private and static so it is directly unit-testable
     * without the EDT or a live MCP connection.
     *
     * @param toolName MCP tool name that was run
     * @param result the tool result, or {@code null} if none was returned
     * @param error the failure, or {@code null} on success
     * @return transcript-ready Markdown
     */
    static String toolCardMarkdown(String toolName, ShaftMcpToolResult result, Throwable error) {
        if (error != null) {
            return AssistantMarkdown.toolFailureMarkdown(toolName, AssistantMarkdown.humanizeError(error));
        }
        if (result == null) {
            return AssistantMarkdown.toolFailureMarkdown(toolName, "No result returned.");
        }
        String markdown = AssistantMarkdown.fromMcpOutput(toolName, result.output());
        return result.success() ? markdown : AssistantMarkdown.toolFailureMarkdown(toolName, markdown);
    }

    /** Package-private test accessor: current composer text. */
    String promptText() {
        return prompt.getText();
    }

    /** Package-private test accessor: rendered transcript markdown (blank until a turn is sent). */
    String transcriptMarkdown() {
        return transcript.markdown();
    }

    /**
     * Text area with a placeholder that wraps to the component's real width. IntelliJ's
     * {@code StatusText} empty text never wraps and clips long lines even when the tool window is
     * wide, cutting off the invite mid-sentence.
     */
    static final class PlaceholderTextArea extends JBTextArea {
        private final String placeholder;

        PlaceholderTextArea(String placeholder) {
            super(6, 40);
            this.placeholder = placeholder;
            putClientProperty("shaft.prompt.placeholder", placeholder);
        }

        @Override
        protected void paintComponent(java.awt.Graphics graphics) {
            super.paintComponent(graphics);
            if (!getText().isEmpty() || placeholder == null || placeholder.isBlank()) {
                return;
            }
            java.awt.Graphics2D paint = (java.awt.Graphics2D) graphics.create();
            try {
                paint.setRenderingHint(java.awt.RenderingHints.KEY_TEXT_ANTIALIASING,
                        java.awt.RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
                paint.setColor(java.util.Objects.requireNonNullElse(
                        javax.swing.UIManager.getColor("Component.infoForeground"), java.awt.Color.GRAY));
                paint.setFont(getFont());
                java.awt.FontMetrics metrics = paint.getFontMetrics();
                java.awt.Insets insets = getInsets();
                int available = Math.max(60, getWidth() - insets.left - insets.right - 4);
                int y = insets.top + metrics.getAscent();
                for (String line : wrapToWidth(placeholder, metrics, available)) {
                    paint.drawString(line, insets.left + 2, y);
                    y += metrics.getHeight();
                }
            } finally {
                paint.dispose();
            }
        }

        private static java.util.List<String> wrapToWidth(
                String text, java.awt.FontMetrics metrics, int available) {
            java.util.List<String> lines = new java.util.ArrayList<>();
            StringBuilder line = new StringBuilder();
            for (String word : text.split(" ")) {
                String candidate = line.isEmpty() ? word : line + " " + word;
                if (metrics.stringWidth(candidate) > available && !line.isEmpty()) {
                    lines.add(line.toString());
                    line = new StringBuilder(word);
                } else {
                    line = new StringBuilder(candidate);
                }
            }
            if (!line.isEmpty()) {
                lines.add(line.toString());
            }
            return lines;
        }
    }

    @Override
    public void removeNotify() {
        // Prevents a stuck-active recording key if the panel closes mid-recording (#3591 item 3).
        ShaftRecordingActivity.stopped(recordingKey);
        super.removeNotify();
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

                After making any guidance or memory updates, rerun `py -3 scripts/ci/validate_agent_setup.py --skip-external` on Windows or `python3 scripts/ci/validate_agent_setup.py --skip-external` elsewhere.
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

    private void resetAgentMilestones(String firstStep) {
        terminalRecorded = false;
        runStartNanos = System.nanoTime();
        lastAgentMilestone = "";
        appendAgentMilestone(firstStep);
    }

    /**
     * Renders one agent/tool-run milestone (a tool selection, "Running", a streamed {@code
     * notifications/progress} update, a terminal Completed/Failed/Cancelled/Killed signal, ...) as
     * its own chat bubble in the main transcript, styled like any other assistant message (issue
     * #3695). Replaces the old separately-scrollable "Run timeline" list this content used to feed
     * exclusively: every milestone that used to become a timeline entry now becomes a bubble instead,
     * via the same {@link #append} path every other assistant message goes through, so nothing that
     * used to surface here is silently dropped.
     */
    private void appendAgentMilestone(String step) {
        if (step == null || step.isBlank()) {
            return;
        }
        if (isTerminalStep(step)) {
            if (terminalRecorded) {
                return;
            }
            terminalRecorded = true;
        }
        if (step.equals(lastAgentMilestone)) {
            return;
        }
        lastAgentMilestone = step;
        String icon = agentMilestoneIcon(step);
        // Every milestone is its own chat bubble (issue #3695), so unlike the single polished
        // final-answer bubble per run, these are the majority of what a user sees -- they must go
        // through the same markdown pass everything else does, or JSON/code-shaped content (e.g. a
        // tool-result summary) renders as an unformatted blob instead of a proper fenced block.
        String markdown = AssistantMarkdown.normalizeMarkdown(step);
        append("assistant", icon.isEmpty() ? markdown : icon + " " + markdown, "");
    }

    /**
     * Same glyph-per-outcome mapping the old Run timeline's custom list-cell renderer used to paint
     * (issue #3546): prefixing the bubble text keeps that at-a-glance success/failure/pending signal
     * now that milestones are chat bubbles instead of a custom-rendered list (issue #3695).
     */
    private static String agentMilestoneIcon(String step) {
        if (step.startsWith("Completed")) {
            return ShaftStatusPresentation.SUCCESS_ICON;
        }
        if (step.startsWith("Failed")) {
            return ShaftStatusPresentation.ERROR_ICON;
        }
        if (step.startsWith("Cancelled") || step.startsWith("Killed") || "Denied".equals(step)) {
            return ShaftStatusPresentation.WARNING_ICON;
        }
        if ("Running".equals(step) || step.startsWith("Tool selected: ")
                || "Cancelling...".equals(step) || "Killing...".equals(step)
                || "Waiting for approval".equals(step)) {
            return ShaftStatusPresentation.PENDING_ICON;
        }
        return "";
    }

    /** A terminal entry ends the current run's milestone stream; only one is ever recorded per run
     * (see {@link #resetAgentMilestones}). */
    private static boolean isTerminalStep(String step) {
        return step.startsWith("Completed") || step.startsWith("Failed")
                || step.startsWith("Cancelled") || step.startsWith("Killed");
    }

    /** Appends a terminal milestone bubble with an elapsed-time suffix, e.g. "Completed (12s)". */
    private void appendTerminalAgentMilestone(String step) {
        appendAgentMilestone(step + elapsedMilestoneSuffix());
    }

    private String elapsedMilestoneSuffix() {
        long elapsedSeconds = (System.nanoTime() - runStartNanos) / 1_000_000_000L;
        if (elapsedSeconds < 1) {
            return " (<1s)";
        }
        if (elapsedSeconds < 60) {
            return " (" + elapsedSeconds + "s)";
        }
        long minutes = elapsedSeconds / 60;
        long seconds = elapsedSeconds % 60;
        return " (" + minutes + "m " + seconds + "s)";
    }

    List<ContextSuggestion> contextSuggestionsForTest(char trigger) {
        return contextSuggestions(trigger, project, openFileContext(project));
    }

    void simulateAppendForTest(String role, String message, String rawResponse) {
        append(role, message, rawResponse);
    }

    /**
     * Slash-command autocomplete (issue #3540, reversing the #3428 retirement): mirrors the
     * {@code @}/{@code #} mechanism ({@link #bindContextInsertion()}, {@link
     * #showContextSuggestions(char)}, {@link #populateContextPopup(List)}, {@link
     * #insertContextSuggestion(char, ContextSuggestion)}) with the {@code case '/'} below, backed by
     * {@link AssistantCommand#commandHints(boolean)}.
     */
    private List<ContextSuggestion> contextSuggestions(
            char trigger,
            Project project,
            AssistantCommand.OpenFileContext openFileContext) {
        if (trigger == '@') {
            return workflowContextSuggestions();
        }
        if (trigger == '#') {
            return projectContextSuggestions(project, openFileContext);
        }
        if (trigger == '/') {
            return slashCommandSuggestions(expertEnabled());
        }
        return List.of();
    }

    private boolean expertEnabled() {
        return settings != null && settings.advancedUiEnabled;
    }

    private static List<ContextSuggestion> workflowContextSuggestions() {
        // Plain-language prefills only: the assistant routes these by intent, so the palette never
        // teaches command syntax the user would have to remember.
        return List.of(
                new ContextSuggestion("@workflow:record-web", "Record my browser actions on https://"),
                new ContextSuggestion("@workflow:record-mobile",
                        "Record my mobile actions on the Android emulator"),
                new ContextSuggestion("@workflow:codegen",
                        "Generate a SHAFT test from "),
                new ContextSuggestion("@workflow:doctor", "Diagnose my last failed test run"),
                new ContextSuggestion("@workflow:upgrade", "Upgrade this project to the latest SHAFT"));
    }

    /**
     * Command entries for the {@code /} popup: canonical name, summary, and example from each
     * {@link AssistantCommand.CommandHint}, core-only unless Expert mode is on. Synonyms are folded
     * into {@code matchText} (never shown) so filtering by an alias, e.g. typing {@code /docs},
     * still surfaces the {@code /guide} entry.
     */
    private static List<ContextSuggestion> slashCommandSuggestions(boolean expertEnabled) {
        return AssistantCommand.commandHints(expertEnabled).stream()
                .map(hint -> new ContextSuggestion(
                        "<html><b>" + hint.canonical() + "</b> — " + hint.summary()
                                + "<br><small>" + hint.example() + "</small></html>",
                        hint.canonical() + " ",
                        hint.canonical() + " " + String.join(" ", hint.synonyms())))
                .toList();
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
        // Live filtering: while the dropdown is open, every keystroke after the trigger narrows
        // the suggestion list in place (Backspace past the trigger, a space, or a send closes it).
        prompt.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent event) {
                SwingUtilities.invokeLater(ShaftAssistantPanel.this::refreshContextPopupFilter);
                SwingUtilities.invokeLater(ShaftAssistantPanel.this::refreshSeleniumPasteHint);
            }

            @Override
            public void removeUpdate(DocumentEvent event) {
                SwingUtilities.invokeLater(ShaftAssistantPanel.this::refreshContextPopupFilter);
                SwingUtilities.invokeLater(ShaftAssistantPanel.this::refreshSeleniumPasteHint);
            }

            @Override
            public void changedUpdate(DocumentEvent event) {
                // Plain-text documents never fire attribute-only changes for typed text.
            }
        });
    }

    /**
     * Slash commands are only offered when "/" opens the composer line, matching the Slack / Discord /
     * CLI convention (issue #3550); a "/" inside a URL or path (e.g. {@code https://}) must not pop the
     * command menu. "@" and "#" keep firing anywhere, matching their existing in-text mention behaviour.
     */
    static boolean slashTriggerAllowed(String textBeforeTrigger) {
        return textBeforeTrigger == null || textBeforeTrigger.isBlank();
    }

    private void showContextSuggestions(char trigger) {
        hideContextPopup();
        if (trigger == '/'
                && !slashTriggerAllowed(prompt.getText().substring(0, Math.max(0, prompt.getCaretPosition() - 1)))) {
            return;
        }
        List<ContextSuggestion> suggestions = contextSuggestionsForTest(trigger);
        if (suggestions.isEmpty()) {
            setStatus(trigger == '#' ? "No project context available"
                    : trigger == '/' ? "No matching command"
                    : "No Assistant context available");
            return;
        }
        if (!prompt.isShowing()) {
            return;
        }
        contextPopupTrigger = trigger;
        contextTriggerOffset = Math.max(0, prompt.getCaretPosition() - 1);
        contextPopup = new JPopupMenu("Assistant context suggestions");
        contextPopup.getAccessibleContext().setAccessibleName("Assistant context suggestions");
        contextPopup.setFocusable(false);
        populateContextPopup(suggestions);
        contextPopup.show(prompt, JBUI.scale(8), Math.max(JBUI.scale(18), prompt.getHeight() - JBUI.scale(4)));
    }

    private void populateContextPopup(List<ContextSuggestion> suggestions) {
        char trigger = contextPopupTrigger;
        contextPopup.removeAll();
        if (suggestions.isEmpty()) {
            JMenuItem none = new JMenuItem("No matching entry — keep typing or press Esc");
            none.setEnabled(false);
            contextPopup.add(none);
        }
        for (ContextSuggestion suggestion : suggestions) {
            JMenuItem item = new JMenuItem(suggestion.label());
            item.getAccessibleContext().setAccessibleName("Insert " + suggestion.matchText());
            item.addActionListener(event -> insertContextSuggestion(trigger, suggestion));
            contextPopup.add(item);
        }
        contextPopup.pack();
        contextPopup.revalidate();
        contextPopup.repaint();
    }

    /**
     * Narrows the open trigger dropdown to entries matching what the user typed after the trigger
     * character, so `@rec` immediately shows the recording workflows instead of the full list.
     * Deleting the trigger or typing whitespace dismisses the dropdown.
     */
    private void refreshContextPopupFilter() {
        if (contextPopup == null || !contextPopup.isVisible()) {
            return;
        }
        String text = prompt.getText();
        int caret = prompt.getCaretPosition();
        if (contextTriggerOffset >= text.length()
                || text.charAt(contextTriggerOffset) != contextPopupTrigger
                || caret <= contextTriggerOffset) {
            hideContextPopup();
            return;
        }
        String filter = text.substring(contextTriggerOffset + 1, Math.min(caret, text.length()));
        if (filter.chars().anyMatch(Character::isWhitespace)) {
            hideContextPopup();
            return;
        }
        populateContextPopup(filteredContextSuggestions(contextPopupTrigger, filter));
    }

    List<ContextSuggestion> filteredContextSuggestions(char trigger, String filter) {
        String needle = (trigger + filter).toLowerCase(Locale.ROOT);
        return contextSuggestionsForTest(trigger).stream()
                .filter(suggestion -> suggestion.matchText().toLowerCase(Locale.ROOT).contains(needle)
                        || suggestion.matchText().toLowerCase(Locale.ROOT).contains(filter.toLowerCase(Locale.ROOT)))
                .toList();
    }

    private void insertContextSuggestion(char trigger, ContextSuggestion suggestion) {
        // Capture the trigger offset before hiding the popup: hideContextPopup() resets it to -1,
        // and reading it afterwards made this method call text.charAt(-1) (issue #3426 B1).
        int triggerOffset = contextTriggerOffset;
        hideContextPopup();
        int caret = prompt.getCaretPosition();
        String text = prompt.getText();
        // Replace the trigger character plus any filter text typed after it, so picking a
        // suggestion after typing "@rec" leaves exactly one clean insertion in the prompt.
        int start = triggerOffset >= 0 && triggerOffset < text.length() && triggerOffset < caret
                && text.charAt(triggerOffset) == trigger
                ? triggerOffset
                : (caret > 0 && caret <= text.length() && text.charAt(caret - 1) == trigger ? caret - 1 : caret);
        if (start < caret) {
            prompt.replaceRange(suggestion.insertion(), start, caret);
        } else {
            prompt.insert(suggestion.insertion(), caret);
        }
        prompt.setCaretPosition(start + suggestion.insertion().length());
        prompt.requestFocusInWindow();
        setStatus("Inserted " + suggestion.matchText());
    }

    /**
     * Guardrails-on-paste (issue #3425 B7): when the composer content reads as native
     * Selenium/Appium Java, proactively offer one-click conversion instead of waiting for the
     * user to discover the guardrail check.
     */
    private void refreshSeleniumPasteHint() {
        if (convertSeleniumHint == null) {
            return;
        }
        String text = prompt.getText();
        boolean seleniumDetected = text != null && text.length() > 40
                && !text.startsWith("Convert this Selenium")
                && AssistantMarkdown.looksLikeNativeSelenium(text);
        if (seleniumDetected != convertSeleniumHint.isVisible()) {
            convertSeleniumHint.setVisible(seleniumDetected);
            revalidate();
            repaint();
        }
    }

    private void wrapPromptAsSeleniumConversion() {
        String code = prompt.getText().trim();
        prompt.setText("""
                Convert this Selenium code to SHAFT syntax (SHAFT.GUI.WebDriver, driver.browser(), \
                driver.element(), SHAFT locator builder; no raw Selenium), then run \
                test_code_guardrails_check on the converted result and include its verdict:

                ```java
                %s
                ```""".formatted(code));
        prompt.setCaretPosition(0);
        convertSeleniumHint.setVisible(false);
        setStatus("Conversion request ready — press send");
    }

    private void hideContextPopup() {
        if (contextPopup != null) {
            contextPopup.setVisible(false);
            contextPopup = null;
        }
        contextTriggerOffset = -1;
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

    /** Package-private (not private): reused by {@link AssistantAttachment#displayName()} so an
     * attachment chip's label uses the same file-name-only convention as the rest of the panel. */
    static String fileName(String path) {
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
        approvedToolsThisRun.clear();
        resetAgentMilestones("Prompt received");
        AssistantCommand.Selection route = selectedRoute();
        boolean agentMode = "AGENT".equals(String.valueOf(mode.getSelectedItem()));
        String selectedMode = String.valueOf(mode.getSelectedItem());
        String workingDirectory = project == null || project.getBasePath() == null ? "" : project.getBasePath();
        String conversationContext = conversationContextForPrompt();
        // Issue #3727: attachments fold into the outbound prompt text only (never into the
        // display bubble appended below, and never into the capture-review approval path, which
        // carries no free-text prompt for attachments to join).
        String attachmentsContext = AssistantAttachments.outboundBlock(attachments);
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
                conversationContext,
                attachmentsContext);
        invocation = routeNaturalStopToActiveRecorder(text, invocation);
        if (!approvingCaptureReview
                && !ShaftProjectDetector.isShaftProject(project)
                && AssistantCommand.requiresShaftProject(invocation)) {
            invocation = AssistantCommand.shaftProjectRequiredNudge(invocation.toolName());
        }
        // Display-only: the transcript/chat-state bubble shows exactly what the user typed -- text
        // is already trimmed above and normalizeMarkdown() here only ever affected what got
        // rendered/persisted, never the actual invocation (built from `text` directly at
        // AssistantCommand.fromPrompt() above), so this never changes agent behavior.
        append("user", text, "");
        if (!approvingCaptureReview && AssistantCommand.requiresAgentModeForMcp(text, selectedMode, invocation)) {
            appendTerminalAgentMilestone("Failed");
            setRunning(false, "Switch to Agent mode");
            showGateConfirmation(
                    project,
                    "This request needs MCP tool access.",
                    "MCP tool access gate",
                    "Switch to Agent mode and resend",
                    () -> mode.setSelectedItem("AGENT"));
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
            appendTerminalAgentMilestone("Failed");
            setRunning(false, "Approve source edits");
            showGateConfirmation(
                    project,
                    "To let the agent make source edits, enable **Allow source edits**.",
                    "Source edit approval gate",
                    "Allow source edits and resend",
                    () -> allowSourceMutation.setSelected(true));
            return;
        }
        prompt.setText("");
        if (!attachments.isEmpty()) {
            attachments = List.of();
            refreshAttachmentsChipRow();
        }
        if (invocation.isLocal()) {
            appendTerminalAgentMilestone("Completed");
            showLocalResponse(invocation.localResponse());
            return;
        }
        if (requiresMcpSetup(invocation, mcpConfigured())) {
            appendTerminalAgentMilestone("Failed");
            showLocalResponse("Configure SHAFT MCP in Settings before running this Assistant feature command.");
            setStatus("Configure MCP");
            return;
        }
        if (AssistantLocalAgentRunner.supports(invocation)) {
            captureIntegrationRunning = approvingCaptureReview;
            maybeAnnounceOrchestrationStages(text);
            int streamToken = ++localAgentStreamToken;
            appendAgentMilestone("Tool selected: local assistant");
            appendAgentMilestone("Running");
            setRunning(true, "Thinking...");
            appendStreamingLocalAgentBubble(streamToken);
            // readAsync calls its output consumer once per stdout/stderr line, from both reader
            // threads concurrently -- coalesce into throttled ~100ms batch flushes on the EDT instead
            // of one invokeLater per line (issue #3751 part 2, HIGH finding 2).
            localAgentOutputCoalescer = new LocalAgentOutputCoalescer(
                    batch -> batch.forEach(line -> appendLocalAgentOutput(streamToken, line)),
                    flush -> {
                        Timer timer = new Timer(100, event -> flush.run());
                        timer.setRepeats(false);
                        timer.start();
                    });
            currentInvocation = AssistantLocalAgentRunner.startWithOptionalCompact(
                    invocation,
                    autoCompact.isSelected(),
                    localAgentOutputCoalescer::enqueue,
                    localAgentApprovalHandler(streamToken));
            currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                    () -> showAgentResult(streamToken, result, error)));
            return;
        }
        rememberCaptureInvocation(text, invocation);
        startMcpInvocation(invocation);
    }

    /**
     * Shows a one-click confirmation bubble for a SHAFT pre-flight gate (issue #3681): SHAFT's own
     * command layer already knows exactly which control resolves the gate and that the choice is a
     * deterministic yes/no, so this renders {@code markdown} plus a single button that flips that
     * control and resends {@link #lastPrompt} -- instead of the plain-text prose the panel used to
     * hand the user via {@link #showResponse}, which left finding and flipping the control as a
     * manual, out-of-band step. Reuses the same ephemeral {@link AssistantTranscriptView#showWidget}
     * slot as {@link ToolApprovalPromptPanel} and the first-run welcome, so the bubble is cleared
     * automatically by the next {@link #append} call -- including the one at the very top of the
     * {@link #send} triggered by clicking the button itself.
     *
     * @param project current project, forwarded to {@link #rerun(Project)} for the resend
     * @param markdown gate explanation shown above the fix button
     * @param accessibleBubbleName distinguishes this gate's bubble from other {@code showWidget} bubbles
     * @param buttonLabel visible and accessible name of the one-click fix button
     * @param applyFix flips the exact control this gate needs (e.g. the mode combo box or the
     *                 source-mutation checkbox) before the prompt is resent
     */
    private void showGateConfirmation(
            Project project,
            String markdown,
            String accessibleBubbleName,
            String buttonLabel,
            Runnable applyFix) {
        JButton fix = new JButton(buttonLabel);
        fix.getAccessibleContext().setAccessibleName(buttonLabel);
        fix.addActionListener(event -> {
            transcript.clearWidget();
            applyFix.run();
            rerun(project);
        });
        transcript.showWidget("assistant", transcript.assistantBubbleWithActions(markdown, fix, accessibleBubbleName));
        runOnEdt(fix::requestFocusInWindow);
    }

    private void startMcpInvocation(AssistantCommand.Invocation invocation) {
        if (invocation.isSequence()) {
            appendAgentMilestone("Tool selected: sequence");
            startToolSequence(invocation.toolCalls());
            return;
        }
        gateTool(invocation.toolName(), invocation.arguments(),
                () -> dispatchApprovedTool(invocation),
                () -> showDeniedToolResult(invocation.toolName()));
    }

    private void dispatchApprovedTool(AssistantCommand.Invocation invocation) {
        appendAgentMilestone("Tool selected: " + invocation.toolName());
        appendAgentMilestone("Running");
        // The sticky capture-review strip is keyed off captureReviewGenerationRunning, which the
        // record -> stop -> generate flow sets in startCaptureCodeReview(). A direct invocation of
        // the same review/replay tools (e.g. an explicit /codegen <recording.json> or "review
        // recording" command) must arm the same gate so showResult() shows the persistent review
        // strip regardless of which entry point produced the result (issue #3500 A7).
        if (isRecordingCodeReviewTool(invocation.toolName())) {
            captureReviewGenerationRunning = true;
        }
        String narration = toolRunNarration(invocation.toolName());
        if (!narration.isBlank()) {
            append("assistant", narration, "");
        }
        if (verboseLocalAgentOutput()) {
            // Verbose promises the user the unfiltered picture, so MCP tool runs echo the exact
            // request being sent — not only local agent CLI streams (issue #3426 B5).
            append("assistant", "**Verbose — exact tool request**\n\n```json\n"
                    + "{\"tool\": \"" + invocation.toolName() + "\", \"arguments\": "
                    + invocation.arguments() + "}\n```", "");
        }
        // #3513 A8: name the routed tool in a plain-language "Running: <tool> …" confirmation.
        setRunning(true, "Running: " + invocation.toolName() + " …");
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startTool(
                invocation.toolName(), invocation.arguments(), this::onToolProgress);
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showResult(invocation.toolName(), result, error)));
    }

    /**
     * Renders a streamed {@code notifications/progress} milestone as its own chat bubble as it
     * arrives (issue #3695; previously a "run timeline" list entry), so a long tool call (for
     * example {@code capture_generate_replay}) shows what it is doing instead of sitting on a bare
     * "Running" line the whole time (issue #3546). The server streams progress best-effort for
     * opted-in tools only; every other tool call never invokes this. Called from the MCP client's
     * background thread, so UI updates are marshaled to the EDT.
     */
    private void onToolProgress(ShaftMcpProgress progress) {
        String message = progress.message();
        if (message == null || message.isBlank()) {
            return;
        }
        ApplicationManager.getApplication().invokeLater(() -> appendAgentMilestone(message));
    }

    /**
     * Up-front plain-language explanation for long-running tools with visible side effects, so the
     * user is never left watching an unexplained browser window and a spinner (issue #3426 B4).
     */
    static String toolRunNarration(String toolName) {
        return switch (toolName == null ? "" : toolName) {
            case "capture_generate_replay" -> """
                    **Generating your test now.** SHAFT is doing three things, in order:
                    1. Reading the recording and generating a Java TestNG class from it,
                    2. Compiling that class against SHAFT to prove it is valid Java,
                    3. Replaying the compiled test in a real browser to prove it works — so a browser window may open (it starts on `about:blank` before the test navigates).

                    _This usually takes a minute or two. A step-by-step report and the generated code will appear here when it finishes._""";
            case "capture_code_blocks" -> """
                    **Generating review code from your recording.** SHAFT converts the recorded steps into a Java TestNG class without executing it — no browser will open for this step.

                    _The generated code will appear here for your review in a few seconds._""";
            default -> "";
        };
    }

    private void showDeniedToolResult(String toolName) {
        setRunning(false, "Denied " + toolName);
        appendAgentMilestone("Denied");
        showResponse("**SHAFT Assistant (" + toolName + " denied)**\n\nThe request was denied.", "");
    }

    private void startToolSequence(List<AssistantCommand.ToolCall> toolCalls) {
        currentToolSequence = List.copyOf(toolCalls);
        sequenceMarkdown = new StringBuilder();
        sequenceRawOutput = new StringBuilder();
        runNextSequenceCall(0);
    }

    private void runNextSequenceCall(int index) {
        if (index >= currentToolSequence.size()) {
            appendTerminalAgentMilestone("Completed");
            setRunning(false, READY_STATUS);
            showResponse("**SHAFT Assistant sequence OK**\n\n" + sequenceMarkdown, sequenceRawOutput.toString());
            clearSequenceState();
            return;
        }
        AssistantCommand.ToolCall toolCall = currentToolSequence.get(index);
        gateTool(toolCall.toolName(), toolCall.arguments(),
                () -> dispatchApprovedSequenceTool(index, toolCall),
                () -> showDeniedSequenceResult(toolCall));
    }

    private void dispatchApprovedSequenceTool(int index, AssistantCommand.ToolCall toolCall) {
        setRunning(true, "Running: " + toolCall.toolName() + " (" + (index + 1) + "/" + currentToolSequence.size() + ") …");
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startTool(
                toolCall.toolName(), toolCall.arguments(), this::onToolProgress);
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showSequenceResult(index, toolCall, result, error)));
    }

    private void showDeniedSequenceResult(AssistantCommand.ToolCall toolCall) {
        sequenceMarkdown.append("### ")
                .append(toolCall.toolName())
                .append(" denied")
                .append("\n\n")
                .append("The request was denied.")
                .append("\n\n");
        setRunning(false, "Denied " + toolCall.toolName());
        appendAgentMilestone("Denied");
        showResponse("**SHAFT Assistant sequence denied**\n\n" + sequenceMarkdown, sequenceRawOutput.toString());
        clearSequenceState();
    }

    /**
     * Gates a SHAFT MCP tool call behind the shared approval flow: tools already approved
     * (permanently, via approve-all, or already decided once in this run) dispatch immediately;
     * anything else renders a {@link ToolApprovalPromptPanel} bubble in the transcript and waits
     * for the user's decision before proceeding. A tool is prompted at most once per distinct name
     * within a single run (tracked by {@link #approvedToolsThisRun}), so a sequence that repeats the
     * same tool never prompt-storms the user.
     *
     * @param toolName MCP tool name about to be dispatched
     * @param arguments tool arguments shown in the approval prompt
     * @param onApproved invoked once the tool is approved (already, or via a fresh decision)
     * @param onDenied invoked if the user denies the tool call
     */
    private void gateTool(String toolName, JsonObject arguments, Runnable onApproved, Runnable onDenied) {
        // Direct dispatch: the panel itself executes the MCP call, so every approval scope SHAFT
        // supports is offered regardless of which agent family is selected -- the selected agent
        // is not involved in running the tool.
        gateTool(toolName, arguments, ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD,
                onApproved, onDenied);
    }

    private void gateTool(String toolName, JsonObject arguments,
                          ToolApprovalPromptPanel.AgentApprovalCapability capability,
                          Runnable onApproved, Runnable onDenied) {
        if (approvedToolsThisRun.contains(toolName) || approvalService().isApproved(toolName)) {
            approvedToolsThisRun.add(toolName);
            onApproved.run();
            return;
        }
        setStatus("Awaiting approval for " + toolName + "...");
        requestToolApproval(toolName, arguments, capability, decision -> {
            if (decision == ToolApprovalDecision.DENY) {
                onDenied.run();
            } else {
                approvedToolsThisRun.add(toolName);
                onApproved.run();
            }
        });
    }

    /**
     * Renders an interactive {@link ToolApprovalPromptPanel} bubble in the transcript and resolves
     * {@code onDecision} once the user clicks a button. The button listener only completes a
     * {@link CompletableFuture}; the continuation (recording the decision, clearing the widget,
     * appending the outcome, and invoking {@code onDecision}) is marshaled back onto the EDT via
     * {@link #runOnEdt} so no blocking {@code get()}/{@code join()} ever runs on the EDT.
     */
    private void requestToolApproval(String toolName, JsonObject arguments,
                                     ToolApprovalPromptPanel.AgentApprovalCapability capability,
                                     Consumer<ToolApprovalDecision> onDecision) {
        CompletableFuture<ToolApprovalDecision> future = new CompletableFuture<>();
        ToolApprovalPromptPanel approvalPanel = new ToolApprovalPromptPanel(toolName, arguments, capability, future::complete);
        transcript.showWidget("assistant", approvalPanel);
        runOnEdt(approvalPanel::focusFirstDecisionButton);
        future.whenComplete((decision, error) -> runOnEdt(() -> {
            transcript.clearWidget();
            prompt.requestFocusInWindow();
            if (decision == null) {
                return;
            }
            approvalService().record(decision, toolName);
            append("assistant", approvalOutcomeMessage(toolName, decision), "");
            onDecision.accept(decision);
        }));
    }

    private static String approvalOutcomeMessage(String toolName, ToolApprovalDecision decision) {
        return switch (decision) {
            case DENY -> "Denied `" + toolName + "`.";
            case APPROVE_ONCE -> "Approved `" + toolName + "` once.";
            case APPROVE_TOOL_ALWAYS -> "Approved `" + toolName + "` in this project.";
            case APPROVE_ALL_TOOLS -> "Approved all SHAFT tools in this project.";
        };
    }

    /**
     * Returns the tool approval service: the real project-level service when running inside an IDE,
     * or a per-panel fallback instance in headless/unit-test contexts where no
     * {@link ApplicationManager} application is bootstrapped. Tests may also inject a stub directly
     * via the {@code approvalServiceOverride} field.
     */
    private ToolApprovalService approvalService() {
        if (approvalServiceOverride != null) {
            return approvalServiceOverride;
        }
        if (ApplicationManager.getApplication() != null && project != null) {
            return ToolApprovalService.getInstance(project);
        }
        approvalServiceOverride = new ToolApprovalService();
        return approvalServiceOverride;
    }

    private void showSequenceResult(
            int index,
            AssistantCommand.ToolCall toolCall,
            ShaftMcpToolResult result,
            Throwable error) {
        boolean cancelled = error instanceof CancellationException;
        boolean success = error == null && result != null && result.success();
        String output = resolveOutput(result, error, "No result returned.");
        boolean rejectedGeneratedJava = AssistantMarkdown.containsRejectedGeneratedJava(output);
        if (rejectedGeneratedJava) {
            showRejectedSequenceResult(toolCall, output);
            return;
        }
        if (!output.isBlank()) {
            appendToolEvidence(toolCall.toolName(), output);
        }
        String statusText = sequenceStatusText(cancelled, success);
        appendSequenceStep(toolCall, statusText, output);
        if (cancelled || !success) {
            showTerminalSequenceResult(cancelled, statusText);
            return;
        }
        runNextSequenceCall(index + 1);
    }

    private void showRejectedSequenceResult(AssistantCommand.ToolCall toolCall, String output) {
        sequenceMarkdown.append("### ")
                .append(toolCall.toolName())
                .append(" rejected")
                .append("\n\n")
                .append(AssistantMarkdown.fromMcpOutput(toolCall.toolName(), output))
                .append("\n\n");
        setRunning(false, "Rejected generated code");
        appendTerminalAgentMilestone("Failed");
        showResponse("**SHAFT Assistant sequence rejected**\n\n" + sequenceMarkdown,
                sequenceRawOutput.toString());
        clearSequenceState();
    }

    private static String sequenceStatusText(boolean cancelled, boolean success) {
        return cancelled ? "cancelled" : success ? "OK" : "failed";
    }

    private void appendSequenceStep(AssistantCommand.ToolCall toolCall, String statusText, String output) {
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
    }

    private void showTerminalSequenceResult(boolean cancelled, String statusText) {
        // Snapshot before setRunning(false, ...), which clears killRequested for the next run.
        boolean killed = cancelled && killRequested;
        String terminalStep = cancelled ? (killed ? "Killed" : "Cancelled") : "Failed";
        setRunning(false, terminalStep);
        appendTerminalAgentMilestone(terminalStep);
        showResponse("**SHAFT Assistant sequence " + statusText + "**\n\n" + sequenceMarkdown,
                sequenceRawOutput.toString());
        clearSequenceState();
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
        settings.cloudModel = editableComboText(cloudModel);
        settings.localModel = editableComboText(localModel);
        settings.assistantEffort = String.valueOf(effort.getSelectedItem());
        settings.defaultAutobotClient = clientFromFamily(settings.assistantFamily);
        if (usesCloud()) {
            settings.pilotAiProvider = settings.cloudProvider;
            settings.pilotAiModel = settings.cloudModel;
            return AssistantCommand.Selection.cloud(settings.cloudProvider, settings.cloudModel,
                    settings.assistantEffort);
        }
        return AssistantCommand.Selection.local(settings.assistantFamily, settings.assistantRuntime,
                settings.localModel, settings.assistantEffort);
    }

    private void showResult(String toolName, ShaftMcpToolResult result, Throwable error) {
        boolean cancelled = error instanceof CancellationException;
        // Snapshot before updateMcpConnectionCheckStatus's setRunning(false, ...) clears killRequested.
        boolean killed = cancelled && killRequested;
        boolean success = error == null && result != null && result.success();
        updateMcpConnectionCheckStatus(toolName, success);
        if (cancelled) {
            showCancelledToolResult(toolName, killed);
            return;
        }
        String output = resolveOutput(result, error, "No result returned.");
        boolean rejectedGeneratedJava = AssistantMarkdown.containsRejectedGeneratedJava(output);
        if (!output.isBlank() && !rejectedGeneratedJava) {
            appendToolEvidence(toolName, output);
        }
        String markdown = AssistantMarkdown.fromMcpOutput(toolName, output);
        if (rejectedGeneratedJava) {
            showRejectedToolResult(toolName, markdown);
            return;
        }
        if (showCaptureReviewApprovalIfPending(toolName, success, markdown, output)) {
            return;
        }
        if (showCaptureStopDiagnosticIfPending(toolName, success, markdown, output)) {
            return;
        }
        if (success && formatUnknownResponse(toolName, output, markdown)) {
            appendTerminalAgentMilestone("Completed");
            return;
        }
        showFinalToolResult(toolName, success, markdown, output);
    }

    private void updateMcpConnectionCheckStatus(String toolName, boolean success) {
        boolean isMcpConnectionCheck = "mcp initialize".equals(toolName);
        setRunning(false, success ? (isMcpConnectionCheck ? "MCP test passed" : READY_STATUS) : "Failed");
        if (isMcpConnectionCheck && success) {
            showTransientStatus("MCP test passed. Ready to chat.");
        }
    }

    private static String resolveOutput(ShaftMcpToolResult result, Throwable error, String noResultFallback) {
        if (error != null) {
            return AssistantMarkdown.humanizeError(error);
        }
        return result == null ? noResultFallback : result.output();
    }

    private void showCancelledToolResult(String toolName, boolean killed) {
        String terminalStep = killed ? "Killed" : "Cancelled";
        appendTerminalAgentMilestone(terminalStep);
        if (isRecordingCodeReviewTool(toolName) && captureReviewGenerationRunning) {
            captureReviewGenerationRunning = false;
            clearPendingCaptureReview();
        }
        showResponse("**SHAFT Assistant (" + toolName + " " + terminalStep.toLowerCase(Locale.ROOT) + ")**", "");
        setStatus(terminalStep);
    }

    private void showRejectedToolResult(String toolName, String markdown) {
        if (captureReviewGenerationRunning && isRecordingCodeReviewTool(toolName)) {
            captureReviewGenerationRunning = false;
        }
        showResponse("**SHAFT Assistant (" + toolName + " rejected)**\n\n" + markdown, "");
        setStatus("Rejected generated code");
        appendTerminalAgentMilestone("Failed");
    }

    private boolean showCaptureReviewApprovalIfPending(
            String toolName, boolean success, String markdown, String output) {
        boolean isCaptureReviewTool = captureReviewGenerationRunning && isRecordingCodeReviewTool(toolName);
        if (!success && isCaptureReviewTool) {
            captureReviewGenerationRunning = false;
            return false;
        }
        if (!success || !isCaptureReviewTool) {
            return false;
        }
        captureReviewGenerationRunning = false;
        pendingCaptureReview = new CaptureReview(markdown, output);
        showPendingCaptureReview();
        showResponse("**SHAFT Assistant (" + toolName + " OK)**\n\n"
                + markdown
                + "\n\n**Review before writing files.** Send `approve`, `okay`, or `generate` to let the Agent create the actual Page Object Model files.",
                output);
        setStatus("Awaiting approval");
        appendAgentMilestone("Waiting for approval");
        return true;
    }

    private boolean showCaptureStopDiagnosticIfPending(
            String toolName, boolean success, String markdown, String output) {
        boolean isCaptureStopTool = "capture_stop".equals(toolName)
                || "capture_api_stop".equals(toolName);
        if (!success || !generateCaptureReviewAfterStop || !isCaptureStopTool) {
            return false;
        }
        // A stop that armed the review DID stop the recording on both branches below (blank
        // sessionPath included), but neither reaches showFinalToolResult -- the only other place
        // that clears the readiness strip's recording badge for stop tools (issue #3767).
        ShaftRecordingActivity.stopped(recordingKey);
        stopCaptureStartDiagnostic();
        if ("capture_api_stop".equals(toolName)) {
            String sessionPath = apiSessionPathFromStopOutput(output);
            if (sessionPath.isBlank()) {
                // Issue #3739: capture_api_generate needs a persisted session path that this stop
                // result did not report -- skip the generate call rather than send it a blank
                // sessionPath, and clear the armed flag so a later unrelated stop can't misfire it.
                generateCaptureReviewAfterStop = false;
                showResponse("**SHAFT Assistant (" + toolName + " OK)**\n\n" + markdown
                        + "\n\n_No session path was returned by the stop result, so no code could be "
                        + "generated automatically. Try stopping the recording again._", output);
                return true;
            }
            activeApiRecordingPath = sessionPath;
        }
        showResponse("**SHAFT Assistant (" + toolName + " OK)**\n\n" + markdown, output);
        startCaptureCodeReview();
        return true;
    }

    /**
     * Extracts the persisted session JSON path from a {@code capture_api_stop} result's {@code
     * CaptureStatus} payload (issue #3739) -- the same parse {@link ApiRecordingSessionPanel} uses
     * to feed its own Generate button, so the chat-driven record -> stop -> generate loop hands
     * {@code capture_api_generate} the same sessionPath the Advanced-UI panel would.
     */
    private static String apiSessionPathFromStopOutput(String output) {
        JsonObject status = AssistantMarkdown.jsonObjectFromMcpOutput(output);
        return status != null && status.has("outputPath") ? status.get("outputPath").getAsString() : "";
    }

    private void showFinalToolResult(String toolName, boolean success, String markdown, String output) {
        String body = success && isRecordingCodeReviewTool(toolName) && !AssistantMarkdown.hasCodeFence(markdown)
                ? markdown + "\n\n" + NO_CODE_GENERATED_NOTE
                : markdown;
        if (verboseLocalAgentOutput() && output != null && !output.isBlank() && !output.equals(body)) {
            body = body + "\n\n**Verbose — raw tool response**\n\n"
                    + AssistantMarkdown.normalizeMarkdown(output);
        }
        // #3513 S3: failures render with a consistent short-headline + one-next-action shape; the
        // success header is unchanged.
        showResponse(success
                ? "**SHAFT Assistant (" + toolName + " OK)**\n\n" + body
                : AssistantMarkdown.toolFailureMarkdown(toolName, body), output);
        appendTerminalAgentMilestone(success ? "Completed" : "Failed");
        if (success && ("capture_start".equals(toolName) || "capture_api_start".equals(toolName))) {
            // Feeds the shared readiness strip's recording badge (issue #3500 A4). capture_api_start
            // (issue #3726) shares CaptureManager's single-session lock with capture_start, so it is
            // the same kind of active recording from the badge's point of view.
            ShaftRecordingActivity.started(recordingKey);
        }
        if (success && ("capture_stop".equals(toolName) || "capture_api_stop".equals(toolName))) {
            ShaftRecordingActivity.stopped(recordingKey);
        }
        if (success && "capture_start".equals(toolName)) {
            scheduleCaptureStartDiagnostic(output);
        }
    }

    private void showAgentResult(ShaftMcpToolResult result, Throwable error) {
        showAgentResult(-1, result, error);
    }

    private void showAgentResult(int streamToken, ShaftMcpToolResult result, Throwable error) {
        if (handleKilledOrStaleAgentStream(streamToken)) {
            return;
        }
        // Drain any lines the coalescer is still holding for its next throttled flush before this
        // terminal path reads localAgentOutput below -- otherwise the run's very last streamed lines
        // could be missing from the partial-output snapshot (issue #3751 part 2, HIGH finding 2).
        if (localAgentOutputCoalescer != null) {
            localAgentOutputCoalescer.flush();
        }
        boolean cancelled = error instanceof CancellationException;
        // Snapshot before setRunning(false, ...) clears killRequested for the next run.
        boolean killed = cancelled && killRequested;
        boolean success = error == null && result != null && result.success();
        boolean currentStream = streamToken == activeLocalAgentStreamToken;
        boolean captureIntegrationRun = captureIntegrationRunning;
        // Capture whatever the streaming bubble already rendered before it is cleared below: a plain
        // Cancel (as opposed to Kill, which finalizes synchronously in stopLocalAgentStreaming) reaches
        // this method asynchronously once the future completes, and the partial output the user already
        // saw would otherwise be silently discarded by showAgentCancelled's hardcoded "_Cancelled._".
        boolean hadPartialOutput = localAgentBubbleRendersContent && localAgentOutput != null && !localAgentOutput.isEmpty();
        String partialOutput = hadPartialOutput ? localAgentOutput.toString() : "";
        localAgentOutput = null;
        if (currentStream) {
            activeLocalAgentStreamToken = -1;
            // Defensive: under normal operation the CLI can't emit its final result while an
            // approval_prompt call is still blocked mid-round-trip, so this is not expected to find
            // anything showing -- but a stuck approval widget after the run has already ended would be
            // a dead end for the user, so clear it unconditionally rather than assume the ordering.
            clearPendingLocalAgentApprovalPrompt();
        }
        setRunning(false, success ? READY_STATUS : "Failed");
        finishCaptureIntegrationIfRunning(success, result);
        if (cancelled) {
            String terminalStep = killed ? "Killed" : "Cancelled";
            // Milestone appended AFTER the cancelled response, not before: for a live stream
            // (currentStream), showAgentCancelled() replaces the transcript's last message (see
            // finishLocalAgentResponse) -- appending the terminal milestone first would just have it
            // silently overwritten by that replace (issue #3695).
            showAgentCancelled(streamToken, currentStream, killed, partialOutput);
            appendTerminalAgentMilestone(terminalStep);
            setStatus(terminalStep);
            return;
        }
        showAgentToolResult(streamToken, currentStream, success, result, error, captureIntegrationRun);
    }

    private boolean handleKilledOrStaleAgentStream(int streamToken) {
        if (streamToken > 0 && streamToken == killedLocalAgentStreamToken) {
            killedLocalAgentStreamToken = -1;
            setRunning(false, "Killed");
            return true;
        }
        return streamToken > 0 && streamToken != activeLocalAgentStreamToken && activeLocalAgentStreamToken != -1;
    }

    private void finishCaptureIntegrationIfRunning(boolean success, ShaftMcpToolResult result) {
        if (!captureIntegrationRunning) {
            return;
        }
        // Keep the reviewed code blocks available for another approval attempt unless the agent
        // demonstrably wrote something: a "successful" CLI exit whose run lost its tool calls to
        // permission denials would otherwise silently discard the only copy of the generated code.
        if (success && captureIntegrationProducedFiles(result)) {
            clearPendingCaptureReview();
        }
        captureIntegrationRunning = false;
    }

    /**
     * Whether a finished capture-integration agent run shows evidence of real output: the activity
     * footer's file list (emitted by {@link AssistantLocalAgentRunner} from the CLI's own Write/Edit
     * calls) or at least one fenced code block in the final answer.
     */
    private static boolean captureIntegrationProducedFiles(ShaftMcpToolResult result) {
        String output = result == null ? "" : result.output();
        return output.contains("Files created or edited:") || AssistantMarkdown.hasCodeFence(output);
    }

    private void showAgentToolResult(
            int streamToken, boolean currentStream, boolean success, ShaftMcpToolResult result, Throwable error) {
        showAgentToolResult(streamToken, currentStream, success, result, error, false);
    }

    private void showAgentToolResult(
            int streamToken, boolean currentStream, boolean success, ShaftMcpToolResult result, Throwable error,
            boolean captureIntegrationRun) {
        String output = resolveOutput(result, error, "No response returned.");
        boolean rejectedGeneratedJava = AssistantMarkdown.containsRejectedGeneratedJava(output);
        if (!output.isBlank() && !rejectedGeneratedJava) {
            appendToolEvidence("autobot_local_agent_run", output);
        }
        // The raw output may carry a trailing usage-metadata JSON line (the structured-stream
        // contract from AssistantLocalAgentRunner). That line is never meant for the transcript, so
        // it is stripped before markdown normalization; withTokenUsage still receives the untouched
        // raw `output` below so AssistantLocalAgentRunner.parseTokenUsage can read the usage from it.
        String response = rejectedGeneratedJava
                ? AssistantMarkdown.nativeSeleniumRejectionMarkdown()
                : AssistantMarkdown.normalizeMarkdown(AssistantLocalAgentRunner.stripTrailingUsageMetadata(output));
        // Claude Code's own built-in "propose this plan, ask to proceed" tool call (ExitPlanMode)
        // sends its proposal in a structured "plan" field that is stripped out of `output` above
        // along with the rest of the trailing usage-metadata line, so the plan text is folded back
        // into the persisted response here -- not only into the ephemeral action card below -- so it
        // survives "Keep refining" dismissing the card and shows up in transcript export/scrollback
        // (issue #3680: today it "only surfaces, if at all, buried in the final unstructured answer").
        String planProposal = success && !rejectedGeneratedJava && "PLAN".equals(String.valueOf(mode.getSelectedItem()))
                ? AssistantLocalAgentRunner.parsePlanProposal(output)
                : null;
        boolean hasPlanProposal = planProposal != null && !planProposal.isBlank();
        if (hasPlanProposal) {
            response = response + "\n\n---\n\n**Plan proposed:**\n\n" + planProposal;
        }
        if (!rejectedGeneratedJava && captureIntegrationRun && success && !captureIntegrationProducedFiles(result)) {
            response = response + "\n\n" + ShaftStatusPresentation.WARNING_ICON
                    + " **The approved Capture code was not written to your project.** The reviewed code"
                    + " blocks are still available — send `approve` to try again (approving any tool"
                    + " prompts that appear), or copy the generated class from the review above.";
        }
        if (rejectedGeneratedJava) {
            setStatus("Rejected generated code");
        }
        // #3703: a local-agent CLI failure (a CLI timeout, a nonzero exit, ...) renders with the same
        // short-headline + details + one-next-action shape showFinalToolResult already gives MCP tool
        // failures, instead of leaving the user with only a bare "Failed" milestone bubble. The
        // rejected-generated-code narrative already carries its own headline, so it is left alone.
        if (!success && !rejectedGeneratedJava) {
            response = AssistantMarkdown.localAgentFailureMarkdown(response);
        }
        showAgentResponse(streamToken, currentStream, response, rejectedGeneratedJava ? "" : output);
        // Milestone appended AFTER the response in both branches, not before: for a live stream
        // (currentStream), showAgentResponse() replaces the transcript's last message (see
        // finishLocalAgentResponse) -- appending the terminal milestone first would just have it
        // silently overwritten by that replace (issue #3695).
        appendTerminalAgentMilestone(rejectedGeneratedJava || !success ? "Failed" : "Completed");
        if (hasPlanProposal) {
            showPlanProposalActions();
        }
    }

    /**
     * Renders a structured, clickable "proceed to Agent / keep refining" choice below Claude Code's
     * terminal {@code ExitPlanMode} proposal (whose text was just folded into the persisted response
     * bubble above, in {@link #showAgentToolResult}) -- instead of leaving the user with only a plain
     * chat bubble they must act on manually (issue #3680). Shown as a trailing ephemeral widget --
     * the same {@code transcript.showWidget} mechanism {@link #showLocalAgentApprovalPrompt} uses --
     * so it never pollutes the persisted chat history/markdown export, and is cleared automatically
     * the next time {@link #append} runs (the next prompt the user sends, in either mode).
     */
    private void showPlanProposalActions() {
        JButton runInAgentMode = button("Switch to Agent and run this plan",
                "Switch to Agent mode, allow source edits, and run the proposed plan",
                event -> proceedWithProposedPlan());
        JButton keepRefining = button("Keep refining",
                "Dismiss this plan proposal and keep discussing it in Plan mode",
                event -> transcript.clearWidget());
        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        actions.setOpaque(false);
        actions.add(runInAgentMode);
        actions.add(keepRefining);
        transcript.showWidget("assistant",
                transcript.assistantBubbleWithActions(
                        "**Ready to proceed with this plan?**", actions, "Plan proposal actions"));
    }

    /**
     * "Switch to Agent and run this plan" handler: prepares Agent-mode state, then resends the exact
     * prompt that produced the plan (mirroring the existing "Rerun" button) so the CLI now runs it
     * for real instead of only proposing it.
     */
    private void proceedWithProposedPlan() {
        prepareAgentModeForProposedPlan();
        rerun(project);
    }

    /**
     * State half of {@link #proceedWithProposedPlan}, split out so it can be verified without
     * exercising the resend/CLI-dispatch tail: auto-selects Agent mode and grants source edits (an
     * Agent run without "Allow source edits" only auto-approves file edits under {@code
     * acceptEdits} in {@code AssistantLocalAgentRunner.claudeCommand}, so a plan whose whole point is
     * to make changes needs the checkbox ticked to actually run unattended), then dismisses the card.
     */
    private void prepareAgentModeForProposedPlan() {
        mode.setSelectedItem("AGENT");
        if (!allowSourceMutation.isSelected()) {
            allowSourceMutation.setSelected(true);
        }
        transcript.clearWidget();
    }

    private void showAgentCancelled(int streamToken, boolean currentStream, boolean killed, String partialOutput) {
        String label = killed ? "Killed" : "Cancelled";
        String canceledResponse = partialOutput == null || partialOutput.isBlank()
                ? "_" + label + "._"
                : formatLocalAgentStreamingResponse(partialOutput) + "\n\n_" + label + "._ (partial output above)";
        showAgentResponse(streamToken, currentStream, canceledResponse, "");
    }

    private void showAgentResponse(int streamToken, boolean currentStream, String response, String output) {
        if (currentStream) {
            finishLocalAgentResponse(streamToken, response, output);
        } else {
            // Not showResponse(): that applies the generic withTokenUsage, which stays silent when no
            // usage metadata is found. Every local-agent terminal response -- this is the "stale/not
            // the active stream" branch, still a local-agent run -- must say so explicitly instead
            // (issue #3703), which withLocalAgentTokenUsage does.
            persistAndAppendResponse(withLocalAgentTokenUsage(response, output), output);
        }
    }

    /**
     * Always appends the streaming placeholder bubble, regardless of the Verbose toggle's state at
     * this moment: a placeholder must exist so {@link #appendLocalAgentOutput} and {@link
     * #finishLocalAgentResponse} always have exactly one bubble to update/replace, no matter how the
     * user flips Verbose mid-run. Non-verbose runs show this bare header as a brief "running"
     * placeholder instead of the prior total silence until completion -- a deliberate, minor UX
     * change that is what makes the toggle safe in both directions (see finishLocalAgentResponse).
     */
    private void appendStreamingLocalAgentBubble(int streamToken) {
        activeLocalAgentStreamToken = streamToken;
        localAgentOutput = new StringBuilder();
        localAgentBubbleRendersContent = false;
        append("assistant", LOCAL_AGENT_STREAMING_HEADER, "");
        localAgentStreamPlaceholderMessageIndex = currentSessionMessageCount() - 1;
    }

    private int currentSessionMessageCount() {
        ShaftAssistantChatState.Session active = chatState.activeSession();
        return active == null || active.messages == null ? 0 : active.messages.size();
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
            localAgentBubbleRendersContent = true;
            replaceLocalAgentStreamPlaceholder(
                    "assistant", formatLocalAgentStreamingResponse(localAgentOutput.toString()), false);
        } else {
            // Verbose gates the full streaming bubble, but a non-verbose run must not look frozen
            // either (issue #3546): surface a compact milestone bubble in the transcript instead
            // (issue #3695: this used to feed the now-removed "Run timeline" list).
            addCompactLocalAgentMilestone(line);
        }
    }

    // Tool names whose "Calling tool X..." milestone is internal harness bookkeeping with zero
    // user-relevant signal (e.g. ToolSearch, the CLI's own tool-schema lookup) rather than a real
    // action -- unlike Bash/Read/Edit/... or any mcp__-prefixed SHAFT tool, which do represent real
    // progress and must keep surfacing. Kept small and named on purpose (#3672); extend only for a
    // confirmed report of another internal meta-tool leaking into the visible milestone bubbles.
    private static final Set<String> NON_ACTIONABLE_META_TOOL_NAMES = Set.of("ToolSearch");

    /**
     * Reflects one local-agent output line as a compact agent-milestone chat bubble for non-verbose
     * runs (issue #3695; this fed a separately-scrollable "Run timeline" list before that removal).
     * Raw NDJSON lines (an unmapped Claude/Codex structured-stream event with no human-readable
     * translation, see {@code AssistantLocalAgentRunner}'s {@code StructuredStreamParser#accept})
     * always start with {@code {}; skipping those keeps this method's visibility contract identical
     * to Verbose mode's pre-#3546 raw-content boundary (#3545: raw stdout stays invisible unless the
     * user opts into Verbose) while still giving a signal of progress for everything else — translated
     * milestones ("Calling tool X...", "Thinking: ...") and plain-prose/banner lines. Sub-lines that
     * name a {@link #NON_ACTIONABLE_META_TOOL_NAMES} tool call are dropped first (#3672: internal
     * harness meta-tool calls like ToolSearch were leaking into the visible milestone bubbles). A
     * "Tool result (X): .../Tool failed (X): ..." sub-line whose content is long enough to look like
     * a raw dump rather than a short status is compacted by {@link #compactIfRawToolResult}, closing
     * a gap where non-JSON raw tool output (a Bash stdout dump, a file's full contents) still leaked
     * through as-is even with Verbose off, contradicting the "only human readable messages" contract
     * this method exists to enforce.
     */
    private void addCompactLocalAgentMilestone(String line) {
        if (line == null) {
            return;
        }
        StringBuilder filtered = new StringBuilder();
        for (String subLine : line.split("\n", -1)) {
            if (isNonActionableMetaToolCall(subLine)) {
                continue;
            }
            if (filtered.length() > 0) {
                filtered.append("\n");
            }
            filtered.append(compactIfRawToolResult(subLine));
        }
        String trimmed = filtered.toString().strip();
        if (trimmed.isEmpty() || trimmed.startsWith("{") || trimmed.startsWith("[")) {
            return;
        }
        appendAgentMilestone(trimmed);
    }

    private static final Pattern TOOL_RESULT_PREFIX =
            Pattern.compile("^(Tool (?:result|failed) \\([^)]*\\)): (.*)$");
    // A short status/warning line (the common case) is left untouched; only content long enough to
    // read as a raw dump gets compacted, so this deliberately doesn't reintroduce the earlier
    // mid-word hard-cut-at-77-chars bug that assistantAgentMilestoneBubbleShowsFullLongLineWithout-
    // MidWordTruncation guards against.
    private static final int TOOL_RESULT_INLINE_CHAR_LIMIT = 400;

    /**
     * Compacts a "Tool result (X): <content>"/"Tool failed (X): <content>" sub-line down to a short,
     * word-boundary-safe preview once {@code content} is long enough to look like raw tool output
     * (full stdout, a whole file's contents, ...) rather than a short human-readable status, pointing
     * the user at Verbose for the rest. Anything else -- including a short "Tool result" entry --
     * passes through unchanged.
     */
    private static String compactIfRawToolResult(String subLine) {
        Matcher matcher = TOOL_RESULT_PREFIX.matcher(subLine);
        if (!matcher.matches()) {
            return subLine;
        }
        String prefix = matcher.group(1);
        String content = matcher.group(2);
        if (content.length() <= TOOL_RESULT_INLINE_CHAR_LIMIT) {
            return subLine;
        }
        int cut = TOOL_RESULT_INLINE_CHAR_LIMIT;
        int wordBoundary = content.lastIndexOf(' ', cut);
        if (wordBoundary > 0) {
            cut = wordBoundary;
        }
        int hiddenChars = content.length() - cut;
        return prefix + ": " + content.substring(0, cut).stripTrailing()
                + " … (" + hiddenChars + " more characters — enable Verbose to see the full output)";
    }

    /** True for a "Calling tool X..." (or "...X (args)...") sub-line naming a denylisted meta-tool. */
    private static boolean isNonActionableMetaToolCall(String subLine) {
        String candidate = subLine.strip();
        for (String toolName : NON_ACTIONABLE_META_TOOL_NAMES) {
            String prefix = "Calling tool " + toolName;
            if (candidate.equals(prefix + "...") || candidate.startsWith(prefix + " (")) {
                return true;
            }
        }
        return false;
    }

    /**
     * Replaces the streaming placeholder bubble appended by {@link #appendStreamingLocalAgentBubble}
     * with the final answer. Always a replace, never a fresh {@link #append}: the placeholder is now
     * unconditionally present (see that method), so an unconditional replace here is what keeps a
     * mid-run Verbose toggle from leaving a stale placeholder behind or clobbering the wrong message --
     * both real bugs when this branched on the live checkbox instead.
     */
    private void finishLocalAgentResponse(int streamToken, String response, String rawResponse) {
        if (streamToken != activeLocalAgentStreamToken && activeLocalAgentStreamToken != -1) {
            return;
        }
        String displayResponse = withLocalAgentTokenUsage(response, rawResponse);
        ResolvedQuestion resolved = resolveQuestion(displayResponse, rawResponse);
        replaceLocalAgentStreamPlaceholder("assistant", resolved.toPersist(), true);
        localAgentStreamPlaceholderMessageIndex = -1;
        lastResponse = resolved.toPersist();
        lastRawResponse = rawResponse == null ? "" : rawResponse;
        copyLastResponse.setEnabled(true);
        copyRawResponse.setEnabled(!lastRawResponse.isBlank());
        updateActionChrome();
        if (resolved.question() != null) {
            showAssistantQuestionOptions(resolved.question());
        }
    }

    /**
     * The clarifying question detected for a terminal response, if any, and the display text to
     * persist alongside it -- bundled together because the two are not independent: a runner-
     * recognized structured question (issue #3719) was already stripped out of {@code
     * displayResponse} upstream by {@code AssistantLocalAgentRunner}'s {@code
     * StructuredStreamParser}, so {@code displayResponse} itself is already the text to persist,
     * while a bare structured line or fence match is still embedded verbatim in {@code
     * displayResponse} and must be stripped via {@code question.promptMarkdown()} here instead.
     */
    private record ResolvedQuestion(AssistantQuestion question, String toPersist) {
    }

    /**
     * Resolves the clarifying question for a terminal response, trying -- in order -- (1) {@link
     * AssistantLocalAgentRunner#parseQuestion} on {@code rawResponse}: the runner-recognized
     * structured protocol (issue #3719), populated only for local Claude Code/Codex runs whose
     * {@code StructuredStreamParser} found a valid trailing structured line; (2) {@link
     * AssistantQuestion#detectStructuredLine} directly on {@code displayResponse}: the same
     * structured protocol, for paths with no runner envelope at all (cloud chat, whose raw answer
     * text is never wrapped by {@code AssistantLocalAgentRunner}); (3) {@link AssistantQuestion#detect}:
     * today's {@code shaft-options} fence, the universal, documented fallback for every other case
     * (Copilot CLI, custom commands, or a model that never attempted the structured form). A
     * compliant CLI is recognized however far along this chain it lands; a non-compliant one still
     * renders the plain answer exactly as before this issue.
     */
    private static ResolvedQuestion resolveQuestion(String displayResponse, String rawResponse) {
        AssistantQuestion runnerRecognized = AssistantLocalAgentRunner.parseQuestion(rawResponse);
        if (runnerRecognized != null) {
            // When tier 1 (structured protocol via runner) succeeds, the StructuredStreamParser has
            // already stripped the structured JSON line from displayResponse, but any shaft-options
            // fence that was also present should be stripped too (issue #3719).
            String strippedDisplay = AssistantQuestion.stripOptionsFence(displayResponse);
            return new ResolvedQuestion(runnerRecognized, strippedDisplay);
        }
        AssistantQuestion question = AssistantQuestion.detectStructuredLine(displayResponse);
        if (question == null) {
            question = AssistantQuestion.detect(displayResponse);
        }
        return new ResolvedQuestion(question, question == null ? displayResponse : question.promptMarkdown());
    }

    /**
     * Shows a detected {@link AssistantQuestion}'s answer chips as a trailing transcript widget
     * (issue #3674), reusing the same ephemeral single-slot mechanism {@link ToolApprovalPromptPanel}
     * uses. Unlike an approval decision, a chip click is non-destructive -- see {@link
     * AssistantQuestionOptionsPanel} -- so this widget is never explicitly cleared; it is simply
     * replaced or cleared the next time {@link #append} runs, which happens as soon as the user's
     * next message (chip-filled or free-typed) is sent.
     */
    private void showAssistantQuestionOptions(AssistantQuestion question) {
        AssistantQuestionOptionsPanel optionsPanel = new AssistantQuestionOptionsPanel(
                question.options(), this::fillPromptWithSuggestedAnswer, this::focusPromptForCustomAnswer);
        transcript.showWidget("assistant", optionsPanel);
    }

    /**
     * Fills the composer with a chosen suggested answer instead of auto-sending it, matching {@link
     * #emptyStateChip}'s existing "review and send it yourself" convention -- the always-available
     * free-text fallback the issue calls for stays exactly as-is; a chip is just a faster way to
     * populate it.
     */
    private void fillPromptWithSuggestedAnswer(String answer) {
        prompt.setText(answer);
        prompt.setCaretPosition(answer.length());
        prompt.requestFocusInWindow();
    }

    /**
     * Handles the question panel's "Answer myself" button: moves focus to the composer without
     * prefilling any suggested text, so a user who wants to answer in their own words has a visible,
     * clickable affordance for that instead of needing to already know the always-available prompt
     * field doubles as the free-text fallback.
     */
    private void focusPromptForCustomAnswer() {
        prompt.requestFocusInWindow();
        // DEFECT 2 fix: if a suggested-answer chip was clicked first, its text remains in the
        // composer. Selecting all existing text lets typing replace it while keeping recovery
        // possible (user can undo or Ctrl+Z to restore the chip's text).
        prompt.selectAll();
    }

    private boolean verboseLocalAgentOutput() {
        return verboseAgentOutput != null && verboseAgentOutput.isSelected();
    }

    private void stopLocalAgentStreaming() {
        if (activeLocalAgentStreamToken > 0) {
            // Same reasoning as showAgentResult: drain the coalescer's not-yet-flushed lines before
            // reading localAgentOutput for the "Killed" finalization below.
            if (localAgentOutputCoalescer != null) {
                localAgentOutputCoalescer.flush();
            }
            killedLocalAgentStreamToken = activeLocalAgentStreamToken;
            // A killed run never reaches finishLocalAgentResponse (handleKilledOrStaleAgentStream
            // short-circuits the eventual async completion for this stream token), so this synchronous
            // path must append the terminal marker itself -- otherwise a bubble that already rendered
            // live content would be left frozen with no indication the run was killed, and a bubble
            // that never rendered anything would be left showing the bare "Running local assistant..."
            // header forever.
            String finalized = localAgentBubbleRendersContent
                    ? formatLocalAgentStreamingResponse(localAgentOutput.toString())
                            + "\n\n_Killed._ (partial output above)"
                    : "_Killed._";
            replaceLocalAgentStreamPlaceholder("assistant", finalized, true);
            localAgentStreamPlaceholderMessageIndex = -1;
        }
        activeLocalAgentStreamToken = -1;
        localAgentOutput = null;
        clearPendingLocalAgentApprovalPrompt();
    }

    /**
     * Returns the callback a {@link LocalAgentApprovalBridge} invokes on one of its own HTTP-handling
     * threads to ask SHAFT for a real, interactive per-tool decision mid-run. Never touches Swing
     * directly (that thread is not the EDT): it marshals the request onto the EDT via {@link
     * #runOnEdt} and returns a future that {@link #handleLocalAgentApprovalRequest} completes once
     * the user (or a stale-run/queue check) decides.
     */
    /**
     * Whether a local-agent tool-approval request targets a SHAFT MCP tool
     * ({@code mcp__shaft-mcp__*}). Package-private for tests.
     */
    static boolean isShaftMcpTool(String toolName) {
        return toolName != null && toolName.startsWith("mcp__shaft-mcp__");
    }

    private LocalAgentApprovalBridge.ApprovalRequestHandler localAgentApprovalHandler(int streamToken) {
        return (toolName, input) -> {
            CompletableFuture<LocalAgentApprovalBridge.Decision> future = new CompletableFuture<>();
            runOnEdt(() -> handleLocalAgentApprovalRequest(streamToken, toolName, input, future));
            return future;
        };
    }

    /**
     * Resolves a local-agent tool-approval request on the EDT: already-approved tools (this run,
     * permanently, or via the local-agent "approve all" sentinel -- see {@link
     * #LOCAL_AGENT_APPROVE_ALL_KEY}) allow immediately, mirroring {@link #gateTool}'s once-per-run
     * dedupe. A genuinely new request renders a {@link ToolApprovalPromptPanel} bubble via the same
     * ephemeral {@code transcript.showWidget} mechanism {@link #requestToolApproval} uses for SHAFT
     * MCP tool calls, or queues behind one already showing (the transcript has a single widget slot,
     * and a CLI can in principle request more than one approval before the first is answered).
     */
    private void handleLocalAgentApprovalRequest(
            int streamToken, String toolName, JsonObject input,
            CompletableFuture<LocalAgentApprovalBridge.Decision> future) {
        if (streamToken != activeLocalAgentStreamToken) {
            future.complete(LocalAgentApprovalBridge.Decision.deny("The Assistant run has ended."));
            return;
        }
        // SHAFT's own MCP tools are first-party Assistant capabilities and are always allowed.
        // The command line already pre-approves them via --allowedTools, so this is defense in
        // depth for CLI versions or configurations where that flag does not take effect.
        if (isShaftMcpTool(toolName)) {
            appendAgentMilestone("Auto-approved SHAFT tool: " + toolName);
            future.complete(LocalAgentApprovalBridge.Decision.allow());
            return;
        }
        String key = LOCAL_AGENT_APPROVAL_KEY_PREFIX + toolName;
        if (approvedToolsThisRun.contains(key)
                || approvalService().isApproved(key)
                || approvalService().isApproved(LOCAL_AGENT_APPROVE_ALL_KEY)) {
            approvedToolsThisRun.add(key);
            future.complete(LocalAgentApprovalBridge.Decision.allow());
            return;
        }
        Runnable showPrompt = () -> showLocalAgentApprovalPrompt(streamToken, toolName, input, future);
        if (localAgentApprovalPromptShowing) {
            queuedLocalAgentApprovalPrompts.add(showPrompt);
            return;
        }
        showPrompt.run();
    }

    private void showLocalAgentApprovalPrompt(
            int streamToken, String toolName, JsonObject input,
            CompletableFuture<LocalAgentApprovalBridge.Decision> future) {
        localAgentApprovalPromptShowing = true;
        setStatus("Awaiting approval for " + toolName + "...");
        CompletableFuture<ToolApprovalDecision> decisionFuture = new CompletableFuture<>();
        ToolApprovalPromptPanel approvalPanel = new ToolApprovalPromptPanel(
                toolName, input, ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decisionFuture::complete);
        transcript.showWidget("assistant", approvalPanel);
        runOnEdt(approvalPanel::focusFirstDecisionButton);
        decisionFuture.whenComplete((decision, error) -> runOnEdt(
                () -> resolveLocalAgentApproval(streamToken, toolName, decision, future)));
    }

    /**
     * Records the user's decision, folds a short outcome line into the streaming bubble instead of
     * appending a standalone transcript message (a standalone append would break the invariant that
     * the streaming bubble is always the transcript's last message -- see {@link
     * #finishLocalAgentResponse}), completes the bridge-facing future, and shows the next queued
     * prompt if one is waiting.
     */
    private void resolveLocalAgentApproval(
            int streamToken, String toolName, ToolApprovalDecision decision,
            CompletableFuture<LocalAgentApprovalBridge.Decision> future) {
        transcript.clearWidget();
        localAgentApprovalPromptShowing = false;
        String key = LOCAL_AGENT_APPROVAL_KEY_PREFIX + toolName;
        String outcomeLine;
        if (decision == null || decision == ToolApprovalDecision.DENY) {
            outcomeLine = "Denied tool " + toolName + ".";
            future.complete(LocalAgentApprovalBridge.Decision.deny("The user denied this tool call."));
        } else {
            approvedToolsThisRun.add(key);
            if (decision == ToolApprovalDecision.APPROVE_ALL_TOOLS) {
                approvalService().record(ToolApprovalDecision.APPROVE_TOOL_ALWAYS, LOCAL_AGENT_APPROVE_ALL_KEY);
                outcomeLine = "Approved all local-agent tool calls for this project.";
            } else {
                approvalService().record(decision, key);
                outcomeLine = decision == ToolApprovalDecision.APPROVE_TOOL_ALWAYS
                        ? "Approved tool " + toolName + " for this project."
                        : "Approved tool " + toolName + " once.";
            }
            future.complete(LocalAgentApprovalBridge.Decision.allow());
        }
        if (streamToken == activeLocalAgentStreamToken) {
            appendLocalAgentOutput(streamToken, outcomeLine);
            setStatus("Thinking...");
        }
        Runnable next = queuedLocalAgentApprovalPrompts.poll();
        if (next != null) {
            next.run();
        } else {
            prompt.requestFocusInWindow();
        }
    }

    private void clearPendingLocalAgentApprovalPrompt() {
        if (localAgentApprovalPromptShowing) {
            transcript.clearWidget();
            localAgentApprovalPromptShowing = false;
        }
        queuedLocalAgentApprovalPrompts.clear();
    }

    private void showLocalResponse(String response) {
        setStatus(READY_STATUS);
        showResponse("**SHAFT Assistant**\n\n" + AssistantMarkdown.normalizeMarkdown(response), response);
    }

    private void showResponse(String response, String rawResponse) {
        persistAndAppendResponse(withTokenUsage(response, rawResponse), rawResponse);
    }

    private void persistAndAppendResponse(String displayResponse, String rawResponse) {
        ResolvedQuestion resolved = resolveQuestion(displayResponse, rawResponse);
        lastResponse = resolved.toPersist();
        lastRawResponse = rawResponse == null ? "" : rawResponse;
        copyLastResponse.setEnabled(true);
        copyRawResponse.setEnabled(!lastRawResponse.isBlank());
        // append() clears any showing widget first, so a detected question's answer chips are only
        // shown AFTER the persisted append -- otherwise this call would immediately wipe them again.
        append("assistant", resolved.toPersist(), rawResponse);
        if (resolved.question() != null) {
            showAssistantQuestionOptions(resolved.question());
        }
    }

    private String withTokenUsage(String response, String rawResponse) {
        String markdown = response == null ? "" : response.stripTrailing();
        if (markdown.isBlank()
                || LOCAL_AGENT_STREAMING_HEADER.equals(markdown)
                || markdown.toLowerCase(Locale.ROOT).contains("tokens consumed:")) {
            return markdown;
        }
        AssistantLocalAgentRunner.TokenUsage reported = AssistantLocalAgentRunner.parseTokenUsage(rawResponse);
        if (reported == null) {
            // No usage metadata was reported -- an invented estimate is worse than no number at all.
            return markdown;
        }
        return markdown + "\n\n**Tokens consumed:** `" + reported.totalTokens() + "` (input: "
                + reported.inputTokens() + ", output: " + reported.outputTokens() + ")";
    }

    /**
     * Every terminal local-agent run (Completed, Failed, Cancelled, or Killed) must say whether
     * token usage is known, not just show it when it happens to be parseable (issue #3703): a user
     * who never sees a token count has no way to tell "this run reported no usage metadata" apart
     * from "this feature doesn't exist here". {@link #withTokenUsage} already appends the real
     * numbers when {@code parseTokenUsage} finds them; this adds the explicit "not available"
     * fallback line on top for local-agent responses only -- unlike {@link #withTokenUsage} itself,
     * which stays silent by design for every other {@link #showResponse} caller (MCP tool calls,
     * canned local replies, ...) that never carries token metadata in the first place.
     */
    private String withLocalAgentTokenUsage(String response, String rawResponse) {
        String withUsage = withTokenUsage(response, rawResponse);
        if (withUsage.isBlank()
                || LOCAL_AGENT_STREAMING_HEADER.equals(withUsage)
                || withUsage.toLowerCase(Locale.ROOT).contains("tokens consumed:")) {
            return withUsage;
        }
        return withUsage + "\n\n**Token usage:** not available for this run.";
    }

    // Issue #3704 (scoped slice): a scenario-description prompt routes to a multi-step orchestrated
    // local-agent run (record -> act -> save -> codegen -> self-heal, per
    // AssistantCommand.SHAFT_CODEGEN_TOOL_GUIDANCE). The agent decides its own actual steps, so this
    // is phrased as an anticipated preview, not a guarantee -- and it is purely informational: it
    // never blocks or delays the run that follows it.
    private static final String ORCHESTRATION_STAGE_ANNOUNCEMENT =
            """
                    This will likely: 1) record your actions in a browser session, 2) save the recording, \
                    3) generate SHAFT test code from it, and 4) verify and self-heal any broken locators. \
                    The agent decides its own actual steps, so treat this as an anticipated plan, not a guarantee.
                    """.stripIndent().trim();

    /**
     * Posts a plain informational preview of the anticipated stages before a local-agent run that
     * is plausibly a multi-step orchestrated flow starts. Reuses {@link
     * AssistantCommand#isScenarioDescriptionIntent} exactly rather than reinventing detection --
     * that predicate already recognizes the record -&gt; act -&gt; save -&gt; codegen -&gt;
     * self-heal orchestration intent (issue #3692). Not a gate: it never blocks or delays the run.
     */
    private void maybeAnnounceOrchestrationStages(String text) {
        if (AssistantCommand.isScenarioDescriptionIntent(text)) {
            append("assistant", ORCHESTRATION_STAGE_ANNOUNCEMENT, "");
        }
    }

    private void append(String role, String text, String rawResponse) {
        // Any real message (user or assistant) ends the first-run welcome (issue #3540): the
        // welcome is only ever valid on a genuinely empty transcript, and this always follows
        // showFirstRunWelcomeIfNeeded() showing it (or a no-op if never shown/already dismissed),
        // so clearWidget() here is safe even when nothing is currently showing. append() is never
        // called while an approval widget occupies the same slot (see showFirstRunWelcomeIfNeeded's
        // javadoc), so this never fights that widget for the slot.
        transcript.clearWidget();
        transcript.append(role, text, rawResponse);
        chatState.append(role, text, rawResponse);
        updateContextTruncationBoundary();
        refreshChatSelector();
        updateActionChrome();
    }

    void setRunning(boolean running, String message) {
        boolean wasRunning = this.running;
        this.running = running;
        if (running && !wasRunning) {
            cancelRequested = false;
            killRequested = false;
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
        localModel.setEnabled(!running);
        effort.setEnabled(!running);
        customCommand.setEnabled(!running);
        allowSourceMutation.setEnabled(!running);
        verboseAgentOutput.setEnabled(!running);
        autoCompact.setEnabled(!running);
        saveCloudApiKey.setEnabled(!running);
        approveCaptureReview.setEnabled(!running && pendingCaptureReview != null);
        copyCaptureReview.setEnabled(!running && pendingCaptureReview != null);
        dismissCaptureReview.setEnabled(!running && pendingCaptureReview != null);
        createTestClassFromReview.setEnabled(!running && pendingCaptureReview != null);
        insertReviewAtOpenFile.setEnabled(!running && pendingCaptureReview != null);
        openCaptureReview.setEnabled(!running && pendingCaptureReview != null);
        captureEvidencePack.setEnabled(!running && pendingCaptureReview != null);
        compareCaptureBackends.setEnabled(!running && pendingCaptureReview != null);
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
            killRequested = false;
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
        // Trim text if it exceeds available width, but keep full text in tooltip
        int availableWidth = Math.max(220, status.getWidth() > 0 ? status.getWidth() : 260);
        FontMetrics metrics = status.getFontMetrics(status.getFont());
        String displayText = trimChatTitleForWidth(value, metrics, availableWidth - JBUI.scale(8));
        status.setText(displayText);
        status.setToolTipText(value);
        status.getAccessibleContext().setAccessibleDescription(value);
        status.setVisible(!READY_STATUS.equals(value));
    }

    private void onModeOrRouteSelectionChanged() {
        // The checkbox state is only honored in local Agent mode (AssistantCommand forces
        // allowSourceMutation=false everywhere else), so switching modes does not silently clear
        // the checked-by-default source-edit approval.
        updateControlVisibility();
    }

    private void updateControlVisibility() {
        boolean advanced = settings.advancedUiEnabled;
        // A cloud provider chosen during first-run setup stays usable in the basic UI; only
        // ad-hoc cloud switches remain an advanced-mode capability.
        boolean cloudConfigured = "CLOUD".equals(normalize(settings.assistantProviderType, "LOCAL"));
        if (!advanced && usesCloud() && !cloudConfigured) {
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
        String currentAgentConfigurationText = currentAgentConfigurationText();
        currentAgentConfiguration.setText(currentAgentConfigurationText);
        // Accessible description mirrors the live agent configuration text (issue #3603): the name
        // stays a stable category label, but a screen reader also needs to hear which agent/runtime
        // is actually configured, which changes as the user reconfigures the route.
        currentAgentConfiguration.getAccessibleContext().setAccessibleDescription(currentAgentConfigurationText);
        currentAgentConfiguration.setToolTipText(currentAgentConfigurationTooltip());
        currentAgentConfiguration.setVisible(lockedRoute);
        customCommand.setVisible(advanced && !lockedRoute && localCli);
        customCommand.setEnabled(controlsEnabled && advanced && !lockedRoute && localCli);
        cloudProvider.setVisible(advanced && !lockedRoute && cloud);
        cloudProvider.setEnabled(controlsEnabled && advanced && !lockedRoute && cloud);
        // Model and effort selectors stay visible for the active route in every UI mode so the
        // user can always pick the right model after provider setup (issue #3369).
        cloudModel.setVisible(cloud);
        cloudModel.setEnabled(controlsEnabled && cloud);
        cloudKeyPanel.setVisible(cloud);
        cloudApiKey.setEnabled(controlsEnabled && cloud);
        saveCloudApiKey.setEnabled(controlsEnabled && cloud);
        boolean agentMode = "AGENT".equals(mode.getSelectedItem());
        allowSourceMutation.setVisible(agentMode && localAgent);
        allowSourceMutation.setEnabled(controlsEnabled && agentMode && localAgent);
        // Chip has no selection/enable state of its own; it only needs to stay in lockstep with
        // the checkbox's own visibility above so it never renders as an empty colored box (#3601
        // B3.4). Selection/enable/listener logic for the checkbox itself is untouched.
        allowSourceMutationChip.setVisible(agentMode && localAgent);
        // Verbose applies to every run shape: local agent CLI streams AND direct MCP tool runs,
        // so it stays available on every route (issue #3426 B5).
        verboseAgentOutput.setVisible(true);
        verboseAgentOutput.setEnabled(controlsEnabled);
        localModel.setVisible(localCli);
        localModel.setEnabled(controlsEnabled && localCli);
        refreshLocalModels.setVisible(localCli);
        refreshLocalModels.setEnabled(controlsEnabled && localCli);
        effort.setVisible(cloud || localCli);
        effort.setEnabled(controlsEnabled && (cloud || localCli));
        autoCompact.setVisible(localAgent && localCli);
        autoCompact.setEnabled(controlsEnabled && localAgent && localCli);
        configure.setVisible(lockedRoute);
        configure.setEnabled(controlsEnabled && lockedRoute);
        if (localCli) {
            refreshLocalModelsIfNeeded();
        }
        if (cloud) {
            updateCloudKeyStatus();
        }
        updateActionChrome();
    }

    private void refreshLocalModelsIfNeeded() {
        if (ApplicationManager.getApplication() == null) {
            // Headless Gradle unit-test JVM: no real CLI is ever reachable here (isCommandAvailable
            // always fails fast), so this async probe -- dispatched off the calling thread and landing
            // back via SwingUtilities.invokeLater on the real, JVM-wide AWT EDT -- adds nothing but a
            // race against tests that drive applyLocalModels/modelListFamily directly and
            // synchronously (issue #3649). Real IDE runs always have a non-null Application.
            return;
        }
        String family = String.valueOf(assistantFamily.getSelectedItem());
        if (modelListRefreshing || family.equals(modelListFamily)) {
            return;
        }
        modelListRefreshing = true;
        JsonObject arguments = new JsonObject();
        arguments.addProperty("client", AssistantCommand.Selection.local(family, "CLI").client());
        CompletableFuture.supplyAsync(() -> AssistantLocalAgentRunner.listModels(arguments),
                        ShaftPluginExecutor.getInstance().executor())
                .whenComplete((models, error) -> runOnEdt(
                        () -> applyLocalModels(family, error == null ? models : List.of())));
    }

    private void applyLocalModels(String family, List<String> models) {
        modelListRefreshing = false;
        modelListFamily = family;
        String previousSelection = localModel.getEditor() == null
                ? null
                : String.valueOf(localModel.getEditor().getItem());
        // The CLI-reported list wins; the curated catalog keeps the selector useful when the CLI
        // cannot list its models (issue #3369).
        localModelListIsFallback = models.isEmpty();
        List<String> effectiveModels = localModelListIsFallback ? AssistantModelCatalog.localModels(family) : models;
        DefaultComboBoxModel<String> model = new DefaultComboBoxModel<>(effectiveModels.toArray(new String[0]));
        localModel.setModel(model);
        String tooltip = localModelListIsFallback ? LOCAL_MODEL_TOOLTIP_FALLBACK : LOCAL_MODEL_TOOLTIP_LIVE;
        localModel.setToolTipText(tooltip);
        if (localModel.getEditor().getEditorComponent() instanceof JTextComponent localModelEditor) {
            localModelEditor.setToolTipText(tooltip);
        }
        if (previousSelection != null && !previousSelection.isBlank()) {
            localModel.setSelectedItem(previousSelection);
        } else if (settings.localModel != null && !settings.localModel.isBlank()) {
            localModel.setSelectedItem(settings.localModel.trim());
        } else if (!effectiveModels.isEmpty()) {
            localModel.setSelectedItem(effectiveModels.get(0));
        }
    }

    private void applyCloudModelChoices(String preferredModel) {
        String provider = normalizeLower(String.valueOf(cloudProvider.getSelectedItem()), "gemini");
        List<String> models = AssistantModelCatalog.cloudModels(provider);
        cloudModel.setModel(new DefaultComboBoxModel<>(models.toArray(new String[0])));
        String preferred = preferredModel == null ? "" : preferredModel.trim();
        cloudModel.setSelectedItem(preferred.isBlank() ? models.get(0) : preferred);
    }

    private static String editableComboText(JComboBox<String> combo) {
        Object item = combo.isEditable() && combo.getEditor() != null
                ? combo.getEditor().getItem()
                : combo.getSelectedItem();
        return item == null ? "" : item.toString().trim();
    }

    private static void runOnEdt(Runnable action) {
        if (ApplicationManager.getApplication() != null) {
            ApplicationManager.getApplication().invokeLater(action);
        } else if (SwingUtilities.isEventDispatchThread()) {
            action.run();
        } else {
            SwingUtilities.invokeLater(action);
        }
    }

    /**
     * Shows the first-run welcome as the Assistant's own first message in the transcript (issue
     * #3500 O1, follow-up #3540), via the same ephemeral {@link AssistantTranscriptView#showWidget}
     * slot used for {@link ToolApprovalPromptPanel} -- never persisted into
     * chatState/markdown/Copy-transcript. A no-op unless the transcript is genuinely empty and the
     * flag isn't set, so it's safe to call after every transition to an empty transcript
     * (construction, New chat, chat switch, Clear); {@link #transcript}'s single widget slot is
     * never contested because those same call sites always {@code transcript.clear()} first, and an
     * approval widget only ever appears after some message has already hidden this welcome (every
     * {@link #append(String, String, String)} call clears it unconditionally).
     */
    private void showFirstRunWelcomeIfNeeded() {
        if (settings.firstRunCoachDismissed || !transcript.markdown().isBlank()) {
            return;
        }
        javax.swing.JButton gotIt = new javax.swing.JButton("Got it");
        gotIt.getAccessibleContext().setAccessibleName("Dismiss first run coach");
        gotIt.setToolTipText("Hide this first-run guide permanently.");
        gotIt.setMargin(JBUI.insets(1, 8));
        gotIt.addActionListener(event -> {
            settings.firstRunCoachDismissed = true;
            transcript.clearWidget();
        });
        transcript.showWidget("assistant", transcript.assistantBubbleWithActions(
                FIRST_RUN_WELCOME_MARKDOWN, gotIt, "Assistant welcome message bubble"));
    }

    /**
     * First-run empty-state chips (issue #3500 A6): three concrete next actions that pre-fill the
     * composer instead of executing anything, so the user stays in control of the first send.
     * {@code WrapLayout} (not plain {@code FlowLayout}) reports correct wrapped preferred height in
     * a narrow tool window; a plain {@code FlowLayout} row under-reports it, letting whatever
     * follows collide with a wrapped second line of chips (root cause of the first-run coach clip,
     * issue #3540 -- the coach strip that used to sit above this row is now the transcript welcome
     * bubble; see {@link #showFirstRunWelcomeIfNeeded()}).
     */
    private javax.swing.JPanel buildEmptyStateChips() {
        javax.swing.JPanel chipRow = new javax.swing.JPanel(new WrapLayout(java.awt.FlowLayout.LEFT, 6, 0));
        chipRow.setOpaque(false);
        chipRow.add(emptyStateChip("Record a sample flow",
                "Record a sample web flow on a practice page, add one assertion, and generate a reviewed test."));
        chipRow.add(emptyStateChip("Ask how to assert",
                "How do I add assertions while recording a web flow?"));
        chipRow.add(emptyStateChip("Diagnose my last failure",
                "Diagnose my most recent failed test run and propose a fix."));
        emptyStateChips = chipRow;
        return emptyStateChips;
    }

    private javax.swing.JButton emptyStateChip(String label, String cannedPrompt) {
        javax.swing.JButton chip = new javax.swing.JButton(label);
        chip.getAccessibleContext().setAccessibleName(label);
        chip.setToolTipText("Fills the message box with this request; review and send it yourself.");
        chip.setMargin(JBUI.insets(1, 8));
        chip.addActionListener(event -> {
            prompt.setText(cannedPrompt);
            prompt.setCaretPosition(cannedPrompt.length());
            prompt.requestFocusInWindow();
        });
        return chip;
    }

    /**
     * Removable-chip row for prompt attachments (issue #3727): current file / all open files / a
     * picked file / a picked image, added via {@link #showAttachMenu()}. Hidden (zero height, via
     * the {@code composerTop} BoxLayout) whenever {@link #attachments} is empty.
     */
    private javax.swing.JPanel buildAttachmentsChipRow() {
        attachmentsChipRow = new javax.swing.JPanel(new WrapLayout(java.awt.FlowLayout.LEFT, 6, 0));
        attachmentsChipRow.getAccessibleContext().setAccessibleName("Prompt attachments");
        attachmentsChipRow.setOpaque(false);
        attachmentsChipRow.setVisible(false);
        return attachmentsChipRow;
    }

    private void refreshAttachmentsChipRow() {
        attachmentsChipRow.removeAll();
        for (AssistantAttachment attachment : attachments) {
            attachmentsChipRow.add(attachmentChip(attachment));
        }
        attachmentsChipRow.setVisible(!attachments.isEmpty());
        attachmentsChipRow.revalidate();
        attachmentsChipRow.repaint();
        Container parent = attachmentsChipRow.getParent();
        if (parent != null) {
            parent.revalidate();
            parent.repaint();
        }
    }

    /** One removable attachment chip: name plus a trailing "×", the whole chip is the remove control. */
    private JButton attachmentChip(AssistantAttachment attachment) {
        JButton chip = new JButton(attachment.displayName() + "  ×");
        chip.getAccessibleContext().setAccessibleName("Remove attachment " + attachment.displayName());
        String note = attachment.truncated()
                ? " (truncated to " + AssistantAttachment.MAX_CONTENT_CHARACTERS + " characters)"
                : "";
        chip.setToolTipText((attachment.image() ? "Attached image: " : "Attached file: ")
                + attachment.path() + note + " — click to remove");
        chip.setMargin(JBUI.insets(1, 8));
        chip.addActionListener(event -> removeAttachment(attachment.path()));
        return chip;
    }

    private void addAttachment(AssistantAttachment attachment) {
        attachments = AssistantAttachments.withAttachment(attachments, attachment);
        refreshAttachmentsChipRow();
    }

    private void removeAttachment(String path) {
        attachments = AssistantAttachments.withoutAttachment(attachments, path);
        refreshAttachmentsChipRow();
    }

    /** Popup menu behind the {@link #attach} toolbar button: the four attach affordances from
     * issue #3727 -- current file, all open files, a picked file, a picked image. */
    private void showAttachMenu() {
        JPopupMenu menu = new JPopupMenu("Attach to prompt");
        JMenuItem currentFile = new JMenuItem("Add current file");
        currentFile.addActionListener(event -> addCurrentFileAttachment());
        JMenuItem allOpenFiles = new JMenuItem("Add all open files");
        allOpenFiles.addActionListener(event -> addAllOpenFilesAttachments());
        JMenuItem pickFile = new JMenuItem("Attach file from disk…");
        pickFile.addActionListener(event -> attachFileFromDisk());
        JMenuItem pickImage = new JMenuItem("Attach screenshot or image…");
        pickImage.addActionListener(event -> attachImageFromDisk());
        menu.add(currentFile);
        menu.add(allOpenFiles);
        menu.addSeparator();
        menu.add(pickFile);
        menu.add(pickImage);
        menu.show(attach, 0, attach.getHeight());
    }

    private void addCurrentFileAttachment() {
        AssistantCommand.OpenFileContext current = currentEditorFileProvider.apply(project);
        if (current == null || !current.present()) {
            setStatus("No file is open to attach");
            return;
        }
        addAttachment(AssistantAttachment.forText(current.path(), current.text()));
        setStatus("Attached " + fileName(current.path()));
    }

    private void addAllOpenFilesAttachments() {
        List<AssistantCommand.OpenFileContext> openFiles = openEditorFilesProvider.apply(project);
        List<AssistantCommand.OpenFileContext> present = openFiles == null ? List.of() : openFiles.stream()
                .filter(AssistantCommand.OpenFileContext::present)
                .toList();
        if (present.isEmpty()) {
            setStatus("No open editor files to attach");
            return;
        }
        for (AssistantCommand.OpenFileContext file : present) {
            addAttachment(AssistantAttachment.forText(file.path(), file.text()));
        }
        setStatus("Attached " + present.size() + " open file" + (present.size() == 1 ? "" : "s"));
    }

    private void attachFileFromDisk() {
        File selected = chooseFile("Attach File", null);
        if (selected != null) {
            attachFileAtPath(selected.toPath());
        }
    }

    private void attachImageFromDisk() {
        File selected = chooseFile("Attach Screenshot or Image",
                new FileNameExtensionFilter("Images", "png", "jpg", "jpeg", "gif", "bmp"));
        if (selected != null) {
            attachImageAtPath(selected.toPath());
        }
    }

    /**
     * Reads {@code path} and adds it as a text attachment. Package-private (not private): factored
     * out of {@link #attachFileFromDisk()} so headless tests can exercise the real read/attach logic
     * without driving a {@link JFileChooser} dialog.
     */
    void attachFileAtPath(Path path) {
        try {
            String content = Files.readString(path);
            addAttachment(AssistantAttachment.forText(path.toAbsolutePath().toString(), content));
            setStatus("Attached " + path.getFileName());
        } catch (IOException | RuntimeException unreadable) {
            setStatus("Could not read " + path.getFileName());
        }
    }

    /** Same test-seam rationale as {@link #attachFileAtPath}, for the image-attach affordance. */
    void attachImageAtPath(Path path) {
        addAttachment(AssistantAttachment.forImage(path.toAbsolutePath().toString()));
        setStatus("Attached image " + path.getFileName());
    }

    private File chooseFile(String title, FileNameExtensionFilter filter) {
        JFileChooser chooser = new JFileChooser(lastSaveDirectory().toFile());
        chooser.setDialogTitle(title);
        if (filter != null) {
            chooser.setFileFilter(filter);
        }
        if (chooser.showOpenDialog(this) != JFileChooser.APPROVE_OPTION) {
            return null;
        }
        return chooser.getSelectedFile();
    }

    /** Default {@link #openEditorFilesProvider}: every open editor's path and text, via the real
     * {@link FileEditorManager} (production path; headless tests inject a fake provider instead). */
    private static List<AssistantCommand.OpenFileContext> realOpenEditorFiles(Project project) {
        if (project == null) {
            return List.of();
        }
        try {
            FileEditorManager manager = FileEditorManager.getInstance(project);
            List<AssistantCommand.OpenFileContext> files = new ArrayList<>();
            for (VirtualFile virtualFile : manager.getOpenFiles()) {
                String text = readVirtualFileText(virtualFile);
                if (!text.isEmpty()) {
                    files.add(new AssistantCommand.OpenFileContext(virtualFile.getPath(), text));
                }
            }
            return files;
        } catch (RuntimeException | Error headlessTestEnvironment) {
            return List.of();
        }
    }

    private static String readVirtualFileText(VirtualFile virtualFile) {
        try {
            return com.intellij.openapi.vfs.VfsUtilCore.loadText(virtualFile);
        } catch (IOException | RuntimeException unreadable) {
            return "";
        }
    }

    private void updateActionChrome() {
        if (copyLastResponse == null || copyRawResponse == null || copyTranscript == null
                || clearTranscript == null || rerunLastPrompt == null || cancel == null
                || saveTranscript == null) {
            return;
        }
        boolean hasResponse = !lastResponse.isBlank();
        boolean hasRawResponse = !lastRawResponse.isBlank();
        boolean hasTranscript = !transcript.markdown().isBlank() || !toolEvidence.isEmpty();
        if (emptyStateChips != null) {
            emptyStateChips.setVisible(!hasTranscript);
        }
        boolean canRerun = !lastPrompt.isBlank();
        copyLastResponse.setVisible(hasResponse);
        copyLastResponse.setEnabled(hasResponse);
        copyRawResponse.setVisible(hasRawResponse);
        copyRawResponse.setEnabled(hasRawResponse);
        copyTranscript.setVisible(hasTranscript);
        copyTranscript.setEnabled(hasTranscript);
        clearTranscript.setVisible(hasTranscript);
        clearTranscript.setEnabled(hasTranscript && !running);
        saveTranscript.setVisible(hasTranscript);
        saveTranscript.setEnabled(hasTranscript);
        rerunLastPrompt.setVisible(canRerun);
        rerunLastPrompt.setEnabled(canRerun && !running);
        cancel.setVisible(running);
        cancel.setEnabled(running);
        refreshActionRowLayout();
    }

    private void refreshActionRowLayout() {
        if (actionRow == null) {
            return;
        }
        Container container = actionRow.getParent();
        if (container == null) {
            actionRow.revalidate();
            actionRow.repaint();
            return;
        }
        container.revalidate();
        container.repaint();
    }

    private void updateCloudKeyStatus() {
        String provider = String.valueOf(cloudProvider.getSelectedItem());
        String keyName = providerKeyName(provider);
        boolean stored = !keyName.isBlank() && storedCloudKey(keyName);
        String providerLabel = ShaftUiLabels.friendly(provider);
        cloudKeyStatus.setText(stored ? providerLabel + " key stored" : "Enter " + providerLabel + " key");
        cloudKeyStatus.setVisible(false);
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
        return !keyName.isBlank() && storedCloudKey(keyName);
    }

    private static boolean storedCloudKey(String keyName) {
        // Password Safe needs a running IntelliJ application; headless panel tests have none.
        return ApplicationManager.getApplication() != null
                && ShaftCredentialService.getInstance().hasApiKey(keyName);
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

    private void copyFullTranscript() {
        copy(exportTranscriptWithEvidence(), "Copied transcript");
    }

    private String exportTranscriptWithEvidence() {
        String transcriptText = transcript.markdown();
        String evidenceText = exportToolEvidence();
        if (evidenceText == null || evidenceText.isBlank()) {
            return transcriptText;
        }
        return transcriptText + "\n\n" + evidenceText;
    }

    private static final String LAST_SAVE_DIRECTORY_KEY = "shaft.intellij.assistant.transcript.lastSaveDirectory";

    private void saveTranscript() {
        Path defaultDirectory = lastSaveDirectory();
        JFileChooser chooser = new JFileChooser(defaultDirectory.toFile());
        chooser.setDialogTitle("Save Transcript");
        chooser.setFileFilter(new FileNameExtensionFilter("Markdown", "md"));
        chooser.setSelectedFile(new File(defaultDirectory.toFile(), defaultTranscriptFileName()));
        if (chooser.showSaveDialog(this) != JFileChooser.APPROVE_OPTION) {
            return;
        }
        Path target = withMarkdownExtension(chooser.getSelectedFile()).toPath();
        Path parent = target.getParent();
        if (parent != null && project != null) {
            PropertiesComponent.getInstance(project).setValue(LAST_SAVE_DIRECTORY_KEY, parent.toString());
        }
        writeTranscriptToFile(target);
    }

    private static File withMarkdownExtension(File selected) {
        if (selected.getName().toLowerCase(Locale.ROOT).endsWith(".md")) {
            return selected;
        }
        return new File(selected.getParentFile(), selected.getName() + ".md");
    }

    private Path lastSaveDirectory() {
        if (project != null) {
            String saved = PropertiesComponent.getInstance(project).getValue(LAST_SAVE_DIRECTORY_KEY);
            if (saved != null && !saved.isBlank()) {
                return Path.of(saved);
            }
            String basePath = project.getBasePath();
            if (basePath != null && !basePath.isBlank()) {
                return Path.of(basePath);
            }
        }
        return Path.of(System.getProperty("user.home", "."));
    }

    private static String defaultTranscriptFileName() {
        String timestamp = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss").format(LocalDateTime.now());
        return "shaft-assistant-" + timestamp + ".md";
    }

    private void writeTranscriptToFile(Path target) {
        String content = exportTranscriptWithEvidence();
        ApplicationManager.getApplication().executeOnPooledThread(() -> {
            try {
                writeTranscriptSync(target, content);
                ApplicationManager.getApplication().invokeLater(() -> notifyTranscriptSaved(target));
            } catch (IOException e) {
                ApplicationManager.getApplication().invokeLater(() -> notifyTranscriptSaveFailed(e));
            }
        });
    }

    /**
     * Synchronous write body run off the EDT by {@link #writeTranscriptToFile(Path)}. Kept as a
     * separate seam (no threading, no notifications) so tests can exercise the actual write logic
     * headlessly and deterministically, without polling a pooled-thread background task.
     */
    private void writeTranscriptSync(Path target, String content) throws IOException {
        Files.writeString(target, content);
    }

    private void notifyTranscriptSaved(Path target) {
        ShaftNotifier.infoWithAction(project, "Transcript saved", target.toString(),
                "Open", () -> openFileInEditor(target));
    }

    private void notifyTranscriptSaveFailed(IOException error) {
        ShaftNotifier.warn(project, "Failed to save transcript",
                error.getMessage() == null ? error.toString() : error.getMessage());
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
        contextTruncationBoundaryIndex = -1;
        showFirstRunWelcomeIfNeeded();
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
        showFirstRunWelcomeIfNeeded();
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
        if (!"capture_stop".equals(invocation.toolName()) || !AssistantCommand.isStopRecording(promptText)) {
            return invocation;
        }
        if (activeRecordingBackend == RecordingBackend.PLAYWRIGHT) {
            return AssistantCommand.stopPlaywrightRecording();
        }
        if (activeRecordingBackend == RecordingBackend.API) {
            return AssistantCommand.stopApiRecording();
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
        if ("capture_api_start".equals(invocation.toolName())) {
            // Issue #3739: capture_api_start has no start-time output path (it arrives only in the
            // capture_api_stop result), and unlike capture_start, it never schedules
            // capture_start's WebDriver-specific startup diagnostic.
            activeRecordingBackend = RecordingBackend.API;
            activeApiRecordingPath = "";
            clearPendingCaptureReview();
            generateCaptureReviewAfterStop = false;
            captureReviewGenerationRunning = false;
            return;
        }
        if (("capture_stop".equals(invocation.toolName())
                || "capture_api_stop".equals(invocation.toolName()))
                && AssistantCommand.isStopRecording(promptText)) {
            if ("capture_api_stop".equals(invocation.toolName())) {
                activeRecordingBackend = RecordingBackend.API;
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
        if (reviewBackend == RecordingBackend.PLAYWRIGHT) {
            lastReviewSessionPath = activePlaywrightRecordingPath;
        } else if (reviewBackend == RecordingBackend.API) {
            lastReviewSessionPath = activeApiRecordingPath;
        } else {
            lastReviewSessionPath = activeCaptureRecordingPath;
        }
        AssistantCommand.Invocation invocation = recordingCodeReviewInvocation(reviewBackend);
        activeRecordingBackend = RecordingBackend.WEBDRIVER;
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startTool(invocation.toolName(), invocation.arguments());
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showResult(invocation.toolName(), result, error)));
    }

    private AssistantCommand.Invocation recordingCodeReviewInvocation(RecordingBackend backend) {
        if (backend == RecordingBackend.API) {
            return AssistantCommand.Invocation.tool(
                    "capture_api_generate", AssistantCommand.apiCaptureGenerate(activeApiRecordingPath));
        }
        boolean playwright = backend == RecordingBackend.PLAYWRIGHT;
        return AssistantCommand.Invocation.tool(
                "capture_code_blocks",
                playwright
                        ? AssistantCommand.playwrightCodeReview(activePlaywrightRecordingPath)
                        : AssistantCommand.captureCodeReview(activeCaptureRecordingPath));
    }

    private static boolean isRecordingCodeReviewTool(String toolName) {
        return "capture_code_blocks".equals(toolName)
                || "capture_generate_replay".equals(toolName)
                || "capture_api_generate".equals(toolName);
    }

    private void showPendingCaptureReview() {
        if (pendingCaptureReview == null) {
            captureReviewPanel.setVisible(false);
            return;
        }
        String captureReviewStatusText = captureReviewSummary(pendingCaptureReview.markdown());
        captureReviewStatus.setText(captureReviewStatusText);
        captureReviewStatus.getAccessibleContext().setAccessibleDescription(captureReviewStatusText);
        approveCaptureReview.setEnabled(!running);
        copyCaptureReview.setEnabled(!running);
        dismissCaptureReview.setEnabled(!running);
        createTestClassFromReview.setEnabled(!running);
        insertReviewAtOpenFile.setEnabled(!running);
        openCaptureReview.setEnabled(!running);
        captureEvidencePack.setEnabled(!running);
        compareCaptureBackends.setEnabled(!running);
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
            createTestClassFromReview.setEnabled(false);
            insertReviewAtOpenFile.setEnabled(false);
            openCaptureReview.setEnabled(false);
            captureEvidencePack.setEnabled(false);
            compareCaptureBackends.setEnabled(false);
            captureReviewPanel.setVisible(false);
            revalidate();
            repaint();
        }
    }

    private JsonObject pendingReviewJson() {
        return pendingCaptureReview == null
                ? null
                : AssistantMarkdown.jsonObjectFromMcpOutput(pendingCaptureReview.rawResult());
    }

    private static String reviewString(JsonObject raw, String key) {
        return raw != null && raw.has(key) && raw.get(key).isJsonPrimitive()
                ? raw.get(key).getAsString()
                : "";
    }

    private static String firstJavaClassBlock(JsonObject raw) {
        if (raw == null || !raw.has("codeBlocks") || !raw.get("codeBlocks").isJsonArray()) {
            return "";
        }
        String fallback = "";
        for (var element : raw.getAsJsonArray("codeBlocks")) {
            if (!element.isJsonObject()) {
                continue;
            }
            JsonObject block = element.getAsJsonObject();
            String code = string(block, "code", string(block, "content", ""));
            if (code.isBlank() || !code.contains("class ")) {
                continue;
            }
            if ("capture-full-class".equals(string(block, "id", ""))) {
                return code;
            }
            if (fallback.isBlank()) {
                fallback = code;
            }
        }
        return fallback;
    }

    /**
     * "Create test class" (issue #3425 B1): writes the reviewed full-class block into the
     * project's {@code src/test/java} tree (never overwriting an existing file) and opens it in
     * the editor, so the Record -> Review -> Insert loop ends inside the IDE, not on the clipboard.
     */
    private void createTestClassFromReview() {
        if (running || pendingCaptureReview == null || project == null || project.getBasePath() == null) {
            setStatus("No reviewed code available");
            return;
        }
        String code = firstJavaClassBlock(pendingReviewJson());
        if (code.isBlank()) {
            setStatus("The review has no full-class code block");
            return;
        }
        java.util.regex.Matcher className = java.util.regex.Pattern
                .compile("class\\s+(\\w+)").matcher(code);
        java.util.regex.Matcher packageName = java.util.regex.Pattern
                .compile("package\\s+([\\w.]+)\\s*;").matcher(code);
        if (!className.find()) {
            setStatus("Could not determine the generated class name");
            return;
        }
        String body = packageName.find() ? code : "package tests.generated;\n\n" + code;
        String packagePath = (packageName.reset().find() ? packageName.group(1) : "tests.generated")
                .replace('.', '/');
        Path target = Path.of(project.getBasePath(), "src", "test", "java")
                .resolve(packagePath).resolve(className.group(1) + ".java");
        try {
            if (Files.exists(target)) {
                setStatus("Already exists — opened " + target.getFileName() + " (not overwritten)");
            } else {
                Files.createDirectories(target.getParent());
                Files.writeString(target, body);
                setStatus("Created " + target.getFileName() + " in src/test/java");
                showResponse("**Test class created.** Wrote the reviewed recording to `"
                        + Path.of(project.getBasePath()).relativize(target) + "` and opened it in the editor. "
                        + "Run it with `/verify mvn -q test-compile` or your normal test run.", "");
            }
            openFileInEditor(target);
        } catch (Exception writeFailure) {
            setStatus("Could not write test class: " + writeFailure.getMessage());
        }
    }

    /**
     * "Insert into open class" (issue #3425 B1): asks SHAFT MCP to regenerate the reviewed steps
     * as insertion-ready blocks anchored to the caret when it resolves to a Java method, otherwise
     * to the file currently open in the editor ({@code capture_record_at_target_code_blocks}). Uses
     * the same {@link InsertionAnchor} the "Record here" action and "Insert at caret" now share
     * (issue #3662) so this button no longer ignores the caret the way it used to.
     */
    private void insertReviewIntoOpenFile() {
        if (running || pendingCaptureReview == null) {
            return;
        }
        InsertionAnchor anchor = InsertionAnchorResolver.resolve(project, selectedEditor());
        if (anchor == null) {
            setStatus("Open the target Java class in the editor first");
            return;
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("sessionPath", lastReviewSessionPath);
        arguments.addProperty("outputDirectory", AssistantCommand.DEFAULT_CAPTURE_REVIEW_DIRECTORY);
        arguments.addProperty("packageName", "tests.generated");
        arguments.addProperty("className", "");
        arguments.addProperty("overwrite", true);
        arguments.addProperty("targetSourcePath", anchor.targetSourcePath());
        arguments.addProperty("insertAfter", anchor.insertAfter());
        arguments.addProperty("driverVariableName", "driver");
        startMcpInvocation(AssistantCommand.Invocation.tool("capture_record_at_target_code_blocks", arguments));
    }

    /** "Open review file" (issue #3425 B1): jumps to the generated review artifact in the editor. */
    private void openCaptureReviewFile() {
        JsonObject raw = pendingReviewJson();
        String reviewPath = reviewString(raw, "reviewPath");
        if (reviewPath.isBlank()) {
            reviewPath = reviewString(raw, "reportPath");
        }
        if (reviewPath.isBlank()) {
            setStatus("The review result did not include a review file path");
            return;
        }
        openFileInEditor(Path.of(reviewPath));
    }

    /** "Evidence pack" (issue #3425 B6): one click bundles source/report/review into a manifest. */
    private void collectCaptureEvidencePack() {
        if (running || pendingCaptureReview == null) {
            return;
        }
        JsonObject raw = pendingReviewJson();
        JsonObject arguments = new JsonObject();
        arguments.addProperty("sourcePath", reviewString(raw, "sourcePath"));
        arguments.addProperty("reportPath", reviewString(raw, "reportPath"));
        arguments.addProperty("reviewPath", reviewString(raw, "reviewPath"));
        arguments.add("screenshotPaths", new JsonArray());
        startMcpInvocation(AssistantCommand.Invocation.tool("capture_evidence_pack", arguments));
    }

    /**
     * "Compare backends" (issue #3425 C2): generates the same recording as both WebDriver and
     * Playwright SHAFT code so the user can judge the differentiation with their own flow.
     */
    private void compareCaptureBackends() {
        if (running || pendingCaptureReview == null) {
            return;
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("sessionPath", lastReviewSessionPath);
        arguments.addProperty("outputDirectory", "target/shaft-capture-comparison");
        arguments.addProperty("packageName", "tests.generated");
        arguments.addProperty("className", "ComparedCaptureTest");
        arguments.addProperty("overwrite", true);
        arguments.addProperty("driverVariableName", "driver");
        startMcpInvocation(AssistantCommand.Invocation.tool("capture_backend_comparison", arguments));
    }

    /** Editor holding the caret, for {@link InsertionAnchorResolver}; null in headless test runs. */
    private Editor selectedEditor() {
        try {
            return project == null ? null : FileEditorManager.getInstance(project).getSelectedTextEditor();
        } catch (RuntimeException | Error headlessTestEnvironment) {
            return null;
        }
    }

    private void openFileInEditor(Path path) {
        try {
            var virtualFile = com.intellij.openapi.vfs.LocalFileSystem.getInstance()
                    .refreshAndFindFileByNioFile(path);
            if (virtualFile != null && project != null) {
                FileEditorManager.getInstance(project).openFile(virtualFile, true);
            }
        } catch (RuntimeException | Error headlessTestEnvironment) {
            // Best effort: the path was already reported in the status/transcript.
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
                killRequested = true;
                stopLocalAgentStreaming();
                currentInvocation.kill();
                setStatus("Killing...");
                // The terminal "Killed" entry is recorded by the completion callback once the kill
                // actually lands (see showCancelledToolResult / showAgentResult / showTerminalSequenceResult);
                // this request-time entry is only an in-flight status.
                appendAgentMilestone("Killing...");
            } else {
                currentInvocation.cancel();
                cancelRequested = true;
                setStatus("Cancelling...");
                // Same as above: the terminal "Cancelled" entry comes from the completion callback.
                appendAgentMilestone("Cancelling...");
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
            updateContextTruncationBoundary();
        } else {
            transcript.clear();
            contextTruncationBoundaryIndex = -1;
            showFirstRunWelcomeIfNeeded();
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
            contextTruncationBoundaryIndex = -1;
            return "";
        }
        List<String> entries = new ArrayList<>();
        int total = 0;
        int oldestIncludedIndex = -1;
        boolean loopCompletedWithoutBreak = true;
        for (int index = messages.size() - 1; index >= 0; index--) {
            ShaftAssistantChatState.Message message = messages.get(index);
            if (message == null || message.markdown == null || message.markdown.isBlank()) {
                continue;
            }
            String entry = contextRole(message.role) + ": " + message.markdown.trim();
            int nextTotal = total + entry.length() + 2;
            if (nextTotal > MAX_AGENT_CONTEXT_CHARACTERS && !entries.isEmpty()) {
                contextTruncationBoundaryIndex = index + 1;
                loopCompletedWithoutBreak = false;
                break;
            }
            oldestIncludedIndex = index;
            entries.add(0, clipContextEntry(entry, MAX_AGENT_CONTEXT_CHARACTERS));
            total = nextTotal;
        }
        if (loopCompletedWithoutBreak && oldestIncludedIndex == 0) {
            contextTruncationBoundaryIndex = -1;
        }
        return String.join("\n\n", entries);
    }

    private void updateContextTruncationBoundary() {
        computeTruncationBoundaryIndex();
        transcript.setTruncationBoundaryIndex(contextTruncationBoundaryIndex);
    }

    /**
     * Computes {@link #contextTruncationBoundaryIndex} only -- issue #3751 part 2, MEDIUM finding 5:
     * {@code append()} calls {@link #updateContextTruncationBoundary()} after every single message
     * (including every non-verbose streamed milestone line), but {@link #conversationContextForPrompt}
     * was doing the FULL context-string build (entries list, per-entry role-prefixed string
     * concatenation, clipping, {@code String.join}) purely to derive this one index as a side effect,
     * then discarding the string. This mirrors that method's exact same truncation-boundary math
     * without allocating any of that -- {@link #conversationContextForPrompt} (called once per actual
     * send, not once per line) is still the only place that needs the real joined text and keeps
     * computing this same index itself when it runs.
     */
    private void computeTruncationBoundaryIndex() {
        List<ShaftAssistantChatState.Message> messages = chatState.activeMessages();
        if (messages.isEmpty()) {
            contextTruncationBoundaryIndex = -1;
            return;
        }
        int total = 0;
        int oldestIncludedIndex = -1;
        boolean loopCompletedWithoutBreak = true;
        for (int index = messages.size() - 1; index >= 0; index--) {
            ShaftAssistantChatState.Message message = messages.get(index);
            if (message == null || message.markdown == null || message.markdown.isBlank()) {
                continue;
            }
            int entryLength = contextRole(message.role).length() + 2 + message.markdown.trim().length();
            int nextTotal = total + entryLength + 2;
            if (nextTotal > MAX_AGENT_CONTEXT_CHARACTERS && oldestIncludedIndex != -1) {
                contextTruncationBoundaryIndex = index + 1;
                loopCompletedWithoutBreak = false;
                break;
            }
            oldestIncludedIndex = index;
            total = nextTotal;
        }
        if (loopCompletedWithoutBreak && oldestIncludedIndex == 0) {
            contextTruncationBoundaryIndex = -1;
        }
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

    /**
     * Replaces the local-agent run's streaming placeholder bubble (recorded by {@link
     * #appendStreamingLocalAgentBubble} in {@link #localAgentStreamPlaceholderMessageIndex}) with
     * {@code message} -- deliberately NOT "replace the last message": agent-milestone bubbles (issue
     * #3695, e.g. "Cancelling...") can now be appended after the placeholder and before this call,
     * so a plain last-message replace would silently clobber the wrong (most recent milestone)
     * bubble instead and leave the true placeholder stuck on screen forever.
     *
     * @param forceRender whether to bypass {@link AssistantTranscriptView}'s ~100ms streamed-render
     *     throttle (issue #3751 part 2, HIGH finding 1) and force this update onto the bubble
     *     immediately -- {@code false} for intermediate per-line streaming updates (the throttle is
     *     the point), {@code true} for the run's terminal call (a completed/killed/cancelled run must
     *     never leave the bubble showing stale throttled-away content)
     */
    private void replaceLocalAgentStreamPlaceholder(String role, String message, boolean forceRender) {
        if (message == null || message.isBlank()) {
            return;
        }
        ShaftAssistantChatState.Session active = chatState.activeSession();
        if (active == null || active.messages == null
                || localAgentStreamPlaceholderMessageIndex < 0
                || localAgentStreamPlaceholderMessageIndex >= active.messages.size()) {
            append(role, message, "");
            return;
        }
        String normalizedRole = role == null || role.isBlank() ? "assistant" : role.trim().toLowerCase(Locale.ROOT);
        ShaftAssistantChatState.Message target = active.messages.get(localAgentStreamPlaceholderMessageIndex);
        target.role = normalizedRole;
        target.markdown = message;
        if (localAgentStreamPlaceholderMessageIndex == active.messages.size() - 1) {
            // Common case: no milestone bubble was appended after the placeholder yet -- the fast,
            // incremental single-bubble update still applies.
            transcript.replaceLast(normalizedRole, message);
            if (forceRender) {
                transcript.flushStreamedRender();
            }
        } else {
            // A milestone bubble now sits after the placeholder -- resync the whole transcript from
            // the chat-state source of truth, so the placeholder's own bubble (not the trailing
            // milestone bubble) is the one that visibly changes. Always a full, immediate render
            // already, so forceRender needs no special handling here.
            transcript.setMessages(active.messages);
        }
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
        boolean warnings = markdown != null && markdown.contains("Warnings**");
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
            markdown.append("\n\n**").append(ShaftStatusPresentation.WARNING_ICON).append(" Warnings**");
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

    /**
     * Fresh/consumer-project hint (#3601 O3): {@link ShaftProjectVersionCheck} already tells the
     * setup wizard's "Upgrade project" step whether this project has any SHAFT
     * ({@code io.github.shafthq}) dependency at all; surfacing that same NOT_A_SHAFT_PROJECT signal
     * here points a user starting from an empty or non-SHAFT project at the Guided tab's "Create a
     * new SHAFT project" template. A separate signal from {@link #showFirstRunWelcomeIfNeeded()} --
     * keyed off project state, not {@code firstRunCoachDismissed} -- and purely a suggestion:
     * dismissing it, or any other Assistant control, works exactly as before.
     */
    private static JPanel freshProjectNotice(Project project) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        JLabel label = new JLabel(
                "Starting fresh? The Guided tab has a \"Create a new SHAFT project\" template to help set one up.");
        label.getAccessibleContext().setAccessibleName("Fresh project hint");
        // Issue #3603: the name above is a stable category label; the description carries the
        // actual informational text so a screen reader hears it, not just the generic name.
        label.getAccessibleContext().setAccessibleDescription(label.getText());
        panel.add(label);
        // Distinct visible text from the existing "Dismiss" button on the Capture review panel
        // (dismissCaptureReview): tests and users alike locate buttons by their plain label, and
        // this panel can be present in the tree (just invisible) at the same time as that one.
        JButton dismiss = new JButton("Got it");
        dismiss.getAccessibleContext().setAccessibleName("Dismiss fresh project hint");
        ShaftIconButtons.apply(dismiss, ShaftIcons.CANCEL);
        dismiss.addActionListener(event -> panel.setVisible(false));
        panel.add(dismiss);
        panel.setVisible(isFreshProject(project));
        return panel;
    }

    /**
     * Reuses the same {@code project.getBasePath()} resolution this file already uses elsewhere
     * (e.g. {@link #addProjectArtifact}) rather than a new path routine. The pom.xml read is the
     * same cheap, synchronous, local-file check {@code ShaftMcpSetupPanel} already runs straight on
     * the EDT for its "Upgrade project" step; a blank {@code latestVersion} is enough here because
     * {@code NOT_A_SHAFT_PROJECT} is decided before any version comparison happens (see {@link
     * ShaftProjectVersionCheck#check}).
     */
    private static boolean isFreshProject(Project project) {
        if (project == null || project.getBasePath() == null || project.getBasePath().isBlank()) {
            return false;
        }
        return ShaftProjectVersionCheck.check(Path.of(project.getBasePath()), "").state()
                == ShaftProjectVersionCheck.State.NOT_A_SHAFT_PROJECT;
    }

    private static JComboBox<String> combo(String accessibleName, String... values) {
        JComboBox<String> combo = new JComboBox<>(values);
        ShaftUiLabels.applyFriendlyRenderer(combo);
        combo.getAccessibleContext().setAccessibleName(accessibleName);
        return combo;
    }

    private static String escapeHtml(String value) {
        if (value == null || value.isBlank()) {
            return "";
        }
        return value.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
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
        if ("CLOUD".equals(normalize(settings.assistantProviderType, "LOCAL"))) {
            String model = settings.cloudModel == null || settings.cloudModel.isBlank() ? "" : " " + settings.cloudModel.trim();
            return ShaftUiLabels.friendly(normalizeLower(settings.cloudProvider, "gemini")) + model;
        }
        return ShaftUiLabels.friendly(resolveFamily(settings)) + " "
                + ShaftUiLabels.friendly(normalize(settings.assistantRuntime, "CLI"));
    }

    private String currentAgentConfigurationTooltip() {
        if ("CLOUD".equals(normalize(settings.assistantProviderType, "LOCAL"))) {
            String model = settings.cloudModel == null || settings.cloudModel.isBlank() ? "" : " / " + settings.cloudModel.trim();
            return "Agent: Cloud / " + ShaftUiLabels.friendly(normalizeLower(settings.cloudProvider, "gemini")) + model;
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
        PLAYWRIGHT,
        API
    }

    private record ToolEvidence(String toolName, String payload, String createdAt) {
    }

    /**
     * One entry of the prompt's trigger-character dropdown.
     *
     * @param label display label; may carry HTML markup for two-part command rows
     * @param insertion text inserted into the prompt when picked
     * @param matchText plain text used for filter matching and accessibility (never HTML)
     */
    record ContextSuggestion(String label, String insertion, String matchText) {
        ContextSuggestion(String label, String insertion) {
            this(label, insertion, label);
        }
    }

    private record CaptureReview(String markdown, String rawResult) {
    }
}
