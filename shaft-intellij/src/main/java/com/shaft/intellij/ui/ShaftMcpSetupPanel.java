package com.shaft.intellij.ui;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.FormBuilder;
import com.intellij.util.ui.JBUI;
import com.intellij.util.ui.WrapLayout;
import com.shaft.intellij.mcp.ShaftMcpConnectionProbe;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftPluginResetService;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTextPane;
import javax.swing.Timer;
import javax.swing.Icon;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.datatransfer.StringSelection;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.lang.reflect.Proxy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.function.Consumer;
import java.util.stream.Stream;

/**
 * First-run SHAFT MCP setup panel.
 */
final class ShaftMcpSetupPanel extends JPanel {
    private static final String INSTALLER_BRANCH = "main";
    private static final String MCP_DOCS_URL = "https://shafthq.github.io/docs/agentic/mcp";
    private static final int TOAST_MILLIS = 2500;
    private static final String[] INSTALLER_TARGETS = {
            "CODEX",
            "CLAUDE_CODE",
            "CLAUDE_DESKTOP",
            "COPILOT_CLI",
            "COPILOT_INTELLIJ",
            "INTELLIJ_PLUGIN"
    };
    private static final String GUIDE_SETUP_STEP =
            "Next: pick your agent, click the install command (a terminal opens with it pre-typed), run it, then check.";
    private static final String CHECK_NEXT_STEP = "Press Check now.";
    private static final String GEMINI_FAMILY = "GEMINI";
    private static final String GEMINI_KEY_NAME = "GEMINI_API_KEY";

    @FunctionalInterface
    interface AgentReadinessProbe {
        ShaftMcpToolResult test(String client, String runtime);
    }

    /**
     * Cloud provider API-key storage used by the setup check. Backed by IntelliJ Password Safe in
     * production and injectable so tests never touch the real credential store.
     */
    interface CloudKeyStore {
        boolean hasKey(String keyName);

        void saveKey(String keyName, char[] secret);
    }

    private final Project project;
    private final ShaftSettingsState.Settings settings;
    private final Runnable connected;
    private final AgentReadinessProbe readinessProbe;
    private final AgentReadinessProbe deepReadinessProbe;
    private final CloudKeyStore cloudKeyStore;
    private final String recommendedFamily;
    private final JBTextArea installerCommand;
    private final JBTextArea mcpCommand;
    private final JComboBox<String> family;
    private final JComboBox<String> runtime;
    private final javax.swing.JPasswordField geminiApiKey;
    private final JLabel geminiKeyStatus;
    private final JPanel runtimeRow;
    private final JPanel apiKeyRow;
    private final JComboBox<String> installerTarget;
    private final JCheckBox manualInstallerTarget;
    private final JButton copyUpgradeCommand;
    private final JButton checkUpgrade;
    private final JLabel upgradeDetail;
    private final JButton copyInstallerCommand;
    private final JButton test;
    private final JButton startChatting;
    private final JButton startWithoutAgent;
    private final JButton resetAndReinstall;
    private final JCheckBox expertMode;
    private final JButton resetEverything;
    private final JProgressBar progress;
    private final JLabel runtimeStatus;
    private final JLabel assistStatus;
    private final JLabel upgradeStep;
    private final JLabel upgradeState;
    private final JLabel chooseStep;
    private final JLabel chooseState;
    private final JLabel installStep;
    private final JLabel installState;
    private final JLabel testStep;
    private final JLabel testState;
    private final JLabel readyState;
    private final JLabel status;
    private final JLabel recommendedAgent;
    private final JLabel setupSummary;
    private final JLabel recoveryStatus;
    private final JLabel toast;
    private final JButton copyCommand;
    private final JButton copyOutput;
    private final JButton copyDocs;
    private final JButton copyRestartCommand;
    private final JTextPane details;
    private final JPanel installerDetailsPanel;
    private final JPanel detailsPanel;
    private final JPanel prerequisitesRow;
    private final JPanel prerequisitesList;
    private final JLabel prerequisitesStep;
    private final JLabel prerequisitesState;
    private final JPanel upgradeRow;
    private final JPanel chooseRow;
    private final JPanel installRow;
    private final JPanel checkRow;
    private final JPanel chatRow;
    private java.util.function.Function<String, List<SetupPrerequisites.Prerequisite>> prerequisitesDetector =
            SetupPrerequisites::detect;
    private String diagnosticCommand = "";
    private String diagnosticOutput = "";
    /** Real on-disk state (issue #3426 A5): whether an installed shaft-mcp was found locally. */
    private boolean installedCommandDetected;
    /** Whether the selected agent's runtime was actually detected on this machine. */
    private boolean selectedAgentDetected;
    /** Whether the last explicit "Check now" run failed, for the failed step badge. */
    private boolean lastCheckFailed;
    private ShaftProjectVersionCheck.Result upgradeCheckResult;
    /** Runs the real project-vs-latest SHAFT version comparison; injectable for tests. */
    private java.util.function.Supplier<ShaftProjectVersionCheck.Result> upgradeChecker = () ->
            ShaftProjectVersionCheck.check(projectRoot(), SetupPrerequisites.knownLatestEngineVersion());
    /**
     * Opens a terminal tab (name, command) with the command pre-typed; injectable for tests.
     * Returns whether the terminal actually opened so status text can say what really happened.
     */
    private java.util.function.BiPredicate<String, String> terminalOpener = this::openTerminalWithPreparedCommand;
    private Consumer<String> copySink = ShaftMcpSetupPanel::copyToClipboard;
    private Consumer<String> toastSink = this::showToast;
    private Timer toastTimer;
    private java.util.function.BooleanSupplier confirmReset = this::confirmResetDialog;
    private Runnable resetAction = () -> ShaftPluginResetService.getInstance().resetEverything();

    ShaftMcpSetupPanel(@NotNull Project project, @NotNull ShaftSettingsState.Settings settings,
                       @NotNull Runnable connected) {
        // Production: PATH-level probe for the fast recommended-agent scan at construction time,
        // and the deep client-can-access-shaft-mcp probe (which spawns the client CLI) reserved
        // for the explicit user-triggered "Check now" on a background thread.
        this(project, settings, connected, AssistantLocalAgentRunner::readiness,
                AssistantLocalAgentRunner::connectionReadiness, passwordSafeKeyStore());
    }

    ShaftMcpSetupPanel(@NotNull Project project, @NotNull ShaftSettingsState.Settings settings,
                       @NotNull Runnable connected, @NotNull AgentReadinessProbe readinessProbe) {
        this(project, settings, connected, readinessProbe, readinessProbe, passwordSafeKeyStore());
    }

    ShaftMcpSetupPanel(@NotNull Project project, @NotNull ShaftSettingsState.Settings settings,
                       @NotNull Runnable connected, @NotNull AgentReadinessProbe readinessProbe,
                       @NotNull AgentReadinessProbe deepReadinessProbe) {
        this(project, settings, connected, readinessProbe, deepReadinessProbe, passwordSafeKeyStore());
    }

    ShaftMcpSetupPanel(@NotNull Project project, @NotNull ShaftSettingsState.Settings settings,
                       @NotNull Runnable connected, @NotNull AgentReadinessProbe readinessProbe,
                       @NotNull CloudKeyStore cloudKeyStore) {
        this(project, settings, connected, readinessProbe, readinessProbe, cloudKeyStore);
    }

    ShaftMcpSetupPanel(@NotNull Project project, @NotNull ShaftSettingsState.Settings settings,
                       @NotNull Runnable connected, @NotNull AgentReadinessProbe readinessProbe,
                       @NotNull AgentReadinessProbe deepReadinessProbe,
                       @NotNull CloudKeyStore cloudKeyStore) {
        super(new BorderLayout(8, 8));
        this.project = project;
        this.settings = settings;
        this.connected = connected;
        this.readinessProbe = readinessProbe;
        this.deepReadinessProbe = deepReadinessProbe;
        this.cloudKeyStore = cloudKeyStore;
        recommendedFamily = recommendedFamily(settings);
        setBorder(JBUI.Borders.empty(12));

        installerCommand = commandArea(3, "MCP installer command");
        installerCommand.setEditable(false);
        installerCommand.getAccessibleContext().setAccessibleDescription(
                "Terminal command that installs SHAFT MCP for the selected assistant client.");
        mcpCommand = commandArea(4, "Managed SHAFT MCP command");
        mcpCommand.setText(settings.mcpCommand == null ? "" : settings.mcpCommand);
        mcpCommand.getAccessibleContext().setAccessibleDescription(
                "Local command used to start SHAFT MCP in stdio mode.");
        mcpCommand.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent event) {
                commandChanged();
            }

            @Override
            public void removeUpdate(DocumentEvent event) {
                commandChanged();
            }

            @Override
            public void changedUpdate(DocumentEvent event) {
                commandChanged();
            }
        });

        progress = new JProgressBar();
        progress.setIndeterminate(true);
        progress.setVisible(false);
        progress.setPreferredSize(JBUI.size(96, 14));
        family = new JComboBox<>(new String[]{"CODEX", "CLAUDE", "COPILOT", GEMINI_FAMILY});
        ShaftUiLabels.applyFriendlyRenderer(family);
        family.setSelectedItem(initialFamily(settings));
        family.getAccessibleContext().setAccessibleName("Assistant family");
        runtime = new JComboBox<>(new String[]{"CLI", "IDE_PLUGIN", "DESKTOP_APP"});
        ShaftUiLabels.applyFriendlyRenderer(runtime);
        runtime.setSelectedItem(normalize(settings.assistantRuntime, "CLI"));
        runtime.getAccessibleContext().setAccessibleName("Assistant runtime");
        geminiApiKey = new javax.swing.JPasswordField(24);
        geminiApiKey.getAccessibleContext().setAccessibleName("Gemini API key");
        geminiApiKey.getAccessibleContext().setAccessibleDescription(
                "Google AI Studio API key stored in IntelliJ Password Safe when the setup check passes.");
        geminiKeyStatus = setupStatusLabel("Gemini API key status");
        installerTarget = new JComboBox<>(INSTALLER_TARGETS);
        ShaftUiLabels.applyFriendlyRenderer(installerTarget);
        installerTarget.setSelectedItem(suggestedInstallerTarget());
        installerTarget.getAccessibleContext().setAccessibleName("MCP installer target");
        installerTarget.getAccessibleContext().setAccessibleDescription(
                "MCP client or plugin target used to build the installer command.");
        installerTarget.setVisible(false);
        manualInstallerTarget = new JCheckBox("MCP target");
        manualInstallerTarget.getAccessibleContext().setAccessibleName("Show manual MCP install target");
        manualInstallerTarget.setToolTipText("Show advanced installer targets when the inferred default is not correct");
        manualInstallerTarget.addActionListener(event -> {
            installerTarget.setVisible(manualInstallerTarget.isSelected());
            assistantSelectionChanged();
        });
        installerCommand.setText(installerCommand());
        copyUpgradeCommand = new JButton("Copy command");
        copyUpgradeCommand.getAccessibleContext().setAccessibleName("Copy SHAFT upgrade command");
        copyUpgradeCommand.setToolTipText("Copy the SHAFT upgrade command and open a terminal with it pre-typed "
                + "— just press Enter there to run it");
        copyUpgradeCommand.setMnemonic(KeyEvent.VK_U);
        applyLabeledAction(copyUpgradeCommand, ShaftIcons.COPY);
        copyUpgradeCommand.addActionListener(event -> copyUpgradeCommand());
        checkUpgrade = new JButton("Check");
        checkUpgrade.getAccessibleContext().setAccessibleName("Check SHAFT project version");
        checkUpgrade.setToolTipText("Read this project's pom.xml and compare its SHAFT version "
                + "against the latest release");
        applyLabeledAction(checkUpgrade, ShaftIcons.CHECK);
        checkUpgrade.addActionListener(event -> runUpgradeCheck(true));
        upgradeDetail = setupStatusLabel("SHAFT project version status");
        copyInstallerCommand = new JButton("Copy command");
        copyInstallerCommand.getAccessibleContext().setAccessibleName("Copy MCP installer command");
        copyInstallerCommand.setToolTipText("Copy the installer command and open a terminal with it pre-typed "
                + "— just press Enter there to run it");
        copyInstallerCommand.setMnemonic(KeyEvent.VK_C);
        applyLabeledAction(copyInstallerCommand, ShaftIcons.COPY);
        copyInstallerCommand.addActionListener(event -> copyInstallerCommand());
        test = new JButton("Check now");
        test.getAccessibleContext().setAccessibleName("Test SHAFT MCP connection");
        test.setToolTipText("Infer the stdio command and verify SHAFT MCP");
        test.setMnemonic(KeyEvent.VK_K);
        applyLabeledAction(test, ShaftIcons.CHECK);
        test.addActionListener(event -> testConnection());
        startChatting = new JButton("Start chatting");
        startChatting.getAccessibleContext().setAccessibleName("Start chatting with SHAFT Assistant");
        startChatting.setToolTipText("Open the Assistant with this verified MCP command");
        startChatting.setMnemonic(KeyEvent.VK_S);
        applyLabeledAction(startChatting, ShaftIcons.SEND);
        startChatting.setVisible(false);
        startChatting.addActionListener(event -> connected.run());
        startWithoutAgent = new JButton("Start without an agent");
        startWithoutAgent.getAccessibleContext().setAccessibleName("Start SHAFT without an agent");
        startWithoutAgent.setToolTipText("Recorder, codegen, doctor, and healer only need the verified SHAFT MCP. "
                + "Connect an agent later for chat.");
        applyLabeledAction(startWithoutAgent, ShaftIcons.SEND);
        startWithoutAgent.setVisible(false);
        startWithoutAgent.addActionListener(event -> connected.run());
        resetAndReinstall = new JButton("Reset / reinstall");
        resetAndReinstall.getAccessibleContext().setAccessibleName("Reset and reinstall SHAFT MCP");
        resetAndReinstall.setToolTipText("Clear the saved MCP command and copy a fresh installer command");
        resetAndReinstall.setMnemonic(KeyEvent.VK_R);
        applyLabeledAction(resetAndReinstall, ShaftIcons.RESET);
        resetAndReinstall.setVisible(false);
        resetAndReinstall.addActionListener(event -> resetAndCopyInstaller());
        boolean postSetupReentry = settings.mcpReady();
        expertMode = new JCheckBox("Enable expert mode");
        expertMode.getAccessibleContext().setAccessibleName("Enable expert mode");
        expertMode.setToolTipText("Show the full workflow selector (Guided, Recorder, Inspector, Triage, "
                + "Evidence, Projects, Advanced) instead of just Assistant");
        expertMode.setMnemonic(KeyEvent.VK_E);
        expertMode.setSelected(settings.advancedUiEnabled);
        expertMode.setVisible(postSetupReentry);
        expertMode.addActionListener(event -> settings.advancedUiEnabled = expertMode.isSelected());
        resetEverything = new JButton("Reset everything");
        resetEverything.getAccessibleContext().setAccessibleName("Reset everything");
        resetEverything.setToolTipText("Factory-reset SHAFT settings, saved provider API keys, tool approvals, "
                + "and Assistant chat history. Your project source code is never touched.");
        resetEverything.setMnemonic(KeyEvent.VK_V);
        applyLabeledAction(resetEverything, ShaftIcons.RESET);
        resetEverything.setForeground(ShaftStatusPresentation.error());
        resetEverything.setVisible(postSetupReentry);
        resetEverything.addActionListener(event -> confirmAndReset());
        runtimeStatus = setupStatusLabel("Assistant runtime setup status");
        assistStatus = setupStatusLabel("Assistant connection setup status");
        recommendedAgent = setupStatusLabel("Recommended assistant agent");
        recommendedAgent.setText(recommendedAgentText());
        recommendedAgent.setVisible(true);
        setupSummary = new JLabel();
        setupSummary.getAccessibleContext().setAccessibleName("SHAFT MCP setup summary");
        setupSummary.setText("Installer source: main. It configures the selected client and installs SHAFT MCP locally.");
        recoveryStatus = new JLabel();
        recoveryStatus.getAccessibleContext().setAccessibleName("SHAFT MCP recovery summary");
        recoveryStatus.setVisible(false);
        toast = new JLabel();
        toast.getAccessibleContext().setAccessibleName("SHAFT setup clipboard toast");
        toast.setOpaque(true);
        toast.setBackground(UIManagerColors.activeBackground());
        toast.setForeground(UIManagerColors.foreground());
        toast.setBorder(JBUI.Borders.compound(
                JBUI.Borders.customLine(ShaftStatusPresentation.progress(), 1),
                JBUI.Borders.empty(4, 8)));
        toast.setVisible(false);
        status = new JLabel();
        status.setPreferredSize(JBUI.size(560, 28));
        setStatusText(GUIDE_SETUP_STEP);
        status.getAccessibleContext().setAccessibleName("SHAFT MCP setup next step");
        copyCommand = new JButton("Copy command");
        copyCommand.getAccessibleContext().setAccessibleName("Copy setup diagnostic command");
        copyCommand.setToolTipText("Copy the diagnostic command");
        ShaftIconButtons.apply(copyCommand, ShaftIcons.CODE);
        copyCommand.setEnabled(false);
        copyCommand.setVisible(false);
        copyCommand.addActionListener(event -> copyDiagnosticCommand());
        copyOutput = new JButton("Copy output");
        copyOutput.getAccessibleContext().setAccessibleName("Copy setup diagnostic output");
        copyOutput.setToolTipText("Copy the setup diagnostic output");
        ShaftIconButtons.apply(copyOutput, ShaftIcons.COPY);
        copyOutput.setEnabled(false);
        copyOutput.addActionListener(event -> copyDiagnosticOutput());
        copyDocs = new JButton("Copy docs link");
        copyDocs.getAccessibleContext().setAccessibleName("Copy SHAFT MCP docs link");
        copyDocs.setToolTipText("Copy the SHAFT MCP setup docs link");
        copyDocs.setMnemonic(KeyEvent.VK_D);
        ShaftIconButtons.apply(copyDocs, ShaftIcons.HELP);
        copyDocs.setEnabled(false);
        copyDocs.setVisible(false);
        copyDocs.addActionListener(event -> copyDocsLink());
        copyRestartCommand = new JButton("Copy restart command");
        copyRestartCommand.getAccessibleContext().setAccessibleName("Copy assistant CLI restart command");
        copyRestartCommand.setToolTipText(
                "Copy a command that stops any running sessions of the selected assistant CLI and "
                        + "re-checks its shaft-mcp access, so it picks up a fresh shaft-mcp install");
        ShaftIconButtons.apply(copyRestartCommand, ShaftIcons.RESET);
        copyRestartCommand.setEnabled(false);
        copyRestartCommand.setVisible(false);
        copyRestartCommand.addActionListener(event -> copy(restartCommand(), "Copied assistant CLI restart command"));
        details = new JTextPane();
        details.setPreferredSize(JBUI.size(560, 180));
        details.getAccessibleContext().setAccessibleName("SHAFT MCP setup output");
        details.setEditable(false);
        details.setFont(new Font(Font.MONOSPACED, Font.PLAIN, details.getFont() == null ? 12 : details.getFont().getSize()));
        details.setBackground(UIManagerColors.background());
        details.setForeground(UIManagerColors.foreground());
        details.setBorder(JBUI.Borders.compound(JBUI.Borders.empty(6), JBUI.Borders.customLine(UIManagerColors.border(), 1)));

        upgradeStep = setupStepLabel("Upgrade project setup step");
        upgradeState = setupStateLabel("Upgrade project setup state");
        chooseStep = setupStepLabel("Choose agent setup step");
        chooseState = setupStateLabel("Choose agent setup state");
        installStep = setupStepLabel("Install SHAFT MCP setup step");
        installState = setupStateLabel("Install SHAFT MCP setup state");
        testStep = setupStepLabel("Check now setup step");
        testState = setupStateLabel("Check now setup state");
        readyState = setupStateLabel("Start chatting setup state");
        JPanel agentControls = new JPanel();
        agentControls.setLayout(new javax.swing.BoxLayout(agentControls, javax.swing.BoxLayout.Y_AXIS));
        agentControls.setOpaque(false);
        agentControls.add(labeledControl("Assistant family", family));
        runtimeRow = labeledControl("Runtime", runtime);
        agentControls.add(runtimeRow);
        apiKeyRow = labeledControl("Gemini API key", geminiApiKey);
        apiKeyRow.add(geminiKeyStatus);
        apiKeyRow.setVisible(false);
        agentControls.add(apiKeyRow);
        agentControls.add(recommendedAgent);
        JPanel checkActions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        checkActions.setOpaque(false);
        checkActions.add(test);
        checkActions.add(progress);
        checkActions.add(assistStatus);
        JPanel upgradeActions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        upgradeActions.setOpaque(false);
        upgradeActions.add(checkUpgrade);
        upgradeActions.add(copyUpgradeCommand);
        upgradeActions.add(upgradeDetail);
        JPanel installActions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        installActions.setOpaque(false);
        installActions.add(copyInstallerCommand);
        upgradeRow = stepRow(upgradeStep, upgradeState, upgradeActions);
        chooseRow = stepRow(chooseStep, chooseState, agentControls);
        installRow = stepRow(installStep, installState, installActions);
        checkRow = stepRow(testStep, testState, checkActions);
        prerequisitesList = new JPanel();
        prerequisitesList.setLayout(new javax.swing.BoxLayout(prerequisitesList, javax.swing.BoxLayout.Y_AXIS));
        prerequisitesList.setOpaque(false);
        prerequisitesList.getAccessibleContext().setAccessibleName("SHAFT setup prerequisites");
        JButton recheckPrerequisites = new JButton("Recheck");
        recheckPrerequisites.getAccessibleContext().setAccessibleName("Recheck prerequisites");
        recheckPrerequisites.setToolTipText("Detect the required tools again after installing one");
        applyLabeledAction(recheckPrerequisites, ShaftIcons.CHECK);
        recheckPrerequisites.addActionListener(event -> refreshPrerequisites());
        JButton copyEngineWarmup = new JButton("Copy SHAFT Engine warm-up command");
        // Resolve the latest engine release off-EDT now so the click below can pin a real version.
        SetupPrerequisites.prefetchLatestEngineVersion();
        copyEngineWarmup.getAccessibleContext().setAccessibleName("Copy SHAFT Engine warm-up command");
        copyEngineWarmup.setToolTipText("Copy a Maven command that downloads SHAFT Engine and its dependencies "
                + "into the local Maven repository so future projects reuse them without re-downloading");
        applyLabeledAction(copyEngineWarmup, ShaftIcons.COPY);
        copyEngineWarmup.addActionListener(event ->
                copy(SetupPrerequisites.shaftEngineWarmupCommand(), "Copied SHAFT Engine warm-up command"));
        JPanel prerequisitesActions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        prerequisitesActions.setOpaque(false);
        prerequisitesActions.add(recheckPrerequisites);
        prerequisitesActions.add(copyEngineWarmup);
        JPanel prerequisitesControls = new JPanel();
        prerequisitesControls.setLayout(new javax.swing.BoxLayout(prerequisitesControls, javax.swing.BoxLayout.Y_AXIS));
        prerequisitesControls.setOpaque(false);
        prerequisitesControls.add(prerequisitesList);
        prerequisitesControls.add(prerequisitesActions);
        prerequisitesStep = setupStepLabel("Prerequisites setup step");
        prerequisitesState = setupStateLabel("Prerequisites setup state");
        prerequisitesRow = stepRow(prerequisitesStep, prerequisitesState, prerequisitesControls);
        JLabel readyStep = setupStepLabel("Start chatting setup step");
        readyStep.setText("Ready");
        JPanel readyActions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        readyActions.setOpaque(false);
        readyActions.add(startChatting);
        readyActions.add(startWithoutAgent);
        chatRow = stepRow(readyStep, readyState, readyActions);
        chatRow.setVisible(false);
        JPanel workflow = new JPanel();
        workflow.setLayout(new javax.swing.BoxLayout(workflow, javax.swing.BoxLayout.Y_AXIS));
        workflow.add(prerequisitesRow);
        workflow.add(javax.swing.Box.createVerticalStrut(6));
        workflow.add(upgradeRow);
        workflow.add(javax.swing.Box.createVerticalStrut(6));
        workflow.add(chooseRow);
        workflow.add(javax.swing.Box.createVerticalStrut(6));
        workflow.add(installRow);
        workflow.add(javax.swing.Box.createVerticalStrut(6));
        workflow.add(checkRow);
        workflow.add(javax.swing.Box.createVerticalStrut(6));
        workflow.add(chatRow);
        JPanel targetRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        targetRow.add(manualInstallerTarget);
        targetRow.add(installerTarget);
        JPanel diagnosticRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        diagnosticRow.add(copyCommand);
        diagnosticRow.add(copyOutput);
        diagnosticRow.add(copyDocs);
        diagnosticRow.add(copyRestartCommand);
        JPanel secondaryActions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        secondaryActions.add(resetAndReinstall);
        JPanel postSetupControls = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        postSetupControls.getAccessibleContext().setAccessibleName("SHAFT plugin post-setup controls");
        postSetupControls.add(expertMode);
        postSetupControls.add(resetEverything);
        family.addActionListener(event -> assistantSelectionChanged());
        runtime.addActionListener(event -> assistantSelectionChanged());
        installerTarget.addActionListener(event -> installerTargetChanged());
        showRuntimeSelected();
        showAssistNotConfigured();
        updateCloudControls();
        JPanel intro = new JPanel(new BorderLayout(4, 2));
        JLabel title = new JLabel("Connect SHAFT Assistant");
        title.setFont(title.getFont().deriveFont(Font.BOLD, title.getFont().getSize2D() + 3f));
        // Agent-agnostic positioning (issue #3425 C3): the same workflows run on every agent.
        JLabel summary = new JLabel("<html><body style='width: 240px'>Pick an agent — the same /record-web, "
                + "/codegen, and /doctor flows work on Codex, Claude Code, Copilot, and Gemini. SHAFT handles the "
                + "wiring.</body></html>");
        summary.setForeground(ShaftStatusPresentation.pending());
        JLabel whyShaft = new JLabel();
        whyShaft.getAccessibleContext().setAccessibleName("Why SHAFT summary");
        whyShaft.setText("<html><body style='width: 240px'><b>Why SHAFT?</b> Privacy-safe recording (typed values "
                + "externalized, secrets redacted) · compile-validated codegen (generated tests compile and replay "
                + "before you see them) · repo-aware generation (reuses your page objects and locators) · a "
                + "maintenance loop (Doctor triage + Healer locator repair when tests break).</body></html>");
        whyShaft.setBorder(JBUI.Borders.empty(4, 0, 0, 0));
        intro.add(title, BorderLayout.NORTH);
        intro.add(summary, BorderLayout.CENTER);
        intro.add(whyShaft, BorderLayout.SOUTH);
        installerDetailsPanel = FormBuilder.createFormBuilder()
                .addComponent(targetRow)
                .addLabeledComponent("Installer command", new JBScrollPane(installerCommand))
                .getPanel();
        installerDetailsPanel.setVisible(false);
        JPanel form = FormBuilder.createFormBuilder()
                .addComponent(intro)
                .addComponent(setupSummary)
                .addComponent(runtimeStatus)
                .addComponent(workflow)
                .addComponent(installerDetailsPanel)
                .addComponent(status)
                .addComponent(toast)
                .addComponent(recoveryStatus)
                .addComponent(secondaryActions)
                .addComponent(postSetupControls)
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
        detailsPanel = new JPanel(new BorderLayout(4, 4));
        detailsPanel.add(diagnosticRow, BorderLayout.NORTH);
        detailsPanel.add(new JBScrollPane(details), BorderLayout.CENTER);
        detailsPanel.setVisible(false);
        // The setup flow has grown taller than many tool-window sizes, so the whole page lives in
        // a vertical scroll pane: the scrollbar appears only when needed and the user can always
        // reach the bottom. Width tracks the viewport so rows re-wrap instead of scrolling sideways.
        JPanel content = new ScrollableContentPanel(new BorderLayout(8, 8));
        content.add(form, BorderLayout.NORTH);
        content.add(detailsPanel, BorderLayout.CENTER);
        JBScrollPane setupScroll = new JBScrollPane(content,
                javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        setupScroll.setBorder(JBUI.Borders.empty());
        setupScroll.getVerticalScrollBar().setUnitIncrement(16);
        setupScroll.getAccessibleContext().setAccessibleName("SHAFT MCP setup scroll pane");
        add(setupScroll, BorderLayout.CENTER);
        refreshPrerequisites();
        refreshRealChecks();
        runUpgradeCheck(false);
        if (!currentCommand().isBlank()) {
            setStatusText(CHECK_NEXT_STEP);
        }
        updateActionState(false);
    }

    /**
     * Re-runs the cheap on-disk/PATH detections every step badge is derived from, so no step is
     * ever marked done just because a button was clicked (issue #3426 A5): the install step is
     * done only when an installed shaft-mcp is actually found, and the agent step only when the
     * selected agent runtime is really detected.
     */
    private void refreshRealChecks() {
        installedCommandDetected = !inferInstalledStdioCommand().isBlank();
        selectedAgentDetected = detectSelectedAgent();
    }

    private boolean detectSelectedAgent() {
        if (cloudFamilySelected()) {
            return cloudKeyStore.hasKey(GEMINI_KEY_NAME);
        }
        ShaftMcpToolResult result = readinessProbe.test(
                clientFromFamily(normalize(String.valueOf(family.getSelectedItem()), "CODEX")),
                normalize(String.valueOf(runtime.getSelectedItem()), "CLI"));
        return result != null && result.success();
    }

    /**
     * Runs the real "Upgrade project" check: parse the project pom, compare its SHAFT version to
     * the latest release, and reflect the truth in the step badge and detail label.
     */
    private void runUpgradeCheck(boolean announce) {
        upgradeCheckResult = upgradeChecker.get();
        upgradeDetail.setText(upgradeDetailText());
        upgradeDetail.setForeground(switch (upgradeCheckResult.state()) {
            case UP_TO_DATE -> ShaftStatusPresentation.success();
            case UPGRADE_AVAILABLE -> ShaftStatusPresentation.progress();
            default -> ShaftStatusPresentation.pending();
        });
        if (announce) {
            setStatusText(upgradeDetailText());
        }
        updateActionState(false);
    }

    private String upgradeDetailText() {
        if (upgradeCheckResult == null) {
            return "";
        }
        return switch (upgradeCheckResult.state()) {
            case UP_TO_DATE -> "SHAFT " + upgradeCheckResult.projectVersion() + " detected — already the latest ("
                    + upgradeCheckResult.latestVersion() + ") or newer. Nothing to do.";
            case UPGRADE_AVAILABLE -> "SHAFT " + upgradeCheckResult.projectVersion() + " detected — "
                    + upgradeCheckResult.latestVersion() + " is available. Run the upgrade command, then press Check.";
            // Project-shape-aware guidance (issue #3425 A5): an empty folder needs a project
            // scaffold, while a plain Maven project needs the SHAFT upgrade/adoption command.
            case NOT_A_SHAFT_PROJECT -> upgradeCheckResult.pomPresent()
                    ? "Maven project detected without a SHAFT dependency. Run the upgrade command to adopt SHAFT, "
                    + "then press Check."
                    : "No pom.xml found here. Ask the Assistant to \"create a SHAFT project\" (or use the Projects "
                    + "workflow) to scaffold one, then press Check.";
            case LATEST_UNKNOWN -> "SHAFT " + upgradeCheckResult.projectVersion() + " detected, but the latest "
                    + "release could not be determined (offline?). Press Check to retry.";
        };
    }

    /**
     * Scroll-pane view that always matches the viewport's width (content re-wraps vertically
     * instead of ever scrolling horizontally) while keeping its own preferred height so the
     * vertical scrollbar appears exactly when the setup flow outgrows the tool window.
     */
    private static final class ScrollableContentPanel extends JPanel implements javax.swing.Scrollable {
        ScrollableContentPanel(java.awt.LayoutManager layout) {
            super(layout);
        }

        @Override
        public java.awt.Dimension getPreferredScrollableViewportSize() {
            return getPreferredSize();
        }

        @Override
        public int getScrollableUnitIncrement(java.awt.Rectangle visibleRect, int orientation, int direction) {
            return 16;
        }

        @Override
        public int getScrollableBlockIncrement(java.awt.Rectangle visibleRect, int orientation, int direction) {
            return orientation == javax.swing.SwingConstants.VERTICAL ? visibleRect.height : visibleRect.width;
        }

        @Override
        public boolean getScrollableTracksViewportWidth() {
            return true;
        }

        @Override
        public boolean getScrollableTracksViewportHeight() {
            return false;
        }
    }

    /**
     * Re-detects the setup prerequisites for the selected assistant family and rebuilds the
     * prerequisites step: each tool is either shown as detected or paired with a copyable per-OS
     * install command, so a fresh machine can be provisioned entirely from this screen.
     */
    private void refreshPrerequisites() {
        prerequisitesList.removeAll();
        List<SetupPrerequisites.Prerequisite> detected =
                prerequisitesDetector.apply(String.valueOf(family.getSelectedItem()));
        boolean allRequiredPresent = true;
        for (SetupPrerequisites.Prerequisite prerequisite : detected) {
            boolean blocking = !prerequisite.present() && prerequisite.required();
            allRequiredPresent &= !blocking;
            JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
            row.setOpaque(false);
            String stateText = prerequisite.present() ? " detected"
                    : prerequisite.required() ? " missing" : " missing (optional; auto-installed when needed)";
            JLabel label = new JLabel((prerequisite.present()
                    ? ShaftStatusPresentation.SUCCESS_ICON
                    : ShaftStatusPresentation.WARNING_ICON) + " " + prerequisite.name() + stateText);
            label.setForeground(prerequisite.present() ? ShaftStatusPresentation.success()
                    : blocking ? ShaftStatusPresentation.error() : ShaftStatusPresentation.pending());
            label.getAccessibleContext().setAccessibleName(prerequisite.name() + stateText.trim());
            row.add(label);
            if (!prerequisite.present()) {
                JButton copyInstall = new JButton("Copy install command");
                copyInstall.getAccessibleContext().setAccessibleName(
                        "Copy " + prerequisite.name() + " install command");
                copyInstall.setToolTipText("Copy the terminal command that installs " + prerequisite.name());
                applyLabeledAction(copyInstall, ShaftIcons.COPY);
                String installCommand = prerequisite.installCommand();
                String copiedMessage = "Copied " + prerequisite.name() + " install command";
                copyInstall.addActionListener(event -> copy(installCommand, copiedMessage));
                row.add(copyInstall);
            }
            prerequisitesList.add(row);
        }
        setStep(prerequisitesStep, prerequisitesState, "0 Prerequisites", allRequiredPresent ? "done" : "next");
        styleStepRow(prerequisitesRow, allRequiredPresent ? "done" : "next");
        prerequisitesList.revalidate();
        prerequisitesList.repaint();
    }

    JComponent preferredFocusComponent() {
        return family;
    }

    private void testConnection() {
        String command = currentCommand();
        if (command.isBlank()) {
            String inferred = inferInstalledStdioCommand();
            if (!inferred.isBlank()) {
                mcpCommand.setText(inferred);
                command = inferred;
            }
        }
        settings.mcpCommand = command;
        applySelectionToSettings();
        if (command.isBlank()) {
            showAssistError();
            settings.mcpSetupComplete = false;
            settings.agentGuidanceOptimizationPromptPending = false;
            setStatusText("Run installer, then check again.");
            setDiagnosticText(troubleshootingDetails("Probe failed",
                    "No SHAFT MCP command configured.", "", true), "");
            updateActionState(false);
            return;
        }
        showAssistConnecting();
        setRunning(true, "Testing...");
        // The deep readiness probe spawns the selected client CLI (Claude's `mcp get` health check
        // takes ~10s), so it runs here on the probe's background completion thread — never on the
        // EDT. Selection-derived inputs are captured before leaving the EDT.
        boolean cloudSelected = cloudFamilySelected();
        String selectedClient = settings.defaultAutobotClient;
        String selectedRuntime = settings.assistantRuntime;
        ShaftMcpConnectionProbe.test(command, settings, projectRoot()).whenComplete((result, error) -> {
            ShaftMcpToolResult precomputedReadiness =
                    error == null && result != null && result.success() && !cloudSelected
                            ? deepReadinessProbe.test(selectedClient, selectedRuntime)
                            : null;
            ApplicationManager.getApplication().invokeLater(
                    () -> showTestResult(result, error, precomputedReadiness));
        });
    }

    private static JBTextArea commandArea(int rows, String accessibleName) {
        JBTextArea area = new JBTextArea(rows, 56);
        area.setLineWrap(true);
        area.setWrapStyleWord(true);
        area.getAccessibleContext().setAccessibleName(accessibleName);
        area.setFont(new Font(Font.MONOSPACED, Font.PLAIN, area.getFont() == null ? 12 : area.getFont().getSize()));
        area.setMinimumSize(JBUI.size(360, Math.max(72, rows * 24)));
        return area;
    }

    private Path projectRoot() {
        return project.getBasePath() == null ? Path.of(".") : Path.of(project.getBasePath());
    }

    /**
     * Whether this project already carries the AGENTS.md guidance scaffold the
     * agent-guidance-optimization prompt and its referenced validator script
     * are designed for. Installing/prompting that validator into an unrelated
     * project makes it crash on files (README.md, host-context files, guidance
     * budgets) it has no reason to expect -- so the prompt must not be shown
     * unless the project has actually adopted this scaffold.
     */
    private boolean hasAgentGuidanceScaffold() {
        return Files.isRegularFile(projectRoot().resolve("AGENTS.md"));
    }

    private void showTestResult(ShaftMcpToolResult result, Throwable error) {
        showTestResult(result, error, null);
    }

    private void showTestResult(ShaftMcpToolResult result, Throwable error, ShaftMcpToolResult precomputedReadiness) {
        boolean success = error == null && result != null && result.success();
        boolean agentReady = false;
        setRunning(false, success ? "Connected" : "Test failed. Retry test.");
        if (error != null) {
            showAssistError();
            setDiagnosticText(troubleshootingDetails("Probe failed", error.getMessage(), mcpCommand(), true), mcpCommand());
        } else if (result == null) {
            showAssistError();
            setDiagnosticText(troubleshootingDetails("Probe failed", "No test result returned.", mcpCommand(), true),
                    mcpCommand());
        } else {
            if (result.success()) {
                clearDiagnostics();
                ShaftMcpToolResult readiness = verifySelectedAgentReadiness(precomputedReadiness);
                agentReady = readiness.success();
                if (agentReady) {
                    showAssistConfigured();
                    showRuntimeVerified();
                    setStatusText(successSummary(result.output()));
                } else {
                    // Two-lane readiness (issue #3425 A2): the recorder, codegen, doctor, and
                    // healer only need a verified SHAFT MCP — a working agent adds chat and is
                    // the optional second lane. A missing agent therefore no longer fails setup.
                    showAssistStatus("MCP verified — agent optional", ShaftStatusPresentation.progress());
                    assistStatus.setVisible(true);
                    showRuntimeVerified();
                    setStatusText("SHAFT MCP verified. Recorder, codegen, and doctor are ready now — "
                            + "connecting an agent adds chat and is optional.");
                    setDiagnosticText(troubleshootingDetails("Agent lane not ready (optional)",
                            "MCP probe output:\n" + result.output() + "\n\nAgent readiness failed: " + readiness.output(),
                            readinessDiagnosticCommand(), true), readinessDiagnosticCommand());
                    showRestartCommandRecovery();
                }
            } else {
                showAssistError();
                setDiagnosticText(troubleshootingDetails("Probe failed", result.output(), mcpCommand(), true),
                        mcpCommand());
            }
        }
        lastCheckFailed = !success;
        refreshRealChecks();
        if (success) {
            settings.mcpSetupComplete = true;
            settings.agentGuidanceOptimizationPromptPending = hasAgentGuidanceScaffold();
            startChatting.setVisible(agentReady);
            startWithoutAgent.setVisible(!agentReady);
            (agentReady ? startChatting : startWithoutAgent).requestFocusInWindow();
        } else {
            settings.mcpSetupComplete = false;
            settings.agentGuidanceOptimizationPromptPending = false;
            startChatting.setVisible(false);
            startWithoutAgent.setVisible(false);
            showRuntimeSelected();
        }
        updateActionState(false);
    }

    private void setRunning(boolean running, String text) {
        updateActionState(running);
        progress.setVisible(running);
        installerCommand.setEnabled(!running);
        installerTarget.setEnabled(!running);
        manualInstallerTarget.setEnabled(!running);
        mcpCommand.setEnabled(!running);
        family.setEnabled(!running);
        runtime.setEnabled(!running);
        geminiApiKey.setEnabled(!running);
        setStatusText(text);
    }

    private void updateActionState(boolean running) {
        boolean hasCommand = !currentCommand().isBlank();
        boolean complete = settings.mcpSetupComplete && hasCommand;
        boolean showCopy = !complete && !(installedCommandDetected || hasCommand);
        // The real check is always available while setup is incomplete: verification is how a
        // step earns its Done badge, never a button click (issue #3426 A5).
        boolean showTest = !complete;
        copyInstallerCommand.setVisible(showCopy);
        copyInstallerCommand.setEnabled(!running && showCopy);
        test.setVisible(showTest);
        test.setEnabled(!running && showTest);
        boolean upgradeDone = upgradeCheckResult != null
                && upgradeCheckResult.state() == ShaftProjectVersionCheck.State.UP_TO_DATE;
        copyUpgradeCommand.setVisible(!upgradeDone);
        copyUpgradeCommand.setEnabled(!running && !upgradeDone);
        checkUpgrade.setEnabled(!running);
        startChatting.setVisible((complete && !startWithoutAgent.isVisible()) || startChatting.isVisible());
        startChatting.setEnabled(!running && startChatting.isVisible());
        startWithoutAgent.setEnabled(!running && startWithoutAgent.isVisible());
        chatRow.setVisible(startChatting.isVisible() || startWithoutAgent.isVisible());
        copyCommand.setEnabled(!running && !diagnosticCommand.isBlank());
        boolean canReset = complete || detailsPanel.isVisible();
        resetAndReinstall.setVisible(canReset);
        resetAndReinstall.setEnabled(!running && canReset);
        updateProgressivePanels();
        updateSetupSteps(running);
        updateWorkflowRows(running);
        updateLiveSummary();
    }

    private void commandChanged() {
        clearDiagnostics();
        showAssistNotConfigured();
        startChatting.setVisible(false);
        startWithoutAgent.setVisible(false);
        settings.mcpSetupComplete = false;
        settings.agentGuidanceOptimizationPromptPending = false;
        lastCheckFailed = false;
        setStatusText(currentCommand().isBlank()
                ? GUIDE_SETUP_STEP
                : CHECK_NEXT_STEP);
        updateActionState(false);
    }

    private void assistantSelectionChanged() {
        updateCloudControls();
        showRuntimeSelected();
        refreshPrerequisites();
        refreshRealChecks();
        if (!manualInstallerTarget.isSelected()) {
            String suggestedTarget = suggestedInstallerTarget();
            if (!suggestedTarget.equals(String.valueOf(installerTarget.getSelectedItem()))) {
                installerTarget.setSelectedItem(suggestedTarget);
                return;
            }
        }
        installerTargetChanged();
    }

    private void installerTargetChanged() {
        clearDiagnostics();
        installerCommand.setText(installerCommand());
        lastCheckFailed = false;
        showAssistNotConfigured();
        startChatting.setVisible(false);
        startWithoutAgent.setVisible(false);
        settings.mcpSetupComplete = false;
        settings.agentGuidanceOptimizationPromptPending = false;
        setStatusText(currentCommand().isBlank()
                ? GUIDE_SETUP_STEP
                : CHECK_NEXT_STEP);
        updateActionState(false);
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

    private static String initialFamily(ShaftSettingsState.Settings settings) {
        boolean geminiCloudConfigured = "CLOUD".equals(normalize(settings.assistantProviderType, "LOCAL"))
                && "gemini".equalsIgnoreCase(settings.cloudProvider == null ? "" : settings.cloudProvider.trim());
        return geminiCloudConfigured ? GEMINI_FAMILY : resolveFamily(settings);
    }

    private boolean cloudFamilySelected() {
        return GEMINI_FAMILY.equals(normalize(String.valueOf(family.getSelectedItem()), "CODEX"));
    }

    /**
     * Writes the current agent selection into settings. A Gemini selection configures the cloud
     * provider route (issue #3369) and never overwrites the last local family, so switching back
     * to a local agent later restores the previous local configuration.
     */
    private void applySelectionToSettings() {
        if (cloudFamilySelected()) {
            settings.assistantProviderType = "CLOUD";
            settings.cloudProvider = "gemini";
            if (settings.cloudModel == null || settings.cloudModel.isBlank()) {
                settings.cloudModel = AssistantModelCatalog.defaultCloudModel("gemini");
            }
            settings.passProviderApiKeysToMcp = true;
            return;
        }
        settings.assistantProviderType = "LOCAL";
        settings.assistantFamily = String.valueOf(family.getSelectedItem());
        settings.assistantRuntime = String.valueOf(runtime.getSelectedItem());
        settings.defaultAutobotClient = clientFromFamily(settings.assistantFamily);
    }

    /**
     * Verifies the selected agent after a successful MCP probe: local families check their CLI
     * runtime and its actual access to shaft-mcp (using the readiness precomputed on the background
     * probe thread when available, since the deep check spawns the client CLI), while the Gemini
     * cloud route stores the entered API key and checks that one is present in Password Safe.
     */
    private ShaftMcpToolResult verifySelectedAgentReadiness(ShaftMcpToolResult precomputedReadiness) {
        if (!cloudFamilySelected()) {
            return precomputedReadiness != null
                    ? precomputedReadiness
                    : deepReadinessProbe.test(settings.defaultAutobotClient, settings.assistantRuntime);
        }
        applySelectionToSettings();
        storeEnteredGeminiKey();
        updateCloudControls();
        return cloudKeyStore.hasKey(GEMINI_KEY_NAME)
                ? ShaftMcpToolResult.success("Gemini API key is stored in IntelliJ Password Safe.")
                : ShaftMcpToolResult.failure(
                "No Gemini API key stored. Paste your Google AI Studio API key, then check again.");
    }

    private void storeEnteredGeminiKey() {
        char[] entered = geminiApiKey.getPassword();
        boolean meaningful = false;
        for (char character : entered) {
            if (!Character.isWhitespace(character)) {
                meaningful = true;
                break;
            }
        }
        if (meaningful) {
            cloudKeyStore.saveKey(GEMINI_KEY_NAME, entered);
            geminiApiKey.setText("");
        } else {
            java.util.Arrays.fill(entered, '\0');
        }
    }

    private void updateCloudControls() {
        boolean cloud = cloudFamilySelected();
        runtimeRow.setVisible(!cloud);
        apiKeyRow.setVisible(cloud);
        // The CLI recommendation only applies to local agent families.
        recommendedAgent.setVisible(!cloud);
        if (cloud) {
            geminiKeyStatus.setText(cloudKeyStore.hasKey(GEMINI_KEY_NAME)
                    ? "Key stored in Password Safe."
                    : "Paste your Google AI Studio API key.");
        }
    }

    private static CloudKeyStore passwordSafeKeyStore() {
        return new CloudKeyStore() {
            @Override
            public boolean hasKey(String keyName) {
                return com.shaft.intellij.settings.ShaftCredentialService.getInstance().hasApiKey(keyName);
            }

            @Override
            public void saveKey(String keyName, char[] secret) {
                com.shaft.intellij.settings.ShaftCredentialService.getInstance().setApiKey(keyName, secret);
            }
        };
    }

    private String recommendedFamily(ShaftSettingsState.Settings settings) {
        String explicit = normalize(settings.assistantFamily, "");
        if (!explicit.isBlank()) {
            return explicit;
        }
        String resolved = resolveFamily(settings);
        if (!"CLI".equals(normalize(settings.assistantRuntime, "CLI"))) {
            return resolved;
        }
        for (String familyCandidate : List.of("CODEX", "CLAUDE", "COPILOT")) {
            ShaftMcpToolResult result = readinessProbe.test(clientFromFamily(familyCandidate), "CLI");
            if (result != null && result.success()) {
                return familyCandidate;
            }
        }
        return resolved;
    }

    private String recommendedAgentText() {
        return "Recommended: " + cliAgentLabel(recommendedFamily) + agentRecommendationSuffix(recommendedFamily);
    }

    private static String cliAgentLabel(String family) {
        return switch (normalize(family, "CODEX")) {
            case "CLAUDE" -> "Claude Code CLI";
            case "COPILOT" -> "GitHub Copilot CLI";
            default -> "Codex CLI";
        };
    }

    private static String agentRecommendationSuffix(String family) {
        return " detected";
    }

    private static String clientFromFamily(String family) {
        return switch (normalize(family, "CODEX")) {
            case "CLAUDE" -> "CLAUDE_CODE";
            case "COPILOT" -> "COPILOT_CLI";
            default -> "CODEX";
        };
    }

    private String installerCommand() {
        return installerCommandFor(installerArgumentFor(String.valueOf(installerTarget.getSelectedItem())));
    }

    private String suggestedInstallerTarget() {
        return switch (normalize(String.valueOf(family.getSelectedItem()), "CODEX")) {
            case "CLAUDE" -> "DESKTOP_APP".equals(normalize(String.valueOf(runtime.getSelectedItem()), "CLI"))
                    ? "CLAUDE_DESKTOP"
                    : "CLAUDE_CODE";
            case "COPILOT" -> "IDE_PLUGIN".equals(normalize(String.valueOf(runtime.getSelectedItem()), "CLI"))
                    ? "COPILOT_INTELLIJ"
                    : "COPILOT_CLI";
            // Gemini prompts run through SHAFT MCP's provider chat, so only this plugin's own
            // MCP integration needs installing.
            case GEMINI_FAMILY -> "INTELLIJ_PLUGIN";
            default -> "CODEX";
        };
    }

    private static String installerArgumentFor(String target) {
        return switch (normalize(target, "CODEX")) {
            case "CLAUDE_CODE" -> "claude";
            case "CLAUDE_DESKTOP" -> "claude-desktop";
            case "COPILOT_CLI" -> "copilot";
            case "COPILOT_INTELLIJ" -> "copilot-intellij";
            case "INTELLIJ_PLUGIN" -> "intellij-plugin";
            default -> "codex";
        };
    }

    private static String installerCommandFor(String target) {
        String url = "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/" + INSTALLER_BRANCH
                + "/scripts/mcp/install-shaft-mcp";
        if (isWindows()) {
            return "powershell -NoProfile -ExecutionPolicy Bypass -Command '$installer=Join-Path $env:TEMP \"install-shaft-mcp.ps1\"; "
                    + "Invoke-WebRequest -UseBasicParsing \"" + url
                    + ".ps1\" -OutFile $installer; & $installer -Client " + target + " --install-shaft-skills'";
        }
        return "tmp=\"${TMPDIR:-/tmp}/install-shaft-mcp.sh\"; curl -fL " + url
                + ".sh -o \"$tmp\" && sh \"$tmp\" --" + target + " --install-shaft-skills";
    }

    /**
     * Downloads and runs the SHAFT modular-upgrade script against the current project. No
     * {@code --yes}: the script prints a diff and asks for interactive confirmation, which is the
     * right default for a first command a user runs against their own project.
     */
    private static String upgradeCommand() {
        String url = "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/" + INSTALLER_BRANCH
                + "/shaft-upgrader/upgrade_to_modular_shaft.py";
        if (isWindows()) {
            return "powershell -NoProfile -ExecutionPolicy Bypass -Command '$upgrader=Join-Path $env:TEMP "
                    + "\"upgrade_to_modular_shaft.py\"; Invoke-WebRequest -UseBasicParsing \"" + url
                    + "\" -OutFile $upgrader; py -3 $upgrader --project .'";
        }
        return "tmp=\"${TMPDIR:-/tmp}/upgrade_to_modular_shaft.py\"; curl -fL " + url
                + " -o \"$tmp\" && python3 \"$tmp\" --project .";
    }

    private void copyUpgradeCommand() {
        String command = upgradeCommand();
        copy(command, "Copied SHAFT upgrade command");
        boolean opened = terminalOpener.test("SHAFT upgrade", command);
        if (opened) {
            openIntellijTerminal();
        }
        setStatusText(opened
                ? "Terminal opened with the upgrade command pre-typed. Press Enter there to run it, then press Check."
                : "Upgrade command copied. Paste it into a terminal, run it, then press Check.");
        updateActionState(false);
    }

    private void copyInstallerCommand() {
        String command = installerCommand();
        copy(command, "Copied MCP installer command");
        boolean opened = terminalOpener.test("SHAFT MCP install", command);
        if (opened) {
            openIntellijTerminal();
        }
        setStatusText(opened
                ? "Terminal opened with the installer pre-typed. Press Enter there to run it; when it finishes, press Check now."
                : "Installer command copied. Paste it into a terminal, run it; when it finishes, press Check now.");
        updateActionState(false);
        test.requestFocusInWindow();
    }

    private void resetAndCopyInstaller() {
        clearDiagnostics();
        mcpCommand.setText("");
        settings.mcpCommand = "";
        settings.mcpSetupComplete = false;
        settings.agentGuidanceOptimizationPromptPending = false;
        installerCommand.setText(installerCommand());
        startChatting.setVisible(false);
        startWithoutAgent.setVisible(false);
        showRuntimeSelected();
        showAssistNotConfigured();
        refreshRealChecks();
        copy(installerCommand(), "Installer command copied. Run it in terminal, then check.");
        updateActionState(false);
        copyInstallerCommand.requestFocusInWindow();
    }

    /**
     * Confirms with the user, then delegates to {@link ShaftPluginResetService#resetEverything()}.
     * Cancelling the confirmation dialog is a no-op. Both the confirmation prompt and the reset
     * action itself are indirected through overridable fields so tests can drive this without a
     * real IntelliJ {@code Application}/dialog.
     */
    private void confirmAndReset() {
        if (!confirmReset.getAsBoolean()) {
            return;
        }
        resetAction.run();
    }

    private boolean confirmResetDialog() {
        return Messages.showYesNoDialog(
                project,
                """
                        This resets the SHAFT plugin to its factory state:
                        • All SHAFT plugin settings (agent selection, MCP command, toggles)
                        • Saved provider API keys in IntelliJ Password Safe
                        • Tool approvals for every open project
                        • Assistant chat history for every open project

                        After the reset, every setup step is re-checked against what is really on this machine — nothing stays green because of earlier clicks.

                        Not removed (they belong to your machine, not this plugin): your project source code, the shaft-mcp files under your local application data, Maven artifacts in ~/.m2, and SHAFT MCP registrations inside agent CLI configs (re-run the installer to update those).""",
                "Reset SHAFT Plugin",
                "Reset everything",
                "Cancel",
                Messages.getWarningIcon()) == Messages.YES;
    }

    private boolean openTerminalWithPreparedCommand(String tabName, String command) {
        return ShaftTerminalCommands.openWithPreparedCommand(project, projectRoot().toString(), tabName, command);
    }

    private void openIntellijTerminal() {
        if (project == null || Proxy.isProxyClass(project.getClass()) || ApplicationManager.getApplication() == null) {
            return;
        }
        ApplicationManager.getApplication().invokeLater(() -> {
            try {
                ToolWindow terminal = ToolWindowManager.getInstance(project).getToolWindow("Terminal");
                if (terminal != null) {
                    terminal.activate(null);
                }
            } catch (RuntimeException ignored) {
                // Terminal plugin/tool window may be unavailable; the command is already copied.
            }
        });
    }

    private static String inferInstalledStdioCommand() {
        return inferInstalledStdioCommand(applicationDataRoot(), bootstrapRoot());
    }

    static String inferInstalledStdioCommand(Path applicationDataRoot, Path bootstrapRoot) {
        Path argsFile = latestArgsFile(applicationDataRoot);
        if (argsFile == null) {
            return "";
        }
        Path java = installerJava(bootstrapRoot);
        return quote(java == null ? "java" : java.toString()) + " " + quote("@" + argsFile);
    }

    private static Path latestArgsFile(Path applicationDataRoot) {
        Path versions = applicationDataRoot.resolve("versions");
        if (!Files.isDirectory(versions)) {
            return null;
        }
        try (Stream<Path> entries = Files.list(versions)) {
            return entries.map(path -> path.resolve("shaft-mcp.args"))
                    .filter(Files::isRegularFile)
                    .max(Comparator.comparingLong(ShaftMcpSetupPanel::lastModified))
                    .orElse(null);
        } catch (IOException ignored) {
            return null;
        }
    }

    private static Path installerJava(Path bootstrapRoot) {
        Path jdkRoot = bootstrapRoot.resolve("tools").resolve("jdk");
        if (!Files.isDirectory(jdkRoot)) {
            return null;
        }
        String executable = isWindows() ? "java.exe" : "java";
        try (Stream<Path> entries = Files.walk(jdkRoot, 8)) {
            return entries.filter(Files::isRegularFile)
                    .filter(path -> executable.equals(path.getFileName().toString()))
                    .filter(path -> path.getParent() != null && "bin".equals(path.getParent().getFileName().toString()))
                    .max(Comparator.comparingLong(ShaftMcpSetupPanel::lastModified))
                    .orElse(null);
        } catch (IOException ignored) {
            return null;
        }
    }

    private static long lastModified(Path path) {
        try {
            return Files.getLastModifiedTime(path).toMillis();
        } catch (IOException ignored) {
            return 0L;
        }
    }

    private static Path applicationDataRoot() {
        String override = System.getProperty("shaft.intellij.mcp.applicationDataRoot");
        if (override != null && !override.isBlank()) {
            return Path.of(override);
        }
        Path home = Path.of(System.getProperty("user.home", "."));
        if (isWindows()) {
            String localAppData = System.getenv("LOCALAPPDATA");
            Path base = localAppData == null || localAppData.isBlank()
                    ? home.resolve("AppData").resolve("Local")
                    : Path.of(localAppData);
            return base.resolve("ShaftHQ").resolve("shaft-mcp");
        }
        if (isMac()) {
            return home.resolve("Library").resolve("Application Support").resolve("ShaftHQ").resolve("shaft-mcp");
        }
        String dataHome = System.getenv("XDG_DATA_HOME");
        Path base = dataHome == null || dataHome.isBlank()
                ? home.resolve(".local").resolve("share")
                : Path.of(dataHome);
        return base.resolve("shafthq").resolve("shaft-mcp");
    }

    private static Path bootstrapRoot() {
        String override = System.getProperty("shaft.intellij.mcp.bootstrapRoot");
        if (override != null && !override.isBlank()) {
            return Path.of(override);
        }
        Path home = Path.of(System.getProperty("user.home", "."));
        if (isWindows()) {
            String localAppData = System.getenv("LOCALAPPDATA");
            Path base = localAppData == null || localAppData.isBlank()
                    ? home.resolve("AppData").resolve("Local")
                    : Path.of(localAppData);
            return base.resolve("ShaftHQ").resolve("shaft-mcp").resolve("bootstrap");
        }
        if (isMac()) {
            return home.resolve("Library").resolve("Caches").resolve("ShaftHQ").resolve("shaft-mcp-bootstrap");
        }
        String cacheHome = System.getenv("XDG_CACHE_HOME");
        Path base = cacheHome == null || cacheHome.isBlank()
                ? home.resolve(".cache")
                : Path.of(cacheHome);
        return base.resolve("shafthq").resolve("shaft-mcp-bootstrap");
    }

    private static boolean isWindows() {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("win");
    }

    private static boolean isMac() {
        return System.getProperty("os.name", "").toLowerCase(Locale.ROOT).contains("mac");
    }

    private static String quote(String value) {
        if (value.isBlank()) {
            return "\"\"";
        }
        if (value.chars().noneMatch(Character::isWhitespace) && !value.contains("\"")) {
            return value;
        }
        return "\"" + value.replace("\"", "\\\"") + "\"";
    }

    private String mcpCommand() {
        return settings.mcpCommand == null ? "" : settings.mcpCommand.trim();
    }

    private String currentCommand() {
        return mcpCommand.getText() == null ? "" : mcpCommand.getText().trim();
    }

    /**
     * A command the user can paste into a terminal to properly restart the selected assistant CLI:
     * running sessions started before a shaft-mcp (re)install keep the old MCP registration, so
     * stale processes are stopped and a fresh invocation re-checks shaft-mcp access.
     */
    String restartCommand() {
        String executable = switch (normalize(String.valueOf(family.getSelectedItem()), "CODEX")) {
            case "CLAUDE" -> "claude";
            case "COPILOT" -> "copilot";
            case GEMINI_FAMILY -> "";
            default -> "codex";
        };
        if (executable.isBlank()) {
            return "";
        }
        String verification = "copilot".equals(executable) ? executable + " --version" : executable + " mcp list";
        if (isWindows()) {
            return "powershell -NoProfile -Command \"Get-Process " + executable
                    + " -ErrorAction SilentlyContinue | Stop-Process -Force; " + verification + "\"";
        }
        return "pkill -x " + executable + " 2>/dev/null; " + verification;
    }

    /**
     * Surfaces the restart-command recovery action after a failed client readiness check for a
     * local CLI family. The restart path is what fixes the common "just reinstalled shaft-mcp but
     * my CLI still cannot see it" case, so the copy button is offered for every readiness failure,
     * not only ones whose message mentions restarting.
     */
    private void showRestartCommandRecovery() {
        if (restartCommand().isBlank()) {
            return;
        }
        copyRestartCommand.setVisible(true);
        copyRestartCommand.setEnabled(true);
        recoveryStatus.setText("Recovery: run the installer or copy the restart command, then check again.");
        recoveryStatus.setVisible(true);
    }

    private String readinessDiagnosticCommand() {
        return switch (normalize(String.valueOf(family.getSelectedItem()), "CODEX")) {
            case "CLAUDE" -> "claude --version";
            case "COPILOT" -> "copilot --version";
            case GEMINI_FAMILY -> "";
            default -> "codex --version";
        };
    }

    private String troubleshootingDetails(String title, String output, String command, boolean probeFailure) {
        String detailsOutput = output == null || output.isBlank() ? "No diagnostic output returned." : output.trim();
        String category = failureCategory(detailsOutput);
        String commandText = visibleDiagnosticCommand(command);
        StringBuilder formatted = new StringBuilder(title).append("\n\n")
                .append("What failed\n")
                .append("- Category: ").append(category).append('\n')
                .append("- Client: ").append(clientDisplayName()).append('\n')
                .append("- Runtime: ").append(runtimeDisplayName()).append("\n\n")
                .append("Next steps\n");
        for (String step : troubleshootingSteps(category, probeFailure)) {
            formatted.append("- ").append(step).append('\n');
        }
        return formatted.append("\nDiagnostic command\n")
                .append(commandText)
                .append("\n\nOutput\n")
                .append(detailsOutput)
                .toString();
    }

    private String failureCategory(String output) {
        String text = output == null ? "" : output.toLowerCase(Locale.ROOT);
        if (text.contains("no shaft mcp command configured")
                || text.contains("enter a shaft mcp stdio command")) {
            return "MCP command";
        }
        if (text.contains("not registered with")) {
            return "Client MCP registration";
        }
        if (text.contains("could not connect to it")) {
            return "Client MCP connection";
        }
        if (text.contains("unsupportedclassversionerror")
                || text.contains("java_home")
                || text.contains("could not find or load main class")
                || text.contains("cannot run program \"java\"")
                || text.contains("java: command not found")
                || text.contains("java is not recognized")
                || text.contains("java runtime")
                || text.contains("local java")) {
            return "Java/runtime";
        }
        if (text.contains("could not resolve")
                || text.contains("could not find artifact")
                || text.contains("failed to collect dependencies")
                || text.contains("could not transfer artifact")
                || text.contains("io.github.shafthq:shaft-mcp")) {
            return "Maven artifact resolution";
        }
        if (text.contains("mcp.json")
                || text.contains("config.toml")
                || text.contains("client configuration")
                || text.contains("failed to write")
                || text.contains("permission denied")) {
            return "Client configuration";
        }
        if (text.contains("not available on path")
                || text.contains("executable is not available")
                || text.contains("cli executable")
                || text.contains("client executable")
                || text.contains("command not found")) {
            return "Client runtime";
        }
        return "MCP probe";
    }

    private List<String> troubleshootingSteps(String category, boolean probeFailure) {
        List<String> steps = new ArrayList<>();
        switch (category) {
            case "MCP command" -> steps.add("Run the installer command, then check again. SHAFT will find the local command automatically.");
            case "Client MCP registration" -> steps.add(
                    "Run the installer command for the selected client, then check again.");
            case "Client MCP connection" -> steps.add(
                    "Restart the selected client CLI (copy the restart command below), then check again.");
            case "Java/runtime" -> steps.add("Install or select a Java runtime that can run shaft-mcp, then retry.");
            case "Maven artifact resolution" -> steps.add(
                    "Check Maven Central/network access and retry once artifact resolution is available.");
            case "Client configuration" -> steps.add(
                    "Check that the selected client can write and read its MCP configuration file.");
            case "Client runtime" -> steps.add(
                    "Install the selected client CLI or add it to PATH, then retry.");
            default -> steps.add("Run the installer command again, then check setup once it finishes.");
        }
        if (probeFailure) {
            steps.add(clientSpecificStep());
        }
        return steps;
    }

    private String clientSpecificStep() {
        return switch (normalize(String.valueOf(family.getSelectedItem()), "CODEX")) {
            case "CLAUDE" -> "For Claude, run `claude mcp list` for Claude Code or restart Claude Desktop after desktop config changes.";
            case "COPILOT" -> "For GitHub Copilot, check the Copilot MCP client configuration and any organization MCP policy.";
            case GEMINI_FAMILY -> "For Gemini, paste a valid Google AI Studio API key in the setup form, then check again.";
            default -> "For Codex, run `codex mcp list` and verify the SHAFT MCP server in the Codex config.";
        };
    }

    private String visibleDiagnosticCommand(String command) {
        if (command == null || command.isBlank()) {
            return "Managed by SHAFT automatically.";
        }
        return isUserRunnableDiagnosticCommand(command) ? command : "Managed by SHAFT automatically.";
    }

    private boolean isUserRunnableDiagnosticCommand(String command) {
        return command != null && !command.isBlank() && !command.trim().equals(mcpCommand());
    }

    private String clientDisplayName() {
        return switch (normalize(String.valueOf(family.getSelectedItem()), "CODEX")) {
            case "CLAUDE" -> "Claude";
            case "COPILOT" -> "GitHub Copilot";
            case GEMINI_FAMILY -> "Gemini";
            default -> "Codex";
        };
    }

    private String runtimeDisplayName() {
        return String.valueOf(runtime.getSelectedItem()).replace('_', ' ');
    }

    private static String normalize(String value, String fallback) {
        String normalized = value == null || value.isBlank() ? fallback : value.trim();
        return normalized.toUpperCase(Locale.ROOT).replace('-', '_').replace(' ', '_');
    }

    private static JLabel setupStatusLabel(String accessibleName) {
        JLabel label = new JLabel();
        label.getAccessibleContext().setAccessibleName(accessibleName);
        return label;
    }

    private static JLabel setupStepLabel(String accessibleName) {
        JLabel label = new JLabel();
        // Don't clamp preferred size; let layout manager compute it from font metrics
        label.setBorder(JBUI.Borders.empty(2, 6));
        label.getAccessibleContext().setAccessibleName(accessibleName);
        return label;
    }

    private static JLabel setupStateLabel(String accessibleName) {
        JLabel label = new JLabel();
        label.setOpaque(true);
        label.setHorizontalAlignment(JLabel.CENTER);
        label.setPreferredSize(JBUI.size(74, 22));
        label.setBorder(JBUI.Borders.compound(
                JBUI.Borders.customLine(UIManagerColors.border(), 1),
                JBUI.Borders.empty(2, 6)));
        label.getAccessibleContext().setAccessibleName(accessibleName);
        return label;
    }

    private static void applyLabeledAction(JButton button, Icon icon) {
        button.setIcon(icon);
        button.setMargin(JBUI.insets(4, 8));
        button.setFocusPainted(true);
        button.setRolloverEnabled(true);
    }

    private void setStatusText(String text) {
        status.setToolTipText(text);
        if (GUIDE_SETUP_STEP.equals(text)) {
            status.setText(GUIDE_SETUP_STEP);
            status.setVisible(false);
            return;
        }
        status.setText(escapeHtml(text));
        status.setVisible(true);
    }

    /**
     * Every step badge below reflects a real check, never a click (issue #3426 A4/A5): the
     * upgrade step compares the project pom against the latest release, the agent step requires
     * the selected runtime to actually be detected, the install step requires an installed
     * shaft-mcp (or an explicit command) to exist on disk, and the check step requires the probe
     * to have actually passed — with an explicit Failed badge when it did not.
     */
    private String upgradeStepState() {
        if (upgradeCheckResult == null) {
            return "optional";
        }
        return switch (upgradeCheckResult.state()) {
            case UP_TO_DATE -> "done";
            case UPGRADE_AVAILABLE, NOT_A_SHAFT_PROJECT -> "next";
            case LATEST_UNKNOWN -> "optional";
        };
    }

    private String chooseStepState() {
        return selectedAgentDetected ? "done" : "next";
    }

    private String installStepState(boolean hasCommand) {
        return installedCommandDetected || hasCommand ? "done" : "next";
    }

    private String checkStepState(boolean running, boolean complete, boolean hasCommand) {
        if (running) {
            return "checking";
        }
        if (complete) {
            return "done";
        }
        if (lastCheckFailed) {
            return "failed";
        }
        return installedCommandDetected || hasCommand ? "next" : "wait";
    }

    private void updateSetupSteps(boolean running) {
        boolean hasCommand = !currentCommand().isBlank();
        boolean complete = settings.mcpSetupComplete && hasCommand;
        setStep(upgradeStep, upgradeState, "1 Upgrade project", upgradeStepState());
        setStep(chooseStep, chooseState, "2 Pick agent", chooseStepState());
        setStep(installStep, installState, "3 Install SHAFT MCP", installStepState(hasCommand));
        setStep(testStep, testState, "4 Check setup", checkStepState(running, complete, hasCommand));
        setStep(null, readyState, "Ready", complete ? "next" : "wait");
    }

    private void updateWorkflowRows(boolean running) {
        boolean hasCommand = !currentCommand().isBlank();
        boolean complete = settings.mcpSetupComplete && hasCommand;
        styleStepRow(upgradeRow, upgradeStepState());
        styleStepRow(chooseRow, chooseStepState());
        styleStepRow(installRow, installStepState(hasCommand));
        styleStepRow(checkRow, checkStepState(running, complete, hasCommand));
        styleStepRow(chatRow, complete ? "next" : "wait");
    }

    private static void setStep(JLabel label, JLabel stateLabel, String name, String state) {
        if (label != null) {
            label.setText(name);
            label.setToolTipText(name + " is " + state);
            label.getAccessibleContext().setAccessibleName(name + " setup step: " + state);
            label.setFont(label.getFont().deriveFont("next".equals(state) || "checking".equals(state)
                    || "optional".equals(state)
                    ? Font.BOLD
                    : Font.PLAIN));
            label.setForeground(switch (state) {
                case "done" -> ShaftStatusPresentation.success();
                case "failed" -> ShaftStatusPresentation.error();
                case "next", "checking", "optional" -> ShaftStatusPresentation.progress();
                default -> UIManagerColors.foreground();
            });
        }
        stateLabel.setText(switch (state) {
            case "done" -> "Done";
            case "failed" -> "Failed";
            case "next" -> "Next";
            case "checking" -> "Checking";
            case "optional" -> "Optional";
            default -> "Waiting";
        });
        stateLabel.setToolTipText(name + " is " + state);
        stateLabel.getAccessibleContext().setAccessibleDescription(name + " setup state: " + state);
        stateLabel.setBackground(switch (state) {
            case "done" -> UIManagerColors.doneBackground();
            case "next", "checking", "optional" -> UIManagerColors.activeBackground();
            default -> UIManagerColors.panelBackground();
        });
        stateLabel.setForeground(switch (state) {
            case "done" -> ShaftStatusPresentation.success();
            case "failed" -> ShaftStatusPresentation.error();
            case "next", "checking", "optional" -> ShaftStatusPresentation.progress();
            default -> UIManagerColors.foreground();
        });
    }

    private static JPanel stepRow(JLabel label, JLabel stateLabel, JComponent action) {
        JPanel row = new JPanel(new WrapLayout(FlowLayout.LEFT, 8, 4));
        row.setOpaque(true);
        row.add(label);
        row.add(stateLabel);
        row.add(action);
        row.setBorder(JBUI.Borders.compound(
                JBUI.Borders.customLine(UIManagerColors.border(), 1),
                JBUI.Borders.empty(8, 10)));
        return row;
    }

    private void updateLiveSummary() {
        String target = String.valueOf(installerTarget.getSelectedItem()).replace('_', ' ');
        setupSummary.setText("Installer source: main. Target: " + target + ". Runtime: " + assistantRuntimeLabel() + ".");
        recommendedAgent.setText(recommendedAgentText());
    }

    private static JPanel labeledControl(String text, JComponent control) {
        JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        row.setOpaque(false);
        JLabel label = new JLabel(text);
        // Don't clamp label size; use JBUI insets for consistent spacing
        label.setBorder(JBUI.Borders.empty(2, 0, 0, 6));
        label.setLabelFor(control);
        row.add(label);
        row.add(control);
        return row;
    }

    private static void styleStepRow(JPanel row, String state) {
        row.setBackground(switch (state) {
            case "next", "checking", "optional" -> UIManagerColors.activeBackground();
            case "done" -> UIManagerColors.doneBackground();
            default -> UIManagerColors.panelBackground();
        });
        row.setBorder(JBUI.Borders.compound(
                JBUI.Borders.customLine(switch (state) {
                    case "next", "checking", "optional" -> ShaftStatusPresentation.progress();
                    case "done" -> ShaftStatusPresentation.success();
                    case "failed" -> ShaftStatusPresentation.error();
                    default -> UIManagerColors.border();
                }, 1),
                JBUI.Borders.empty(8, 10)));
    }

    private static String escapeHtml(String text) {
        if (text == null || text.isBlank()) {
            return "";
        }
        return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
    }

    private void showRuntimeSelected() {
        showStatus(runtimeStatus, "Runtime", assistantRuntimeLabel() + " selected", ShaftStatusPresentation.pending());
        runtimeStatus.setVisible(false);
    }

    private void showRuntimeVerified() {
        showStatus(runtimeStatus, "Runtime", assistantRuntimeLabel() + " verified", ShaftStatusPresentation.success());
        runtimeStatus.setVisible(true);
    }

    private String assistantRuntimeLabel() {
        String selectedFamily = normalize(String.valueOf(family.getSelectedItem()), "CODEX");
        String selectedRuntime = normalize(String.valueOf(runtime.getSelectedItem()), "CLI");
        return switch (selectedFamily) {
            case "CLAUDE" -> "DESKTOP_APP".equals(selectedRuntime) ? "Claude Desktop" : "Claude Code CLI";
            case "COPILOT" -> "IDE_PLUGIN".equals(selectedRuntime)
                    ? "GitHub Copilot in IntelliJ"
                    : "GitHub Copilot CLI";
            case GEMINI_FAMILY -> "Gemini cloud API";
            default -> "Codex " + ShaftUiLabels.friendly(selectedRuntime);
        };
    }

    private void showAssistNotConfigured() {
        showAssistStatus("Not configured", ShaftStatusPresentation.pending());
        assistStatus.setVisible(false);
    }

    private void showAssistConnecting() {
        showAssistStatus("Connecting", ShaftStatusPresentation.progress());
        assistStatus.setVisible(true);
    }

    private void showAssistConfigured() {
        showAssistStatus("Configured", ShaftStatusPresentation.success());
        assistStatus.setVisible(true);
    }

    private void showAssistError() {
        showAssistStatus("Error", ShaftStatusPresentation.error());
        assistStatus.setVisible(true);
    }

    private void showAssistStatus(String state, Color color) {
        showStatus(assistStatus, "Assist", state, color);
    }

    private static void showStatus(JLabel label, String prefix, String state, Color color) {
        label.setText(prefix + ": " + state);
        label.setForeground(color);
    }

    private void setDiagnosticText(String text, String command) {
        diagnosticOutput = text == null ? "" : text;
        diagnosticCommand = isUserRunnableDiagnosticCommand(command) ? command : "";
        copyRestartCommand.setVisible(false);
        copyRestartCommand.setEnabled(false);
        copyCommand.setVisible(!diagnosticCommand.isBlank());
        copyCommand.setEnabled(!diagnosticCommand.isBlank());
        copyOutput.setEnabled(!diagnosticOutput.isBlank());
        copyDocs.setVisible(!diagnosticOutput.isBlank());
        copyDocs.setEnabled(!diagnosticOutput.isBlank());
        recoveryStatus.setText("Recovery: retry Check setup, copy diagnostics, or open the SHAFT MCP docs link.");
        recoveryStatus.setVisible(!diagnosticOutput.isBlank());
        details.setText(diagnosticOutput);
        details.setCaretPosition(details.getDocument().getLength());
        detailsPanel.setVisible(!diagnosticOutput.isBlank());
        updateProgressivePanels();
        detailsPanel.revalidate();
    }

    private void updateProgressivePanels() {
        installerDetailsPanel.revalidate();
    }

    private void clearDiagnostics() {
        diagnosticOutput = "";
        diagnosticCommand = "";
        copyCommand.setEnabled(false);
        copyCommand.setVisible(false);
        copyOutput.setEnabled(false);
        copyDocs.setEnabled(false);
        copyDocs.setVisible(false);
        copyRestartCommand.setEnabled(false);
        copyRestartCommand.setVisible(false);
        recoveryStatus.setVisible(false);
        details.setText("");
        detailsPanel.setVisible(false);
        detailsPanel.revalidate();
    }

    private void copyDiagnosticCommand() {
        copy(diagnosticCommand, "Copied diagnostic command");
    }

    private void copyDiagnosticOutput() {
        copy(diagnosticOutput, "Copied diagnostic output");
    }

    private void copyDocsLink() {
        copy(MCP_DOCS_URL, "Copied SHAFT MCP docs link");
    }

    private void copy(String value, String message) {
        if (value != null && !value.isBlank()) {
            copySink.accept(value);
            setStatusText(message);
            toastSink.accept(toastMessage(message));
        }
    }

    private static String toastMessage(String message) {
        String lower = message == null ? "" : message.toLowerCase(Locale.ROOT);
        return lower.contains("command") ? "Command copied to clipboard" : "Copied to clipboard";
    }

    private void showToast(String message) {
        if (toastTimer != null) {
            toastTimer.stop();
        }
        toast.setText(message);
        toast.setVisible(true);
        if (!isShowing()) {
            toast.setVisible(false);
            return;
        }
        toastTimer = new Timer(TOAST_MILLIS, event -> {
            toast.setVisible(false);
            toastTimer = null;
        });
        toastTimer.setRepeats(false);
        toastTimer.start();
    }

    private String successSummary(String output) {
        String workspace = verifiedWorkspace(output);
        return workspace.isBlank()
                ? "Ready to chat. Verified " + assistantRuntimeLabel() + "."
                : "Ready to chat. Verified " + assistantRuntimeLabel() + " for workspace " + workspace + ".";
    }

    private static String verifiedWorkspace(String output) {
        if (output == null || output.isBlank()) {
            return "";
        }
        for (String line : output.split("\\R")) {
            String trimmed = line.trim();
            String lower = trimmed.toLowerCase(Locale.ROOT);
            if (lower.startsWith("mcp workspace:")) {
                return trimmed.substring(trimmed.indexOf(':') + 1).trim();
            }
            if (lower.startsWith("workspace:")) {
                return trimmed.substring(trimmed.indexOf(':') + 1).trim();
            }
        }
        return "";
    }

    private static void copyToClipboard(String value) {
        CopyPasteManager.getInstance().setContents(new StringSelection(value));
    }

    private void appendConsoleSuccess(String line) {
        appendLine("SUCCESS: " + line, true);
    }

    private void appendLine(String line, boolean success) {
        String value = line == null ? "" : line;
        if (!value.isEmpty()) {
            StyledDocument document = details.getStyledDocument();
            try {
                int length = document.getLength();
                if (length > 0) {
                    document.insertString(length, "\n", new SimpleAttributeSet());
                }
                document.insertString(document.getLength(), value, consoleStyle(success));
            } catch (BadLocationException ignored) {
                // noop: console append is best-effort.
            }
            diagnosticOutput = details.getText();
            copyOutput.setEnabled(!diagnosticOutput.isBlank());
            details.setCaretPosition(document.getLength());
            detailsPanel.setVisible(!diagnosticOutput.isBlank());
            detailsPanel.revalidate();
        }
    }

    private SimpleAttributeSet consoleStyle(boolean success) {
        SimpleAttributeSet style = new SimpleAttributeSet();
        StyleConstants.setFontFamily(style, details.getFont() == null ? Font.MONOSPACED : details.getFont().getFamily());
        StyleConstants.setFontSize(style, details.getFont() == null ? 12 : details.getFont().getSize());
        StyleConstants.setForeground(style, success ? ShaftStatusPresentation.success() : UIManagerColors.foreground());
        if (success) {
            StyleConstants.setBold(style, true);
        }
        return style;
    }

    private static final class UIManagerColors {
        private static Color background() {
            Color background = javax.swing.UIManager.getColor("TextArea.background");
            return background == null ? Color.WHITE : background;
        }

        private static Color panelBackground() {
            Color background = javax.swing.UIManager.getColor("Panel.background");
            return background == null ? background() : background;
        }

        private static Color activeBackground() {
            return mix(panelBackground(), ShaftStatusPresentation.progress(), 0.08D);
        }

        private static Color doneBackground() {
            return mix(panelBackground(), ShaftStatusPresentation.success(), 0.06D);
        }

        private static Color border() {
            Color border = javax.swing.UIManager.getColor("Component.borderColor");
            return border == null ? Color.LIGHT_GRAY : border;
        }

        private static Color foreground() {
            Color foreground = javax.swing.UIManager.getColor("TextArea.foreground");
            return foreground == null ? Color.BLACK : foreground;
        }

        private static Color mix(Color base, Color overlay, double overlayWeight) {
            double bounded = Math.max(0.0D, Math.min(1.0D, overlayWeight));
            double baseWeight = 1.0D - bounded;
            return new Color(
                    channel(base.getRed(), overlay.getRed(), baseWeight, bounded),
                    channel(base.getGreen(), overlay.getGreen(), baseWeight, bounded),
                    channel(base.getBlue(), overlay.getBlue(), baseWeight, bounded));
        }

        private static int channel(int base, int overlay, double baseWeight, double overlayWeight) {
            return Math.max(0, Math.min(255, (int) Math.round(base * baseWeight + overlay * overlayWeight)));
        }
    }
}
