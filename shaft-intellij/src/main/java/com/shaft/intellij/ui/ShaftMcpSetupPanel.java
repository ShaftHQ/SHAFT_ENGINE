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
            "Next: choose agent, copy command, open terminal, then check.";
    private static final String CHECK_NEXT_STEP = "Press Check now.";

    @FunctionalInterface
    interface AgentReadinessProbe {
        ShaftMcpToolResult test(String client, String runtime);
    }

    private final Project project;
    private final ShaftSettingsState.Settings settings;
    private final Runnable connected;
    private final AgentReadinessProbe readinessProbe;
    private final String recommendedFamily;
    private final JBTextArea installerCommand;
    private final JBTextArea mcpCommand;
    private final JComboBox<String> family;
    private final JComboBox<String> runtime;
    private final JComboBox<String> installerTarget;
    private final JCheckBox manualInstallerTarget;
    private final JButton copyInstallerCommand;
    private final JButton openTerminal;
    private final JButton test;
    private final JButton startChatting;
    private final JButton resetAndReinstall;
    private final JCheckBox expertMode;
    private final JButton resetEverything;
    private final JProgressBar progress;
    private final JLabel runtimeStatus;
    private final JLabel assistStatus;
    private final JLabel chooseStep;
    private final JLabel chooseState;
    private final JLabel installStep;
    private final JLabel installState;
    private final JLabel detectStep;
    private final JLabel detectState;
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
    private final JTextPane details;
    private final JPanel installerDetailsPanel;
    private final JPanel detailsPanel;
    private final JPanel chooseRow;
    private final JPanel copyRow;
    private final JPanel terminalRow;
    private final JPanel checkRow;
    private final JPanel chatRow;
    private String diagnosticCommand = "";
    private String diagnosticOutput = "";
    private boolean installerCommandCopied;
    private boolean terminalOpened;
    private Consumer<String> copySink = ShaftMcpSetupPanel::copyToClipboard;
    private Consumer<String> toastSink = this::showToast;
    private Timer toastTimer;
    private java.util.function.BooleanSupplier confirmReset = this::confirmResetDialog;
    private Runnable resetAction = () -> ShaftPluginResetService.getInstance().resetEverything();

    ShaftMcpSetupPanel(@NotNull Project project, @NotNull ShaftSettingsState.Settings settings,
                       @NotNull Runnable connected) {
        this(project, settings, connected, AssistantLocalAgentRunner::readiness);
    }

    ShaftMcpSetupPanel(@NotNull Project project, @NotNull ShaftSettingsState.Settings settings,
                       @NotNull Runnable connected, @NotNull AgentReadinessProbe readinessProbe) {
        super(new BorderLayout(8, 8));
        this.project = project;
        this.settings = settings;
        this.connected = connected;
        this.readinessProbe = readinessProbe;
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
        family = new JComboBox<>(new String[]{"CODEX", "CLAUDE", "COPILOT"});
        ShaftUiLabels.applyFriendlyRenderer(family);
        family.setSelectedItem(resolveFamily(settings));
        family.getAccessibleContext().setAccessibleName("Assistant family");
        runtime = new JComboBox<>(new String[]{"CLI", "IDE_PLUGIN", "DESKTOP_APP"});
        ShaftUiLabels.applyFriendlyRenderer(runtime);
        runtime.setSelectedItem(normalize(settings.assistantRuntime, "CLI"));
        runtime.getAccessibleContext().setAccessibleName("Assistant runtime");
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
        copyInstallerCommand = new JButton("Copy command");
        copyInstallerCommand.getAccessibleContext().setAccessibleName("Copy MCP installer command");
        copyInstallerCommand.setToolTipText("Copy the terminal installer command");
        copyInstallerCommand.setMnemonic(KeyEvent.VK_C);
        applyLabeledAction(copyInstallerCommand, ShaftIcons.COPY);
        copyInstallerCommand.addActionListener(event -> copyInstallerCommand());
        openTerminal = new JButton("Open terminal");
        openTerminal.getAccessibleContext().setAccessibleName("Open terminal for MCP installer");
        openTerminal.setToolTipText("Open the IntelliJ terminal after copying the installer command");
        openTerminal.setMnemonic(KeyEvent.VK_T);
        applyLabeledAction(openTerminal, ShaftIcons.CODE);
        openTerminal.addActionListener(event -> openTerminalForInstaller());
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
        details = new JTextPane();
        details.setPreferredSize(JBUI.size(560, 180));
        details.getAccessibleContext().setAccessibleName("SHAFT MCP setup output");
        details.setEditable(false);
        details.setFont(new Font(Font.MONOSPACED, Font.PLAIN, details.getFont() == null ? 12 : details.getFont().getSize()));
        details.setBackground(UIManagerColors.background());
        details.setForeground(UIManagerColors.foreground());
        details.setBorder(JBUI.Borders.compound(JBUI.Borders.empty(6), JBUI.Borders.customLine(UIManagerColors.border(), 1)));

        chooseStep = setupStepLabel("Choose agent setup step");
        chooseState = setupStateLabel("Choose agent setup state");
        installStep = setupStepLabel("Copy command setup step");
        installState = setupStateLabel("Copy command setup state");
        detectStep = setupStepLabel("Open terminal setup step");
        detectState = setupStateLabel("Open terminal setup state");
        testStep = setupStepLabel("Check now setup step");
        testState = setupStateLabel("Check now setup state");
        readyState = setupStateLabel("Start chatting setup state");
        JPanel agentControls = new JPanel();
        agentControls.setLayout(new javax.swing.BoxLayout(agentControls, javax.swing.BoxLayout.Y_AXIS));
        agentControls.setOpaque(false);
        agentControls.add(labeledControl("Assistant family", family));
        agentControls.add(labeledControl("Runtime", runtime));
        agentControls.add(recommendedAgent);
        JPanel checkActions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        checkActions.setOpaque(false);
        checkActions.add(test);
        checkActions.add(progress);
        checkActions.add(assistStatus);
        chooseRow = stepRow(chooseStep, chooseState, agentControls);
        copyRow = stepRow(installStep, installState, copyInstallerCommand);
        terminalRow = stepRow(detectStep, detectState, openTerminal);
        checkRow = stepRow(testStep, testState, checkActions);
        JLabel readyStep = setupStepLabel("Start chatting setup step");
        readyStep.setText("Ready");
        chatRow = stepRow(readyStep, readyState, startChatting);
        chatRow.setVisible(false);
        JPanel workflow = new JPanel();
        workflow.setLayout(new javax.swing.BoxLayout(workflow, javax.swing.BoxLayout.Y_AXIS));
        workflow.add(chooseRow);
        workflow.add(copyRow);
        workflow.add(terminalRow);
        workflow.add(checkRow);
        workflow.add(chatRow);
        JPanel targetRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        targetRow.add(manualInstallerTarget);
        targetRow.add(installerTarget);
        JPanel diagnosticRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        diagnosticRow.add(copyCommand);
        diagnosticRow.add(copyOutput);
        diagnosticRow.add(copyDocs);
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
        JPanel intro = new JPanel(new BorderLayout(4, 2));
        JLabel title = new JLabel("Connect SHAFT Assistant");
        title.setFont(title.getFont().deriveFont(Font.BOLD));
        JLabel summary = new JLabel("Pick an agent. SHAFT handles the wiring.");
        intro.add(title, BorderLayout.NORTH);
        intro.add(summary, BorderLayout.CENTER);
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
        add(form, BorderLayout.NORTH);
        add(detailsPanel, BorderLayout.CENTER);
        if (!currentCommand().isBlank()) {
            setStatusText(CHECK_NEXT_STEP);
        }
        updateActionState(false);
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
        settings.assistantProviderType = "LOCAL";
        settings.assistantFamily = String.valueOf(family.getSelectedItem());
        settings.assistantRuntime = String.valueOf(runtime.getSelectedItem());
        settings.defaultAutobotClient = clientFromFamily(settings.assistantFamily);
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
        ShaftMcpConnectionProbe.test(command, settings, projectRoot()).whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> showTestResult(result, error)));
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
        boolean success = error == null && result != null && result.success();
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
                ShaftMcpToolResult readiness = readinessProbe.test(settings.defaultAutobotClient, settings.assistantRuntime);
                if (readiness.success()) {
                    showAssistConfigured();
                    showRuntimeVerified();
                    setStatusText(successSummary(result.output()));
                } else {
                    success = false;
                    showAssistError();
                    setStatusText("Agent not ready. Retry test.");
                    setDiagnosticText(troubleshootingDetails("Client readiness failed",
                            "MCP probe output:\n" + result.output() + "\n\nAgent readiness failed: " + readiness.output(),
                            readinessDiagnosticCommand(), true), readinessDiagnosticCommand());
                }
            } else {
                showAssistError();
                setDiagnosticText(troubleshootingDetails("Probe failed", result.output(), mcpCommand(), true),
                        mcpCommand());
            }
        }
        if (success) {
            settings.mcpSetupComplete = true;
            settings.agentGuidanceOptimizationPromptPending = hasAgentGuidanceScaffold();
            startChatting.setVisible(true);
            startChatting.requestFocusInWindow();
        } else {
            settings.mcpSetupComplete = false;
            settings.agentGuidanceOptimizationPromptPending = false;
            startChatting.setVisible(false);
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
        setStatusText(text);
    }

    private void updateActionState(boolean running) {
        boolean hasCommand = !currentCommand().isBlank();
        boolean complete = settings.mcpSetupComplete && hasCommand;
        boolean showCopy = !complete && !hasCommand && !installerCommandCopied;
        boolean showTerminal = !complete && !hasCommand && installerCommandCopied && !terminalOpened;
        boolean showTest = !complete && (hasCommand || terminalOpened);
        copyInstallerCommand.setVisible(showCopy);
        copyInstallerCommand.setEnabled(!running && showCopy);
        openTerminal.setVisible(showTerminal);
        openTerminal.setEnabled(!running && showTerminal);
        test.setVisible(showTest);
        test.setEnabled(!running && showTest);
        startChatting.setVisible(complete || startChatting.isVisible());
        startChatting.setEnabled(!running && startChatting.isVisible());
        chatRow.setVisible(startChatting.isVisible());
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
        settings.mcpSetupComplete = false;
        settings.agentGuidanceOptimizationPromptPending = false;
        if (!currentCommand().isBlank()) {
            installerCommandCopied = true;
            terminalOpened = true;
        }
        setStatusText(currentCommand().isBlank()
                ? GUIDE_SETUP_STEP
                : CHECK_NEXT_STEP);
        updateActionState(false);
    }

    private void assistantSelectionChanged() {
        showRuntimeSelected();
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
        installerCommandCopied = false;
        terminalOpened = false;
        showAssistNotConfigured();
        startChatting.setVisible(false);
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

    private void copyInstallerCommand() {
        copy(installerCommand(), "Copied MCP installer command");
        installerCommandCopied = true;
        terminalOpened = false;
        updateActionState(false);
        openTerminal.requestFocusInWindow();
    }

    private void openTerminalForInstaller() {
        copy(installerCommand(), "Copied installer command");
        installerCommandCopied = true;
        terminalOpened = true;
        openIntellijTerminal();
        setStatusText("Run command in terminal, then check.");
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
        installerCommandCopied = true;
        terminalOpened = false;
        startChatting.setVisible(false);
        showRuntimeSelected();
        showAssistNotConfigured();
        copy(installerCommand(), "Installer command copied. Run it in terminal, then check.");
        updateActionState(false);
        openTerminal.requestFocusInWindow();
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
                "This clears SHAFT settings, saved provider API keys, tool approvals, and Assistant chat "
                        + "history.\n\nYour project source code is never touched.",
                "Reset SHAFT Plugin",
                "Reset everything",
                "Cancel",
                Messages.getWarningIcon()) == Messages.YES;
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

    private String readinessDiagnosticCommand() {
        return switch (normalize(String.valueOf(family.getSelectedItem()), "CODEX")) {
            case "CLAUDE" -> "claude --version";
            case "COPILOT" -> "copilot --version";
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

    private void updateSetupSteps(boolean running) {
        boolean hasCommand = !currentCommand().isBlank();
        boolean complete = settings.mcpSetupComplete && hasCommand;
        setStep(chooseStep, chooseState, "1 Pick agent", "done");
        setStep(installStep, installState, "2 Copy command", hasCommand || installerCommandCopied || complete ? "done" : "next");
        setStep(detectStep, detectState, "3 Run in terminal", hasCommand || terminalOpened || complete
                ? "done"
                : installerCommandCopied ? "next" : "wait");
        setStep(testStep, testState, "4 Check setup", running ? "checking" : complete ? "done" : hasCommand || terminalOpened ? "next" : "wait");
        setStep(null, readyState, "Ready", complete ? "next" : "wait");
    }

    private void updateWorkflowRows(boolean running) {
        boolean hasCommand = !currentCommand().isBlank();
        boolean complete = settings.mcpSetupComplete && hasCommand;
        styleStepRow(chooseRow, "done");
        styleStepRow(copyRow, hasCommand || installerCommandCopied || complete ? "done" : "next");
        styleStepRow(terminalRow, hasCommand || terminalOpened || complete
                ? "done"
                : installerCommandCopied ? "next" : "wait");
        styleStepRow(checkRow, running ? "checking" : complete ? "done" : hasCommand || terminalOpened ? "next" : "wait");
        styleStepRow(chatRow, complete ? "next" : "wait");
    }

    private static void setStep(JLabel label, JLabel stateLabel, String name, String state) {
        if (label != null) {
            label.setText(name);
            label.setToolTipText(name + " is " + state);
            label.getAccessibleContext().setAccessibleName(name + " setup step: " + state);
            label.setFont(label.getFont().deriveFont("next".equals(state) || "checking".equals(state)
                    ? Font.BOLD
                    : Font.PLAIN));
            label.setForeground(switch (state) {
                case "done" -> ShaftStatusPresentation.success();
                case "next", "checking" -> ShaftStatusPresentation.progress();
                default -> UIManagerColors.foreground();
            });
        }
        stateLabel.setText(switch (state) {
            case "done" -> "Done";
            case "next" -> "Next";
            case "checking" -> "Checking";
            default -> "Waiting";
        });
        stateLabel.setToolTipText(name + " is " + state);
        stateLabel.getAccessibleContext().setAccessibleDescription(name + " setup state: " + state);
        stateLabel.setBackground(switch (state) {
            case "done" -> UIManagerColors.doneBackground();
            case "next", "checking" -> UIManagerColors.activeBackground();
            default -> UIManagerColors.panelBackground();
        });
        stateLabel.setForeground(switch (state) {
            case "done" -> ShaftStatusPresentation.success();
            case "next", "checking" -> ShaftStatusPresentation.progress();
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
                JBUI.Borders.empty(6)));
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
            case "next", "checking" -> UIManagerColors.activeBackground();
            case "done" -> UIManagerColors.doneBackground();
            default -> UIManagerColors.panelBackground();
        });
        row.setBorder(JBUI.Borders.compound(
                JBUI.Borders.customLine(switch (state) {
                    case "next", "checking" -> ShaftStatusPresentation.progress();
                    case "done" -> ShaftStatusPresentation.success();
                    default -> UIManagerColors.border();
                }, 1),
                JBUI.Borders.empty(6)));
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
