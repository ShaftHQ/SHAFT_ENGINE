package com.shaft.intellij.ui;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.FormBuilder;
import com.intellij.util.ui.JBUI;
import com.intellij.util.ui.WrapLayout;
import com.shaft.intellij.mcp.ShaftMcpConnectionProbe;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
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
import java.io.IOException;
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
    private static final String INSTALLER_REF = "a95e891cbd3d79cdaaaf4b0d608fd56d09b8c69b";
    private static final String[] INSTALLER_TARGETS = {
            "CODEX",
            "CLAUDE_CODE",
            "CLAUDE_DESKTOP",
            "COPILOT_CLI",
            "COPILOT_INTELLIJ",
            "INTELLIJ_PLUGIN"
    };
    private static final String GUIDE_SETUP_STEP =
            "Copy the installer command, run it in a terminal, detect the stdio command, then test.";

    @FunctionalInterface
    interface AgentReadinessProbe {
        ShaftMcpToolResult test(String client, String runtime);
    }

    private final Project project;
    private final ShaftSettingsState.Settings settings;
    private final Runnable connected;
    private final AgentReadinessProbe readinessProbe;
    private final JBTextArea installerCommand;
    private final JBTextArea mcpCommand;
    private final JComboBox<String> family;
    private final JComboBox<String> runtime;
    private final JComboBox<String> installerTarget;
    private final JCheckBox manualInstallerTarget;
    private final JButton copyInstallerCommand;
    private final JButton inferCommand;
    private final JButton test;
    private final JButton startChatting;
    private final JProgressBar progress;
    private final JLabel projectStatus;
    private final JLabel commandStatus;
    private final JLabel runtimeStatus;
    private final JLabel assistStatus;
    private final JLabel status;
    private final JButton copyCommand;
    private final JButton copyOutput;
    private final JTextPane details;
    private String diagnosticCommand = "";
    private String diagnosticOutput = "";
    private Consumer<String> copySink = ShaftMcpSetupPanel::copyToClipboard;

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
        setBorder(JBUI.Borders.empty(12));

        installerCommand = commandArea(3, "MCP installer command");
        installerCommand.setEditable(false);
        installerCommand.getAccessibleContext().setAccessibleDescription(
                "Terminal command that installs SHAFT MCP for the selected assistant client.");
        mcpCommand = commandArea(2, "MCP stdio command");
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
        manualInstallerTarget.setToolTipText("Show manual MCP install target");
        manualInstallerTarget.addActionListener(event -> {
            installerTarget.setVisible(manualInstallerTarget.isSelected());
            assistantSelectionChanged();
        });
        installerCommand.setText(installerCommand());
        copyInstallerCommand = new JButton("Copy MCP installer command");
        copyInstallerCommand.getAccessibleContext().setAccessibleName("Copy MCP installer command");
        ShaftIconButtons.apply(copyInstallerCommand, ShaftIcons.COPY);
        copyInstallerCommand.addActionListener(event -> copyInstallerCommand());
        inferCommand = new JButton("Use inferred MCP command");
        inferCommand.getAccessibleContext().setAccessibleName("Use inferred MCP command");
        ShaftIconButtons.apply(inferCommand, ShaftIcons.SEARCH);
        inferCommand.addActionListener(event -> useInferredMcpCommand());
        test = new JButton("Test connection and start chatting");
        test.getAccessibleContext().setAccessibleName("Test SHAFT MCP connection");
        ShaftIconButtons.apply(test, ShaftIcons.CHECK);
        test.addActionListener(event -> testConnection());
        startChatting = new JButton("Start chatting with SHAFT Assistant");
        startChatting.getAccessibleContext().setAccessibleName("Start chatting with SHAFT Assistant");
        ShaftIconButtons.apply(startChatting, ShaftIcons.SEND);
        startChatting.setVisible(false);
        startChatting.addActionListener(event -> connected.run());
        projectStatus = setupStatusLabel("Project setup status");
        commandStatus = setupStatusLabel("SHAFT MCP command status");
        runtimeStatus = setupStatusLabel("Assistant runtime setup status");
        assistStatus = setupStatusLabel("Assistant connection setup status");
        status = new JLabel();
        status.setPreferredSize(JBUI.size(360, 44));
        setStatusText(GUIDE_SETUP_STEP);
        status.getAccessibleContext().setAccessibleName("SHAFT MCP setup next step");
        copyCommand = new JButton("Copy command");
        copyCommand.getAccessibleContext().setAccessibleName("Copy setup diagnostic command");
        ShaftIconButtons.apply(copyCommand, ShaftIcons.CODE);
        copyCommand.setEnabled(false);
        copyCommand.addActionListener(event -> copyDiagnosticCommand());
        copyOutput = new JButton("Copy output");
        copyOutput.getAccessibleContext().setAccessibleName("Copy setup diagnostic output");
        ShaftIconButtons.apply(copyOutput, ShaftIcons.COPY);
        copyOutput.setEnabled(false);
        copyOutput.addActionListener(event -> copyDiagnosticOutput());
        details = new JTextPane();
        details.setPreferredSize(JBUI.size(560, 180));
        details.getAccessibleContext().setAccessibleName("SHAFT MCP setup output");
        details.setEditable(false);
        details.setFont(new Font(Font.MONOSPACED, Font.PLAIN, details.getFont() == null ? 12 : details.getFont().getSize()));
        details.setBackground(UIManagerColors.background());
        details.setForeground(UIManagerColors.foreground());
        details.setBorder(JBUI.Borders.compound(JBUI.Borders.empty(6), JBUI.Borders.customLine(UIManagerColors.border(), 1)));

        JPanel installRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        installRow.add(copyInstallerCommand);
        installRow.add(inferCommand);
        JPanel testRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        testRow.add(progress);
        testRow.add(test);
        testRow.add(startChatting);
        testRow.add(assistStatus);
        JPanel pathRow = new JPanel(new WrapLayout(FlowLayout.LEFT, 8, 0));
        pathRow.add(new JLabel("1 Choose assistant"));
        pathRow.add(new JLabel("2 Install MCP"));
        pathRow.add(new JLabel("3 Detect command"));
        pathRow.add(new JLabel("4 Test connection"));
        JPanel targetRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        targetRow.add(manualInstallerTarget);
        targetRow.add(installerTarget);
        JPanel diagnosticRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        diagnosticRow.add(copyCommand);
        diagnosticRow.add(copyOutput);
        family.addActionListener(event -> assistantSelectionChanged());
        runtime.addActionListener(event -> assistantSelectionChanged());
        installerTarget.addActionListener(event -> installerTargetChanged());
        showProjectConfigured();
        updateCommandStatus();
        showRuntimeSelected();
        showAssistNotConfigured();
        if (!currentCommand().isBlank()) {
            setStatusText("Press Test connection next.");
        }
        updateActionState(false);

        JPanel form = FormBuilder.createFormBuilder()
                .addComponent(pathRow)
                .addComponent(section("Project"))
                .addComponent(projectStatus)
                .addComponent(section("Runtime"))
                .addLabeledComponent("Assistant family", family)
                .addLabeledComponent("Assistant runtime", runtime)
                .addComponent(runtimeStatus)
                .addComponent(section("Install"))
                .addComponent(targetRow)
                .addLabeledComponent("Installer command", new JBScrollPane(installerCommand))
                .addComponent(installRow)
                .addComponent(guidanceArea("Run the installer command in a terminal,\nthen detect the stdio command."))
                .addComponent(section("MCP"))
                .addLabeledComponent("MCP stdio command", new JBScrollPane(mcpCommand))
                .addComponent(commandStatus)
                .addComponent(guidanceArea(
                        "Copy the installer command,\nrun it in a terminal,\ndetect the stdio command,\nthen test."))
                .addComponent(section("Assist"))
                .addComponent(new JLabel("Test connection"))
                .addComponent(testRow)
                .addComponent(status)
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
        JPanel detailsPanel = new JPanel(new BorderLayout(4, 4));
        detailsPanel.add(diagnosticRow, BorderLayout.NORTH);
        detailsPanel.add(new JBScrollPane(details), BorderLayout.CENTER);
        add(form, BorderLayout.NORTH);
        add(detailsPanel, BorderLayout.CENTER);
    }

    JComponent preferredFocusComponent() {
        return mcpCommand;
    }

    private void testConnection() {
        String command = currentCommand();
        settings.mcpCommand = command;
        settings.assistantProviderType = "LOCAL";
        settings.assistantFamily = String.valueOf(family.getSelectedItem());
        settings.assistantRuntime = String.valueOf(runtime.getSelectedItem());
        settings.defaultAutobotClient = clientFromFamily(settings.assistantFamily);
        if (command.isBlank()) {
            showMcpNotConfigured();
            showAssistError();
            settings.mcpSetupComplete = false;
            settings.agentGuidanceOptimizationPromptPending = false;
            setStatusText("Paste command first.");
            setDiagnosticText(troubleshootingDetails("Probe failed",
                    "No SHAFT MCP command configured.", "", true), "");
            updateActionState(false);
            return;
        }
        showMcpConfigured();
        showAssistConnecting();
        setRunning(true, "Testing...");
        ShaftMcpConnectionProbe.test(command, settings, projectRoot()).whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> showTestResult(result, error)));
    }

    private static JBTextArea commandArea(int rows, String accessibleName) {
        JBTextArea area = new JBTextArea(rows, 48);
        area.setLineWrap(true);
        area.setWrapStyleWord(true);
        area.getAccessibleContext().setAccessibleName(accessibleName);
        area.setFont(new Font(Font.MONOSPACED, Font.PLAIN, area.getFont() == null ? 12 : area.getFont().getSize()));
        return area;
    }

    private Path projectRoot() {
        return project.getBasePath() == null ? Path.of(".") : Path.of(project.getBasePath());
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
                showMcpConfigured();
                setDiagnosticText(result.output(), mcpCommand());
                ShaftMcpToolResult readiness = readinessProbe.test(settings.defaultAutobotClient, settings.assistantRuntime);
                if (readiness.success()) {
                    showAssistConfigured();
                    showRuntimeVerified();
                    appendConsoleSuccess("Connected to SHAFT MCP.");
                    appendConsoleSuccess(readiness.output());
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
            settings.agentGuidanceOptimizationPromptPending = true;
            startChatting.setVisible(true);
        } else {
            settings.mcpSetupComplete = false;
            settings.agentGuidanceOptimizationPromptPending = false;
            startChatting.setVisible(false);
            showRuntimeSelected();
        }
    }

    private void setRunning(boolean running, String text) {
        updateActionState(running);
        progress.setVisible(running);
        installerCommand.setEnabled(!running);
        copyInstallerCommand.setEnabled(!running);
        inferCommand.setEnabled(!running);
        installerTarget.setEnabled(!running);
        manualInstallerTarget.setEnabled(!running);
        mcpCommand.setEnabled(!running);
        family.setEnabled(!running);
        runtime.setEnabled(!running);
        setStatusText(text);
    }

    private void updateActionState(boolean running) {
        test.setEnabled(!running);
        copyCommand.setEnabled(!running && (!diagnosticCommand.isBlank() || !currentCommand().isBlank()));
    }

    private void commandChanged() {
        updateCommandStatus();
        showAssistNotConfigured();
        startChatting.setVisible(false);
        settings.mcpSetupComplete = false;
        settings.agentGuidanceOptimizationPromptPending = false;
        setStatusText(currentCommand().isBlank()
                ? GUIDE_SETUP_STEP
                : "Press Test connection next.");
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
        installerCommand.setText(installerCommand());
        updateCommandStatus();
        showAssistNotConfigured();
        startChatting.setVisible(false);
        settings.mcpSetupComplete = false;
        settings.agentGuidanceOptimizationPromptPending = false;
        setStatusText(currentCommand().isBlank()
                ? GUIDE_SETUP_STEP
                : "Press Test connection next.");
        updateActionState(false);
    }

    private void updateCommandStatus() {
        if (currentCommand().isBlank()) {
            showMcpNotConfigured();
        } else {
            showMcpConfigured();
        }
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
        String url = "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/" + INSTALLER_REF
                + "/scripts/mcp/install-shaft-mcp";
        if (isWindows()) {
            return "powershell -NoProfile -ExecutionPolicy Bypass -Command '$env:SHAFT_MCP_INSTALLER_REF=\""
                    + INSTALLER_REF + "\"; $installer=Join-Path $env:TEMP \"install-shaft-mcp.ps1\"; "
                    + "Invoke-WebRequest -UseBasicParsing \"" + url
                    + ".ps1\" -OutFile $installer; & $installer -Client " + target + "'";
        }
        return "ref=" + INSTALLER_REF + "; tmp=\"${TMPDIR:-/tmp}/install-shaft-mcp.sh\"; curl -fL " + url
                + ".sh -o \"$tmp\" && SHAFT_MCP_INSTALLER_REF=\"$ref\" sh \"$tmp\" --" + target;
    }

    private void copyInstallerCommand() {
        copy(installerCommand(), "Copied MCP installer command");
    }

    private void useInferredMcpCommand() {
        String command = inferInstalledStdioCommand();
        if (command.isBlank()) {
            setStatusText("Run installer command first.");
            setDiagnosticText(troubleshootingDetails("Setup command not found",
                    "No installed shaft-mcp argfile was found under " + applicationDataRoot() + ".",
                    installerCommand(), false), installerCommand());
            return;
        }
        mcpCommand.setText(command);
        setStatusText("Press Test connection next.");
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
        String commandText = command == null || command.isBlank() ? "No command available." : command;
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
            case "MCP command" -> steps.add("Run the installer command, use the inferred command, or paste a manual stdio command.");
            case "Java/runtime" -> steps.add("Install or select a Java runtime that can run shaft-mcp, then retry.");
            case "Maven artifact resolution" -> steps.add(
                    "Check Maven Central/network access and retry once artifact resolution is available.");
            case "Client configuration" -> steps.add(
                    "Check that the selected client can write and read its MCP configuration file.");
            case "Client runtime" -> steps.add(
                    "Install the selected client CLI or add it to PATH, then retry.");
            default -> steps.add("Run the diagnostic command in a terminal to confirm the MCP stdio command starts outside IntelliJ.");
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
        label.setPreferredSize(JBUI.size(180, 22));
        label.getAccessibleContext().setAccessibleName(accessibleName);
        return label;
    }

    private static JLabel section(String text) {
        JLabel label = new JLabel(text);
        label.getAccessibleContext().setAccessibleName(text + " setup section");
        return label;
    }

    private void setStatusText(String text) {
        status.setToolTipText(text);
        if (GUIDE_SETUP_STEP.equals(text)) {
            status.setText("<html>Copy installer, detect command,<br>then test.</html>");
            return;
        }
        status.setText(escapeHtml(text));
    }

    private static JBTextArea guidanceArea(String text) {
        JBTextArea area = new JBTextArea(text, Math.max(2, text.split("\\R", -1).length), 32);
        area.setLineWrap(true);
        area.setWrapStyleWord(true);
        area.setEditable(false);
        area.setOpaque(false);
        area.setFocusable(false);
        area.setBorder(JBUI.Borders.empty());
        area.setForeground(UIManagerColors.foreground());
        area.getAccessibleContext().setAccessibleName("SHAFT MCP setup guidance");
        area.getAccessibleContext().setAccessibleDescription(text);
        return area;
    }

    private static String escapeHtml(String text) {
        if (text == null || text.isBlank()) {
            return "";
        }
        return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
    }

    private void showProjectConfigured() {
        showStatus(projectStatus, "Project", "Configured", UIManagerColors.success());
    }

    private void showMcpNotConfigured() {
        showStatus(commandStatus, "MCP", "Not configured", UIManagerColors.pending());
    }

    private void showMcpConfigured() {
        showStatus(commandStatus, "MCP", "Configured", UIManagerColors.success());
    }

    private void showRuntimeSelected() {
        showStatus(runtimeStatus, "Runtime", assistantRuntimeLabel() + " selected", UIManagerColors.pending());
    }

    private void showRuntimeVerified() {
        showStatus(runtimeStatus, "Runtime", assistantRuntimeLabel() + " verified", UIManagerColors.success());
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
        showAssistStatus("Not configured", UIManagerColors.pending());
    }

    private void showAssistConnecting() {
        showAssistStatus("Connecting", UIManagerColors.progress());
    }

    private void showAssistConfigured() {
        showAssistStatus("Configured", UIManagerColors.success());
    }

    private void showAssistError() {
        showAssistStatus("Error", UIManagerColors.error());
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
        diagnosticCommand = command == null ? "" : command;
        copyCommand.setEnabled(!diagnosticCommand.isBlank() || !currentCommand().isBlank());
        copyOutput.setEnabled(!diagnosticOutput.isBlank());
        details.setText(diagnosticOutput);
        details.setCaretPosition(details.getDocument().getLength());
    }

    private void copyDiagnosticCommand() {
        String value = diagnosticCommand.isBlank() ? currentCommand() : diagnosticCommand;
        copy(value, "Copied diagnostic command");
    }

    private void copyDiagnosticOutput() {
        copy(diagnosticOutput, "Copied diagnostic output");
    }

    private void copy(String value, String message) {
        if (value != null && !value.isBlank()) {
            copySink.accept(value);
            setStatusText(message);
        }
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
        }
    }

    private SimpleAttributeSet consoleStyle(boolean success) {
        SimpleAttributeSet style = new SimpleAttributeSet();
        StyleConstants.setFontFamily(style, details.getFont() == null ? Font.MONOSPACED : details.getFont().getFamily());
        StyleConstants.setFontSize(style, details.getFont() == null ? 12 : details.getFont().getSize());
        StyleConstants.setForeground(style, success ? UIManagerColors.success() : UIManagerColors.foreground());
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

        private static Color border() {
            Color border = javax.swing.UIManager.getColor("Component.borderColor");
            return border == null ? Color.LIGHT_GRAY : border;
        }

        private static Color foreground() {
            Color foreground = javax.swing.UIManager.getColor("TextArea.foreground");
            return foreground == null ? Color.BLACK : foreground;
        }

        private static Color success() {
            return new Color(0x0A7F26);
        }

        private static Color pending() {
            Color foreground = javax.swing.UIManager.getColor("Label.disabledForeground");
            return foreground == null ? Color.GRAY : foreground;
        }

        private static Color progress() {
            Color foreground = javax.swing.UIManager.getColor("Component.focusColor");
            return foreground == null ? new Color(0x0550AE) : foreground;
        }

        private static Color error() {
            Color foreground = javax.swing.UIManager.getColor("ValidationTooltip.errorForeground");
            return foreground == null ? new Color(0xB42318) : foreground;
        }
    }
}
