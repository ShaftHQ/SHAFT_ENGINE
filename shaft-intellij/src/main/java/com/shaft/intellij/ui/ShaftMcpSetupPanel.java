package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.util.ui.FormBuilder;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpConnectionProbe;
import com.shaft.intellij.mcp.ShaftMcpInstallResult;
import com.shaft.intellij.mcp.ShaftMcpInstaller;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTextPane;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Color;
import java.awt.Font;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import java.awt.datatransfer.StringSelection;
import java.util.function.Consumer;

/**
 * First-run SHAFT MCP setup panel.
 */
final class ShaftMcpSetupPanel extends JPanel {
    @FunctionalInterface
    interface AgentReadinessProbe {
        ShaftMcpToolResult test(String client, String runtime);
    }

    private final Project project;
    private final ShaftSettingsState.Settings settings;
    private final Runnable connected;
    private final AgentReadinessProbe readinessProbe;
    private final JButton install;
    private final JComboBox<String> family;
    private final JComboBox<String> runtime;
    private final JButton test;
    private final JProgressBar installProgress;
    private final JLabel projectStatus;
    private final JLabel installStatus;
    private final JLabel runtimeStatus;
    private final JLabel assistStatus;
    private final JLabel status;
    private final JButton copyCommand;
    private final JButton copyOutput;
    private final JTextPane details;
    private boolean installing;
    private String installedSelectionKey = "";
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

        install = new JButton("Install / Update SHAFT MCP");
        install.getAccessibleContext().setAccessibleName("Install or update SHAFT MCP");
        ShaftIconButtons.apply(install, ShaftIcons.DOWNLOAD);
        install.addActionListener(event -> installMcp());
        installProgress = new JProgressBar();
        installProgress.setIndeterminate(true);
        installProgress.setVisible(false);
        installProgress.setPreferredSize(JBUI.size(96, 14));
        family = new JComboBox<>(new String[]{"CODEX", "CLAUDE", "COPILOT"});
        ShaftUiLabels.applyFriendlyRenderer(family);
        family.setSelectedItem(resolveFamily(settings));
        family.getAccessibleContext().setAccessibleName("Assistant family");
        runtime = new JComboBox<>(new String[]{"CLI", "IDE_PLUGIN", "DESKTOP_APP"});
        ShaftUiLabels.applyFriendlyRenderer(runtime);
        runtime.setSelectedItem(normalize(settings.assistantRuntime, "CLI"));
        runtime.getAccessibleContext().setAccessibleName("Assistant runtime");
        test = new JButton("Test connection and start chatting");
        test.getAccessibleContext().setAccessibleName("Test SHAFT MCP connection");
        ShaftIconButtons.apply(test, ShaftIcons.SEND);
        test.addActionListener(event -> testConnection());
        projectStatus = setupStatusLabel("Project setup status");
        installStatus = new JLabel();
        installStatus.setPreferredSize(JBUI.size(180, 22));
        installStatus.getAccessibleContext().setAccessibleName("SHAFT MCP install status");
        runtimeStatus = setupStatusLabel("Assistant runtime setup status");
        assistStatus = setupStatusLabel("Assistant connection setup status");
        status = new JLabel("Install SHAFT MCP first.");
        status.setPreferredSize(JBUI.size(320, 22));
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
        installRow.add(installProgress);
        installRow.add(install);
        installRow.add(installStatus);
        JPanel testRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        testRow.add(test);
        testRow.add(assistStatus);
        JPanel diagnosticRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        diagnosticRow.add(copyCommand);
        diagnosticRow.add(copyOutput);
        family.addActionListener(event -> assistantSelectionChanged());
        runtime.addActionListener(event -> assistantSelectionChanged());
        showProjectConfigured();
        showMcpNotConfigured();
        showRuntimeConfigured();
        showAssistNotConfigured();
        if (settings.mcpCommand != null && !settings.mcpCommand.isBlank()) {
            markInstalledForCurrentSelection();
            showAssistStatus(settings.mcpSetupComplete ? "Configured" : "Not configured",
                    settings.mcpSetupComplete ? UIManagerColors.success() : UIManagerColors.pending());
            status.setText("Press Test connection next.");
        }
        updateActionState(false);

        JPanel form = FormBuilder.createFormBuilder()
                .addComponent(section("Project"))
                .addComponent(projectStatus)
                .addComponent(section("MCP"))
                .addComponent(installRow)
                .addComponent(section("Runtime"))
                .addLabeledComponent("Assistant family", family)
                .addLabeledComponent("Assistant runtime", runtime)
                .addComponent(runtimeStatus)
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
        return family;
    }

    private void installMcp() {
        installing = true;
        showMcpConnecting();
        showAssistNotConfigured();
        setRunning(true, "Installing...");
        setDiagnosticText("SHAFT MCP installation\n\n- Starting installer...", installerDiagnosticCommand());
        ShaftMcpInstaller.installForPluginAndClient(installerClientForSelection(), createInstallerOutputHandler())
                .whenComplete((result, error) ->
                        ApplicationManager.getApplication().invokeLater(() -> showInstallResult(result, error)));
    }

    void showInstallResult(ShaftMcpInstallResult result, Throwable error) {
        installing = false;
        boolean success = error == null && result != null && result.success();
        setRunning(false, success ? "Press Test connection next." : "Install failed. Retry install.");
        if (error != null) {
            showMcpError();
            setDiagnosticText(troubleshootingDetails("Install failed", error.getMessage(), installerDiagnosticCommand(), false),
                    installerDiagnosticCommand());
            return;
        }
        if (result == null) {
            showMcpError();
            setDiagnosticText(troubleshootingDetails("Install failed", "No installer result returned.",
                    installerDiagnosticCommand(), false), installerDiagnosticCommand());
            return;
        }
        if (result.success()) {
            setDiagnosticText(formatInstallOutput(result.output()), result.commandLine());
            appendConsoleSuccess("Installation completed successfully. Next: press \"Test connection and start chatting\".");
            settings.mcpCommand = result.commandLine();
            settings.mcpSetupComplete = false;
            settings.agentGuidanceOptimizationPromptPending = false;
            markInstalledForCurrentSelection();
            showAssistNotConfigured();
            updateActionState(false);
        } else {
            showMcpError();
            setDiagnosticText(troubleshootingDetails("Install failed", result.output(), installerDiagnosticCommand(), false),
                    installerDiagnosticCommand());
        }
    }

    private Consumer<String> createInstallerOutputHandler() {
        return line -> ApplicationManager.getApplication().invokeLater(() -> appendInstallerOutput(line));
    }

    private void appendInstallerOutput(String line) {
        if (!installing || line == null) {
            return;
        }
        String text = line.trim();
        if (text.isBlank() || isNoisyInstallLine(text) || parseJsonObject(text) != null) {
            return;
        }
        appendInstallerLine(text);
    }

    private void testConnection() {
        String command = settings.mcpCommand == null ? "" : settings.mcpCommand.trim();
        if (command.isBlank()) {
            showAssistError();
            status.setText("Install first");
            setDiagnosticText(troubleshootingDetails("Probe failed",
                    "No SHAFT MCP command is configured yet.", installerDiagnosticCommand(), true),
                    installerDiagnosticCommand());
            return;
        }
        settings.assistantProviderType = "LOCAL";
        settings.assistantFamily = String.valueOf(family.getSelectedItem());
        settings.assistantRuntime = String.valueOf(runtime.getSelectedItem());
        settings.defaultAutobotClient = clientFromFamily(settings.assistantFamily);
        showAssistConnecting();
        setRunning(true, "Testing...");
        ShaftMcpConnectionProbe.test(command, settings, projectRoot()).whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> showTestResult(result, error)));
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
                setDiagnosticText(result.output(), mcpCommand());
                ShaftMcpToolResult readiness = readinessProbe.test(settings.defaultAutobotClient, settings.assistantRuntime);
                if (readiness.success()) {
                    showAssistConfigured();
                    appendConsoleSuccess("Connected to SHAFT MCP.");
                    appendConsoleSuccess(readiness.output());
                } else {
                    success = false;
                    showAssistError();
                    status.setText("Agent not ready. Retry test.");
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
            connected.run();
        } else {
            settings.mcpSetupComplete = false;
            settings.agentGuidanceOptimizationPromptPending = false;
        }
    }

    private void setRunning(boolean running, String text) {
        updateActionState(running);
        installProgress.setVisible(running);
        family.setEnabled(!running);
        runtime.setEnabled(!running);
        status.setText(text);
    }

    private void updateActionState(boolean running) {
        boolean installedForSelection = isInstalledForCurrentSelection();
        install.setEnabled(!running && !installedForSelection);
        test.setEnabled(!running
                && installedForSelection
                && settings.mcpCommand != null
                && !settings.mcpCommand.isBlank());
    }

    private void assistantSelectionChanged() {
        if (installing) {
            return;
        }
        showRuntimeConfigured();
        if (isInstalledForCurrentSelection()) {
            showInstalledStatus();
            showAssistNotConfigured();
            status.setText("Press Test connection next.");
        } else {
            showMcpNotConfigured();
            showAssistNotConfigured();
            settings.mcpSetupComplete = false;
            settings.agentGuidanceOptimizationPromptPending = false;
            status.setText("Install SHAFT MCP for the selected assistant.");
        }
        updateActionState(false);
    }

    private void markInstalledForCurrentSelection() {
        installedSelectionKey = currentSelectionKey();
        showInstalledStatus();
    }

    private void showInstalledStatus() {
        showMcpConfigured();
    }

    private boolean isInstalledForCurrentSelection() {
        return !installedSelectionKey.isBlank() && installedSelectionKey.equals(currentSelectionKey());
    }

    private String currentSelectionKey() {
        return normalize(String.valueOf(family.getSelectedItem()), "CODEX")
                + "|"
                + normalize(String.valueOf(runtime.getSelectedItem()), "CLI");
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

    private String installerClientForSelection() {
        String selectedFamily = String.valueOf(family.getSelectedItem());
        String selectedRuntime = String.valueOf(runtime.getSelectedItem());
        return switch (normalize(selectedFamily, "CODEX")) {
            case "CLAUDE" -> "DESKTOP_APP".equals(normalize(selectedRuntime, "CLI")) ? "claude-desktop" : "claude";
            case "COPILOT" -> "IDE_PLUGIN".equals(normalize(selectedRuntime, "CLI")) ? "copilot-intellij" : "copilot";
            default -> "codex";
        };
    }

    private String installerDiagnosticCommand() {
        return ShaftMcpInstaller.diagnosticInstallCommand(installerClientForSelection());
    }

    private String mcpCommand() {
        return settings.mcpCommand == null ? "" : settings.mcpCommand.trim();
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
        String category = failureCategory(detailsOutput, probeFailure);
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

    private String failureCategory(String output, boolean probeFailure) {
        String text = output == null ? "" : output.toLowerCase(Locale.ROOT);
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
        return probeFailure ? "MCP probe" : "MCP installer";
    }

    private List<String> troubleshootingSteps(String category, boolean probeFailure) {
        List<String> steps = new ArrayList<>();
        switch (category) {
            case "Java/runtime" -> steps.add("Install or select a Java runtime that can run shaft-mcp, then retry.");
            case "Maven artifact resolution" -> steps.add(
                    "Check Maven Central/network access and retry once artifact resolution is available.");
            case "Client configuration" -> steps.add(
                    "Check that the selected client can write and read its MCP configuration file.");
            case "Client runtime" -> steps.add(
                    "Install the selected client CLI or add it to PATH, then retry.");
            default -> steps.add(probeFailure
                    ? "Run the diagnostic command in a terminal to confirm the MCP stdio command starts outside IntelliJ."
                    : "Retry the installer after fixing the output shown below.");
        }
        steps.add(clientSpecificStep());
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

    static String formatInstallOutput(String output) {
        String text = output == null ? "" : output.trim();
        if (text.isBlank()) {
            return "No installer output returned.";
        }
        JsonObject summary = null;
        List<String> log = new ArrayList<>();
        for (String rawLine : text.split("\\R")) {
            String line = rawLine.trim();
            if (line.isBlank() || isNoisyInstallLine(line)) {
                continue;
            }
            JsonObject parsed = parseJsonObject(line);
            if (parsed != null) {
                summary = parsed;
                continue;
            }
            log.add(line);
        }
        StringBuilder formatted = new StringBuilder("SHAFT MCP installation\n\n");
        if (summary != null) {
            formatted.append("Summary\n")
                    .append("- Client: ").append(text(summary, "client")).append('\n')
                    .append("- Version: ").append(text(summary, "version")).append('\n')
                    .append("- Command: ").append(text(summary, "command"));
            JsonElement args = summary.get("args");
            if (args != null && args.isJsonArray() && !args.getAsJsonArray().isEmpty()) {
                formatted.append(' ').append(args.getAsJsonArray().get(0).getAsString());
            }
            formatted.append("\n\n");
        }
        if (!log.isEmpty()) {
            formatted.append("Installation log\n");
            for (String line : log) {
                formatted.append("- ").append(line).append('\n');
            }
        }
        return formatted.toString().trim();
    }

    private static boolean isNoisyInstallLine(String line) {
        if ("SHAFT MCP installer".equals(line) || "MCP installer".equals(line)) {
            return true;
        }
        if (line.matches("^[=#_\\-\\s]+$") || line.matches("^\\d+(?:\\.\\d+)?%$")) {
            return true;
        }
        String compact = line.replace(" ", "");
        return compact.length() >= 4
                && (compact.contains("____") || compact.contains("/__") || compact.contains("\\__")
                || compact.matches("^[#/\\\\_|]+$"));
    }

    private static JsonObject parseJsonObject(String line) {
        if (!line.startsWith("{") || !line.endsWith("}")) {
            return null;
        }
        try {
            JsonElement parsed = JsonParser.parseString(line);
            return parsed.isJsonObject() ? parsed.getAsJsonObject() : null;
        } catch (RuntimeException exception) {
            return null;
        }
    }

    private static String text(JsonObject object, String key) {
        JsonElement value = object.get(key);
        return value == null || value.isJsonNull() ? "" : value.getAsString();
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

    private void showProjectConfigured() {
        showStatus(projectStatus, "Project", "Configured", UIManagerColors.success());
    }

    private void showMcpNotConfigured() {
        showStatus(installStatus, "MCP", "Not configured", UIManagerColors.pending());
    }

    private void showMcpConnecting() {
        showStatus(installStatus, "MCP", "Connecting", UIManagerColors.progress());
    }

    private void showMcpConfigured() {
        showStatus(installStatus, "MCP", "Configured", UIManagerColors.success());
    }

    private void showMcpError() {
        showStatus(installStatus, "MCP", "Error", UIManagerColors.error());
    }

    private void showRuntimeConfigured() {
        showStatus(runtimeStatus, "Runtime", "Configured", UIManagerColors.success());
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
        copyCommand.setEnabled(!diagnosticCommand.isBlank());
        copyOutput.setEnabled(!diagnosticOutput.isBlank());
        details.setText(diagnosticOutput);
        details.setCaretPosition(details.getDocument().getLength());
    }

    private void copyDiagnosticCommand() {
        copy(diagnosticCommand, "Copied diagnostic command");
    }

    private void copyDiagnosticOutput() {
        copy(diagnosticOutput, "Copied diagnostic output");
    }

    private void copy(String value, String message) {
        if (value != null && !value.isBlank()) {
            copySink.accept(value);
            status.setText(message);
        }
    }

    private static void copyToClipboard(String value) {
        CopyPasteManager.getInstance().setContents(new StringSelection(value));
    }

    private void appendInstallerLine(String line) {
        appendLine("- " + line, false);
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
