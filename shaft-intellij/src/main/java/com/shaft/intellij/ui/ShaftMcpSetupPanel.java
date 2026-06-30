package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
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
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * First-run SHAFT MCP setup panel.
 */
final class ShaftMcpSetupPanel extends JPanel {
    private final Project project;
    private final ShaftSettingsState.Settings settings;
    private final Runnable connected;
    private final JButton install;
    private final JComboBox<String> family;
    private final JComboBox<String> runtime;
    private final JButton test;
    private final JLabel status;
    private final JBTextArea details;

    ShaftMcpSetupPanel(@NotNull Project project, @NotNull ShaftSettingsState.Settings settings,
                       @NotNull Runnable connected) {
        super(new BorderLayout(8, 8));
        this.project = project;
        this.settings = settings;
        this.connected = connected;
        setBorder(JBUI.Borders.empty(12));

        install = new JButton("Install / Update SHAFT MCP");
        install.getAccessibleContext().setAccessibleName("Install or update SHAFT MCP");
        install.addActionListener(event -> installMcp());
        family = new JComboBox<>(new String[]{"CODEX", "CLAUDE", "COPILOT"});
        ShaftUiLabels.applyFriendlyRenderer(family);
        family.setSelectedItem(resolveFamily(settings));
        family.getAccessibleContext().setAccessibleName("Assistant family");
        runtime = new JComboBox<>(new String[]{"CLI", "IDE_PLUGIN", "DESKTOP_APP"});
        ShaftUiLabels.applyFriendlyRenderer(runtime);
        runtime.setSelectedItem(normalize(settings.assistantRuntime, "CLI"));
        runtime.getAccessibleContext().setAccessibleName("Assistant runtime");
        test = new JButton("Test connection");
        test.getAccessibleContext().setAccessibleName("Test SHAFT MCP connection");
        test.setEnabled(settings.mcpCommand != null && !settings.mcpCommand.isBlank());
        test.addActionListener(event -> testConnection());
        status = new JLabel("Select assistant runtime, then install SHAFT MCP.");
        details = new JBTextArea(8, 48);
        details.getAccessibleContext().setAccessibleName("SHAFT MCP setup output");
        details.setEditable(false);
        details.setLineWrap(true);
        details.setWrapStyleWord(true);

        JPanel installRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        installRow.add(install);
        JPanel testRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        testRow.add(test);
        testRow.add(status);

        JPanel form = FormBuilder.createFormBuilder()
                .addLabeledComponent("1. Assistant family", family)
                .addLabeledComponent("2. Assistant runtime", runtime)
                .addComponent(new JLabel("3. Install or update SHAFT MCP"))
                .addComponent(installRow)
                .addComponent(new JLabel("4. Test connection"))
                .addComponent(testRow)
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
        add(form, BorderLayout.NORTH);
        add(new JBScrollPane(details), BorderLayout.CENTER);
    }

    JComponent preferredFocusComponent() {
        return family;
    }

    private void installMcp() {
        setRunning(true, "Installing...");
        details.setText("");
        ShaftMcpInstaller.installForPluginAndClient(installerClientForSelection()).whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> showInstallResult(result, error)));
    }

    private void showInstallResult(ShaftMcpInstallResult result, Throwable error) {
        setRunning(false, error == null && result != null && result.success() ? "Installed" : "Install failed");
        if (error != null) {
            details.setText(error.getMessage());
            return;
        }
        if (result == null) {
            details.setText("No installer result returned.");
            return;
        }
        details.setText(formatInstallOutput(result.output()));
        if (result.success()) {
            settings.mcpCommand = result.commandLine();
            settings.mcpSetupComplete = false;
            test.setEnabled(true);
        }
    }

    private void testConnection() {
        String command = settings.mcpCommand == null ? "" : settings.mcpCommand.trim();
        if (command.isBlank()) {
            status.setText("Install first");
            return;
        }
        settings.assistantProviderType = "LOCAL";
        settings.assistantFamily = String.valueOf(family.getSelectedItem());
        settings.assistantRuntime = String.valueOf(runtime.getSelectedItem());
        settings.defaultAutobotClient = clientFromFamily(settings.assistantFamily);
        setRunning(true, "Testing...");
        ShaftMcpConnectionProbe.test(command, settings, projectRoot()).whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> showTestResult(result, error)));
    }

    private Path projectRoot() {
        return project.getBasePath() == null ? Path.of(".") : Path.of(project.getBasePath());
    }

    private void showTestResult(ShaftMcpToolResult result, Throwable error) {
        setRunning(false, error == null && result != null && result.success() ? "Connected" : "Test failed");
        if (error != null) {
            details.setText(error.getMessage());
        } else if (result == null) {
            details.setText("No test result returned.");
        } else {
            details.setText(result.output());
        }
        if (error == null && result != null && result.success()) {
            settings.mcpSetupComplete = true;
            connected.run();
        }
    }

    private void setRunning(boolean running, String text) {
        install.setEnabled(!running);
        family.setEnabled(!running);
        runtime.setEnabled(!running);
        test.setEnabled(!running && settings.mcpCommand != null && !settings.mcpCommand.isBlank());
        status.setText(text);
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
}
