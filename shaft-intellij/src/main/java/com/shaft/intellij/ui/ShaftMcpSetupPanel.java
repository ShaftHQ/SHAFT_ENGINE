package com.shaft.intellij.ui;

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

/**
 * First-run SHAFT MCP setup panel.
 */
final class ShaftMcpSetupPanel extends JPanel {
    private final ShaftSettingsState.Settings settings;
    private final Runnable connected;
    private final JButton install;
    private final JComboBox<String> provider;
    private final JButton test;
    private final JLabel status;
    private final JBTextArea details;

    ShaftMcpSetupPanel(Project project, @NotNull ShaftSettingsState.Settings settings, @NotNull Runnable connected) {
        super(new BorderLayout(8, 8));
        this.settings = settings;
        this.connected = connected;
        setBorder(JBUI.Borders.empty(12));

        install = new JButton("Install / Update SHAFT MCP");
        install.getAccessibleContext().setAccessibleName("Install or update SHAFT MCP");
        install.addActionListener(event -> installMcp());
        provider = new JComboBox<>(new String[]{"CODEX", "CLAUDE_CODE", "COPILOT_CLI"});
        provider.setSelectedItem(settings.defaultAutobotClient);
        provider.getAccessibleContext().setAccessibleName("Assistant provider");
        test = new JButton("Test connection");
        test.getAccessibleContext().setAccessibleName("Test SHAFT MCP connection");
        test.setEnabled(settings.mcpCommand != null && !settings.mcpCommand.isBlank());
        test.addActionListener(event -> testConnection());
        status = new JLabel("Install SHAFT MCP to start.");
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
                .addComponent(new JLabel("1. Install or update SHAFT MCP"))
                .addComponent(installRow)
                .addLabeledComponent("2. Assistant provider", provider)
                .addComponent(new JLabel("3. Test connection"))
                .addComponent(testRow)
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
        add(form, BorderLayout.NORTH);
        add(new JBScrollPane(details), BorderLayout.CENTER);
    }

    JComponent preferredFocusComponent() {
        return install;
    }

    private void installMcp() {
        setRunning(true, "Installing...");
        details.setText("");
        ShaftMcpInstaller.installForPlugin().whenComplete((result, error) ->
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
        details.setText(result.output());
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
        settings.defaultAutobotClient = String.valueOf(provider.getSelectedItem());
        setRunning(true, "Testing...");
        ShaftMcpConnectionProbe.test(command, settings).whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> showTestResult(result, error)));
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
        provider.setEnabled(!running);
        test.setEnabled(!running && settings.mcpCommand != null && !settings.mcpCommand.isBlank());
        status.setText(text);
    }
}
