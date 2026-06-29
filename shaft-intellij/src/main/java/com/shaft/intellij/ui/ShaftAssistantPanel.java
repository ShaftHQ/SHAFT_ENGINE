package com.shaft.intellij.ui;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftCredentialService;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JProgressBar;
import javax.swing.KeyStroke;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.datatransfer.StringSelection;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.Locale;
import java.util.concurrent.CancellationException;

/**
 * SHAFT Assistant chat-style panel.
 */
final class ShaftAssistantPanel extends JPanel {
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
    private final JBTextArea transcript;
    private final JButton send;
    private final JButton cancel;
    private final JButton copyLastResponse;
    private final JButton copyTranscript;
    private final JButton clearTranscript;
    private final JButton rerunLastPrompt;
    private final JButton testConnection;
    private final JProgressBar progress;
    private final JLabel status;
    private final ShaftSettingsState.Settings settings;
    private String lastResponse = "";
    private String lastPrompt = "";
    private ShaftMcpInvocation currentInvocation;

    ShaftAssistantPanel(Project project) {
        this(project, ShaftSettingsState.getInstance().getState());
    }

    ShaftAssistantPanel(Project project, @NotNull ShaftSettingsState.Settings settings) {
        super(new BorderLayout(6, 6));
        this.settings = settings;
        setBorder(JBUI.Borders.empty(8));

        mode = combo("Assistant mode", "ASK", "PLAN", "AGENT");
        mode.setSelectedItem(normalize(settings.defaultAutobotMode, "ASK"));
        providerType = combo("Assistant provider type", "LOCAL", "CLOUD");
        providerType.setSelectedItem(normalize(settings.assistantProviderType, "LOCAL"));
        assistantFamily = combo("Assistant family", "CODEX", "CLAUDE", "COPILOT");
        assistantFamily.setSelectedItem(resolveFamily(settings));
        assistantRuntime = combo("Assistant runtime", "CLI", "IDE_PLUGIN", "DESKTOP_APP");
        assistantRuntime.setSelectedItem(normalize(settings.assistantRuntime, "CLI"));
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
        saveCloudApiKey.addActionListener(event -> saveCloudApiKey());
        cloudKeyStatus = new JLabel();
        cloudKeyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        cloudKeyPanel.add(cloudKeyStatus);
        cloudKeyPanel.add(cloudApiKey);
        cloudKeyPanel.add(saveCloudApiKey);

        allowSourceMutation = new JBCheckBox("Allow edits");
        allowSourceMutation.getAccessibleContext().setAccessibleName("Approve source mutation for Agent mode");
        prompt = new JBTextArea(5, 32);
        prompt.getAccessibleContext().setAccessibleName("Assistant prompt");
        prompt.setLineWrap(true);
        prompt.setWrapStyleWord(true);
        transcript = new JBTextArea(18, 32);
        transcript.getAccessibleContext().setAccessibleName("Assistant transcript");
        transcript.setEditable(false);
        transcript.setLineWrap(true);
        transcript.setWrapStyleWord(true);
        transcript.setText("Type a question or use /help for SHAFT commands.");
        status = new JLabel("Ready");
        progress = new JProgressBar();
        progress.setIndeterminate(true);
        progress.setVisible(false);

        send = button("Send", "Send assistant prompt", event -> send(project));
        cancel = button("Cancel", "Cancel assistant request", event -> cancelCurrent());
        cancel.setEnabled(false);
        copyLastResponse = button("Copy response", "Copy last assistant response", event -> copyLastResponse());
        copyLastResponse.setEnabled(false);
        copyTranscript = button("Copy all", "Copy assistant transcript", event -> copy(transcript.getText(), "Copied transcript"));
        clearTranscript = button("Clear", "Clear assistant transcript", event -> clearTranscript());
        rerunLastPrompt = button("Rerun", "Rerun last assistant prompt", event -> rerun(project));
        rerunLastPrompt.setEnabled(false);
        testConnection = button("Test MCP", "Test SHAFT MCP connection", event -> testConnection(project));

        mode.addActionListener(event -> updateControlVisibility());
        providerType.addActionListener(event -> updateControlVisibility());
        assistantRuntime.addActionListener(event -> updateControlVisibility());
        cloudProvider.addActionListener(event -> updateControlVisibility());
        updateControlVisibility();
        bindKeyboard(project);

        JPanel transcriptPanel = new JPanel(new BorderLayout(4, 4));
        transcriptPanel.add(new JBScrollPane(transcript), BorderLayout.CENTER);

        JPanel actionRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        actionRow.add(testConnection);
        actionRow.add(copyLastResponse);
        actionRow.add(copyTranscript);
        actionRow.add(clearTranscript);
        actionRow.add(rerunLastPrompt);
        actionRow.add(cancel);
        actionRow.add(status);

        JPanel routeRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        routeRow.add(mode);
        routeRow.add(providerType);
        routeRow.add(assistantFamily);
        routeRow.add(assistantRuntime);
        routeRow.add(customCommand);
        routeRow.add(cloudProvider);
        routeRow.add(cloudModel);
        routeRow.add(allowSourceMutation);

        JPanel promptActions = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 0));
        promptActions.add(progress);
        promptActions.add(send);

        JPanel composerFooter = new JPanel(new BorderLayout(4, 4));
        composerFooter.add(routeRow, BorderLayout.CENTER);
        composerFooter.add(promptActions, BorderLayout.EAST);

        JPanel composer = new JPanel(new BorderLayout(4, 4));
        composer.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createEtchedBorder(),
                JBUI.Borders.empty(6)));
        composer.add(new JLabel("Add context (#), extensions (@), commands (/)"), BorderLayout.NORTH);
        composer.add(new JBScrollPane(prompt), BorderLayout.CENTER);
        composer.add(cloudKeyPanel, BorderLayout.WEST);
        composer.add(composerFooter, BorderLayout.SOUTH);

        JPanel south = new JPanel(new BorderLayout(4, 4));
        south.add(actionRow, BorderLayout.NORTH);
        south.add(composer, BorderLayout.CENTER);

        add(setupNotice(project, settings), BorderLayout.NORTH);
        add(transcriptPanel, BorderLayout.CENTER);
        add(south, BorderLayout.SOUTH);
    }

    JComponent preferredFocusComponent() {
        return prompt;
    }

    private void send(Project project) {
        String text = prompt.getText().trim();
        if (text.isBlank()) {
            status.setText("Enter a prompt");
            return;
        }
        if (usesCloud() && !hasSelectedCloudKey()) {
            status.setText("Enter " + cloudProvider.getSelectedItem() + " key");
            updateCloudKeyStatus();
            return;
        }
        lastPrompt = text;
        rerunLastPrompt.setEnabled(true);
        AssistantCommand.Selection route = selectedRoute();
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                text,
                route,
                String.valueOf(mode.getSelectedItem()),
                project == null || project.getBasePath() == null ? "" : project.getBasePath(),
                customCommand.getText(),
                allowSourceMutation.isSelected());
        append("You [" + mode.getSelectedItem() + " via " + routeLabel(route) + "]:\n" + text);
        prompt.setText("");
        if (invocation.isLocal()) {
            showLocalResponse(invocation.localResponse());
            return;
        }
        if (!mcpConfigured()) {
            showLocalResponse("Configure SHAFT MCP in Settings before sending Assistant requests.");
            status.setText("Configure MCP");
            return;
        }
        setRunning(true, "Running " + invocation.toolName() + "...");
        currentInvocation = ShaftMcpInvocationService.getInstance(project).startTool(invocation.toolName(), invocation.arguments());
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showResult(invocation.toolName(), result, error)));
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
        setRunning(false, success ? "Finished" : "Failed");
        if (cancelled) {
            showResponse("SHAFT Assistant [" + toolName + " cancelled]");
            status.setText("Cancelled");
            return;
        }
        String output = error != null ? error.getMessage()
                : result == null ? "No result returned."
                : JsonText.prettyOrOriginal(result.output());
        showResponse("SHAFT Assistant [" + toolName + (success ? " OK" : " failed") + "]:\n" + output);
    }

    private void showLocalResponse(String response) {
        status.setText("Ready");
        showResponse("SHAFT Assistant:\n" + response);
    }

    private void showResponse(String response) {
        lastResponse = response;
        copyLastResponse.setEnabled(true);
        append(response);
    }

    private void append(String text) {
        if (transcript.getText().isBlank() || transcript.getText().startsWith("Type a question")) {
            transcript.setText(text);
        } else {
            transcript.append("\n\n" + text);
        }
        transcript.setCaretPosition(transcript.getDocument().getLength());
    }

    private void setRunning(boolean running, String message) {
        send.setEnabled(!running);
        testConnection.setEnabled(!running);
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
        cancel.setEnabled(running);
        progress.setVisible(running);
        status.setText(message);
        if (!running) {
            currentInvocation = null;
        }
    }

    private void updateControlVisibility() {
        boolean cloud = usesCloud();
        if (cloud && "AGENT".equals(mode.getSelectedItem())) {
            mode.setSelectedItem("PLAN");
        }
        boolean localCli = !cloud && "CLI".equals(assistantRuntime.getSelectedItem());
        assistantFamily.setVisible(!cloud);
        assistantRuntime.setVisible(!cloud);
        customCommand.setVisible(localCli);
        cloudProvider.setVisible(cloud);
        cloudModel.setVisible(cloud);
        cloudKeyPanel.setVisible(cloud);
        boolean agentMode = "AGENT".equals(mode.getSelectedItem());
        allowSourceMutation.setVisible(agentMode && localCli);
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
        cloudKeyStatus.setText(stored ? provider + " key stored" : "Enter " + provider + " key");
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

    private void clearTranscript() {
        transcript.setText("");
        lastResponse = "";
        copyLastResponse.setEnabled(false);
        status.setText("Cleared");
    }

    private void rerun(Project project) {
        if (!lastPrompt.isBlank()) {
            prompt.setText(lastPrompt);
            send(project);
        }
    }

    private void testConnection(Project project) {
        if (!mcpConfigured()) {
            showLocalResponse("Configure SHAFT MCP in Settings before testing the connection.");
            status.setText("Configure MCP");
            return;
        }
        setRunning(true, "Testing MCP...");
        currentInvocation = ShaftMcpInvocationService.getInstance(project).testConnection();
        currentInvocation.future().whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                () -> showResult("mcp initialize", result, error)));
    }

    private void cancelCurrent() {
        if (currentInvocation != null) {
            currentInvocation.cancel();
            status.setText("Cancelling...");
        }
    }

    private void copy(String value, String message) {
        if (!value.isBlank()) {
            CopyPasteManager.getInstance().setContents(new StringSelection(value));
            status.setText(message);
        }
    }

    private boolean mcpConfigured() {
        return settings.mcpCommand != null && !settings.mcpCommand.isBlank();
    }

    private boolean usesCloud() {
        return "CLOUD".equals(providerType.getSelectedItem());
    }

    private static JPanel setupNotice(Project project, ShaftSettingsState.Settings settings) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        panel.add(new JLabel("Configure SHAFT MCP to run Assistant and Tools."));
        JButton openSettings = new JButton("Open Settings");
        openSettings.getAccessibleContext().setAccessibleName("Open SHAFT settings");
        openSettings.addActionListener(event -> {
            if (project != null) {
                ShowSettingsUtil.getInstance().showSettingsDialog(project, "SHAFT");
            }
        });
        panel.add(openSettings);
        panel.setVisible(settings.mcpCommand == null || settings.mcpCommand.isBlank());
        return panel;
    }

    private static JComboBox<String> combo(String accessibleName, String... values) {
        JComboBox<String> combo = new JComboBox<>(values);
        combo.getAccessibleContext().setAccessibleName(accessibleName);
        return combo;
    }

    private static JButton button(String text, String accessibleName, java.awt.event.ActionListener action) {
        JButton button = new JButton(text);
        button.getAccessibleContext().setAccessibleName(accessibleName);
        button.addActionListener(action);
        return button;
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
            return route.cloudProvider();
        }
        return route.displayName();
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
}
