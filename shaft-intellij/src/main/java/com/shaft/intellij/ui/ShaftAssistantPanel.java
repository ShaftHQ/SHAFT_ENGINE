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
import com.shaft.intellij.settings.ShaftSettingsState;
import org.jetbrains.annotations.NotNull;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.KeyStroke;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.datatransfer.StringSelection;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.concurrent.CancellationException;

/**
 * SHAFT Assistant chat-style panel.
 */
final class ShaftAssistantPanel extends JPanel {
    private final JComboBox<String> mode;
    private final JComboBox<String> client;
    private final JBTextField customCommand;
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

    ShaftAssistantPanel(@NotNull Project project) {
        this(project, ShaftSettingsState.getInstance().getState());
    }

    ShaftAssistantPanel(Project project, @NotNull ShaftSettingsState.Settings settings) {
        super(new BorderLayout(6, 6));
        this.settings = settings;
        setBorder(JBUI.Borders.empty(8));
        mode = new JComboBox<>(new String[]{"ASK", "PLAN", "AGENT"});
        client = new JComboBox<>(new String[]{"CODEX", "CLAUDE_CODE", "COPILOT_CLI"});
        mode.setSelectedItem(settings.defaultAutobotMode);
        client.setSelectedItem(settings.defaultAutobotClient);
        customCommand = new JBTextField();
        customCommand.getEmptyText().setText("Optional local agent command");
        allowSourceMutation = new JBCheckBox("Approve source mutation for Agent mode");
        prompt = new JBTextArea(5, 32);
        prompt.setLineWrap(true);
        prompt.setWrapStyleWord(true);
        transcript = new JBTextArea(18, 32);
        transcript.setEditable(false);
        transcript.setLineWrap(true);
        transcript.setWrapStyleWord(true);
        transcript.setText("Type a question or use /help for SHAFT commands.");
        status = new JLabel("Ready");
        progress = new JProgressBar();
        progress.setIndeterminate(true);
        progress.setVisible(false);

        send = new JButton("Send");
        send.addActionListener(event -> send(project));
        cancel = new JButton("Cancel");
        cancel.setEnabled(false);
        cancel.addActionListener(event -> cancelCurrent());
        copyLastResponse = new JButton("Copy response");
        copyLastResponse.setEnabled(false);
        copyLastResponse.addActionListener(event -> copyLastResponse());
        copyTranscript = new JButton("Copy all");
        copyTranscript.addActionListener(event -> copy(transcript.getText(), "Copied transcript"));
        clearTranscript = new JButton("Clear");
        clearTranscript.addActionListener(event -> clearTranscript());
        rerunLastPrompt = new JButton("Rerun");
        rerunLastPrompt.setEnabled(false);
        rerunLastPrompt.addActionListener(event -> rerun(project));
        testConnection = new JButton("Test MCP");
        testConnection.addActionListener(event -> testConnection(project));
        mode.addActionListener(event -> updateMutationVisibility());
        updateMutationVisibility();
        bindKeyboard(project);

        JPanel controls = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        controls.add(label("Mode", 'M', mode));
        controls.add(mode);
        controls.add(label("Client", 'L', client));
        controls.add(client);
        controls.add(allowSourceMutation);

        JPanel commandPanel = new JPanel(new BorderLayout(4, 4));
        commandPanel.add(label("Command", 'U', customCommand), BorderLayout.WEST);
        commandPanel.add(customCommand, BorderLayout.CENTER);

        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        actions.add(testConnection);
        actions.add(copyLastResponse);
        actions.add(copyTranscript);
        actions.add(clearTranscript);
        actions.add(rerunLastPrompt);
        actions.add(cancel);
        actions.add(status);

        JPanel north = new JPanel(new BorderLayout(4, 4));
        north.add(controls, BorderLayout.NORTH);
        north.add(commandPanel, BorderLayout.CENTER);
        north.add(setupNotice(project, settings), BorderLayout.SOUTH);

        JPanel transcriptPanel = new JPanel(new BorderLayout(4, 4));
        transcriptPanel.add(label("Transcript", 'T', transcript), BorderLayout.NORTH);
        transcriptPanel.add(new JBScrollPane(transcript), BorderLayout.CENTER);

        JPanel promptActions = new JPanel(new FlowLayout(FlowLayout.RIGHT, 6, 0));
        promptActions.add(progress);
        promptActions.add(send);

        JPanel composer = new JPanel(new BorderLayout(4, 4));
        composer.add(label("Prompt", 'P', prompt), BorderLayout.NORTH);
        composer.add(new JBScrollPane(prompt), BorderLayout.CENTER);
        composer.add(promptActions, BorderLayout.SOUTH);

        JPanel south = new JPanel(new BorderLayout(4, 4));
        south.add(actions, BorderLayout.NORTH);
        south.add(composer, BorderLayout.CENTER);

        add(north, BorderLayout.NORTH);
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
        lastPrompt = text;
        rerunLastPrompt.setEnabled(true);
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                text,
                String.valueOf(client.getSelectedItem()),
                String.valueOf(mode.getSelectedItem()),
                project == null || project.getBasePath() == null ? "" : project.getBasePath(),
                customCommand.getText(),
                allowSourceMutation.isSelected());
        append("You [" + mode.getSelectedItem() + " via " + client.getSelectedItem() + "]:\n" + text);
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
        client.setEnabled(!running);
        customCommand.setEnabled(!running);
        allowSourceMutation.setEnabled(!running);
        cancel.setEnabled(running);
        progress.setVisible(running);
        status.setText(message);
        if (!running) {
            currentInvocation = null;
        }
    }

    private void updateMutationVisibility() {
        boolean agentMode = "AGENT".equals(mode.getSelectedItem());
        allowSourceMutation.setVisible(agentMode);
        if (!agentMode) {
            allowSourceMutation.setSelected(false);
        }
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

    private static JPanel setupNotice(Project project, ShaftSettingsState.Settings settings) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        panel.add(new JLabel("Configure SHAFT MCP to run Assistant and Tools."));
        JButton openSettings = new JButton("Open Settings");
        openSettings.addActionListener(event -> {
            if (project != null) {
                ShowSettingsUtil.getInstance().showSettingsDialog(project, "SHAFT");
            }
        });
        panel.add(openSettings);
        panel.setVisible(settings.mcpCommand == null || settings.mcpCommand.isBlank());
        return panel;
    }

    private static JLabel label(String text, char mnemonic, JComponent target) {
        JLabel label = new JLabel(text);
        label.setDisplayedMnemonic(mnemonic);
        label.setLabelFor(target);
        return label;
    }
}
