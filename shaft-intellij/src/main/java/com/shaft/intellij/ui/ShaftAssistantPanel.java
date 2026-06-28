package com.shaft.intellij.ui;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;
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
import javax.swing.KeyStroke;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.datatransfer.StringSelection;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

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
    private final JButton copyLastResponse;
    private final JLabel status;
    private String lastResponse = "";

    ShaftAssistantPanel(@NotNull Project project) {
        super(new BorderLayout(8, 8));
        setBorder(JBUI.Borders.empty(8));
        ShaftSettingsState.Settings settings = ShaftSettingsState.getInstance().getState();
        mode = new JComboBox<>(new String[]{"ASK", "PLAN", "AGENT"});
        client = new JComboBox<>(new String[]{"CODEX", "CLAUDE_CODE", "COPILOT_CLI"});
        mode.setSelectedItem(settings.defaultAutobotMode);
        client.setSelectedItem(settings.defaultAutobotClient);
        customCommand = new JBTextField();
        allowSourceMutation = new JBCheckBox("Approve source mutation for Agent mode");
        prompt = new JBTextArea(8, 56);
        transcript = new JBTextArea(14, 56);
        transcript.setEditable(false);
        transcript.setText("Type a question or use /help for SHAFT commands.");
        status = new JLabel("Ready");

        send = new JButton("Send");
        send.addActionListener(event -> send(project));
        copyLastResponse = new JButton("Copy response");
        copyLastResponse.setEnabled(false);
        copyLastResponse.addActionListener(event -> copyLastResponse());
        mode.addActionListener(event -> updateMutationVisibility());
        updateMutationVisibility();
        bindKeyboard(project);

        JPanel controls = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        controls.add(label("Mode", 'M', mode));
        controls.add(mode);
        controls.add(label("Client", 'L', client));
        controls.add(client);
        controls.add(allowSourceMutation);
        controls.add(send);
        controls.add(copyLastResponse);
        controls.add(status);

        JPanel commandPanel = new JPanel(new BorderLayout(6, 6));
        commandPanel.add(label("Custom local agent command", 'U', customCommand), BorderLayout.WEST);
        commandPanel.add(customCommand, BorderLayout.CENTER);

        JPanel north = new JPanel(new BorderLayout(6, 6));
        north.add(controls, BorderLayout.NORTH);
        north.add(commandPanel, BorderLayout.SOUTH);

        JPanel center = new JPanel(new BorderLayout(6, 6));
        center.add(label("Prompt", 'P', prompt), BorderLayout.NORTH);
        center.add(new JBScrollPane(prompt), BorderLayout.CENTER);

        JPanel south = new JPanel(new BorderLayout(6, 6));
        south.add(label("Transcript", 'T', transcript), BorderLayout.NORTH);
        south.add(new JBScrollPane(transcript), BorderLayout.CENTER);

        add(north, BorderLayout.NORTH);
        add(center, BorderLayout.CENTER);
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
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                text,
                String.valueOf(client.getSelectedItem()),
                String.valueOf(mode.getSelectedItem()),
                project.getBasePath() == null ? "" : project.getBasePath(),
                customCommand.getText(),
                allowSourceMutation.isSelected());
        append("You [" + mode.getSelectedItem() + " via " + client.getSelectedItem() + "]:\n" + text);
        prompt.setText("");
        if (invocation.isLocal()) {
            showLocalResponse(invocation.localResponse());
            return;
        }
        setRunning(true, "Running " + invocation.toolName() + "...");
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool(invocation.toolName(), invocation.arguments())
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                        () -> showResult(invocation.toolName(), result, error)));
    }

    private void showResult(String toolName, ShaftMcpToolResult result, Throwable error) {
        boolean success = error == null && result != null && result.success();
        setRunning(false, success ? "Finished" : "Failed");
        String output = error != null ? error.getMessage()
                : result == null ? "No result returned."
                : result.output();
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
        mode.setEnabled(!running);
        client.setEnabled(!running);
        customCommand.setEnabled(!running);
        allowSourceMutation.setEnabled(!running);
        status.setText(message);
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
            CopyPasteManager.getInstance().setContents(new StringSelection(lastResponse));
            status.setText("Copied response");
        }
    }

    private static JLabel label(String text, char mnemonic, JComponent target) {
        JLabel label = new JLabel(text);
        label.setDisplayedMnemonic(mnemonic);
        label.setLabelFor(target);
        return label;
    }
}
