package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.shaft.intellij.mcp.ShaftCommandLine;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
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
 * Optional SHAFT Autobot chat-style panel.
 */
final class ShaftAutobotPanel extends JPanel {
    private final JComboBox<String> mode;
    private final JComboBox<String> client;
    private final JBTextField customCommand;
    private final JBCheckBox allowSourceMutation;
    private final JBTextArea prompt;
    private final JBTextArea transcript;

    ShaftAutobotPanel(@NotNull Project project) {
        super(new BorderLayout(8, 8));
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

        JButton send = new JButton("Send");
        send.addActionListener(event -> send(project));

        JPanel controls = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        controls.add(new JLabel("Mode"));
        controls.add(mode);
        controls.add(new JLabel("Client"));
        controls.add(client);
        controls.add(allowSourceMutation);
        controls.add(send);

        JPanel commandPanel = new JPanel(new BorderLayout(6, 6));
        commandPanel.add(new JLabel("Custom local agent command"), BorderLayout.WEST);
        commandPanel.add(customCommand, BorderLayout.CENTER);

        JPanel north = new JPanel(new BorderLayout(6, 6));
        north.add(controls, BorderLayout.NORTH);
        north.add(commandPanel, BorderLayout.SOUTH);

        JPanel center = new JPanel(new BorderLayout(6, 6));
        center.add(new JLabel("Prompt"), BorderLayout.NORTH);
        center.add(new JBScrollPane(prompt), BorderLayout.CENTER);

        JPanel south = new JPanel(new BorderLayout(6, 6));
        south.add(new JLabel("Transcript"), BorderLayout.NORTH);
        south.add(new JBScrollPane(transcript), BorderLayout.CENTER);

        add(north, BorderLayout.NORTH);
        add(center, BorderLayout.CENTER);
        add(south, BorderLayout.SOUTH);
    }

    JComponent preferredFocusComponent() {
        return prompt;
    }

    private void send(Project project) {
        if (prompt.getText().isBlank()) {
            return;
        }
        JsonObject arguments = new JsonObject();
        arguments.addProperty("client", String.valueOf(client.getSelectedItem()));
        arguments.addProperty("mode", String.valueOf(mode.getSelectedItem()));
        arguments.addProperty("prompt", prompt.getText());
        arguments.addProperty("workingDirectory", project.getBasePath() == null ? "" : project.getBasePath());
        arguments.add("command", commandArray(customCommand.getText()));
        arguments.add("environment", new JsonObject());
        arguments.addProperty("timeoutSeconds", 300);
        arguments.addProperty("allowSourceMutation", allowSourceMutation.isSelected());

        append("You [" + mode.getSelectedItem() + " via " + client.getSelectedItem() + "]:\n" + prompt.getText());
        prompt.setText("");
        ShaftMcpInvocationService.getInstance(project)
                .invokeTool("autobot_local_agent_run", arguments)
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(
                        () -> showResult(result, error)));
    }

    private void showResult(ShaftMcpToolResult result, Throwable error) {
        append(error == null ? result.output() : error.getMessage());
    }

    private void append(String text) {
        if (transcript.getText().isBlank()) {
            transcript.setText(text);
        } else {
            transcript.append("\n\n" + text);
        }
        transcript.setCaretPosition(transcript.getDocument().getLength());
    }

    private static JsonArray commandArray(String value) {
        JsonArray array = new JsonArray();
        if (value == null || value.isBlank()) {
            return array;
        }
        for (String token : ShaftCommandLine.parse(value)) {
            array.add(token);
        }
        return array;
    }
}
