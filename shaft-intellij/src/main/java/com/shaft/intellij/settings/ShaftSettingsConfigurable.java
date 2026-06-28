package com.shaft.intellij.settings;

import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import java.util.Arrays;
import java.util.Objects;

/**
 * Settings page for SHAFT IntelliJ integration.
 */
public final class ShaftSettingsConfigurable implements Configurable {
    private JPanel panel;
    private JBTextField mcpCommand;
    private JBCheckBox autobotEnabled;
    private JComboBox<String> defaultClient;
    private JComboBox<String> defaultMode;
    private JBCheckBox passProviderKeys;
    private JPasswordField openAiKey;
    private JPasswordField anthropicKey;
    private JPasswordField githubKey;

    @Override
    public @Nls String getDisplayName() {
        return "SHAFT";
    }

    @Override
    public @Nullable JComponent createComponent() {
        mcpCommand = new JBTextField();
        autobotEnabled = new JBCheckBox("Enable Autobot tool window tab");
        defaultClient = new JComboBox<>(model("CODEX", "CLAUDE_CODE", "COPILOT_CLI"));
        defaultMode = new JComboBox<>(model("ASK", "PLAN", "AGENT"));
        passProviderKeys = new JBCheckBox("Pass stored provider keys to SHAFT MCP environment");
        openAiKey = new JPasswordField();
        anthropicKey = new JPasswordField();
        githubKey = new JPasswordField();

        panel = FormBuilder.createFormBuilder()
                .addLabeledComponent("MCP stdio command", mcpCommand)
                .addComponent(autobotEnabled)
                .addLabeledComponent("Default Autobot client", defaultClient)
                .addLabeledComponent("Default Autobot mode", defaultMode)
                .addComponent(passProviderKeys)
                .addLabeledComponent("OpenAI API key", openAiKey)
                .addLabeledComponent("Anthropic API key", anthropicKey)
                .addLabeledComponent("GitHub API key", githubKey)
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
        reset();
        return panel;
    }

    @Override
    public boolean isModified() {
        ShaftSettingsState.Settings state = ShaftSettingsState.getInstance().getState();
        return !Objects.equals(state.mcpCommand, mcpCommand.getText())
                || state.autobotEnabled != autobotEnabled.isSelected()
                || !Objects.equals(state.defaultAutobotClient, defaultClient.getSelectedItem())
                || !Objects.equals(state.defaultAutobotMode, defaultMode.getSelectedItem())
                || state.passProviderApiKeysToMcp != passProviderKeys.isSelected()
                || hasPassword(openAiKey)
                || hasPassword(anthropicKey)
                || hasPassword(githubKey);
    }

    @Override
    public void apply() throws ConfigurationException {
        ShaftSettingsState.Settings state = ShaftSettingsState.getInstance().getState();
        state.mcpCommand = mcpCommand.getText().trim();
        state.autobotEnabled = autobotEnabled.isSelected();
        state.defaultAutobotClient = String.valueOf(defaultClient.getSelectedItem());
        state.defaultAutobotMode = String.valueOf(defaultMode.getSelectedItem());
        state.passProviderApiKeysToMcp = passProviderKeys.isSelected();

        ShaftCredentialService credentials = ShaftCredentialService.getInstance();
        saveIfPresent(credentials, "OPENAI_API_KEY", openAiKey);
        saveIfPresent(credentials, "ANTHROPIC_API_KEY", anthropicKey);
        saveIfPresent(credentials, "GITHUB_TOKEN", githubKey);
    }

    @Override
    public void reset() {
        ShaftSettingsState.Settings state = ShaftSettingsState.getInstance().getState();
        mcpCommand.setText(state.mcpCommand);
        autobotEnabled.setSelected(state.autobotEnabled);
        defaultClient.setSelectedItem(state.defaultAutobotClient);
        defaultMode.setSelectedItem(state.defaultAutobotMode);
        passProviderKeys.setSelected(state.passProviderApiKeysToMcp);
        clear(openAiKey);
        clear(anthropicKey);
        clear(githubKey);
    }

    @Override
    public void disposeUIResources() {
        panel = null;
        mcpCommand = null;
        autobotEnabled = null;
        defaultClient = null;
        defaultMode = null;
        passProviderKeys = null;
        openAiKey = null;
        anthropicKey = null;
        githubKey = null;
    }

    private static ComboBoxModel<String> model(String... values) {
        return new DefaultComboBoxModel<>(values);
    }

    private static boolean hasPassword(JPasswordField field) {
        return field != null && field.getPassword().length > 0;
    }

    private static void saveIfPresent(ShaftCredentialService credentials, String key, JPasswordField field) {
        char[] password = field.getPassword();
        if (password.length > 0) {
            credentials.setApiKey(key, password);
            clear(field);
        }
    }

    private static void clear(JPasswordField field) {
        char[] password = field.getPassword();
        Arrays.fill(password, '\0');
        field.setText("");
    }
}
