package com.shaft.intellij.settings;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.ui.Messages;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import com.shaft.intellij.mcp.ShaftMcpConnectionProbe;
import com.shaft.intellij.mcp.ShaftMcpInstallResult;
import com.shaft.intellij.mcp.ShaftMcpInstaller;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.JButton;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import java.awt.FlowLayout;
import java.util.Arrays;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * Settings page for SHAFT IntelliJ integration.
 */
public final class ShaftSettingsConfigurable implements SearchableConfigurable {
    private static final String OPENAI_PROVIDER_KEY = "OPENAI_API_KEY";
    private static final String ANTHROPIC_PROVIDER_KEY = "ANTHROPIC_API_KEY";
    private static final String GEMINI_PROVIDER_KEY = "GEMINI_API_KEY";
    private static final String GITHUB_PROVIDER_KEY = "GITHUB_TOKEN";

    private final Supplier<ShaftSettingsState.Settings> settingsProvider;
    private final Supplier<CredentialAccess> credentialsProvider;
    private JPanel panel;
    private JBTextField mcpCommand;
    private JButton installMcp;
    private JButton testMcp;
    private JButton configureCopilotMcp;
    private JLabel testStatus;
    private JComboBox<String> defaultClient;
    private JComboBox<String> defaultMode;
    private JComboBox<String> pilotAiProvider;
    private JBTextField pilotAiModel;
    private JBCheckBox passProviderKeys;
    private JPasswordField openAiKey;
    private JPasswordField anthropicKey;
    private JPasswordField geminiKey;
    private JPasswordField githubKey;
    private JButton clearOpenAiKey;
    private JButton clearAnthropicKey;
    private JButton clearGeminiKey;
    private JButton clearGithubKey;
    private JLabel openAiKeyStatus;
    private JLabel anthropicKeyStatus;
    private JLabel geminiKeyStatus;
    private JLabel githubKeyStatus;
    private boolean openAiClearRequested;
    private boolean anthropicClearRequested;
    private boolean geminiClearRequested;
    private boolean githubClearRequested;

    /**
     * Creates a settings page backed by IntelliJ persistent services.
     */
    public ShaftSettingsConfigurable() {
        this(() -> ShaftSettingsState.getInstance().getState(), ShaftSettingsConfigurable::credentialAccess);
    }

    ShaftSettingsConfigurable(ShaftSettingsState.Settings settings, CredentialAccess credentials) {
        this(() -> settings, () -> credentials);
    }

    private ShaftSettingsConfigurable(Supplier<ShaftSettingsState.Settings> settingsProvider,
                                      Supplier<CredentialAccess> credentialsProvider) {
        this.settingsProvider = settingsProvider;
        this.credentialsProvider = credentialsProvider;
    }

    @Override
    public @Nls String getDisplayName() {
        return "SHAFT";
    }

    @Override
    public @NotNull String getId() {
        return "shaft.settings";
    }

    @Override
    public @Nullable JComponent createComponent() {
        mcpCommand = new JBTextField();
        mcpCommand.getEmptyText().setText("java -jar path/to/shaft-mcp.jar stdio");
        mcpCommand.getAccessibleContext().setAccessibleName("MCP stdio command");
        mcpCommand.getAccessibleContext().setAccessibleDescription("Command used to start SHAFT MCP in stdio mode.");
        installMcp = new JButton("Install / Update SHAFT MCP");
        installMcp.getAccessibleContext().setAccessibleName("Install or update SHAFT MCP");
        installMcp.getAccessibleContext().setAccessibleDescription(
                "Install or update shaft-mcp and fill the plugin stdio command automatically.");
        installMcp.addActionListener(event -> installMcp());
        testMcp = new JButton("Test MCP");
        testMcp.getAccessibleContext().setAccessibleName("Test MCP");
        testMcp.getAccessibleContext().setAccessibleDescription(
                "Run a one-time SHAFT MCP connection check with current settings.");
        testMcp.addActionListener(event -> testMcpConnection());
        configureCopilotMcp = new JButton("Connect GitHub Copilot MCP");
        configureCopilotMcp.getAccessibleContext().setAccessibleName("Connect GitHub Copilot MCP");
        configureCopilotMcp.getAccessibleContext().setAccessibleDescription(
                "Install or update shaft-mcp and configure GitHub Copilot for IntelliJ IDEA.");
        configureCopilotMcp.addActionListener(event -> configureCopilotMcp());
        testStatus = help("Not tested");
        defaultClient = new JComboBox<>(model("CODEX", "CLAUDE_CODE", "COPILOT_CLI"));
        defaultClient.getAccessibleContext().setAccessibleName("Default assistant provider");
        defaultClient.getAccessibleContext().setAccessibleDescription("Default assistant provider used when opening the assistant panel.");
        defaultMode = new JComboBox<>(model("ASK", "PLAN", "AGENT"));
        defaultMode.getAccessibleContext().setAccessibleName("Default assistant mode");
        defaultMode.getAccessibleContext().setAccessibleDescription("Default assistant mode used when opening the assistant panel.");
        pilotAiProvider = new JComboBox<>(model("none", "openai", "anthropic", "gemini", "ollama"));
        pilotAiProvider.getAccessibleContext().setAccessibleName("SHAFT AI provider");
        pilotAiProvider.getAccessibleContext().setAccessibleDescription(
                "Optional SHAFT AI provider used by MCP tools that request configured provider assistance.");
        pilotAiModel = new JBTextField();
        pilotAiModel.getEmptyText().setText("Provider model, for example gpt-4.1");
        pilotAiModel.getAccessibleContext().setAccessibleName("SHAFT AI provider model");
        pilotAiModel.getAccessibleContext().setAccessibleDescription("Model name passed to SHAFT MCP provider tools.");
        passProviderKeys = new JBCheckBox("Pass stored provider keys to SHAFT MCP environment");
        passProviderKeys.getAccessibleContext().setAccessibleDescription(
                "If enabled, SHAFT MCP is started with stored provider keys in process environment.");
        openAiKey = new JPasswordField();
        openAiKey.getAccessibleContext().setAccessibleName("OpenAI API key");
        openAiKey.getAccessibleContext().setAccessibleDescription("Stored key remains masked; enter a replacement to save.");
        anthropicKey = new JPasswordField();
        anthropicKey.getAccessibleContext().setAccessibleName("Anthropic API key");
        anthropicKey.getAccessibleContext().setAccessibleDescription("Stored key remains masked; enter a replacement to save.");
        geminiKey = new JPasswordField();
        geminiKey.getAccessibleContext().setAccessibleName("Gemini API key");
        geminiKey.getAccessibleContext().setAccessibleDescription("Stored key remains masked; enter a replacement to save.");
        githubKey = new JPasswordField();
        githubKey.getAccessibleContext().setAccessibleName("GitHub API key");
        githubKey.getAccessibleContext().setAccessibleDescription("Stored key remains masked; enter a replacement to save.");
        openAiKeyStatus = keyStatusLabel("OpenAI");
        anthropicKeyStatus = keyStatusLabel("Anthropic");
        geminiKeyStatus = keyStatusLabel("Gemini");
        githubKeyStatus = keyStatusLabel("GitHub");
        clearOpenAiKey = new JButton("Clear");
        configureClearButton(clearOpenAiKey, "Clear stored OpenAI API key", openAiKey, openAiKeyStatus, () -> openAiClearRequested = true);
        clearAnthropicKey = new JButton("Clear");
        configureClearButton(clearAnthropicKey, "Clear stored Anthropic API key", anthropicKey, anthropicKeyStatus, () -> anthropicClearRequested = true);
        clearGeminiKey = new JButton("Clear");
        configureClearButton(clearGeminiKey, "Clear stored Gemini API key", geminiKey, geminiKeyStatus, () -> geminiClearRequested = true);
        clearGithubKey = new JButton("Clear");
        configureClearButton(clearGithubKey, "Clear stored GitHub API key", githubKey, githubKeyStatus, () -> githubClearRequested = true);

        panel = FormBuilder.createFormBuilder()
                .addComponent(section("MCP"))
                .addComponent(actionRow(installMcp, configureCopilotMcp))
                .addLabeledComponent(label("MCP stdio command", 'M', mcpCommand), mcpCommand)
                .addLabeledComponent(testMcp, testStatus)
                .addComponent(help("The command is filled by the installer. Edit it only for a custom local shaft-mcp runtime."))
                .addComponent(section("Assistant"))
                .addLabeledComponent(label("Default assistant provider", 'C', defaultClient), defaultClient)
                .addLabeledComponent(label("Default assistant mode", 'D', defaultMode), defaultMode)
                .addComponent(help("The Assistant tab is always available. Agent mode still requires explicit source mutation approval per request."))
                .addComponent(section("SHAFT AI provider"))
                .addLabeledComponent(label("Provider", 'P', pilotAiProvider), pilotAiProvider)
                .addLabeledComponent(label("Model", 'L', pilotAiModel), pilotAiModel)
                .addComponent(help("Provider settings apply only to MCP tools that explicitly request configured SHAFT AI assistance."))
                .addComponent(section("Provider keys"))
                .addComponent(passProviderKeys)
                .addComponent(help("Passing keys exposes them only to the SHAFT MCP process. Disable to keep provider credentials local to IntelliJ only."))
                .addComponent(help("Provider keys are stored in IntelliJ Password Safe. Use 'Clear' only to remove a stored key."))
                .addLabeledComponent(label("OpenAI API key", 'O', openAiKey), openAiKey)
                .addComponent(keyRow(clearOpenAiKey, openAiKeyStatus))
                .addLabeledComponent(label("Anthropic API key", 'A', anthropicKey), anthropicKey)
                .addComponent(keyRow(clearAnthropicKey, anthropicKeyStatus))
                .addLabeledComponent(label("Gemini API key", 'I', geminiKey), geminiKey)
                .addComponent(keyRow(clearGeminiKey, geminiKeyStatus))
                .addLabeledComponent(label("GitHub API key", 'G', githubKey), githubKey)
                .addComponent(keyRow(clearGithubKey, githubKeyStatus))
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
        reset();
        return panel;
    }

    @Override
    public boolean isModified() {
        ShaftSettingsState.Settings state = settingsProvider.get();
        return !Objects.equals(state.mcpCommand, mcpCommand.getText())
                || !Objects.equals(state.defaultAutobotClient, defaultClient.getSelectedItem())
                || !Objects.equals(state.defaultAutobotMode, defaultMode.getSelectedItem())
                || !Objects.equals(state.pilotAiProvider, pilotAiProvider.getSelectedItem())
                || !Objects.equals(state.pilotAiModel, pilotAiModel.getText())
                || state.passProviderApiKeysToMcp != passProviderKeys.isSelected()
                || openAiClearRequested
                || anthropicClearRequested
                || geminiClearRequested
                || githubClearRequested
                || hasPassword(openAiKey)
                || hasPassword(anthropicKey)
                || hasPassword(geminiKey)
                || hasPassword(githubKey);
    }

    @Override
    public void apply() throws ConfigurationException {
        ShaftSettingsState.Settings state = settingsProvider.get();
        String command = mcpCommand.getText().trim();
        if (!Objects.equals(state.mcpCommand, command)) {
            state.mcpSetupComplete = false;
        }
        state.mcpCommand = command;
        state.defaultAutobotClient = String.valueOf(defaultClient.getSelectedItem());
        state.defaultAutobotMode = String.valueOf(defaultMode.getSelectedItem());
        state.pilotAiProvider = String.valueOf(pilotAiProvider.getSelectedItem());
        state.pilotAiModel = pilotAiModel.getText().trim();
        state.passProviderApiKeysToMcp = passProviderKeys.isSelected();

        CredentialAccess credentials = credentialsProvider.get();
        applyCredentialChange(credentials, OPENAI_PROVIDER_KEY, openAiKey, openAiClearRequested);
        applyCredentialChange(credentials, ANTHROPIC_PROVIDER_KEY, anthropicKey, anthropicClearRequested);
        applyCredentialChange(credentials, GEMINI_PROVIDER_KEY, geminiKey, geminiClearRequested);
        applyCredentialChange(credentials, GITHUB_PROVIDER_KEY, githubKey, githubClearRequested);
        updateStoredKeyStatus(credentials);
        openAiClearRequested = false;
        anthropicClearRequested = false;
        geminiClearRequested = false;
        githubClearRequested = false;
    }

    @Override
    public void reset() {
        ShaftSettingsState.Settings state = settingsProvider.get();
        mcpCommand.setText(state.mcpCommand);
        defaultClient.setSelectedItem(state.defaultAutobotClient);
        defaultMode.setSelectedItem(state.defaultAutobotMode);
        pilotAiProvider.setSelectedItem(state.pilotAiProvider == null || state.pilotAiProvider.isBlank()
                ? "none" : state.pilotAiProvider);
        pilotAiModel.setText(state.pilotAiModel == null ? "" : state.pilotAiModel);
        passProviderKeys.setSelected(state.passProviderApiKeysToMcp);
        openAiClearRequested = false;
        anthropicClearRequested = false;
        geminiClearRequested = false;
        githubClearRequested = false;
        clear(openAiKey);
        clear(anthropicKey);
        clear(geminiKey);
        clear(githubKey);
        updateStoredKeyStatus(credentialsProvider.get());
    }

    @Override
    public void disposeUIResources() {
        panel = null;
        mcpCommand = null;
        installMcp = null;
        testMcp = null;
        configureCopilotMcp = null;
        testStatus = null;
        defaultClient = null;
        defaultMode = null;
        pilotAiProvider = null;
        pilotAiModel = null;
        passProviderKeys = null;
        openAiKey = null;
        anthropicKey = null;
        geminiKey = null;
        githubKey = null;
        clearOpenAiKey = null;
        clearAnthropicKey = null;
        clearGeminiKey = null;
        clearGithubKey = null;
        openAiKeyStatus = null;
        anthropicKeyStatus = null;
        geminiKeyStatus = null;
        githubKeyStatus = null;
    }

    private static ComboBoxModel<String> model(String... values) {
        return new DefaultComboBoxModel<>(values);
    }

    private static JLabel section(String text) {
        JLabel label = new JLabel(text);
        label.getAccessibleContext().setAccessibleName(text + " settings section");
        return label;
    }

    private static JLabel help(String text) {
        JLabel label = new JLabel(text);
        label.setEnabled(false);
        return label;
    }

    private static JLabel label(String text, char mnemonic, JComponent target) {
        JLabel label = new JLabel(text);
        label.setDisplayedMnemonic(mnemonic);
        label.setLabelFor(target);
        return label;
    }

    private static JButton configureClearButton(JButton button,
                                               String accessibleName,
                                               JPasswordField field,
                                               JLabel statusLabel,
                                               Runnable clearRequestedSetter) {
        button.getAccessibleContext().setAccessibleName(accessibleName);
        button.getAccessibleContext().setAccessibleDescription("Mark this provider key as ready to clear on apply.");
        button.addActionListener(event -> {
            clearRequestedSetter.run();
            clear(field);
            if (statusLabel != null) {
                statusLabel.setText("Clear requested on apply.");
            }
        });
        return button;
    }

    private static JLabel keyStatusLabel(String providerName) {
        JLabel label = new JLabel("Checking...");
        label.getAccessibleContext().setAccessibleName(providerName + " key storage status");
        label.getAccessibleContext().setAccessibleDescription("Shows whether a key is stored for this provider.");
        return label;
    }

    private static JPanel keyRow(JButton clearButton, JLabel statusLabel) {
        JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        row.add(clearButton);
        row.add(statusLabel);
        return row;
    }

    private static JPanel actionRow(JButton first, JButton second) {
        JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        row.add(first);
        row.add(second);
        return row;
    }

    private static boolean hasPassword(JPasswordField field) {
        return field != null && field.getPassword().length > 0;
    }

    private static void applyCredentialChange(CredentialAccess credentials, String key, JPasswordField field, boolean clearRequested) {
        char[] password = field.getPassword();
        boolean hasRealValue = hasMeaningfulValue(password);
        if (hasRealValue) {
            credentials.setApiKey(key, password);
        } else if (clearRequested) {
            credentials.setApiKey(key, password);
        }
        if (clearRequested || hasRealValue || password.length > 0) {
            clear(field);
        }
    }

    private static boolean hasMeaningfulValue(char[] password) {
        for (char c : password) {
            if (!Character.isWhitespace(c)) {
                return true;
            }
        }
        return false;
    }

    private static void clear(JPasswordField field) {
        char[] password = field.getPassword();
        Arrays.fill(password, '\0');
        field.setText("");
    }

    private void updateStoredKeyStatus(CredentialAccess credentials) {
        updateStoredState(openAiKeyStatus, credentials.hasApiKey(OPENAI_PROVIDER_KEY));
        updateStoredState(anthropicKeyStatus, credentials.hasApiKey(ANTHROPIC_PROVIDER_KEY));
        updateStoredState(geminiKeyStatus, credentials.hasApiKey(GEMINI_PROVIDER_KEY));
        updateStoredState(githubKeyStatus, credentials.hasApiKey(GITHUB_PROVIDER_KEY));
    }

    private static void updateStoredState(JLabel statusLabel, boolean stored) {
        if (statusLabel == null) {
            return;
        }
        String text = stored ? "Stored in Password Safe." : "No stored key.";
        statusLabel.setText(text);
    }

    private void testMcpConnection() {
        JButton button = testMcp;
        JLabel statusLabel = testStatus;
        JPanel host = panel;
        if (button == null || statusLabel == null || host == null || mcpCommand == null || passProviderKeys == null) {
            return;
        }
        button.setEnabled(false);
        statusLabel.setText("Testing...");
        String command = mcpCommand.getText() == null ? "" : mcpCommand.getText().trim();
        ShaftMcpConnectionProbe.test(command, formSettings()).whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> {
                    if (button == null || statusLabel == null || host == null) {
                        return;
                    }
                    button.setEnabled(true);
                    if (error != null) {
                        statusLabel.setText("Failed");
                        Messages.showErrorDialog(host, error.getMessage(), "SHAFT MCP");
                    } else {
                        showProbeResult(host, statusLabel, result);
                    }
                }));
    }

    private void showProbeResult(JPanel host, JLabel statusLabel, ShaftMcpToolResult result) {
        if (host == null || statusLabel == null) {
            return;
        }
        if (result != null && result.success()) {
            statusLabel.setText("Connected");
            saveConnectedSettings();
        } else {
            statusLabel.setText("Failed");
            Messages.showErrorDialog(host, result == null ? "No result returned." : result.output(), "SHAFT MCP");
        }
    }

    private void installMcp() {
        JButton button = installMcp;
        if (button == null || testStatus == null) {
            return;
        }
        button.setEnabled(false);
        testStatus.setText("Installing...");
        ShaftMcpInstaller.installForPlugin().whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> showInstallResult(button, result, error)));
    }

    private void showInstallResult(JButton button, ShaftMcpInstallResult result, Throwable error) {
        if (button != null) {
            button.setEnabled(true);
        }
        if (error != null) {
            testStatus.setText("Install failed");
            Messages.showErrorDialog(panel, error.getMessage(), "SHAFT MCP");
            return;
        }
        if (result != null && result.success()) {
            mcpCommand.setText(result.commandLine());
            ShaftSettingsState.Settings state = settingsProvider.get();
            state.mcpCommand = result.commandLine();
            state.mcpSetupComplete = false;
            testStatus.setText("Installed");
        } else {
            testStatus.setText("Install failed");
            Messages.showErrorDialog(panel, result == null ? "No installer result returned." : result.output(),
                    "SHAFT MCP");
        }
    }

    private void configureCopilotMcp() {
        JButton button = configureCopilotMcp;
        if (button == null || testStatus == null) {
            return;
        }
        button.setEnabled(false);
        testStatus.setText("Configuring Copilot...");
        ShaftMcpInstaller.configureCopilotIntellij().whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> showCopilotResult(button, result, error)));
    }

    private void showCopilotResult(JButton button, ShaftMcpInstallResult result, Throwable error) {
        if (button != null) {
            button.setEnabled(true);
        }
        if (error == null && result != null && result.success()) {
            testStatus.setText("Copilot configured");
        } else {
            testStatus.setText("Copilot failed");
            Messages.showErrorDialog(panel,
                    error != null ? error.getMessage() : result == null ? "No installer result returned." : result.output(),
                    "SHAFT MCP");
        }
    }

    private ShaftSettingsState.Settings formSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = mcpCommand.getText() == null ? "" : mcpCommand.getText().trim();
        settings.defaultAutobotClient = String.valueOf(defaultClient.getSelectedItem());
        settings.defaultAutobotMode = String.valueOf(defaultMode.getSelectedItem());
        settings.pilotAiProvider = String.valueOf(pilotAiProvider.getSelectedItem());
        settings.pilotAiModel = pilotAiModel.getText() == null ? "" : pilotAiModel.getText().trim();
        settings.passProviderApiKeysToMcp = passProviderKeys.isSelected();
        return settings;
    }

    private void saveConnectedSettings() {
        ShaftSettingsState.Settings state = settingsProvider.get();
        ShaftSettingsState.Settings form = formSettings();
        state.mcpCommand = form.mcpCommand;
        state.defaultAutobotClient = form.defaultAutobotClient;
        state.defaultAutobotMode = form.defaultAutobotMode;
        state.pilotAiProvider = form.pilotAiProvider;
        state.pilotAiModel = form.pilotAiModel;
        state.passProviderApiKeysToMcp = form.passProviderApiKeysToMcp;
        state.mcpSetupComplete = true;
    }

    interface CredentialAccess {
        void setApiKey(String provider, char[] secret);

        boolean hasApiKey(String provider);
    }

    private static CredentialAccess credentialAccess() {
        ShaftCredentialService service = ShaftCredentialService.getInstance();
        return new CredentialAccess() {
            @Override
            public void setApiKey(String provider, char[] secret) {
                service.setApiKey(provider, secret);
            }

            @Override
            public boolean hasApiKey(String provider) {
                return service.hasApiKey(provider);
            }
        };
    }
}
