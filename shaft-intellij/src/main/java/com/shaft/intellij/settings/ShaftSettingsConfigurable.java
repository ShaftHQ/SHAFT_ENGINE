package com.shaft.intellij.settings;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.ui.Messages;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import com.shaft.intellij.mcp.ShaftMcpConnectionProbe;
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
import java.awt.datatransfer.StringSelection;
import java.util.Arrays;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * Settings page for SHAFT IntelliJ integration.
 */
public final class ShaftSettingsConfigurable implements SearchableConfigurable {
    private static final String OPENAI_PROVIDER_KEY = "OPENAI_API_KEY";
    private static final String ANTHROPIC_PROVIDER_KEY = "ANTHROPIC_API_KEY";
    private static final String GITHUB_PROVIDER_KEY = "GITHUB_TOKEN";
    private static final String MCP_INSTALLER_COMMANDS = """
            Windows:
            powershell -ExecutionPolicy Bypass -File scripts\\mcp\\install-shaft-mcp.ps1 --copilot-intellij

            macOS/Linux:
            sh scripts/mcp/install-shaft-mcp.sh --copilot-intellij
            """.stripIndent().trim();

    private final Supplier<ShaftSettingsState.Settings> settingsProvider;
    private final Supplier<CredentialAccess> credentialsProvider;
    private JPanel panel;
    private JBTextField mcpCommand;
    private JButton testMcp;
    private JButton copyInstallerCommand;
    private JLabel testStatus;
    private JComboBox<String> defaultClient;
    private JComboBox<String> defaultMode;
    private JBCheckBox passProviderKeys;
    private JPasswordField openAiKey;
    private JPasswordField anthropicKey;
    private JPasswordField githubKey;
    private JButton clearOpenAiKey;
    private JButton clearAnthropicKey;
    private JButton clearGithubKey;
    private JLabel openAiKeyStatus;
    private JLabel anthropicKeyStatus;
    private JLabel githubKeyStatus;
    private boolean openAiClearRequested;
    private boolean anthropicClearRequested;
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
        testMcp = new JButton("Test MCP");
        testMcp.getAccessibleContext().setAccessibleName("Test MCP");
        testMcp.getAccessibleContext().setAccessibleDescription(
                "Run a one-time SHAFT MCP connection check with current settings.");
        testMcp.addActionListener(event -> testMcpConnection());
        copyInstallerCommand = new JButton("Copy installer command");
        copyInstallerCommand.getAccessibleContext().setAccessibleName("Copy SHAFT MCP installer command");
        copyInstallerCommand.getAccessibleContext().setAccessibleDescription(
                "Copy the supported SHAFT MCP installer commands without running them.");
        copyInstallerCommand.addActionListener(event -> copyInstallerCommand());
        testStatus = help("Not tested");
        defaultClient = new JComboBox<>(model("CODEX", "CLAUDE_CODE", "COPILOT_CLI"));
        defaultClient.getAccessibleContext().setAccessibleName("Default assistant client");
        defaultClient.getAccessibleContext().setAccessibleDescription("Default assistant client used when opening the assistant panel.");
        defaultMode = new JComboBox<>(model("ASK", "PLAN", "AGENT"));
        defaultMode.getAccessibleContext().setAccessibleName("Default assistant mode");
        defaultMode.getAccessibleContext().setAccessibleDescription("Default assistant mode used when opening the assistant panel.");
        passProviderKeys = new JBCheckBox("Pass stored provider keys to SHAFT MCP environment");
        passProviderKeys.getAccessibleContext().setAccessibleDescription(
                "If enabled, SHAFT MCP is started with stored provider keys in process environment.");
        openAiKey = new JPasswordField();
        openAiKey.getAccessibleContext().setAccessibleName("OpenAI API key");
        openAiKey.getAccessibleContext().setAccessibleDescription("Stored key remains masked; enter a replacement to save.");
        anthropicKey = new JPasswordField();
        anthropicKey.getAccessibleContext().setAccessibleName("Anthropic API key");
        anthropicKey.getAccessibleContext().setAccessibleDescription("Stored key remains masked; enter a replacement to save.");
        githubKey = new JPasswordField();
        githubKey.getAccessibleContext().setAccessibleName("GitHub API key");
        githubKey.getAccessibleContext().setAccessibleDescription("Stored key remains masked; enter a replacement to save.");
        openAiKeyStatus = keyStatusLabel("OpenAI");
        anthropicKeyStatus = keyStatusLabel("Anthropic");
        githubKeyStatus = keyStatusLabel("GitHub");
        clearOpenAiKey = new JButton("Clear");
        configureClearButton(clearOpenAiKey, "Clear stored OpenAI API key", openAiKey, openAiKeyStatus, () -> openAiClearRequested = true);
        clearAnthropicKey = new JButton("Clear");
        configureClearButton(clearAnthropicKey, "Clear stored Anthropic API key", anthropicKey, anthropicKeyStatus, () -> anthropicClearRequested = true);
        clearGithubKey = new JButton("Clear");
        configureClearButton(clearGithubKey, "Clear stored GitHub API key", githubKey, githubKeyStatus, () -> githubClearRequested = true);

        panel = FormBuilder.createFormBuilder()
                .addComponent(section("MCP"))
                .addLabeledComponent(label("MCP stdio command", 'M', mcpCommand), mcpCommand)
                .addLabeledComponent(testMcp, testStatus)
                .addComponent(copyInstallerCommand)
                .addComponent(help("Run the installer from a SHAFT_ENGINE checkout, then paste the generated java @shaft-mcp.args command here."))
                .addComponent(section("Assistant"))
                .addLabeledComponent(label("Default assistant client", 'C', defaultClient), defaultClient)
                .addLabeledComponent(label("Default assistant mode", 'D', defaultMode), defaultMode)
                .addComponent(help("The Assistant tab is always available. Agent mode still requires explicit source mutation approval per request."))
                .addComponent(section("Provider keys"))
                .addComponent(passProviderKeys)
                .addComponent(help("Passing keys exposes them only to the SHAFT MCP process. Disable to keep provider credentials local to IntelliJ only."))
                .addComponent(help("OpenAI, Anthropic, and GitHub keys are stored in IntelliJ Password Safe. Use 'Clear' only to remove a stored key."))
                .addLabeledComponent(label("OpenAI API key", 'O', openAiKey), openAiKey)
                .addComponent(keyRow(clearOpenAiKey, openAiKeyStatus))
                .addLabeledComponent(label("Anthropic API key", 'A', anthropicKey), anthropicKey)
                .addComponent(keyRow(clearAnthropicKey, anthropicKeyStatus))
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
                || state.passProviderApiKeysToMcp != passProviderKeys.isSelected()
                || openAiClearRequested
                || anthropicClearRequested
                || githubClearRequested
                || hasPassword(openAiKey)
                || hasPassword(anthropicKey)
                || hasPassword(githubKey);
    }

    @Override
    public void apply() throws ConfigurationException {
        ShaftSettingsState.Settings state = settingsProvider.get();
        state.mcpCommand = mcpCommand.getText().trim();
        state.defaultAutobotClient = String.valueOf(defaultClient.getSelectedItem());
        state.defaultAutobotMode = String.valueOf(defaultMode.getSelectedItem());
        state.passProviderApiKeysToMcp = passProviderKeys.isSelected();

        CredentialAccess credentials = credentialsProvider.get();
        applyCredentialChange(credentials, OPENAI_PROVIDER_KEY, openAiKey, openAiClearRequested);
        applyCredentialChange(credentials, ANTHROPIC_PROVIDER_KEY, anthropicKey, anthropicClearRequested);
        applyCredentialChange(credentials, GITHUB_PROVIDER_KEY, githubKey, githubClearRequested);
        updateStoredKeyStatus(credentials);
        openAiClearRequested = false;
        anthropicClearRequested = false;
        githubClearRequested = false;
    }

    @Override
    public void reset() {
        ShaftSettingsState.Settings state = settingsProvider.get();
        mcpCommand.setText(state.mcpCommand);
        defaultClient.setSelectedItem(state.defaultAutobotClient);
        defaultMode.setSelectedItem(state.defaultAutobotMode);
        passProviderKeys.setSelected(state.passProviderApiKeysToMcp);
        openAiClearRequested = false;
        anthropicClearRequested = false;
        githubClearRequested = false;
        clear(openAiKey);
        clear(anthropicKey);
        clear(githubKey);
        updateStoredKeyStatus(credentialsProvider.get());
    }

    @Override
    public void disposeUIResources() {
        panel = null;
        mcpCommand = null;
        testMcp = null;
        copyInstallerCommand = null;
        testStatus = null;
        defaultClient = null;
        defaultMode = null;
        passProviderKeys = null;
        openAiKey = null;
        anthropicKey = null;
        githubKey = null;
        clearOpenAiKey = null;
        clearAnthropicKey = null;
        clearGithubKey = null;
        openAiKeyStatus = null;
        anthropicKeyStatus = null;
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
        boolean passProviderKeys = this.passProviderKeys.isSelected();
        String command = mcpCommand.getText() == null ? "" : mcpCommand.getText().trim();
        ShaftMcpConnectionProbe.test(command, passProviderKeys).whenComplete((result, error) ->
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
        } else {
            statusLabel.setText("Failed");
            Messages.showErrorDialog(host, result == null ? "No result returned." : result.output(), "SHAFT MCP");
        }
    }

    private void copyInstallerCommand() {
        CopyPasteManager.getInstance().setContents(new StringSelection(MCP_INSTALLER_COMMANDS));
        if (testStatus != null) {
            testStatus.setText("Installer command copied");
        }
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
