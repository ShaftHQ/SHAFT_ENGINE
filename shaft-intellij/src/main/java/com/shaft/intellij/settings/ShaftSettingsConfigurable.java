package com.shaft.intellij.settings;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.ui.Messages;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.McpInvocationError;
import com.shaft.intellij.mcp.ShaftMcpConnectionProbe;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.ui.ShaftIconButtons;
import com.shaft.intellij.ui.ShaftIcons;
import com.shaft.intellij.ui.ShaftStatusPresentation;
import com.shaft.intellij.ui.ShaftUiLabels;
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
import java.util.Locale;
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
    private JButton testMcp;
    private JLabel testStatus;
    private JLabel currentAgentConfigurationTitle;
    private JLabel currentAgentConfiguration;
    private JPanel currentAgentConfigurationRow;
    private JButton configureAgent;
    private JLabel assistantProviderTypeLabel;
    private JLabel assistantFamilyLabel;
    private JLabel assistantRuntimeLabel;
    private JLabel cloudProviderLabel;
    private JLabel cloudModelLabel;
    private JLabel defaultModeLabel;
    private JLabel shaftAiSection;
    private JLabel shaftAiProviderLabel;
    private JLabel shaftAiModelLabel;
    private JLabel providerKeysSection;
    private JLabel shaftAiHelp;
    private JLabel providerKeysHelp;
    private JLabel providerKeysStorageHelp;
    private JLabel openAiKeyLabel;
    private JLabel anthropicKeyLabel;
    private JLabel geminiKeyLabel;
    private JLabel githubKeyLabel;
    private JComboBox<String> assistantProviderType;
    private JComboBox<String> assistantFamily;
    private JComboBox<String> assistantRuntime;
    private JComboBox<String> cloudProvider;
    private JBTextField cloudModel;
    private JComboBox<String> defaultClient;
    private JComboBox<String> defaultMode;
    private JComboBox<String> pilotAiProvider;
    private JBTextField pilotAiModel;
    private JBCheckBox passProviderKeys;
    private JBCheckBox advancedUiEnabled;
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
    private boolean editingAgentConfiguration;

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
        ShaftIconButtons.apply(testMcp, ShaftIcons.SEND);
        testMcp.addActionListener(event -> testMcpConnection());
        testStatus = statusLabel("Not tested");
        testStatus.getAccessibleContext().setAccessibleName("SHAFT MCP test status");
        mcpCommand.getDocument().addDocumentListener(new javax.swing.event.DocumentListener() {
            @Override
            public void insertUpdate(javax.swing.event.DocumentEvent event) {
                resetTestStatus();
            }

            @Override
            public void removeUpdate(javax.swing.event.DocumentEvent event) {
                resetTestStatus();
            }

            @Override
            public void changedUpdate(javax.swing.event.DocumentEvent event) {
                resetTestStatus();
            }
        });
        currentAgentConfiguration = new JLabel();
        currentAgentConfiguration.getAccessibleContext().setAccessibleName("Current agent configuration");
        currentAgentConfiguration.getAccessibleContext().setAccessibleDescription(
                "Read-only assistant agent configuration saved after the MCP setup check.");
        configureAgent = new JButton("Configure");
        configureAgent.getAccessibleContext().setAccessibleName("Configure assistant agent");
        configureAgent.getAccessibleContext().setAccessibleDescription(
                "Edit the assistant agent configuration used by the SHAFT plugin.");
        ShaftIconButtons.apply(configureAgent, ShaftIcons.SETTINGS);
        configureAgent.addActionListener(event -> {
            editingAgentConfiguration = true;
            updateAgentConfigurationControls();
        });
        assistantProviderType = new JComboBox<>(model("LOCAL", "CLOUD"));
        ShaftUiLabels.applyFriendlyRenderer(assistantProviderType);
        assistantProviderType.getAccessibleContext().setAccessibleName("Assistant provider type");
        assistantProviderType.getAccessibleContext().setAccessibleDescription("Select whether Assistant prompts use local tools or a cloud provider.");
        assistantFamily = new JComboBox<>(model("CODEX", "CLAUDE", "COPILOT"));
        ShaftUiLabels.applyFriendlyRenderer(assistantFamily);
        assistantFamily.getAccessibleContext().setAccessibleName("Assistant family");
        assistantFamily.getAccessibleContext().setAccessibleDescription("Local assistant family used by the Assistant tab.");
        assistantRuntime = new JComboBox<>(model("CLI", "IDE_PLUGIN", "DESKTOP_APP"));
        ShaftUiLabels.applyFriendlyRenderer(assistantRuntime);
        assistantRuntime.getAccessibleContext().setAccessibleName("Assistant runtime");
        assistantRuntime.getAccessibleContext().setAccessibleDescription("Local runtime: command line, IDE plugin, or desktop app.");
        cloudProvider = new JComboBox<>(model("gemini", "openai", "anthropic", "github"));
        ShaftUiLabels.applyFriendlyRenderer(cloudProvider);
        cloudProvider.getAccessibleContext().setAccessibleName("Assistant cloud provider");
        cloudProvider.getAccessibleContext().setAccessibleDescription("Cloud provider used by Assistant Ask and Plan prompts.");
        cloudModel = new JBTextField();
        cloudModel.getEmptyText().setText("Cloud model, for example gemini-3.5-flash");
        cloudModel.getAccessibleContext().setAccessibleName("Assistant cloud model");
        cloudModel.getAccessibleContext().setAccessibleDescription("Model name passed to the selected cloud provider.");
        defaultClient = new JComboBox<>(model("CODEX", "CLAUDE_CODE", "COPILOT_CLI"));
        ShaftUiLabels.applyFriendlyRenderer(defaultClient);
        defaultClient.getAccessibleContext().setAccessibleName("Default assistant provider");
        defaultClient.getAccessibleContext().setAccessibleDescription("Default assistant provider used when opening the assistant panel.");
        defaultMode = new JComboBox<>(model("ASK", "PLAN", "AGENT"));
        ShaftUiLabels.applyFriendlyRenderer(defaultMode);
        defaultMode.getAccessibleContext().setAccessibleName("Default assistant mode");
        defaultMode.getAccessibleContext().setAccessibleDescription("Default assistant mode used when opening the assistant panel.");
        advancedUiEnabled = new JBCheckBox("Enable advanced workflows and provider options");
        advancedUiEnabled.getAccessibleContext().setAccessibleName("Enable advanced SHAFT UI");
        advancedUiEnabled.getAccessibleContext().setAccessibleDescription(
                "Shows guided workflows, direct tool panels, cloud provider controls, and provider key forwarding.");
        advancedUiEnabled.addActionListener(event -> updateAgentConfigurationControls());
        pilotAiProvider = new JComboBox<>(model("none", "openai", "anthropic", "gemini", "github", "ollama"));
        ShaftUiLabels.applyFriendlyRenderer(pilotAiProvider);
        pilotAiProvider.getAccessibleContext().setAccessibleName("SHAFT AI provider");
        pilotAiProvider.getAccessibleContext().setAccessibleDescription(
                "Optional SHAFT AI provider used by MCP tools that request configured provider assistance.");
        pilotAiModel = new JBTextField();
        pilotAiModel.getEmptyText().setText("Provider model, for example gemini-3.5-flash");
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
        currentAgentConfigurationTitle = label("Current agent", 'C', currentAgentConfiguration);
        currentAgentConfigurationRow = agentConfigurationRow(currentAgentConfiguration, configureAgent);
        assistantProviderTypeLabel = label("Provider type", 'Y', assistantProviderType);
        assistantFamilyLabel = label("Family", 'F', assistantFamily);
        assistantRuntimeLabel = label("Runtime", 'R', assistantRuntime);
        cloudProviderLabel = label("Cloud provider", 'V', cloudProvider);
        cloudModelLabel = label("Cloud model", 'W', cloudModel);
        defaultModeLabel = label("Default assistant mode", 'D', defaultMode);
        shaftAiSection = section("Advanced");
        shaftAiProviderLabel = label("Provider", 'P', pilotAiProvider);
        shaftAiModelLabel = label("Model", 'L', pilotAiModel);
        providerKeysSection = section("Credentials");
        shaftAiHelp = help("Provider settings apply only to MCP tools that explicitly request configured SHAFT AI assistance.");
        providerKeysHelp = help("Passing keys exposes them only to the SHAFT MCP process. Disable to keep provider credentials local to IntelliJ only.");
        providerKeysStorageHelp = help("Provider keys are stored in IntelliJ Password Safe. Use 'Clear' only to remove a stored key.");
        openAiKeyLabel = label("OpenAI API key", 'O', openAiKey);
        anthropicKeyLabel = label("Anthropic API key", 'A', anthropicKey);
        geminiKeyLabel = label("Gemini API key", 'I', geminiKey);
        githubKeyLabel = label("GitHub API key", 'G', githubKey);

        panel = FormBuilder.createFormBuilder()
                .addComponent(section("Connection"))
                .addLabeledComponent(label("MCP stdio command", 'M', mcpCommand), mcpCommand)
                .addLabeledComponent(testMcp, testStatus)
                .addComponent(help("Visit the SHAFT MCP user guide, install the MCP integration, paste the stdio command, then test the connection."))
                .addComponent(section("Execution"))
                .addLabeledComponent(currentAgentConfigurationTitle, currentAgentConfigurationRow)
                .addLabeledComponent(assistantProviderTypeLabel, assistantProviderType)
                .addLabeledComponent(assistantFamilyLabel, assistantFamily)
                .addLabeledComponent(assistantRuntimeLabel, assistantRuntime)
                .addLabeledComponent(cloudProviderLabel, cloudProvider)
                .addLabeledComponent(cloudModelLabel, cloudModel)
                .addLabeledComponent(defaultModeLabel, defaultMode)
                .addComponent(advancedUiEnabled)
                .addComponent(help("The Assistant tab is always available. Agent mode still requires explicit source mutation approval per request."))
                .addComponent(shaftAiSection)
                .addLabeledComponent(shaftAiProviderLabel, pilotAiProvider)
                .addLabeledComponent(shaftAiModelLabel, pilotAiModel)
                .addComponent(shaftAiHelp)
                .addComponent(providerKeysSection)
                .addComponent(passProviderKeys)
                .addComponent(providerKeysHelp)
                .addComponent(providerKeysStorageHelp)
                .addLabeledComponent(openAiKeyLabel, openAiKey)
                .addComponent(keyRow(clearOpenAiKey, openAiKeyStatus))
                .addLabeledComponent(anthropicKeyLabel, anthropicKey)
                .addComponent(keyRow(clearAnthropicKey, anthropicKeyStatus))
                .addLabeledComponent(geminiKeyLabel, geminiKey)
                .addComponent(keyRow(clearGeminiKey, geminiKeyStatus))
                .addLabeledComponent(githubKeyLabel, githubKey)
                .addComponent(keyRow(clearGithubKey, githubKeyStatus))
                .addComponentFillVertically(new JPanel(), 0)
                .getPanel();
        panel.setBorder(JBUI.Borders.empty(8));
        reset();
        return panel;
    }

    @Override
    public boolean isModified() {
        ShaftSettingsState.Settings state = settingsProvider.get();
        boolean advancedSelected = advancedUiEnabled.isSelected();
        String selectedProviderType = advancedSelected ? String.valueOf(assistantProviderType.getSelectedItem()) : "LOCAL";
        String stateProviderType = state.advancedUiEnabled ? normalize(state.assistantProviderType, "LOCAL") : "LOCAL";
        return !Objects.equals(state.mcpCommand, mcpCommand.getText())
                || state.advancedUiEnabled != advancedSelected
                || !Objects.equals(stateProviderType, selectedProviderType)
                || !Objects.equals(resolveFamily(state), assistantFamily.getSelectedItem())
                || !Objects.equals(normalize(state.assistantRuntime, "CLI"), assistantRuntime.getSelectedItem())
                || !Objects.equals(normalizeLower(state.cloudProvider, "gemini"), cloudProvider.getSelectedItem())
                || !Objects.equals(state.cloudModel == null ? "" : state.cloudModel, cloudModel.getText())
                || !Objects.equals(state.defaultAutobotClient, clientFromFamily(String.valueOf(assistantFamily.getSelectedItem())))
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
            state.agentGuidanceOptimizationPromptPending = false;
        }
        state.mcpCommand = command;
        state.advancedUiEnabled = advancedUiEnabled.isSelected();
        state.assistantProviderType = state.advancedUiEnabled
                ? String.valueOf(assistantProviderType.getSelectedItem())
                : "LOCAL";
        state.assistantFamily = String.valueOf(assistantFamily.getSelectedItem());
        state.assistantRuntime = String.valueOf(assistantRuntime.getSelectedItem());
        state.cloudProvider = String.valueOf(cloudProvider.getSelectedItem());
        state.cloudModel = cloudModel.getText().trim();
        state.defaultAutobotClient = clientFromFamily(state.assistantFamily);
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
        editingAgentConfiguration = false;
        updateAgentConfigurationControls();
    }

    @Override
    public void reset() {
        ShaftSettingsState.Settings state = settingsProvider.get();
        mcpCommand.setText(state.mcpCommand);
        advancedUiEnabled.setSelected(state.advancedUiEnabled);
        assistantProviderType.setSelectedItem(normalize(state.assistantProviderType, "LOCAL"));
        assistantFamily.setSelectedItem(resolveFamily(state));
        assistantRuntime.setSelectedItem(normalize(state.assistantRuntime, "CLI"));
        cloudProvider.setSelectedItem(normalizeLower(state.cloudProvider, "gemini"));
        cloudModel.setText(state.cloudModel == null ? "" : state.cloudModel);
        defaultClient.setSelectedItem(clientFromFamily(resolveFamily(state)));
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
        editingAgentConfiguration = false;
        updateAgentConfigurationControls();
    }

    @Override
    public void disposeUIResources() {
        panel = null;
        mcpCommand = null;
        testMcp = null;
        testStatus = null;
        currentAgentConfigurationTitle = null;
        currentAgentConfiguration = null;
        currentAgentConfigurationRow = null;
        configureAgent = null;
        assistantProviderTypeLabel = null;
        assistantFamilyLabel = null;
        assistantRuntimeLabel = null;
        cloudProviderLabel = null;
        cloudModelLabel = null;
        assistantProviderType = null;
        assistantFamily = null;
        assistantRuntime = null;
        cloudProvider = null;
        cloudModel = null;
        defaultClient = null;
        defaultMode = null;
        pilotAiProvider = null;
        pilotAiModel = null;
        passProviderKeys = null;
        advancedUiEnabled = null;
        defaultModeLabel = null;
        shaftAiSection = null;
        shaftAiProviderLabel = null;
        shaftAiModelLabel = null;
        providerKeysSection = null;
        shaftAiHelp = null;
        providerKeysHelp = null;
        providerKeysStorageHelp = null;
        openAiKeyLabel = null;
        anthropicKeyLabel = null;
        geminiKeyLabel = null;
        githubKeyLabel = null;
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
        editingAgentConfiguration = false;
    }

    private static ComboBoxModel<String> model(String... values) {
        return new DefaultComboBoxModel<>(values);
    }

    private static String resolveFamily(ShaftSettingsState.Settings state) {
        String family = normalize(state.assistantFamily, "");
        if (!family.isBlank()) {
            return family;
        }
        return switch (normalize(state.defaultAutobotClient, "CODEX")) {
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

    private static String normalize(String value, String fallback) {
        String normalized = value == null || value.isBlank() ? fallback : value.trim();
        return normalized.toUpperCase(Locale.ROOT).replace('-', '_').replace(' ', '_');
    }

    private static String normalizeLower(String value, String fallback) {
        String normalized = value == null || value.isBlank() ? fallback : value.trim();
        return normalized.toLowerCase(Locale.ROOT);
    }

    private static JLabel section(String text) {
        JLabel label = new JLabel(text);
        label.setFont(label.getFont().deriveFont(java.awt.Font.BOLD, label.getFont().getSize2D() + 1f));
        label.setBorder(JBUI.Borders.emptyTop(10));
        label.getAccessibleContext().setAccessibleName(text + " settings section");
        return label;
    }

    private static JLabel help(String text) {
        JLabel label = new JLabel(text);
        label.setEnabled(false);
        return label;
    }

    private static JLabel statusLabel(String text) {
        // Swing's own disabled-label rendering (not a manual UIManager color lookup, which is
        // unreliable for "Label.disabledForeground" outside a fully initialized IDE look-and-feel)
        // gives a clean, theme-correct muted look for the idle state.
        return help(text);
    }

    private void resetTestStatus() {
        if (testStatus != null) {
            testStatus.setText("Not tested");
            testStatus.setEnabled(false);
        }
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
        ShaftIconButtons.apply(button, ShaftIcons.CLEAR);
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

    private static JPanel agentConfigurationRow(JLabel currentConfiguration, JButton configureButton) {
        JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        row.add(currentConfiguration);
        row.add(configureButton);
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
        statusLabel.setEnabled(true);
        statusLabel.setText("Testing...");
        statusLabel.setForeground(ShaftStatusPresentation.progress());
        String command = mcpCommand.getText() == null ? "" : mcpCommand.getText().trim();
        ShaftMcpConnectionProbe.test(command, formSettings()).whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> {
                    if (button == null || statusLabel == null || host == null) {
                        return;
                    }
                    button.setEnabled(true);
                    if (error != null) {
                        statusLabel.setText("Failed");
                        statusLabel.setForeground(ShaftStatusPresentation.error());
                        McpInvocationError category = McpInvocationError.categorize(error);
                        StringBuilder sb = new StringBuilder();
                        sb.append(category.message());
                        if (category.recoveryAction() != null) {
                            sb.append("\n\nRecovery: ").append(category.recoveryAction());
                        }
                        Messages.showErrorDialog(host, sb.toString(), "SHAFT MCP");
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
            statusLabel.setForeground(ShaftStatusPresentation.success());
            saveConnectedSettings();
            editingAgentConfiguration = false;
            updateAgentConfigurationControls();
        } else {
            statusLabel.setText("Failed");
            statusLabel.setForeground(ShaftStatusPresentation.error());
            String message = formatErrorMessage(result);
            Messages.showErrorDialog(host, message, "SHAFT MCP");
        }
    }

    private String formatErrorMessage(ShaftMcpToolResult result) {
        if (result == null) {
            return "No result returned.";
        }
        if (result.errorCategory() != null) {
            StringBuilder sb = new StringBuilder();
            sb.append(result.errorCategory().message());
            if (result.recoveryAction() != null) {
                sb.append("\n\nRecovery: ").append(result.recoveryAction());
            }
            return sb.toString();
        }
        return result.output();
    }

    private ShaftSettingsState.Settings formSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = mcpCommand.getText() == null ? "" : mcpCommand.getText().trim();
        settings.advancedUiEnabled = advancedUiEnabled.isSelected();
        settings.assistantProviderType = settings.advancedUiEnabled
                ? String.valueOf(assistantProviderType.getSelectedItem())
                : "LOCAL";
        settings.assistantFamily = String.valueOf(assistantFamily.getSelectedItem());
        settings.assistantRuntime = String.valueOf(assistantRuntime.getSelectedItem());
        settings.cloudProvider = String.valueOf(cloudProvider.getSelectedItem());
        settings.cloudModel = cloudModel.getText() == null ? "" : cloudModel.getText().trim();
        settings.defaultAutobotClient = clientFromFamily(settings.assistantFamily);
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
        state.advancedUiEnabled = form.advancedUiEnabled;
        state.assistantProviderType = form.assistantProviderType;
        state.assistantFamily = form.assistantFamily;
        state.assistantRuntime = form.assistantRuntime;
        state.cloudProvider = form.cloudProvider;
        state.cloudModel = form.cloudModel;
        state.defaultAutobotClient = form.defaultAutobotClient;
        state.defaultAutobotMode = form.defaultAutobotMode;
        state.pilotAiProvider = form.pilotAiProvider;
        state.pilotAiModel = form.pilotAiModel;
        state.passProviderApiKeysToMcp = form.passProviderApiKeysToMcp;
        state.mcpSetupComplete = true;
    }

    private void updateAgentConfigurationControls() {
        if (currentAgentConfiguration == null) {
            return;
        }
        ShaftSettingsState.Settings state = settingsProvider.get();
        currentAgentConfiguration.setText(currentAgentConfigurationText(state));
        boolean advanced = advancedUiEnabled != null && advancedUiEnabled.isSelected();
        if (!advanced && "CLOUD".equals(assistantProviderType.getSelectedItem())) {
            assistantProviderType.setSelectedItem("LOCAL");
        }
        boolean showSummary = mcpReady(state) && !editingAgentConfiguration;
        boolean cloud = advanced && "CLOUD".equals(assistantProviderType.getSelectedItem());
        currentAgentConfigurationTitle.setVisible(showSummary);
        currentAgentConfigurationRow.setVisible(showSummary);
        currentAgentConfiguration.setVisible(showSummary);
        configureAgent.setVisible(showSummary);
        assistantProviderTypeLabel.setVisible(advanced && !showSummary);
        assistantProviderType.setVisible(advanced && !showSummary);
        assistantFamilyLabel.setVisible(!showSummary);
        assistantFamily.setVisible(!showSummary);
        assistantRuntimeLabel.setVisible(!showSummary);
        assistantRuntime.setVisible(!showSummary);
        cloudProviderLabel.setVisible(cloud && !showSummary);
        cloudProvider.setVisible(cloud && !showSummary);
        cloudModelLabel.setVisible(cloud && !showSummary);
        cloudModel.setVisible(cloud && !showSummary);
        defaultModeLabel.setVisible(advanced && !showSummary);
        defaultMode.setVisible(advanced && !showSummary);
        setVisible(advanced, shaftAiSection, shaftAiProviderLabel, shaftAiModelLabel, shaftAiHelp,
                providerKeysSection, providerKeysHelp, providerKeysStorageHelp, openAiKeyLabel,
                anthropicKeyLabel, geminiKeyLabel, githubKeyLabel, openAiKeyStatus, anthropicKeyStatus,
                geminiKeyStatus, githubKeyStatus);
        pilotAiProvider.setVisible(advanced);
        pilotAiModel.setVisible(advanced);
        passProviderKeys.setVisible(advanced);
        openAiKey.setVisible(advanced);
        anthropicKey.setVisible(advanced);
        geminiKey.setVisible(advanced);
        githubKey.setVisible(advanced);
        clearOpenAiKey.setVisible(advanced);
        clearAnthropicKey.setVisible(advanced);
        clearGeminiKey.setVisible(advanced);
        clearGithubKey.setVisible(advanced);
    }

    private static void setVisible(boolean visible, JComponent... components) {
        for (JComponent component : components) {
            component.setVisible(visible);
        }
    }

    private static boolean mcpReady(ShaftSettingsState.Settings state) {
        return state != null && state.mcpReady();
    }

    private static String currentAgentConfigurationText(ShaftSettingsState.Settings state) {
        if (state.advancedUiEnabled && "CLOUD".equals(normalize(state.assistantProviderType, "LOCAL"))) {
            String model = state.cloudModel == null || state.cloudModel.isBlank() ? "" : " / " + state.cloudModel.trim();
            return "Agent: Cloud / " + ShaftUiLabels.friendly(normalizeLower(state.cloudProvider, "gemini")) + model;
        }
        return "Agent: Local / " + ShaftUiLabels.friendly(resolveFamily(state))
                + " / " + ShaftUiLabels.friendly(normalize(state.assistantRuntime, "CLI"));
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
