package com.shaft.intellij.settings;

import com.intellij.ide.DataManager;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.FormBuilder;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.McpInvocationError;
import com.shaft.intellij.mcp.RecoveryActions;
import com.shaft.intellij.mcp.ShaftMcpConnectionProbe;
import com.shaft.intellij.mcp.ShaftMcpInvocationService;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.mcp.ShaftPluginExecutor;
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
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.function.Supplier;

/**
 * Settings page for SHAFT IntelliJ integration.
 */
public final class ShaftSettingsConfigurable implements SearchableConfigurable {
    private java.util.function.Function<String, String> providerEnvironmentLookup = System::getenv;
    private static final String OPENAI_PROVIDER_KEY = "OPENAI_API_KEY";
    private static final String ANTHROPIC_PROVIDER_KEY = "ANTHROPIC_API_KEY";
    private static final String GEMINI_PROVIDER_KEY = "GEMINI_API_KEY";
    private static final String GITHUB_PROVIDER_KEY = "GITHUB_TOKEN";
    private static final String TEST_MCP_TOOLTIP = "Test MCP";
    private static final String TESTING_MCP_TOOLTIP = "Testing...";
    private static final String CUSTOM_PROPERTIES_RELATIVE_PATH = "src/main/resources/properties/custom.properties";
    private static final String TARGET_BROWSER_NAME_KEY = "targetBrowserName";
    private static final String HEADLESS_EXECUTION_KEY = "headlessExecution";
    private static final String TEST_EXECUTION_BROWSER_DEFAULT = "(use SHAFT default: chrome)";
    private static final String[] TEST_EXECUTION_BROWSERS =
            {TEST_EXECUTION_BROWSER_DEFAULT, "chrome", "firefox", "edge", "safari"};

    private final Supplier<ShaftSettingsState.Settings> settingsProvider;
    private final Supplier<CredentialAccess> credentialsProvider;
    /** Test-only seam (issue #3665 part A): overrides {@link #resolveProjectRoot()} so unit tests can
     * point the Test Execution section's custom.properties read/write at a temp directory instead of
     * touching the real project tree. {@code null} in production, where {@link #resolveProject()}'s
     * real {@code Project} lookup is used, unchanged. */
    private final Supplier<Path> projectRootProvider;
    private JPanel panel;
    private JBTextField mcpCommand;
    private JBCheckBox mcpCommandManualEdit;
    /** Whether {@code state.mcpCommand} already carried a wizard-set value as of the last
     * {@link #reset()} (issue #3601 B2.1): gates {@link #mcpCommand} read-only until the user opts
     * in via {@link #mcpCommandManualEdit}, so a fresh install (nothing to protect yet) is left
     * directly editable as before. */
    private boolean mcpCommandManaged;
    private JButton testMcp;
    private JLabel testStatus;
    private JLabel testRecovery;
    private JButton testRecoveryAction;
    private JLabel currentAgentConfigurationTitle;
    private JLabel currentAgentConfiguration;
    /**
     * Bordered chip wrapping {@link #currentAgentConfiguration} + {@link #configureAgent} together
     * so the "which agent is this / configure it" pair reads as one grouped unit in the "Current
     * agent" form row, instead of a plain unbordered value label sitting directly against a boxed
     * icon button (issue #4322, an adjacent finding from #4316/PR #4320). Mirrors the {@code
     * currentAgentChip} idiom {@code ShaftAssistantPanel} uses for the same label/gear pair in its
     * {@code routeRow}, with the same neutral informational tint ({@link
     * ShaftStatusPresentation#progress()}). Visibility mirrors both wrapped controls' shared
     * {@code showSummary} gate in {@link #updateAgentConfigurationControls()}.
     */
    private JPanel currentAgentChip;
    private JButton configureAgent;
    private JLabel assistantProviderTypeLabel;
    private JLabel assistantAgentLabel;
    private JLabel assistantFamilyLabel;
    private JLabel assistantRuntimeLabel;
    private JLabel cloudProviderLabel;
    private JLabel cloudModelLabel;
    private JLabel cloudCredentialSourceLabel;
    private JLabel defaultModeLabel;
    private JLabel shaftAiSection;
    private JLabel shaftAiProviderLabel;
    private JLabel shaftAiModelLabel;
    private JLabel shaftAiEndpointLabel;
    private JLabel pilotCredentialSourceLabel;
    private JLabel providerKeysSection;
    private JLabel shaftAiHelp;
    private JLabel providerKeysHelp;
    private JLabel providerKeysStorageHelp;
    private JLabel openAiKeyLabel;
    private JLabel anthropicKeyLabel;
    private JLabel geminiKeyLabel;
    private JLabel githubKeyLabel;
    private JLabel lmStudioKeyLabel;
    private JLabel ollamaKeyLabel;
    private JComboBox<String> assistantProviderType;
    private JComboBox<AssistantAgentRoute> assistantAgent;
    private JComboBox<String> assistantFamily;
    private JComboBox<String> assistantRuntime;
    private JComboBox<String> cloudProvider;
    private JBTextField cloudModel;
    private JComboBox<String> cloudCredentialSource;
    private JComboBox<String> defaultClient;
    private JComboBox<String> defaultMode;
    private JComboBox<String> pilotAiProvider;
    private JBTextField pilotAiModel;
    private JBTextField pilotAiEndpoint;
    private JComboBox<String> pilotCredentialSource;
    private JBCheckBox passProviderKeys;
    private JBCheckBox advancedUiEnabled;
    private JBCheckBox watchModeEnabled;
    private JLabel testExecutionSection;
    private JBCheckBox overrideExecutionProperties;
    private JLabel targetBrowserNameLabel;
    private JComboBox<String> targetBrowserName;
    private JBCheckBox headlessExecution;
    private JLabel testExecutionHelp;
    private JPasswordField openAiKey;
    private JPasswordField anthropicKey;
    private JPasswordField geminiKey;
    private JPasswordField githubKey;
    private JPasswordField lmStudioKey;
    private JPasswordField ollamaKey;
    private JButton clearOpenAiKey;
    private JButton clearAnthropicKey;
    private JButton clearGeminiKey;
    private JButton clearGithubKey;
    private JButton clearLmStudioKey;
    private JButton clearOllamaKey;
    private JButton testOpenAiKey;
    private JButton testAnthropicKey;
    private JButton testGeminiKey;
    private JButton testGithubKey;
    private JLabel openAiKeyStatus;
    private JLabel anthropicKeyStatus;
    private JLabel geminiKeyStatus;
    private JLabel githubKeyStatus;
    private JLabel lmStudioKeyStatus;
    private JLabel ollamaKeyStatus;
    private boolean openAiClearRequested;
    private boolean anthropicClearRequested;
    private boolean geminiClearRequested;
    private boolean githubClearRequested;
    private boolean lmStudioClearRequested;
    private boolean ollamaClearRequested;
    private boolean editingAgentConfiguration;
    private boolean testMcpInFlight;

    /**
     * Creates a settings page backed by IntelliJ persistent services.
     */
    public ShaftSettingsConfigurable() {
        this(() -> ShaftSettingsState.getInstance().getState(), ShaftSettingsConfigurable::credentialAccess, null);
    }

    ShaftSettingsConfigurable(ShaftSettingsState.Settings settings, CredentialAccess credentials) {
        this(() -> settings, () -> credentials, null);
    }

    /** Test-only overload (issue #3665 part A) that injects {@link #projectRootProvider}. */
    ShaftSettingsConfigurable(ShaftSettingsState.Settings settings, CredentialAccess credentials,
                               Supplier<Path> projectRootProvider) {
        this(() -> settings, () -> credentials, projectRootProvider);
    }

    private ShaftSettingsConfigurable(Supplier<ShaftSettingsState.Settings> settingsProvider,
                                      Supplier<CredentialAccess> credentialsProvider,
                                      Supplier<Path> projectRootProvider) {
        this.settingsProvider = settingsProvider;
        this.credentialsProvider = credentialsProvider;
        this.projectRootProvider = projectRootProvider;
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
        mcpCommandManualEdit = new JBCheckBox("Edit manually");
        mcpCommandManualEdit.getAccessibleContext().setAccessibleName("Edit MCP command manually");
        mcpCommandManualEdit.setToolTipText(
                "Edit the wizard-configured MCP command directly when it is not correct");
        mcpCommandManualEdit.addActionListener(event -> updateMcpCommandEditableState());
        testMcp = new JButton("Test MCP");
        testMcp.getAccessibleContext().setAccessibleName("Test MCP");
        testMcp.getAccessibleContext().setAccessibleDescription(
                "Run a one-time SHAFT MCP connection check with current settings.");
        ShaftIconButtons.apply(testMcp, ShaftIcons.SEND);
        testMcp.addActionListener(event -> testMcpConnection());
        testStatus = statusLabel("Not tested");
        testStatus.getAccessibleContext().setAccessibleName("SHAFT MCP test status");
        testRecovery = new JLabel();
        testRecovery.getAccessibleContext().setAccessibleName("SHAFT MCP test recovery");
        testRecovery.setVisible(false);
        testRecoveryAction = new JButton();
        testRecoveryAction.getAccessibleContext().setAccessibleName("SHAFT MCP test recovery action");
        // Not run through ShaftIconButtons.apply(): that fixes a button to an icon-only 32x32 slot,
        // but this button's whole point is to show which recovery action applies ("Retry" / "Restart
        // MCP server" / "View logs" from configureRecoveryAction()) -- an icon alone can't convey
        // that (#3626).
        testRecoveryAction.setIcon(ShaftIcons.RERUN);
        testRecoveryAction.setVisible(false);
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
        overrideExecutionProperties = new JBCheckBox("Override SHAFT execution properties");
        overrideExecutionProperties.getAccessibleContext().setAccessibleName("Override SHAFT execution properties");
        overrideExecutionProperties.getAccessibleContext().setAccessibleDescription(
                "When checked, the browser and headless selections below are written to this project's "
                        + "custom.properties; when unchecked, SHAFT's own defaults apply.");
        overrideExecutionProperties.addActionListener(event -> updateTestExecutionControlsEnabled());
        targetBrowserName = new JComboBox<>(model(TEST_EXECUTION_BROWSERS));
        targetBrowserName.getAccessibleContext().setAccessibleName("Test execution browser");
        targetBrowserName.getAccessibleContext().setAccessibleDescription(
                "Sets targetBrowserName in this project's custom.properties.");
        targetBrowserNameLabel = label("Browser", 'B', targetBrowserName);
        headlessExecution = new JBCheckBox("Headless");
        headlessExecution.getAccessibleContext().setAccessibleName("Test execution headless");
        headlessExecution.getAccessibleContext().setAccessibleDescription(
                "Sets headlessExecution in this project's custom.properties.");
        testExecutionSection = section("Test Execution");
        testExecutionHelp = help("Overrides are written to this project's " + CUSTOM_PROPERTIES_RELATIVE_PATH
                + ". Uncheck to remove the overrides and inherit SHAFT's own defaults.");

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
        assistantAgent = new JComboBox<>(AssistantAgentRoute.values());
        assistantAgent.getAccessibleContext().setAccessibleName("Assistant agent");
        assistantAgent.getAccessibleContext().setAccessibleDescription(
                "Select the agent route used by SHAFT Assistant and MCP setup.");
        assistantAgent.addActionListener(event -> syncLegacyAgentControls());
        assistantFamily = new JComboBox<>(model("CODEX", "CLAUDE", "COPILOT", "GROK"));
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
        cloudProvider.addActionListener(event -> {
            String provider = String.valueOf(cloudProvider.getSelectedItem());
            updateCredentialSources(cloudCredentialSource, provider,
                    settingsProvider.get().providerApiKeyEnvironmentVariable(provider));
        });
        cloudModel = new JBTextField();
        cloudModel.getEmptyText().setText("Cloud model, for example gemini-3.5-flash");
        cloudModel.getAccessibleContext().setAccessibleName("Assistant cloud model");
        cloudModel.getAccessibleContext().setAccessibleDescription("Model name passed to the selected cloud provider.");
        cloudCredentialSource = new JComboBox<>();
        cloudCredentialSource.getAccessibleContext().setAccessibleName("Assistant cloud credential source");
        cloudCredentialSource.getAccessibleContext().setAccessibleDescription(
                "Use a provider-standard environment variable without storing its secret value, or use Password Safe.");
        defaultClient = new JComboBox<>(model("CODEX", "CLAUDE_CODE", "COPILOT_CLI", "GROK"));
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
        watchModeEnabled = new JBCheckBox("Enable watch mode (rerun the last test on source save)");
        watchModeEnabled.getAccessibleContext().setAccessibleName("Enable SHAFT watch mode");
        watchModeEnabled.getAccessibleContext().setAccessibleDescription(
                "Reruns the last SHAFT test run configuration when a src/test/ file changes, "
                        + "throttled to at most 6 reruns per rolling 5-minute window.");
        pilotAiProvider = new JComboBox<>(model("none", "openai", "anthropic", "gemini", "github", "lmstudio", "ollama"));
        ShaftUiLabels.applyFriendlyRenderer(pilotAiProvider);
        pilotAiProvider.getAccessibleContext().setAccessibleName("SHAFT AI provider");
        pilotAiProvider.getAccessibleContext().setAccessibleDescription(
                "Optional SHAFT AI provider used by MCP tools that request configured provider assistance.");
        pilotAiModel = new JBTextField();
        pilotAiModel.getEmptyText().setText("Provider model, for example gemini-3.5-flash");
        pilotAiModel.getAccessibleContext().setAccessibleName("SHAFT AI provider model");
        pilotAiModel.getAccessibleContext().setAccessibleDescription("Model name passed to SHAFT MCP provider tools.");
        pilotAiEndpoint = new JBTextField();
        pilotAiEndpoint.getEmptyText().setText("Ollama: http://127.0.0.1:11434/api/chat");
        pilotAiEndpoint.getAccessibleContext().setAccessibleName("SHAFT AI local endpoint");
        pilotAiEndpoint.getAccessibleContext().setAccessibleDescription(
                "Absolute HTTP(S) endpoint for the selected LM Studio or Ollama provider.");
        pilotAiProvider.addActionListener(event -> {
            String provider = String.valueOf(pilotAiProvider.getSelectedItem());
            pilotAiEndpoint.setText(settingsProvider.get().localEndpointFor(provider));
            updateCredentialSources(pilotCredentialSource, provider,
                    settingsProvider.get().providerApiKeyEnvironmentVariable(provider));
        });
        pilotCredentialSource = new JComboBox<>();
        pilotCredentialSource.getAccessibleContext().setAccessibleName("SHAFT AI credential source");
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
        lmStudioKey = new JPasswordField();
        lmStudioKey.getAccessibleContext().setAccessibleName("LM Studio API key");
        lmStudioKey.getAccessibleContext().setAccessibleDescription("Optional local gateway key stored in Password Safe.");
        ollamaKey = new JPasswordField();
        ollamaKey.getAccessibleContext().setAccessibleName("Ollama API key");
        ollamaKey.getAccessibleContext().setAccessibleDescription("Optional local gateway key stored in Password Safe.");
        openAiKeyStatus = keyStatusLabel("OpenAI");
        anthropicKeyStatus = keyStatusLabel("Anthropic");
        geminiKeyStatus = keyStatusLabel("Gemini");
        githubKeyStatus = keyStatusLabel("GitHub");
        lmStudioKeyStatus = keyStatusLabel("LM Studio");
        ollamaKeyStatus = keyStatusLabel("Ollama");
        clearOpenAiKey = new JButton("Clear");
        configureClearButton(clearOpenAiKey, "Clear stored OpenAI API key", openAiKey, openAiKeyStatus, () -> openAiClearRequested = true);
        testOpenAiKey = new JButton("Test");
        ShaftIconButtons.apply(testOpenAiKey, "Test OpenAI API key", "Test OpenAI API key", ShaftIcons.CHECK);
        testOpenAiKey.addActionListener(event -> testProviderKey(
                OPENAI_PROVIDER_KEY, openAiKey, openAiKeyStatus, testOpenAiKey, "OpenAI", ProviderKeyProbe::testOpenAi));
        clearAnthropicKey = new JButton("Clear");
        configureClearButton(clearAnthropicKey, "Clear stored Anthropic API key", anthropicKey, anthropicKeyStatus, () -> anthropicClearRequested = true);
        testAnthropicKey = new JButton("Test");
        ShaftIconButtons.apply(testAnthropicKey, "Test Anthropic API key", "Test Anthropic API key", ShaftIcons.CHECK);
        testAnthropicKey.addActionListener(event -> testProviderKey(
                ANTHROPIC_PROVIDER_KEY, anthropicKey, anthropicKeyStatus, testAnthropicKey, "Anthropic", ProviderKeyProbe::testAnthropic));
        clearGeminiKey = new JButton("Clear");
        configureClearButton(clearGeminiKey, "Clear stored Gemini API key", geminiKey, geminiKeyStatus, () -> geminiClearRequested = true);
        testGeminiKey = new JButton("Test");
        ShaftIconButtons.apply(testGeminiKey, "Test Gemini API key", "Test Gemini API key", ShaftIcons.CHECK);
        testGeminiKey.addActionListener(event -> testProviderKey(
                GEMINI_PROVIDER_KEY, geminiKey, geminiKeyStatus, testGeminiKey, "Gemini", ProviderKeyProbe::testGemini));
        clearGithubKey = new JButton("Clear");
        configureClearButton(clearGithubKey, "Clear stored GitHub API key", githubKey, githubKeyStatus, () -> githubClearRequested = true);
        clearLmStudioKey = new JButton("Clear");
        configureClearButton(clearLmStudioKey, "Clear stored LM Studio API key", lmStudioKey, lmStudioKeyStatus,
                () -> lmStudioClearRequested = true);
        clearOllamaKey = new JButton("Clear");
        configureClearButton(clearOllamaKey, "Clear stored Ollama API key", ollamaKey, ollamaKeyStatus,
                () -> ollamaClearRequested = true);
        testGithubKey = new JButton("Test");
        ShaftIconButtons.apply(testGithubKey, "Test GitHub API key", "Test GitHub API key", ShaftIcons.CHECK);
        testGithubKey.addActionListener(event -> testProviderKey(
                GITHUB_PROVIDER_KEY, githubKey, githubKeyStatus, testGithubKey, "GitHub", ProviderKeyProbe::testGithub));
        currentAgentConfigurationTitle = label("Current agent", 'C', currentAgentConfiguration);
        currentAgentChip = agentConfigurationRow(currentAgentConfiguration, configureAgent);
        assistantProviderTypeLabel = label("Provider type", 'Y', assistantProviderType);
        assistantAgentLabel = label("Agent", 'T', assistantAgent);
        assistantFamilyLabel = label("Family", 'F', assistantFamily);
        assistantRuntimeLabel = label("Runtime", 'R', assistantRuntime);
        cloudProviderLabel = label("Cloud provider", 'V', cloudProvider);
        cloudModelLabel = label("Cloud model", 'W', cloudModel);
        cloudCredentialSourceLabel = label("Credential source", 'U', cloudCredentialSource);
        defaultModeLabel = label("Default assistant mode", 'D', defaultMode);
        shaftAiSection = section("Advanced");
        shaftAiProviderLabel = label("Provider for Doctor/Healer AI features", 'P', pilotAiProvider);
        shaftAiEndpointLabel = label("Local endpoint", 'E', pilotAiEndpoint);
        pilotCredentialSourceLabel = label("Credential source", 'Q', pilotCredentialSource);
        shaftAiModelLabel = label("Model", 'L', pilotAiModel);
        providerKeysSection = section("Credentials");
        shaftAiHelp = help("This provider powers only SHAFT MCP's own AI-assisted tools, such as Doctor/Healer diagnosis, separate and independent from the Assistant's cloud provider selection above.");
        providerKeysHelp = help("Passing keys exposes them only to the SHAFT MCP process. Disable to keep provider credentials local to IntelliJ only.");
        providerKeysStorageHelp = help("Provider keys are stored in IntelliJ Password Safe. Use 'Clear' only to remove a stored key.");
        openAiKeyLabel = label("OpenAI API key", 'O', openAiKey);
        anthropicKeyLabel = label("Anthropic API key", 'A', anthropicKey);
        geminiKeyLabel = label("Gemini API key", 'I', geminiKey);
        githubKeyLabel = label("GitHub API key", 'G', githubKey);
        lmStudioKeyLabel = label("LM Studio API key", 'S', lmStudioKey);
        ollamaKeyLabel = label("Ollama API key", 'K', ollamaKey);

        panel = FormBuilder.createFormBuilder()
                .addComponent(section("Connection"))
                .addLabeledComponent(label("MCP stdio command", 'M', mcpCommand), mcpCommand)
                .addComponent(mcpCommandManualEdit)
                .addLabeledComponent(testMcp, testStatus)
                .addComponent(testRecoveryRow(testRecovery, testRecoveryAction))
                .addComponent(help("Visit the SHAFT MCP user guide, install the MCP integration, paste the stdio command, then test the connection."))
                .addComponent(testExecutionSection)
                .addComponent(overrideExecutionProperties)
                .addLabeledComponent(targetBrowserNameLabel, targetBrowserName)
                .addComponent(headlessExecution)
                .addComponent(testExecutionHelp)
                .addComponent(section("Execution"))
                .addLabeledComponent(currentAgentConfigurationTitle, currentAgentChip)
                .addLabeledComponent(assistantAgentLabel, assistantAgent)
                .addLabeledComponent(assistantProviderTypeLabel, assistantProviderType)
                .addLabeledComponent(assistantFamilyLabel, assistantFamily)
                .addLabeledComponent(assistantRuntimeLabel, assistantRuntime)
                .addLabeledComponent(cloudProviderLabel, cloudProvider)
                .addLabeledComponent(cloudModelLabel, cloudModel)
                .addLabeledComponent(cloudCredentialSourceLabel, cloudCredentialSource)
                .addLabeledComponent(defaultModeLabel, defaultMode)
                .addComponent(advancedUiEnabled)
                .addComponent(help("The Assistant tab is always available. Agent mode still requires explicit source mutation approval per request."))
                .addComponent(watchModeEnabled)
                .addComponent(shaftAiSection)
                .addLabeledComponent(shaftAiProviderLabel, pilotAiProvider)
                .addLabeledComponent(shaftAiEndpointLabel, pilotAiEndpoint)
                .addLabeledComponent(shaftAiModelLabel, pilotAiModel)
                .addLabeledComponent(pilotCredentialSourceLabel, pilotCredentialSource)
                .addComponent(shaftAiHelp)
                .addComponent(providerKeysSection)
                .addComponent(passProviderKeys)
                .addComponent(providerKeysHelp)
                .addComponent(providerKeysStorageHelp)
                .addLabeledComponent(openAiKeyLabel, openAiKey)
                .addComponent(keyRow(clearOpenAiKey, testOpenAiKey, openAiKeyStatus))
                .addLabeledComponent(anthropicKeyLabel, anthropicKey)
                .addComponent(keyRow(clearAnthropicKey, testAnthropicKey, anthropicKeyStatus))
                .addLabeledComponent(geminiKeyLabel, geminiKey)
                .addComponent(keyRow(clearGeminiKey, testGeminiKey, geminiKeyStatus))
                .addLabeledComponent(githubKeyLabel, githubKey)
                .addComponent(keyRow(clearGithubKey, testGithubKey, githubKeyStatus))
                .addLabeledComponent(lmStudioKeyLabel, lmStudioKey)
                .addComponent(keyRow(clearLmStudioKey, null, lmStudioKeyStatus))
                .addLabeledComponent(ollamaKeyLabel, ollamaKey)
                .addComponent(keyRow(clearOllamaKey, null, ollamaKeyStatus))
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
        AssistantAgentRoute selectedRoute = selectedAgentRoute();
        String selectedProviderType = advancedSelected
                ? String.valueOf(assistantProviderType.getSelectedItem())
                : (selectedRoute == null
                        ? normalize(state.assistantProviderType, "LOCAL")
                        : selectedRoute.providerType());
        String stateProviderType = normalize(state.assistantProviderType, "LOCAL");
        String cloudProviderName = String.valueOf(cloudProvider.getSelectedItem());
        String pilotProviderName = String.valueOf(pilotAiProvider.getSelectedItem());
        boolean cloudRouteActive = "CLOUD".equalsIgnoreCase(selectedProviderType);
        boolean cloudSourceModified = cloudRouteActive && !Objects.equals(
                state.providerApiKeyEnvironmentVariable(cloudProviderName), selectedCloudEnvironmentVariable());
        boolean pilotSourceModified = (!cloudRouteActive || !pilotProviderName.equals(cloudProviderName))
                && !Objects.equals(state.providerApiKeyEnvironmentVariable(pilotProviderName),
                selectedPilotEnvironmentVariable());
        return !Objects.equals(state.mcpCommand, mcpCommand.getText())
                || state.advancedUiEnabled != advancedSelected
                || state.watchModeEnabled != watchModeEnabled.isSelected()
                || AssistantAgentRoute.fromSettings(state) != selectedRoute
                || !Objects.equals(stateProviderType, selectedProviderType)
                || !Objects.equals(normalizeLower(state.cloudProvider, "gemini"), cloudProvider.getSelectedItem())
                || !Objects.equals(state.cloudModel == null ? "" : state.cloudModel, cloudModel.getText())
                || cloudSourceModified
                || !Objects.equals(state.defaultAutobotMode, defaultMode.getSelectedItem())
                || !Objects.equals(state.pilotAiProvider, pilotAiProvider.getSelectedItem())
                || !Objects.equals(state.pilotAiModel, pilotAiModel.getText())
                || pilotSourceModified
                || !Objects.equals(state.localEndpointFor(String.valueOf(pilotAiProvider.getSelectedItem())), pilotAiEndpoint.getText())
                || state.passProviderApiKeysToMcp != passProviderKeys.isSelected()
                || openAiClearRequested
                || anthropicClearRequested
                || geminiClearRequested
                || githubClearRequested
                || lmStudioClearRequested
                || ollamaClearRequested
                || hasPassword(openAiKey)
                || hasPassword(anthropicKey)
                || hasPassword(geminiKey)
                || hasPassword(githubKey)
                || hasPassword(lmStudioKey)
                || hasPassword(ollamaKey)
                || testExecutionModified();
    }

    @Override
    public void apply() throws ConfigurationException {
        String provider = String.valueOf(pilotAiProvider.getSelectedItem());
        String endpoint = pilotAiEndpoint.getText();
        validateLocalEndpoint(provider, endpoint);
        ShaftSettingsState.Settings state = settingsProvider.get();
        String command = mcpCommand.getText().trim();
        boolean routeChanged = AssistantAgentRoute.fromSettings(state) != selectedAgentRoute();
        if (!Objects.equals(state.mcpCommand, command) || routeChanged) {
            state.mcpSetupComplete = false;
            state.agentLaneReady = false;
            state.agentGuidanceOptimizationPromptPending = false;
        }
        state.mcpCommand = command;
        state.advancedUiEnabled = advancedUiEnabled.isSelected();
        state.watchModeEnabled = watchModeEnabled.isSelected();
        AssistantAgentRoute selectedRoute = selectedAgentRoute();
        if (selectedRoute != null) {
            selectedRoute.applyTo(state);
            state.assistantProviderType = state.advancedUiEnabled
                    ? String.valueOf(assistantProviderType.getSelectedItem())
                    : selectedRoute.providerType();
        }
        state.cloudProvider = String.valueOf(cloudProvider.getSelectedItem());
        state.cloudModel = cloudModel.getText().trim();
        boolean cloudRouteActive = "CLOUD".equalsIgnoreCase(state.assistantProviderType);
        if (cloudRouteActive) {
            state.setProviderApiKeyEnvironmentVariable(state.cloudProvider, selectedCloudEnvironmentVariable());
        }
        state.defaultAutobotMode = String.valueOf(defaultMode.getSelectedItem());
        state.pilotAiProvider = provider;
        state.pilotAiModel = pilotAiModel.getText().trim();
        if (!cloudRouteActive || !provider.equals(state.cloudProvider)) {
            state.setProviderApiKeyEnvironmentVariable(provider, selectedPilotEnvironmentVariable());
        }
        saveLocalEndpoint(state, provider, endpoint);
        state.passProviderApiKeysToMcp = passProviderKeys.isSelected();

        CredentialAccess credentials = credentialsProvider.get();
        CompletableFuture<Void> openAiFuture = applyCredentialChange(credentials, OPENAI_PROVIDER_KEY, openAiKey, openAiClearRequested);
        CompletableFuture<Void> anthropicFuture = applyCredentialChange(credentials, ANTHROPIC_PROVIDER_KEY, anthropicKey, anthropicClearRequested);
        CompletableFuture<Void> geminiFuture = applyCredentialChange(credentials, GEMINI_PROVIDER_KEY, geminiKey, geminiClearRequested);
        CompletableFuture<Void> githubFuture = applyCredentialChange(credentials, GITHUB_PROVIDER_KEY, githubKey, githubClearRequested);
        CompletableFuture<Void> lmStudioFuture = applyCredentialChange(credentials, "LMSTUDIO_API_KEY", lmStudioKey, lmStudioClearRequested);
        CompletableFuture<Void> ollamaFuture = applyCredentialChange(credentials, "OLLAMA_API_KEY", ollamaKey, ollamaClearRequested);
        // Deliberately not thenRunAsync with an extra executor here (issue #3623): each input future
        // already hops onto the EDT internally (ShaftCredentialService.*Async completes on the EDT;
        // the test fake's pre-completed futures run inline). A second executor hop would be redundant.
        CompletableFuture.allOf(openAiFuture, anthropicFuture, geminiFuture, githubFuture, lmStudioFuture, ollamaFuture)
                .thenRun(() -> updateStoredKeyStatus(credentials));
        openAiClearRequested = false;
        anthropicClearRequested = false;
        geminiClearRequested = false;
        githubClearRequested = false;
        lmStudioClearRequested = false;
        ollamaClearRequested = false;
        lmStudioClearRequested = false;
        ollamaClearRequested = false;
        editingAgentConfiguration = false;
        updateAgentConfigurationControls();
        applyTestExecutionOverrides();
    }

    @Override
    public void reset() {
        ShaftSettingsState.Settings state = settingsProvider.get();
        mcpCommand.setText(state.mcpCommand);
        mcpCommandManaged = state.mcpCommand != null && !state.mcpCommand.isBlank();
        mcpCommandManualEdit.setSelected(false);
        updateMcpCommandEditableState();
        advancedUiEnabled.setSelected(state.advancedUiEnabled);
        watchModeEnabled.setSelected(state.watchModeEnabled);
        assistantAgent.setSelectedItem(AssistantAgentRoute.fromSettings(state));
        assistantProviderType.setSelectedItem(normalize(state.assistantProviderType, "LOCAL"));
        assistantFamily.setSelectedItem(resolveFamily(state));
        assistantRuntime.setSelectedItem(normalize(state.assistantRuntime, "CLI"));
        cloudProvider.setSelectedItem(normalizeLower(state.cloudProvider, "gemini"));
        cloudModel.setText(state.cloudModel == null ? "" : state.cloudModel);
        updateCloudCredentialSources();
        selectCloudCredentialSource(state.providerApiKeyEnvironmentVariable(state.cloudProvider));
        defaultClient.setSelectedItem(clientFromFamily(resolveFamily(state)));
        defaultMode.setSelectedItem(state.defaultAutobotMode);
        pilotAiProvider.setSelectedItem(state.pilotAiProvider == null || state.pilotAiProvider.isBlank()
                ? "none" : state.pilotAiProvider);
        pilotAiModel.setText(state.pilotAiModel == null ? "" : state.pilotAiModel);
        updateCredentialSources(pilotCredentialSource, String.valueOf(pilotAiProvider.getSelectedItem()),
                state.providerApiKeyEnvironmentVariable(String.valueOf(pilotAiProvider.getSelectedItem())));
        pilotAiEndpoint.setText(state.localEndpointFor(String.valueOf(pilotAiProvider.getSelectedItem())));
        passProviderKeys.setSelected(state.passProviderApiKeysToMcp);
        openAiClearRequested = false;
        anthropicClearRequested = false;
        geminiClearRequested = false;
        githubClearRequested = false;
        clear(openAiKey);
        clear(anthropicKey);
        clear(geminiKey);
        clear(githubKey);
        clear(lmStudioKey);
        clear(ollamaKey);
        updateStoredKeyStatus(credentialsProvider.get());
        editingAgentConfiguration = false;
        updateAgentConfigurationControls();
        resetTestExecutionOverrides();
    }

    @Override
    public void disposeUIResources() {
        panel = null;
        mcpCommand = null;
        mcpCommandManualEdit = null;
        testMcp = null;
        testStatus = null;
        testRecovery = null;
        testRecoveryAction = null;
        currentAgentConfigurationTitle = null;
        currentAgentConfiguration = null;
        currentAgentChip = null;
        configureAgent = null;
        assistantProviderTypeLabel = null;
        assistantAgentLabel = null;
        assistantFamilyLabel = null;
        assistantRuntimeLabel = null;
        cloudProviderLabel = null;
        cloudModelLabel = null;
        assistantProviderType = null;
        assistantAgent = null;
        assistantFamily = null;
        assistantRuntime = null;
        cloudProvider = null;
        cloudModel = null;
        cloudCredentialSource = null;
        pilotCredentialSource = null;
        defaultClient = null;
        defaultMode = null;
        pilotAiProvider = null;
        pilotAiModel = null;
        pilotAiEndpoint = null;
        passProviderKeys = null;
        advancedUiEnabled = null;
        watchModeEnabled = null;
        testExecutionSection = null;
        overrideExecutionProperties = null;
        targetBrowserNameLabel = null;
        targetBrowserName = null;
        headlessExecution = null;
        testExecutionHelp = null;
        defaultModeLabel = null;
        shaftAiSection = null;
        shaftAiProviderLabel = null;
        shaftAiModelLabel = null;
        shaftAiEndpointLabel = null;
        providerKeysSection = null;
        shaftAiHelp = null;
        providerKeysHelp = null;
        providerKeysStorageHelp = null;
        openAiKeyLabel = null;
        anthropicKeyLabel = null;
        geminiKeyLabel = null;
        githubKeyLabel = null;
        lmStudioKeyLabel = null;
        ollamaKeyLabel = null;
        openAiKey = null;
        anthropicKey = null;
        geminiKey = null;
        githubKey = null;
        lmStudioKey = null;
        ollamaKey = null;
        clearOpenAiKey = null;
        clearAnthropicKey = null;
        clearGeminiKey = null;
        clearGithubKey = null;
        clearLmStudioKey = null;
        clearOllamaKey = null;
        testOpenAiKey = null;
        testAnthropicKey = null;
        testGeminiKey = null;
        testGithubKey = null;
        openAiKeyStatus = null;
        anthropicKeyStatus = null;
        geminiKeyStatus = null;
        githubKeyStatus = null;
        lmStudioKeyStatus = null;
        ollamaKeyStatus = null;
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
            case "GROK" -> "GROK";
            default -> "CODEX";
        };
    }

    private static String clientFromFamily(String family) {
        return switch (normalize(family, "CODEX")) {
            case "CLAUDE" -> "CLAUDE_CODE";
            case "COPILOT" -> "COPILOT_CLI";
            case "GROK" -> "GROK";
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
            testStatus.getAccessibleContext().setAccessibleDescription(testStatus.getText());
            testStatus.setEnabled(false);
        }
        if (testRecovery != null) {
            testRecovery.setVisible(false);
        }
        if (testRecoveryAction != null) {
            testRecoveryAction.setVisible(false);
        }
    }

    /**
     * Applies the read-only gate (issue #3601 B2.1): once {@link #mcpCommandManaged} is true, a
     * casual Settings visitor sees the wizard-configured command rather than an editable raw shell
     * command to construct from scratch -- {@link #mcpCommandManualEdit} is the opt-in escape
     * hatch, mirroring {@code ShaftMcpSetupPanel}'s manual-installer-target disclosure. A fresh
     * install with no managed command yet stays directly editable, and the toggle itself only makes
     * sense once there is something to protect.
     */
    private void updateMcpCommandEditableState() {
        if (mcpCommand == null || mcpCommandManualEdit == null) {
            return;
        }
        mcpCommandManualEdit.setVisible(mcpCommandManaged);
        mcpCommand.setEditable(!mcpCommandManaged || mcpCommandManualEdit.isSelected());
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
        // Accessible description mirrors the live text (issue #3605), not a static generic
        // explanation: see updateStoredState(), the choke point every later update runs through.
        label.getAccessibleContext().setAccessibleDescription(label.getText());
        return label;
    }

    private static JPanel keyRow(JButton clearButton, JButton testButton, JLabel statusLabel) {
        JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        row.add(clearButton);
        if (testButton != null) {
            row.add(testButton);
        }
        row.add(statusLabel);
        return row;
    }

    private static JPanel testRecoveryRow(JLabel recoveryLabel, JButton recoveryButton) {
        JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        row.add(recoveryLabel);
        row.add(recoveryButton);
        return row;
    }

    /**
     * Builds the bordered "chip" wrapping {@code currentConfiguration} + {@code configureButton}
     * (issue #4322). Neutral informational tint ({@code progress()}) -- the same idiom {@code
     * ShaftAssistantPanel#currentAgentChip} uses for the identical label/gear pair in its
     * {@code routeRow} (PR #4320) -- distinct from a high-stakes-toggle warning tint, since this
     * pair is just "which agent is this / configure it".
     */
    private static JPanel agentConfigurationRow(JLabel currentConfiguration, JButton configureButton) {
        JPanel row = new JPanel(new BorderLayout(4, 0));
        row.setOpaque(true);
        row.setBackground(ShaftStatusPresentation.tint(
                javax.swing.UIManager.getColor("Panel.background") == null
                        ? java.awt.Color.WHITE
                        : javax.swing.UIManager.getColor("Panel.background"),
                ShaftStatusPresentation.progress(), 0.08D));
        row.setBorder(JBUI.Borders.compound(
                JBUI.Borders.customLine(ShaftStatusPresentation.progress(), 1),
                JBUI.Borders.empty(2, 6)));
        row.add(currentConfiguration, BorderLayout.CENTER);
        row.add(configureButton, BorderLayout.EAST);
        return row;
    }

    private static boolean hasPassword(JPasswordField field) {
        return field != null && field.getPassword().length > 0;
    }

    private static CompletableFuture<Void> applyCredentialChange(CredentialAccess credentials, String key, JPasswordField field, boolean clearRequested) {
        char[] password = field.getPassword();
        boolean hasRealValue = hasMeaningfulValue(password);
        CompletableFuture<Void> future = CompletableFuture.completedFuture(null);
        if (hasRealValue || clearRequested) {
            future = credentials.setApiKeyAsync(key, password);
        }
        if (clearRequested || hasRealValue || password.length > 0) {
            clear(field);
        }
        return future;
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
        updateStoredStateAsync(credentials, OPENAI_PROVIDER_KEY, openAiKeyStatus);
        updateStoredStateAsync(credentials, ANTHROPIC_PROVIDER_KEY, anthropicKeyStatus);
        updateStoredStateAsync(credentials, GEMINI_PROVIDER_KEY, geminiKeyStatus);
        updateStoredStateAsync(credentials, GITHUB_PROVIDER_KEY, githubKeyStatus);
        updateStoredStateAsync(credentials, "LMSTUDIO_API_KEY", lmStudioKeyStatus);
        updateStoredStateAsync(credentials, "OLLAMA_API_KEY", ollamaKeyStatus);
    }

    private static void updateStoredStateAsync(CredentialAccess credentials, String key, JLabel label) {
        credentials.hasApiKeyAsync(key).thenAccept(stored -> updateStoredState(label, stored));
    }

    private static void updateStoredState(JLabel statusLabel, boolean stored) {
        if (statusLabel == null) {
            return;
        }
        String text = stored ? "Stored in Password Safe." : "No stored key.";
        statusLabel.setText(text);
        statusLabel.getAccessibleContext().setAccessibleDescription(text);
    }

    private void testProviderKey(String providerKey, JPasswordField field, JLabel statusLabel, JButton button,
                                  String providerLabel,
                                  java.util.function.Function<char[], ProviderKeyProbe.Result> probeFn) {
        if (button == null || statusLabel == null || field == null || !button.isEnabled()) {
            return;
        }
        button.setEnabled(false);
        char[] liveValue = field.getPassword();
        if (hasMeaningfulValue(liveValue)) {
            runProviderKeyProbe(button, statusLabel, providerLabel, probeFn, liveValue);
            return;
        }
        Arrays.fill(liveValue, '\0');
        String provider = providerForKey(providerKey);
        String cloudProviderName = String.valueOf(cloudProvider.getSelectedItem());
        String pilotProviderName = String.valueOf(pilotAiProvider.getSelectedItem());
        boolean cloudRouteActive = "CLOUD".equalsIgnoreCase(String.valueOf(assistantProviderType.getSelectedItem()));
        String selectedVariable = cloudRouteActive && provider.equals(cloudProviderName)
                ? selectedCloudEnvironmentVariable()
                : provider.equals(pilotProviderName)
                ? selectedPilotEnvironmentVariable()
                : settingsProvider.get().providerApiKeyEnvironmentVariable(provider);
        if (ProviderCredentialSource.supports(provider, selectedVariable)) {
            String environmentValue = providerEnvironmentLookup.apply(selectedVariable);
            char[] environmentSecret = environmentValue == null ? new char[0] : environmentValue.toCharArray();
            if (hasMeaningfulValue(environmentSecret)) {
                runProviderKeyProbe(button, statusLabel, providerLabel, probeFn, environmentSecret);
                return;
            }
            Arrays.fill(environmentSecret, '\0');
            button.setEnabled(true);
            showProviderKeyResult(statusLabel, providerLabel,
                    ProviderKeyProbe.Result.fail(selectedVariable + " is not configured."));
            return;
        }
        credentialsProvider.get().apiKeyAsync(providerKey).thenAccept(stored -> {
            char[] storedValue = stored == null ? new char[0] : stored.toCharArray();
            if (!hasMeaningfulValue(storedValue)) {
                button.setEnabled(true);
                showProviderKeyResult(statusLabel, providerLabel, ProviderKeyProbe.Result.fail("No key to test."));
                return;
            }
            runProviderKeyProbe(button, statusLabel, providerLabel, probeFn, storedValue);
        });
    }

    private static String providerForKey(String providerKey) {
        return switch (providerKey) {
            case OPENAI_PROVIDER_KEY -> "openai";
            case ANTHROPIC_PROVIDER_KEY -> "anthropic";
            case GEMINI_PROVIDER_KEY -> "gemini";
            case GITHUB_PROVIDER_KEY -> "github";
            case "LMSTUDIO_API_KEY" -> "lmstudio";
            case "OLLAMA_API_KEY" -> "ollama";
            default -> "";
        };
    }

    private static void runProviderKeyProbe(JButton button, JLabel statusLabel, String providerLabel,
                                             java.util.function.Function<char[], ProviderKeyProbe.Result> probeFn,
                                             char[] key) {
        statusLabel.setText("Testing...");
        statusLabel.getAccessibleContext().setAccessibleDescription(statusLabel.getText());
        statusLabel.setForeground(ShaftStatusPresentation.progress());
        CompletableFuture.supplyAsync(() -> probeFn.apply(key), ShaftPluginExecutor.getInstance().executor())
                .whenComplete((result, error) -> ApplicationManager.getApplication().invokeLater(() -> {
                    Arrays.fill(key, '\0');
                    button.setEnabled(true);
                    showProviderKeyResult(statusLabel, providerLabel,
                            error != null ? ProviderKeyProbe.Result.fail("Could not run the check.") : result);
                }));
    }

    private static void showProviderKeyResult(JLabel statusLabel, String providerLabel, ProviderKeyProbe.Result result) {
        String text = result.success()
                ? ShaftStatusPresentation.SUCCESS_ICON + " " + providerLabel + " key OK."
                : ShaftStatusPresentation.ERROR_ICON + " " + result.reason();
        statusLabel.setText(text);
        statusLabel.getAccessibleContext().setAccessibleDescription(text);
        statusLabel.setForeground(result.success() ? ShaftStatusPresentation.success() : ShaftStatusPresentation.error());
    }

    private void testMcpConnection() {
        JButton button = testMcp;
        JLabel statusLabel = testStatus;
        JPanel host = panel;
        if (button == null || statusLabel == null || host == null || mcpCommand == null || passProviderKeys == null) {
            return;
        }
        if (testMcpInFlight) {
            return;
        }
        if (!validFormLocalEndpoint()) {
            statusLabel.setEnabled(true);
            statusLabel.setText(ShaftStatusPresentation.ERROR_ICON + " Invalid local endpoint");
            statusLabel.getAccessibleContext().setAccessibleDescription(statusLabel.getText());
            statusLabel.setForeground(ShaftStatusPresentation.error());
            return;
        }
        testMcpInFlight = true;
        button.setEnabled(false);
        button.setToolTipText(TESTING_MCP_TOOLTIP);
        statusLabel.setEnabled(true);
        statusLabel.setText("Testing...");
        statusLabel.getAccessibleContext().setAccessibleDescription(statusLabel.getText());
        statusLabel.setForeground(ShaftStatusPresentation.progress());
        if (testRecovery != null) {
            testRecovery.setVisible(false);
        }
        if (testRecoveryAction != null) {
            testRecoveryAction.setVisible(false);
        }
        // Race fix (issue #3551): a check is starting, so the persisted state must not read ready
        // for the whole in-flight window (mirrors ShaftMcpSetupPanel#testConnection()).
        settingsProvider.get().mcpSetupComplete = false;
        String command = mcpCommand.getText() == null ? "" : mcpCommand.getText().trim();
        ShaftMcpConnectionProbe.test(command, formSettings(), resolveProjectRoot()).whenComplete((result, error) ->
                ApplicationManager.getApplication().invokeLater(() -> {
                    if (button == null || statusLabel == null || host == null) {
                        return;
                    }
                    testMcpInFlight = false;
                    button.setEnabled(true);
                    button.setToolTipText(TEST_MCP_TOOLTIP);
                    if (error != null) {
                        statusLabel.setText(ShaftStatusPresentation.ERROR_ICON + " Failed");
                        statusLabel.getAccessibleContext().setAccessibleDescription(statusLabel.getText());
                        statusLabel.setForeground(ShaftStatusPresentation.error());
                        McpInvocationError category = McpInvocationError.categorize(error);
                        StringBuilder sb = new StringBuilder();
                        sb.append(McpInvocationError.detail(error, category));
                        if (category.recoveryAction() != null) {
                            sb.append("\n\nRecovery: ").append(category.recoveryAction());
                        }
                        if (testRecovery != null) {
                            String testRecoveryText = sb.toString();
                            testRecovery.setText(testRecoveryText);
                            testRecovery.getAccessibleContext().setAccessibleDescription(testRecoveryText);
                            testRecovery.setVisible(true);
                        }
                        configureRecoveryAction(category, this::testMcpConnection);
                    } else {
                        showProbeResult(host, statusLabel, result);
                    }
                }));
    }

    /**
     * Resolves the currently open project so "Test Connection" scopes the MCP workspace to it
     * instead of the IDE process's own working directory. The Settings dialog carries the
     * triggering project in its {@link com.intellij.openapi.actionSystem.DataContext}; falls back to
     * the IDE process's cwd (the pre-existing behavior) when no project can be resolved, e.g. in
     * headless/unit-test contexts.
     *
     * @return the open project's root, or {@code Path.of(".")} when no project is available
     */
    private Project resolveProject() {
        if (panel == null || ApplicationManager.getApplication() == null) {
            return null;
        }
        return CommonDataKeys.PROJECT.getData(DataManager.getInstance().getDataContext(panel));
    }

    private Path resolveProjectRoot() {
        if (projectRootProvider != null) {
            return projectRootProvider.get();
        }
        Project project = resolveProject();
        return project != null && project.getBasePath() != null ? Path.of(project.getBasePath()) : Path.of(".");
    }

    private Path resolveCustomPropertiesFile() {
        return resolveProjectRoot().resolve(CUSTOM_PROPERTIES_RELATIVE_PATH);
    }

    /**
     * Reflects this project's on-disk {@code custom.properties} into the gate/combo/checkbox
     * (issue #3665 part A): the gate is checked, and the combo/checkbox reflect the file's values,
     * only when a {@code targetBrowserName} or {@code headlessExecution} line is actually present;
     * otherwise the gate stays unchecked and the controls show their "use SHAFT default" state.
     */
    private void resetTestExecutionOverrides() {
        if (overrideExecutionProperties == null) {
            return;
        }
        Map<String, String> properties = ShaftCustomPropertiesFile.read(resolveCustomPropertiesFile());
        String browser = properties.get(TARGET_BROWSER_NAME_KEY);
        String headless = properties.get(HEADLESS_EXECUTION_KEY);
        overrideExecutionProperties.setSelected(browser != null || headless != null);
        targetBrowserName.setSelectedItem(browser == null || browser.isBlank() ? TEST_EXECUTION_BROWSER_DEFAULT : browser);
        headlessExecution.setSelected(headless != null && Boolean.parseBoolean(headless));
        updateTestExecutionControlsEnabled();
    }

    /**
     * Writes the gate/combo/checkbox back to this project's {@code custom.properties}: unchecking
     * the gate removes both keys (restoring SHAFT's own defaults); checking it writes
     * {@code headlessExecution} always, and {@code targetBrowserName} unless the combo is on the
     * "use SHAFT default" sentinel item.
     */
    private void applyTestExecutionOverrides() {
        if (overrideExecutionProperties == null) {
            return;
        }
        Path file = resolveCustomPropertiesFile();
        if (!overrideExecutionProperties.isSelected()) {
            ShaftCustomPropertiesFile.write(file, Map.of(), Set.of(TARGET_BROWSER_NAME_KEY, HEADLESS_EXECUTION_KEY));
            return;
        }
        Map<String, String> setKeys = new LinkedHashMap<>();
        Object selectedBrowser = targetBrowserName.getSelectedItem();
        if (selectedBrowser != null && !TEST_EXECUTION_BROWSER_DEFAULT.equals(selectedBrowser)) {
            setKeys.put(TARGET_BROWSER_NAME_KEY, String.valueOf(selectedBrowser));
        }
        setKeys.put(HEADLESS_EXECUTION_KEY, String.valueOf(headlessExecution.isSelected()));
        ShaftCustomPropertiesFile.write(file, setKeys, Set.of());
    }

    /**
     * Re-reads this project's {@code custom.properties} fresh (never cached: the dialog can stay
     * open while a user hand-edits the file externally) and compares it against the current
     * gate/combo/checkbox UI state.
     */
    private boolean testExecutionModified() {
        if (overrideExecutionProperties == null) {
            return false;
        }
        Map<String, String> properties = ShaftCustomPropertiesFile.read(resolveCustomPropertiesFile());
        String browser = properties.get(TARGET_BROWSER_NAME_KEY);
        String headless = properties.get(HEADLESS_EXECUTION_KEY);
        boolean overridden = browser != null || headless != null;
        if (overrideExecutionProperties.isSelected() != overridden) {
            return true;
        }
        if (!overridden) {
            return false;
        }
        String expectedBrowser = browser == null || browser.isBlank() ? TEST_EXECUTION_BROWSER_DEFAULT : browser;
        boolean expectedHeadless = headless != null && Boolean.parseBoolean(headless);
        return !Objects.equals(expectedBrowser, targetBrowserName.getSelectedItem())
                || expectedHeadless != headlessExecution.isSelected();
    }

    private void updateTestExecutionControlsEnabled() {
        boolean enabled = overrideExecutionProperties != null && overrideExecutionProperties.isSelected();
        if (targetBrowserNameLabel != null) {
            targetBrowserNameLabel.setEnabled(enabled);
        }
        if (targetBrowserName != null) {
            targetBrowserName.setEnabled(enabled);
        }
        if (headlessExecution != null) {
            headlessExecution.setEnabled(enabled);
        }
    }

    private void showProbeResult(JPanel host, JLabel statusLabel, ShaftMcpToolResult result) {
        if (host == null || statusLabel == null) {
            return;
        }
        if (result != null && result.success()) {
            statusLabel.setText(ShaftStatusPresentation.SUCCESS_ICON + " Connected");
            statusLabel.getAccessibleContext().setAccessibleDescription(statusLabel.getText());
            statusLabel.setForeground(ShaftStatusPresentation.success());
            if (testRecovery != null) {
                testRecovery.setVisible(false);
            }
            if (testRecoveryAction != null) {
                testRecoveryAction.setVisible(false);
            }
            saveConnectedSettings();
            editingAgentConfiguration = false;
            updateAgentConfigurationControls();
        } else {
            statusLabel.setText(ShaftStatusPresentation.ERROR_ICON + " Failed");
            statusLabel.getAccessibleContext().setAccessibleDescription(statusLabel.getText());
            statusLabel.setForeground(ShaftStatusPresentation.error());
            String message = formatErrorMessage(result);
            if (testRecovery != null) {
                testRecovery.setText(message);
                testRecovery.getAccessibleContext().setAccessibleDescription(message);
                testRecovery.setVisible(true);
            }
            configureRecoveryAction(result != null ? result.errorCategory() : null, this::testMcpConnection);
        }
    }

    private void configureRecoveryAction(McpInvocationError category, Runnable retryAction) {
        if (testRecoveryAction == null) {
            return;
        }
        if (category == null) {
            testRecoveryAction.setVisible(false);
            return;
        }
        RecoveryActions.Kind kind = RecoveryActions.forCategory(category);
        testRecoveryAction.setText(switch (kind) {
            case RETRY -> "Retry";
            case RESTART -> "Restart MCP server";
            case VIEW_LOGS -> "View logs";
        });
        for (var listener : testRecoveryAction.getActionListeners()) {
            testRecoveryAction.removeActionListener(listener);
        }
        testRecoveryAction.addActionListener(event -> runTestRecoveryAction(kind, retryAction));
        testRecoveryAction.setVisible(true);
    }

    private void runTestRecoveryAction(RecoveryActions.Kind kind, Runnable retryAction) {
        switch (kind) {
            case RETRY -> retryAction.run();
            case RESTART -> restartMcpConnectionFromTestRecovery();
            case VIEW_LOGS -> RecoveryActions.activateEventLog(resolveProject());
            default -> throw new IllegalStateException("Unexpected recovery kind: " + kind);
        }
    }

    private void restartMcpConnectionFromTestRecovery() {
        Project project = resolveProject();
        if (project != null) {
            ShaftMcpInvocationService.getInstance(project).restartConnection();
        }
        if (testStatus != null) {
            testStatus.setText("Not tested");
            testStatus.getAccessibleContext().setAccessibleDescription(testStatus.getText());
        }
        testRecoveryAction.setVisible(false);
        if (testRecovery != null) {
            testRecovery.setVisible(false);
        }
    }

    private String formatErrorMessage(ShaftMcpToolResult result) {
        if (result == null) {
            return "No result returned.";
        }
        if (result.errorCategory() != null) {
            StringBuilder sb = new StringBuilder();
            sb.append(result.output());
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
        AssistantAgentRoute selectedRoute = selectedAgentRoute();
        if (selectedRoute != null) {
            selectedRoute.applyTo(settings);
            settings.assistantProviderType = settings.advancedUiEnabled
                    ? String.valueOf(assistantProviderType.getSelectedItem())
                    : selectedRoute.providerType();
        }
        settings.cloudProvider = String.valueOf(cloudProvider.getSelectedItem());
        settings.cloudModel = cloudModel.getText() == null ? "" : cloudModel.getText().trim();
        boolean cloudRouteActive = "CLOUD".equalsIgnoreCase(settings.assistantProviderType);
        if (cloudRouteActive) {
            settings.setProviderApiKeyEnvironmentVariable(settings.cloudProvider, selectedCloudEnvironmentVariable());
        }
        settings.defaultAutobotMode = String.valueOf(defaultMode.getSelectedItem());
        settings.pilotAiProvider = String.valueOf(pilotAiProvider.getSelectedItem());
        settings.pilotAiModel = pilotAiModel.getText() == null ? "" : pilotAiModel.getText().trim();
        if (!cloudRouteActive || !settings.pilotAiProvider.equals(settings.cloudProvider)) {
            settings.setProviderApiKeyEnvironmentVariable(settings.pilotAiProvider, selectedPilotEnvironmentVariable());
        }
        saveLocalEndpoint(settings, settings.pilotAiProvider, pilotAiEndpoint.getText() == null ? "" : pilotAiEndpoint.getText());
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
        boolean cloudRouteActive = "CLOUD".equalsIgnoreCase(form.assistantProviderType);
        if (cloudRouteActive) {
            state.setProviderApiKeyEnvironmentVariable(form.cloudProvider,
                    form.providerApiKeyEnvironmentVariable(form.cloudProvider));
        }
        state.defaultAutobotClient = form.defaultAutobotClient;
        state.defaultAutobotMode = form.defaultAutobotMode;
        state.pilotAiProvider = form.pilotAiProvider;
        state.pilotAiModel = form.pilotAiModel;
        if (!cloudRouteActive || !form.pilotAiProvider.equals(form.cloudProvider)) {
            state.setProviderApiKeyEnvironmentVariable(form.pilotAiProvider,
                    form.providerApiKeyEnvironmentVariable(form.pilotAiProvider));
        }
        state.ollamaEndpoint = form.ollamaEndpoint;
        state.lmStudioEndpoint = form.lmStudioEndpoint;
        state.passProviderApiKeysToMcp = form.passProviderApiKeysToMcp;
        state.mcpSetupComplete = true;
    }

    private void updateCloudCredentialSources() {
        if (cloudCredentialSource == null || cloudProvider == null) {
            return;
        }
        String provider = String.valueOf(cloudProvider.getSelectedItem());
        cloudCredentialSource.removeAllItems();
        cloudCredentialSource.addItem("IntelliJ Password Safe");
        ProviderCredentialSource.present(provider, providerEnvironmentLookup)
                .forEach(source -> cloudCredentialSource.addItem(source.label()));
        selectCloudCredentialSource(settingsProvider.get().providerApiKeyEnvironmentVariable(provider));
    }

    private void updateCredentialSources(JComboBox<String> combo, String provider, String selectedVariable) {
        if (combo == null) {
            return;
        }
        combo.removeAllItems();
        combo.addItem("IntelliJ Password Safe");
        ProviderCredentialSource.present(provider, providerEnvironmentLookup)
                .forEach(source -> combo.addItem(source.label()));
        String label = "Use configured " + selectedVariable;
        for (int index = 0; selectedVariable != null && !selectedVariable.isBlank()
                && index < combo.getItemCount(); index++) {
            if (label.equals(combo.getItemAt(index))) {
                combo.setSelectedIndex(index);
                return;
            }
        }
        combo.setSelectedIndex(0);
    }

    private void selectCloudCredentialSource(String variableName) {
        if (cloudCredentialSource == null || variableName == null || variableName.isBlank()) {
            if (cloudCredentialSource != null) {
                cloudCredentialSource.setSelectedIndex(0);
            }
            return;
        }
        String label = "Use configured " + variableName;
        for (int index = 0; index < cloudCredentialSource.getItemCount(); index++) {
            if (label.equals(cloudCredentialSource.getItemAt(index))) {
                cloudCredentialSource.setSelectedIndex(index);
                return;
            }
        }
        cloudCredentialSource.setSelectedIndex(0);
    }

    private String selectedCloudEnvironmentVariable() {
        if (cloudCredentialSource == null || cloudCredentialSource.getSelectedIndex() <= 0) {
            return "";
        }
        String item = String.valueOf(cloudCredentialSource.getSelectedItem());
        String prefix = "Use configured ";
        return item.startsWith(prefix) ? item.substring(prefix.length()) : "";
    }

    private String selectedPilotEnvironmentVariable() {
        if (pilotCredentialSource == null || pilotCredentialSource.getSelectedIndex() <= 0) {
            return "";
        }
        String item = String.valueOf(pilotCredentialSource.getSelectedItem());
        return item.startsWith("Use configured ") ? item.substring("Use configured ".length()) : "";
    }

    private AssistantAgentRoute selectedAgentRoute() {
        Object selected = assistantAgent == null ? null : assistantAgent.getSelectedItem();
        return selected instanceof AssistantAgentRoute route ? route : null;
    }

    private void syncLegacyAgentControls() {
        if (assistantFamily == null || assistantRuntime == null || assistantProviderType == null) {
            return;
        }
        AssistantAgentRoute route = selectedAgentRoute();
        if (route == null) {
            return;
        }
        assistantProviderType.setSelectedItem(route.providerType());
        assistantFamily.setSelectedItem(route.family());
        assistantRuntime.setSelectedItem(route.runtime());
        if (defaultClient != null) {
            defaultClient.setSelectedItem(route.client());
        }
        if (route.gemini() && cloudProvider != null) {
            cloudProvider.setSelectedItem("gemini");
        }
        updateAgentConfigurationControls();
    }

    private void updateAgentConfigurationControls() {
        if (currentAgentConfiguration == null) {
            return;
        }
        ShaftSettingsState.Settings state = settingsProvider.get();
        String currentAgentConfigurationText = currentAgentConfigurationText(state);
        currentAgentConfiguration.setText(currentAgentConfigurationText);
        // Accessible description mirrors the live agent configuration text (issue #3603): the name
        // stays a stable category label, but a screen reader also needs to hear which agent/runtime
        // is actually configured, which changes as the user reconfigures the route.
        currentAgentConfiguration.getAccessibleContext().setAccessibleDescription(currentAgentConfigurationText);
        boolean advanced = advancedUiEnabled != null && advancedUiEnabled.isSelected();
        if (!advanced && "CLOUD".equals(assistantProviderType.getSelectedItem())) {
            assistantProviderType.setSelectedItem("LOCAL");
        }
        boolean showSummary = mcpReady(state) && !editingAgentConfiguration;
        boolean cloud = advanced && "CLOUD".equals(assistantProviderType.getSelectedItem());
        currentAgentConfigurationTitle.setVisible(showSummary);
        // Chip has no state of its own; it only needs to stay in lockstep with the label's and
        // configureAgent button's own shared showSummary gate so it never renders as an empty
        // colored box (mirrors ShaftAssistantPanel's currentAgentChip comment, issue #4316/PR #4320).
        currentAgentChip.setVisible(showSummary);
        currentAgentConfiguration.setVisible(showSummary);
        configureAgent.setVisible(showSummary);
        assistantAgentLabel.setVisible(!showSummary);
        assistantAgent.setVisible(!showSummary);
        assistantProviderTypeLabel.setVisible(advanced && !showSummary);
        assistantProviderType.setVisible(advanced && !showSummary);
        assistantFamilyLabel.setVisible(false);
        assistantFamily.setVisible(false);
        assistantRuntimeLabel.setVisible(false);
        assistantRuntime.setVisible(false);
        cloudProviderLabel.setVisible(cloud && !showSummary);
        cloudProvider.setVisible(cloud && !showSummary);
        cloudModelLabel.setVisible(cloud && !showSummary);
        cloudModel.setVisible(cloud && !showSummary);
        cloudCredentialSourceLabel.setVisible(cloud && !showSummary);
        cloudCredentialSource.setVisible(cloud && !showSummary);
        defaultModeLabel.setVisible(advanced && !showSummary);
        defaultMode.setVisible(advanced && !showSummary);
        setVisible(advanced, shaftAiSection, shaftAiProviderLabel, shaftAiEndpointLabel, shaftAiModelLabel, shaftAiHelp,
                pilotCredentialSourceLabel,
                providerKeysSection, providerKeysHelp, providerKeysStorageHelp, openAiKeyLabel,
                anthropicKeyLabel, geminiKeyLabel, githubKeyLabel, lmStudioKeyLabel, ollamaKeyLabel,
                openAiKeyStatus, anthropicKeyStatus, geminiKeyStatus, githubKeyStatus, lmStudioKeyStatus, ollamaKeyStatus);
        pilotAiProvider.setVisible(advanced);
        pilotAiModel.setVisible(advanced);
        pilotAiEndpoint.setVisible(advanced);
        pilotCredentialSource.setVisible(advanced);
        passProviderKeys.setVisible(advanced);
        openAiKey.setVisible(advanced);
        anthropicKey.setVisible(advanced);
        geminiKey.setVisible(advanced);
        githubKey.setVisible(advanced);
        lmStudioKey.setVisible(advanced);
        ollamaKey.setVisible(advanced);
        clearOpenAiKey.setVisible(advanced);
        clearAnthropicKey.setVisible(advanced);
        clearGeminiKey.setVisible(advanced);
        clearGithubKey.setVisible(advanced);
        clearLmStudioKey.setVisible(advanced);
        clearOllamaKey.setVisible(advanced);
        testOpenAiKey.setVisible(advanced);
        testAnthropicKey.setVisible(advanced);
        testGeminiKey.setVisible(advanced);
        testGithubKey.setVisible(advanced);
    }

    private static void setVisible(boolean visible, JComponent... components) {
        for (JComponent component : components) {
            component.setVisible(visible);
        }
    }

    private static boolean mcpReady(ShaftSettingsState.Settings state) {
        return state != null && state.mcpReady();
    }

    private boolean validFormLocalEndpoint() {
        String provider = String.valueOf(pilotAiProvider.getSelectedItem());
        if (!"ollama".equals(provider) && !"lmstudio".equals(provider)) {
            return true;
        }
        return pilotAiEndpoint.getText().isBlank() || ShaftSettingsState.validLocalEndpoint(provider, pilotAiEndpoint.getText());
    }

    private static void validateLocalEndpoint(String provider, String endpoint) throws ConfigurationException {
        if (!"ollama".equals(provider) && !"lmstudio".equals(provider)) {
            return;
        }
        if (endpoint.isBlank()) {
            return;
        }
        if (!ShaftSettingsState.validLocalEndpoint(provider, endpoint)) {
            throw new ConfigurationException("Local endpoint must be a literal loopback HTTP(S) URL without credentials, query, fragment, or whitespace.");
        }
    }

    private static void saveLocalEndpoint(ShaftSettingsState.Settings settings, String provider, String endpoint) {
        if ("ollama".equals(provider)) {
            settings.ollamaEndpoint = endpoint;
        } else if ("lmstudio".equals(provider)) {
            settings.lmStudioEndpoint = endpoint;
        }
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
        CompletableFuture<Void> setApiKeyAsync(String provider, char[] secret);

        CompletableFuture<Boolean> hasApiKeyAsync(String provider);

        CompletableFuture<String> apiKeyAsync(String provider);
    }

    private static CredentialAccess credentialAccess() {
        ShaftCredentialService service = ShaftCredentialService.getInstance();
        return new CredentialAccess() {
            @Override
            public CompletableFuture<Void> setApiKeyAsync(String provider, char[] secret) {
                return service.setApiKeyAsync(provider, secret);
            }

            @Override
            public CompletableFuture<Boolean> hasApiKeyAsync(String provider) {
                return service.hasApiKeyAsync(provider);
            }

            @Override
            public CompletableFuture<String> apiKeyAsync(String provider) {
                return service.apiKeyAsync(provider);
            }
        };
    }
}
