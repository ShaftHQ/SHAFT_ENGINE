package com.shaft.intellij.settings;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * Persistent SHAFT IntelliJ plugin settings.
 */
@State(name = "ShaftSettings", storages = @Storage("shaft.xml"))
public final class ShaftSettingsState implements PersistentStateComponent<ShaftSettingsState.Settings> {
    public static final String OLLAMA_ENDPOINT = "http://127.0.0.1:11434/api/chat";
    public static final String LMSTUDIO_ENDPOINT = "http://127.0.0.1:1234/v1/responses";
    private Settings settings = new Settings();

    /**
     * Returns the application-level settings service.
     *
     * @return settings service
     */
    public static ShaftSettingsState getInstance() {
        return ApplicationManager.getApplication().getService(ShaftSettingsState.class);
    }

    @Override
    public Settings getState() {
        return settings;
    }

    @Override
    public void loadState(@NotNull Settings state) {
        XmlSerializerUtil.copyBean(state, settings);
    }

    /**
     * Returns the documented factory-default settings bean used to reset the plugin to a
     * fresh-install state. Every field matches {@link Settings}' own field defaults, including
     * {@link Settings#mcpSetupComplete} (now {@code false} by default, issue #3551); the explicit
     * assignment here is kept as defense-in-depth so a reset is provably fresh even if the bean
     * default ever changes again.
     *
     * @return a new Settings instance holding the factory defaults
     */
    public static Settings factoryDefaults() {
        Settings defaults = new Settings();
        defaults.mcpCommand = "";
        defaults.mcpSetupComplete = false;
        return defaults;
    }

    public static boolean validLocalEndpoint(String provider, String endpoint) {
        if (!("ollama".equals(provider) || "lmstudio".equals(provider)) || endpoint == null || endpoint.isBlank()
                || !endpoint.equals(endpoint.trim())) {
            return false;
        }
        try {
            URI uri = new URI(endpoint);
            if (!uri.isAbsolute() || uri.getHost() == null || uri.getUserInfo() != null || uri.getQuery() != null
                    || uri.getFragment() != null || !("http".equalsIgnoreCase(uri.getScheme())
                    || "https".equalsIgnoreCase(uri.getScheme()))) {
                return false;
            }
            return "127.0.0.1".equals(uri.getHost()) || "::1".equals(uri.getHost());
        } catch (URISyntaxException exception) {
            return false;
        }
    }

    public static String defaultLocalEndpoint(String provider) {
        return "lmstudio".equals(provider) ? LMSTUDIO_ENDPOINT : OLLAMA_ENDPOINT;
    }

    /**
     * Mutable XML-serializable settings bean.
     */
    public static final class Settings {
        public String mcpCommand = "";
        /**
         * Defaults to {@code false} (issue #3551): a fresh install (or a check that is still in
         * flight) must never read as ready. Because IntelliJ's XML serializer omits properties
         * that equal the bean default, a pre-existing user whose {@code shaft.xml} predates this
         * change and never wrote {@code mcpSetupComplete} will load {@code false} here once and
         * see one self-healing re-check after upgrading — acceptable and honest, not a bug.
         */
        public boolean mcpSetupComplete = false;
        /**
         * Last verified agent-lane readiness (two-lane setup, issue #3425): true only when the
         * optional agent check actually passed during setup. Read by the readiness strip
         * (issue #3500 A4/O4) so it never claims an agent is ready without a real check.
         */
        public boolean agentLaneReady = false;
        /**
         * Whether the first-run happy-path coach (issue #3500 O1) was dismissed; it never
         * reappears once acknowledged.
         */
        public boolean firstRunCoachDismissed = false;
        public boolean agentGuidanceOptimizationPromptPending = false;
        public String assistantProviderType = "LOCAL";
        public String assistantFamily = "";
        public String assistantRuntime = "CLI";
        public String cloudProvider = "gemini";
        public String cloudModel = "gemini-3.5-flash";
        /** Provider-standard variable names; blank means IntelliJ Password Safe. Values are never persisted. */
        public String openAiApiKeyEnvironmentVariable = "";
        public String anthropicApiKeyEnvironmentVariable = "";
        public String geminiApiKeyEnvironmentVariable = "";
        public String githubApiKeyEnvironmentVariable = "";
        public String ollamaApiKeyEnvironmentVariable = "";
        public String lmStudioApiKeyEnvironmentVariable = "";
        public String localModel = "";
        public String assistantEffort = "DEFAULT";
        public String defaultAutobotClient = "";
        /**
         * Agent mode is the first-contact default: the assistant can actually execute recording,
         * codegen, and diagnosis flows instead of only talking about them.
         */
        public String defaultAutobotMode = "AGENT";
        public String pilotAiProvider = "none";
        public String pilotAiModel = "";
        /** Non-secret endpoint for the selected local SHAFT AI provider. */
        public String pilotAiEndpoint = OLLAMA_ENDPOINT;
        /** Explicit Ollama endpoint; blank preserves the legacy {@link #pilotAiEndpoint} fallback. */
        public String ollamaEndpoint = "";
        /** Explicit LM Studio endpoint, independent of historical Ollama configuration. */
        public String lmStudioEndpoint = LMSTUDIO_ENDPOINT;
        public boolean passProviderApiKeysToMcp = false;
        public boolean advancedUiEnabled = false;
        public boolean autoCompactEnabled = false;
        /** Explicit opt-in for local-agent commands to run outside the project sandbox. */
        public boolean unrestrictedLocalAgentAccess = false;
        /**
         * Opt-in, default-off "watch mode": reruns the last SHAFT test run configuration when a
         * {@code src/test/} file changes, bounded by {@code WatchRerunThrottle} (see
         * {@code ShaftTestWatchService}).
         */
        public boolean watchModeEnabled = false;
        /**
         * Recorder browser visibility preference shared by the Guided workflow panel and the
         * assistant web/mobile recording flows.
         */
        public boolean recorderHeadless = false;
        /**
         * The plugin version last seen by {@code ShaftPluginUpgradeActivity} (issue: plugin-upgrade
         * auto-reset). Blank on a fresh install. Deliberately left out of {@link #factoryDefaults()}'s
         * special-cased overrides: a plain {@code new Settings()} already defaults it to {@code ""},
         * and an upgrade reset must overwrite it with the running version <em>after</em> the reset runs
         * so it survives that same reset instead of being wiped by it.
         */
        public String lastSeenPluginVersion = "";

        /**
         * Returns whether the configured MCP command has passed setup verification.
         *
         * @return true when setup is complete and the stdio command is present
         */
        public boolean mcpReady() {
            return mcpSetupComplete && mcpCommand != null && !mcpCommand.isBlank();
        }

        public String localEndpointFor(String provider) {
            if ("lmstudio".equals(provider)) {
                return lmStudioEndpoint == null ? "" : lmStudioEndpoint;
            }
            if ("ollama".equals(provider)) {
                return ollamaEndpoint == null || ollamaEndpoint.isBlank()
                        ? (pilotAiEndpoint == null ? "" : pilotAiEndpoint) : ollamaEndpoint;
            }
            return "";
        }

        public String providerApiKeyEnvironmentVariable(String provider) {
            return switch (provider == null ? "" : provider.trim().toLowerCase(java.util.Locale.ROOT)) {
                case "openai" -> openAiApiKeyEnvironmentVariable;
                case "anthropic" -> anthropicApiKeyEnvironmentVariable;
                case "gemini" -> geminiApiKeyEnvironmentVariable;
                case "github" -> githubApiKeyEnvironmentVariable;
                case "ollama" -> ollamaApiKeyEnvironmentVariable;
                case "lmstudio" -> lmStudioApiKeyEnvironmentVariable;
                default -> "";
            };
        }

        public void setProviderApiKeyEnvironmentVariable(String provider, String variableName) {
            String value = variableName == null ? "" : variableName.trim();
            switch (provider == null ? "" : provider.trim().toLowerCase(java.util.Locale.ROOT)) {
                case "openai" -> openAiApiKeyEnvironmentVariable = value;
                case "anthropic" -> anthropicApiKeyEnvironmentVariable = value;
                case "gemini" -> geminiApiKeyEnvironmentVariable = value;
                case "github" -> githubApiKeyEnvironmentVariable = value;
                case "ollama" -> ollamaApiKeyEnvironmentVariable = value;
                case "lmstudio" -> lmStudioApiKeyEnvironmentVariable = value;
                default -> { }
            }
        }
    }
}
