package com.shaft.intellij.settings;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;

/**
 * Persistent SHAFT IntelliJ plugin settings.
 */
@State(name = "ShaftSettings", storages = @Storage("shaft.xml"))
public final class ShaftSettingsState implements PersistentStateComponent<ShaftSettingsState.Settings> {
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
        public String localModel = "";
        public String assistantEffort = "DEFAULT";
        public String defaultAutobotClient = "CODEX";
        /**
         * Agent mode is the first-contact default: the assistant can actually execute recording,
         * codegen, and diagnosis flows instead of only talking about them.
         */
        public String defaultAutobotMode = "AGENT";
        public String pilotAiProvider = "none";
        public String pilotAiModel = "";
        public boolean passProviderApiKeysToMcp = false;
        public boolean advancedUiEnabled = false;
        public boolean autoCompactEnabled = false;
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
         * Returns whether the configured MCP command has passed setup verification.
         *
         * @return true when setup is complete and the stdio command is present
         */
        public boolean mcpReady() {
            return mcpSetupComplete && mcpCommand != null && !mcpCommand.isBlank();
        }
    }
}
