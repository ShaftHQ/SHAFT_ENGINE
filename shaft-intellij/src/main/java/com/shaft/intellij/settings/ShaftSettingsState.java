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
     * fresh-install state. Every field matches {@link Settings}' own field defaults except
     * {@link Settings#mcpSetupComplete}, which is explicitly forced to {@code false} here so the
     * setup view renders after a reset even though the bean's own default is {@code true}.
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
        public boolean mcpSetupComplete = true;
        public boolean agentGuidanceOptimizationPromptPending = false;
        public String assistantProviderType = "LOCAL";
        public String assistantFamily = "";
        public String assistantRuntime = "CLI";
        public String cloudProvider = "gemini";
        public String cloudModel = "gemini-3.5-flash";
        public String localModel = "";
        public String assistantEffort = "DEFAULT";
        public String defaultAutobotClient = "CODEX";
        public String defaultAutobotMode = "ASK";
        public String pilotAiProvider = "none";
        public String pilotAiModel = "";
        public boolean passProviderApiKeysToMcp = false;
        public boolean advancedUiEnabled = false;
        public boolean autoCompactEnabled = false;

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
