package com.shaft.intellij.settings;

import com.shaft.intellij.ui.ShaftAssistantChatState;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPluginResetServiceTest {

    @Test
    void factoryDefaultsForceNotSetUpStateEvenThoughBeanDefaultsToComplete() {
        ShaftSettingsState.Settings bareBeanDefaults = new ShaftSettingsState.Settings();
        ShaftSettingsState.Settings factoryDefaults = ShaftSettingsState.factoryDefaults();

        assertAll(
                () -> assertTrue(bareBeanDefaults.mcpSetupComplete,
                        "Sanity check: the bean's own default is mcpSetupComplete = true"),
                () -> assertFalse(factoryDefaults.mcpReady(), "Factory defaults must not be MCP-ready"),
                () -> assertEquals("", factoryDefaults.mcpCommand),
                () -> assertFalse(factoryDefaults.mcpSetupComplete,
                        "Factory reset must explicitly override mcpSetupComplete to false"),
                () -> assertFalse(factoryDefaults.agentGuidanceOptimizationPromptPending),
                () -> assertEquals("LOCAL", factoryDefaults.assistantProviderType),
                () -> assertEquals("", factoryDefaults.assistantFamily),
                () -> assertEquals("CLI", factoryDefaults.assistantRuntime),
                () -> assertEquals("gemini", factoryDefaults.cloudProvider),
                () -> assertEquals("gemini-3.5-flash", factoryDefaults.cloudModel),
                () -> assertEquals("CODEX", factoryDefaults.defaultAutobotClient),
                () -> assertEquals("ASK", factoryDefaults.defaultAutobotMode),
                () -> assertEquals("none", factoryDefaults.pilotAiProvider),
                () -> assertEquals("", factoryDefaults.pilotAiModel),
                () -> assertFalse(factoryDefaults.passProviderApiKeysToMcp),
                () -> assertFalse(factoryDefaults.advancedUiEnabled),
                () -> assertFalse(factoryDefaults.autoCompactEnabled));
    }

    @Test
    void resetSettingsRestoresFactoryDefaultsOnAMutatedState() {
        ShaftSettingsState settingsState = new ShaftSettingsState();
        ShaftSettingsState.Settings mutated = new ShaftSettingsState.Settings();
        mutated.mcpCommand = "npx shaft-mcp";
        mutated.mcpSetupComplete = true;
        mutated.assistantProviderType = "CLOUD";
        mutated.cloudProvider = "openai";
        mutated.cloudModel = "gpt-5";
        mutated.advancedUiEnabled = true;
        mutated.autoCompactEnabled = true;
        mutated.passProviderApiKeysToMcp = true;
        settingsState.loadState(mutated);
        assertTrue(settingsState.getState().mcpReady(), "Precondition: mutated state should be MCP-ready");

        ShaftPluginResetService.resetSettings(settingsState);

        ShaftSettingsState.Settings result = settingsState.getState();
        ShaftSettingsState.Settings expected = ShaftSettingsState.factoryDefaults();
        assertAll(
                () -> assertFalse(result.mcpReady()),
                () -> assertEquals(expected.mcpCommand, result.mcpCommand),
                () -> assertEquals(expected.mcpSetupComplete, result.mcpSetupComplete),
                () -> assertEquals(expected.agentGuidanceOptimizationPromptPending, result.agentGuidanceOptimizationPromptPending),
                () -> assertEquals(expected.assistantProviderType, result.assistantProviderType),
                () -> assertEquals(expected.assistantFamily, result.assistantFamily),
                () -> assertEquals(expected.assistantRuntime, result.assistantRuntime),
                () -> assertEquals(expected.cloudProvider, result.cloudProvider),
                () -> assertEquals(expected.cloudModel, result.cloudModel),
                () -> assertEquals(expected.defaultAutobotClient, result.defaultAutobotClient),
                () -> assertEquals(expected.defaultAutobotMode, result.defaultAutobotMode),
                () -> assertEquals(expected.pilotAiProvider, result.pilotAiProvider),
                () -> assertEquals(expected.pilotAiModel, result.pilotAiModel),
                () -> assertEquals(expected.passProviderApiKeysToMcp, result.passProviderApiKeysToMcp),
                () -> assertEquals(expected.advancedUiEnabled, result.advancedUiEnabled),
                () -> assertEquals(expected.autoCompactEnabled, result.autoCompactEnabled));
    }

    @Test
    void knownProvidersEnumeratesEveryKeyThePluginWrites() {
        assertEquals(
                List.of("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GEMINI_API_KEY", "GITHUB_TOKEN"),
                ShaftCredentialService.KNOWN_PROVIDERS);
    }

    @Test
    void clearAllOnCredentialServiceClearsEveryKnownProvider() throws Exception {
        // PasswordSafe requires a running IntelliJ Application (unavailable in this plain unit
        // test), so the write-path itself is verified by source inspection rather than execution -
        // mirroring the existing pattern in ShaftSettingsConfigurableTest. setApiKey(provider, null)
        // is proven elsewhere to remove the stored credential (a blank/null secret clears the entry),
        // so clearAll() looping setApiKey(provider, null) over every KNOWN_PROVIDERS entry leaves
        // hasApiKey false for each.
        String source = Files.readString(Path.of(
                "src/main/java/com/shaft/intellij/settings/ShaftCredentialService.java"));

        assertAll(
                () -> assertTrue(source.contains("public void clearAll()")),
                () -> assertTrue(source.contains("for (String provider : KNOWN_PROVIDERS)")),
                () -> assertTrue(source.contains("setApiKey(provider, null)")));
    }

    @Test
    void chatStateClearAllLeavesZeroSessionsAndBlankActiveId() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();
        state.loadState(sessionWithOneMessage("session-1", "first message"));
        assertFalse(state.isCleared(), "Precondition: state with a message should not be cleared");

        state.clearAll();

        assertTrue(state.isCleared(), "clearAll() must leave zero sessions and a blank active session id");
    }

    @Test
    void toolApprovalServiceClearAllEmptiesTheApprovalStore() {
        ToolApprovalService approvals = new ToolApprovalService();
        approvals.approve("capture_start");
        assertFalse(approvals.isEmpty(), "Precondition: an approved tool should make the store non-empty");

        approvals.clearAll();

        assertTrue(approvals.isEmpty(), "clearAll() must leave the approval store empty");
    }

    @Test
    void resetEverythingRunsEveryResetStepAndClearsEveryOpenProjectChatState() {
        ShaftAssistantChatState projectOne = new ShaftAssistantChatState();
        ShaftAssistantChatState projectTwo = new ShaftAssistantChatState();
        projectOne.loadState(sessionWithOneMessage("session-1", "hello from project one"));
        projectTwo.loadState(sessionWithOneMessage("session-2", "hello from project two"));

        boolean[] settingsResetRan = {false};
        boolean[] credentialsResetRan = {false};
        boolean[] approvalsResetRan = {false};
        boolean[] toolWindowRerendered = {false};

        ShaftPluginResetService service = new ShaftPluginResetService(
                () -> settingsResetRan[0] = true,
                () -> credentialsResetRan[0] = true,
                () -> approvalsResetRan[0] = true,
                () -> List.of(projectOne, projectTwo),
                () -> toolWindowRerendered[0] = true);

        service.resetEverything();

        assertAll(
                () -> assertTrue(settingsResetRan[0], "Settings reset step must run"),
                () -> assertTrue(credentialsResetRan[0], "Credential clear step must run"),
                () -> assertTrue(approvalsResetRan[0], "Tool approval clear step must run"),
                () -> assertTrue(projectOne.isCleared(), "Every open project's chat state must be cleared"),
                () -> assertTrue(projectTwo.isCleared(), "Every open project's chat state must be cleared"),
                () -> assertTrue(toolWindowRerendered[0], "Tool window re-render step must run"));
    }

    private static ShaftAssistantChatState.StateData sessionWithOneMessage(String sessionId, String markdown) {
        ShaftAssistantChatState.Message message = new ShaftAssistantChatState.Message();
        message.role = "user";
        message.markdown = markdown;
        message.createdAt = "2026-07-07T00:00:00Z";

        ShaftAssistantChatState.Session session = new ShaftAssistantChatState.Session();
        session.id = sessionId;
        session.title = markdown;
        session.createdAt = "2026-07-07T00:00:00Z";
        session.updatedAt = "2026-07-07T00:00:00Z";
        session.messages.add(message);

        ShaftAssistantChatState.StateData data = new ShaftAssistantChatState.StateData();
        data.sessions.add(session);
        data.activeSessionId = sessionId;
        return data;
    }
}
