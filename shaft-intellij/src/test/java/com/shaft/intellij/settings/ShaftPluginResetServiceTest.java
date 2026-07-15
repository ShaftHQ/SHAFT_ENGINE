package com.shaft.intellij.settings;

import com.shaft.intellij.approval.ToolApprovalDecision;
import com.shaft.intellij.approval.ToolApprovalService;
import com.shaft.intellij.ui.ShaftAssistantChatState;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPluginResetServiceTest {

    @Test
    void factoryDefaultsForceNotSetUpStateMatchingTheBeansOwnDefault() {
        ShaftSettingsState.Settings bareBeanDefaults = new ShaftSettingsState.Settings();
        ShaftSettingsState.Settings factoryDefaults = ShaftSettingsState.factoryDefaults();

        assertAll(
                () -> assertFalse(bareBeanDefaults.mcpSetupComplete,
                        "Sanity check: the bean's own default is mcpSetupComplete = false (issue #3551)"),
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
                () -> assertEquals("AGENT", factoryDefaults.defaultAutobotMode),
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
    void clearAllOnCredentialServiceClearsEveryKnownProviderAndHasApiKeyIsFalseForEachAfterward() {
        // ShaftCredentialService.clearAll() delegates to this exact package-visible algorithm (see
        // ShaftCredentialService#clearAll(BiConsumer)) so it can be executed here against an in-memory
        // store instead of the real Password Safe, which requires a running IntelliJ Application
        // (unavailable in this plain unit test). Seed every known provider with a stored secret first
        // so the assertions below prove clearAll() actually removes each one, rather than passing
        // vacuously on an already-empty store.
        Map<String, String> store = new HashMap<>();
        for (String provider : ShaftCredentialService.KNOWN_PROVIDERS) {
            store.put(provider, "stored-secret-for-" + provider);
        }
        BiConsumer<String, char[]> setter = (provider, secret) -> {
            if (secret == null) {
                store.remove(provider);
            } else {
                store.put(provider, new String(secret));
            }
        };

        ShaftCredentialService.clearAll(setter);

        for (String provider : ShaftCredentialService.KNOWN_PROVIDERS) {
            String remaining = store.get(provider);
            boolean hasApiKey = remaining != null && !remaining.isBlank();
            assertFalse(hasApiKey, "hasApiKey must be false for " + provider + " after clearAll()");
        }
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
    void toolApprovalServiceResetClearsPermanentApprovalsAndTheApproveAllFlag() {
        // This must exercise com.shaft.intellij.approval.ToolApprovalService -- the store the chat
        // approval UI (ShaftAssistantPanel/ToolApprovalPromptPanel) actually writes to via
        // record()/isApproved(). A second, settings-package ToolApprovalService once existed and was
        // reset instead, leaving the approve-all flag and permanent approvals alive after
        // "Reset everything"; it has been deleted.
        ToolApprovalService approvals = new ToolApprovalService();
        approvals.record(ToolApprovalDecision.APPROVE_TOOL_ALWAYS, "capture_start");
        approvals.record(ToolApprovalDecision.APPROVE_ALL_TOOLS, "capture_stop");
        assertTrue(approvals.isApproved("capture_start"),
                "Precondition: a permanently approved tool should be approved");
        assertTrue(approvals.getState().approveAllTools,
                "Precondition: approve-all flag should be set");

        approvals.reset();

        assertAll(
                () -> assertFalse(approvals.getState().approveAllTools,
                        "reset() must clear the approve-all flag"),
                () -> assertFalse(approvals.isApproved("capture_start"),
                        "reset() must clear permanently approved tools"));
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

    @Test
    void resetForUpgradeRunsEveryResetStepButPreservesEveryOpenProjectChatState() {
        // Mirrors resetEverythingRunsEveryResetStepAndClearsEveryOpenProjectChatState above, but
        // asserts the opposite outcome for chat: an upgrade must reset the same stale UI/setup state
        // (settings, credentials, approvals) while leaving a user's conversation history alone.
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

        service.resetForUpgrade();

        assertAll(
                () -> assertTrue(settingsResetRan[0], "Settings reset step must run"),
                () -> assertTrue(credentialsResetRan[0], "Credential clear step must run"),
                () -> assertTrue(approvalsResetRan[0], "Tool approval clear step must run"),
                () -> assertFalse(projectOne.isCleared(), "resetForUpgrade must preserve chat history"),
                () -> assertFalse(projectTwo.isCleared(), "resetForUpgrade must preserve chat history"),
                () -> assertTrue(toolWindowRerendered[0], "Tool window re-render step must run"));
    }

    @Test
    void productionNoArgConstructorDoesNotEagerlyResolvePlatformServices() {
        // The no-arg constructor (the one plugin.xml's <applicationService> instantiates) wires
        // ShaftSettingsState.getInstance(), ShaftCredentialService.getInstance(),
        // the per-open-project ToolApprovalService reset loop, and the tool-window rerenderer only
        // inside deferred Runnable/Supplier lambda bodies. Constructing it here - with no running
        // IntelliJ Application in this plain unit test - must not throw, proving those getInstance()
        // calls are lazy and only fire when resetEverything() actually runs (which requires the real
        // platform and is exercised by the other constructor-injected tests in this class instead).
        assertDoesNotThrow(() -> {
            new ShaftPluginResetService();
        }, "production no-arg constructor must not eagerly resolve platform services");
    }

    @Test
    void pluginXmlRegistersEveryServiceGetInstanceResolvesDuringReset() throws Exception {
        // resetEverything() (via the production no-arg constructor) resolves ShaftPluginResetService,
        // ShaftCredentialService, and ShaftSettingsState through
        // ApplicationManager.getApplication().getService(...), which throws at IDE runtime unless the
        // class is registered as an <applicationService> in plugin.xml; it resolves ToolApprovalService
        // per open project through Project.getService(...), which requires a <projectService>
        // registration instead. Assert every one of them is registered so a future removal of an entry
        // (as previously happened for ToolApprovalService and ShaftPluginResetService itself) fails
        // this test instead of only failing inside a running IDE.
        String pluginXml = Files.readString(Path.of("src/main/resources/META-INF/plugin.xml"));

        assertAll(
                () -> assertTrue(pluginXml.contains(
                        "<applicationService serviceImplementation=\"com.shaft.intellij.settings.ShaftSettingsState\"/>"),
                        "ShaftSettingsState must be registered as an applicationService"),
                () -> assertTrue(pluginXml.contains(
                        "<applicationService serviceImplementation=\"com.shaft.intellij.settings.ShaftCredentialService\"/>"),
                        "ShaftCredentialService must be registered as an applicationService"),
                () -> assertTrue(pluginXml.contains(
                        "<projectService serviceImplementation=\"com.shaft.intellij.approval.ToolApprovalService\"/>"),
                        "The approval-package ToolApprovalService (the store the chat approval UI"
                                + " writes to) must be registered as a projectService so approvals never"
                                + " leak from one open project to another"),
                () -> assertTrue(pluginXml.contains(
                        "<applicationService serviceImplementation=\"com.shaft.intellij.settings.ShaftPluginResetService\"/>"),
                        "ShaftPluginResetService must be registered as an applicationService"));
    }

    @Test
    void pluginXmlRegistersThePostStartupActivityAndTheClassResolves() throws Exception {
        // ShaftPluginUpgradeActivity must be registered as a postStartupActivity or it never fires at
        // IDE runtime, silently disabling plugin-upgrade auto-reset.
        String pluginXml = Files.readString(Path.of("src/main/resources/META-INF/plugin.xml"));

        assertTrue(pluginXml.contains(
                "<postStartupActivity implementation=\"com.shaft.intellij.settings.ShaftPluginUpgradeActivity\"/>"),
                "ShaftPluginUpgradeActivity must be registered as a postStartupActivity");
        assertDoesNotThrow(() -> Class.forName("com.shaft.intellij.settings.ShaftPluginUpgradeActivity"),
                "The registered class must resolve");
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
