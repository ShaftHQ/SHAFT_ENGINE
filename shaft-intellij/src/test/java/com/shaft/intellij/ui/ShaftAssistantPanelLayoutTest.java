package com.shaft.intellij.ui;

import com.shaft.intellij.settings.ShaftSettingsState;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.util.messages.MessageBus;
import com.intellij.util.messages.MessageBusConnection;
import org.junit.jupiter.api.Test;

import javax.swing.JComponent;
import javax.swing.JButton;
import javax.swing.JScrollPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JToggleButton;
import javax.swing.SwingUtilities;
import java.awt.Component;
import java.awt.Container;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.lang.reflect.Field;
import java.lang.reflect.Proxy;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertAll;

/**
 * Covers issue #3694: the notices banner strip (setup notice + fresh-project hint) above the chat
 * header used {@link java.awt.GridLayout}, which -- unlike {@link java.awt.BorderLayout} or
 * {@link javax.swing.BoxLayout} -- reserves layout space for invisible children instead of
 * collapsing them to zero height. In a normal, already-configured, non-fresh project (the
 * everyday "fresh/new-chat Assistant panel" state), both banners are {@code setVisible(false)},
 * yet {@code GridLayout(0, 1)} still reserved two rows' worth of blank height above the
 * "New chat" dropdown, reading as a large empty gap between the panel header and the dropdown.
 */
class ShaftAssistantPanelLayoutTest {

    @Test
    void environmentBackedCloudRouteNamesItsSourceAndHidesManualKeyEntry() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        settings.cloudProvider = "openai";
        settings.passProviderApiKeysToMcp = true;
        settings.setProviderApiKeyEnvironmentVariable("openai", "OPENAI_API_KEY");
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        setField(panel, "providerEnvironmentLookup",
                (java.util.function.Function<String, String>) name -> "OPENAI_API_KEY".equals(name)
                        ? "never-render-this-secret" : null);
        ((javax.swing.JComboBox<?>) fieldOf(panel, "cloudProvider")).setSelectedItem("openai");
        setField(panel, "providerKeyReplacementRequired", false);
        setField(panel, "providerModelState", "AVAILABLE");
        java.lang.reflect.Method update = ShaftAssistantPanel.class.getDeclaredMethod("updateCloudKeyStatus");
        update.setAccessible(true);
        update.invoke(panel);

        JLabel status = (JLabel) fieldOf(panel, "cloudKeyStatus");
        javax.swing.JPasswordField key = (javax.swing.JPasswordField) fieldOf(panel, "cloudApiKey");
        JButton save = (JButton) fieldOf(panel, "saveCloudApiKey");

        assertAll(
                () -> assertTrue(status.getText().contains("OPENAI_API_KEY")),
                () -> assertFalse(status.getText().contains("never-render-this-secret")),
                () -> assertFalse(key.isVisible()),
                () -> assertFalse(save.isVisible()));
    }

    @Test
    void missingSelectedEnvironmentCredentialDoesNotFallBackToPasswordSafe() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        settings.cloudProvider = "openai";
        settings.passProviderApiKeysToMcp = true;
        settings.setProviderApiKeyEnvironmentVariable("openai", "OPENAI_API_KEY");
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        setField(panel, "providerEnvironmentLookup",
                (java.util.function.Function<String, String>) ignored -> null);
        setField(panel, "selectedCloudKeySupplier", (java.util.function.BooleanSupplier) () -> true);
        ((javax.swing.JComboBox<?>) fieldOf(panel, "cloudProvider")).setSelectedItem("openai");

        java.lang.reflect.Method hasKey = ShaftAssistantPanel.class.getDeclaredMethod("hasSelectedCloudKey");
        hasKey.setAccessible(true);
        java.lang.reflect.Method update = ShaftAssistantPanel.class.getDeclaredMethod("updateCloudKeyStatus");
        update.setAccessible(true);
        update.invoke(panel);
        JLabel status = (JLabel) fieldOf(panel, "cloudKeyStatus");

        assertAll(
                () -> assertFalse((boolean) hasKey.invoke(panel)),
                () -> assertTrue(status.getText().contains("OPENAI_API_KEY")),
                () -> assertTrue(status.getText().contains("not configured")),
                () -> assertFalse(status.getText().contains("key stored")));
    }

    @Test
    void rejectedEnvironmentCredentialNamesTheVariableAndRecoveryOwner() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        settings.cloudProvider = "openai";
        settings.passProviderApiKeysToMcp = true;
        settings.setProviderApiKeyEnvironmentVariable("openai", "OPENAI_API_KEY");
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        setField(panel, "providerEnvironmentLookup",
                (java.util.function.Function<String, String>) ignored -> "rejected-secret");
        ((javax.swing.JComboBox<?>) fieldOf(panel, "cloudProvider")).setSelectedItem("openai");
        setField(panel, "providerKeyReplacementRequired", true);
        java.lang.reflect.Method update = ShaftAssistantPanel.class.getDeclaredMethod("updateCloudKeyStatus");
        update.setAccessible(true);
        update.invoke(panel);

        JLabel status = (JLabel) fieldOf(panel, "cloudKeyStatus");
        assertAll(
                () -> assertTrue(status.getText().contains("OPENAI_API_KEY was rejected")),
                () -> assertTrue(status.getText().contains("environment")),
                () -> assertFalse(status.getText().contains("rejected-secret")));
    }

    @Test
    void presentEnvironmentCredentialReportsForwardingDisabledWhenForwardingIsOff() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        settings.cloudProvider = "openai";
        settings.passProviderApiKeysToMcp = false;
        settings.setProviderApiKeyEnvironmentVariable("openai", "OPENAI_API_KEY");
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        setField(panel, "providerEnvironmentLookup",
                (java.util.function.Function<String, String>) ignored -> "present-secret");
        ((javax.swing.JComboBox<?>) fieldOf(panel, "cloudProvider")).setSelectedItem("openai");
        setField(panel, "providerModelState", "KEY_FORWARDING_DISABLED");
        java.lang.reflect.Method update = ShaftAssistantPanel.class.getDeclaredMethod("updateCloudKeyStatus");
        update.setAccessible(true);
        update.invoke(panel);

        String status = ((JLabel) fieldOf(panel, "cloudKeyStatus")).getText();
        assertAll(
                () -> assertTrue(status.toLowerCase(java.util.Locale.ROOT).contains("forwarding")),
                () -> assertFalse(status.contains("not configured")),
                () -> assertFalse(status.contains("present-secret")));
    }

    @Test
    void providerModelSelectorStartsDisabledUntilProviderDiscoveryReturns() throws ReflectiveOperationException {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, ShaftAssistantChatState.getInstance(null));

        javax.swing.JComboBox<?> model = (javax.swing.JComboBox<?>) fieldOf(panel, "cloudModel");

        assertAll(
                () -> assertFalse(model.isEnabled(), "a provider model cannot be selected before discovery"),
                () -> assertEquals(0, model.getItemCount(), "the panel must not render a static provider model catalog"));
    }

    @Test
    void providerDiscoveryRendersOnlyReturnedModelIdsAndDisablesUnavailableState() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, ShaftAssistantChatState.getInstance(null));
        javax.swing.JComboBox<?> model = (javax.swing.JComboBox<?>) fieldOf(panel, "cloudModel");

        applyProviderModels(panel, "gemini", "AVAILABLE", List.of("returned-z", "returned-a"));

        assertAll(
                () -> assertTrue(model.isEnabled()),
                () -> assertEquals(2, model.getItemCount()),
                () -> assertEquals("returned-a", model.getItemAt(0)),
                () -> assertEquals("returned-z", model.getItemAt(1)));

        applyProviderModels(panel, "gemini", "UNAVAILABLE", List.of("must-not-render"));

        assertAll(
                () -> assertFalse(model.isEnabled()),
                () -> assertEquals(0, model.getItemCount()),
                () -> assertTrue(model.getAccessibleContext().getAccessibleDescription().contains("unavailable")));
    }

    @Test
    void lockedCloudRouteRefreshesItsCurrentConfigurationWhenDiscoveryOrSelectionChanges() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        settings.cloudProvider = "gemini";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState(), () -> { });
        javax.swing.JComboBox<String> model = (javax.swing.JComboBox<String>) fieldOf(panel, "cloudModel");
        JLabel configuration = (JLabel) fieldOf(panel, "currentAgentConfiguration");

        applyProviderModels(panel, "gemini", "AVAILABLE", List.of("gemini-a", "gemini-b"));
        model.setSelectedItem("gemini-b");

        assertEquals("Gemini provider gemini-b", configuration.getText());
        assertEquals("Agent: Gemini provider / gemini-b", configuration.getToolTipText());
    }

    @Test
    void lockedUnavailableCloudRouteShowsAccessibleRetryAction() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState(), () -> { });
        JButton retry = (JButton) fieldOf(panel, "retryProviderModels");

        applyProviderModels(panel, "gemini", "UNAVAILABLE", List.of());

        assertAll(
                () -> assertTrue(retry.isVisible()),
                () -> assertTrue(retry.isEnabled()),
                () -> assertEquals("Retry provider models", retry.getText()),
                () -> assertEquals("Retry provider models", retry.getAccessibleContext().getAccessibleName()));
    }

    @Test
    void providerDiscoveryRejectsWrongSchemaProviderStateAndNonStringModels() throws Exception {
        assertEquals(List.of(), providerModelIds("""
                {"schemaVersion":"2.0","provider":"other","state":"UNKNOWN","modelIds":["safe",7,true]}
                """), "discovery data is untrusted until every contract field validates");
    }

    @Test
    void providerDiscoveryRejectsUnsafeModelIdsAndNonStringSchemaFields() throws Exception {
        assertAll(
                () -> assertEquals(List.of("models/gemini-2.5-flash", "llama3.2:latest", "org/model"), providerModelIds("""
                        {"schemaVersion":"1.0","provider":"gemini","state":"AVAILABLE","modelIds":["models/gemini-2.5-flash","llama3.2:latest","org/model"]}
                        """), "real provider model-ID shapes remain accepted"),
                () -> assertEquals(List.of(), providerModelIds("""
                        {"schemaVersion":1.0,"provider":"gemini","state":"AVAILABLE","modelIds":["models/gemini-2.5-flash"]}
                        """), "schemaVersion is a string-only wire field"),
                () -> assertEquals(List.of(), providerModelIds("""
                        {"schemaVersion":"1.0","provider":"gemini","state":"AVAILABLE","modelIds":["Authorization: Bearer https://endpoint.example/?api_key=leak"]}
                        """), "header- and endpoint-looking model IDs must never reach UI text"));
    }

    @Test
    void providerDiscoveryRejectsBareCredentialsAndSchemelessEndpointsEverywhere() throws Exception {
        List<String> canaries = List.of("sk-proj-abc123", "AIzaSyAbc", "github_pat_abc", "ghp_abc123",
                "xoxb-abc123", "xapp-abc123", "rk_live_abc123", "whsec_abc123", "sk_live_abc123",
                "localhost:11434", "192.168.1.9:8080", "provider.example:443",
                "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJzZWNyZXQifQ.signature", "glpat-abc123", "hf_Abc123", "npm_Abc123");
        assertEquals(List.of(), providerModelIds("""
                {"schemaVersion":"1.0","provider":"gemini","state":"AVAILABLE","modelIds":["sk-proj-abc123","AIzaSyAbc","github_pat_abc","ghp_abc123","xoxb-abc123","xapp-abc123","rk_live_abc123","whsec_abc123","sk_live_abc123","localhost:11434","192.168.1.9:8080","provider.example:443","eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJzZWNyZXQifQ.signature","glpat-abc123","hf_Abc123","npm_Abc123"]}
                """));

        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        javax.swing.JComboBox<?> model = (javax.swing.JComboBox<?>) fieldOf(panel, "cloudModel");
        applyProviderModels(panel, "gemini", "AVAILABLE", canaries);

        String visibleText = ((JLabel) fieldOf(panel, "cloudKeyStatus")).getText()
                + model.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(0, model.getItemCount()),
                () -> canaries.forEach(canary -> assertFalse(visibleText.contains(canary), canary)));
    }

    @Test
    void providerDiscoveryRejectsAwsAccessKeyIds() throws Exception {
        assertEquals(List.of(), providerModelIds("""
                {"schemaVersion":"1.0","provider":"gemini","state":"AVAILABLE","modelIds":["AKIA1234567890ABCDEF","ASIA1234567890ABCDEF"]}
                """));
    }

    @Test
    void localProviderWithoutGatewayKeyCanUseAnAvailableModel() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        settings.ollamaEndpoint = "http://localhost:11434";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        ((javax.swing.JComboBox<?>) fieldOf(panel, "cloudProvider")).setSelectedItem("ollama");
        ((javax.swing.JTextArea) fieldOf(panel, "prompt")).setText("answer this");
        setField(panel, "selectedCloudKeySupplier", (java.util.function.BooleanSupplier) () -> false);
        setField(panel, "providerModelRequestKey", "ollama|http://localhost:11434|false");
        applyProviderModels(panel, "ollama", "AVAILABLE", List.of("llama3.2:latest"), 0L);

        invokeSendIgnoringRequestBoundary(panel);

        assertEquals("answer this", fieldValue(panel, "lastPrompt"));
    }

    @Test
    void reattachedCloudPanelStartsCurrentDiscoveryAndAcceptsItsResult() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        settings.ollamaEndpoint = "http://localhost:11434";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        ((javax.swing.JComboBox<?>) fieldOf(panel, "cloudProvider")).setSelectedItem("ollama");
        long before = (long) fieldValue(panel, "providerModelRequestGeneration");
        panel.removeNotify();
        long detached = (long) fieldValue(panel, "providerModelRequestGeneration");

        Disposable applicationDisposable = Disposer.newDisposable("provider-model-reattach-test");
        MessageBusConnection messageBusConnection = (MessageBusConnection) Proxy.newProxyInstance(
                MessageBusConnection.class.getClassLoader(), new Class<?>[] { MessageBusConnection.class },
                (proxy, method, arguments) -> primitiveDefault(method.getReturnType()));
        MessageBus messageBus = (MessageBus) Proxy.newProxyInstance(
                MessageBus.class.getClassLoader(), new Class<?>[] { MessageBus.class },
                (proxy, method, arguments) -> "connect".equals(method.getName())
                        ? messageBusConnection : primitiveDefault(method.getReturnType()));
        Application fakeApplication = (Application) Proxy.newProxyInstance(
                Application.class.getClassLoader(), new Class<?>[] { Application.class },
                (proxy, method, arguments) -> "getMessageBus".equals(method.getName())
                        ? messageBus : primitiveDefault(method.getReturnType()));
        ApplicationManager.setApplication(fakeApplication, applicationDisposable);
        try {
            try {
                panel.addNotify();
            } catch (NullPointerException ignored) {
                // The request reaches the unavailable MCP service boundary after it starts.
            }
        } finally {
            Disposer.dispose(applicationDisposable);
            ApplicationManager.setApplication(null);
        }
        long reattached = (long) fieldValue(panel, "providerModelRequestGeneration");
        try {
            applyProviderModels(panel, "ollama", "AVAILABLE", List.of("llama3.2:latest"), reattached);

            assertAll(
                    () -> assertTrue(detached > before),
                    () -> assertTrue(reattached > detached, "reattaching must start current discovery"),
                    () -> assertTrue(((javax.swing.JComboBox<?>) fieldOf(panel, "cloudModel")).isEnabled()),
                    () -> assertTrue(((JButton) fieldOf(panel, "send")).isEnabled()));
        } finally {
            panel.removeNotify();
        }
    }

    @Test
    void changedProviderEndpointInvalidatesCachedModelsBeforeSend() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        settings.ollamaEndpoint = "http://old-endpoint:11434";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        ((javax.swing.JComboBox<?>) fieldOf(panel, "cloudProvider")).setSelectedItem("ollama");
        ((javax.swing.JTextArea) fieldOf(panel, "prompt")).setText("answer this");
        setField(panel, "providerModelRequestKey", "ollama|http://old-endpoint:11434|true");
        applyProviderModels(panel, "ollama", "AVAILABLE", List.of("llama3.2:latest"), 0L);
        settings.ollamaEndpoint = "http://new-endpoint:11434";

        invokeSendIgnoringRequestBoundary(panel);

        assertAll(
                () -> assertEquals("", fieldValue(panel, "lastPrompt")),
                () -> assertEquals(0, ((javax.swing.JComboBox<?>) fieldOf(panel, "cloudModel")).getItemCount()),
                () -> assertEquals("VALIDATING", fieldValue(panel, "providerModelState")));

        setField(panel, "providerModelRequestKey", "ollama|http://new-endpoint:11434|true");
        applyProviderModels(panel, "ollama", "AVAILABLE", List.of("llama3.2:latest"), 0L);
        invokeSendIgnoringRequestBoundary(panel);
        assertEquals("answer this", fieldValue(panel, "lastPrompt"), "only a current matching result may pass Send");
    }

    @Test
    void newlyPresentCloudKeyInvalidatesCachedModelsBeforeSend() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        ((javax.swing.JTextArea) fieldOf(panel, "prompt")).setText("answer this");
        setField(panel, "selectedCloudKeySupplier", (java.util.function.BooleanSupplier) () -> false);
        setField(panel, "providerModelRequestKey", "gemini||false");
        applyProviderModels(panel, "gemini", "AVAILABLE", List.of("models/gemini-2.5-flash"), 0L);
        setField(panel, "selectedCloudKeySupplier", (java.util.function.BooleanSupplier) () -> true);

        invokeSendIgnoringRequestBoundary(panel);

        assertAll(
                () -> assertEquals("", fieldValue(panel, "lastPrompt")),
                () -> assertEquals(0, ((javax.swing.JComboBox<?>) fieldOf(panel, "cloudModel")).getItemCount()),
                () -> assertEquals("VALIDATING", fieldValue(panel, "providerModelState")));
    }

    @Test
    void matchingAvailableDiscoverySurvivesRunStateChangesWithoutResettingTheModel() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        settings.ollamaEndpoint = "http://localhost:11434";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        javax.swing.JComboBox<?> model = (javax.swing.JComboBox<?>) fieldOf(panel, "cloudModel");
        ((javax.swing.JComboBox<?>) fieldOf(panel, "cloudProvider")).setSelectedItem("ollama");
        setField(panel, "providerModelRequestKey", "ollama|http://localhost:11434|true");
        setField(panel, "providerModelRequestGeneration", 700L);
        applyProviderModels(panel, "ollama", "AVAILABLE", List.of("llama3.2:latest"), 0L);

        Disposable applicationDisposable = Disposer.newDisposable("provider-model-cache-test");
        Application fakeApplication = (Application) Proxy.newProxyInstance(
                Application.class.getClassLoader(), new Class<?>[] { Application.class },
                (proxy, method, arguments) -> primitiveDefault(method.getReturnType()));
        ApplicationManager.setApplication(fakeApplication, applicationDisposable);
        try {
            panel.setRunning(true, "Thinking");
            panel.setRunning(false, "Ready");
        } finally {
            Disposer.dispose(applicationDisposable);
            ApplicationManager.setApplication(null);
        }

        assertAll(
                () -> assertEquals("llama3.2:latest", model.getSelectedItem()),
                () -> assertEquals(700L, fieldValue(panel, "providerModelRequestGeneration")),
                () -> assertEquals("AVAILABLE", fieldValue(panel, "providerModelState")));
    }

    @Test
    void detachedPanelRejectsLateProviderModels() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        javax.swing.JComboBox<?> model = (javax.swing.JComboBox<?>) fieldOf(panel, "cloudModel");
        AtomicBoolean cancelled = new AtomicBoolean();
        setField(panel, "providerModelInvocation", new ShaftMcpInvocation(new CompletableFuture<>(),
                () -> cancelled.set(true)));
        setField(panel, "providerModelRequestGeneration", 800L);

        panel.removeNotify();
        applyProviderModels(panel, "gemini", "AVAILABLE", List.of("models/gemini-2.5-flash"), 800L);

        assertAll(
                () -> assertEquals(0, model.getItemCount(), "a detached panel must ignore late discovery results"),
                () -> assertFalse(model.isEnabled()),
                () -> assertTrue(cancelled.get(), "detaching must cancel the active provider discovery"),
                () -> assertEquals(null, fieldValue(panel, "providerModelInvocation")));
    }

    @Test
    void unavailableAndValidatingProviderStatesStayVisibleAndAuthenticationFailureOffersReplacement() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        javax.swing.JComboBox<?> model = (javax.swing.JComboBox<?>) fieldOf(panel, "cloudModel");
        JButton send = (JButton) fieldOf(panel, "send");
        JLabel keyStatus = (JLabel) fieldOf(panel, "cloudKeyStatus");
        javax.swing.JPasswordField key = (javax.swing.JPasswordField) fieldOf(panel, "cloudApiKey");

        invokeUnchecked(panel, "invalidateProviderModels");

        assertAll(
                () -> assertFalse(model.isEnabled()),
                () -> assertFalse(send.isEnabled()),
                () -> assertTrue(keyStatus.isVisible(), "validating discovery must be visible"),
                () -> assertTrue(keyStatus.getText().contains("Validating")),
                () -> assertTrue(model.getAccessibleContext().getAccessibleDescription().contains("Validating")));

        applyProviderModels(panel, "gemini", "AUTHENTICATION_FAILED", List.of());

        assertAll(
                () -> assertTrue(keyStatus.isVisible()),
                () -> assertTrue(key.isVisible(), "an invalid stored key must be replaceable"),
                () -> assertTrue(keyStatus.getText().contains("Replace invalid Gemini key")),
                () -> assertTrue(model.getAccessibleContext().getAccessibleDescription().contains("Replace invalid Gemini key")));
    }

    @Test
    void localProvidersHaveUnambiguousProviderLabelsAndNoCloudSummary() throws Exception {
        assertAll(
                () -> assertEquals("Ollama (local)", ShaftUiLabels.friendly("ollama")),
                () -> assertEquals("LM Studio (local)", ShaftUiLabels.friendly("lmstudio")));

        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        ((javax.swing.JComboBox<?>) fieldOf(panel, "cloudProvider")).setSelectedItem("ollama");

        String summary = ((JToggleButton) fieldOf(panel, "runSettingsToggle")).getText();
        assertAll(
                () -> assertTrue(summary.contains("Ollama (local)")),
                () -> assertFalse(summary.toLowerCase(java.util.Locale.ROOT).contains("cloud")));
    }

    @Test
    void providerSendStaysDisabledUntilAnAvailableReturnedModelIsSelected() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        javax.swing.JButton send = (javax.swing.JButton) fieldOf(panel, "send");
        javax.swing.JTextArea prompt = (javax.swing.JTextArea) fieldOf(panel, "prompt");
        JLabel providerStatus = (JLabel) fieldOf(panel, "cloudKeyStatus");

        for (String state : List.of("VALIDATING", "KEY_NEEDED", "AUTHENTICATION_FAILED", "UNAVAILABLE", "EMPTY")) {
            applyProviderModels(panel, "gemini", state, List.of());
            prompt.setText("answer this");
            invokeSend(panel);
            assertAll(
                    () -> assertFalse(send.isEnabled(), state + " must fail closed without a provider chat request"),
                    () -> assertTrue(providerStatus.isVisible(), state + " must remain visible to the user"));
        }

        applyProviderModels(panel, "gemini", "AVAILABLE", List.of("returned-model"));
        assertTrue(send.isEnabled(), "an available returned model enables provider chat");
    }

    @Test
    void providerSwitchRejectsStaleModelsUntilTheCurrentProviderResultArrives() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        javax.swing.JComboBox<?> provider = (javax.swing.JComboBox<?>) fieldOf(panel, "cloudProvider");
        javax.swing.JComboBox<?> model = (javax.swing.JComboBox<?>) fieldOf(panel, "cloudModel");
        JButton send = (JButton) fieldOf(panel, "send");
        ((javax.swing.JTextArea) fieldOf(panel, "prompt")).setText("answer this");
        setField(panel, "providerModelRequestGeneration", 100L);

        provider.setSelectedItem("openai");
        applyProviderModels(panel, "gemini", "AVAILABLE", List.of("stale-gemini"), 100L);

        assertAll(
                () -> assertFalse(model.isEnabled(), "a previous provider's completion must not enable its model picker"),
                () -> assertEquals(0, model.getItemCount(), "a previous provider's models must not render"),
                () -> assertFalse(send.isEnabled(), "Send stays closed until the current provider completes"));

        applyProviderModels(panel, "openai", "AVAILABLE", List.of("current-openai"), 101L);

        assertAll(
                () -> assertTrue(model.isEnabled()),
                () -> assertEquals("current-openai", model.getItemAt(0)),
                () -> assertTrue(send.isEnabled()));
    }

    @Test
    void successfulSameProviderKeySaveRejectsItsPreSaveModelCompletion() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        settings.passProviderApiKeysToMcp = true;
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState(), null,
                (keyName, secret) -> CompletableFuture.completedFuture(null));
        javax.swing.JComboBox<?> model = (javax.swing.JComboBox<?>) fieldOf(panel, "cloudModel");
        JButton send = (JButton) fieldOf(panel, "send");
        ((javax.swing.JTextArea) fieldOf(panel, "prompt")).setText("answer this");
        ((javax.swing.JPasswordField) fieldOf(panel, "cloudApiKey")).setText("key");
        setField(panel, "providerModelRequestGeneration", 200L);

        SwingUtilities.invokeAndWait(() -> invokeUnchecked(panel, "saveCloudApiKey"));
        applyProviderModels(panel, "gemini", "AVAILABLE", List.of("stale-before-save"), 200L);

        assertAll(
                () -> assertTrue(settings.passProviderApiKeysToMcp),
                () -> assertFalse(model.isEnabled(), "the pre-save completion must remain stale"),
                () -> assertEquals(0, model.getItemCount()),
                () -> assertFalse(send.isEnabled()));

        applyProviderModels(panel, "gemini", "AVAILABLE", List.of("current-after-save"), 201L);

        assertAll(
                () -> assertTrue(model.isEnabled()),
                () -> assertEquals("current-after-save", model.getItemAt(0)),
                () -> assertTrue(send.isEnabled()));
    }

    @Test
    void unavailableProviderReasonAndRetrySurviveModeChange() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState(), () -> { });
        JLabel keyStatus = (JLabel) fieldOf(panel, "cloudKeyStatus");
        JButton retry = (JButton) fieldOf(panel, "retryProviderModels");

        applyProviderModels(panel, "gemini", "UNAVAILABLE", List.of());
        ((javax.swing.JComboBox<?>) fieldOf(panel, "mode")).setSelectedItem("ASK");

        assertAll(
                () -> assertTrue(keyStatus.getText().contains("unavailable")),
                () -> assertTrue(retry.isVisible() && retry.isEnabled()));
    }

    @Test
    void savingKeyKeepsDisabledForwardingOptInAndShowsSafeDiscoveryState() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState(), null,
                (keyName, secret) -> CompletableFuture.completedFuture(null));
        ((javax.swing.JPasswordField) fieldOf(panel, "cloudApiKey")).setText("key");

        SwingUtilities.invokeAndWait(() -> invokeUnchecked(panel, "saveCloudApiKey"));

        assertAll(
                () -> assertFalse(settings.passProviderApiKeysToMcp),
                () -> assertEquals("KEY_FORWARDING_DISABLED", fieldValue(panel, "providerModelState")),
                () -> assertTrue(((JLabel) fieldOf(panel, "cloudKeyStatus")).getText().contains("forwarding")));
    }

    @Test
    void disablingKeyForwardingInvalidatesAvailableModelsBeforeSend() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        settings.passProviderApiKeysToMcp = true;
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
        setField(panel, "selectedCloudKeySupplier", (java.util.function.BooleanSupplier) () -> true);
        setField(panel, "providerModelRequestKey", "gemini||true");
        setField(panel, "providerModelForwardingEnabled", true);
        applyProviderModels(panel, "gemini", "AVAILABLE", List.of("safe-model"));
        ((javax.swing.JTextArea) fieldOf(panel, "prompt")).setText("answer this");
        settings.passProviderApiKeysToMcp = false;

        invokeSendIgnoringRequestBoundary(panel);

        assertAll(
                () -> assertEquals("VALIDATING", fieldValue(panel, "providerModelState")),
                () -> assertFalse(((JButton) fieldOf(panel, "send")).isEnabled()));
    }

    @Test
    void rejectedCredentialDispatchClearsSecretWithoutChangingSettingsOrDiscovery() throws Exception {
        ShaftSettingsState.Settings settings = readySettingsForExistingProject();
        settings.assistantProviderType = "CLOUD";
        AtomicReference<char[]> captured = new AtomicReference<>();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState(), null,
                (keyName, secret) -> {
                    captured.set(secret);
                    throw new AssertionError("dispatch rejected");
                });
        ((javax.swing.JPasswordField) fieldOf(panel, "cloudApiKey")).setText("secret");
        setField(panel, "providerModelRequestGeneration", 300L);
        setField(panel, "providerModelRequestKey", "gemini|unchanged");

        assertThrows(java.lang.reflect.InvocationTargetException.class,
                () -> SwingUtilities.invokeAndWait(() -> invokeUnchecked(panel, "saveCloudApiKey")));

        assertAll(
                () -> assertArrayEquals(new char[6], captured.get(), "the rejected dispatch must clear its captured secret"),
                () -> assertFalse(settings.passProviderApiKeysToMcp),
                () -> assertEquals(300L, fieldValue(panel, "providerModelRequestGeneration")),
                () -> assertEquals("gemini|unchanged", fieldValue(panel, "providerModelRequestKey")));
    }

    @Test
    void noticesContainerCollapsesToZeroHeightWhenBothBannersAreHidden() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, readySettingsForExistingProject(), ShaftAssistantChatState.getInstance(null));

        JPanel notices = noticesOf(panel);

        assertTrue(notices.getComponentCount() >= 1, "the notices strip should still contain the banner panels");
        assertEquals(0, notices.getPreferredSize().height,
                "both banners are hidden (MCP configured, no project to flag as fresh), so the notices "
                        + "strip must not reserve any height above the chat header");
    }

    /**
     * Issue #4316: {@code currentAgentConfiguration} (the "Claude CLI"-style label) and
     * {@code configure} (the settings gear) used to be added to {@code routeRow} as two bare
     * controls, reading as clutter among the ~13 other dropdowns/checkboxes in that row. They now
     * share one bordered chip, mirroring the {@code allowSourceMutationChip} idiom already used in
     * this file for {@code allowSourceMutation}.
     */
    @Test
    void currentAgentChipGroupsLabelAndGearButtonWhenRouteIsLocked() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, readySettingsForExistingProject(), ShaftAssistantChatState.getInstance(null), () -> { });

        JPanel chip = currentAgentChipOf(panel);
        Component label = fieldOf(panel, "currentAgentConfiguration");
        Component gear = fieldOf(panel, "configure");

        assertTrue(chip.isVisible(), "route is locked (MCP configured + setup flow present), so the chip "
                + "grouping the current-agent label and settings gear must be visible");
        assertTrue(containsComponent(chip, label), "chip must contain the current-agent-configuration label");
        assertTrue(containsComponent(chip, gear), "chip must contain the configure/settings gear button");
    }

    /** Same route-locked gate {@code currentAgentConfiguration}/{@code configure} already used individually. */
    @Test
    void currentAgentChipHiddenWhenRouteIsNotLocked() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, new ShaftSettingsState.Settings(), ShaftAssistantChatState.getInstance(null));

        JPanel chip = currentAgentChipOf(panel);

        assertFalse(chip.isVisible(), "no setup flow/MCP configuration -- the route is not locked, so the "
                + "grouped chip must stay hidden exactly like the two controls it wraps used to");
    }

    @Test
    void runSettingsDisclosureStartsCollapsedAndKeepsRouteControlsTogether() throws ReflectiveOperationException {
        ShaftSettingsState.Settings configured = readySettingsForExistingProject();
        configured.assistantFamily = "CODEX";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, configured, ShaftAssistantChatState.getInstance(null));

        JToggleButton toggle = (JToggleButton) fieldOf(panel, "runSettingsToggle");
        JPanel settings = (JPanel) fieldOf(panel, "runSettingsPanel");
        Component mode = fieldOf(panel, "mode");

        assertAll(
                () -> assertEquals("Run settings", toggle.getAccessibleContext().getAccessibleName()),
                () -> assertFalse(toggle.isSelected(), "the everyday composer must start compact"),
                () -> assertFalse(settings.isVisible(), "route and configuration controls belong behind Run settings"),
                () -> assertTrue(containsDescendant(settings, mode), "mode must remain in the settings disclosure"),
                () -> assertTrue(toggle.getText().contains("CLI"),
                        "the collapsed summary must name the effective agent/runtime"),
                () -> assertTrue(toggle.getText().toLowerCase(java.util.Locale.ROOT).contains("effort"),
                        "the collapsed summary must name the selected effort"));

        toggle.doClick();

        assertTrue(settings.isVisible(), "Run settings must expand with the keyboard-accessible toggle");

        ((javax.swing.JComboBox<?>) mode).setSelectedItem("PLAN");
        assertTrue(toggle.getText().contains("Plan"), "the summary must update with the selected mode");
    }

    @Test
    void expandedRunSettingsUseOneAlignedNativeSettingsGroup() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, readySettingsForExistingProject(), ShaftAssistantChatState.getInstance(null));
        JPanel settings = (JPanel) fieldOf(panel, "runSettingsPanel");

        assertTrue(Arrays.stream(settings.getComponents()).allMatch(component -> component instanceof Container row
                        && row.getLayout() instanceof BorderLayout),
                "Run settings must use aligned label/control rows, not independent FlowLayout rows");
    }

    @Test
    void expandedRunSettingsStackLabelsAboveControlsAtNarrowWidth() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, readySettingsForExistingProject(), ShaftAssistantChatState.getInstance(null));
        JToggleButton toggle = (JToggleButton) fieldOf(panel, "runSettingsToggle");
        JPanel settings = (JPanel) fieldOf(panel, "runSettingsPanel");
        JScrollPane settingsScroll = (JScrollPane) fieldOf(panel, "runSettingsScroll");

        SwingUtilities.invokeAndWait(() -> {
            panel.setSize(new Dimension(860, 780));
            layoutTree(panel);
            toggle.doClick();
            panel.setSize(new Dimension(360, 780));
            layoutTree(panel);
            layoutTree(panel);
        });

        assertTrue(settingsScroll.getVerticalScrollBar().isVisible(),
                "the narrow disclosure must scroll rather than clip settings rows vertically");

        for (Component component : settings.getComponents()) {
            if (!(component instanceof JPanel row) || !row.isVisible()) {
                continue;
            }
            assertTrue(row.getLayout() instanceof javax.swing.BoxLayout,
                    "narrow Run settings must use a one-column label/control layout (settings width "
                            + settings.getWidth() + ")");
            Component label = row.getComponent(0);
            Component control = row.getComponent(1);
            assertAll(
                    () -> assertTrue(control.getY() >= label.getY() + label.getHeight(),
                            "the control must sit below its label at narrow width: " + ((JLabel) label).getText()
                                    + " (label=" + label.getBounds() + ", control=" + control.getBounds() + ")"),
                    () -> assertTrue(control.getX() >= 0 && control.getX() + control.getWidth() <= row.getWidth(),
                            "the narrow control must remain contained within its settings row"));
        }
    }

    @Test
    void activeRunUsesOneStableStatusStripWithCancelOnTheRight() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                null, readySettingsForExistingProject(), ShaftAssistantChatState.getInstance(null));
        JPanel statusStrip = (JPanel) fieldOf(panel, "transcriptStatusStrip");
        JPanel actionRow = (JPanel) fieldOf(panel, "actionRow");
        JButton cancel = (JButton) fieldOf(panel, "cancel");
        JProgressBar progress = (JProgressBar) fieldOf(panel, "progress");
        JLabel status = (JLabel) fieldOf(panel, "status");

        panel.setRunning(true, "Thinking...");
        panel.setSize(new Dimension(360, 780));
        layoutTree(panel);

        BorderLayout layout = (BorderLayout) statusStrip.getLayout();
        assertAll(
                () -> assertTrue(statusStrip.isVisible(), "the active run status strip must remain visible"),
                () -> assertEquals(cancel, layout.getLayoutComponent(BorderLayout.EAST),
                        "Cancel must occupy the stable right edge of the status strip"),
                () -> assertTrue(containsDescendant(statusStrip, progress)),
                () -> assertTrue(containsDescendant(statusStrip, status)),
                () -> assertFalse(containsDescendant(actionRow, cancel),
                        "Cancel must not be duplicated in the transcript action row"),
                () -> assertTrue(cancel.isVisible() && cancel.isEnabled()),
                () -> assertTrue(cancel.getX() > status.getX(), "Cancel must remain to the right of status text"),
                () -> assertTrue(cancel.getX() + cancel.getWidth() <= statusStrip.getWidth(),
                        "Cancel must remain contained at narrow width"));

        panel.setRunning(false, "Ready");
        assertFalse(statusStrip.isVisible(), "the status strip must collapse when the run finishes");
    }

    /** MCP configured (hides the setup notice) with a {@code null} project (never "fresh", hides that notice too). */
    private static ShaftSettingsState.Settings readySettingsForExistingProject() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        return settings;
    }

    private static JPanel noticesOf(ShaftAssistantPanel panel) throws ReflectiveOperationException {
        Field field = ShaftAssistantPanel.class.getDeclaredField("notices");
        field.setAccessible(true); // NOPMD - test-only field injection, matching the established pattern in ShaftPanelSetupTest
        return (JPanel) field.get(panel);
    }

    private static JPanel currentAgentChipOf(ShaftAssistantPanel panel) throws ReflectiveOperationException {
        return (JPanel) fieldOf(panel, "currentAgentChip");
    }

    private static Component fieldOf(ShaftAssistantPanel panel, String name) throws ReflectiveOperationException {
        Field field = ShaftAssistantPanel.class.getDeclaredField(name);
        field.setAccessible(true); // NOPMD - test-only field injection, matching the established pattern in ShaftPanelSetupTest
        return (Component) field.get(panel);
    }

    private static Object fieldValue(ShaftAssistantPanel panel, String name) throws ReflectiveOperationException {
        Field field = ShaftAssistantPanel.class.getDeclaredField(name);
        field.setAccessible(true); // NOPMD - test-only observation of the async request lifecycle
        return field.get(panel);
    }

    private static void setField(ShaftAssistantPanel panel, String name, Object value) throws ReflectiveOperationException {
        Field field = ShaftAssistantPanel.class.getDeclaredField(name);
        field.setAccessible(true); // NOPMD - test-only setup for deterministic stale completions
        field.set(panel, value);
    }

    private static void applyProviderModels(
            ShaftAssistantPanel panel, String provider, String state, List<String> models) throws Exception {
        applyProviderModels(panel, provider, state, models, 0L);
    }

    private static void applyProviderModels(
            ShaftAssistantPanel panel, String provider, String state, List<String> models, long generation) throws Exception {
        java.lang.reflect.Method method = ShaftAssistantPanel.class.getDeclaredMethod(
                "applyProviderModels", String.class, String.class, List.class, long.class);
        method.setAccessible(true); // NOPMD - test-only access to the panel's async completion seam
        method.invoke(panel, provider, state, models, generation);
    }

    @SuppressWarnings("unchecked")
    private static List<String> providerModelIds(String output) throws Exception {
        java.lang.reflect.Method method = ShaftAssistantPanel.class.getDeclaredMethod("providerModelIds", String.class);
        method.setAccessible(true); // NOPMD - package-private parser is deliberately exercised through its UI seam
        return (List<String>) method.invoke(null, output);
    }

    private static void invokeSend(ShaftAssistantPanel panel) throws Exception {
        java.lang.reflect.Method method = ShaftAssistantPanel.class.getDeclaredMethod("send", com.intellij.openapi.project.Project.class);
        method.setAccessible(true); // NOPMD - package-private UI action is tested through the real guard
        method.invoke(panel, new Object[] { null });
    }

    private static void invokeSendIgnoringRequestBoundary(ShaftAssistantPanel panel) throws Exception {
        try {
            invokeSend(panel);
        } catch (java.lang.reflect.InvocationTargetException ignored) {
            // The pre-fix path reaches the unavailable MCP request boundary after mutating panel state.
        }
    }

    private static void invokeUnchecked(ShaftAssistantPanel panel, String methodName) {
        try {
            java.lang.reflect.Method method = ShaftAssistantPanel.class.getDeclaredMethod(methodName);
            method.setAccessible(true); // NOPMD - invokes the real UI action on the EDT
            method.invoke(panel);
        } catch (ReflectiveOperationException exception) {
            throw new AssertionError(exception);
        }
    }

    private static Object primitiveDefault(Class<?> type) {
        if (!type.isPrimitive()) {
            return null;
        }
        if (type == boolean.class) {
            return false;
        }
        if (type == char.class) {
            return '\0';
        }
        if (type == long.class) {
            return 0L;
        }
        if (type == float.class) {
            return 0.0F;
        }
        if (type == double.class) {
            return 0.0D;
        }
        if (type == byte.class) {
            return (byte) 0;
        }
        if (type == short.class) {
            return (short) 0;
        }
        return 0;
    }

    private static boolean containsComponent(JComponent container, Component target) {
        return Arrays.stream(container.getComponents()).anyMatch(child -> child == target);
    }

    private static boolean containsDescendant(Container container, Component target) {
        return Arrays.stream(container.getComponents()).anyMatch(child -> child == target
                || child instanceof Container nested && containsDescendant(nested, target));
    }

    private static void layoutTree(Container container) {
        container.doLayout();
        for (Component child : container.getComponents()) {
            if (child instanceof Container nested) {
                layoutTree(nested);
            }
        }
    }
}
