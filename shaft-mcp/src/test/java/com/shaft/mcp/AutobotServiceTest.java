package com.shaft.mcp;

import com.shaft.pilot.agent.LocalAgentClient;
import com.shaft.pilot.agent.LocalAgentProcessResult;
import com.shaft.pilot.agent.LocalAgentProcessRunner;
import com.shaft.pilot.agent.LocalAgentResponse;
import com.shaft.pilot.agent.LocalAgentService;
import com.shaft.pilot.agent.LocalAgentStatus;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.AiProvider;
import com.shaft.pilot.ai.AiProviderAvailability;
import com.shaft.pilot.ai.AiProviderRegistry;
import com.shaft.pilot.ai.AiCapabilities;
import com.shaft.pilot.ai.AiModelDiscovery;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.config.PilotConfiguration;
import com.shaft.driver.SHAFT;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AutobotServiceTest {
    @TempDir
    private Path workspace;

    @Test
    void runLocalAgentDelegatesThroughPilotCoreWithoutEchoingEnvironment() throws Exception {
        CapturingRunner runner = new CapturingRunner();
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, runner));

        LocalAgentResponse response = service.runLocalAgent("codex", "ask", "Explain the selected Java method.",
                "", List.of(), Map.of("SHAFT_AUTOBOT_TOKEN", "secret-value"), 10, false);

        assertEquals(LocalAgentStatus.SUCCESS, response.status());
        assertEquals(List.of("codex", "exec", "--skip-git-repo-check", "--sandbox", "read-only", "-"), runner.command.get());
        assertEquals(workspace.toRealPath(), runner.workingDirectory.get());
        assertEquals("Explain the selected Java method.", runner.stdin.get());
        assertFalse(response.toString().contains("secret-value"));
        assertFalse(response.requiresCloudApiKey());
    }

    @Test
    void agentModeWithoutMutationApprovalUsesReadOnlyDefaultCommand() throws Exception {
        CapturingRunner runner = new CapturingRunner();
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, runner));

        LocalAgentResponse response = service.runLocalAgent("codex", "agent", "Inspect this browser flow.",
                "", List.of(), Map.of(), 10, false);

        assertEquals(LocalAgentStatus.SUCCESS, response.status());
        assertEquals(List.of(
                        "codex", "exec", "--skip-git-repo-check",
                        "--sandbox", "read-only",
                        "-c", "mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\"",
                        "-c", "mcp_servers.shaft-mcp.tool_timeout_sec=600",
                        "-"),
                runner.command.get());
        assertEquals(workspace.toRealPath(), runner.workingDirectory.get());
    }

    @Test
    void localConsentIsNotToolApprovalForCustomAgentCommands() {
        com.shaft.driver.SHAFT.Properties.pilot.set().enabled(true).localConsent(true);
        try {
            CapturingRunner runner = new CapturingRunner();
            AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                    new LocalAgentService(client -> true, runner));

            LocalAgentResponse response = service.runLocalAgent(
                    "codex", "agent", "Delete every test.",
                    "", List.of("codex", "exec", "--full-auto", "-"), Map.of(), 10, false);

            assertEquals(LocalAgentStatus.REJECTED, response.status());
            assertTrue(runner.command.get() == null || runner.command.get().isEmpty(),
                    "Rejected Agent command must not reach the process runner.");
        } finally {
            com.shaft.properties.internal.Properties.clearForCurrentThread();
        }
    }

    @Test
    void clientsExposeLocalRoutesWithoutCloudApiKeys() {
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()));

        List<AutobotLocalAgentClient> clients = service.localAgentClients();

        assertEquals(LocalAgentClient.values().length, clients.size());
        assertTrue(clients.stream().anyMatch(client -> "CODEX".equals(client.id())));
        assertTrue(clients.stream().noneMatch(AutobotLocalAgentClient::requiresCloudApiKey));
    }

    @Test
    void providerChatRejectsAgentModeBecauseCloudChatCannotMutateSources() {
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()), request -> {
                    throw new AssertionError("Cloud provider should not be invoked");
                });

        AutobotProviderChatResponse response = service.runProviderChat(
                "github", "openai/gpt-4.1", "AGENT", "Edit the tests", "", 10, true);

        assertEquals("REJECTED", response.status());
        assertTrue(response.warnings().contains(AutobotService.CLOUD_AGENT_MODE_WARNING));
    }

    @Test
    void providerChatDelegatesAskAndReturnsAnswer() {
        AtomicReference<String> capturedText = new AtomicReference<>("");
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()), request -> {
                    capturedText.set(request.text());
                    return AiResponse.success("github", "openai/gpt-4.1",
                            tools.jackson.databind.node.JsonNodeFactory.instance.objectNode().put("answer", "ok"),
                            Duration.ofMillis(10), com.shaft.pilot.ai.AiUsage.empty(), request.deterministicFallback());
                });

        AutobotProviderChatResponse response = service.runProviderChat(
                "github", "openai/gpt-4.1", "ASK", "Explain this failure", "", 10, false);

        assertEquals(AiResponseStatus.SUCCESS.name(), response.status());
        assertEquals("ok", response.answer());
        assertEquals("Explain this failure", capturedText.get());
    }

    @Test
    void providerChatRedactsSecretValuesAndRemoteProvidersStillRequireRemoteConsent() {
        SHAFT.Properties.pilot.set().enabled(true).localConsent(true).remoteConsent(false);
        try {
            assertFalse(PilotConfiguration.current().approvalPolicy().remoteInferenceAllowed(),
                    "Remote providers require remoteConsent independently of localConsent.");
            AtomicReference<AiRequest> captured = new AtomicReference<>();
            AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                    new LocalAgentService(client -> true, new CapturingRunner()), request -> {
                        captured.set(request);
                        return AiResponse.success("ollama", "local-model",
                                tools.jackson.databind.node.JsonNodeFactory.instance.objectNode()
                                        .put("answer", "ok"),
                                Duration.ofMillis(10), com.shaft.pilot.ai.AiUsage.empty(),
                                request.deterministicFallback());
                    });

            AutobotProviderChatResponse response = service.runProviderChat(
                    "ollama",
                    "local-model",
                    "ASK",
                    "Explain Authorization: Basic dXNlcjpwYXNz and Authorization: planted-no-scheme-token-value and api_key=supersecretvalue123",
                    "",
                    10,
                    false);

            assertEquals(AiResponseStatus.SUCCESS.name(), response.status());
            assertNotNull(captured.get(), "Chat must submit a request through AiExecutionService.");
            String text = captured.get().text();
            assertFalse(text.contains("Basic dXNlcjpwYXNz"), text);
            assertFalse(text.contains("planted-no-scheme-token-value"), text);
            assertFalse(text.contains("supersecretvalue123"), text);
        } finally {
            SHAFT.Properties.clearForCurrentThread();
        }
    }

    @Test
    void providerChatDefaultsBlankProviderToGeminiDefaultModel() {
        AtomicReference<String> capturedProvider = new AtomicReference<>("");
        AtomicReference<String> capturedModel = new AtomicReference<>("");
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()), request -> {
                    PilotConfiguration configuration = PilotConfiguration.current();
                    capturedProvider.set(configuration.provider());
                    capturedModel.set(configuration.provider("gemini").model());
                    return AiResponse.success("gemini", capturedModel.get(),
                            tools.jackson.databind.node.JsonNodeFactory.instance.objectNode().put("answer", "ok"),
                            Duration.ofMillis(10), com.shaft.pilot.ai.AiUsage.empty(), request.deterministicFallback());
                });

        AutobotProviderChatResponse response = service.runProviderChat(
                "", "", "ASK", "Explain this failure", "", 10, false);

        assertEquals(AiResponseStatus.SUCCESS.name(), response.status());
        assertEquals("gemini", response.provider());
        assertEquals("gemini-3.5-flash", response.model());
        assertEquals("gemini", capturedProvider.get());
        assertEquals("gemini-3.5-flash", capturedModel.get());
    }

    @Test
    void providerStatusReportsKeyPresenceWithoutLeakingValue() {
        AutobotService service = new AutobotService(
                McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()),
                request -> {
                    throw new AssertionError("Cloud provider should not be invoked for status");
                },
                name -> "GEMINI_API_KEY".equals(name) ? "secret-value" : "");

        AutobotProviderStatus status = service.providerStatus("gemini", "gemini-3.5-flash");

        assertEquals("gemini", status.provider());
        assertEquals("gemini-3.5-flash", status.model());
        assertTrue(status.apiKeyPresent());
        assertEquals("GEMINI_API_KEY", status.apiKeyEnvironmentVariable());
        assertTrue(status.structuredOutputSupported());
        assertFalse(status.toString().contains("secret-value"));
        assertTrue(status.warnings().isEmpty());
    }

    @Test
    void providerStatusUsesTheConfiguredCredentialAlias() {
        System.setProperty("pilot.ai.gemini.apiKeyEnvironmentVariable", "GOOGLE_API_KEY");
        try {
            AutobotService service = new AutobotService(
                    McpWorkspacePolicy.of(workspace),
                    new LocalAgentService(client -> true, new CapturingRunner()),
                    request -> { throw new AssertionError("Cloud provider should not be invoked for status"); },
                    name -> "GOOGLE_API_KEY".equals(name) ? "secret-value" : "");

            AutobotProviderStatus status = service.providerStatus("gemini", "gemini-3.5-flash");

            assertTrue(status.apiKeyPresent());
            assertEquals("GOOGLE_API_KEY", status.apiKeyEnvironmentVariable());
            assertFalse(status.toString().contains("secret-value"));
        } finally {
            System.clearProperty("pilot.ai.gemini.apiKeyEnvironmentVariable");
        }
    }

    @Test
    void providerStatusWarnsWhenKeyOrModelMissing() {
        AutobotService service = new AutobotService(
                McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()),
                request -> {
                    throw new AssertionError("Cloud provider should not be invoked for status");
                },
                name -> "");

        AutobotProviderStatus status = service.providerStatus("openai", "");

        assertFalse(status.apiKeyPresent());
        assertTrue(status.warnings().stream().anyMatch(warning -> warning.contains("OPENAI_API_KEY")));
        assertTrue(status.warnings().stream().anyMatch(warning -> warning.contains("No model configured")));
    }

    @Test
    void providerModelsUsesTheResolvedProviderWithoutStaticFallbackAndConfiguresLocalConsent() {
        AtomicReference<PilotConfiguration> capturedConfiguration = new AtomicReference<>();
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(new DiscoveringProvider("ollama", configuration -> {
            capturedConfiguration.set(configuration);
            return new AiModelDiscovery(AiModelDiscovery.Status.AVAILABLE, List.of("runtime-model"));
        }));
        try {
            AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                    new LocalAgentService(client -> true, new CapturingRunner()), request -> {
                        throw new AssertionError("Provider chat executor is not used for model discovery");
                    });

            AutobotProviderModels response = service.providerModels("ollama", "selected-local-model");

            assertEquals("1.0", response.schemaVersion());
            assertEquals("ollama", response.provider());
            assertEquals("AVAILABLE", response.state());
            assertEquals(List.of("runtime-model"), response.modelIds());
            assertTrue(response.warnings().isEmpty());
            assertEquals("ollama", capturedConfiguration.get().provider());
            assertEquals("selected-local-model", capturedConfiguration.get().provider("ollama").model());
            assertTrue(capturedConfiguration.get().approvalPolicy().localInferenceAllowed());
            assertFalse(capturedConfiguration.get().approvalPolicy().remoteInferenceAllowed());
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void providerModelsNeverSerializesCredentialOrEndpointShapedModelIds() {
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(new DiscoveringProvider("gemini", configuration ->
                new AiModelDiscovery(AiModelDiscovery.Status.AVAILABLE, List.of(
                        "models/gemini-2.5-flash", "org/model", "llama3.2:latest",
                        "sk-proj-secret", "AKIA1234567890ABCDEF", "https://provider.example/models",
                        "provider.example:443", "model name", "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJzZWNyZXQifQ.signature",
                        "glpat-abc123", "hf_Abc123", "npm_Abc123"))));
        try {
            AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                    new LocalAgentService(client -> true, new CapturingRunner()), request -> null);

            AutobotProviderModels response = service.providerModels("gemini", "");

            assertEquals(List.of("llama3.2:latest", "models/gemini-2.5-flash", "org/model"), response.modelIds());
            assertFalse(response.toString().contains("sk-proj-secret"));
            assertFalse(response.toString().contains("AKIA1234567890ABCDEF"));
            assertFalse(response.toString().contains("provider.example:443"));
            assertFalse(response.toString().contains("eyJhbGciOiJIUzI1NiJ9"));
            assertFalse(response.toString().contains("glpat-abc123"));
            assertFalse(response.toString().contains("hf_Abc123"));
            assertFalse(response.toString().contains("npm_Abc123"));
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void providerModelsConfiguresLmStudioLocalConsentAndSelectedModel() {
        AtomicReference<PilotConfiguration> capturedConfiguration = new AtomicReference<>();
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(new DiscoveringProvider("lmstudio", configuration -> {
            capturedConfiguration.set(configuration);
            return new AiModelDiscovery(AiModelDiscovery.Status.EMPTY, List.of());
        }));
        try {
            AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                    new LocalAgentService(client -> true, new CapturingRunner()), request -> null);

            AutobotProviderModels response = service.providerModels("lmstudio", "selected-lmstudio-model");

            assertEquals("EMPTY", response.state());
            assertEquals("lmstudio", capturedConfiguration.get().provider());
            assertEquals("selected-lmstudio-model", capturedConfiguration.get().provider("lmstudio").model());
            assertTrue(capturedConfiguration.get().approvalPolicy().localInferenceAllowed());
            assertFalse(capturedConfiguration.get().approvalPolicy().remoteInferenceAllowed());
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void providerModelsFailureDoesNotExposeProviderExceptionDetails() {
        AtomicReference<PilotConfiguration> capturedConfiguration = new AtomicReference<>();
        AiProviderRegistry registry = new AiProviderRegistry();
        registry.registerForCurrentThread(new DiscoveringProvider("gemini", configuration -> {
            capturedConfiguration.set(configuration);
            throw new IllegalStateException("Bearer secret-value; response body; Authorization header");
        }));
        try {
            AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                    new LocalAgentService(client -> true, new CapturingRunner()), request -> {
                        throw new AssertionError("Provider chat executor is not used for model discovery");
                    });

            AutobotProviderModels response = service.providerModels("gemini", "gemini-test-model");

            assertEquals("FAILED", response.state());
            assertEquals("gemini", capturedConfiguration.get().provider());
            assertFalse(capturedConfiguration.get().approvalPolicy().localInferenceAllowed());
            assertTrue(capturedConfiguration.get().approvalPolicy().remoteInferenceAllowed());
            assertFalse(response.toString().contains("secret-value"));
            assertFalse(response.toString().contains("Authorization"));
        } finally {
            registry.clearForCurrentThread();
        }
    }

    @Test
    void providerModelsClearsThreadPropertiesAfterSuccessAndFailure() {
        AiProviderRegistry registry = new AiProviderRegistry();
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()), request -> null);
        try {
            registry.registerForCurrentThread(new DiscoveringProvider("ollama",
                    configuration -> new AiModelDiscovery(AiModelDiscovery.Status.AVAILABLE, List.of("runtime-model"))));
            SHAFT.Properties.pilot.set().provider("thread-only-before-success");

            service.providerModels("ollama", "runtime-model");

            assertNotEquals("ollama", PilotConfiguration.current().provider());
            registry.registerForCurrentThread(new DiscoveringProvider("gemini", configuration -> {
                throw new IllegalStateException("secret-value");
            }));
            SHAFT.Properties.pilot.set().provider("thread-only-before-failure");

            service.providerModels("gemini", "gemini-test-model");

            assertNotEquals("gemini", PilotConfiguration.current().provider());
        } finally {
            registry.clearForCurrentThread();
            SHAFT.Properties.clearForCurrentThread();
        }
    }

    @Test
    void providerChatSurfacesStructuredCodeBlocksAndGuardrailStatus() {
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()), request -> {
                    var payload = tools.jackson.databind.node.JsonNodeFactory.instance.objectNode();
                    payload.put("answer", "done");
                    payload.put("summary", "adds a sign-in test");
                    var block = payload.putArray("codeBlocks").addObject();
                    block.put("language", "java");
                    block.put("path", "src/test/java/tests/SignInTest.java");
                    block.put("code", "driver.element().click(signIn);");
                    payload.putArray("citedGuideUrls").add("https://shafthq.github.io/docs/testing/web");
                    return AiResponse.success("gemini", "gemini-3.5-flash", payload,
                            Duration.ofMillis(10), com.shaft.pilot.ai.AiUsage.empty(),
                            request.deterministicFallback());
                });

        AutobotProviderChatResponse response = service.runProviderChat(
                "gemini", "gemini-3.5-flash", "PLAN", "Write a sign-in test", "", 10, false);

        assertEquals("done", response.answer());
        assertEquals("adds a sign-in test", response.summary());
        assertEquals(1, response.codeBlocks().size());
        assertEquals("src/test/java/tests/SignInTest.java", response.codeBlocks().get(0).path());
        assertEquals("PASSED", response.guardrailStatus());
        assertTrue(response.citedGuideUrls().contains("https://shafthq.github.io/docs/testing/web"));
    }

    @Test
    void providerChatFlagsGuardrailViolationsInReturnedCode() {
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()), request -> {
                    var payload = tools.jackson.databind.node.JsonNodeFactory.instance.objectNode();
                    payload.put("answer", "done");
                    payload.putArray("codeBlocks").addObject().put("code", "Thread.sleep(1000);");
                    return AiResponse.success("gemini", "gemini-3.5-flash", payload,
                            Duration.ofMillis(10), com.shaft.pilot.ai.AiUsage.empty(),
                            request.deterministicFallback());
                });

        AutobotProviderChatResponse response = service.runProviderChat(
                "gemini", "gemini-3.5-flash", "PLAN", "Write code", "", 10, false);

        assertTrue(response.guardrailStatus().startsWith("VIOLATIONS"));
    }

    @Test
    void providerChatWithholdsCodeContainingSmartLocatorGuardrailErrors() {
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()), request -> {
                    var payload = tools.jackson.databind.node.JsonNodeFactory.instance.objectNode();
                    payload.put("answer", "done");
                    payload.putArray("codeBlocks").addObject()
                            .put("code", "driver.element().click(SHAFT.GUI.Locator.clickableField(\"Sign in\"));");
                    return AiResponse.success("gemini", "gemini-3.5-flash", payload,
                            Duration.ofMillis(10), com.shaft.pilot.ai.AiUsage.empty(),
                            request.deterministicFallback());
                });

        AutobotProviderChatResponse response = service.runProviderChat(
                "gemini", "gemini-3.5-flash", "PLAN", "Write a sign-in test", "", 10, false);

        assertTrue(response.codeBlocks().isEmpty(),
                "Generated code with a SMART_LOCATOR guardrail ERROR must never reach the MCP caller");
        assertTrue(response.guardrailStatus().startsWith("VIOLATIONS"));
        assertTrue(response.warnings().stream().anyMatch(warning -> warning.contains("guardrail")),
                "A withheld code block must explain why via warnings");
    }

    @Test
    void providerChatStillReturnsAriaRoleAndPlainXpathCodeThatPassesGuardrails() {
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()), request -> {
                    var payload = tools.jackson.databind.node.JsonNodeFactory.instance.objectNode();
                    payload.put("answer", "done");
                    payload.putArray("codeBlocks").addObject().put("code", """
                            By signIn = SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasText("Sign in").build();
                            By legacyFallback = By.xpath("//button[@id='legacy-submit']");
                            driver.element().click(signIn);
                            """);
                    return AiResponse.success("gemini", "gemini-3.5-flash", payload,
                            Duration.ofMillis(10), com.shaft.pilot.ai.AiUsage.empty(),
                            request.deterministicFallback());
                });

        AutobotProviderChatResponse response = service.runProviderChat(
                "gemini", "gemini-3.5-flash", "PLAN", "Write a sign-in test", "", 10, false);

        assertEquals(1, response.codeBlocks().size());
        assertEquals("PASSED", response.guardrailStatus());
    }

    private static final class CapturingRunner implements LocalAgentProcessRunner {
        private final AtomicReference<List<String>> command = new AtomicReference<>();
        private final AtomicReference<Path> workingDirectory = new AtomicReference<>();
        private final AtomicReference<String> stdin = new AtomicReference<>();

        @Override
        public LocalAgentProcessResult run(
                List<String> command,
                Path workingDirectory,
                Map<String, String> environment,
                String stdin,
                Duration timeout) {
            this.command.set(command);
            this.workingDirectory.set(workingDirectory);
            this.stdin.set(stdin);
            return new LocalAgentProcessResult(0, "answer", "", false, Duration.ofMillis(10));
        }
    }

    private record DiscoveringProvider(
            String id,
            java.util.function.Function<PilotConfiguration, AiModelDiscovery> discovery)
            implements AiProvider {
        @Override
        public AiCapabilities capabilities() {
            return null;
        }

        @Override
        public AiProviderAvailability availability() {
            return AiProviderAvailability.ready();
        }

        @Override
        public AiModelDiscovery discoverModels() {
            return discovery.apply(PilotConfiguration.current());
        }

        @Override
        public AiResponse execute(AiRequest request) {
            return null;
        }
    }
}
