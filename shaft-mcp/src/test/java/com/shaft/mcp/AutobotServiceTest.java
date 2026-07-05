package com.shaft.mcp;

import com.shaft.pilot.agent.LocalAgentClient;
import com.shaft.pilot.agent.LocalAgentProcessResult;
import com.shaft.pilot.agent.LocalAgentProcessRunner;
import com.shaft.pilot.agent.LocalAgentResponse;
import com.shaft.pilot.agent.LocalAgentService;
import com.shaft.pilot.agent.LocalAgentStatus;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.config.PilotConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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
        assertEquals(List.of("codex", "exec", "--sandbox", "read-only", "-"), runner.command.get());
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
                        "codex", "exec",
                        "--sandbox", "read-only",
                        "-c", "mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\"",
                        "-c", "mcp_servers.shaft-mcp.tool_timeout_sec=600",
                        "-"),
                runner.command.get());
        assertEquals(workspace.toRealPath(), runner.workingDirectory.get());
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
}
