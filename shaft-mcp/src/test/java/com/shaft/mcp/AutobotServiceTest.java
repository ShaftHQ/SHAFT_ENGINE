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
