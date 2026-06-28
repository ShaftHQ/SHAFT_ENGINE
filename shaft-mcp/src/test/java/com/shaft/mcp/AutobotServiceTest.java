package com.shaft.mcp;

import com.shaft.pilot.agent.LocalAgentClient;
import com.shaft.pilot.agent.LocalAgentProcessResult;
import com.shaft.pilot.agent.LocalAgentProcessRunner;
import com.shaft.pilot.agent.LocalAgentResponse;
import com.shaft.pilot.agent.LocalAgentService;
import com.shaft.pilot.agent.LocalAgentStatus;
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
    void agentModeRemainsBlockedUntilMutationApprovalIsExplicit() {
        AutobotService service = new AutobotService(McpWorkspacePolicy.of(workspace),
                new LocalAgentService(client -> true, new CapturingRunner()));

        LocalAgentResponse response = service.runLocalAgent("claude_code", "agent", "Refactor this class.",
                "", List.of(), Map.of(), 10, false);

        assertEquals(LocalAgentStatus.REJECTED, response.status());
        assertTrue(response.warnings().contains(LocalAgentService.AGENT_MODE_APPROVAL_WARNING));
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
