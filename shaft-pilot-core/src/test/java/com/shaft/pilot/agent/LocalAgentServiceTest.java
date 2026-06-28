package com.shaft.pilot.agent;

import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LocalAgentServiceTest {
    @Test
    void askModeRoutesPromptToCodexCliThroughStdin() {
        CapturingRunner runner = new CapturingRunner();
        LocalAgentService service = new LocalAgentService(client -> true, runner);
        LocalAgentRequest request = LocalAgentRequest.builder(LocalAgentClient.CODEX, LocalAgentMode.ASK,
                        "How should I test this?")
                .workingDirectory(Path.of("."))
                .timeout(Duration.ofSeconds(5))
                .build();

        LocalAgentResponse response = service.execute(request);

        assertEquals(LocalAgentStatus.SUCCESS, response.status());
        assertEquals(List.of("codex", "exec", "--sandbox", "read-only", "-"), runner.command.get());
        assertEquals("How should I test this?", runner.stdin.get());
        assertEquals("answer", response.stdout());
        assertFalse(response.requiresCloudApiKey());
    }

    @Test
    void planModeUsesClaudePlanPermissionWithoutPluginOwnedProviderLogic() {
        CapturingRunner runner = new CapturingRunner();
        LocalAgentService service = new LocalAgentService(client -> true, runner);
        LocalAgentRequest request = LocalAgentRequest.builder(LocalAgentClient.CLAUDE_CODE, LocalAgentMode.PLAN,
                        "Plan the fix")
                .build();

        LocalAgentResponse response = service.execute(request);

        assertEquals(LocalAgentStatus.SUCCESS, response.status());
        assertEquals(List.of("claude", "--print", "--permission-mode", "plan"), runner.command.get());
        assertEquals("Plan the fix", runner.stdin.get());
    }

    @Test
    void agentModeRequiresExplicitMutationApproval() {
        LocalAgentService service = new LocalAgentService(client -> true, new CapturingRunner());
        LocalAgentRequest request = LocalAgentRequest.builder(LocalAgentClient.CODEX, LocalAgentMode.AGENT,
                        "Implement the change")
                .build();

        LocalAgentResponse response = service.execute(request);

        assertEquals(LocalAgentStatus.REJECTED, response.status());
        assertTrue(response.warnings().contains("Agent mode requires explicit source-mutation approval."));
    }

    @Test
    void configuredCommandOverridesDefaultsAndKeepsEnvironmentOutOfResponse() {
        CapturingRunner runner = new CapturingRunner();
        LocalAgentService service = new LocalAgentService(client -> true, runner);
        LocalAgentRequest request = LocalAgentRequest.builder(LocalAgentClient.COPILOT_CLI, LocalAgentMode.ASK,
                        "Explain this failure")
                .command(List.of("custom-agent", "--safe"))
                .environment(Map.of("OPENAI_API_KEY", "secret-value", "VISIBLE_FLAG", "1"))
                .build();

        LocalAgentResponse response = service.execute(request);

        assertEquals(LocalAgentStatus.SUCCESS, response.status());
        assertEquals(List.of("custom-agent", "--safe"), runner.command.get());
        assertEquals(Map.of("OPENAI_API_KEY", "secret-value", "VISIBLE_FLAG", "1"), runner.environment.get());
        assertFalse(response.toString().contains("secret-value"));
    }

    @Test
    void unavailableDefaultExecutableDoesNotInvokeRunner() {
        CapturingRunner runner = new CapturingRunner();
        LocalAgentService service = new LocalAgentService(client -> false, runner);
        LocalAgentRequest request = LocalAgentRequest.builder(LocalAgentClient.COPILOT_CLI, LocalAgentMode.ASK,
                        "Explain this failure")
                .build();

        LocalAgentResponse response = service.execute(request);

        assertEquals(LocalAgentStatus.UNAVAILABLE, response.status());
        assertEquals(null, runner.command.get());
    }

    @Test
    void requestRejectsBlankPromptAndNonPositiveTimeout() {
        assertThrows(IllegalArgumentException.class,
                () -> LocalAgentRequest.builder(LocalAgentClient.CODEX, LocalAgentMode.ASK, " ").build());
        assertThrows(IllegalArgumentException.class,
                () -> LocalAgentRequest.builder(LocalAgentClient.CODEX, LocalAgentMode.ASK, "prompt")
                        .timeout(Duration.ZERO)
                        .build());
    }

    private static final class CapturingRunner implements LocalAgentProcessRunner {
        private final AtomicReference<List<String>> command = new AtomicReference<>();
        private final AtomicReference<String> stdin = new AtomicReference<>();
        private final AtomicReference<Map<String, String>> environment = new AtomicReference<>();

        @Override
        public LocalAgentProcessResult run(
                List<String> command,
                Path workingDirectory,
                Map<String, String> environment,
                String stdin,
                Duration timeout) {
            this.command.set(command);
            this.stdin.set(stdin);
            this.environment.set(environment);
            return new LocalAgentProcessResult(0, "answer", "", false, Duration.ofMillis(10));
        }
    }
}
