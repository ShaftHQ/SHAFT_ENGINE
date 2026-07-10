package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins the deep "can the selected CLI actually access shaft-mcp" readiness used by the setup
 * screen's Check now button (issue: a reinstall must yield a real client-side verdict). Empirical
 * contract, probed against the real CLIs: {@code claude mcp get shaft-mcp} health-checks the
 * server and exits 0 even for an unknown server (the OUTPUT is the contract, with
 * "Status: ✔ Connected" / "No MCP server named" markers); {@code codex mcp get shaft-mcp} is a
 * fast config read whose positive marker is "enabled: true". Unknown output shapes fail OPEN so
 * an older CLI without an {@code mcp get} subcommand never blocks setup.
 */
class AssistantLocalAgentRunnerReadinessTest {
    @Test
    void claudeConnectedStatusReportsAccessSuccess() {
        ShaftMcpToolResult result = AssistantLocalAgentRunner.mcpAccessReadiness("CLAUDE_CODE", stub("""
                shaft-mcp:
                  Scope: User config (available in all your projects)
                  Status: ✔ Connected
                  Type: stdio
                """, 0));

        assertTrue(result.success(), result.output());
        assertTrue(result.output().contains("can access shaft-mcp"), result.output());
    }

    @Test
    void claudeMissingServerFailsWithInstallerGuidanceDespiteExitCodeZero() {
        ShaftMcpToolResult result = AssistantLocalAgentRunner.mcpAccessReadiness("CLAUDE_CODE", stub(
                "No MCP server named \"shaft-mcp\". Configured servers: context7 (and 3 more)", 0));

        assertFalse(result.success());
        assertTrue(result.output().contains("not registered with Claude Code"), result.output());
    }

    @Test
    void claudeFailedConnectionFailsWithRestartGuidance() {
        ShaftMcpToolResult result = AssistantLocalAgentRunner.mcpAccessReadiness("CLAUDE_CODE", stub("""
                shaft-mcp:
                  Status: ✘ Failed to connect
                """, 0));

        assertFalse(result.success());
        assertTrue(result.output().contains("restart"), result.output());
    }

    @Test
    void claudeUnknownOutputFailsOpenSoOlderClisNeverBlockSetup() {
        ShaftMcpToolResult result = AssistantLocalAgentRunner.mcpAccessReadiness("CLAUDE_CODE", stub(
                "usage: claude [options]", 0));

        assertTrue(result.success(), result.output());
    }

    @Test
    void codexEnabledRegistrationSucceedsAndMissingRegistrationFails() {
        ShaftMcpToolResult registered = AssistantLocalAgentRunner.mcpAccessReadiness("CODEX", stub("""
                shaft-mcp
                  enabled: true
                  transport: stdio
                """, 0));
        ShaftMcpToolResult missing = AssistantLocalAgentRunner.mcpAccessReadiness("CODEX", stub(
                "error: no MCP server named shaft-mcp", 1));

        assertTrue(registered.success(), registered.output());
        assertFalse(missing.success());
        assertTrue(missing.output().contains("not registered with Codex CLI"), missing.output());
    }

    @Test
    void copilotHasNoMcpInspectionCommandAndStaysPathLevel() {
        assertTrue(AssistantLocalAgentRunner.mcpAccessCommandFor("COPILOT_CLI").isEmpty());
        ShaftMcpToolResult result = AssistantLocalAgentRunner.mcpAccessReadiness("COPILOT_CLI",
                (command, workingDirectory, environment) -> {
                    throw new IllegalStateException("no probe process may be launched for Copilot");
                });
        assertTrue(result.success(), result.output());
    }

    @Test
    void probeLaunchFailureFailsOpen() {
        ShaftMcpToolResult result = AssistantLocalAgentRunner.mcpAccessReadiness("CLAUDE_CODE",
                (command, workingDirectory, environment) -> {
                    throw new java.io.IOException("simulated launch failure");
                });

        assertTrue(result.success(), result.output());
        assertTrue(result.output().contains("could not run"), result.output());
    }

    @Test
    void nonCliRuntimesSkipTheDeepCheckEntirely() {
        assertEquals(
                AssistantLocalAgentRunner.readiness("CLAUDE_CODE", "DESKTOP_APP").output(),
                AssistantLocalAgentRunner.connectionReadiness("CLAUDE_CODE", "DESKTOP_APP").output());
    }

    @Test
    void mcpAccessProbeUsesTheExpectedCliCommands() {
        AtomicReference<List<String>> launched = new AtomicReference<>();
        AssistantLocalAgentRunner.mcpAccessReadiness("CLAUDE_CODE", (command, workingDirectory, environment) -> {
            launched.set(command);
            return stubProcess("Status: ✔ Connected", 0);
        });

        assertEquals(List.of("claude", "mcp", "get", "shaft-mcp"), launched.get());
        assertEquals(List.of("codex", "mcp", "get", "shaft-mcp"),
                AssistantLocalAgentRunner.mcpAccessCommandFor("CODEX"));
    }

    private static AssistantLocalAgentRunner.ProcessLauncher stub(String stdout, int exitCode) {
        return (command, workingDirectory, environment) -> stubProcess(stdout, exitCode);
    }

    private static Process stubProcess(String stdout, int exitCode) {
        return new Process() {
            private final InputStream out = new ByteArrayInputStream(stdout.getBytes(StandardCharsets.UTF_8));

            @Override
            public OutputStream getOutputStream() {
                return new ByteArrayOutputStream();
            }

            @Override
            public InputStream getInputStream() {
                return out;
            }

            @Override
            public InputStream getErrorStream() {
                return new ByteArrayInputStream(new byte[0]);
            }

            @Override
            public int waitFor() {
                return exitCode;
            }

            @Override
            public boolean waitFor(long timeout, TimeUnit unit) {
                return true;
            }

            @Override
            public int exitValue() {
                return exitCode;
            }

            @Override
            public void destroy() {
                // No-op: stub process has nothing to terminate.
            }

            @Override
            public Process destroyForcibly() {
                return this;
            }

            @Override
            public boolean isAlive() {
                return false;
            }
        };
    }
}
