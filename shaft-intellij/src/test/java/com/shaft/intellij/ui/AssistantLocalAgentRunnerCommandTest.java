package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.shaft.intellij.approval.LocalAgentApprovalBridge;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link AssistantLocalAgentRunner} command construction (auto-approve launch-time flags)
 * and its stdin lifecycle: every local CLI is launched with plain text input, so the runner must
 * close stdin immediately after writing the prompt for every client, every time. Holding it open
 * for any client would hang that CLI until the timeout fires, since none of them support a
 * stream-json *input* protocol today.
 */
class AssistantLocalAgentRunnerCommandTest {

    @Test
    void codexAgentCommandOnlyEmitsAutoApproveConfigFlagWhenApprovalStoreGrantsIt() {
        List<String> granted = AssistantLocalAgentRunner.commandFor(arguments("CODEX", "AGENT", true));
        List<String> ungranted = AssistantLocalAgentRunner.commandFor(arguments("CODEX", "AGENT", false));

        assertAll(
                () -> assertTrue(
                        granted.contains("mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\""),
                        "Granted AGENT mode should auto-approve: " + granted),
                () -> assertFalse(
                        ungranted.contains("mcp_servers.shaft-mcp.default_tools_approval_mode=\"approve\""),
                        "Ungranted AGENT mode must not hard-code auto-approve: " + ungranted),
                () -> assertTrue(ungranted.contains("read-only"), "Ungranted AGENT mode keeps the read-only sandbox: "
                        + ungranted));
    }

    @Test
    void claudeAgentCommandOnlyUsesAcceptEditsPermissionModeWhenApprovalStoreGrantsIt() {
        List<String> granted = AssistantLocalAgentRunner.commandFor(arguments("CLAUDE_CODE", "AGENT", true));
        List<String> ungranted = AssistantLocalAgentRunner.commandFor(arguments("CLAUDE_CODE", "AGENT", false));

        assertAll(
                () -> assertTrue(granted.contains("acceptEdits"), "Granted AGENT mode should accept edits: " + granted),
                () -> assertFalse(granted.contains("plan"), "Granted AGENT mode should not fall back to plan: " + granted),
                () -> assertTrue(ungranted.contains("plan"), "Ungranted AGENT mode must fall back to plan: " + ungranted),
                () -> assertFalse(ungranted.contains("acceptEdits"),
                        "Ungranted AGENT mode must not accept edits: " + ungranted));
    }

    @Test
    void bufferedCustomCommandClosesStdinImmediatelyAfterWritingThePrompt() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "stub-agent --print", false);
        StubProcess process = StubProcess.completing("plain buffered output\n");

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertTrue(process.stdinClosed(), "Buffered/custom command paths must close stdin immediately"));
    }

    /**
     * Regression test for the bug behind #5/#6/#7: a prior approval-bridge implementation held
     * stdin open past the initial prompt write for every Claude Code invocation, but Claude's
     * default text-input {@code --print} mode reads stdin until EOF before doing anything at all,
     * so every single Claude Code prompt hung until the 300-second timeout regardless of its
     * content. Closing stdin immediately, for every client including Claude Code, is what makes a
     * default-command invocation ever produce output at all.
     */
    @Test
    void claudeDefaultCommandAlsoClosesStdinImmediatelyAfterWritingThePrompt() throws Exception {
        String terminalEvent =
                "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"Done.\","
                        + "\"usage\":{\"input_tokens\":10,\"output_tokens\":5}}";
        StubProcess process = StubProcess.completing(terminalEvent + "\n");

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeInvocation(), line -> { }, (command, workingDirectory, environment) -> process, false);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertTrue(process.stdinClosed(),
                        "Claude Code's default command must close stdin immediately, not hold it open"));
    }

    @Test
    void claudeAgentCommandWiresApprovalBridgeFlagsWhenSourceEditsNotGranted() throws Exception {
        AtomicReference<List<String>> capturedCommand = new AtomicReference<>();
        StubProcess process = StubProcess.completing(claudeResultEvent("Done."));

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeAgentInvocation(false),
                line -> { },
                (command, workingDirectory, environment) -> {
                    capturedCommand.set(command);
                    return process;
                },
                false,
                allowAllHandler());
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        List<String> command = capturedCommand.get();
        assertAll(
                () -> assertTrue(result.success()),
                () -> assertTrue(command.contains("--permission-prompt-tool"), command.toString()),
                () -> assertTrue(command.contains("mcp__shaft-approval__approval_prompt"), command.toString()),
                () -> assertTrue(command.contains("--mcp-config"), command.toString()),
                () -> assertFalse(command.contains("plan"), command.toString()),
                () -> assertFalse(command.contains("acceptEdits"), command.toString()));
    }

    /**
     * Regression test for the "codegen does nothing" report: {@code acceptEdits} only
     * auto-approves file edits, so a granted run without the bridge has every Bash and MCP tool
     * call auto-denied in {@code --print} mode (reproduced live as 16 denials in one
     * capture-integration run). Granted AGENT runs therefore keep {@code acceptEdits} for edits
     * AND wire the interactive bridge for everything else.
     */
    @Test
    void grantedAgentModeAlsoLaunchesTheBridgeForNonEditToolCalls() throws Exception {
        AtomicReference<List<String>> capturedCommand = new AtomicReference<>();
        AtomicReference<LocalAgentApprovalBridge> launched = new AtomicReference<>();
        AssistantLocalAgentRunner.ApprovalBridgeLauncher launcher = (handler, timeout) -> {
            LocalAgentApprovalBridge bridge = LocalAgentApprovalBridge.start(handler, timeout);
            launched.set(bridge);
            return bridge;
        };
        StubProcess process = StubProcess.completing(claudeResultEvent("Done."));

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeAgentInvocation(true),
                line -> { },
                (command, workingDirectory, environment) -> {
                    capturedCommand.set(command);
                    return process;
                },
                false,
                allowAllHandler(), launcher);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        List<String> command = capturedCommand.get();
        assertAll(
                () -> assertTrue(result.success()),
                () -> assertTrue(launched.get() != null, "A granted run should launch the bridge for non-edit tools"),
                () -> assertTrue(command.contains("acceptEdits"), command.toString()),
                () -> assertTrue(command.contains("--permission-prompt-tool"), command.toString()),
                () -> assertTrue(command.contains("--mcp-config"), command.toString()),
                () -> assertTrue(launched.get().isClosed(), "The bridge must be closed once the run finishes"));
    }

    @Test
    void askModeNeverLaunchesTheBridgeEvenWithAHandlerSupplied() throws Exception {
        AtomicBoolean launcherInvoked = new AtomicBoolean();
        AssistantLocalAgentRunner.ApprovalBridgeLauncher launcher = (handler, timeout) -> {
            launcherInvoked.set(true);
            throw new IOException("should not be called");
        };
        StubProcess process = StubProcess.completing(claudeResultEvent("Done."));

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeInvocation(), line -> { }, (command, workingDirectory, environment) -> process, false,
                allowAllHandler(), launcher);
        running.future().get(5, TimeUnit.SECONDS);

        assertFalse(launcherInvoked.get());
    }

    @Test
    void codexClientNeverLaunchesTheBridgeEvenInAgentModeWithAHandlerSupplied() throws Exception {
        AtomicBoolean launcherInvoked = new AtomicBoolean();
        AssistantLocalAgentRunner.ApprovalBridgeLauncher launcher = (handler, timeout) -> {
            launcherInvoked.set(true);
            throw new IOException("should not be called");
        };
        StubProcess process = StubProcess.completing("plain buffered output\n");

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                AssistantCommand.fromPrompt("Explain this failure", "CODEX", "AGENT", ".", "", false),
                line -> { }, (command, workingDirectory, environment) -> process, false,
                allowAllHandler(), launcher);
        running.future().get(5, TimeUnit.SECONDS);

        assertFalse(launcherInvoked.get());
    }

    @Test
    void approvalBridgeIsClosedAfterTheRunCompletes() throws Exception {
        AtomicReference<LocalAgentApprovalBridge> launched = new AtomicReference<>();
        AssistantLocalAgentRunner.ApprovalBridgeLauncher launcher = (handler, timeout) -> {
            LocalAgentApprovalBridge bridge = LocalAgentApprovalBridge.start(handler, timeout);
            launched.set(bridge);
            return bridge;
        };
        StubProcess process = StubProcess.completing(claudeResultEvent("Done."));

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeAgentInvocation(false), line -> { }, (command, workingDirectory, environment) -> process, false,
                allowAllHandler(), launcher);
        running.future().get(5, TimeUnit.SECONDS);

        assertTrue(launched.get().isClosed(), "The bridge must be closed once the run finishes");
    }

    @Test
    void bridgeLaunchFailureFallsBackToPlanModeWithANotice() throws Exception {
        AtomicReference<List<String>> capturedCommand = new AtomicReference<>();
        List<String> consumedLines = new CopyOnWriteArrayList<>();
        StubProcess process = StubProcess.completing(claudeResultEvent("Done."));
        AssistantLocalAgentRunner.ApprovalBridgeLauncher failingLauncher = (handler, timeout) -> {
            throw new IOException("port bind failed");
        };

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeAgentInvocation(false),
                consumedLines::add,
                (command, workingDirectory, environment) -> {
                    capturedCommand.set(command);
                    return process;
                },
                false,
                allowAllHandler(),
                failingLauncher);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        List<String> command = capturedCommand.get();
        assertAll(
                () -> assertTrue(result.success()),
                () -> assertTrue(command.contains("plan"), command.toString()),
                () -> assertFalse(command.contains("--permission-prompt-tool"), command.toString()),
                () -> assertTrue(consumedLines.stream().anyMatch(line -> line.contains("Interactive approval is unavailable")),
                        consumedLines.toString()));
    }

    private static LocalAgentApprovalBridge.ApprovalRequestHandler allowAllHandler() {
        return (toolName, input) -> CompletableFuture.completedFuture(LocalAgentApprovalBridge.Decision.allow());
    }

    private static AssistantCommand.Invocation claudeAgentInvocation(boolean allowSourceMutation) {
        return AssistantCommand.fromPrompt("Explain this failure", "CLAUDE_CODE", "AGENT", ".", "", allowSourceMutation);
    }

    private static String claudeResultEvent(String result) {
        return "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"" + result + "\","
                + "\"usage\":{\"input_tokens\":10,\"output_tokens\":5}}\n";
    }

    private static JsonObject arguments(String client, String mode, boolean allowSourceMutation) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("client", client);
        arguments.addProperty("mode", mode);
        arguments.addProperty("allowSourceMutation", allowSourceMutation);
        arguments.addProperty("prompt", "irrelevant");
        arguments.addProperty("workingDirectory", ".");
        return arguments;
    }

    private static AssistantCommand.Invocation claudeInvocation() {
        return AssistantCommand.fromPrompt("Explain this failure", "CLAUDE_CODE", "ASK", ".", "", false);
    }

    /**
     * Minimal stub {@link Process} that captures everything written to its stdin (so tests can
     * prove it was closed promptly) and reports a clean, prompt exit.
     */
    private static final class StubProcess extends Process {
        private final InputStream stdout;
        private final RecordingOutputStream stdin = new RecordingOutputStream();

        private StubProcess(InputStream stdout) {
            this.stdout = stdout;
        }

        static StubProcess completing(String stdoutContent) {
            return new StubProcess(new ByteArrayInputStream(stdoutContent.getBytes(StandardCharsets.UTF_8)));
        }

        boolean stdinClosed() {
            return stdin.isClosed();
        }

        @Override
        public OutputStream getOutputStream() {
            return stdin;
        }

        @Override
        public InputStream getInputStream() {
            return stdout;
        }

        @Override
        public InputStream getErrorStream() {
            return new ByteArrayInputStream(new byte[0]);
        }

        @Override
        public int waitFor() {
            return 0;
        }

        @Override
        public boolean waitFor(long timeout, TimeUnit unit) {
            return true;
        }

        @Override
        public int exitValue() {
            return 0;
        }

        @Override
        public void destroy() {
            // Not exercised: this stub always reports a prompt, successful exit.
        }

        @Override
        public boolean isAlive() {
            return false;
        }
    }

    /**
     * Captures every byte written to it and tracks whether {@link #close()} was called, so tests
     * can prove stdin was closed promptly after the prompt was written.
     */
    private static final class RecordingOutputStream extends OutputStream {
        private final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        private volatile boolean closed;

        @Override
        public void write(int b) throws IOException {
            if (closed) {
                throw new IOException("stream closed");
            }
            buffer.write(b);
        }

        @Override
        public void write(byte[] b, int off, int len) throws IOException {
            if (closed) {
                throw new IOException("stream closed");
            }
            buffer.write(b, off, len);
        }

        @Override
        public void close() {
            closed = true;
        }

        boolean isClosed() {
            return closed;
        }
    }
}
