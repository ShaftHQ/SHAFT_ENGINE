package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
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
import java.util.concurrent.TimeUnit;

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
