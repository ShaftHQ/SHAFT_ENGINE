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
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers the approval bridge wired into {@link AssistantLocalAgentRunner}: command builders no
 * longer hard-code an auto-approve flag, and a registered {@link
 * AssistantLocalAgentRunner.ApprovalRequestHandler} is consulted (with a deny-on-timeout default)
 * for structured-stream CLIs that support interactive approval.
 */
class AssistantLocalAgentRunnerApprovalTest {

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
    void interactiveApprovalHandlerReceivesToolNameAndWritesApproveReplyToOpenStdin() throws Exception {
        AtomicReference<String> receivedToolName = new AtomicReference<>();
        AtomicReference<JsonObject> receivedArguments = new AtomicReference<>();
        AssistantLocalAgentRunner.ApprovalRequestHandler handler = (toolName, toolArguments) -> {
            receivedToolName.set(toolName);
            receivedArguments.set(toolArguments);
            return AssistantLocalAgentRunner.ApprovalDecision.APPROVE;
        };
        String approvalRequestLine =
                "{\"type\":\"control_request\",\"request_id\":\"req-1\",\"request\":{\"subtype\":\"can_use_tool\","
                        + "\"tool_name\":\"Bash\",\"input\":{\"command\":\"ls\"}}}";
        String terminalEvent =
                "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"Done.\","
                        + "\"usage\":{\"input_tokens\":10,\"output_tokens\":5}}";
        StubProcess process = StubProcess.completing(approvalRequestLine + "\n" + terminalEvent + "\n");

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeInvocation(), line -> { }, (command, workingDirectory, environment) -> process, false, handler);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertEquals("Bash", receivedToolName.get()),
                () -> assertEquals("ls", receivedArguments.get().get("command").getAsString()),
                () -> assertTrue(process.stdinCaptured().contains("\"behavior\":\"allow\""),
                        "Expected an allow control_response written to stdin: " + process.stdinCaptured()),
                () -> assertFalse(process.stdinCaptured().contains("\"behavior\":\"deny\"")));
    }

    @Test
    void interactiveApprovalHandlerDenialWritesDenyReplyToOpenStdin() throws Exception {
        AssistantLocalAgentRunner.ApprovalRequestHandler handler =
                (toolName, toolArguments) -> AssistantLocalAgentRunner.ApprovalDecision.DENY;
        String approvalRequestLine =
                "{\"type\":\"control_request\",\"request_id\":\"req-2\",\"request\":{\"subtype\":\"can_use_tool\","
                        + "\"tool_name\":\"Write\",\"input\":{\"path\":\"secrets.txt\"}}}";
        String terminalEvent =
                "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"Stopped.\","
                        + "\"usage\":{\"input_tokens\":3,\"output_tokens\":2}}";
        StubProcess process = StubProcess.completing(approvalRequestLine + "\n" + terminalEvent + "\n");

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeInvocation(), line -> { }, (command, workingDirectory, environment) -> process, false, handler);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertTrue(process.stdinCaptured().contains("\"behavior\":\"deny\""),
                        "Expected a deny control_response written to stdin: " + process.stdinCaptured()),
                () -> assertFalse(process.stdinCaptured().contains("\"behavior\":\"allow\"")));
    }

    @Test
    void neverAnsweringApprovalHandlerDeniesWithinTimeoutAndDestroysTheHungProcessWithoutHanging() throws Exception {
        AssistantLocalAgentRunner.ApprovalRequestHandler neverAnswers = (toolName, toolArguments) -> {
            try {
                Thread.sleep(Long.MAX_VALUE);
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
            }
            return AssistantLocalAgentRunner.ApprovalDecision.APPROVE;
        };
        String approvalRequestLine =
                "{\"type\":\"control_request\",\"request_id\":\"req-3\",\"request\":{\"subtype\":\"can_use_tool\","
                        + "\"tool_name\":\"Bash\",\"input\":{\"command\":\"rm -rf /\"}}}";
        // No terminal event ever arrives and waitFor never reports completion: this stub models a CLI
        // that hangs after asking for approval, exactly the scenario the approval timeout must survive.
        StubProcess process = StubProcess.neverExiting(approvalRequestLine + "\n");
        JsonObject arguments = claudeInvocation().arguments();
        // The overall process timeout must stay comfortably larger than the approval timeout: the
        // deny-on-timeout decision is resolved and written back to stdin on a background thread, and
        // the main thread only tears the process (and stdin) down once its own longer timeout elapses.
        // Setting both timeouts to the same value would race the stdin close against the deny write.
        arguments.addProperty("timeoutSeconds", 3);
        arguments.addProperty("approvalTimeoutSeconds", 1);

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                AssistantCommand.Invocation.tool(AssistantLocalAgentRunner.LOCAL_AGENT_TOOL, arguments),
                line -> { }, (command, workingDirectory, environment) -> process, false, neverAnswers);
        ShaftMcpToolResult result = running.future().get(10, TimeUnit.SECONDS);

        assertAll(
                () -> assertFalse(result.success(), "A never-answering handler must not let the run succeed"),
                () -> assertTrue(process.wasDestroyedForcibly(), "The hung process must be destroyed, not left running"),
                () -> assertTrue(process.stdinCaptured().contains("\"behavior\":\"deny\""),
                        "Expected the timeout default to write a deny reply: " + process.stdinCaptured()));
    }

    @Test
    void bufferedCustomCommandStillClosesStdinImmediatelyWhenNoApprovalHandlerIsRegistered() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "stub-agent --print", false);
        StubProcess process = StubProcess.completing("plain buffered output\n");

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertTrue(process.stdinClosed(), "Buffered/custom command paths must still close stdin immediately"));
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
     * inspect the control_response replies the approval bridge writes back) and can simulate either
     * a clean, prompt exit or a process that never reports completion.
     */
    private static final class StubProcess extends Process {
        private final InputStream stdout;
        private final RecordingOutputStream stdin = new RecordingOutputStream();
        private final int exitValue;
        private final boolean exitsPromptly;
        private volatile boolean destroyForciblyCalled;

        private StubProcess(InputStream stdout, int exitValue, boolean exitsPromptly) {
            this.stdout = stdout;
            this.exitValue = exitValue;
            this.exitsPromptly = exitsPromptly;
        }

        static StubProcess completing(String stdoutContent) {
            return new StubProcess(new ByteArrayInputStream(stdoutContent.getBytes(StandardCharsets.UTF_8)), 0, true);
        }

        static StubProcess neverExiting(String stdoutContent) {
            return new StubProcess(new ByteArrayInputStream(stdoutContent.getBytes(StandardCharsets.UTF_8)), 1, false);
        }

        String stdinCaptured() {
            return stdin.captured();
        }

        boolean stdinClosed() {
            return stdin.isClosed();
        }

        boolean wasDestroyedForcibly() {
            return destroyForciblyCalled;
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
            return exitValue;
        }

        @Override
        public boolean waitFor(long timeout, TimeUnit unit) throws InterruptedException {
            if (exitsPromptly) {
                return true;
            }
            // Simulates a process that never reports completion within the caller's timeout, exactly
            // like a hung real CLI would, so the runner's forced-destroy path is genuinely exercised.
            Thread.sleep(unit.toMillis(timeout));
            return false;
        }

        @Override
        public int exitValue() {
            return exitValue;
        }

        @Override
        public void destroy() {
            destroyForciblyCalled = true;
        }

        @Override
        public Process destroyForcibly() {
            destroyForciblyCalled = true;
            return this;
        }

        @Override
        public boolean isAlive() {
            return !exitsPromptly;
        }
    }

    /**
     * Captures every byte written to it and rejects further writes after {@link #close()}, so tests
     * can prove a reply was (or was not) written while stdin was genuinely still open.
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
        public void close() {
            closed = true;
        }

        String captured() {
            return buffer.toString(StandardCharsets.UTF_8);
        }

        boolean isClosed() {
            return closed;
        }
    }
}
