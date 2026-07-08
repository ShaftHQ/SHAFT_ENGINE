package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins what the "Verbose" local-agent toggle is allowed to surface to the chat transcript: SHAFT's
 * own translated progress ("Calling tool X...", the final answer text) — never a wrapped CLI's raw
 * non-JSON stdout or internal chain-of-thought (Claude {@code thinking} blocks, Codex
 * {@code reasoning} items). Without this, live streaming could leak whatever the wrapped CLI happens
 * to print, straight into the user-facing chat.
 */
class AssistantLocalAgentRunnerVerboseStreamTest {
    @Test
    void nonJsonStdoutLinesAreNeverForwardedToTheLiveOutputConsumer() throws Exception {
        List<String> lines = run(claudeInvocation(),
                "2026-07-08 INFO some internal CLI banner\n"
                        + claudeAssistantTextEvent("hello") + "\n"
                        + claudeResultEvent("hello") + "\n");

        assertFalse(lines.stream().anyMatch(line -> line.contains("internal CLI banner")),
                "Raw non-JSON stdout must never reach the chat transcript: " + lines);
        assertTrue(lines.contains("hello"));
    }

    @Test
    void claudeThinkingBlocksAreNeverForwardedEvenWhenAloneInAnEvent() throws Exception {
        String thinkingOnlyEvent = "{\"type\":\"assistant\",\"message\":{\"content\":"
                + "[{\"type\":\"thinking\",\"thinking\":\"secret reasoning about the user's code\"}]}}";

        List<String> lines = run(claudeInvocation(), thinkingOnlyEvent + "\n" + claudeResultEvent("done") + "\n");

        assertFalse(lines.stream().anyMatch(line -> line.contains("secret reasoning")),
                "Claude's extended-thinking content must never reach the chat transcript: " + lines);
    }

    @Test
    void claudeThinkingBlockMixedWithTextOnlyShowsTheText() throws Exception {
        String mixedEvent = "{\"type\":\"assistant\",\"message\":{\"content\":["
                + "{\"type\":\"thinking\",\"thinking\":\"secret reasoning\"},"
                + "{\"type\":\"text\",\"text\":\"visible answer\"}]}}";

        List<String> lines = run(claudeInvocation(), mixedEvent + "\n" + claudeResultEvent("visible answer") + "\n");

        assertFalse(lines.stream().anyMatch(line -> line.contains("secret reasoning")), lines.toString());
        assertTrue(lines.contains("visible answer"));
    }

    @Test
    void codexReasoningItemsAreNeverForwardedButToolCallsStillAre() throws Exception {
        String reasoningEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"reasoning\","
                + "\"text\":\"secret codex reasoning\"}}";
        String toolEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"tool_call\",\"name\":\"shell\"}}";
        String turnCompleted = "{\"type\":\"turn.completed\",\"last_agent_message\":\"ok\","
                + "\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}";

        List<String> lines = run(codexInvocation(), reasoningEvent + "\n" + toolEvent + "\n" + turnCompleted + "\n");

        assertFalse(lines.stream().anyMatch(line -> line.contains("secret codex reasoning")), lines.toString());
        assertTrue(lines.stream().anyMatch(line -> line.contains("Calling tool shell")), lines.toString());
    }

    private static String claudeAssistantTextEvent(String text) {
        return "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"" + text + "\"}]}}";
    }

    private static String claudeResultEvent(String result) {
        return "{\"type\":\"result\",\"result\":\"" + result + "\",\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}";
    }

    private static List<String> run(AssistantCommand.Invocation invocation, String stdout) throws Exception {
        List<String> lines = new CopyOnWriteArrayList<>();
        StubProcess process = new StubProcess(stdout);
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, lines::add, (command, workingDirectory, environment) -> process, false);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);
        assertTrue(result.success(), "Expected the stub run to succeed: " + result.output());
        return lines;
    }

    private static AssistantCommand.Invocation claudeInvocation() {
        return AssistantCommand.fromPrompt("Explain this failure", "CLAUDE_CODE", "ASK", ".", "", false);
    }

    private static AssistantCommand.Invocation codexInvocation() {
        return AssistantCommand.fromPrompt("Explain this failure", "CODEX", "ASK", ".", "", false);
    }

    /**
     * Minimal stub {@link Process} that replays fixed stdout content and exits successfully as soon
     * as {@link #waitFor(long, TimeUnit)} is polled, mirroring the proven pattern in
     * {@code AssistantLocalAgentRunnerApprovalTest}'s {@code StubProcess.completing}.
     */
    private static final class StubProcess extends Process {
        private final InputStream stdout;

        StubProcess(String stdoutContent) {
            this.stdout = new ByteArrayInputStream(stdoutContent.getBytes(StandardCharsets.UTF_8));
        }

        @Override
        public OutputStream getOutputStream() {
            return new ByteArrayOutputStream();
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
            // No-op: these tests never exercise cancellation/destroy, only completed-run streaming.
        }

        @Override
        public Process destroyForcibly() {
            return this;
        }

        @Override
        public boolean isAlive() {
            return false;
        }
    }
}
