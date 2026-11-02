package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link AssistantLocalAgentRunner#parseTokenUsage} and
 * {@link AssistantLocalAgentRunner#stripTrailingUsageMetadata}: the static parsing helpers that let
 * the chat transcript report token usage when a CLI's terminal event carries it, and that strip the
 * trailing usage JSON blob out of the visible answer text without touching ordinary prose.
 */
class AssistantLocalAgentRunnerTokenUsageTest {

    @Test
    void stripTrailingUsageMetadataRemovesUsageLineButKeepsProseAndAnswerText() {
        String withUsage = "The answer text.\n\n{\"usage\":{\"input_tokens\":123,\"output_tokens\":45}}";
        String plainProse = "Just a plain multi-line\nanswer with no usage metadata.";

        assertAll(
                () -> assertEquals("The answer text.", AssistantLocalAgentRunner.stripTrailingUsageMetadata(withUsage)),
                () -> assertEquals(plainProse, AssistantLocalAgentRunner.stripTrailingUsageMetadata(plainProse)));
    }

    @Test
    void tokenUsageParsingPrefersReportedFieldsAndFallsBackToNullForEstimate() {
        String claudeJsonResult = """
                {"type":"result","subtype":"success","result":"Done","usage":{"input_tokens":123,"output_tokens":45}}
                """.strip();

        AssistantLocalAgentRunner.TokenUsage reported = AssistantLocalAgentRunner.parseTokenUsage(claudeJsonResult);
        AssistantLocalAgentRunner.TokenUsage absent = AssistantLocalAgentRunner.parseTokenUsage("Plain markdown response with no usage metadata.");

        assertAll(
                () -> assertEquals(123, reported.inputTokens()),
                () -> assertEquals(45, reported.outputTokens()),
                () -> assertFalse(reported.estimated()),
                () -> assertEquals(168, reported.totalTokens()),
                () -> assertNull(absent));
    }

    @Test
    void plainProseThroughCustomCommandNeverInventsUsageMetadata() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "stub-agent --print", false);
        String prose = "Here is the explanation.\nIt spans several lines.\nNo structured data anywhere.";
        StubProcess process = new StubProcess(prose + "\n");

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertEquals(prose, result.output()),
                () -> assertNull(AssistantLocalAgentRunner.parseTokenUsage(result.output())));
    }

    /**
     * Minimal stub {@link Process} that replays fixed stdout content and exits successfully as soon
     * as {@link #waitFor(long, TimeUnit)} is polled.
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
            // No-op: this test never exercises cancellation/destroy.
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
