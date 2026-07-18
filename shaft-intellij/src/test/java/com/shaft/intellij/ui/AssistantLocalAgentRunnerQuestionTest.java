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
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers issue #3719's structured question/options protocol: {@code AssistantLocalAgentRunner}'s
 * {@code StructuredStreamParser} recognizes a trailing {@code {"shaft-question": "...",
 * "shaft-options": [...]}} line in the CLI's own terminal answer text (Claude's {@code result}
 * event, Codex's {@code turn.completed} event) at the moment that text is captured -- before any
 * activity-footer or usage-metadata lines are appended -- strips it out of the visible answer, and
 * carries it instead as a {@code question} field in the trailing usage-metadata JSON line, mirroring
 * how {@code ExitPlanMode}'s {@code plan} field is already carried (see
 * AssistantLocalAgentRunnerPlanProposalTest). {@link AssistantLocalAgentRunner#parseQuestion} is the
 * runner-recognized counterpart to {@link AssistantQuestion#detect}'s documented fence fallback.
 */
class AssistantLocalAgentRunnerQuestionTest {

    @Test
    void claudeStructuredQuestionLineIsRecoverableViaParseQuestion() throws Exception {
        String resultText = "Want me to actually run through a recording now?\\n"
                + "{\\\"shaft-question\\\": \\\"Want me to actually run through a recording now?\\\", "
                + "\\\"shaft-options\\\": [\\\"Use the sample page\\\", \\\"I'll give you a URL\\\"]}";

        String output = finalOutput(claudeInvocation(), claudeResultEvent(resultText) + "\n");
        AssistantQuestion question = AssistantLocalAgentRunner.parseQuestion(output);

        assertAll(
                () -> assertTrue(question != null, "expected a structured question to be recovered: " + output),
                () -> assertEquals(List.of("Use the sample page", "I'll give you a URL"), question.options()));
    }

    @Test
    void claudeStructuredQuestionLineIsStrippedFromTheVisibleAnswerText() throws Exception {
        String resultText = "Want me to actually run through a recording now?\\n"
                + "{\\\"shaft-question\\\": \\\"Want me to actually run through a recording now?\\\", "
                + "\\\"shaft-options\\\": [\\\"Use the sample page\\\", \\\"I'll give you a URL\\\"]}";

        String output = finalOutput(claudeInvocation(), claudeResultEvent(resultText) + "\n");

        assertAll(
                () -> assertTrue(output.contains("Want me to actually run through a recording now?"), output),
                () -> assertFalse(output.contains("shaft-question"),
                        "the raw structured marker must never leak into the visible answer: " + output));
    }

    @Test
    void codexStructuredQuestionLineIsRecoverableViaParseQuestion() throws Exception {
        String turnCompleted = "{\"type\":\"turn.completed\",\"last_agent_message\":"
                + "\"Pick one:\\n{\\\"shaft-question\\\": \\\"Pick one:\\\", \\\"shaft-options\\\": "
                + "[\\\"Yes\\\", \\\"No\\\"]}\",\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}";

        String output = finalOutput(codexInvocation(), turnCompleted + "\n");
        AssistantQuestion question = AssistantLocalAgentRunner.parseQuestion(output);

        assertAll(
                () -> assertTrue(question != null, "expected a structured question to be recovered: " + output),
                () -> assertEquals(List.of("Yes", "No"), question.options()));
    }

    @Test
    void parseQuestionReturnsNullWhenNoStructuredLineWasPresent() throws Exception {
        String output = finalOutput(claudeInvocation(), claudeResultEvent("just an ordinary answer") + "\n");

        assertNull(AssistantLocalAgentRunner.parseQuestion(output),
                "a plain run without a structured line must never synthesize a question: " + output);
    }

    @Test
    void parseQuestionFallsBackCleanlyWhenTheStructuredLineIsMalformed() throws Exception {
        // The model attempted the structured line but left it malformed (missing shaft-options) --
        // parseQuestion must return null without throwing, so callers fall through to fence detection
        // (or plain text) exactly as they do for a run that never attempted the protocol at all.
        String resultText = "Pick one:\\n{\\\"shaft-question\\\": \\\"Pick one:\\\"}";

        String output = finalOutput(claudeInvocation(), claudeResultEvent(resultText) + "\n");

        assertNull(AssistantLocalAgentRunner.parseQuestion(output), output);
    }

    @Test
    void parseQuestionReturnsNullForBlankOrMalformedOutput() {
        assertAll(
                () -> assertNull(AssistantLocalAgentRunner.parseQuestion(null)),
                () -> assertNull(AssistantLocalAgentRunner.parseQuestion("")),
                () -> assertNull(AssistantLocalAgentRunner.parseQuestion("plain prose answer, no trailing JSON")));
    }

    private static String finalOutput(AssistantCommand.Invocation invocation, String stdout) throws Exception {
        StubProcess process = new StubProcess(stdout);
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process, false);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);
        assertTrue(result.success(), "Expected the stub run to succeed: " + result.output());
        return result.output();
    }

    private static AssistantCommand.Invocation claudeInvocation() {
        return AssistantCommand.fromPrompt("Should I record a sample flow?", "CLAUDE_CODE", "ASK", ".", "", false);
    }

    private static AssistantCommand.Invocation codexInvocation() {
        return AssistantCommand.fromPrompt("Should I record a sample flow?", "CODEX", "ASK", ".", "", false);
    }

    private static String claudeResultEvent(String result) {
        return "{\"type\":\"result\",\"result\":\"" + result + "\",\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}";
    }

    /**
     * Minimal stub {@link Process} that replays fixed stdout content and exits successfully as soon
     * as {@link #waitFor(long, TimeUnit)} is polled, mirroring the proven pattern in
     * {@code AssistantLocalAgentRunnerPlanProposalTest}'s {@code StubProcess}.
     */
    private static final class StubProcess extends Process {
        private final InputStream stdout;
        private final InputStream stderr;

        StubProcess(String stdoutContent) {
            this.stdout = new ByteArrayInputStream(stdoutContent.getBytes(StandardCharsets.UTF_8));
            this.stderr = new ByteArrayInputStream(new byte[0]);
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
            return stderr;
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
