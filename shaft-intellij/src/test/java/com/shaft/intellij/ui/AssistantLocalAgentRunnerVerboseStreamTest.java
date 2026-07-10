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

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins what the "Verbose" local-agent toggle surfaces to the chat transcript: the toggle exists so
 * the user can watch what the wrapped CLI is actually doing, so its live stream is deliberately
 * generous rather than sparse — raw non-JSON stdout still passes through, and Claude's
 * {@code thinking} blocks / Codex's {@code reasoning} items are shown with a clear label instead of
 * hidden, alongside tool-call notices and assistant text.
 */
class AssistantLocalAgentRunnerVerboseStreamTest {
    @Test
    void nonJsonStdoutLinesAreForwardedToTheLiveOutputConsumer() throws Exception {
        List<String> lines = run(claudeInvocation(),
                "2026-07-08 INFO some internal CLI banner\n"
                        + claudeAssistantTextEvent("hello") + "\n"
                        + claudeResultEvent("hello") + "\n");

        assertTrue(lines.stream().anyMatch(line -> line.contains("internal CLI banner")),
                "Non-JSON stdout (banners, warnings) is useful live progress and must reach the chat: " + lines);
        assertTrue(lines.contains("hello"));
    }

    @Test
    void claudeThinkingBlocksAreShownWithALabelEvenWhenAloneInAnEvent() throws Exception {
        String thinkingOnlyEvent = "{\"type\":\"assistant\",\"message\":{\"content\":"
                + "[{\"type\":\"thinking\",\"thinking\":\"reasoning about the user's code\"}]}}";

        List<String> lines = run(claudeInvocation(), thinkingOnlyEvent + "\n" + claudeResultEvent("done") + "\n");

        assertTrue(lines.contains("Thinking: reasoning about the user's code"),
                "Claude's extended-thinking content should be surfaced with a label: " + lines);
    }

    @Test
    void claudeThinkingBlockMixedWithTextShowsBothOnSeparateLines() throws Exception {
        String mixedEvent = "{\"type\":\"assistant\",\"message\":{\"content\":["
                + "{\"type\":\"thinking\",\"thinking\":\"weighing two approaches\"},"
                + "{\"type\":\"text\",\"text\":\"visible answer\"}]}}";

        List<String> lines = run(claudeInvocation(), mixedEvent + "\n" + claudeResultEvent("visible answer") + "\n");

        assertTrue(lines.contains("Thinking: weighing two approaches\nvisible answer"), lines.toString());
    }

    @Test
    void redactedThinkingBlocksShowANoteInsteadOfBeingDropped() throws Exception {
        String redactedEvent = "{\"type\":\"assistant\",\"message\":{\"content\":"
                + "[{\"type\":\"redacted_thinking\",\"data\":\"opaque\"}]}}";

        List<String> lines = run(claudeInvocation(), redactedEvent + "\n" + claudeResultEvent("done") + "\n");

        assertTrue(lines.contains("Thinking: (redacted by Claude for safety)"), lines.toString());
    }

    @Test
    void codexReasoningItemsAreShownWithALabelAlongsideToolCalls() throws Exception {
        String reasoningEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"reasoning\","
                + "\"text\":\"deciding which file to inspect\"}}";
        String toolEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"tool_call\",\"name\":\"shell\"}}";
        String turnCompleted = "{\"type\":\"turn.completed\",\"last_agent_message\":\"ok\","
                + "\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}";

        List<String> lines = run(codexInvocation(), reasoningEvent + "\n" + toolEvent + "\n" + turnCompleted + "\n");

        assertTrue(lines.contains("Reasoning: deciding which file to inspect"), lines.toString());
        assertTrue(lines.stream().anyMatch(line -> line.contains("Calling tool shell")), lines.toString());
    }

    @Test
    void claudeToolUseLinesIncludeASalientInputSummary() throws Exception {
        String toolUseEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"Bash\",\"input\":{\"command\":\"echo hi\"}}]}}";

        List<String> lines = run(claudeInvocation(), toolUseEvent + "\n" + claudeResultEvent("done") + "\n");

        assertTrue(lines.contains("Calling tool Bash (echo hi)..."), lines.toString());
    }

    @Test
    void claudeToolUseWithNoKnownInputFieldFallsBackToTheBareCallLine() throws Exception {
        String toolUseEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"CustomTool\",\"input\":{\"count\":3}}]}}";

        List<String> lines = run(claudeInvocation(), toolUseEvent + "\n" + claudeResultEvent("done") + "\n");

        assertTrue(lines.contains("Calling tool CustomTool..."), lines.toString());
    }

    @Test
    void claudeToolResultsSurfaceACorrelatedShortResultLine() throws Exception {
        String toolUseEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"Bash\",\"input\":{\"command\":\"echo hi\"}}]}}";
        String toolResultEvent = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\","
                + "\"tool_use_id\":\"tool-1\",\"content\":\"hi\",\"is_error\":false}]}}";

        List<String> lines = run(claudeInvocation(),
                toolUseEvent + "\n" + toolResultEvent + "\n" + claudeResultEvent("done") + "\n");

        assertTrue(lines.contains("Tool result (Bash): hi"),
                "Expected a correlated tool-result line, not silence after the call: " + lines);
    }

    @Test
    void claudeFailedToolResultsAreLabelledToolFailed() throws Exception {
        String toolUseEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"Bash\",\"input\":{\"command\":\"exit 1\"}}]}}";
        String toolResultEvent = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\","
                + "\"tool_use_id\":\"tool-1\",\"content\":\"command failed\",\"is_error\":true}]}}";

        List<String> lines = run(claudeInvocation(),
                toolUseEvent + "\n" + toolResultEvent + "\n" + claudeResultEvent("done") + "\n");

        assertTrue(lines.contains("Tool failed (Bash): command failed"), lines.toString());
    }

    @Test
    void claudeToolResultsTruncateToTheFirstLine() throws Exception {
        String toolUseEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"Bash\",\"input\":{\"command\":\"ls\"}}]}}";
        String toolResultEvent = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\","
                + "\"tool_use_id\":\"tool-1\",\"content\":\"first line\\nsecond line\",\"is_error\":false}]}}";

        List<String> lines = run(claudeInvocation(),
                toolUseEvent + "\n" + toolResultEvent + "\n" + claudeResultEvent("done") + "\n");

        assertTrue(lines.contains("Tool result (Bash): first line"), lines.toString());
        assertTrue(lines.stream().noneMatch(line -> line.contains("second line")), lines.toString());
    }

    @Test
    void claudeToolResultArrayContentIsFlattenedToText() throws Exception {
        String toolUseEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"Read\",\"input\":{\"file_path\":\"a.txt\"}}]}}";
        String toolResultEvent = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\","
                + "\"tool_use_id\":\"tool-1\",\"content\":[{\"type\":\"text\",\"text\":\"file contents\"}],"
                + "\"is_error\":false}]}}";

        List<String> lines = run(claudeInvocation(),
                toolUseEvent + "\n" + toolResultEvent + "\n" + claudeResultEvent("done") + "\n");

        assertTrue(lines.contains("Tool result (Read): file contents"), lines.toString());
    }

    @Test
    void claudeToolResultWithUnknownIdUsesUncorrelatedLabel() throws Exception {
        String toolResultEvent = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\","
                + "\"tool_use_id\":\"unseen-id\",\"content\":\"ok\",\"is_error\":false}]}}";

        List<String> lines = run(claudeInvocation(), toolResultEvent + "\n" + claudeResultEvent("done") + "\n");

        assertTrue(lines.contains("Tool result (tool): ok"), lines.toString());
    }

    @Test
    void codexCompletedCommandExecutionShowsCommandAndOutputSummary() throws Exception {
        String toolEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"command_execution\","
                + "\"name\":\"shell\",\"command\":\"echo hi\",\"aggregated_output\":\"hi\",\"exit_code\":0}}";

        List<String> lines = run(codexInvocation(), toolEvent + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(lines.contains("Calling tool shell (echo hi)...\nTool result (shell): hi"), lines.toString());
    }

    @Test
    void codexCompletedCommandExecutionWithNonZeroExitIsLabelledToolFailed() throws Exception {
        String toolEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"command_execution\","
                + "\"name\":\"shell\",\"command\":\"exit 1\",\"aggregated_output\":\"boom\",\"exit_code\":1}}";

        List<String> lines = run(codexInvocation(), toolEvent + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(lines.contains("Calling tool shell (exit 1)...\nTool failed (shell, exit 1): boom"), lines.toString());
    }

    @Test
    void unmappedJsonEventsPassThroughRawSoVerboseModeNeverHidesInformation() throws Exception {
        String systemInitEvent = "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"abc\"}";

        List<String> lines = run(claudeInvocation(),
                systemInitEvent + "\n" + claudeAssistantTextEvent("hi") + "\n" + claudeResultEvent("hi") + "\n");

        assertTrue(lines.contains(systemInitEvent),
                "Events with no human-readable mapping must be shared as-is (raw JSON): " + lines);
    }

    @Test
    void terminalResultEventsAreConsumedIntoTheFinalAnswerNotEchoedRaw() throws Exception {
        String resultEvent = claudeResultEvent("final answer");

        List<String> lines = run(claudeInvocation(),
                claudeAssistantTextEvent("final answer") + "\n" + resultEvent + "\n");

        assertTrue(lines.stream().noneMatch(line -> line.contains("\"type\":\"result\"")),
                "The terminal result event is mapped (it becomes the final answer), not raw noise: " + lines);
    }

    @Test
    void unmappedCodexEventsPassThroughRaw() throws Exception {
        String threadStarted = "{\"type\":\"thread.started\",\"thread_id\":\"t-1\"}";

        List<String> lines = run(codexInvocation(), threadStarted + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(lines.contains(threadStarted), lines.toString());
        assertTrue(lines.stream().noneMatch(line -> line.contains("turn.completed")),
                "The terminal usage event is consumed, not echoed: " + lines);
    }

    @Test
    void bufferedDefaultClisForwardTheirRawOutputAfterTheOneTimeNotice() throws Exception {
        List<String> lines = run(copilotInvocation(), "copilot line one\ncopilot line two\n");

        assertTrue(lines.size() >= 3, lines.toString());
        assertTrue(lines.get(0).contains("raw output is shown as-is"),
                "The one-time notice explains the raw stream: " + lines);
        assertTrue(lines.contains("copilot line one"), lines.toString());
        assertTrue(lines.contains("copilot line two"),
                "Buffered CLIs must share their output as-is instead of swallowing it: " + lines);
    }

    /**
     * Regression tests for the "useless Done bubble" report: the final answer must always carry a
     * factual activity footer when the run wrote files or lost tool calls to permission denials,
     * even with Verbose off (the footer lives in the final output, not the live stream).
     */
    @Test
    void finalOutputListsFilesWrittenByTheCliEvenWhenTheAnswerIsTerse() throws Exception {
        String writeEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"Write\",\"input\":{\"file_path\":\"src/test/java/pages/LoginPage.java\"}}]}}";

        String output = finalOutput(claudeInvocation(), writeEvent + "\n" + claudeResultEvent("Done") + "\n");

        assertTrue(output.contains("Files created or edited: `src/test/java/pages/LoginPage.java`"), output);
    }

    @Test
    void finalOutputSurfacesPermissionDenialsWithPerToolCounts() throws Exception {
        String resultWithDenials = "{\"type\":\"result\",\"result\":\"Done\","
                + "\"usage\":{\"input_tokens\":1,\"output_tokens\":1},"
                + "\"permission_denials\":["
                + "{\"tool_name\":\"Bash\"},{\"tool_name\":\"Bash\"},"
                + "{\"tool_name\":\"mcp__shaft-mcp__shaft_coding_partner_plan\"}]}";

        String output = finalOutput(claudeInvocation(), resultWithDenials + "\n");

        assertTrue(output.contains("No files were created or edited by this run."), output);
        assertTrue(output.contains("Denied tool calls: Bash ×2, mcp__shaft-mcp__shaft_coding_partner_plan"), output);
    }

    @Test
    void finalOutputStaysCleanWhenNothingWasWrittenAndNothingWasDenied() throws Exception {
        String output = finalOutput(claudeInvocation(),
                claudeAssistantTextEvent("plain answer") + "\n" + claudeResultEvent("plain answer") + "\n");

        assertTrue(!output.contains("Local agent activity"),
                "A plain Q&A run must not grow an activity footer: " + output);
    }

    private static String finalOutput(AssistantCommand.Invocation invocation, String stdout) throws Exception {
        StubProcess process = new StubProcess(stdout);
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process, false);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);
        assertTrue(result.success(), "Expected the stub run to succeed: " + result.output());
        return result.output();
    }

    private static AssistantCommand.Invocation copilotInvocation() {
        return AssistantCommand.fromPrompt("Explain this failure", "COPILOT_CLI", "ASK", ".", "", false);
    }

    private static String codexTurnCompletedEvent() {
        return "{\"type\":\"turn.completed\",\"last_agent_message\":\"ok\","
                + "\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}";
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
