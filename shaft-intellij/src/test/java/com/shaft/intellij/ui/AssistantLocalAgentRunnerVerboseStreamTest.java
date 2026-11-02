package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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
    void nonJsonStdoutLinesCarryTheRawPassthroughMarkerSoNonVerboseModeCanSuppressThem() throws Exception {
        // Issue #3918: ShaftAssistantPanel's compact non-verbose milestone bubble must not surface
        // genuinely raw, untranslated CLI stdout (banners/warnings) -- only its own synthesized
        // milestone lines (Calling tool..., Thinking:...). Since both travel through the same
        // Consumer<String>, the non-JSON branch tags its line with RAW_STDOUT_MARKER so the panel can
        // tell the two apart without content-sniffing; a translated line never carries this marker.
        List<String> lines = run(claudeInvocation(),
                "2026-07-08 INFO some internal CLI banner\n"
                        + claudeAssistantTextEvent("hello") + "\n"
                        + claudeResultEvent("hello") + "\n");

        assertTrue(lines.stream().anyMatch(line -> line.startsWith(AssistantLocalAgentRunner.RAW_STDOUT_MARKER)
                        && line.contains("internal CLI banner")),
                "The raw banner line must carry the raw-passthrough marker: " + lines);
        assertTrue(lines.stream().noneMatch(line -> line.startsWith(AssistantLocalAgentRunner.RAW_STDOUT_MARKER)
                        && line.contains("hello")),
                "A translated/assistant-text line must never carry the raw-passthrough marker: " + lines);
    }

    @Test
    void hasStructuredStreamIsTrueOnlyForClaudeAndCodexDefaultCommands() {
        assertAll(
                () -> assertTrue(AssistantLocalAgentRunner.hasStructuredStream(claudeInvocation().arguments()),
                        "Claude Code's default command streams stream-json and gets a structured parser"),
                () -> assertTrue(AssistantLocalAgentRunner.hasStructuredStream(codexInvocation().arguments()),
                        "Codex's default command streams --json and gets a structured parser"),
                () -> assertFalse(AssistantLocalAgentRunner.hasStructuredStream(copilotInvocation().arguments()),
                        "Copilot has no structured stream flag -- its default command is buffered raw output"),
                () -> assertFalse(AssistantLocalAgentRunner.hasStructuredStream(
                                AssistantCommand.fromPrompt(
                                                "Explain this failure", "CODEX", "ASK", ".", "my-custom-cli --flag", false)
                                        .arguments()),
                        "A hand-typed custom command is forwarded raw, unwrapped, regardless of client"));
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
        // "command_execution" is the real Codex --json item variant (exec_events.rs); the stale
        // "tool_call" variant this fixture used to use is covered separately below (issue #3922).
        String toolEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"command_execution\",\"command\":\"shell\"}}";
        String turnCompleted = "{\"type\":\"turn.completed\",\"last_agent_message\":\"ok\","
                + "\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}";

        List<String> lines = run(codexInvocation(), reasoningEvent + "\n" + toolEvent + "\n" + turnCompleted + "\n");

        assertTrue(lines.contains("Reasoning: deciding which file to inspect"), lines.toString());
        assertTrue(lines.stream().anyMatch(line -> line.contains("Calling tool shell")), lines.toString());
    }

    @Test
    void codexToolCallItemTypeIsNotARealVariantAndFallsBackToRawPassthrough() throws Exception {
        // Issue #3922: the real Codex --json item variants are command_execution/mcp_tool_call (see
        // codex-rs/exec/src/exec_events.rs); "tool_call" never appears in live Codex output and was a
        // stale assumption that must not be treated as a known item type going forward.
        String staleEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"tool_call\",\"name\":\"shell\"}}";

        List<String> lines = run(codexInvocation(), staleEvent + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(lines.contains(staleEvent),
                "An item.type Codex never actually emits must fall back to raw passthrough, not render as a tool call: "
                        + lines);
        assertTrue(lines.stream().noneMatch(line -> line.contains("Calling tool shell")), lines.toString());
    }

    @Test
    void codexDeclinedCommandExecutionCountsAsAFailedOrDeniedToolCall() throws Exception {
        // CommandExecutionStatus::Declined (an approval/sandbox rejection) carries no non-zero
        // exit_code and no error object, so it must be recognized via status alone or a decline
        // silently misses the "Failed or denied tool calls" footer built for exactly this (#3679).
        String toolEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"command_execution\","
                + "\"name\":\"shell\",\"command\":\"rm -rf /\",\"status\":\"declined\"}}";

        String output = finalOutput(codexInvocation(), toolEvent + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(output.contains("Failed or denied tool calls: shell"), output);
    }

    @Test
    void codexCollabToolCallItemsAreRenderedLikeOtherToolCalls() throws Exception {
        String collabEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"collab_tool_call\","
                + "\"name\":\"pair_review\",\"status\":\"completed\"}}";

        List<String> lines = run(codexInvocation(), collabEvent + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(lines.stream().anyMatch(line -> line.contains("Calling tool pair_review")), lines.toString());
    }

    @Test
    void codexWebSearchItemsAreShownWithTheQuery() throws Exception {
        String webSearchEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"web_search\","
                + "\"id\":\"ws-1\",\"query\":\"latest Selenium 4 release notes\"}}";

        List<String> lines = run(codexInvocation(), webSearchEvent + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(lines.contains("Web search: latest Selenium 4 release notes"), lines.toString());
    }

    @Test
    void codexTodoListItemsAreShownAsAChecklist() throws Exception {
        String todoListEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"todo_list\",\"items\":["
                + "{\"text\":\"Write failing test\",\"completed\":true},"
                + "{\"text\":\"Implement fix\",\"completed\":false}]}}";

        List<String> lines = run(codexInvocation(), todoListEvent + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(lines.contains("Todo list:\n- [x] Write failing test\n- [ ] Implement fix"), lines.toString());
    }

    @Test
    void codexItemLevelErrorsAreShownWithoutFailingTheRun() throws Exception {
        String itemError = "{\"type\":\"item.completed\",\"item\":{\"type\":\"error\","
                + "\"message\":\"command output truncated\"}}";

        List<String> lines = run(codexInvocation(), itemError + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(lines.contains("Error: command output truncated"), lines.toString());
    }

    @Test
    void codexThreadStartedAndTurnStartedEventsAreShownAsProgressLines() throws Exception {
        String threadStarted = "{\"type\":\"thread.started\",\"thread_id\":\"t-1\"}";
        String turnStarted = "{\"type\":\"turn.started\"}";

        List<String> lines = run(codexInvocation(),
                threadStarted + "\n" + turnStarted + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(lines.contains("Codex session started."), lines.toString());
        assertTrue(lines.contains("Codex turn started."), lines.toString());
    }

    @Test
    void grokStreamDoesNotUseCodexSessionWording() throws Exception {
        String threadStarted = "{\"type\":\"thread.started\",\"thread_id\":\"t-1\"}";
        List<String> lines = run(grokInvocation(),
                threadStarted + "\n" + "done\n");
        assertFalse(lines.contains("Codex session started."), lines.toString());
        assertFalse(lines.stream().anyMatch(line -> line.contains("Codex turn")), lines.toString());
    }

    @Test
    void codexTopLevelErrorFeedsAPlainLanguageFailureReasonInsteadOfTheGenericExitCodeFallback() throws Exception {
        // ThreadEvent::Error is Codex's fatal top-level error (distinct from turn.failed's nested
        // error) and previously had no handling at all, so failureOutput fell back to the generic
        // "exited with code N" message even when Codex sent an explicit reason.
        String fatalError = "{\"type\":\"error\",\"message\":\"context length exceeded\"}";

        ShaftMcpToolResult result = failingRun(codexInvocation(), fatalError + "\n", "", 1);

        assertFalse(result.success(), result.output());
        assertTrue(result.output().contains("context length exceeded"),
                "The top-level error's message must drive the failure reason: " + result.output());
        assertFalse(result.output().contains("exited with code 1"),
                "A real reason must replace the generic exit-code fallback: " + result.output());
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
    void claudeToolResultsIncludeAllLinesNotJustTheFirst() throws Exception {
        // Regression: a multi-line tool result used to be silently cut to its first line only, e.g.
        // hiding the rest of a Bash command's output. The user asked for full messages, no trimming.
        String toolUseEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"Bash\",\"input\":{\"command\":\"ls\"}}]}}";
        String toolResultEvent = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\","
                + "\"tool_use_id\":\"tool-1\",\"content\":\"first line\\nsecond line\",\"is_error\":false}]}}";

        List<String> lines = run(claudeInvocation(),
                toolUseEvent + "\n" + toolResultEvent + "\n" + claudeResultEvent("done") + "\n");

        assertTrue(lines.contains("Tool result (Bash): first line\nsecond line"), lines.toString());
    }

    @Test
    void codexToolResultsIncludeAllLinesNotJustTheFirst() throws Exception {
        // Same fix, Codex side: describeCodexEvent must not cut a multi-line aggregated_output down
        // to its first line either.
        String toolEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"command_execution\","
                + "\"name\":\"shell\",\"command\":\"ls\",\"aggregated_output\":\"first line\\nsecond line\","
                + "\"exit_code\":0}}";

        List<String> lines = run(codexInvocation(), toolEvent + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(lines.contains("Calling tool shell (ls)...\nTool result (shell): first line\nsecond line"),
                lines.toString());
    }

    @Test
    void claudeToolUseArgumentSummaryIsNotTruncatedForLongCommands() throws Exception {
        // Regression: toolInputSummary used to cap the "Calling tool X (...)" argument preview to 80
        // chars, cutting a long Bash command mid-word. The user asked for full messages, no trimming.
        String longCommand = "npm run build && npm run test && npm run lint && npm run typecheck "
                + "&& echo build pipeline finished successfully";
        String toolUseEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"Bash\",\"input\":{\"command\":\"" + longCommand + "\"}}]}}";

        List<String> lines = run(claudeInvocation(), toolUseEvent + "\n" + claudeResultEvent("done") + "\n");

        assertTrue(lines.contains("Calling tool Bash (" + longCommand + ")..."), lines.toString());
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
        // subtype "api_retry" has no rendering yet (deferred per issue #3922's scope call) -- still a
        // valid example of a genuinely unmapped event, unlike "init"/"compact_boundary" below which
        // now render compactly.
        String apiRetryEvent = "{\"type\":\"system\",\"subtype\":\"api_retry\",\"attempt\":1}";

        List<String> lines = run(claudeInvocation(),
                apiRetryEvent + "\n" + claudeAssistantTextEvent("hi") + "\n" + claudeResultEvent("hi") + "\n");

        assertTrue(lines.contains(apiRetryEvent),
                "Events with no human-readable mapping must be shared as-is (raw JSON): " + lines);
    }

    @Test
    void claudeSystemInitEventShowsSessionAndModelInfo() throws Exception {
        String systemInit = "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"abc123\","
                + "\"model\":\"claude-sonnet-5\"}";

        List<String> lines = run(claudeInvocation(),
                systemInit + "\n" + claudeAssistantTextEvent("hi") + "\n" + claudeResultEvent("hi") + "\n");

        assertTrue(lines.contains("Session started (model claude-sonnet-5, session abc123)."), lines.toString());
    }

    @Test
    void claudeSystemCompactBoundaryEventShowsACompactionNotice() throws Exception {
        String compactBoundary = "{\"type\":\"system\",\"subtype\":\"compact_boundary\"}";

        List<String> lines = run(claudeInvocation(),
                compactBoundary + "\n" + claudeAssistantTextEvent("hi") + "\n" + claudeResultEvent("hi") + "\n");

        assertTrue(lines.contains("Conversation history was compacted to save context."), lines.toString());
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
        // "item.started" in-flight status is explicitly deferred (issue #3922 scope) -- still a valid
        // example of a genuinely unmapped event, unlike "thread.started"/"turn.started" below which
        // now render as progress lines.
        String itemStarted = "{\"type\":\"item.started\",\"item\":{\"type\":\"reasoning\",\"text\":\"x\"}}";

        List<String> lines = run(codexInvocation(), itemStarted + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(lines.contains(itemStarted), lines.toString());
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

    /**
     * Issue #4424 review round 2: distinguishes the two sides of the widened "no silent bare
     * confirmation" guarantee. A plain Q&A run that never attempted a tool call AND never received
     * any stderr has genuinely nothing to report, so it must stay exactly as terse as before this
     * issue -- contrast {@code successfulRunWithNonBootstrapStderrAndZeroToolCallsStillSurfacesIt
     * RegardlessOfVerbose} below, which pins the opposite case (identical zero-tool-call shape, but
     * WITH stderr) to prove the new rule fires on the stderr signal, not on "no tool calls" alone.
     * Uses {@code successfulRun} (explicit blank stderr, Verbose ON) instead of the shorter {@code
     * finalOutput} helper specifically to make that precondition textual, not merely implicit in a
     * stub default.
     */
    @Test
    void finalOutputStaysCleanWhenNothingWasWrittenAndNothingWasDenied() throws Exception {
        ShaftMcpToolResult result = successfulRun(claudeInvocation(),
                claudeAssistantTextEvent("plain answer") + "\n" + claudeResultEvent("plain answer") + "\n", "", true);

        assertTrue(result.success(), result.output());
        assertTrue(!result.output().contains("Local agent activity"),
                "A plain Q&A run with no stderr must not grow an activity footer: " + result.output());
    }

    /**
     * Parity regression for issue #3679: Codex (the plugin's default agent client) must grow the
     * same "Local agent activity" footer that Claude runs already get -- today it never does,
     * because {@code describeCodexEvent} never records file mutations or failed/denied tool calls.
     */
    @Test
    void finalOutputListsFilesWrittenByCodexEvenWhenTheAnswerIsTerse() throws Exception {
        String fileChangeEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"file_change\","
                + "\"status\":\"completed\",\"changes\":["
                + "{\"path\":\"src/test/java/pages/LoginPage.java\",\"kind\":\"add\"}]}}";

        String output = finalOutput(codexInvocation(), fileChangeEvent + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(output.contains("Files created or edited: `src/test/java/pages/LoginPage.java`"), output);
    }

    @Test
    void finalOutputSurfacesCodexCommandFailuresAsFailedOrDeniedToolCalls() throws Exception {
        String toolEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"command_execution\","
                + "\"name\":\"shell\",\"command\":\"echo secret > file\",\"aggregated_output\":\"denied\","
                + "\"exit_code\":1,\"status\":\"failed\"}}";

        String output = finalOutput(codexInvocation(), toolEvent + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(output.contains("No files were created or edited by this run."), output);
        assertTrue(output.contains("Failed or denied tool calls: shell"), output);
        assertTrue(!output.contains("approve them when prompted"),
                "Codex has no interactive approval prompt, so the Claude-specific hint must not appear: " + output);
    }

    @Test
    void finalOutputTreatsAFailedCodexPatchAsADenialNotAFileEdit() throws Exception {
        String failedPatch = "{\"type\":\"item.completed\",\"item\":{\"type\":\"file_change\","
                + "\"status\":\"failed\",\"changes\":[{\"path\":\"a.txt\",\"kind\":\"update\"}]}}";

        String output = finalOutput(codexInvocation(), failedPatch + "\n" + codexTurnCompletedEvent() + "\n");

        assertTrue(output.contains("No files were created or edited by this run."), output);
        assertTrue(output.contains("Failed or denied tool calls: file_change"), output);
    }

    /**
     * Codex-side sibling of {@code finalOutputStaysCleanWhenNothingWasWrittenAndNothingWasDenied}
     * above (issue #4424 review round 2): explicit blank stderr, Verbose ON.
     */
    @Test
    void finalOutputStaysCleanForCodexWhenNothingWasWrittenAndNothingFailed() throws Exception {
        ShaftMcpToolResult result = successfulRun(codexInvocation(), codexTurnCompletedEvent() + "\n", "", true);

        assertTrue(result.success(), result.output());
        assertTrue(!result.output().contains("Local agent activity"),
                "A plain Codex Q&A run with no stderr must not grow an activity footer: " + result.output());
    }

    /**
     * Regression for the "rerun failure dumps raw JSON" report: a structured (stream-json) CLI that
     * exits non-zero after emitting only native NDJSON events (system/thinking lines, no terminal
     * result) must render a clean, plain-language failure message -- never the raw stream.
     */
    @Test
    void failedStructuredRunWithoutTerminalEventNeverLeaksRawNdjson() throws Exception {
        String nativeNoise = "{\"type\":\"system\",\"subtype\":\"thinking_tokens\",\"tokens\":42}\n"
                + "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"abc\"}\n";

        ShaftMcpToolResult result = failingRun(claudeInvocation(), nativeNoise,
                "chrome exited: session not created", 1);

        assertFalse(result.success(), "A non-zero exit must fail the run: " + result.output());
        String output = result.output();
        assertFalse(output.contains("thinking_tokens"),
                "Raw native NDJSON must never leak into the failure output: " + output);
        assertFalse(output.contains("\"type\":\"system\""),
                "Raw native NDJSON must never leak into the failure output: " + output);
        assertTrue(output.contains("exited with code 1"),
                "A run with no terminal event must explain the exit: " + output);
        assertTrue(output.contains("chrome exited: session not created"),
                "stderr carries the real cause and must be surfaced: " + output);
    }

    /**
     * When the structured CLI DOES emit a terminal error result, its human answer text (and error
     * subtype) drive the failure message instead of the raw stream.
     */
    @Test
    void failedStructuredRunWithTerminalErrorResultShowsHumanReadableReason() throws Exception {
        String errorResult = "{\"type\":\"result\",\"subtype\":\"error_during_execution\",\"is_error\":true,"
                + "\"result\":\"Could not reach the target URL.\",\"usage\":{\"input_tokens\":3,\"output_tokens\":1}}";

        ShaftMcpToolResult result = failingRun(claudeInvocation(),
                "{\"type\":\"system\",\"subtype\":\"thinking_tokens\",\"tokens\":9}\n" + errorResult + "\n", "", 1);

        assertFalse(result.success(), result.output());
        String output = result.output();
        assertTrue(output.contains("Could not reach the target URL."),
                "The terminal error answer text must be shown: " + output);
        assertFalse(output.contains("thinking_tokens"),
                "Raw native NDJSON must never leak into the failure output: " + output);
    }

    /**
     * Issue #3965 (4th of 4 raw-output-leaks-in-non-verbose-mode paths from #3918, deferred by PR
     * #3958): {@code failureOutput}'s stderr fence must only reach the compact terminal answer when
     * Verbose is on. Verbose mode still gets exactly what
     * {@code failedStructuredRunWithoutTerminalEventNeverLeaksRawNdjson} above pins; a non-verbose
     * run must not see the raw stderr text baked into its answer bubble.
     */
    @Test
    void failedStructuredRunWithoutTerminalEventWithholdsStderrFromTheCompactAnswerWhenVerboseIsOff()
            throws Exception {
        String nativeNoise = "{\"type\":\"system\",\"subtype\":\"thinking_tokens\",\"tokens\":42}\n";

        ShaftMcpToolResult result = failingRun(claudeInvocation(), nativeNoise,
                "chrome exited: session not created", 1, false);

        assertFalse(result.success(), "A non-zero exit must fail the run: " + result.output());
        assertFalse(result.output().contains("chrome exited: session not created"),
                "Verbose off must withhold stderr from the compact failure answer: " + result.output());
        assertTrue(result.output().contains("exited with code 1"),
                "The plain-language exit reason must still explain the failure: " + result.output());
    }

    @Test
    void failedStructuredRunWithoutTerminalEventStillSurfacesStderrWhenVerboseIsOn() throws Exception {
        String nativeNoise = "{\"type\":\"system\",\"subtype\":\"thinking_tokens\",\"tokens\":42}\n";

        ShaftMcpToolResult result = failingRun(claudeInvocation(), nativeNoise,
                "chrome exited: session not created", 1, true);

        assertFalse(result.success(), "A non-zero exit must fail the run: " + result.output());
        assertTrue(result.output().contains("chrome exited: session not created"),
                "Verbose on must still surface stderr in the failure answer: " + result.output());
    }

    /**
     * Same gating, buffered/custom-command path (Copilot's default command or any hand-typed custom
     * command has no {@link StructuredStreamParser}, so the failure answer is composed by {@link
     * AssistantLocalAgentRunner#agentOutput} instead of {@code failureOutput}).
     */
    @Test
    void failedBufferedCommandWithholdsStderrFromTheCompactAnswerWhenVerboseIsOff() throws Exception {
        ShaftMcpToolResult result = failingRun(customCommandInvocation(), "", "permission denied", 1, false);

        assertFalse(result.success(), "A non-zero exit must fail the run: " + result.output());
        assertFalse(result.output().contains("permission denied"),
                "Verbose off must withhold stderr from the compact failure answer: " + result.output());
    }

    @Test
    void failedBufferedCommandStillSurfacesStderrWhenVerboseIsOn() throws Exception {
        ShaftMcpToolResult result = failingRun(customCommandInvocation(), "", "permission denied", 1, true);

        assertFalse(result.success(), "A non-zero exit must fail the run: " + result.output());
        assertTrue(result.output().contains("permission denied"),
                "Verbose on must still surface stderr in the failure answer: " + result.output());
    }

    /**
     * Direct unit coverage for {@link AssistantLocalAgentRunner#agentOutput}'s new verbose
     * parameter, alongside {@code AssistantCommandTest}'s existing pin of its unconditional
     * (verbose) 4-arg overload.
     */
    @Test
    void agentOutputWithholdsStderrWhenVerboseIsOffAndSurfacesItWhenVerboseIsOn() {
        assertAll(
                () -> assertEquals("partial",
                        AssistantLocalAgentRunner.agentOutput(false, "partial", "error", "", false)),
                () -> assertEquals("partial\n\nerror",
                        AssistantLocalAgentRunner.agentOutput(false, "partial", "error", "", true)),
                () -> assertEquals("",
                        AssistantLocalAgentRunner.agentOutput(true, "", "warning", "", false)),
                () -> assertEquals("warning",
                        AssistantLocalAgentRunner.agentOutput(true, "", "warning", "", true)));
    }

    /**
     * Issue #4424 review round 2, finding 3: {@code run()}'s buffered/custom-command success path
     * had its own, unreachable-by-the-structured-stream-fix instance of the same bug. {@link
     * AssistantLocalAgentRunner#agentOutput} itself keeps its original short-circuit (a custom
     * command's success answer no longer routes through it -- see {@code bufferedSuccessOutput}'s
     * javadoc for why that short-circuit is a deliberate, separately-pinned contract for its other
     * caller, {@code runCompactPreamble}). {@code bufferedSuccessOutput} folds {@code stdout} and
     * {@code verbose}-gated {@code stderr} together the same way whether or not {@code stdout} also
     * has content, closing the "Verbose is ignored on success" gap the reviewer's probe found.
     */
    @Test
    void bufferedSuccessOutputSurfacesStderrAlongsideNonBlankStdoutWhenVerboseIsOn() {
        assertEquals("ok\n\nsandbox-setup.exe failed to launch: Access is denied (os error 5)",
                AssistantLocalAgentRunner.bufferedSuccessOutput(
                        "ok", "sandbox-setup.exe failed to launch: Access is denied (os error 5)", true));
    }

    /**
     * Companion pin: Verbose off keeps withholding stderr here too (matching the already-established
     * #3965 contract), since (unlike the structured-stream path) there is no reliable "nothing else
     * happened" signal here to distinguish a silent failure from ordinary successful-run stderr
     * chatter.
     */
    @Test
    void bufferedSuccessOutputStillWithholdsStderrAlongsideNonBlankStdoutWhenVerboseIsOff() {
        assertEquals("ok", AssistantLocalAgentRunner.bufferedSuccessOutput("ok", "some stderr chatter", false));
    }

    /**
     * End-to-end reproduction of the reviewer's exact probe: a custom command whose stdout looks like
     * a complete, confident answer while its stderr carries the sandbox-bootstrap failure, Verbose
     * ON. Before this fix, {@code result.output()} was bare {@code "ok"} with zero trace.
     */
    @Test
    void customCommandSurfacesStderrAlongsideNonBlankStdoutWhenVerboseIsOn() throws Exception {
        String stderr = "sandbox-setup.exe failed to launch: Access is denied (os error 5)";

        ShaftMcpToolResult result = successfulRun(customCommandInvocation(), "ok", stderr, true);

        assertTrue(result.success(), result.output());
        assertTrue(result.output().contains("Access is denied (os error 5)"),
                "A custom command's stderr must surface alongside its stdout when Verbose is on: "
                        + result.output());
    }

    /**
     * Issue #4424 review round 3, finding 2: the round-2 PR body claimed the buffered path "has no
     * reliable way to tell a silent failure apart from ordinary successful-run stderr chatter" --
     * false for the sub-case that matters. {@code sections.isEmpty()} (blank stdout) IS the buffered
     * analogue of the structured-stream path's {@code toolCallObserved == false}: nothing else
     * happened, so stderr must surface regardless of Verbose. A run that DID produce stdout keeps
     * withholding stderr on Verbose off exactly as before (pinned by the sibling test above and
     * {@code bufferedSuccessOutputStillWithholdsStderrAlongsideNonBlankStdoutWhenVerboseIsOff}).
     */
    @Test
    void bufferedSuccessOutputSurfacesStderrWhenStdoutIsBlankRegardlessOfVerbose() {
        assertEquals("sandbox-setup.exe failed to launch: Access is denied (os error 5)",
                AssistantLocalAgentRunner.bufferedSuccessOutput(
                        "", "sandbox-setup.exe failed to launch: Access is denied (os error 5)", false));
    }

    /**
     * End-to-end sibling of the probe above via a real custom-command invocation: concretely, this
     * is the shape Copilot's default command (a first-party client with no structured stream at all)
     * or any custom command hits when its CLI produced no stdout at all but reported a real reason on
     * stderr.
     */
    @Test
    void customCommandWithBlankStdoutSurfacesStderrEvenWithVerboseOff() throws Exception {
        String stderr = "sandbox-setup.exe failed to launch: Access is denied (os error 5)";

        ShaftMcpToolResult result = successfulRun(customCommandInvocation(), "", stderr, false);

        assertTrue(result.success(), result.output());
        assertTrue(result.output().contains("Access is denied (os error 5)"),
                "Blank stdout with real stderr must surface it regardless of Verbose: " + result.output());
    }

    /**
     * Issue #4424 review round 3, finding 4: a structured CLI (Claude/Codex) that exits 0 without
     * ever emitting a terminal event has only raw NDJSON in its stdout -- {@code rawStdout} must
     * never reach {@code bufferedSuccessOutput} as literal stdout content, mirroring the guard the
     * failure branch a few lines below already has (its own comment names {@code thinking_tokens} as
     * exactly the leak this prevents). A genuinely buffered/custom command ({@code streamParser ==
     * null}) is unaffected -- its raw stdout is legitimate text, not NDJSON.
     */
    @Test
    void successfulCodexRunWithoutATerminalEventNeverLeaksRawNdjson() throws Exception {
        String rawLines = "{\"type\":\"thread.started\",\"thread_id\":\"t-1\"}\n{\"type\":\"turn.started\"}\n";

        ShaftMcpToolResult result = successfulRun(codexInvocation(), rawLines, "", true);

        assertTrue(result.success(), result.output());
        assertFalse(result.output().contains("thread.started"),
                "Raw native NDJSON must never leak into a successful-but-terminal-eventless answer: "
                        + result.output());
    }

    /**
     * Same shape, combined with finding 2's blank-stdout discriminator: once the raw NDJSON is
     * correctly discarded (not shown as "stdout"), the run's stdout is effectively blank, so a real
     * sandbox-bootstrap stderr must surface instead of the composed answer staying silent.
     */
    @Test
    void successfulCodexRunWithoutATerminalEventStillSurfacesStderrInsteadOfRawNdjson() throws Exception {
        String rawLines = "{\"type\":\"thread.started\",\"thread_id\":\"t-1\"}\n{\"type\":\"turn.started\"}\n";
        String stderr = "sandbox-setup.exe failed to launch: Access is denied (os error 5)";

        ShaftMcpToolResult result = successfulRun(codexInvocation(), rawLines, stderr, false);

        assertTrue(result.success(), result.output());
        assertFalse(result.output().contains("thread.started"), result.output());
        assertTrue(result.output().contains("Access is denied (os error 5)"), result.output());
    }

    /**
     * Issue #4424 review round 3, finding 3: round 2 silently gated the recognized sandbox-bootstrap
     * notice behind {@code silentRun}, so a partially-degraded run (a patch-application tool call
     * that succeeded via one path, while a DIFFERENT operation's sandbox helper failed to launch and
     * left its only trace on stderr) dropped the bootstrap failure entirely -- exactly the case a
     * user cannot diagnose alone. A recognized bootstrap shape must surface regardless of tool
     * activity, alongside (not instead of) the general {@code silentRun} rule.
     */
    @Test
    void recognizedSandboxBootstrapFailureSurfacesEvenAlongsideSuccessfulFileActivity() throws Exception {
        String fileChangeEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"file_change\","
                + "\"status\":\"completed\",\"changes\":[{\"path\":\"a.txt\",\"kind\":\"update\"}]}}";
        String stderr = "sandbox-setup.exe failed to launch: Access is denied (os error 5)";

        ShaftMcpToolResult result = successfulRun(codexInvocation(),
                fileChangeEvent + "\n" + codexTurnCompletedEvent() + "\n", stderr, false);

        assertTrue(result.success(), result.output());
        assertTrue(result.output().contains("Access is denied (os error 5)"),
                "A recognized bootstrap failure must surface even alongside real file activity: "
                        + result.output());
        assertTrue(result.output().contains("Files created or edited: `a.txt`"), result.output());
    }

    /**
     * Issue #4424 review round 3, finding 1: {@code recordToolCallObserved()} was only wired into
     * {@code describeToolCallItem} (command_execution/mcp_tool_call/collab_tool_call), so a Codex
     * {@code web_search} item -- a real tool call per this mapper's own javadoc -- left {@code
     * toolCallObserved} false, falsely classifying the run as "silently did nothing" and both
     * re-injecting suppressed ordinary stderr noise into a successful Verbose-off run AND asserting
     * a false "no tool calls ran" footer alongside a run that plainly used one.
     */
    @Test
    void codexWebSearchItemCountsAsAToolCallObservedSoIncidentalStderrStaysWithheld() throws Exception {
        String webSearchEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"web_search\","
                + "\"id\":\"ws-1\",\"query\":\"latest Selenium 4 release notes\"}}";

        ShaftMcpToolResult result = successfulRun(codexInvocation(),
                webSearchEvent + "\n" + codexTurnCompletedEvent() + "\n", "some incidental progress chatter", false);

        assertTrue(result.success(), result.output());
        assertFalse(result.output().contains("incidental progress chatter"), result.output());
        assertFalse(result.output().contains("no tool calls ran"), result.output());
    }

    /**
     * Same gap, Codex {@code file_change} item -- including the edge case the reviewer specifically
     * probed, a completed item with no {@code changes} array at all, which also makes {@code
     * recordCodexFileMutation} return early without touching {@code filesTouched}.
     */
    @Test
    void codexFileChangeItemWithNoChangesArrayStillCountsAsAToolCallObserved() throws Exception {
        String fileChangeEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"file_change\",\"status\":\"completed\"}}";

        ShaftMcpToolResult result = successfulRun(codexInvocation(),
                fileChangeEvent + "\n" + codexTurnCompletedEvent() + "\n", "some incidental progress chatter", false);

        assertTrue(result.success(), result.output());
        assertFalse(result.output().contains("incidental progress chatter"), result.output());
        assertFalse(result.output().contains("no tool calls ran"), result.output());
    }

    /**
     * Claude-side sibling: {@code server_tool_use} is a real content-block type (Claude's
     * server-executed tools, e.g. web search) that {@code describeAssistantEvent} never recognized
     * at all, so it never reached {@code recordToolCallObserved()} either.
     */
    @Test
    void claudeServerToolUseBlockCountsAsAToolCallObserved() throws Exception {
        String serverToolUseEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"server_tool_use\","
                + "\"id\":\"stu-1\",\"name\":\"web_search\",\"input\":{\"query\":\"selenium 4\"}}]}}";

        ShaftMcpToolResult result = successfulRun(claudeInvocation(),
                serverToolUseEvent + "\n" + claudeResultEvent("done") + "\n", "incidental chatter", false);

        assertTrue(result.success(), result.output());
        assertFalse(result.output().contains("incidental chatter"), result.output());
        assertFalse(result.output().contains("no tool calls ran"), result.output());
    }

    /**
     * A {@code tool_result} block proves a tool call happened even if the requesting {@code
     * tool_use} event was somehow missed -- correlated here to an id SHAFT never saw a {@code
     * tool_use} block for.
     */
    @Test
    void claudeToolResultWithNoPriorToolUseEventStillCountsAsAToolCallObserved() throws Exception {
        String toolResultEvent = "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\","
                + "\"tool_use_id\":\"unseen-id\",\"content\":\"ok\",\"is_error\":false}]}}";

        ShaftMcpToolResult result = successfulRun(claudeInvocation(),
                toolResultEvent + "\n" + claudeResultEvent("done") + "\n", "incidental chatter", false);

        assertTrue(result.success(), result.output());
        assertFalse(result.output().contains("incidental chatter"), result.output());
        assertFalse(result.output().contains("no tool calls ran"), result.output());
    }

    /**
     * Regression coverage for issue #4424: on a <em>successful</em> exit (unlike the failure paths
     * exercised above), {@code rawStderr} used to be computed and then discarded entirely once a
     * structured stream had a terminal event, so a sandbox/environment bootstrap failure that
     * happened before any tool could run (Codex's {@code sandbox-setup.exe ... Access is denied
     * (os error 5)}) silently vanished from the composed answer -- the CLI exited 0, the model
     * narrated a plausible-sounding confirmation, and nothing in the transcript hinted that no tool
     * had actually run. This must be surfaced <em>even with Verbose off</em>, unlike the ordinary
     * stderr noise gated by issue #3965 below: a sandbox bootstrap failure means the entire request
     * was silently impossible, not just noisy.
     */
    @Test
    void successfulRunWithSandboxBootstrapFailureStderrSurfacesAnActionableNoticeEvenWithVerboseOff()
            throws Exception {
        String stderr = "codex-windows-sandbox-setup.exe failed to launch: Access is denied (os error 5)";

        ShaftMcpToolResult result =
                successfulRun(codexInvocation(), codexTurnCompletedEvent() + "\n", stderr, false);

        assertTrue(result.success(), result.output());
        String lowerOutput = result.output().toLowerCase(java.util.Locale.ROOT);
        assertTrue(lowerOutput.contains("sandbox-setup") && result.output().contains("Access is denied"),
                "A sandbox bootstrap failure must surface an actionable notice even with Verbose off: "
                        + result.output());
    }

    /**
     * Regression guard for the fix above: a normal successful structured run, whose stderr never
     * matches the sandbox-bootstrap-failure shape, must render byte-identical output to today's --
     * no notice, no extra blank lines -- so consulting stderr on the success path never turns into
     * noise for the overwhelmingly common case.
     */
    @Test
    void successfulRunWithoutSandboxBootstrapStderrStaysByteIdenticalToTodaysOutput() throws Exception {
        String output = finalOutput(claudeInvocation(),
                claudeAssistantTextEvent("plain answer") + "\n" + claudeResultEvent("plain answer") + "\n");

        assertEquals("plain answer\n\n{\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}", output);
    }

    /**
     * Issue #4424 review round 2, finding 1: the fix above must not be fitted to the one observed
     * "sandbox-setup" string. The actual rule is broader -- a zero-exit run with zero tool calls,
     * zero files touched and zero denials, but non-blank stderr, must never be silent, whatever the
     * stderr says. An MCP-server startup failure is a different, unrelated cause from the sandbox
     * bootstrap failure and must surface all the same, with Verbose ON (matching the reviewer's
     * probe exactly: the bug reproduced even with Verbose on, since {@code finalOutput()} never took
     * stderr into account at all before this issue).
     */
    @Test
    void successfulRunWithNonBootstrapStderrAndZeroToolCallsStillSurfacesItRegardlessOfVerbose()
            throws Exception {
        String stderr = "Error: MCP server 'shaft-tools' failed to start (ECONNREFUSED 127.0.0.1:5173)";

        ShaftMcpToolResult result = successfulRun(codexInvocation(), codexTurnCompletedEvent() + "\n", stderr, true);

        assertTrue(result.success(), result.output());
        assertTrue(result.output().contains("ECONNREFUSED"),
                "Any stderr on an otherwise-silent successful run must surface, not only the "
                        + "sandbox-setup shape: " + result.output());
    }

    /**
     * Same as above, Verbose OFF -- proves the rule is genuinely ungated by Verbose, not merely
     * coincidentally true for Verbose-on inputs.
     */
    @Test
    void successfulRunWithNonBootstrapStderrSurfacesItEvenWithVerboseOff() throws Exception {
        String stderr = "Error: MCP server 'shaft-tools' failed to start (ECONNREFUSED 127.0.0.1:5173)";

        ShaftMcpToolResult result = successfulRun(codexInvocation(), codexTurnCompletedEvent() + "\n", stderr, false);

        assertTrue(result.success(), result.output());
        assertTrue(result.output().contains("ECONNREFUSED"), result.output());
    }

    /**
     * Issue #4424 review round 2, finding 2: detection fitted to one exact string is exactly the
     * failure mode issue #4469 recorded against a directive list fitted to its known offenders. Only
     * the hyphen changed here (a plausible naming-convention drift); "Access is denied (os error 5)"
     * is verbatim present. The general stderr-surfacing rule must not depend on recognizing
     * "sandbox-setup" literally -- it must surface this too.
     */
    @Test
    void successfulRunWithASandboxSetupNamingVariantStillSurfacesTheStderr() throws Exception {
        String stderr = "SandboxSetup.exe failed to launch: Access is denied (os error 5)";

        ShaftMcpToolResult result = successfulRun(codexInvocation(), codexTurnCompletedEvent() + "\n", stderr, false);

        assertTrue(result.success(), result.output());
        assertTrue(result.output().contains("Access is denied (os error 5)"), result.output());
    }

    /**
     * Boundary regression proving the fix is not "always surface any stderr on a successful run":
     * a run that DID attempt a tool call (so it is not the "silently did nothing" shape #4424
     * reported), touched no files, and had no denials must keep withholding incidental stderr noise
     * when Verbose is off -- exactly the ordinary-noise contract issue #3965 established. This is
     * what proves {@code toolCallObserved} genuinely narrows the new rule instead of widening
     * {@code activitySummary}'s guard unconditionally.
     */
    @Test
    void successfulCodexRunWithAToolCallButNothingToReportWithholdsIncidentalStderrWhenVerboseIsOff()
            throws Exception {
        String toolEvent = "{\"type\":\"item.completed\",\"item\":{\"type\":\"command_execution\","
                + "\"name\":\"shell\",\"command\":\"ls\",\"aggregated_output\":\"\",\"exit_code\":0}}";

        ShaftMcpToolResult result = successfulRun(codexInvocation(),
                toolEvent + "\n" + codexTurnCompletedEvent() + "\n", "some incidental progress chatter", false);

        assertTrue(result.success(), result.output());
        assertFalse(result.output().contains("incidental progress chatter"), result.output());
    }

    /**
     * Same boundary proof on the Claude side, confirming {@code ClaudeStreamEventMapper} also wires
     * a {@code tool_use} block into {@code toolCallObserved}.
     */
    @Test
    void successfulClaudeRunWithAToolCallButNothingToReportWithholdsIncidentalStderrWhenVerboseIsOff()
            throws Exception {
        String toolUseEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"Read\",\"input\":{\"file_path\":\"a.txt\"}}]}}";

        ShaftMcpToolResult result = successfulRun(claudeInvocation(),
                toolUseEvent + "\n" + claudeResultEvent("done") + "\n", "incidental chatter", false);

        assertTrue(result.success(), result.output());
        assertFalse(result.output().contains("incidental chatter"), result.output());
    }

    /**
     * Edge case: when the terminal answer text is itself blank, the stderr notice becomes the whole
     * core -- must not leave a leading blank line from the (skipped) "core + notice" concatenation.
     */
    @Test
    void successfulRunWithNoAnswerTextAtAllStillSurfacesTheStderrNoticeCleanly() throws Exception {
        String emptyResultEvent =
                "{\"type\":\"result\",\"result\":\"\",\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}";
        String stderr = "Error: MCP server 'shaft-tools' failed to start (ECONNREFUSED 127.0.0.1:5173)";

        ShaftMcpToolResult result = successfulRun(claudeInvocation(), emptyResultEvent + "\n", stderr, true);

        assertTrue(result.success(), result.output());
        assertTrue(result.output().contains("ECONNREFUSED"), result.output());
        assertFalse(result.output().startsWith("\n\n"),
                "A blank core must not leave a leading blank line: " + result.output());
    }

    private static ShaftMcpToolResult successfulRun(
            AssistantCommand.Invocation invocation, String stdout, String stderr, boolean verbose) throws Exception {
        StubProcess process = new StubProcess(stdout, stderr, 0);
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process, false, verbose);
        return running.future().get(5, TimeUnit.SECONDS);
    }

    private static ShaftMcpToolResult failingRun(
            AssistantCommand.Invocation invocation, String stdout, String stderr, int exitCode) throws Exception {
        StubProcess process = new StubProcess(stdout, stderr, exitCode);
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process, false);
        return running.future().get(5, TimeUnit.SECONDS);
    }

    private static ShaftMcpToolResult failingRun(
            AssistantCommand.Invocation invocation, String stdout, String stderr, int exitCode, boolean verbose)
            throws Exception {
        StubProcess process = new StubProcess(stdout, stderr, exitCode);
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process, false, verbose);
        return running.future().get(5, TimeUnit.SECONDS);
    }

    private static String finalOutput(AssistantCommand.Invocation invocation, String stdout) throws Exception {
        StubProcess process = new StubProcess(stdout);
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process, false);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);
        assertTrue(result.success(), "Expected the stub run to succeed: " + result.output());
        return result.output();
    }

    // -- Streaming timing: lines must arrive incrementally, not only buffered at EOF ------------

    @Test
    void verboseStreamingDeliversEachStubbedProcessLineIncrementallyBeforeExit() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "stub-agent --print", false);
        DelayedLinesStubProcess process = new DelayedLinesStubProcess(
                List.of("line one", "line two", "line three"), 40);
        List<String> delivered = new CopyOnWriteArrayList<>();

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, delivered::add, (command, workingDirectory, environment) -> process);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertEquals(3, delivered.size()),
                () -> assertEquals("line one", delivered.get(0)),
                () -> assertEquals("line two", delivered.get(1)),
                () -> assertEquals("line three", delivered.get(2)));
    }

    @Test
    void structuredClaudeStreamProducesHumanReadableProgressAndParsesTerminalUsage() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CLAUDE_CODE", "ASK", ".", "", false);
        String toolUseEvent = """
                {"type":"assistant","message":{"content":[{"type":"tool_use","id":"tool_1","name":"shaft_guide_search","input":{}}]}}""";
        String assistantTextEvent = """
                {"type":"assistant","message":{"content":[{"type":"text","text":"Looking into the failure now."}]}}""";
        String terminalEvent = """
                {"type":"result","subtype":"success","result":"The failure was a stale locator.","usage":{"input_tokens":123,"output_tokens":45}}""";
        StubProcess process = new StubProcess(
                String.join("\n", toolUseEvent, assistantTextEvent, terminalEvent) + "\n");
        List<String> delivered = new CopyOnWriteArrayList<>();

        // requireCommandAvailable=false: this exercises the real claude default command shape without
        // depending on the claude CLI actually being installed on the test machine's PATH.
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, delivered::add, (command, workingDirectory, environment) -> process, false);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        AssistantLocalAgentRunner.TokenUsage usage = AssistantLocalAgentRunner.parseTokenUsage(result.output());

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertTrue(result.output().contains("The failure was a stale locator.")),
                () -> assertFalse(delivered.isEmpty()),
                () -> assertTrue(delivered.stream().anyMatch(line -> line.contains("shaft_guide_search")),
                        "Expected a human-readable tool_use line: " + delivered),
                () -> assertTrue(delivered.stream().anyMatch(line -> line.contains("Looking into the failure now.")),
                        "Expected the assistant text to be delivered: " + delivered),
                () -> assertTrue(delivered.stream().noneMatch(line -> line.startsWith("{")),
                        "Consumer lines should be human-readable, not raw JSON: " + delivered),
                () -> assertEquals(123, usage.inputTokens()),
                () -> assertEquals(45, usage.outputTokens()),
                () -> assertFalse(usage.estimated()));
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

    private static AssistantCommand.Invocation grokInvocation() {
        return AssistantCommand.fromPrompt("Explain this failure", "GROK", "ASK", ".", "", false);
    }

    private static AssistantCommand.Invocation codexInvocation() {
        return AssistantCommand.fromPrompt("Explain this failure", "CODEX", "ASK", ".", "", false);
    }

    private static AssistantCommand.Invocation customCommandInvocation() {
        return AssistantCommand.fromPrompt("Explain this failure", "CODEX", "ASK", ".", "stub-agent --print", false);
    }

    /**
     * Minimal stub {@link Process} that replays fixed stdout content and exits successfully as soon
     * as {@link #waitFor(long, TimeUnit)} is polled, mirroring the proven pattern in
     * {@code AssistantLocalAgentRunnerApprovalTest}'s {@code StubProcess.completing}.
     */
    private static final class StubProcess extends Process {
        private final InputStream stdout;
        private final InputStream stderr;
        private final int exitCode;

        StubProcess(String stdoutContent) {
            this(stdoutContent, "", 0);
        }

        StubProcess(String stdoutContent, String stderrContent, int exitCode) {
            this.stdout = new ByteArrayInputStream(stdoutContent.getBytes(StandardCharsets.UTF_8));
            this.stderr = new ByteArrayInputStream(stderrContent.getBytes(StandardCharsets.UTF_8));
            this.exitCode = exitCode;
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

    /**
     * Stub {@link Process} whose stdout drips out one line at a time with a delay between lines,
     * proving the live-output consumer receives each line as it streams rather than only at EOF --
     * {@link StubProcess} above replays its whole stdout content at once, which cannot distinguish
     * "delivered incrementally" from "buffered until the process exits."
     */
    private static final class DelayedLinesStubProcess extends Process {
        private final InputStream stdout;

        DelayedLinesStubProcess(List<String> lines, long delayMillisPerLine) {
            this.stdout = new DelayedLinesInputStream(lines, delayMillisPerLine);
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
            // No underlying OS process to terminate.
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

    /**
     * Feeds newline-delimited lines to a reader with a delay between each line, simulating a slow
     * streaming CLI process for verbose-output tests.
     */
    private static final class DelayedLinesInputStream extends InputStream {
        private final List<byte[]> chunks = new ArrayList<>();
        private final long delayMillisPerLine;
        private int chunkIndex;
        private int positionInChunk;
        private boolean delayedForCurrentChunk;

        DelayedLinesInputStream(List<String> lines, long delayMillisPerLine) {
            this.delayMillisPerLine = delayMillisPerLine;
            for (String line : lines) {
                chunks.add((line + "\n").getBytes(StandardCharsets.UTF_8));
            }
        }

        @Override
        public synchronized int read() throws IOException {
            while (chunkIndex < chunks.size() && positionInChunk >= chunks.get(chunkIndex).length) {
                chunkIndex++;
                positionInChunk = 0;
                delayedForCurrentChunk = false;
            }
            if (chunkIndex >= chunks.size()) {
                return -1;
            }
            if (!delayedForCurrentChunk) {
                delayedForCurrentChunk = true;
                try {
                    Thread.sleep(delayMillisPerLine);
                } catch (InterruptedException exception) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while simulating delayed output", exception);
                }
            }
            return chunks.get(chunkIndex)[positionInChunk++] & 0xFF;
        }
    }
}
