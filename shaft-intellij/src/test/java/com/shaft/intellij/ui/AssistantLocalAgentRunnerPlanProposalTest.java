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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins how Claude Code's {@code ExitPlanMode} tool call -- the built-in the CLI uses to propose a
 * plan and ask "proceed with this plan?" -- is surfaced by {@link AssistantLocalAgentRunner},
 * closing the gap described in issue #3680: before this, {@code describeClaudeEvent}'s generic
 * {@code tool_use} handling treated it like any other tool (a bare "Calling tool ExitPlanMode..."
 * line, since {@code toolInputSummary} does not recognize the {@code plan} input key), and the
 * plan content itself only surfaced, if at all, buried in the final unstructured answer text.
 */
class AssistantLocalAgentRunnerPlanProposalTest {

    @Test
    void exitPlanModeSurfacesThePlanFieldInsteadOfTheGenericCallingToolLine() throws Exception {
        String exitPlanModeEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"ExitPlanMode\",\"input\":{\"plan\":\"1. Add a test.\\n2. Fix it.\"}}]}}";

        List<String> lines = run(planInvocation(), exitPlanModeEvent + "\n" + claudeResultEvent("Ready") + "\n");

        assertTrue(lines.stream().anyMatch(line -> line.contains("1. Add a test.") && line.contains("2. Fix it.")),
                "ExitPlanMode's plan field content must be surfaced in the live stream: " + lines);
        assertTrue(lines.stream().noneMatch(line -> line.equals("Calling tool ExitPlanMode...")),
                "ExitPlanMode must never fall through to the generic bare Calling-tool line: " + lines);
    }

    @Test
    void finalOutputCarriesThePlanProposalTextForTheTerminalCard() throws Exception {
        String exitPlanModeEvent = "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"tool_use\","
                + "\"id\":\"tool-1\",\"name\":\"ExitPlanMode\",\"input\":{\"plan\":\"Step one\\nStep two\"}}]}}";

        String output = finalOutput(planInvocation(), exitPlanModeEvent + "\n" + claudeResultEvent("Sounds good") + "\n");

        assertEquals("Step one\nStep two", AssistantLocalAgentRunner.parsePlanProposal(output),
                "The plan text must be recoverable from the final output for the structured card: " + output);
    }

    @Test
    void parsePlanProposalReturnsNullWhenNoExitPlanModeCallOccurred() throws Exception {
        String output = finalOutput(planInvocation(),
                claudeAssistantTextEvent("just an answer") + "\n" + claudeResultEvent("just an answer") + "\n");

        assertNull(AssistantLocalAgentRunner.parsePlanProposal(output),
                "A plain run without ExitPlanMode must never synthesize a plan proposal: " + output);
    }

    @Test
    void parsePlanProposalReturnsNullForBlankOrMalformedOutput() {
        assertNull(AssistantLocalAgentRunner.parsePlanProposal(null));
        assertNull(AssistantLocalAgentRunner.parsePlanProposal(""));
        assertNull(AssistantLocalAgentRunner.parsePlanProposal("plain prose answer, no trailing JSON"));
    }

    private static String finalOutput(AssistantCommand.Invocation invocation, String stdout) throws Exception {
        StubProcess process = new StubProcess(stdout);
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process, false);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);
        assertTrue(result.success(), "Expected the stub run to succeed: " + result.output());
        return result.output();
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

    private static AssistantCommand.Invocation planInvocation() {
        return AssistantCommand.fromPrompt("Refactor this class", "CLAUDE_CODE", "PLAN", ".", "", false);
    }

    private static String claudeAssistantTextEvent(String text) {
        return "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"" + text + "\"}]}}";
    }

    private static String claudeResultEvent(String result) {
        return "{\"type\":\"result\",\"result\":\"" + result + "\",\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}";
    }

    /**
     * Minimal stub {@link Process} that replays fixed stdout content and exits successfully as soon
     * as {@link #waitFor(long, TimeUnit)} is polled, mirroring the proven pattern in
     * {@code AssistantLocalAgentRunnerVerboseStreamTest}'s {@code StubProcess}.
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
