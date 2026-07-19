package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.shaft.intellij.approval.LocalAgentApprovalBridge;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link AssistantLocalAgentRunner#runCompactPreamble} (both overloads) and
 * {@link AssistantLocalAgentRunner#startWithOptionalCompact} (the two overloads that hard-code the
 * real {@code launchProcess} for their main invocation). To stay unit-fast and machine-independent,
 * these tests never rely on {@code isCommandAvailable} returning false for a real client executable
 * -- on this dev machine {@code claude} and {@code codex} ARE actually installed on PATH (confirmed
 * via {@code command -v}), so a test asserting "unavailable" for either would be a false assumption
 * that breaks the moment it runs here. Instead:
 * <ul>
 *   <li>The two-arg {@code runCompactPreamble(JsonObject, ProcessLauncher)} overload is exercised
 *   directly with a stub launcher, bypassing the PATH gate entirely (that's exactly why it exists).</li>
 *   <li>The single-arg {@code runCompactPreamble(JsonObject)} overload is only proven for its
 *   PATH-independent "unsupported client" branch (CODEX/COPILOT_CLI have no compact command at all).</li>
 *   <li>{@code startWithOptionalCompact}'s real-launch main invocation uses a deliberately
 *   nonexistent custom-command executable, which makes {@code ProcessBuilder#start()} throw an
 *   {@link java.io.IOException} synchronously and deterministically -- the same "bogus executable"
 *   technique already used elsewhere in this module's tests, and safe because compaction for a CODEX
 *   client is a config-only no-op (no process launched for the compact step at all).</li>
 * </ul>
 */
class AssistantLocalAgentRunnerCompactPreambleTest {

    // -- runCompactPreamble(JsonObject, ProcessLauncher) ---------------------------------------

    @Test
    void successfulCompactionReturnsTheAgentsOutputAsSuccess() {
        StubProcess process = new StubProcess("Conversation compacted: 12 turns summarized.", "", 0, true);

        ShaftMcpToolResult result = AssistantLocalAgentRunner.runCompactPreamble(
                claudeArguments(), (command, workingDirectory, environment) -> process);

        assertAll(
                () -> assertTrue(result.success(), result.output()),
                () -> assertTrue(result.output().contains("Conversation compacted"), result.output()));
    }

    @Test
    void blankOutputOnSuccessFallsBackToACompleteMessage() {
        StubProcess process = new StubProcess("", "", 0, true);

        ShaftMcpToolResult result = AssistantLocalAgentRunner.runCompactPreamble(
                claudeArguments(), (command, workingDirectory, environment) -> process);

        assertAll(
                () -> assertTrue(result.success(), result.output()),
                () -> assertTrue(result.output().equals("Compaction complete."), result.output()));
    }

    @Test
    void timeoutDestroysTheProcessForciblyAndReportsATimeoutMessageAsSuccess() {
        StubProcess process = new StubProcess("partial", "", 0, false);

        ShaftMcpToolResult result = AssistantLocalAgentRunner.runCompactPreamble(
                claudeArguments(), (command, workingDirectory, environment) -> process);

        assertAll(
                () -> assertTrue(result.success(), "A compact timeout must never fail the caller's request: "
                        + result.output()),
                () -> assertTrue(result.output().contains("timed out"), result.output()),
                () -> assertTrue(process.destroyForciblyCalled(), "A timed-out compact must force-kill the process"));
    }

    @Test
    void processLauncherFailureIsReportedAsASkippedSuccessNotAFailure() {
        ShaftMcpToolResult result = AssistantLocalAgentRunner.runCompactPreamble(claudeArguments(),
                (command, workingDirectory, environment) -> {
                    throw new java.io.IOException("simulated launch failure");
                });

        assertAll(
                () -> assertTrue(result.success(), "A compact launch failure must never fail the caller's request: "
                        + result.output()),
                () -> assertTrue(result.output().contains("Compaction skipped"), result.output()),
                () -> assertTrue(result.output().contains("simulated launch failure"), result.output()));
    }

    @Test
    void unsupportedClientReturnsANotSupportedMessageWithoutLaunchingAnyProcess() {
        AtomicBoolean launcherInvoked = new AtomicBoolean();

        ShaftMcpToolResult result = AssistantLocalAgentRunner.runCompactPreamble(codexArguments(),
                (command, workingDirectory, environment) -> {
                    launcherInvoked.set(true);
                    throw new IllegalStateException("no process may be launched for a client with no compact command");
                });

        assertAll(
                () -> assertTrue(result.success(), result.output()),
                () -> assertTrue(result.output().contains("not supported"), result.output()),
                () -> assertFalse(launcherInvoked.get(), "An unsupported client must never reach the process launcher"));
    }

    // -- runCompactPreamble(JsonObject): the PATH-independent branch ----------------------------

    @Test
    void singleArgOverloadReturnsNotSupportedForClientsWithNoCompactCommandRegardlessOfPath() {
        ShaftMcpToolResult codexResult = AssistantLocalAgentRunner.runCompactPreamble(codexArguments());
        ShaftMcpToolResult copilotResult = AssistantLocalAgentRunner.runCompactPreamble(
                arguments("COPILOT_CLI", ""));

        assertAll(
                () -> assertTrue(codexResult.success(), codexResult.output()),
                () -> assertTrue(codexResult.output().contains("not supported"), codexResult.output()),
                () -> assertTrue(copilotResult.success(), copilotResult.output()),
                () -> assertTrue(copilotResult.output().contains("not supported"), copilotResult.output()));
    }

    // -- startWithOptionalCompact(Invocation, boolean, Consumer) --------------------------------

    @Test
    void autoCompactEnabledForwardsTheSkippedCompactNoticeBeforeAttemptingTheMainInvocation() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "definitely-not-a-real-executable-xyz-12345", false);
        java.util.List<String> consumedLines = new CopyOnWriteArrayList<>();

        ShaftMcpInvocation running = AssistantLocalAgentRunner.startWithOptionalCompact(
                invocation, true, consumedLines::add);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertFalse(result.success(),
                        "The bogus custom-command executable must fail to launch: " + result.output()),
                () -> assertTrue(consumedLines.stream().anyMatch(line -> line.contains("not supported")),
                        "autoCompactEnabled=true must run the compact preamble and forward its notice: "
                                + consumedLines));
    }

    @Test
    void autoCompactDisabledSkipsTheCompactPreambleEntirely() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "definitely-not-a-real-executable-xyz-12345", false);
        java.util.List<String> consumedLines = new CopyOnWriteArrayList<>();

        ShaftMcpInvocation running = AssistantLocalAgentRunner.startWithOptionalCompact(
                invocation, false, consumedLines::add);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertFalse(result.success(), result.output()),
                () -> assertTrue(consumedLines.isEmpty(),
                        "autoCompactEnabled=false must never run the compact preamble: " + consumedLines));
    }

    // -- startWithOptionalCompact(Invocation, boolean, Consumer, ApprovalRequestHandler) --------

    @Test
    void approvalOverloadAlsoRunsCompactWhenEnabledAndForwardsToTheMainInvocation() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "AGENT", ".", "definitely-not-a-real-executable-xyz-67890", false);
        java.util.List<String> consumedLines = new CopyOnWriteArrayList<>();
        LocalAgentApprovalBridge.ApprovalRequestHandler handler =
                (toolName, input) -> CompletableFuture.completedFuture(LocalAgentApprovalBridge.Decision.allow());

        ShaftMcpInvocation running = AssistantLocalAgentRunner.startWithOptionalCompact(
                invocation, true, consumedLines::add, handler);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertFalse(result.success(), result.output()),
                () -> assertTrue(consumedLines.stream().anyMatch(line -> line.contains("not supported")),
                        consumedLines.toString()));
    }

    @Test
    void approvalOverloadSkipsCompactWhenDisabled() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "AGENT", ".", "definitely-not-a-real-executable-xyz-67890", false);
        java.util.List<String> consumedLines = new CopyOnWriteArrayList<>();
        LocalAgentApprovalBridge.ApprovalRequestHandler handler =
                (toolName, input) -> CompletableFuture.completedFuture(LocalAgentApprovalBridge.Decision.allow());

        ShaftMcpInvocation running = AssistantLocalAgentRunner.startWithOptionalCompact(
                invocation, false, consumedLines::add, handler);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertFalse(result.success(), result.output()),
                () -> assertTrue(consumedLines.isEmpty(), consumedLines.toString()));
    }

    // -- startWithOptionalCompact(Invocation, boolean, Consumer, ProcessLauncher): command ordering --

    @Test
    void autoCompactIssuesCompactCommandBeforeUserPromptInvocation() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CLAUDE_CODE", "ASK", ".", "stub-agent --print", false);
        List<List<String>> launchedCommands = new CopyOnWriteArrayList<>();
        AtomicInteger launchCount = new AtomicInteger();

        AssistantLocalAgentRunner.ProcessLauncher launcher = (command, workingDirectory, environment) -> {
            launchedCommands.add(command);
            int index = launchCount.getAndIncrement();
            return index == 0
                    ? new StubProcess("Compaction complete.", "", 0, true)
                    : new StubProcess("Explanation of the failure.", "", 0, true);
        };

        ShaftMcpInvocation running = AssistantLocalAgentRunner.startWithOptionalCompact(
                invocation, true, output -> { }, launcher);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertEquals(2, launchedCommands.size()),
                () -> assertTrue(launchedCommands.get(0).contains("/compact"),
                        "Compact command should be issued first: " + launchedCommands),
                () -> assertEquals(AssistantLocalAgentRunner.commandFor(invocation.arguments()),
                        launchedCommands.get(1)));
    }

    @Test
    void autoCompactSkipsGracefullyWithoutFailingRequestWhenCliDoesNotSupportCompaction() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "COPILOT_CLI", "ASK", ".", "stub-agent ask", false);
        List<List<String>> launchedCommands = new CopyOnWriteArrayList<>();
        AssistantLocalAgentRunner.ProcessLauncher launcher = (command, workingDirectory, environment) -> {
            launchedCommands.add(command);
            return new StubProcess("ask response", "", 0, true);
        };

        ShaftMcpInvocation running = AssistantLocalAgentRunner.startWithOptionalCompact(
                invocation, true, output -> { }, launcher);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertAll(
                () -> assertTrue(result.success()),
                () -> assertEquals(1, launchedCommands.size(), "Unsupported CLI should skip compaction, not fail: "
                        + launchedCommands),
                () -> assertEquals(AssistantLocalAgentRunner.commandFor(invocation.arguments()),
                        launchedCommands.get(0)));
    }

    private static JsonObject claudeArguments() {
        return arguments("CLAUDE_CODE", ".");
    }

    private static JsonObject codexArguments() {
        return arguments("CODEX", ".");
    }

    private static JsonObject arguments(String client, String workingDirectory) {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("client", client);
        arguments.addProperty("workingDirectory", workingDirectory);
        return arguments;
    }

    /**
     * Minimal stub {@link Process}, mirroring the pattern in
     * {@code AssistantLocalAgentRunnerModelDiscoveryTest}, that replays fixed stdout/stderr, an exit
     * code, and whether {@link #waitFor(long, TimeUnit)} reports finished or timed out.
     */
    private static final class StubProcess extends Process {
        private final InputStream stdout;
        private final InputStream stderr;
        private final int exitCode;
        private final boolean finishes;
        private final AtomicBoolean destroyForciblyCalled = new AtomicBoolean();

        StubProcess(String stdout, String stderr, int exitCode, boolean finishes) {
            this.stdout = new ByteArrayInputStream(stdout.getBytes(StandardCharsets.UTF_8));
            this.stderr = new ByteArrayInputStream(stderr.getBytes(StandardCharsets.UTF_8));
            this.exitCode = exitCode;
            this.finishes = finishes;
        }

        boolean destroyForciblyCalled() {
            return destroyForciblyCalled.get();
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
            return finishes;
        }

        @Override
        public int exitValue() {
            return exitCode;
        }

        @Override
        public void destroy() {
            // Not exercised: runCompactPreamble only ever force-destroys on timeout.
        }

        @Override
        public Process destroyForcibly() {
            destroyForciblyCalled.set(true);
            return this;
        }

        @Override
        public boolean isAlive() {
            return !finishes;
        }
    }
}
