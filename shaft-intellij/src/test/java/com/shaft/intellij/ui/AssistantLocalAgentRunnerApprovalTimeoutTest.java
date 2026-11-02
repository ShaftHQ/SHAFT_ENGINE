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
import java.time.Duration;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.function.LongSupplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers issue #3633: time spent by the user deliberating on an interactive tool-approval prompt
 * must not be charged against the local-agent run's own timeout, up to a bounded extension cap
 * that still guarantees abandonment cleanup. The polling seam exercised here,
 * {@link AssistantLocalAgentRunner#awaitProcessWithDeadlineExtension}, is package-private
 * specifically so these tests can drive it with a fake accumulated-pending-time supplier and a
 * tiny fake cap, without waiting on real approval-bridge wall-clock time or the real 10-minute cap.
 */
class AssistantLocalAgentRunnerApprovalTimeoutTest {

    @Test
    void pendingApprovalExtendsTheDeadlineBeyondTheNominalTimeout() throws Exception {
        Duration nominalTimeout = Duration.ofMillis(50);
        long maxExtensionMillis = 5000L;
        LongSupplier accumulatedPendingMillis = () -> 5000L;
        // Reports "not finished" until real elapsed time passes 150ms -- past the nominal 50ms
        // deadline, but nowhere near the extended ~5050ms deadline, so only the extension explains
        // survival past 50ms.
        RealTimeFakeProcess process = new RealTimeFakeProcess(150L);

        long startNanos = System.nanoTime();
        boolean finished = AssistantLocalAgentRunner.awaitProcessWithDeadlineExtension(
                process, nominalTimeout, maxExtensionMillis, accumulatedPendingMillis);
        long elapsedMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);

        assertTrue(finished, "Deliberation up to the cap must never kill the run");
        assertTrue(elapsedMillis >= 50L,
                "Should have survived past the nominal 50ms deadline, took " + elapsedMillis + "ms");
    }

    @Test
    void abandonmentPastTheExtensionCapStillKillsTheRun() throws Exception {
        Duration nominalTimeout = Duration.ofMillis(50);
        long tinyMaxExtensionMillis = 100L;
        // An abandoned/still-pending approval that would extend forever if the cap were not applied.
        LongSupplier hugeAccumulatedPendingMillis = () -> 999_999L;
        NeverFinishingFakeProcess process = new NeverFinishingFakeProcess();

        long startNanos = System.nanoTime();
        boolean finished = AssistantLocalAgentRunner.awaitProcessWithDeadlineExtension(
                process, nominalTimeout, tinyMaxExtensionMillis, hugeAccumulatedPendingMillis);
        long elapsedMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);

        assertFalse(finished, "Abandonment past the extension cap must still be cleaned up");
        assertTrue(elapsedMillis < 2000L,
                "Total effective deadline was only 50+100=150ms, took " + elapsedMillis + "ms");
    }

    @Test
    void noApprovalRunsAreUnchanged() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "stub-agent --print", false);
        RecordingWaitForProcess process = RecordingWaitForProcess.completing("plain buffered output\n");

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertTrue(result.success());
        assertEquals(1, process.waitForCallCount(),
                "A no-bridge run must bypass the polling loop entirely, not poll in slices");
        // AssistantCommand.fromPrompt hard-codes its own 300s default timeout.
        assertEquals(TimeUnit.SECONDS.toMillis(300), process.lastWaitForTimeoutMillis(),
                "A no-bridge run must wait on the full nominal timeout in a single call");
    }

    @Test
    void narrateApprovalEmitsAWaitingLineWhenAnApprovalRequestStartsAndAResolvedLineWhenItCompletes() {
        List<String> capturedLines = new CopyOnWriteArrayList<>();
        CompletableFuture<LocalAgentApprovalBridge.Decision> pendingFuture = new CompletableFuture<>();
        LocalAgentApprovalBridge.ApprovalRequestHandler fakeHandler = (toolName, input) -> pendingFuture;

        LocalAgentApprovalBridge.ApprovalRequestHandler wrapped =
                AssistantLocalAgentRunner.narrateApproval(fakeHandler, capturedLines::add);
        wrapped.requestApproval("some_tool", new JsonObject());

        assertEquals(1, capturedLines.size(), "A waiting line must be emitted before the future resolves");
        assertTrue(capturedLines.get(0).contains("Waiting for your approval"), capturedLines.get(0));
        assertTrue(capturedLines.get(0).contains("some_tool"), capturedLines.get(0));

        pendingFuture.complete(LocalAgentApprovalBridge.Decision.allow());

        assertEquals(2, capturedLines.size(), "A resolved line must be emitted once the future completes");
        assertTrue(capturedLines.get(1).contains("Approval resolved"), capturedLines.get(1));
    }

    /**
     * Reports "not finished" until real elapsed wall-clock time (since construction) passes
     * {@code finishAfterMillis}, actually sleeping for each requested slice so the deadline-extension
     * polling loop's real-time behavior is exercised rather than short-circuited.
     */
    private static final class RealTimeFakeProcess extends Process {
        private final long finishAfterMillis;
        private final long startNanos = System.nanoTime();

        private RealTimeFakeProcess(long finishAfterMillis) {
            this.finishAfterMillis = finishAfterMillis;
        }

        @Override
        public boolean waitFor(long timeout, TimeUnit unit) throws InterruptedException {
            Thread.sleep(unit.toMillis(timeout));
            long elapsedMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);
            return elapsedMillis >= finishAfterMillis;
        }

        @Override
        public OutputStream getOutputStream() {
            return OutputStream.nullOutputStream();
        }

        @Override
        public InputStream getInputStream() {
            return InputStream.nullInputStream();
        }

        @Override
        public InputStream getErrorStream() {
            return InputStream.nullInputStream();
        }

        @Override
        public int waitFor() {
            return 0;
        }

        @Override
        public int exitValue() {
            return 0;
        }

        @Override
        public void destroy() {
            // Not exercised by this test.
        }

        @Override
        public boolean isAlive() {
            return true;
        }
    }

    /** Never reports finished, and never blocks -- used to prove the extension cap is respected. */
    private static final class NeverFinishingFakeProcess extends Process {
        @Override
        public boolean waitFor(long timeout, TimeUnit unit) {
            return false;
        }

        @Override
        public OutputStream getOutputStream() {
            return OutputStream.nullOutputStream();
        }

        @Override
        public InputStream getInputStream() {
            return InputStream.nullInputStream();
        }

        @Override
        public InputStream getErrorStream() {
            return InputStream.nullInputStream();
        }

        @Override
        public int waitFor() {
            return 0;
        }

        @Override
        public int exitValue() {
            throw new IllegalThreadStateException("process has not exited");
        }

        @Override
        public void destroy() {
            // Not exercised by this test.
        }

        @Override
        public boolean isAlive() {
            return true;
        }
    }

    /**
     * Minimal stub {@link Process} (mirrors {@code StubProcess} in
     * {@code AssistantLocalAgentRunnerCommandTest}) that additionally records how many times, and
     * with what timeout argument, {@link #waitFor(long, TimeUnit)} was called -- so a no-bridge run
     * can be proven to bypass the deadline-extension polling loop and wait exactly once on the full
     * nominal timeout.
     */
    private static final class RecordingWaitForProcess extends Process {
        private final InputStream stdout;
        private final RecordingOutputStream stdin = new RecordingOutputStream();
        private int waitForCallCount;
        private long lastWaitForTimeoutMillis;

        private RecordingWaitForProcess(InputStream stdout) {
            this.stdout = stdout;
        }

        static RecordingWaitForProcess completing(String stdoutContent) {
            return new RecordingWaitForProcess(new ByteArrayInputStream(stdoutContent.getBytes(StandardCharsets.UTF_8)));
        }

        int waitForCallCount() {
            return waitForCallCount;
        }

        long lastWaitForTimeoutMillis() {
            return lastWaitForTimeoutMillis;
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
            waitForCallCount++;
            lastWaitForTimeoutMillis = unit.toMillis(timeout);
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
     * Captures every byte written to it and tracks whether {@link #close()} was called (mirrors
     * {@code RecordingOutputStream} in {@code AssistantLocalAgentRunnerCommandTest}).
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
    }
}
