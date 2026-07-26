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
import java.util.function.Consumer;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression coverage for issue #4164: {@code AssistantLocalAgentRunner.readAsync} used to discard
 * every line already buffered from a process's stdout/stderr when the underlying stream threw
 * {@link IOException} partway through (e.g. a pipe torn down abruptly), resolving to an empty
 * string instead of the content genuinely captured so far. A code-path-distinct sibling of issue
 * #3962's discard class (that one is {@code Future}-cancellation ordering; this one is a plain
 * stream-read exception with no cancellation involved).
 */
class AssistantLocalAgentRunnerStreamReadResilienceTest {

    @Test
    void alreadyBufferedStdoutSurvivesAnIOExceptionPartwayThroughTheStream() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "stub-agent --print", false);
        StubProcess process = StubProcess.stdoutThrowingAfterLines("line one", "line two");

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertTrue(result.success(), result.output());
        assertTrue(result.output().contains("line one"),
                "Already-buffered stdout must survive a mid-stream IOException: " + result.output());
        assertTrue(result.output().contains("line two"),
                "Already-buffered stdout must survive a mid-stream IOException: " + result.output());
    }

    /**
     * Sibling defect in the same file, same investigation (issue #4164 follow-up): {@code
     * stdoutNow}/{@code stderrNow} returned {@code ""} on <em>any</em> exception from {@code
     * future.get(2, TimeUnit.SECONDS)} -- including a plain {@link java.util.concurrent.TimeoutException}
     * -- discarding whatever the background reader thread had already captured. The process has
     * already terminated by every caller of these methods (this stub reports a prompt, successful
     * exit), so a still-incomplete read at the 2-second mark means the drain is slow, not silent;
     * its buffered content is exactly what a user needs to see when something hangs.
     */
    @Test
    void alreadyBufferedStdoutSurvivesWhenTheDrainReadIsStillRunningPastTheGraceWindow() throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "stub-agent --print", false);
        StubProcess process = StubProcess.stdoutBlockingAfterOneLine(
                "buffered before the drain stalled", 5000);

        long startNanos = System.nanoTime();
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process);
        ShaftMcpToolResult result = running.future().get(8, TimeUnit.SECONDS);
        long elapsedMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);

        assertTrue(elapsedMillis < 4500,
                "The result must arrive via stdoutNow's own 2-second grace window timing out, not by "
                        + "waiting out the simulated 5-second stall: took " + elapsedMillis + "ms");
        assertTrue(result.success(), result.output());
        assertTrue(result.output().contains("buffered before the drain stalled"),
                "Buffered stdout must survive stdoutNow's own get() timing out while the background "
                        + "read is still running: " + result.output());
    }

    /**
     * Same defect, a different triggering path: the read future can also complete <em>exceptionally</em>
     * for a reason that has nothing to do with a timeout (here, the caller-supplied live
     * {@code outputConsumer} itself throws) -- {@code stdoutNow}'s old blanket {@code catch (Exception)}
     * discarded the buffer in this case too. The already-buffered line is appended before the consumer
     * is invoked (see {@code readAsync}), so it must survive regardless of what the consumer does.
     */
    @Test
    void alreadyBufferedStdoutSurvivesWhenTheReadFutureCompletesExceptionallyForAnUnrelatedReason()
            throws Exception {
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", "CODEX", "ASK", ".", "stub-agent --print", false);
        StubProcess process = StubProcess.stdoutThrowingAfterLines("captured before the consumer failed");
        Consumer<String> throwingConsumer = line -> {
            throw new IllegalStateException("consumer boom");
        };

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, throwingConsumer, (command, workingDirectory, environment) -> process);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);

        assertTrue(result.output().contains("captured before the consumer failed"),
                "Buffered stdout must survive the read future completing exceptionally for a non-timeout "
                        + "reason: " + result.output());
    }

    /**
     * Minimal stub {@link Process} whose stdout yields a handful of complete lines and then throws
     * {@link IOException} instead of reaching EOF, simulating a stream torn down mid-read. Mirrors
     * the proven {@code StubProcess} pattern in {@code AssistantLocalAgentRunnerCommandTest}.
     */
    private static final class StubProcess extends Process {
        private final InputStream stdout;

        private StubProcess(InputStream stdout) {
            this.stdout = stdout;
        }

        static StubProcess stdoutThrowingAfterLines(String... lines) {
            return new StubProcess(new ThrowingAfterLinesInputStream(lines));
        }

        static StubProcess stdoutBlockingAfterOneLine(String line, long blockMillis) {
            return new StubProcess(new BlockingAfterLineInputStream(line, blockMillis));
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
            // Not exercised: this stub always reports a prompt, successful exit.
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
     * Yields the given lines (newline-terminated) and then throws {@link IOException} on the next
     * read attempt instead of signalling clean EOF (-1), simulating a pipe torn down mid-stream.
     */
    private static final class ThrowingAfterLinesInputStream extends InputStream {
        private final byte[] bytes;
        private int position;

        ThrowingAfterLinesInputStream(String... lines) {
            StringBuilder builder = new StringBuilder();
            for (String line : lines) {
                builder.append(line).append('\n');
            }
            this.bytes = builder.toString().getBytes(StandardCharsets.UTF_8);
        }

        @Override
        public int read() throws IOException {
            if (position >= bytes.length) {
                throw new IOException("Simulated pipe teardown after buffered lines");
            }
            return bytes[position++] & 0xFF;
        }
    }

    /**
     * Yields one newline-terminated line in its very first bulk read, then blocks for {@code
     * blockMillis} on the next read before signalling clean EOF -- simulating a background reader
     * thread whose drain is still genuinely in flight when a caller's short grace-period wait gives
     * up on it. Overrides the bulk {@link #read(byte[], int, int)} directly (rather than relying on
     * {@link InputStream}'s default single-byte-at-a-time loop) so the line's bytes are handed back
     * in one fill -- otherwise the default loop would keep calling {@link #read()} within the very
     * same bulk read and block before ever returning the line to {@code BufferedReader.readLine()}.
     */
    private static final class BlockingAfterLineInputStream extends InputStream {
        private final byte[] bytes;
        private final long blockMillis;
        private boolean lineDelivered;
        private boolean blocked;

        BlockingAfterLineInputStream(String line, long blockMillis) {
            this.bytes = (line + "\n").getBytes(StandardCharsets.UTF_8);
            this.blockMillis = blockMillis;
        }

        @Override
        public synchronized int read() throws IOException {
            byte[] single = new byte[1];
            int read = read(single, 0, 1);
            return read == -1 ? -1 : single[0] & 0xFF;
        }

        @Override
        public synchronized int read(byte[] b, int off, int len) throws IOException {
            if (!lineDelivered) {
                lineDelivered = true;
                int length = Math.min(len, bytes.length);
                System.arraycopy(bytes, 0, b, off, length);
                return length;
            }
            if (!blocked) {
                blocked = true;
                try {
                    Thread.sleep(blockMillis);
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while simulating a stalled drain", interrupted);
                }
            }
            return -1;
        }
    }
}
