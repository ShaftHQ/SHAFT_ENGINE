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
}
