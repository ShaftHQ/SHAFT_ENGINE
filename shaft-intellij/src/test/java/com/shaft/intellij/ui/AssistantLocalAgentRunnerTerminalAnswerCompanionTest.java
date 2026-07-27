package com.shaft.intellij.ui;

import com.shaft.intellij.approval.LocalAgentApprovalBridge;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #3962: {@code ShaftMcpInvocation.cancel()}/{@code kill()} call {@code future.cancel(true)}
 * <em>before</em> running their cancel/kill action (issue #3758/#3768's own validated ordering,
 * unchanged here) -- so once that call wins, any value {@link AssistantLocalAgentRunner#run}
 * eventually returns or throws is silently discarded by the JDK's own {@code Future} contract, even
 * when the structured stream parser had already captured a real terminal answer. A
 * {@code terminalAnswerConsumer} passed to {@link AssistantLocalAgentRunner#start} exists precisely
 * to survive that discard: {@code run()}'s own {@code finally} block notifies it exactly once, with
 * the parsed terminal answer (or {@code null} if none was ever captured), regardless of whether the
 * primary future was cancelled first -- a side channel {@code future.cancel(true)} cannot touch.
 *
 * <p>Mirrors {@link AssistantLocalAgentRunnerCancellationTest}'s {@code BlockingStubProcess} pattern:
 * the stub blocks inside {@code waitFor} until killed, exactly like a real long-running CLI process.
 */
class AssistantLocalAgentRunnerTerminalAnswerCompanionTest {

    @Test
    void terminalAnswerConsumerReceivesTheParsedAnswerEvenWhenKillWinsTheRace() throws Exception {
        CountDownLatch stdoutFullyRead = new CountDownLatch(1);
        TerminalEventStubProcess process = new TerminalEventStubProcess(
                claudeResultEvent("The finished answer"), stdoutFullyRead);
        List<String> terminalAnswers = new CopyOnWriteArrayList<>();
        // kill() only cancels the primary future and unblocks the background run() thread -- it does
        // NOT wait for that thread to actually reach its own finally block and notify the companion,
        // so asserting on terminalAnswers right after kill() returns would be a race. This latch makes
        // "the companion has fired" an observable, awaited event instead.
        CountDownLatch companionNotified = new CountDownLatch(1);
        Consumer<String> terminalAnswerConsumer = answer -> {
            terminalAnswers.add(answer);
            companionNotified.countDown();
        };

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeAskInvocation(), line -> { }, (command, workingDirectory, environment) -> process,
                false, null, LocalAgentApprovalBridge::start, false, terminalAnswerConsumer);

        assertTrue(process.enteredWaitFor.await(5, TimeUnit.SECONDS),
                "The stub process must be blocked inside waitFor before killing, or the race is nondeterministic");
        assertTrue(stdoutFullyRead.await(5, TimeUnit.SECONDS),
                "The terminal event must already be parsed before kill() races it, matching the issue's scenario");

        boolean acknowledged = running.kill();

        // The PRIMARY future is still cancelled deterministically first -- #3758's own invariant,
        // untouched by this change (asserted here as a guard the fix must never regress).
        assertTrue(running.future().isCancelled(),
                "kill() must still mark the future cancelled synchronously via future.cancel(true)");
        assertTrue(companionNotified.await(5, TimeUnit.SECONDS),
                "The companion must fire even though kill() itself does not wait for run()'s own finally block");

        assertAll(
                () -> assertTrue(acknowledged),
                () -> assertEquals(1, terminalAnswers.size(),
                        "The companion consumer must fire exactly once even though the primary future discarded "
                                + "its value: " + terminalAnswers),
                () -> assertTrue(terminalAnswers.get(0).contains("The finished answer"),
                        "The companion must carry the real parsed terminal answer: " + terminalAnswers));
    }

    @Test
    void terminalAnswerConsumerReceivesNullWhenTheRunIsKilledBeforeAnyTerminalEvent() throws Exception {
        CountDownLatch stdoutFullyRead = new CountDownLatch(1);
        // No terminal event on stdout at all -- just an empty stream, so hasTerminalEvent() stays
        // false for the whole run.
        TerminalEventStubProcess process = new TerminalEventStubProcess("", stdoutFullyRead);
        List<String> terminalAnswers = new CopyOnWriteArrayList<>();
        CountDownLatch companionNotified = new CountDownLatch(1);
        Consumer<String> terminalAnswerConsumer = answer -> {
            terminalAnswers.add(answer);
            companionNotified.countDown();
        };

        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeAskInvocation(), line -> { }, (command, workingDirectory, environment) -> process,
                false, null, LocalAgentApprovalBridge::start, false, terminalAnswerConsumer);

        assertTrue(process.enteredWaitFor.await(5, TimeUnit.SECONDS),
                "The stub process must be blocked inside waitFor before killing, or the race is nondeterministic");

        running.kill();

        assertTrue(companionNotified.await(5, TimeUnit.SECONDS),
                "The companion must fire even though kill() itself does not wait for run()'s own finally block");
        assertEquals(Collections.singletonList(null), terminalAnswers,
                "The companion must still fire (with null) so a caller waiting on it is never left hanging: "
                        + terminalAnswers);
    }

    private static AssistantCommand.Invocation claudeAskInvocation() {
        return AssistantCommand.fromPrompt("Explain this failure", "CLAUDE_CODE", "ASK", ".", "", false);
    }

    private static String claudeResultEvent(String result) {
        return "{\"type\":\"result\",\"result\":\"" + result + "\",\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}";
    }

    /**
     * Blocks inside {@link #waitFor(long, TimeUnit)} until {@link #destroyForcibly()} is called
     * (mirrors {@code AssistantLocalAgentRunnerCancellationTest.BlockingStubProcess}), but replays a
     * real structured-stream terminal event on stdout first, signalling {@code stdoutFullyRead} once
     * every line has actually been drained -- the background reader thread (started before the main
     * run() thread even reaches {@code awaitProcessWithApprovalExtension}) processes each line
     * through {@code StructuredStreamParser.accept} synchronously as it reads, so EOF is proof the
     * terminal event was already parsed.
     */
    private static final class TerminalEventStubProcess extends Process {
        private final CountDownLatch enteredWaitFor = new CountDownLatch(1);
        private final CountDownLatch destroyLatch = new CountDownLatch(1);
        private final InputStream stdout;

        TerminalEventStubProcess(String stdoutContent, CountDownLatch stdoutFullyRead) {
            byte[] bytes = (stdoutContent + "\n").getBytes(StandardCharsets.UTF_8);
            InputStream raw = new ByteArrayInputStream(bytes);
            this.stdout = new InputStream() {
                @Override
                public int read() throws IOException {
                    int value = raw.read();
                    if (value == -1) {
                        stdoutFullyRead.countDown();
                    }
                    return value;
                }

                @Override
                public int read(byte[] b, int off, int len) throws IOException {
                    int count = raw.read(b, off, len);
                    if (count == -1) {
                        stdoutFullyRead.countDown();
                    }
                    return count;
                }
            };
        }

        @Override
        public OutputStream getOutputStream() {
            return OutputStream.nullOutputStream();
        }

        @Override
        public InputStream getInputStream() {
            return stdout;
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
        public boolean waitFor(long timeout, TimeUnit unit) throws InterruptedException {
            enteredWaitFor.countDown();
            return destroyLatch.await(timeout, unit);
        }

        @Override
        public int exitValue() {
            return 0;
        }

        @Override
        public void destroy() {
            destroyLatch.countDown();
        }

        @Override
        public Process destroyForcibly() {
            destroyLatch.countDown();
            return this;
        }

        @Override
        public boolean isAlive() {
            return destroyLatch.getCount() > 0;
        }
    }
}
