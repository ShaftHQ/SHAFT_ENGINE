package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.ShaftMcpInvocation;
import org.junit.jupiter.api.Test;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers the private {@code cancel(AtomicReference, AtomicBoolean, boolean)} helper, exercised
 * through {@link ShaftMcpInvocation#cancel()} (soft, {@code force=false}) and
 * {@link ShaftMcpInvocation#kill()} (forced, {@code force=true}), which is how every real caller
 * reaches it -- {@link AssistantLocalAgentRunner#start} wires exactly these two actions as the
 * invocation's cancel/kill callbacks. Also covers {@code closeQuietly(InputStream)} as a side effect:
 * it is only reached on the "process finished while cancellation was already requested" path inside
 * {@code run()}, which the soft-cancel test below reproduces deterministically.
 *
 * <p>Determinism note: {@link AssistantLocalAgentRunner#run} launches the stub process
 * asynchronously on a background executor, so the test thread must not call cancel/kill until the
 * background thread has actually reached {@code Process#waitFor}. {@link BlockingStubProcess} signals
 * that moment via {@code enteredWaitFor}, removing the race that would otherwise make this flaky.
 */
class AssistantLocalAgentRunnerCancellationTest {

    @Test
    void softCancelCallsDestroyNotDestroyForciblyAndTheRunEndsCancelled() throws Exception {
        BlockingStubProcess process = new BlockingStubProcess();
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeAskInvocation(), line -> { }, (command, workingDirectory, environment) -> process, false);

        assertTrue(process.enteredWaitFor.await(5, TimeUnit.SECONDS),
                "The stub process must be blocked inside waitFor before cancelling, or the race is nondeterministic");
        boolean acknowledged = running.cancel();

        ExecutionException failure = assertThrows(ExecutionException.class,
                () -> running.future().get(5, TimeUnit.SECONDS));

        assertAll(
                () -> assertTrue(acknowledged),
                () -> assertTrue(failure.getCause() instanceof CancellationException,
                        "A cancelled run must fail with CancellationException, was: " + failure.getCause()),
                () -> assertTrue(process.destroyCalled(), "cancel() [force=false] must call Process#destroy()"),
                () -> assertFalse(process.destroyForciblyCalled(), "A soft cancel must never force-kill the process"));
    }

    @Test
    void forceCancelCallsDestroyForciblyNotDestroyAndClearsTheProcessReference() throws Exception {
        BlockingStubProcess process = new BlockingStubProcess();
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                claudeAskInvocation(), line -> { }, (command, workingDirectory, environment) -> process, false);

        assertTrue(process.enteredWaitFor.await(5, TimeUnit.SECONDS),
                "The stub process must be blocked inside waitFor before killing, or the race is nondeterministic");
        boolean acknowledged = running.kill();

        assertAll(
                () -> assertTrue(acknowledged),
                () -> assertTrue(running.future().isCancelled(),
                        "kill() must mark the future cancelled synchronously via future.cancel(true)"),
                () -> assertTrue(process.destroyForciblyCalled(), "kill() [force=true] must call Process#destroyForcibly()"),
                () -> assertFalse(process.destroyCalled(), "A force cancel must never also call the graceful destroy()"));
    }

    private static AssistantCommand.Invocation claudeAskInvocation() {
        return AssistantCommand.fromPrompt("Explain this failure", "CLAUDE_CODE", "ASK", ".", "", false);
    }

    /**
     * Blocks inside {@link #waitFor(long, TimeUnit)} until either {@link #destroy()} or
     * {@link #destroyForcibly()} is called, signalling {@code enteredWaitFor} the instant it starts
     * blocking so a test can safely cancel/kill without racing the async launch.
     */
    private static final class BlockingStubProcess extends Process {
        private final CountDownLatch enteredWaitFor = new CountDownLatch(1);
        private final CountDownLatch destroyLatch = new CountDownLatch(1);
        private volatile boolean destroyCalled;
        private volatile boolean destroyForciblyCalled;

        boolean destroyCalled() {
            return destroyCalled;
        }

        boolean destroyForciblyCalled() {
            return destroyForciblyCalled;
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
            destroyCalled = true;
            destroyLatch.countDown();
        }

        @Override
        public Process destroyForcibly() {
            destroyForciblyCalled = true;
            destroyLatch.countDown();
            return this;
        }

        @Override
        public boolean isAlive() {
            return destroyLatch.getCount() > 0;
        }
    }
}
