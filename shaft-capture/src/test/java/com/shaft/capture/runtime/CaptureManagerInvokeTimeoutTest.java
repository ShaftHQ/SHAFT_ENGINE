package com.shaft.capture.runtime;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.CountDownLatch;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * CaptureManager.invoke() used to call executor.submit(operation).get() with no timeout (#4066):
 * a recorder operation that blocks forever (a wedged CDP/BiDi round trip, a stuck WebDriver quit)
 * hung the MCP tool call forever, with no upper bound and no diagnosable failure. Each test carries
 * its own hard wall-clock bound (assertTimeoutPreemptively) so a regression in the fix -- the exact
 * failure mode under repair -- cannot hang this suite.
 */
class CaptureManagerInvokeTimeoutTest {
    @TempDir
    Path temp;

    @Test
    void startTimesOutWithANamedDiagnosableErrorInsteadOfHangingForever() {
        CountDownLatch releaseBlockedStart = new CountDownLatch(1);
        CaptureManager manager = new CaptureManager(
                request -> new BlockingRecorder(request, releaseBlockedStart),
                Duration.ofMillis(200));

        try {
            IllegalStateException failure = assertTimeoutPreemptively(Duration.ofSeconds(5), () ->
                    assertThrows(IllegalStateException.class, () -> manager.start(request("blocked.json"))));

            assertTrue(failure.getMessage().contains("start"), failure.getMessage());
            assertTrue(failure.getMessage().toLowerCase(java.util.Locale.ROOT).contains("timed out"),
                    failure.getMessage());
        } finally {
            // Let the blocked worker thread unwind so the (non-daemon) executor thread can exit
            // cleanly instead of leaking a permanently blocked thread past this test.
            releaseBlockedStart.countDown();
            manager.close();
        }
    }

    @Test
    void aLegitimatelySlowOperationStillSucceedsUnderTheBound() {
        CaptureManager manager = new CaptureManager(SlowRecorder::new, Duration.ofSeconds(2));

        try {
            CaptureStatus status = assertTimeoutPreemptively(Duration.ofSeconds(5),
                    () -> manager.start(request("slow.json")));

            assertEquals(CaptureStatus.State.ACTIVE, status.state());
        } finally {
            manager.stop(false);
            manager.close();
        }
    }

    private CaptureStartRequest request(String outputName) {
        return new CaptureStartRequest(
                "https://example.test",
                CaptureBrowser.CHROME,
                temp.resolve(outputName),
                temp.resolve("runtime"),
                true);
    }

    private static final class BlockingRecorder extends ManagedCaptureRecorder {
        private final CountDownLatch releaseSignal;
        private volatile CaptureStatus.State state = CaptureStatus.State.STARTING;

        BlockingRecorder(CaptureStartRequest request, CountDownLatch releaseSignal) {
            super(request);
            this.releaseSignal = releaseSignal;
        }

        @Override
        void start() {
            try {
                releaseSignal.await();
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
            }
            state = CaptureStatus.State.ACTIVE;
        }

        @Override
        CaptureStatus status() {
            return fakeStatus(state);
        }

        @Override
        void checkpoint(String description, com.shaft.capture.model.Checkpoint.CheckpointKind kind) {
            // Unused: this double only exercises start()'s timeout path.
        }

        @Override
        CaptureStatus stop(boolean discard) {
            state = discard ? CaptureStatus.State.DISCARDED : CaptureStatus.State.COMPLETED;
            return status();
        }

        @Override
        CaptureStatus interrupt() {
            state = CaptureStatus.State.INCOMPLETE;
            return status();
        }

        @Override
        boolean isBrowserAlive() {
            return state == CaptureStatus.State.ACTIVE;
        }
    }

    private static final class SlowRecorder extends ManagedCaptureRecorder {
        private volatile CaptureStatus.State state = CaptureStatus.State.STARTING;

        SlowRecorder(CaptureStartRequest request) {
            super(request);
        }

        @Override
        void start() {
            try {
                // Well under the 2s bound configured above, but not instantaneous: proves a
                // legitimately slow (bounded) operation is not spuriously killed by the timeout.
                Thread.sleep(300);
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
            }
            state = CaptureStatus.State.ACTIVE;
        }

        @Override
        CaptureStatus status() {
            return fakeStatus(state);
        }

        @Override
        void checkpoint(String description, com.shaft.capture.model.Checkpoint.CheckpointKind kind) {
            // Unused: this double only exercises the slow-but-successful start() path.
        }

        @Override
        CaptureStatus stop(boolean discard) {
            state = discard ? CaptureStatus.State.DISCARDED : CaptureStatus.State.COMPLETED;
            return status();
        }

        @Override
        CaptureStatus interrupt() {
            state = CaptureStatus.State.INCOMPLETE;
            return status();
        }

        @Override
        boolean isBrowserAlive() {
            return state == CaptureStatus.State.ACTIVE;
        }
    }

    private static CaptureStatus fakeStatus(CaptureStatus.State state) {
        return new CaptureStatus(
                state,
                "fake-session",
                CaptureBrowser.CHROME.name().toLowerCase(),
                "https://example.test",
                0,
                List.of(),
                "",
                false,
                ProcessHandle.current().pid(),
                Instant.parse("2026-01-02T03:04:05Z"));
    }
}
