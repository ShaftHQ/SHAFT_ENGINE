package com.shaft.capture.runtime;

import com.shaft.capture.model.Checkpoint;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The periodic browser health check must only tear a session down after consecutive liveness
 * failures: one transient WebDriver hiccup (a busy page, a slow DevTools round trip) used to
 * interrupt the recorder immediately, silently discarding every later interaction (issue #3365).
 */
class CaptureManagerHealthCheckTest {
    @TempDir
    Path temp;

    @Test
    void transientLivenessFailuresDoNotInterruptTheSession() throws Exception {
        // Browser reports dead for two consecutive checks, then recovers.
        AtomicReference<FlakyRecorder> recorderRef = new AtomicReference<>();
        CaptureManager manager = new CaptureManager(request -> {
            FlakyRecorder recorder = new FlakyRecorder(request, 2);
            recorderRef.set(recorder);
            return recorder;
        });

        assertEquals(CaptureStatus.State.ACTIVE, manager.start(request("transient.json")).state());
        waitFor(() -> recorderRef.get().livenessChecks.get() >= 4, Duration.ofSeconds(15));

        assertEquals(0, recorderRef.get().interruptCount.get(),
                "Transient liveness failures must not interrupt the recording session.");
        assertEquals(CaptureStatus.State.ACTIVE, manager.status().state());
        assertEquals(CaptureStatus.State.COMPLETED, manager.stop(false).state());
        manager.close();
    }

    @Test
    void persistentLivenessFailuresInterruptTheSession() throws Exception {
        AtomicReference<FlakyRecorder> recorderRef = new AtomicReference<>();
        CaptureManager manager = new CaptureManager(request -> {
            FlakyRecorder recorder = new FlakyRecorder(request, Integer.MAX_VALUE);
            recorderRef.set(recorder);
            return recorder;
        });

        assertEquals(CaptureStatus.State.ACTIVE, manager.start(request("persistent.json")).state());
        waitFor(() -> recorderRef.get().interruptCount.get() > 0, Duration.ofSeconds(15));

        assertEquals(1, recorderRef.get().interruptCount.get());
        assertTrue(recorderRef.get().livenessChecks.get() >= 3,
                "The session must only be interrupted after consecutive failed liveness checks.");
        assertEquals(CaptureStatus.State.INCOMPLETE, manager.status().state());
        manager.close();
    }

    private CaptureStartRequest request(String outputName) {
        return new CaptureStartRequest(
                "https://example.test",
                CaptureBrowser.CHROME,
                temp.resolve(outputName),
                temp.resolve("runtime"),
                true);
    }

    private static void waitFor(java.util.function.BooleanSupplier condition, Duration timeout)
            throws InterruptedException {
        long deadline = System.nanoTime() + timeout.toNanos();
        while (!condition.getAsBoolean() && System.nanoTime() < deadline) {
            Thread.sleep(100);
        }
        assertTrue(condition.getAsBoolean());
    }

    private static final class FlakyRecorder extends ManagedCaptureRecorder {
        private final CaptureStartRequest request;
        private final int deadChecks;
        private final AtomicInteger livenessChecks = new AtomicInteger();
        private final AtomicInteger interruptCount = new AtomicInteger();
        private CaptureStatus.State state = CaptureStatus.State.STARTING;

        FlakyRecorder(CaptureStartRequest request, int deadChecks) {
            super(request);
            this.request = request;
            this.deadChecks = deadChecks;
        }

        @Override
        void start() {
            state = CaptureStatus.State.ACTIVE;
        }

        @Override
        synchronized CaptureStatus status() {
            return new CaptureStatus(
                    state,
                    "flaky-session",
                    request.browser().name().toLowerCase(),
                    request.targetUrl(),
                    0,
                    List.of(),
                    request.outputPath().toString(),
                    false,
                    ProcessHandle.current().pid(),
                    Instant.parse("2026-01-02T03:04:05Z"));
        }

        @Override
        synchronized void checkpoint(String description, Checkpoint.CheckpointKind kind) {
            // Unused in health-check tests.
        }

        @Override
        synchronized CaptureStatus stop(boolean discard) {
            state = discard ? CaptureStatus.State.DISCARDED : CaptureStatus.State.COMPLETED;
            return status();
        }

        @Override
        synchronized CaptureStatus interrupt() {
            interruptCount.incrementAndGet();
            state = CaptureStatus.State.INCOMPLETE;
            return status();
        }

        @Override
        synchronized boolean isBrowserAlive() {
            if (state != CaptureStatus.State.ACTIVE) {
                return false;
            }
            return livenessChecks.incrementAndGet() > deadChecks;
        }
    }
}
