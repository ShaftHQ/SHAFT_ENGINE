package com.shaft.capture.runtime;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureManagerLifecycleTest {
    @TempDir
    Path temp;

    @Test
    void managerRunsLifecycleThroughInjectedRecorder() {
        AtomicReference<FakeRecorder> recorderRef = new AtomicReference<>();
        CaptureManager manager = new CaptureManager(request -> {
            FakeRecorder recorder = new FakeRecorder(request);
            recorderRef.set(recorder);
            return recorder;
        });
        CaptureStartRequest request = request("capture.json");

        assertEquals(CaptureStatus.State.ACTIVE, manager.start(request).state());
        assertThrows(IllegalStateException.class, () -> manager.start(request));
        assertEquals(CaptureStatus.State.ACTIVE, manager.status().state());
        assertEquals(CaptureStatus.State.ACTIVE,
                manager.checkpoint("review", Checkpoint.CheckpointKind.USER_MARKER).state());
        assertEquals(1, recorderRef.get().checkpointCount);
        CaptureStatus stopped = manager.stop(false);
        assertEquals(CaptureStatus.State.COMPLETED, stopped.state());
        assertEquals(request.outputPath().toString(), stopped.outputPath());
        assertFalse(recorderRef.get().isBrowserAlive());
        assertEquals(CaptureStatus.State.COMPLETED, manager.stop(false).state());
        manager.close();
    }

    @Test
    void managerStopWarnsInsteadOfSilentlySucceedingWhenNoRecorderIsActiveInThisProcess() {
        CaptureManager neverStarted = new CaptureManager(FakeRecorder::new);

        CaptureStatus status = neverStarted.stop(false);

        assertEquals(CaptureStatus.State.NOT_RUNNING, status.state());
        assertTrue(status.warnings().stream().anyMatch(warning -> warning.contains("No active recording")),
                status.warnings().toString());
        neverStarted.close();
    }

    @Test
    void managerRejectsInactiveCheckpointAndInterruptsActiveRecorderOnClose() {
        AtomicReference<FakeRecorder> recorderRef = new AtomicReference<>();
        CaptureManager inactive = new CaptureManager(FakeRecorder::new);

        assertThrows(IllegalStateException.class,
                () -> inactive.checkpoint("review", Checkpoint.CheckpointKind.USER_MARKER));
        inactive.close();

        CaptureManager active = new CaptureManager(request -> {
            FakeRecorder recorder = new FakeRecorder(request);
            recorderRef.set(recorder);
            return recorder;
        });
        active.start(request("active.json"));
        active.close();

        assertEquals(1, recorderRef.get().interruptCount);
    }

    @Test
    void managerAcceptsRecorderCompletedFromBrowserUiAndAllowsNextSession() {
        AtomicReference<FakeRecorder> recorderRef = new AtomicReference<>();
        CaptureManager manager = new CaptureManager(request -> {
            FakeRecorder recorder = new FakeRecorder(request);
            recorderRef.set(recorder);
            return recorder;
        });

        assertEquals(CaptureStatus.State.ACTIVE, manager.start(request("first.json")).state());
        recorderRef.get().completeFromBrowserUi();
        assertEquals(CaptureStatus.State.COMPLETED, manager.status().state());
        assertEquals(CaptureStatus.State.COMPLETED, manager.stop(false).state());

        assertEquals(CaptureStatus.State.ACTIVE, manager.start(request("second.json")).state());
        assertEquals(CaptureStatus.State.COMPLETED, manager.stop(false).state());
        manager.close();
    }

    @Test
    void managerRejectsNullFactoryAndReleasesLockWhenRecorderStartFails() {
        assertThrows(IllegalArgumentException.class, () -> new CaptureManager(null));

        CaptureManager manager = new CaptureManager(FailingRecorder::new);
        assertThrows(IllegalStateException.class, () -> manager.start(request("failed.json")));
        assertEquals(CaptureStatus.State.STARTING, manager.status().state());
        manager.close();
    }

    @Test
    void managerReplacesExistingValidCaptureSessionOutputBeforeStart() throws Exception {
        Path existingSession = temp.resolve("existing.json");
        new CaptureJsonCodec().write(existingSession, CaptureSession.start(
                "stale-session",
                CaptureFixtures.STARTED,
                CaptureFixtures.browser()));
        CaptureManager manager = new CaptureManager(FakeRecorder::new);

        CaptureStatus status = manager.start(newCaptureRequest(existingSession));

        assertEquals(CaptureStatus.State.ACTIVE, status.state());
        assertEquals(CaptureStatus.State.COMPLETED, manager.stop(false).state());
        assertEquals(CaptureStatus.State.COMPLETED, manager.status().state());
        assertFalse(Files.exists(existingSession));
        manager.close();
    }

    private CaptureStartRequest newCaptureRequest(Path outputPath) {
        return new CaptureStartRequest(
                "https://example.test",
                CaptureBrowser.CHROME,
                outputPath,
                temp.resolve("runtime"),
                true);
    }

    @Test
    void managerRejectsSecondConcurrentSessionRegardlessOfApiCaptureFlag() {
        AtomicReference<FakeRecorder> recorderRef = new AtomicReference<>();
        CaptureManager manager = new CaptureManager(request -> {
            FakeRecorder recorder = new FakeRecorder(request);
            recorderRef.set(recorder);
            return recorder;
        });

        assertEquals(CaptureStatus.State.ACTIVE, manager.start(request("first.json")).state());
        IllegalStateException failure = assertThrows(IllegalStateException.class,
                () -> manager.start(apiCaptureRequest("second.json")));

        assertTrue(failure.getMessage().contains("already active"));
        assertEquals(CaptureStatus.State.ACTIVE, manager.status().state());
        manager.stop(false);
        manager.close();
    }

    private CaptureStartRequest apiCaptureRequest(String outputName) {
        NetworkCaptureOptions networkOptions = new NetworkCaptureOptions();
        return new CaptureStartRequest(
                "https://example.test",
                CaptureBrowser.CHROME,
                temp.resolve(outputName),
                temp.resolve("runtime"),
                true,
                new CaptureStartOptions(
                        "", "", "", "", "", "", "", false, false,
                        "", "", "", "", "", "", "", "",
                        java.time.Duration.ZERO, "", null, "", true, networkOptions));
    }

    @Test
    void managerRejectsPreexistingUnknownOutputFile() throws IOException {
        Path notCaptureSession = temp.resolve("other.json");
        Files.writeString(notCaptureSession, "not a capture session");

        CaptureManager manager = new CaptureManager(FakeRecorder::new);
        assertThrows(IllegalStateException.class, () -> manager.start(request("other.json")));
        assertEquals(CaptureStatus.State.STARTING, manager.status().state());
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

    private static class FakeRecorder extends ManagedCaptureRecorder {
        private final CaptureStartRequest request;
        private CaptureStatus.State state = CaptureStatus.State.STARTING;
        private int checkpointCount;
        private int interruptCount;

        FakeRecorder(CaptureStartRequest request) {
            super(request);
            this.request = request;
        }

        @Override
        void start() {
            state = CaptureStatus.State.ACTIVE;
        }

        @Override
        CaptureStatus status() {
            return new CaptureStatus(
                    state,
                    "fake-session",
                    request.browser().name().toLowerCase(),
                    request.targetUrl(),
                    checkpointCount,
                    List.of(),
                    request.outputPath().toString(),
                    false,
                    ProcessHandle.current().pid(),
                    Instant.parse("2026-01-02T03:04:05Z"));
        }

        @Override
        void checkpoint(String description, Checkpoint.CheckpointKind kind) {
            checkpointCount++;
        }

        @Override
        CaptureStatus stop(boolean discard) {
            state = discard ? CaptureStatus.State.DISCARDED : CaptureStatus.State.COMPLETED;
            return status();
        }

        @Override
        CaptureStatus interrupt() {
            interruptCount++;
            state = CaptureStatus.State.INCOMPLETE;
            return status();
        }

        @Override
        boolean isBrowserAlive() {
            return state == CaptureStatus.State.ACTIVE;
        }

        void completeFromBrowserUi() {
            state = CaptureStatus.State.COMPLETED;
        }
    }

    private static final class FailingRecorder extends FakeRecorder {
        FailingRecorder(CaptureStartRequest request) {
            super(request);
        }

        @Override
        void start() {
            throw new IllegalStateException("boom");
        }
    }
}
