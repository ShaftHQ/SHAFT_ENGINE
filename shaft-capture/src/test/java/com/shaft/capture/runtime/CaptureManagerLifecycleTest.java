package com.shaft.capture.runtime;

import com.shaft.capture.model.Checkpoint;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

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
        assertEquals(CaptureStatus.State.COMPLETED, manager.stop(false).state());
        assertEquals(CaptureStatus.State.COMPLETED, manager.stop(false).state());
        manager.close();
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
