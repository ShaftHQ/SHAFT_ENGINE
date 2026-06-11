package com.shaft.capture.storage;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureSessionStoreTest {
    @Test
    void lifecycleKeepsIncompleteAndCompletedSessionsReadable(@TempDir Path temp) {
        Path path = temp.resolve("session.json");
        CaptureSessionStore store = new CaptureSessionStore(path);
        store.start(CaptureSession.start("lifecycle", CaptureFixtures.STARTED, CaptureFixtures.browser()));

        store.append(new CaptureEvent.NavigationEvent(
                CaptureFixtures.context(1), CaptureEvent.NavigationAction.OPEN, "https://example.test"));
        store.checkpoint(new Checkpoint("loaded", 1, CaptureFixtures.STARTED.plusSeconds(2),
                Checkpoint.CheckpointKind.PAGE_TRANSITION, "Page loaded"));
        CaptureSession interrupted = store.markIncomplete(CaptureFixtures.STARTED.plusSeconds(3));

        assertEquals(CaptureSession.SessionStatus.INCOMPLETE, interrupted.status());
        assertNotNull(interrupted.endedAt());
        assertEquals(interrupted, new CaptureJsonCodec().read(path));

        CaptureSession completed = store.stop(CaptureFixtures.STARTED.plusSeconds(4));
        assertEquals(CaptureSession.SessionStatus.COMPLETED, completed.status());
        assertThrows(IllegalStateException.class, () -> store.append(
                new CaptureEvent.NavigationEvent(
                        CaptureFixtures.context(2), CaptureEvent.NavigationAction.REFRESH, "")));
    }

    @Test
    void concurrentAppendsPublishOnlyCompleteJsonSnapshots(@TempDir Path temp) throws Exception {
        Path path = temp.resolve("concurrent.json");
        CaptureSessionStore store = new CaptureSessionStore(path);
        CaptureJsonCodec externalReader = new CaptureJsonCodec();
        store.start(CaptureSession.start("concurrent", CaptureFixtures.STARTED, CaptureFixtures.browser()));
        var executor = Executors.newFixedThreadPool(8);
        AtomicBoolean running = new AtomicBoolean(true);
        AtomicReference<Throwable> readerFailure = new AtomicReference<>();
        Future<?> reader = executor.submit(() -> {
            while (running.get()) {
                try {
                    externalReader.read(path);
                } catch (Throwable throwable) {
                    readerFailure.compareAndSet(null, throwable);
                    return;
                }
            }
        });
        List<Callable<CaptureSession>> tasks = new ArrayList<>();
        for (int sequence = 1; sequence <= 24; sequence++) {
            int eventSequence = sequence;
            tasks.add(() -> store.append(new CaptureEvent.NavigationEvent(
                    CaptureFixtures.context(eventSequence),
                    CaptureEvent.NavigationAction.REFRESH,
                    "")));
        }

        List<Future<CaptureSession>> futures = executor.invokeAll(tasks);
        for (Future<CaptureSession> future : futures) {
            future.get();
        }
        running.set(false);
        reader.get();
        executor.shutdown();

        assertEquals(24, store.read().events().size());
        assertEquals(1, store.read().events().getFirst().context().sequence());
        assertEquals(24, store.read().events().getLast().context().sequence());
        assertFalse(readerFailure.get() != null, () -> String.valueOf(readerFailure.get()));
    }

    @Test
    void failedAppendDoesNotReplaceLastValidSnapshot(@TempDir Path temp) {
        Path path = temp.resolve("duplicate.json");
        CaptureSessionStore store = new CaptureSessionStore(path);
        store.start(CaptureSession.start("duplicate", CaptureFixtures.STARTED, CaptureFixtures.browser()));
        store.append(new CaptureEvent.NavigationEvent(
                CaptureFixtures.context(1), CaptureEvent.NavigationAction.REFRESH, ""));

        assertThrows(IllegalArgumentException.class, () -> store.append(
                new CaptureEvent.NavigationEvent(
                        CaptureFixtures.context(1), CaptureEvent.NavigationAction.BACK, "")));

        CaptureSession persisted = store.read();
        assertEquals(1, persisted.events().size());
        assertTrue(persisted.events().getFirst() instanceof CaptureEvent.NavigationEvent);
    }
}
