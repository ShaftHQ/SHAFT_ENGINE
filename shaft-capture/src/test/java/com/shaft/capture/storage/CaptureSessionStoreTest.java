package com.shaft.capture.storage;

import tools.jackson.databind.node.StringNode;
import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.CaptureStep;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.EventContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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

    @Test
    void stepsExposeServerAuthoritativeDescriptionsAndSurviveDeletion(@TempDir Path temp) {
        Path path = temp.resolve("steps.json");
        CaptureSessionStore store = new CaptureSessionStore(path);
        store.start(CaptureSession.start("steps", CaptureFixtures.STARTED, CaptureFixtures.browser()));

        store.append(new CaptureEvent.NavigationEvent(
                contextWithAction(1, "instance-a-1", "Open https://a.test/"),
                CaptureEvent.NavigationAction.OPEN, "https://a.test/"));
        store.append(new CaptureEvent.NavigationEvent(
                contextWithAction(2, "instance-a-2", "Click submit"),
                CaptureEvent.NavigationAction.REFRESH, ""));

        List<CaptureStep> steps = store.steps();
        assertEquals(2, steps.size());
        assertEquals("instance-a-1", steps.get(0).clientActionId());
        assertEquals("Open https://a.test/", steps.get(0).description());
        assertEquals("instance-a-2", steps.get(1).clientActionId());
        assertEquals("Click submit", steps.get(1).description());

        store.updateEvents(events -> events.stream()
                .filter(event -> !"instance-a-1".equals(
                        event.context().extensions().get("clientActionId").asText("")))
                .toList());

        List<CaptureStep> afterDelete = store.steps();
        assertEquals(1, afterDelete.size());
        assertEquals("instance-a-2", afterDelete.getFirst().clientActionId());
    }

    @Test
    void stepsPreferUserEditedDescriptionOverOriginal(@TempDir Path temp) {
        Path path = temp.resolve("steps-edited.json");
        CaptureSessionStore store = new CaptureSessionStore(path);
        store.start(CaptureSession.start("steps-edited", CaptureFixtures.STARTED, CaptureFixtures.browser()));
        store.append(new CaptureEvent.NavigationEvent(
                contextWithAction(1, "instance-a-1", "Open https://a.test/"),
                CaptureEvent.NavigationAction.OPEN, "https://a.test/"));

        store.updateEvents(events -> events.stream()
                .<CaptureEvent>map(event -> new CaptureEvent.NavigationEvent(
                        withUserDescription(event.context(), "Edited: go to homepage"),
                        ((CaptureEvent.NavigationEvent) event).action(),
                        ((CaptureEvent.NavigationEvent) event).targetUrl()))
                .toList());

        List<CaptureStep> steps = store.steps();
        assertEquals(1, steps.size());
        assertEquals("Edited: go to homepage", steps.getFirst().description());
    }

    @Test
    void vanishedSessionFileSelfHealsFromLastPersistedSnapshot(@TempDir Path temp) throws Exception {
        Path path = temp.resolve("vanished.json");
        CaptureSessionStore store = new CaptureSessionStore(path);
        store.start(CaptureSession.start("vanished", CaptureFixtures.STARTED, CaptureFixtures.browser()));
        store.append(new CaptureEvent.NavigationEvent(
                CaptureFixtures.context(1), CaptureEvent.NavigationAction.OPEN, "https://example.test"));

        // External interference (antivirus, IDE file watcher racing the Windows replace-move
        // fallback) removes the session file mid-recording (issue #3429).
        java.nio.file.Files.delete(path);

        CaptureSession healedRead = store.read();
        assertEquals(1, healedRead.events().size());
        assertTrue(java.nio.file.Files.isRegularFile(path), "read() should republish the snapshot");

        java.nio.file.Files.delete(path);
        CaptureSession healedAppend = store.append(new CaptureEvent.NavigationEvent(
                CaptureFixtures.context(2), CaptureEvent.NavigationAction.REFRESH, ""));
        assertEquals(2, healedAppend.events().size());
        assertEquals(3, store.nextSequence());
        assertEquals(2, new CaptureJsonCodec().read(path).events().size());
    }

    @Test
    void discardForgetsSnapshotSoSelfHealCannotResurrectIt(@TempDir Path temp) {
        Path path = temp.resolve("discarded.json");
        CaptureSessionStore store = new CaptureSessionStore(path);
        store.start(CaptureSession.start("discarded", CaptureFixtures.STARTED, CaptureFixtures.browser()));
        store.append(new CaptureEvent.NavigationEvent(
                CaptureFixtures.context(1), CaptureEvent.NavigationAction.OPEN, "https://example.test"));

        store.discard();

        assertFalse(java.nio.file.Files.exists(path));
        assertThrows(IllegalStateException.class, store::read);
        assertFalse(java.nio.file.Files.exists(path), "a failed read must not resurrect the discarded file");
    }

    private static EventContext contextWithAction(long sequence, String clientActionId, String description) {
        Map<String, tools.jackson.databind.JsonNode> extensions = new LinkedHashMap<>();
        extensions.put("clientActionId", StringNode.valueOf(clientActionId));
        extensions.put("stepDescription", StringNode.valueOf(description));
        return new EventContext(sequence, CaptureFixtures.STARTED.plusSeconds(sequence), CaptureFixtures.page(),
                EventContext.ReplayStatus.NOT_REPLAYED, List.of(), extensions);
    }

    private static EventContext withUserDescription(EventContext context, String description) {
        Map<String, tools.jackson.databind.JsonNode> extensions = new LinkedHashMap<>(context.extensions());
        extensions.put("userDescription", StringNode.valueOf(description));
        return new EventContext(context.sequence(), context.timestamp(), context.page(),
                context.replayStatus(), context.evidence(), extensions);
    }
}
