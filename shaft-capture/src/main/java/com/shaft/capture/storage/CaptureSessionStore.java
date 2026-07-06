package com.shaft.capture.storage;

import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.RedactionSummary;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Thread-safe start, append, checkpoint, stop, and interruption lifecycle for one capture file.
 */
public final class CaptureSessionStore {
    private final Path path;
    private final CaptureJsonCodec codec;
    private final ReentrantLock lock = new ReentrantLock();

    /**
     * Creates a store for one capture path.
     *
     * @param path session JSON path
     */
    public CaptureSessionStore(Path path) {
        this(path, new CaptureJsonCodec());
    }

    /**
     * Creates a store with an explicit codec.
     *
     * @param path session JSON path
     * @param codec capture codec
     */
    public CaptureSessionStore(Path path, CaptureJsonCodec codec) {
        if (path == null || codec == null) {
            throw new IllegalArgumentException("Capture path and codec are required.");
        }
        this.path = path.toAbsolutePath().normalize();
        this.codec = codec;
    }

    /**
     * Starts and persists a new incomplete session.
     *
     * @param session new incomplete session
     */
    public void start(CaptureSession session) {
        lock.lock();
        try {
            if (Files.exists(path)) {
                throw new IllegalStateException("Capture session already exists.");
            }
            if (session.status() != CaptureSession.SessionStatus.INCOMPLETE) {
                throw new IllegalArgumentException("New capture sessions must be incomplete.");
            }
            codec.write(path, session);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Appends an event and atomically publishes the updated session.
     *
     * @param event event to append
     * @return updated session
     */
    public CaptureSession append(CaptureEvent event) {
        return update(session -> session.append(event));
    }

    /**
     * Appends an event with privacy references and summary in one atomic snapshot.
     *
     * @param event event to append
     * @param references external references used by the event
     * @param summary safe privacy summary
     * @return updated session
     */
    public CaptureSession append(
            CaptureEvent event,
            List<ExternalTestDataReference> references,
            RedactionSummary summary) {
        return update(session -> session.withDataReferences(references, summary).append(event));
    }

    /**
     * Adds a checkpoint and atomically publishes the updated session.
     *
     * @param checkpoint checkpoint to add
     * @return updated session
     */
    public CaptureSession checkpoint(Checkpoint checkpoint) {
        return update(session -> session.checkpoint(checkpoint));
    }

    /**
     * Adds a checkpoint and its sanitized-text summary atomically.
     *
     * @param checkpoint checkpoint to add
     * @param summary safe privacy summary
     * @return updated session
     */
    public CaptureSession checkpoint(Checkpoint checkpoint, RedactionSummary summary) {
        return update(session -> session.withDataReferences(List.of(), summary).checkpoint(checkpoint));
    }

    /**
     * Atomically rewrites the event list.
     *
     * @param operation event list rewrite
     * @return updated session
     */
    public CaptureSession updateEvents(java.util.function.UnaryOperator<List<CaptureEvent>> operation) {
        if (operation == null) {
            throw new IllegalArgumentException("Capture event update operation is required.");
        }
        return update(session -> session.withEvents(operation.apply(session.events())));
    }

    /**
     * Completes and atomically publishes the session.
     *
     * @param stoppedAt stop time
     * @return completed session
     */
    public CaptureSession stop(Instant stoppedAt) {
        return update(session -> session.complete(stoppedAt));
    }

    /**
     * Explicitly marks an interrupted session while keeping it readable.
     *
     * @param interruptedAt interruption time
     * @return incomplete interrupted session
     */
    public CaptureSession markIncomplete(Instant interruptedAt) {
        return update(session -> session.interrupt(interruptedAt));
    }

    /**
     * Reads the latest complete file snapshot.
     *
     * @return current session
     */
    public CaptureSession read() {
        lock.lock();
        try {
            if (!Files.isRegularFile(path)) {
                throw new IllegalStateException("Capture session has not been started.");
            }
            return codec.read(path);
        } finally {
            lock.unlock();
        }
    }

    /**
     * Returns the current step list derived from the on-disk session, so the recorder UI can
     * rehydrate its step list from the server instead of page-scoped storage across navigations.
     *
     * @return ordered safe step summaries for every event carrying a client action ID
     */
    public List<com.shaft.capture.model.CaptureStep> steps() {
        return read().events().stream()
                .map(CaptureSessionStore::step)
                .filter(java.util.Objects::nonNull)
                .toList();
    }

    private static com.shaft.capture.model.CaptureStep step(CaptureEvent event) {
        var extensions = event.context().extensions();
        String clientActionId = text(extensions.get("clientActionId"));
        if (clientActionId.isBlank()) {
            return null;
        }
        String description = text(extensions.get("userDescription"));
        if (description.isBlank()) {
            description = text(extensions.get("stepDescription"));
        }
        return new com.shaft.capture.model.CaptureStep(
                clientActionId, event.context().sequence(), description);
    }

    private static String text(tools.jackson.databind.JsonNode node) {
        return node == null ? "" : node.asText("");
    }

    private CaptureSession update(java.util.function.UnaryOperator<CaptureSession> operation) {
        lock.lock();
        try {
            if (!Files.isRegularFile(path)) {
                throw new IllegalStateException("Capture session has not been started.");
            }
            CaptureSession updated = operation.apply(codec.read(path));
            codec.write(path, updated);
            return updated;
        } finally {
            lock.unlock();
        }
    }
}
