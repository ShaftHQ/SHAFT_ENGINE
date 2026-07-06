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
     * Atomically reserves and returns the next unique event sequence number for this session,
     * based on the highest sequence currently persisted. Intended for recorders (for example
     * {@code CaptureNetworkRecorder}, via {@code ManagedCaptureRecorder}) that build a
     * {@link CaptureEvent} off the main {@code CaptureEventPipeline} thread and must still avoid
     * colliding with its independently-assigned sequence numbers.
     *
     * @return the next sequence number, one greater than the highest currently persisted
     */
    public long nextSequence() {
        lock.lock();
        try {
            if (!Files.isRegularFile(path)) {
                throw new IllegalStateException("Capture session has not been started.");
            }
            return codec.read(path).events().stream()
                    .mapToLong(event -> event.context().sequence())
                    .max()
                    .orElse(0L) + 1;
        } finally {
            lock.unlock();
        }
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

    /**
     * Returns the current network transactions derived from the on-disk session.
     *
     * @return ordered safe network transaction summaries
     */
    public List<com.shaft.capture.runtime.NetworkTransaction> networkTransactions() {
        return read().events().stream()
                .flatMap(event -> java.util.stream.Stream.ofNullable(
                        networkTransaction(event)))
                .toList();
    }

    private static com.shaft.capture.runtime.NetworkTransaction networkTransaction(CaptureEvent event) {
        if (!(event instanceof CaptureEvent.NetworkEvent networkEvent)) {
            return null;
        }
        java.util.Map<String, Object> bodyRefMetadata = new java.util.LinkedHashMap<>();
        putBodyRefMetadata(bodyRefMetadata, "request", networkEvent.request() == null
                ? null : networkEvent.request().body());
        putBodyRefMetadata(bodyRefMetadata, "response", networkEvent.response() == null
                ? null : networkEvent.response().body());
        return new com.shaft.capture.runtime.NetworkTransaction(
                networkEvent.transactionId(),
                networkEvent.request() == null ? "" : networkEvent.request().method(),
                networkEvent.request() == null ? "" : networkEvent.request().url(),
                networkEvent.response() == null ? 0 : networkEvent.response().statusCode(),
                networkEvent.resourceKind() == null ? "" : networkEvent.resourceKind().name(),
                timingMillis(networkEvent.timing()),
                bodyRefMetadata,
                networkEvent.correlatedUiSequence() == null
                        ? List.of()
                        : List.of(Math.toIntExact(Math.min(Integer.MAX_VALUE, networkEvent.correlatedUiSequence()))));
    }

    private static void putBodyRefMetadata(
            java.util.Map<String, Object> target, String key, com.shaft.capture.model.network.BodyRef bodyRef) {
        if (bodyRef == null) {
            return;
        }
        target.put(key, java.util.Map.of(
                "ref", bodyRef.ref(),
                "sizeBytes", bodyRef.sizeBytes(),
                "truncated", bodyRef.truncated()));
    }

    private static long timingMillis(com.shaft.capture.model.network.NetworkTiming timing) {
        if (timing == null) {
            return 0L;
        }
        return java.util.stream.Stream.of(
                        timing.blocked(), timing.dns(), timing.connect(),
                        timing.send(), timing.ttfb(), timing.receive())
                .filter(java.util.Objects::nonNull)
                .mapToLong(java.time.Duration::toMillis)
                .sum();
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
