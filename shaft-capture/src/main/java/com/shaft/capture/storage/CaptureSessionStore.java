package com.shaft.capture.storage;

import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
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
     * Adds a checkpoint and atomically publishes the updated session.
     *
     * @param checkpoint checkpoint to add
     * @return updated session
     */
    public CaptureSession checkpoint(Checkpoint checkpoint) {
        return update(session -> session.checkpoint(checkpoint));
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
