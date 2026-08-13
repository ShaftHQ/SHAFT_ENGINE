package com.shaft.ai.local;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/** Cancellable asynchronous managed-local lifecycle operation. */
public final class ManagedLocalAiOperation {
    private final CompletableFuture<ManagedLocalAiSnapshot> completion = new CompletableFuture<>();
    private final AtomicReference<Status> status = new AtomicReference<>(Status.RUNNING);
    private final AtomicReference<ManagedLocalAiSnapshot> snapshot;
    private final AtomicReference<Thread> worker = new AtomicReference<>();

    ManagedLocalAiOperation(ManagedLocalAiSnapshot initial) {
        snapshot = new AtomicReference<>(Objects.requireNonNull(initial, "initial"));
    }

    /** @return completion that resolves to the final immutable lifecycle snapshot */
    public CompletableFuture<ManagedLocalAiSnapshot> completion() {
        return completion;
    }

    /** @return the latest immutable operation snapshot */
    public ManagedLocalAiSnapshot snapshot() {
        return snapshot.get();
    }

    /** Requests cooperative cancellation. Repeated or completed cancellation returns {@code false}. */
    public boolean cancel() {
        boolean requested = status.compareAndSet(Status.RUNNING, Status.CANCELLED);
        if (requested) {
            Thread running = worker.get();
            if (running != null) {
                running.interrupt();
            }
        }
        return requested;
    }

    boolean isCancelled() {
        return status.get() == Status.CANCELLED;
    }

    void publish(ManagedLocalAiSnapshot value) {
        snapshot.set(value);
    }

    void attach(Thread running) {
        worker.set(Objects.requireNonNull(running, "running"));
        if (isCancelled()) {
            running.interrupt();
        }
    }

    boolean complete(ManagedLocalAiSnapshot value) {
        if (!status.compareAndSet(Status.RUNNING, Status.COMPLETED)) {
            return false;
        }
        snapshot.set(value);
        completion.complete(value);
        worker.set(null);
        return true;
    }

    void fail(Throwable failure) {
        if (status.compareAndSet(Status.RUNNING, Status.COMPLETED)) {
            completion.completeExceptionally(failure);
        } else if (status.get() == Status.CANCELLED) {
            completion.cancel(false);
        }
        worker.set(null);
    }

    void cancelled() {
        status.compareAndSet(Status.RUNNING, Status.CANCELLED);
        completion.cancel(false);
        worker.set(null);
    }

    private enum Status { RUNNING, CANCELLED, COMPLETED }
}
