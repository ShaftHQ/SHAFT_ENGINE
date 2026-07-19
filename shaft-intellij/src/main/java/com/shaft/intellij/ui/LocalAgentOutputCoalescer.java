package com.shaft.intellij.ui;

import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

/**
 * Coalesces per-line producer-thread output into throttled batch flushes (issue #3751 part 2, HIGH
 * finding 2): {@code AssistantLocalAgentRunner}'s {@code readAsync} calls its output consumer once
 * per stdout/stderr line -- from the stdout AND stderr reader threads concurrently, since both are
 * wired to the same consumer -- and a verbose Claude/Codex {@code --json}/{@code stream-json} run can
 * emit many lines a second. Dispatching one EDT {@code invokeLater} per line (the old behavior at
 * {@code ShaftAssistantPanel}'s stream wiring) floods the EDT's event queue with runnables that each
 * redo Finding 1's whole-buffer re-render. This coalescer instead: the first {@link #enqueue} after
 * an idle flush schedules exactly one flush via the caller-supplied {@code scheduler} (production
 * wires a throttled ~100ms one-shot {@code javax.swing.Timer}); every enqueue in between is added to
 * a thread-safe queue with no extra scheduling. When the scheduled flush runs, it drains every
 * currently queued line -- in FIFO order -- and hands the whole batch to {@code batchConsumer} in one
 * pass, so N queued lines cost one re-render instead of N.
 *
 * <p>Deliberately decoupled from Swing/timing specifics so it can be tested deterministically: the
 * {@code scheduler} is just "run this flush later, once" -- a test supplies a synchronous scheduler
 * that just records the runnable, while production supplies a real throttled Timer wired back through
 * {@code ApplicationManager#invokeLater}.
 *
 * <p>Thread-safety: {@link #enqueue} may be called concurrently from any number of threads (matching
 * the stdout/stderr reader threads); {@link #flush()} is expected to run on the EDT in production
 * (since {@code batchConsumer} mutates Swing components) but is otherwise just plain synchronized-free
 * queue draining -- callers needing a synchronous final drain (a run completing or being
 * cancelled/killed) can call {@link #flush()} directly instead of waiting for the scheduled one.
 */
final class LocalAgentOutputCoalescer {
    private final Queue<String> pending = new ConcurrentLinkedQueue<>();
    private final AtomicBoolean flushScheduled = new AtomicBoolean(false);
    private final Consumer<List<String>> batchConsumer;
    private final Consumer<Runnable> scheduler;

    LocalAgentOutputCoalescer(Consumer<List<String>> batchConsumer, Consumer<Runnable> scheduler) {
        this.batchConsumer = batchConsumer;
        this.scheduler = scheduler;
    }

    /**
     * Enqueues one output line. Safe to call from any thread. Schedules exactly one flush per idle
     * period: if a flush is already scheduled (or currently running), this just adds to the queue and
     * returns without scheduling anything further -- the already-scheduled flush will pick this line
     * up too.
     */
    void enqueue(String line) {
        pending.add(line);
        if (flushScheduled.compareAndSet(false, true)) {
            scheduler.accept(this::flush);
        }
    }

    /**
     * Drains every line currently queued, in FIFO order, and -- if any were queued -- hands them to
     * {@code batchConsumer} as a single batch. Resets the scheduling flag first, so a line enqueued
     * concurrently with (or immediately after) this drain either lands in this batch or correctly
     * schedules a fresh flush of its own -- never both, never neither.
     *
     * <p>Safe to call directly (bypassing the scheduler) to force a synchronous final drain, e.g. when
     * a local-agent run completes or is cancelled/killed before its throttled flush has fired --
     * otherwise the last queued lines would be delayed past (or lost relative to) the terminal render.
     */
    void flush() {
        flushScheduled.set(false);
        List<String> batch = new ArrayList<>();
        String line;
        while ((line = pending.poll()) != null) {
            batch.add(line);
        }
        if (!batch.isEmpty()) {
            batchConsumer.accept(batch);
        }
    }
}
