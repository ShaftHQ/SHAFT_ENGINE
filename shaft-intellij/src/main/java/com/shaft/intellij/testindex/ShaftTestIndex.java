package com.shaft.intellij.testindex;

import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Project-level index of recent SHAFT test run results, backing the "SHAFT Tests" tool-window
 * tab ({@code ShaftTestsPanel}).
 * <p>
 * <b>Row identity:</b> rows are keyed by whatever id the caller supplies. {@link ShaftTestIndexListener#processTerminated}
 * keys by run-configuration display name (class granularity -- see {@link #recordRun}); the live
 * SM Test Runner bridge ({@code ShaftSmTestRunnerBridge}, issue #3688) additionally keys per-method
 * rows by {@code qualifiedClassName#methodName} via {@link #recordStatus}, so a method-scoped run
 * decorates only its own tree node instead of the whole class.
 * <p>
 * <b>Live progression (issue #3688):</b> {@link #recordStatus} lets a row move through
 * {@link Status#RUNNING} while a run is in flight, then to {@link Status#PASS}/{@link Status#FAIL}
 * as the SM protocol reports each test's outcome, instead of only updating once the whole process
 * terminates. {@link #recordRun} (exit-code driven) and {@link #recordStatus} (SM-protocol driven)
 * share the same row storage keyed by test id, so whichever reports last for a given id wins --
 * the process-terminated exit code is the authoritative fallback for runs the SM bridge could not
 * identify a location for.
 */
public final class ShaftTestIndex {
    /**
     * Pass-fail status of the most recent run recorded for a test id, plus the in-flight
     * {@link #RUNNING} state a live SM Test Runner event can report before the run finishes.
     */
    public enum Status { RUNNING, PASS, FAIL }

    /**
     * One recorded row: either a class-level, exit-code-driven result from {@link #recordRun}, or
     * a live, SM-protocol-driven status from {@link #recordStatus} (see class javadoc for the two
     * key schemes).
     *
     * @param testId run-configuration display name, or {@code qualifiedClassName#methodName} for a
     *               live per-method row (see class javadoc)
     * @param status current status; {@link Status#RUNNING} only ever comes from {@link #recordStatus}
     * @param lastRunAtMillis epoch millis the row was last recorded
     * @param lastExitCode raw process exit code for a {@link #recordRun} row, or a synthesized
     *                     {@code 0}/{@code 1} for a {@link #recordStatus} row (see that method's javadoc)
     */
    public record TestRowState(String testId, Status status, long lastRunAtMillis, int lastExitCode) {
    }

    private final Map<String, TestRowState> rowsByTestId = new ConcurrentHashMap<>();
    private final List<Runnable> listeners = new ArrayList<>();

    /**
     * Returns the project-level test index.
     *
     * @param project the project to scope the index to, or {@code null} for a fresh, unpersisted
     *                instance
     * @return test index service
     */
    public static ShaftTestIndex getInstance(@Nullable Project project) {
        if (project == null) {
            return new ShaftTestIndex();
        }
        ShaftTestIndex index = project.getService(ShaftTestIndex.class);
        return index == null ? new ShaftTestIndex() : index;
    }

    /**
     * Records (or overwrites) the most recent result for a test id.
     *
     * @param testId run-configuration display name; blank/{@code null} ids are ignored
     * @param exitCode raw process exit code ({@code 0} is treated as pass)
     * @param timestampMillis epoch millis the run terminated
     */
    public void recordRun(@Nullable String testId, int exitCode, long timestampMillis) {
        recordStatus(testId, exitCode == 0 ? Status.PASS : Status.FAIL, timestampMillis, exitCode);
    }

    /**
     * Records (or overwrites) the most recent status for a test id, driven by a live SM Test
     * Runner event rather than a terminated process's exit code (issue #3688). {@code lastExitCode}
     * is meaningless for a live event, so it is set to {@code 0} for {@link Status#PASS}/
     * {@link Status#RUNNING} and {@code 1} for {@link Status#FAIL}, matching the same "zero is
     * pass" convention {@link #recordRun} already exposes to callers of {@link TestRowState}.
     *
     * @param testId testId to record, matching whichever key scheme the caller uses (see class
     *               javadoc); blank/{@code null} ids are ignored
     * @param status status to record
     * @param timestampMillis epoch millis the event was observed
     */
    public void recordStatus(@Nullable String testId, Status status, long timestampMillis) {
        recordStatus(testId, status, timestampMillis, status == Status.FAIL ? 1 : 0);
    }

    private void recordStatus(@Nullable String testId, Status status, long timestampMillis, int exitCode) {
        if (testId == null || testId.isBlank()) {
            return;
        }
        rowsByTestId.put(testId, new TestRowState(testId, status, timestampMillis, exitCode));
        notifyListeners();
    }

    /**
     * Returns all recorded rows, most recently run first.
     *
     * @return an immutable, time-ordered snapshot
     */
    public List<TestRowState> snapshot() {
        return rowsByTestId.values().stream()
                .sorted(Comparator.comparingLong(TestRowState::lastRunAtMillis).reversed())
                .toList();
    }

    /**
     * Clears every recorded row.
     */
    public void clear() {
        rowsByTestId.clear();
        notifyListeners();
    }

    /**
     * Registers a listener to be notified after any row is recorded or the index is cleared, so
     * {@code ShaftTestsPanel} can refresh its tree live as SM Test Runner events arrive (issue
     * #3688) instead of only on a manual Refresh click.
     *
     * @param listener callback to invoke on every change; {@code null} is ignored
     */
    public synchronized void addListener(@Nullable Runnable listener) {
        if (listener != null) {
            listeners.add(listener);
        }
    }

    /**
     * Removes a previously registered listener.
     * <p>
     * <b>Caution:</b> this is a plain {@code List.remove(Object)}, matched by identity. The caller
     * must pass the exact same {@link Runnable} reference registered via {@link #addListener} --
     * mirrors {@code ShaftMcpConnectionState#removeStateChangeListener}'s documented pitfall
     * (issue #3621): capture the listener once in a field and reuse that same reference for both
     * add and remove.
     *
     * @param listener listener to remove
     */
    public synchronized void removeListener(@Nullable Runnable listener) {
        listeners.remove(listener);
    }

    private synchronized void notifyListeners() {
        for (Runnable listener : new ArrayList<>(listeners)) {
            listener.run();
        }
    }

    /**
     * Test-support accessor for the number of currently registered listeners. Not used by
     * production code; lets tests assert that add/removeListener registration stays balanced
     * across a component's lifecycle instead of leaking (mirrors {@code ShaftMcpConnectionState#listenerCount}).
     *
     * @return number of currently registered listeners
     */
    public synchronized int listenerCountForTest() {
        return listeners.size();
    }
}
