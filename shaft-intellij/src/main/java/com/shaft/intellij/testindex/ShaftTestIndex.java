package com.shaft.intellij.testindex;

import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.Nullable;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Project-level index of recent SHAFT test run results, backing the "SHAFT Tests" tool-window
 * tab ({@code ShaftTestsPanel}).
 * <p>
 * <b>Granularity (v1):</b> one row per run-configuration name, not per {@code @Test} method.
 * Populated from {@link ShaftTestIndexListener#processTerminated}, which only sees the run
 * profile's name and exit code -- parsing the per-test SM Test Runner protocol tree
 * ({@code SMTestProxy}) to get per-method rows would pull in platform test-framework UI
 * dependencies this lightweight plugin module intentionally avoids. A future iteration could add
 * per-method rows without changing this class's public shape.
 */
public final class ShaftTestIndex {
    /** Pass-fail status of the most recent run recorded for a test id. */
    public enum Status { PASS, FAIL }

    /**
     * One recorded run-configuration result.
     *
     * @param testId run-configuration display name (v1 granularity; see class javadoc)
     * @param status {@link Status#PASS} when {@code lastExitCode == 0}, otherwise {@link Status#FAIL}
     * @param lastRunAtMillis epoch millis the run terminated
     * @param lastExitCode raw process exit code
     */
    public record TestRowState(String testId, Status status, long lastRunAtMillis, int lastExitCode) {
    }

    private final Map<String, TestRowState> rowsByTestId = new ConcurrentHashMap<>();

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
        if (testId == null || testId.isBlank()) {
            return;
        }
        Status status = exitCode == 0 ? Status.PASS : Status.FAIL;
        rowsByTestId.put(testId, new TestRowState(testId, status, timestampMillis, exitCode));
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
    }
}
