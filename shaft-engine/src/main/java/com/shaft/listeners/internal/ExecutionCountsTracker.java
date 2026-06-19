package com.shaft.listeners.internal;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Tracks deduplicated execution counts by stable test id across runner adapters.
 */
public class ExecutionCountsTracker {
    private final Set<String> passedIds = ConcurrentHashMap.newKeySet();
    private final Set<String> failedIds = ConcurrentHashMap.newKeySet();
    private final Set<String> skippedIds = ConcurrentHashMap.newKeySet();

    /**
     * Records a passing test id.
     *
     * @param info current test execution metadata
     */
    public void recordPassed(TestExecutionInfo info) {
        passedIds.add(stableId(info));
    }

    /**
     * Records a failing test id.
     *
     * @param info current test execution metadata
     */
    public void recordFailed(TestExecutionInfo info) {
        failedIds.add(stableId(info));
    }

    /**
     * Records a skipped test id.
     *
     * @param info current test execution metadata
     */
    public void recordSkipped(TestExecutionInfo info) {
        skippedIds.add(stableId(info));
    }

    /**
     * Clears all tracked ids.
     */
    public void clear() {
        passedIds.clear();
        failedIds.clear();
        skippedIds.clear();
    }

    /**
     * Creates a deduplicated count snapshot.
     *
     * @return immutable count snapshot
     */
    public Counts snapshot() {
        Set<String> flakySet = new HashSet<>(failedIds);
        flakySet.retainAll(passedIds);

        Set<String> finalFailedSet = new HashSet<>(failedIds);
        finalFailedSet.removeAll(passedIds);

        Set<String> finalPassedSet = new HashSet<>(passedIds);
        finalPassedSet.removeAll(flakySet);

        Set<String> resolvedSet = new HashSet<>(passedIds);
        resolvedSet.addAll(finalFailedSet);

        Set<String> finalSkippedSet = new HashSet<>(skippedIds);
        finalSkippedSet.removeAll(resolvedSet);

        return new Counts(finalPassedSet.size(), finalFailedSet.size(), finalSkippedSet.size(), flakySet.size());
    }

    private static String stableId(TestExecutionInfo info) {
        if (info == null || info.stableId() == null || info.stableId().isBlank()) {
            return "unknown";
        }
        return info.stableId();
    }

    /**
     * Deduplicated execution count snapshot.
     *
     * @param passed passed tests excluding flaky tests
     * @param failed failed tests that never passed
     * @param skipped skipped tests that did not later pass or fail
     * @param flaky tests that failed and later passed
     */
    public record Counts(int passed, int failed, int skipped, int flaky) {
        /**
         * Counts final passed tests including flaky tests that eventually passed.
         *
         * @return final pass count
         */
        public int finalPassed() {
            return passed + flaky;
        }
    }
}
