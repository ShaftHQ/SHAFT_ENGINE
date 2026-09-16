package com.shaft.coverage.journey;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Records execution observations keyed by stable {@link CoverageTarget} identity.
 *
 * <p>Retry and duplicate-view mutations collapse onto the same identity and do not inflate
 * coverage (SC-2). Coverage is derived only from recorded evidence (FR-2).
 */
public final class ObservationRecorder {
    private final Map<CoverageTarget, ExecutionObservation> firstByTarget = new LinkedHashMap<>();
    private final Map<CoverageTarget, Integer> hitCounts = new LinkedHashMap<>();

    /**
     * Records one observation. Subsequent hits for the same stable target increase the hit count
     * but do not add a second covered entry.
     *
     * @param observation execution evidence
     */
    public synchronized void record(ExecutionObservation observation) {
        if (observation == null) {
            throw new IllegalArgumentException("observation must not be null");
        }
        CoverageTarget target = observation.target();
        firstByTarget.putIfAbsent(target, observation);
        hitCounts.merge(target, 1, Integer::sum);
    }

    /**
     * Convenience recorder.
     *
     * @param target coverage target
     * @param evidenceId evidence reference
     */
    public void record(CoverageTarget target, String evidenceId) {
        record(new ExecutionObservation(target, evidenceId, Instant.now()));
    }

    /**
     * @return distinct observed targets in insertion order
     */
    public synchronized Set<CoverageTarget> observedTargets() {
        return Collections.unmodifiableSet(new LinkedHashSet<>(firstByTarget.keySet()));
    }

    /**
     * @return first observation per distinct target
     */
    public synchronized List<ExecutionObservation> observations() {
        return List.copyOf(new ArrayList<>(firstByTarget.values()));
    }

    /**
     * @param target coverage target
     * @return number of times the target was recorded (including retries/duplicates)
     */
    public synchronized int hitCount(CoverageTarget target) {
        return hitCounts.getOrDefault(target, 0);
    }

    /**
     * @return number of distinct covered targets
     */
    public synchronized int distinctCount() {
        return firstByTarget.size();
    }

    /**
     * Clears all recorded evidence.
     */
    public synchronized void reset() {
        firstByTarget.clear();
        hitCounts.clear();
    }

    /**
     * @param observations observations to seed
     * @return recorder containing those observations
     */
    public static ObservationRecorder of(Collection<ExecutionObservation> observations) {
        ObservationRecorder recorder = new ObservationRecorder();
        if (observations != null) {
            for (ExecutionObservation observation : observations) {
                recorder.record(observation);
            }
        }
        return recorder;
    }
}
