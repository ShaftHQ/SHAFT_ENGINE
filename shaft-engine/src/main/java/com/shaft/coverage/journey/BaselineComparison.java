package com.shaft.coverage.journey;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

/**
 * Compares pull-request coverage against a declared baseline (FR-3, SC-3).
 *
 * @param baselineName baseline name
 * @param map coverage map for the PR run
 * @param reductions baseline-covered (or baseline-declared observable) targets missing in the PR
 * @param additions PR-covered targets not present in the baseline
 * @param hasReductions {@code true} when merge would reduce baseline coverage
 */
public record BaselineComparison(
        String baselineName,
        CoverageMap map,
        Set<CoverageTarget> reductions,
        Set<CoverageTarget> additions,
        boolean hasReductions) {

    /**
     * Creates an immutable comparison result.
     */
    public BaselineComparison {
        if (baselineName == null || baselineName.isBlank()) {
            throw new IllegalArgumentException("baselineName must not be blank");
        }
        map = Objects.requireNonNull(map, "map");
        reductions = copy(reductions);
        additions = copy(additions);
        hasReductions = !reductions.isEmpty();
    }

    private static Set<CoverageTarget> copy(Set<CoverageTarget> value) {
        return Collections.unmodifiableSet(new LinkedHashSet<>(Objects.requireNonNullElseGet(value, Set::of)));
    }

    /**
     * Compares observed PR evidence against a declared baseline so reductions are visible before
     * merge (SC-3).
     *
     * @param baseline declared baseline
     * @param recorder PR observations
     * @return comparison with reductions called out separately from unknown/unobservable
     */
    public static BaselineComparison compare(CoverageBaseline baseline, ObservationRecorder recorder) {
        Objects.requireNonNull(baseline, "baseline");
        Objects.requireNonNull(recorder, "recorder");
        CoverageMap map = CoverageMapDeriver.derive(baseline.targets(), recorder);

        Set<CoverageTarget> baselineObservable = new LinkedHashSet<>();
        for (DeclaredTarget declared : baseline.targets()) {
            if (declared.observable() && declared.target().state().kind() != StateKind.UNOBSERVABLE) {
                baselineObservable.add(declared.target());
            }
        }

        Set<CoverageTarget> reductions = new LinkedHashSet<>();
        for (CoverageTarget target : baselineObservable) {
            if (!recorder.observedTargets().contains(target)) {
                reductions.add(target);
            }
        }

        Set<CoverageTarget> additions = new LinkedHashSet<>();
        for (CoverageTarget target : recorder.observedTargets()) {
            if (!baselineObservable.contains(target) && target.state().kind() != StateKind.UNKNOWN
                    && target.state().kind() != StateKind.UNOBSERVABLE) {
                boolean declaredUnobservable = baseline.targets().stream()
                        .anyMatch(d -> d.target().equals(target) && !d.observable());
                if (!declaredUnobservable) {
                    additions.add(target);
                }
            }
        }

        return new BaselineComparison(baseline.name(), map, reductions, additions, !reductions.isEmpty());
    }
}
