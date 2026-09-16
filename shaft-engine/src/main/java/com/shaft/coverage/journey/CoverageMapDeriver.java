package com.shaft.coverage.journey;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Derives a {@link CoverageMap} only from declared catalog entries plus observed execution
 * evidence (FR-2). Unknown and unobservable targets are reported separately from uncovered
 * (FR-4).
 */
public final class CoverageMapDeriver {
    private CoverageMapDeriver() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * @param declared catalog / baseline targets
     * @param observedTargets distinct observed targets
     * @return coverage map
     */
    public static CoverageMap derive(Collection<DeclaredTarget> declared, Collection<CoverageTarget> observedTargets) {
        Objects.requireNonNull(declared, "declared");
        Objects.requireNonNull(observedTargets, "observedTargets");

        Map<CoverageTarget, DeclaredTarget> catalog = new LinkedHashMap<>();
        for (DeclaredTarget entry : declared) {
            catalog.putIfAbsent(entry.target(), entry);
        }

        Set<CoverageTarget> observed = new LinkedHashSet<>(observedTargets);
        Set<CoverageTarget> covered = new LinkedHashSet<>();
        Set<CoverageTarget> uncovered = new LinkedHashSet<>();
        Set<CoverageTarget> unknown = new LinkedHashSet<>();
        Set<CoverageTarget> unobservable = new LinkedHashSet<>();

        for (DeclaredTarget entry : catalog.values()) {
            CoverageTarget target = entry.target();
            if (!entry.observable() || target.state().kind() == StateKind.UNOBSERVABLE) {
                unobservable.add(target);
                continue;
            }
            if (observed.contains(target)) {
                covered.add(target);
            } else {
                uncovered.add(target);
            }
        }

        for (CoverageTarget target : observed) {
            if (!catalog.containsKey(target) || target.state().kind() == StateKind.UNKNOWN) {
                unknown.add(target);
            }
        }

        return new CoverageMap(covered, uncovered, unknown, unobservable);
    }

    /**
     * @param declared catalog entries
     * @param recorder observation recorder
     * @return coverage map
     */
    public static CoverageMap derive(Collection<DeclaredTarget> declared, ObservationRecorder recorder) {
        return derive(declared, recorder.observedTargets());
    }
}
