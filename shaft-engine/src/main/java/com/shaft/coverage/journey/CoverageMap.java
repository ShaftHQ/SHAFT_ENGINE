package com.shaft.coverage.journey;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

/**
 * Coverage buckets derived from declared catalog entries and observed evidence (FR-2, FR-4).
 *
 * @param covered declared observable targets that were observed
 * @param uncovered declared observable targets that were not observed
 * @param unknown observed targets not present in the catalog, or classified as {@link StateKind#UNKNOWN}
 * @param unobservable declared targets marked not observable
 */
public record CoverageMap(
        Set<CoverageTarget> covered,
        Set<CoverageTarget> uncovered,
        Set<CoverageTarget> unknown,
        Set<CoverageTarget> unobservable) {

    /**
     * Creates an immutable coverage map.
     */
    public CoverageMap {
        covered = copy(covered);
        uncovered = copy(uncovered);
        unknown = copy(unknown);
        unobservable = copy(unobservable);
    }

    private static Set<CoverageTarget> copy(Set<CoverageTarget> value) {
        return Collections.unmodifiableSet(new LinkedHashSet<>(Objects.requireNonNullElseGet(value, Set::of)));
    }
}
