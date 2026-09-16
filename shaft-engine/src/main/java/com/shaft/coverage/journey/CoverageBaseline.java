package com.shaft.coverage.journey;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Declared coverage baseline used for pull-request comparison (FR-3).
 *
 * @param name baseline name (for example {@code main} or a release tag)
 * @param targets declared targets
 */
public record CoverageBaseline(String name, List<DeclaredTarget> targets) {
    /**
     * Creates an immutable baseline.
     */
    public CoverageBaseline {
        if (name == null || name.isBlank()) {
            throw new IllegalArgumentException("name must not be blank");
        }
        Map<CoverageTarget, DeclaredTarget> unique = new LinkedHashMap<>();
        if (targets != null) {
            for (DeclaredTarget target : targets) {
                unique.putIfAbsent(target.target(), target);
            }
        }
        targets = List.copyOf(unique.values());
    }

    /**
     * @param name baseline name
     * @param targets declared targets
     * @return baseline
     */
    public static CoverageBaseline of(String name, Collection<DeclaredTarget> targets) {
        return new CoverageBaseline(name, new ArrayList<>(Objects.requireNonNullElseGet(targets, List::of)));
    }

}
