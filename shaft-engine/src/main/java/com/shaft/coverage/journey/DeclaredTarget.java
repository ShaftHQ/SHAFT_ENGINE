package com.shaft.coverage.journey;

/**
 * One catalog/baseline entry describing a target that may be covered by execution evidence.
 *
 * @param target stable coverage target
 * @param observable {@code false} when the target is declared but cannot be observed (FR-4)
 */
public record DeclaredTarget(CoverageTarget target, boolean observable) {
    /**
     * Creates a declared target.
     */
    public DeclaredTarget {
        if (target == null) {
            throw new IllegalArgumentException("target must not be null");
        }
    }

    /**
     * @param target coverage target
     * @return observable declared target
     */
    public static DeclaredTarget observable(CoverageTarget target) {
        return new DeclaredTarget(target, true);
    }

    /**
     * @param target coverage target
     * @return unobservable declared target
     */
    public static DeclaredTarget unobservable(CoverageTarget target) {
        return new DeclaredTarget(target, false);
    }
}
