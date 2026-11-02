package com.shaft.coverage.journey;

/**
 * Interactive UI state classification used in coverage identities.
 *
 * <p>{@link #UNKNOWN} and {@link #UNOBSERVABLE} are reporting buckets (FR-4), not ordinary
 * covered/uncovered states.
 */
public enum StateKind {
    /** Content is still loading. */
    LOADING,
    /** View rendered with no primary content. */
    EMPTY,
    /** Error or failure state was shown. */
    ERROR,
    /** Successful / happy-path interactive state. */
    SUCCESS,
    /** Evidence existed but could not be classified into a known state. */
    UNKNOWN,
    /** Declared in the catalog but not observable in this environment (hidden, flagged off, etc.). */
    UNOBSERVABLE
}
