package com.shaft.tools.io.internal;

/**
 * Represents the category of a recorded checkpoint.
 */
public enum CheckpointType {
    /** A hard assertion that fails the test immediately on failure. */
    ASSERTION,
    /** A soft verification that accumulates failures until the end of the test. */
    VERIFICATION
}
