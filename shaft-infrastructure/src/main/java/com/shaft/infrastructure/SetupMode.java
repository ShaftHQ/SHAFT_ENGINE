package com.shaft.infrastructure;

/** Ownership policy for infrastructure setup. External is the safe default. */
public enum SetupMode {
    EXTERNAL,
    MANAGED,
    HYBRID
}
