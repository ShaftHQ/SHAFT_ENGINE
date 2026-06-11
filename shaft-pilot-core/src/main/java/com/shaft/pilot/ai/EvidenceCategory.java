package com.shaft.pilot.ai;

/**
 * Evidence categories used by approval and redaction policies.
 */
public enum EvidenceCategory {
    TEXT,
    DOM,
    SCREENSHOT,
    LOG,
    SOURCE,
    TEST_DATA,
    NETWORK,
    CONFIGURATION
}
