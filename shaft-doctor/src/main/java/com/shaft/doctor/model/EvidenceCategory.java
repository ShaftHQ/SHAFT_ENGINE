package com.shaft.doctor.model;

/**
 * Evidence categories supported by the deterministic collector.
 */
public enum EvidenceCategory {
    ALLURE_RESULT,
    SHAFT_LOG,
    EXCEPTION_CHAIN,
    SCREENSHOT,
    PAGE_SNAPSHOT,
    ENVIRONMENT,
    DEPENDENCY_BUILD,
    CONFIGURATION,
    OTHER
}
