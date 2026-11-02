package com.shaft.pilot.ai;

/**
 * Normalized provider and policy outcomes.
 */
public enum AiResponseStatus {
    SUCCESS,
    DISABLED,
    CONSENT_REQUIRED,
    BUDGET_EXCEEDED,
    REQUEST_TOO_LARGE,
    PROVIDER_UNAVAILABLE,
    AUTHENTICATION_FAILED,
    RATE_LIMITED,
    TIMEOUT,
    INVALID_RESPONSE,
    CIRCUIT_OPEN,
    ERROR
}
