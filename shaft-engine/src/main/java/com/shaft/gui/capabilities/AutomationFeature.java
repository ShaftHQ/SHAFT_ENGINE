package com.shaft.gui.capabilities;

/**
 * Stable feature identifiers used to inspect an active GUI session before invoking an operation.
 */
public enum AutomationFeature {
    /** Browser navigation and page automation. */
    BROWSER_AUTOMATION,
    /** Native or hybrid mobile application automation. */
    MOBILE_AUTOMATION,
    /** Direct access to the backend's primary native driver object. */
    NATIVE_DRIVER_ACCESS,
    /** A negotiated W3C WebDriver BiDi channel. */
    BIDI,
    /** Observation of browser or hybrid-context network traffic. */
    NETWORK_OBSERVATION,
    /** Request routing, blocking, modification, or mocking. */
    NETWORK_INTERCEPTION,
    /** Browser console messages or equivalent runtime logs. */
    CONSOLE_LOGS,
    /** Browser windows, frames, tabs, or mobile native/web contexts. */
    BROWSING_CONTEXTS,
    /** Script evaluation in the current browser context. */
    SCRIPT_EXECUTION,
    /** Cookies, local state, or reusable browser storage state. */
    STORAGE,
    /** Browser permission inspection or mutation. */
    PERMISSIONS,
    /** Managed browser download operations. */
    DOWNLOADS,
    /** SHAFT or backend-native execution tracing. */
    TRACE,
    /** Touch, swipe, drag, and related gestures. */
    TOUCH_GESTURES,
    /** Install, activate, terminate, or query application lifecycle. */
    APP_LIFECYCLE,
    /** Device-level controls such as lock state. */
    DEVICE_CONTROL,
    /** Fingerprint, Touch ID, or equivalent biometric simulation. */
    BIOMETRICS,
    /** Backend-provided performance samples. */
    PERFORMANCE_DATA,
    /** Push and pull file operations. */
    FILE_TRANSFER,
    /** Screen recording controlled by the automation backend. */
    SCREEN_RECORDING,
    /** W3C virtual-authenticator and passkey operations. */
    WEBAUTHN,
    /** HTTP authentication registration or request adaptation. */
    AUTHENTICATION
}
