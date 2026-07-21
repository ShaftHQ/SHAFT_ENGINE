package com.shaft.mcp;

/**
 * The MCP session's currently active automation engine, tracked by {@link EngineService} so
 * engine-dispatching tools (for example {@code element_click}) know which underlying
 * implementation to route to. Every unified tool response echoes this value.
 */
public enum ActiveEngine {
    /** No session initialized yet. */
    NONE,
    /** A plain Selenium/CDP web browser session, initialized by {@code driver_initialize} (default engine). */
    WEB,
    /**
     * An Appium native (Android/iOS) session, initialized by {@code driver_initialize} with
     * {@code engine=mobile_native} and an optional nested {@code mobileOptions} request.
     */
    MOBILE_NATIVE,
    /**
     * A Chrome/Edge mobile web emulation session, initialized by {@code driver_initialize} with
     * {@code engine=mobile_web} and an optional nested {@code mobileOptions} request.
     */
    MOBILE_WEB,
    /** A SHAFT Playwright session, initialized by {@code driver_initialize} with {@code engine=playwright}. */
    PLAYWRIGHT
}
