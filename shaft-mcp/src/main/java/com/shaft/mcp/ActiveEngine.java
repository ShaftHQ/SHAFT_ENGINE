package com.shaft.mcp;

/**
 * The MCP session's currently active automation engine, tracked by {@link EngineService} so
 * engine-dispatching tools (for example {@code element_click}) know which underlying
 * implementation to route to. Every unified tool response echoes this value.
 */
public enum ActiveEngine {
    /** No session initialized yet. */
    NONE,
    /** A plain Selenium/CDP web browser session, initialized by {@code driver_initialize}. */
    WEB,
    /** An Appium native (Android/iOS) session, initialized by {@code mobile_initialize_native}. */
    MOBILE_NATIVE,
    /** A Chrome/Edge mobile web emulation session, initialized by {@code mobile_initialize_web_emulation}. */
    MOBILE_WEB,
    /** A SHAFT Playwright session, initialized by {@code playwright_initialize}. */
    PLAYWRIGHT
}
