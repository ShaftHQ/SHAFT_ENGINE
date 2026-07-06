package com.shaft.capture.runtime;

/**
 * Options for network capture filtering and behavior in SHAFT Capture.
 * Used by MCP API capture tools to control what network transactions are recorded.
 */
public class NetworkCaptureOptions {
    /**
     * Whether to capture network requests.
     */
    public boolean enabled = true;

    /**
     * Whether to exclude asset resource types (images, fonts, stylesheets).
     */
    public boolean excludeAssets = false;

    /**
     * Glob pattern for URL paths to exclude from capture.
     */
    public String excludePattern = "";

    /**
     * Glob pattern for URL paths to include in capture.
     * Empty string means include all (subject to other filters).
     */
    public String includePattern = "";

    /**
     * Whether to record HTTP response bodies.
     */
    public boolean captureResponseBodies = false;

    /**
     * Whether to record HTTP request bodies.
     */
    public boolean captureRequestBodies = false;

    /**
     * Creates default network capture options.
     */
    public NetworkCaptureOptions() {
    }
}
