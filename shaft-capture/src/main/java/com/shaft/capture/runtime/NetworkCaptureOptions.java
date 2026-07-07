package com.shaft.capture.runtime;

import java.util.Objects;

/**
 * Options for network capture filtering and behavior in SHAFT Capture.
 * Used by MCP API capture tools to control what network transactions are recorded.
 *
 * <p>Kept as a plain mutable class with public fields (rather than a record) because Spring AI
 * binds {@code @Tool} method parameters of this type directly from tool-call JSON; {@link #equals}
 * and {@link #hashCode} are still overridden for structural equality since instances round-trip
 * through {@link CaptureStartOptions}, a record whose generated {@code equals} depends on it.
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

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof NetworkCaptureOptions that)) {
            return false;
        }
        return enabled == that.enabled
                && excludeAssets == that.excludeAssets
                && captureResponseBodies == that.captureResponseBodies
                && captureRequestBodies == that.captureRequestBodies
                && excludePattern.equals(that.excludePattern)
                && includePattern.equals(that.includePattern);
    }

    @Override
    public int hashCode() {
        return Objects.hash(enabled, excludeAssets, excludePattern, includePattern,
                captureResponseBodies, captureRequestBodies);
    }

    @Override
    public String toString() {
        return "NetworkCaptureOptions[enabled=" + enabled
                + ", excludeAssets=" + excludeAssets
                + ", excludePattern=" + excludePattern
                + ", includePattern=" + includePattern
                + ", captureResponseBodies=" + captureResponseBodies
                + ", captureRequestBodies=" + captureRequestBodies + "]";
    }
}
