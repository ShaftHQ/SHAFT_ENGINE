package com.shaft.capture.runtime;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Objects;

/**
 * Options for network capture filtering and behavior in SHAFT Capture.
 * Used by MCP API capture tools to control what network transactions are recorded.
 *
 * <p>Kept as a plain mutable class with public fields (rather than a record) because Spring AI
 * binds {@code @Tool} method parameters of this type directly from tool-call JSON; {@link #equals}
 * and {@link #hashCode} are still overridden for structural equality since instances round-trip
 * through {@link CaptureStartOptions}, a record whose generated {@code equals} depends on it.
 *
 * <p><b>Do not switch the field annotations below to {@code @ToolParam(required = false)}.</b> That
 * is the usual Spring AI idiom (see {@code CaptureService#apiStart} parameters and {@code
 * McpMobileInitOptions}), but this class lives in {@code shaft-capture} -- a low-level runtime
 * module that {@code shaft-mcp} depends on, not the reverse -- and {@code @ToolParam} lives in
 * {@code spring-ai-model}, which {@code shaft-capture} deliberately does not depend on. Pulling that
 * in here would invert the module boundary just to annotate a DTO. Instead this relies on Spring
 * AI's JSON schema generator (victools' {@code JacksonModule} with {@code
 * RESPECT_JSONPROPERTY_REQUIRED}) honouring the framework-neutral {@code
 * com.fasterxml.jackson.annotation.JsonProperty(required = ...)} as an equivalent signal --
 * confirmed by decompiling {@code spring-ai-model}'s {@code JsonSchemaGenerator}. {@code
 * ShaftMcpApplicationTests#captureApiStartNetworkOptionsSchemaHasNoRequiredFieldsSoPartialNetworkOptionsSucceed}
 * guards this behavior against regression (issue #3986).
 */
public class NetworkCaptureOptions {
    /**
     * Whether to capture network requests.
     */
    @JsonProperty(required = false)
    public boolean enabled = true;

    /**
     * Whether to exclude asset resource types (images, fonts, stylesheets).
     */
    @JsonProperty(required = false)
    public boolean excludeAssets = false;

    /**
     * Glob pattern for URL paths to exclude from capture.
     */
    @JsonProperty(required = false)
    public String excludePattern = "";

    /**
     * Glob pattern for URL paths to include in capture.
     * Empty string means include all (subject to other filters).
     */
    @JsonProperty(required = false)
    public String includePattern = "";

    /**
     * Whether to record HTTP response bodies.
     */
    @JsonProperty(required = false)
    public boolean captureResponseBodies = false;

    /**
     * Whether to record HTTP request bodies.
     */
    @JsonProperty(required = false)
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
