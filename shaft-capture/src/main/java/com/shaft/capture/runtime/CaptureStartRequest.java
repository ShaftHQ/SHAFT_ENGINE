package com.shaft.capture.runtime;

import java.net.URI;
import java.nio.file.Path;

/**
 * Validated request for a managed browser capture session.
 *
 * @param targetUrl initial browser URL
 * @param browser supported browser family
 * @param outputPath capture JSON output
 * @param runtimeDirectory local control and temporary-profile directory
 * @param headless whether to launch without a visible browser window
 * @param options codegen-compatible capture options
 */
public record CaptureStartRequest(
        String targetUrl,
        CaptureBrowser browser,
        Path outputPath,
        Path runtimeDirectory,
        boolean headless,
        CaptureStartOptions options) {
    /**
     * Creates a request with default codegen options.
     *
     * @param targetUrl initial browser URL
     * @param browser supported browser family
     * @param outputPath capture JSON output
     * @param runtimeDirectory local control and temporary-profile directory
     * @param headless whether to launch without a visible browser window
     */
    public CaptureStartRequest(
            String targetUrl,
            CaptureBrowser browser,
            Path outputPath,
            Path runtimeDirectory,
            boolean headless) {
        this(targetUrl, browser, outputPath, runtimeDirectory, headless, CaptureStartOptions.defaults());
    }

    /**
     * Creates a validated request.
     */
    public CaptureStartRequest {
        targetUrl = validateUrl(targetUrl);
        browser = browser == null ? CaptureBrowser.CHROME : browser;
        if (outputPath == null || runtimeDirectory == null) {
            throw new IllegalArgumentException("Capture output and runtime directory are required.");
        }
        outputPath = outputPath.toAbsolutePath().normalize();
        runtimeDirectory = runtimeDirectory.toAbsolutePath().normalize();
        options = options == null ? CaptureStartOptions.defaults() : options;
        if (outputPath.startsWith(runtimeDirectory.resolve("profiles"))) {
            throw new IllegalArgumentException("Capture output cannot be stored in the temporary profile directory.");
        }
    }

    private static String validateUrl(String value) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException("Capture target URL is required.");
        }
        URI uri;
        try {
            uri = URI.create(value.trim());
        } catch (IllegalArgumentException exception) {
            throw new IllegalArgumentException("Capture target URL is invalid.", exception);
        }
        String scheme = uri.getScheme();
        if (scheme == null || (!scheme.equalsIgnoreCase("http")
                && !scheme.equalsIgnoreCase("https")
                && !scheme.equalsIgnoreCase("file"))) {
            throw new IllegalArgumentException("Capture target URL must use http, https, or file.");
        }
        return uri.toString();
    }
}
