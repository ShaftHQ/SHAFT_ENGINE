package com.shaft.capture.runtime;

import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Playwright-codegen-shaped options accepted by SHAFT Capture.
 *
 * @param targetLanguage requested generation target
 * @param testIdAttribute preferred test id attribute
 * @param channel Chromium channel hint
 * @param deviceName device emulation hint
 * @param viewportSize viewport size as {@code width,height}
 * @param colorScheme preferred color scheme hint
 * @param geolocation geolocation as {@code latitude,longitude}
 * @param ignoreHttpsErrors whether HTTPS certificate errors are ignored
 * @param blockServiceWorkers whether service workers should be blocked
 * @param loadStoragePath storage-state input path
 * @param saveStoragePath storage-state output path
 * @param language locale/language hint
 * @param timezone timezone hint
 * @param proxyServer proxy server URL
 * @param proxyBypass comma-separated proxy bypass list
 * @param saveHarPath HAR output path
 * @param saveHarGlob HAR URL glob filter
 * @param timeout maximum browser timeout
 * @param userAgent user-agent override
 * @param userDataDirectory persistent browser profile directory
 */
public record CaptureStartOptions(
        String targetLanguage,
        String testIdAttribute,
        String channel,
        String deviceName,
        String viewportSize,
        String colorScheme,
        String geolocation,
        boolean ignoreHttpsErrors,
        boolean blockServiceWorkers,
        String loadStoragePath,
        String saveStoragePath,
        String language,
        String timezone,
        String proxyServer,
        String proxyBypass,
        String saveHarPath,
        String saveHarGlob,
        Duration timeout,
        String userAgent,
        Path userDataDirectory) {
    private static final List<String> DEFAULT_TEST_ID_ATTRIBUTES = List.of("data-testid", "data-test", "data-qa");

    /**
     * Creates normalized capture options.
     */
    public CaptureStartOptions {
        targetLanguage = text(targetLanguage);
        testIdAttribute = text(testIdAttribute);
        channel = text(channel);
        deviceName = text(deviceName);
        viewportSize = text(viewportSize);
        colorScheme = text(colorScheme);
        geolocation = text(geolocation);
        loadStoragePath = text(loadStoragePath);
        saveStoragePath = text(saveStoragePath);
        language = text(language);
        timezone = text(timezone);
        proxyServer = text(proxyServer);
        proxyBypass = text(proxyBypass);
        saveHarPath = text(saveHarPath);
        saveHarGlob = text(saveHarGlob);
        userAgent = text(userAgent);
        timeout = timeout == null ? Duration.ZERO : timeout;
        if (timeout.isNegative()) {
            throw new IllegalArgumentException("Capture timeout cannot be negative.");
        }
        userDataDirectory = userDataDirectory == null ? null : userDataDirectory.toAbsolutePath().normalize();
        viewport(viewportSize);
    }

    /**
     * Returns default options.
     *
     * @return default options
     */
    public static CaptureStartOptions defaults() {
        return new CaptureStartOptions("", "", "", "", "", "", "", false, false,
                "", "", "", "", "", "", "", "", Duration.ZERO, "", null);
    }

    /**
     * Returns preferred test id attributes in locator-priority order.
     *
     * @return test id attributes
     */
    public List<String> testIdAttributes() {
        LinkedHashSet<String> attributes = new LinkedHashSet<>();
        if (!testIdAttribute.isBlank()) {
            attributes.add(testIdAttribute);
        }
        attributes.addAll(DEFAULT_TEST_ID_ATTRIBUTES);
        return List.copyOf(attributes);
    }

    /**
     * Parses the requested viewport.
     *
     * @return parsed viewport or {@code null}
     */
    public Viewport viewport() {
        return viewport(viewportSize);
    }

    /**
     * Returns safe warnings for accepted-but-not-native Playwright codegen options.
     *
     * @return warning list
     */
    public List<String> warnings() {
        List<String> warnings = new ArrayList<>();
        if (!targetLanguage.isBlank() && !Set.of("java", "java-testng", "shaft-java").contains(normalize(targetLanguage))) {
            warnings.add("Codegen target " + targetLanguage + " is recorded as metadata; SHAFT generates Java TestNG.");
        }
        addIfSet(warnings, channel, "Chromium channel selection is mapped through SHAFT browser selection.");
        addIfSet(warnings, deviceName, "Device emulation is recorded as metadata; set viewport and user agent for replay.");
        addIfSet(warnings, colorScheme, "Color-scheme emulation is recorded as metadata.");
        addIfSet(warnings, geolocation, "Geolocation emulation is recorded as metadata.");
        addIfTrue(warnings, blockServiceWorkers, "Service-worker blocking is recorded as metadata.");
        addIfSet(warnings, loadStoragePath, "Storage-state loading is recorded as metadata; use user-data-dir for browser state reuse.");
        addIfSet(warnings, saveStoragePath, "Storage-state saving is recorded as metadata; use user-data-dir for browser state reuse.");
        addIfSet(warnings, timezone, "Timezone emulation is recorded as metadata.");
        addIfSet(warnings, saveHarPath, "HAR capture is recorded as metadata; SHAFT Capture does not write HAR files.");
        addIfSet(warnings, saveHarGlob, "HAR filtering is recorded as metadata; SHAFT Capture does not write HAR files.");
        return List.copyOf(warnings);
    }

    private static void addIfSet(List<String> warnings, String value, String warning) {
        if (value != null && !value.isBlank()) {
            warnings.add(warning);
        }
    }

    private static void addIfTrue(List<String> warnings, boolean value, String warning) {
        if (value) {
            warnings.add(warning);
        }
    }

    private static Viewport viewport(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        String[] parts = value.trim().split("\\s*,\\s*");
        if (parts.length != 2) {
            throw new IllegalArgumentException("Capture viewport size must be width,height.");
        }
        try {
            int width = Integer.parseInt(parts[0]);
            int height = Integer.parseInt(parts[1]);
            if (width <= 0 || height <= 0) {
                throw new NumberFormatException();
            }
            return new Viewport(width, height);
        } catch (NumberFormatException exception) {
            throw new IllegalArgumentException("Capture viewport size must contain positive integers.", exception);
        }
    }

    private static String normalize(String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }

    /**
     * Browser viewport dimensions.
     *
     * @param width viewport width
     * @param height viewport height
     */
    public record Viewport(int width, int height) {
    }
}
