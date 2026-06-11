package com.shaft.capture.runtime;

import com.shaft.driver.DriverFactory;

import java.util.Locale;

/**
 * Browser families supported by managed SHAFT Capture recording.
 */
public enum CaptureBrowser {
    /**
     * Google Chrome.
     */
    CHROME(DriverFactory.DriverType.CHROME),
    /**
     * Microsoft Edge.
     */
    EDGE(DriverFactory.DriverType.EDGE);

    private final DriverFactory.DriverType driverType;

    CaptureBrowser(DriverFactory.DriverType driverType) {
        this.driverType = driverType;
    }

    DriverFactory.DriverType driverType() {
        return driverType;
    }

    /**
     * Parses a supported browser name.
     *
     * @param value browser name
     * @return supported browser
     */
    public static CaptureBrowser parse(String value) {
        String normalized = value == null ? "" : value.trim().toUpperCase(Locale.ROOT);
        if ("FIREFOX".equals(normalized)) {
            throw new IllegalArgumentException(
                    "Firefox is not supported by SHAFT Capture v1. Use Chrome or Edge.");
        }
        try {
            return valueOf(normalized);
        } catch (IllegalArgumentException exception) {
            throw new IllegalArgumentException(
                    "Unsupported capture browser. Use Chrome or Edge.", exception);
        }
    }
}
