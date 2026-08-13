package com.shaft.infrastructure;

import java.util.Locale;

/** Supported host platform selected explicitly by planning and path resolution. */
public enum SetupPlatform {
    WINDOWS,
    MACOS,
    LINUX;

    public static SetupPlatform current() {
        return fromOsName(System.getProperty("os.name"));
    }

    public static SetupPlatform fromOsName(String osName) {
        if (osName == null || osName.isBlank()) {
            throw new IllegalArgumentException("Operating-system name must not be blank.");
        }
        String normalized = osName.toLowerCase(Locale.ROOT);
        if (normalized.contains("mac") || normalized.contains("darwin")) return MACOS;
        if (normalized.contains("win")) return WINDOWS;
        if (normalized.contains("linux")) return LINUX;
        throw new IllegalArgumentException("Unsupported operating system: " + osName);
    }
}
