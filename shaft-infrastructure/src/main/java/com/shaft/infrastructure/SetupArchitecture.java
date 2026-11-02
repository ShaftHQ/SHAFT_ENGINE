package com.shaft.infrastructure;

import java.util.Locale;

/** Supported host architecture bound into setup plans and artifact selection. */
public enum SetupArchitecture {
    X64("x64"),
    ARM64("arm64");

    private final String artifactName;

    SetupArchitecture(String artifactName) {
        this.artifactName = artifactName;
    }

    /**
     * Returns the architecture name used in artifact file names.
     */
    public String artifactName() {
        return artifactName;
    }

    /**
     * Returns the architecture of the running JVM.
     */
    public static SetupArchitecture current() {
        return fromOsArch(System.getProperty("os.arch"));
    }

    /**
     * Maps an {@code os.arch} value to a setup architecture.
     */
    public static SetupArchitecture fromOsArch(String osArch) {
        if (osArch == null || osArch.isBlank()) {
            throw new IllegalArgumentException("Operating-system architecture must not be blank.");
        }
        return switch (osArch.toLowerCase(Locale.ROOT)) {
            case "amd64", "x86_64", "x64" -> X64;
            case "aarch64", "arm64" -> ARM64;
            default -> throw new IllegalArgumentException("Unsupported operating-system architecture: " + osArch);
        };
    }
}
