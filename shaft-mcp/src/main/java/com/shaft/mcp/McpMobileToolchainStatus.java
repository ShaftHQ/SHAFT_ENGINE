package com.shaft.mcp;

import java.nio.file.Path;
import java.util.List;

/**
 * Local mobile toolchain discovery status.
 */
public record McpMobileToolchainStatus(
        String platformName,
        boolean nodeAvailable,
        boolean npmAvailable,
        boolean appiumAvailable,
        boolean appiumInspectorAvailable,
        boolean adbAvailable,
        boolean emulatorAvailable,
        boolean sdkManagerAvailable,
        boolean avdManagerAvailable,
        Path toolRoot,
        Path androidSdkRoot,
        Path androidAvdHome,
        Path appiumRoot,
        String appiumVersion,
        String appiumInspectorPluginVersion,
        List<McpMobileDevice> androidDevices,
        List<String> cachedAndroidEmulators,
        List<String> missingDependencies,
        List<String> warnings) {
    /**
     * Creates an immutable toolchain status.
     */
    public McpMobileToolchainStatus {
        platformName = text(platformName);
        appiumVersion = text(appiumVersion);
        appiumInspectorPluginVersion = text(appiumInspectorPluginVersion);
        androidDevices = androidDevices == null ? List.of() : List.copyOf(androidDevices);
        cachedAndroidEmulators = cachedAndroidEmulators == null ? List.of() : List.copyOf(cachedAndroidEmulators);
        missingDependencies = missingDependencies == null ? List.of() : List.copyOf(missingDependencies);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
