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
        List<String> warnings,
        List<McpMobileToolchainDiagnostic> diagnostics) {
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
        diagnostics = diagnostics == null ? List.of() : List.copyOf(diagnostics);
    }

    /**
     * Creates an immutable toolchain status without structured dependency diagnostics.
     *
     * @param platformName normalized mobile platform name
     * @param nodeAvailable whether Node.js was found
     * @param npmAvailable whether npm was found
     * @param appiumAvailable whether Appium was found
     * @param appiumInspectorAvailable whether the Appium Inspector plugin was found
     * @param adbAvailable whether adb was found
     * @param emulatorAvailable whether the Android emulator binary was found
     * @param sdkManagerAvailable whether sdkmanager was found
     * @param avdManagerAvailable whether avdmanager was found
     * @param toolRoot SHAFT-managed tool cache root
     * @param androidSdkRoot Android SDK root
     * @param androidAvdHome Android AVD home
     * @param appiumRoot SHAFT-managed Appium root
     * @param appiumVersion detected Appium version
     * @param appiumInspectorPluginVersion configured Appium Inspector plugin version
     * @param androidDevices detected Android devices
     * @param cachedAndroidEmulators cached Android AVD names
     * @param missingDependencies missing dependency labels
     * @param warnings readiness warnings
     */
    public McpMobileToolchainStatus(
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
        this(platformName, nodeAvailable, npmAvailable, appiumAvailable, appiumInspectorAvailable, adbAvailable,
                emulatorAvailable, sdkManagerAvailable, avdManagerAvailable, toolRoot, androidSdkRoot, androidAvdHome,
                appiumRoot, appiumVersion, appiumInspectorPluginVersion, androidDevices, cachedAndroidEmulators,
                missingDependencies, warnings, List.of());
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
