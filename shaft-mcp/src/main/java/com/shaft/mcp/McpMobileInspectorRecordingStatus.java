package com.shaft.mcp;

import java.nio.file.Path;
import java.util.List;

/**
 * Runtime status for a wrapped Appium Inspector recording session.
 */
public record McpMobileInspectorRecordingStatus(
        boolean active,
        boolean paused,
        String platformName,
        String deviceId,
        String androidAvdName,
        boolean managedEmulator,
        Path outputPath,
        String inspectorUrl,
        String appiumServerUrl,
        int actionCount,
        List<McpCodeBlock> codeBlocks,
        List<String> warnings) {
    /**
     * Creates an immutable inspector recording status.
     */
    public McpMobileInspectorRecordingStatus {
        platformName = text(platformName);
        deviceId = text(deviceId);
        androidAvdName = text(androidAvdName);
        inspectorUrl = text(inspectorUrl);
        appiumServerUrl = text(appiumServerUrl);
        codeBlocks = codeBlocks == null ? List.of() : List.copyOf(codeBlocks);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
