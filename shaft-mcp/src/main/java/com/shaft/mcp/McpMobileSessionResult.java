package com.shaft.mcp;

import java.util.List;
import java.util.Map;

/**
 * Mobile session setup result returned by MCP mobile tools.
 */
public record McpMobileSessionResult(
        String mode,
        String platformName,
        String deviceName,
        String browserName,
        boolean active,
        List<McpCodeBlock> codeBlocks,
        List<String> warnings,
        Map<String, String> deviceProfile) {
    /**
     * Creates an immutable mobile session result.
     */
    public McpMobileSessionResult {
        mode = mode == null ? "" : mode.trim();
        platformName = platformName == null ? "" : platformName.trim();
        deviceName = deviceName == null ? "" : deviceName.trim();
        browserName = browserName == null ? "" : browserName.trim();
        codeBlocks = codeBlocks == null ? List.of() : List.copyOf(codeBlocks);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
        deviceProfile = deviceProfile == null ? Map.of() : Map.copyOf(deviceProfile);
    }

    /**
     * Creates an immutable mobile session result without device-profile details.
     */
    public McpMobileSessionResult(String mode, String platformName, String deviceName, String browserName,
                                  boolean active, List<McpCodeBlock> codeBlocks, List<String> warnings) {
        this(mode, platformName, deviceName, browserName, active, codeBlocks, warnings, Map.of());
    }
}
