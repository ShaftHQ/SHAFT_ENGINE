package com.shaft.mcp;

import java.util.List;
import java.util.Map;

/**
 * Confirmation-ready plan for a wrapped Appium Inspector recording session.
 */
public record McpMobileInspectorPlan(
        String confirmationToken,
        String platformName,
        boolean readyToStart,
        boolean confirmationRequired,
        boolean willProvisionAndroidEmulator,
        boolean realDeviceAvailable,
        String selectedDeviceId,
        String selectedAndroidAvdName,
        String outputPath,
        boolean includeSensitiveValues,
        Map<String, Object> appiumCapabilities,
        McpMobileToolchainStatus toolchainStatus,
        McpAndroidEmulatorProposal androidEmulatorProposal,
        List<McpCodeBlock> codeBlocks,
        List<String> nextSteps,
        List<String> warnings) {
    /**
     * Creates an immutable inspector recording plan.
     */
    public McpMobileInspectorPlan {
        confirmationToken = text(confirmationToken);
        platformName = text(platformName);
        selectedDeviceId = text(selectedDeviceId);
        selectedAndroidAvdName = text(selectedAndroidAvdName);
        outputPath = text(outputPath);
        appiumCapabilities = appiumCapabilities == null ? Map.of() : Map.copyOf(appiumCapabilities);
        codeBlocks = codeBlocks == null ? List.of() : List.copyOf(codeBlocks);
        nextSteps = nextSteps == null ? List.of() : List.copyOf(nextSteps);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
