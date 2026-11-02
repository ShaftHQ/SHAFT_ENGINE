package com.shaft.mcp;

import java.nio.file.Path;
import java.util.List;

/**
 * User-confirmable Android emulator provisioning proposal.
 */
public record McpAndroidEmulatorProposal(
        String avdName,
        String deviceProfile,
        int apiLevel,
        String imageTag,
        String abi,
        int ramMb,
        int cores,
        Path sdkRoot,
        Path avdHome,
        List<String> sdkPackages,
        List<String> commands,
        List<String> warnings) {
    /**
     * Creates an immutable Android emulator proposal.
     */
    public McpAndroidEmulatorProposal {
        avdName = text(avdName);
        deviceProfile = text(deviceProfile);
        apiLevel = Math.max(apiLevel, 1);
        imageTag = text(imageTag);
        abi = text(abi);
        ramMb = Math.max(ramMb, 512);
        cores = Math.max(cores, 1);
        sdkPackages = sdkPackages == null ? List.of() : List.copyOf(sdkPackages);
        commands = commands == null ? List.of() : List.copyOf(commands);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
