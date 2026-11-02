package com.shaft.mcp;

import java.util.List;

/**
 * Mobile device discovered by local platform tooling.
 */
public record McpMobileDevice(
        String id,
        String name,
        String platformName,
        String state,
        boolean emulator,
        List<String> warnings) {
    /**
     * Creates an immutable mobile device descriptor.
     */
    public McpMobileDevice {
        id = text(id);
        name = text(name);
        platformName = text(platformName);
        state = text(state);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
