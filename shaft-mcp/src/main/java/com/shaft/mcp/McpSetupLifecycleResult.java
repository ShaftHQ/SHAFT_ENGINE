package com.shaft.mcp;

import java.util.Map;

/** Structured result for provider-neutral setup service lifecycle operations. */
public record McpSetupLifecycleResult(boolean supported, String profile, String operation, String message,
                                      String endpoint, Map<String, String> connectionProperties,
                                      String planDigest, String logs) {
    public McpSetupLifecycleResult {
        endpoint = endpoint == null ? "" : endpoint;
        connectionProperties = connectionProperties == null ? Map.of() : Map.copyOf(connectionProperties);
        planDigest = planDigest == null ? "" : planDigest;
        logs = logs == null ? "" : logs;
    }

    public McpSetupLifecycleResult(boolean supported, String profile, String operation, String message) {
        this(supported, profile, operation, message, "", Map.of(), "", "");
    }
}
