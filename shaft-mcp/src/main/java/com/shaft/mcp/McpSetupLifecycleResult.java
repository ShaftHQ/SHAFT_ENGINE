package com.shaft.mcp;

/** Explicit lifecycle capability result for profiles that do not yet own a service lease. */
public record McpSetupLifecycleResult(boolean supported, String profile, String operation, String message) { }
