package com.shaft.mcp;

/**
 * MCP-visible local agent client descriptor.
 *
 * @param id stable SHAFT client id
 * @param displayName human-readable client name
 * @param executableName default executable name
 * @param requiresCloudApiKey whether SHAFT requires a cloud API key for this route
 */
public record AutobotLocalAgentClient(
        String id,
        String displayName,
        String executableName,
        boolean requiresCloudApiKey) {
}
