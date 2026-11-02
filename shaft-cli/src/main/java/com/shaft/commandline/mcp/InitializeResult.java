package com.shaft.commandline.mcp;

/**
 * The negotiated result of the MCP {@code initialize} handshake.
 *
 * @param serverName      the server identity, expected to be {@code shaft-mcp}
 * @param serverVersion   the server version string
 * @param protocolVersion the negotiated MCP protocol version, e.g. {@code 2025-03-26}
 */
public record InitializeResult(String serverName, String serverVersion, String protocolVersion) {
}
