package com.shaft.commandline.mcp;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;

import java.util.List;

/**
 * A minimal MCP client speaking JSON-RPC 2.0 to shaft-mcp. Implementations perform the
 * {@code initialize} / {@code notifications/initialized} handshake lazily on first use.
 *
 * <p>Two implementations exist: an ephemeral stdio child (one-shot mode) and a persistent
 * streamable-HTTP daemon connection (session mode).
 */
public interface McpClient extends AutoCloseable {

    /**
     * Performs the {@code initialize} handshake if it has not already run.
     *
     * @return the negotiated server identity
     */
    InitializeResult initialize();

    /**
     * Lists the tools advertised by the server via {@code tools/list}.
     *
     * @return the advertised tools
     */
    List<Tool> listTools();

    /**
     * Invokes {@code tools/call} for the given tool name and arguments.
     *
     * @param name      the tool name
     * @param arguments the arguments object (may be empty, never {@code null})
     * @return the tool result
     */
    CallToolResult callTool(String name, ObjectNode arguments);

    /**
     * @return the raw JSON-RPC {@code result} node of a {@code tools/list} call (for {@code --json})
     */
    JsonNode listToolsRaw();

    /**
     * Invokes a tool and returns the raw JSON-RPC {@code result} node (for {@code --json}).
     *
     * @param name      the tool name
     * @param arguments the arguments object
     * @return the raw {@code result} node
     */
    JsonNode callToolRaw(String name, ObjectNode arguments);

    @Override
    void close();
}
