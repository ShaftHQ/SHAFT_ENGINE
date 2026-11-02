package com.shaft.commandline.runtime;

import com.shaft.commandline.mcp.CallToolResult;
import com.shaft.commandline.mcp.McpClient;
import com.shaft.commandline.util.Json;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;

import java.io.PrintWriter;
import java.util.List;

/**
 * Runs a single tool call end-to-end: routes the connection, parses arguments, renders the result,
 * and returns a process exit code. This is the shared path behind {@code call} and every curated alias.
 */
public final class ToolInvoker {

    private ToolInvoker() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * @param connector       the connection provider
     * @param toolName        the MCP tool name
     * @param options         the shared tool options ({@code --json}, {@code --args}, {@code --stdio-ok})
     * @param keyValues       repeated {@code key=value} argument tokens
     * @param requiresSession whether the tool needs live browser/device state
     * @param out             standard output writer
     * @param err             standard error writer
     * @return the process exit code (0 success, 1 tool/transport error)
     */
    public static int invoke(McpConnector connector, String toolName, ToolOptions options,
                             List<String> keyValues, boolean requiresSession, PrintWriter out, PrintWriter err) {
        ObjectNode arguments = ArgumentParser.parse(options.argsJson, keyValues);
        try (McpClient client = connector.connect(requiresSession, options.stdioOk)) {
            if (options.json) {
                JsonNode raw = client.callToolRaw(toolName, arguments);
                out.println(Json.MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(raw));
                out.flush();
                return raw.path("isError").asBoolean(false) ? 1 : 0;
            }
            CallToolResult result = client.callTool(toolName, arguments);
            if (result.isError()) {
                err.println(result.text());
                err.flush();
                return 1;
            }
            out.println(result.text());
            out.flush();
            return 0;
        }
    }
}
