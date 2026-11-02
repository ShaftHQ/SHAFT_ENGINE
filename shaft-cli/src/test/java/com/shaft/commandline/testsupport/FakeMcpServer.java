package com.shaft.commandline.testsupport;

import com.shaft.commandline.util.Json;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;

/**
 * A tiny deterministic MCP server speaking minimal JSON-RPC over stdio, for tests. It handles
 * {@code initialize}, {@code notifications/initialized} (no reply), {@code tools/list} (a small fixed
 * catalog that includes an arbitrary tool name to exercise generic dispatch), and {@code tools/call}
 * (echoes the arguments as text; the {@code failing_tool} returns {@code isError:true}).
 *
 * <p>Reusable in-process via {@link #serve(InputStream, OutputStream)} and as a subprocess via
 * {@link #main(String[])}.
 */
public final class FakeMcpServer {

    /** The arbitrary tool name used to prove generic {@code call} dispatch (parity contract). */
    public static final String ARBITRARY_TOOL = "some_new_tool_xyz";
    /** A tool that always returns an error result. */
    public static final String FAILING_TOOL = "failing_tool";
    /** A tool that echoes its arguments. */
    public static final String ECHO_TOOL = "echo_tool";

    private FakeMcpServer() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Subprocess entry point: serves on {@code System.in}/{@code System.out}.
     *
     * @param args ignored
     * @throws IOException if the streams fail
     */
    public static void main(String[] args) throws IOException {
        serve(System.in, System.out);
    }

    /**
     * Serves requests until the input stream is exhausted.
     *
     * @param in  the request stream (newline-delimited JSON-RPC)
     * @param out the response stream (newline-delimited JSON-RPC)
     * @throws IOException if reading or writing fails
     */
    public static void serve(InputStream in, OutputStream out) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8));
        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(out, StandardCharsets.UTF_8));
        String line;
        while ((line = reader.readLine()) != null) {
            if (line.isBlank()) {
                continue;
            }
            JsonNode request = Json.MAPPER.readTree(line);
            String method = request.path("method").asText("");
            if ("notifications/initialized".equals(method)) {
                continue;
            }
            ObjectNode response = handle(request, method);
            if (response != null) {
                writer.write(Json.MAPPER.writeValueAsString(response));
                writer.write("\n");
                writer.flush();
            }
        }
    }

    private static ObjectNode handle(JsonNode request, String method) {
        ObjectNode response = Json.newObject();
        response.put("jsonrpc", "2.0");
        response.set("id", request.path("id"));
        ObjectNode result = Json.newObject();
        switch (method) {
            case "initialize" -> {
                result.put("protocolVersion", "2025-03-26");
                result.set("capabilities", Json.newObject());
                ObjectNode serverInfo = Json.newObject();
                serverInfo.put("name", "shaft-mcp");
                serverInfo.put("version", "fake-1.0");
                result.set("serverInfo", serverInfo);
            }
            case "tools/list" -> result.set("tools", catalog());
            case "tools/call" -> {
                String name = request.path("params").path("name").asText("");
                JsonNode arguments = request.path("params").path("arguments");
                ArrayNode content = Json.MAPPER.createArrayNode();
                ObjectNode block = Json.newObject();
                block.put("type", "text");
                if (FAILING_TOOL.equals(name)) {
                    block.put("text", "boom");
                    content.add(block);
                    result.set("content", content);
                    result.put("isError", true);
                } else {
                    block.put("text", "called " + name + " with " + arguments.toString());
                    content.add(block);
                    result.set("content", content);
                    result.put("isError", false);
                }
            }
            default -> {
                ObjectNode error = Json.newObject();
                error.put("code", -32601);
                error.put("message", "Method not found: " + method);
                response.set("error", error);
                return response;
            }
        }
        response.set("result", result);
        return response;
    }

    private static ArrayNode catalog() {
        ArrayNode tools = Json.MAPPER.createArrayNode();
        tools.add(tool(ECHO_TOOL, "Echoes arguments back. Extra detail here."));
        tools.add(tool(ARBITRARY_TOOL, "An arbitrary future tool the CLI never hard-coded."));
        tools.add(tool(FAILING_TOOL, "Always returns an error."));
        return tools;
    }

    private static ObjectNode tool(String name, String description) {
        ObjectNode tool = Json.newObject();
        tool.put("name", name);
        tool.put("description", description);
        ObjectNode schema = Json.newObject();
        schema.put("type", "object");
        schema.set("properties", Json.newObject());
        tool.set("inputSchema", schema);
        return tool;
    }
}
