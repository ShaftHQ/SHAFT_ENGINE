package com.shaft.commandline.mcp;

import com.shaft.commandline.util.Json;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;

import java.util.ArrayList;
import java.util.List;

/**
 * Base MCP client that owns the JSON-RPC framing and the {@code initialize} /
 * {@code notifications/initialized} handshake. Subclasses only implement the raw transport:
 * {@link #rpc(ObjectNode)} for request/response and {@link #sendNotification(ObjectNode)} for
 * fire-and-forget notifications.
 */
public abstract class AbstractMcpClient implements McpClient {

    /**
     * The MCP protocol version this client negotiates (matches shaft-mcp's advertised version).
     */
    protected static final String PROTOCOL_VERSION = "2025-03-26";

    private long nextId = 1;
    private boolean initialized;
    private InitializeResult initializeResult;

    /**
     * Sends a JSON-RPC request and returns the parsed response envelope (containing either
     * {@code result} or {@code error}).
     *
     * @param request the JSON-RPC request object (with {@code id})
     * @return the parsed response envelope
     */
    protected abstract JsonNode rpc(ObjectNode request);

    /**
     * Sends a JSON-RPC notification (no {@code id}, no response expected).
     *
     * @param notification the JSON-RPC notification object
     */
    protected abstract void sendNotification(ObjectNode notification);

    @Override
    public InitializeResult initialize() {
        if (initialized) {
            return initializeResult;
        }
        ObjectNode params = Json.newObject();
        params.put("protocolVersion", PROTOCOL_VERSION);
        params.set("capabilities", Json.newObject());
        ObjectNode clientInfo = Json.newObject();
        clientInfo.put("name", "shaft-cli");
        clientInfo.put("version", clientVersion());
        params.set("clientInfo", clientInfo);

        JsonNode response = rpc(request("initialize", params));
        checkError(response);
        JsonNode result = response.path("result");
        JsonNode serverInfo = result.path("serverInfo");
        initializeResult = new InitializeResult(
                serverInfo.path("name").asText(""),
                serverInfo.path("version").asText(""),
                result.path("protocolVersion").asText(PROTOCOL_VERSION));

        ObjectNode initializedNotification = Json.newObject();
        initializedNotification.put("jsonrpc", "2.0");
        initializedNotification.put("method", "notifications/initialized");
        initializedNotification.set("params", Json.newObject());
        sendNotification(initializedNotification);

        initialized = true;
        return initializeResult;
    }

    @Override
    public List<Tool> listTools() {
        ensureInitialized();
        JsonNode response = rpc(request("tools/list", Json.newObject()));
        checkError(response);
        List<Tool> tools = new ArrayList<>();
        JsonNode toolsNode = response.path("result").path("tools");
        if (toolsNode instanceof ArrayNode array) {
            for (JsonNode tool : array) {
                JsonNode schema = tool.path("inputSchema");
                tools.add(new Tool(
                        tool.path("name").asText(""),
                        tool.path("description").asText(""),
                        schema.isMissingNode() ? null : schema));
            }
        }
        return tools;
    }

    @Override
    public CallToolResult callTool(String name, ObjectNode arguments) {
        ensureInitialized();
        ObjectNode params = Json.newObject();
        params.put("name", name);
        params.set("arguments", arguments == null ? Json.newObject() : arguments);
        JsonNode response = rpc(request("tools/call", params));
        checkError(response);
        JsonNode result = response.path("result");
        List<Content> content = new ArrayList<>();
        JsonNode contentNode = result.path("content");
        if (contentNode instanceof ArrayNode array) {
            for (JsonNode block : array) {
                content.add(new Content(
                        block.path("type").asText(""),
                        block.has("text") ? block.path("text").asText("") : null));
            }
        }
        return new CallToolResult(content, result.path("isError").asBoolean(false));
    }

    /**
     * @return the raw {@code result} node of a fresh {@code tools/list} call, for {@code --json} output
     */
    public JsonNode listToolsRaw() {
        ensureInitialized();
        JsonNode response = rpc(request("tools/list", Json.newObject()));
        checkError(response);
        return response.path("result");
    }

    /**
     * Invokes a tool and returns the raw {@code result} node, for {@code --json} output.
     *
     * @param name      the tool name
     * @param arguments the arguments object
     * @return the raw JSON-RPC {@code result} node
     */
    public JsonNode callToolRaw(String name, ObjectNode arguments) {
        ensureInitialized();
        ObjectNode params = Json.newObject();
        params.put("name", name);
        params.set("arguments", arguments == null ? Json.newObject() : arguments);
        JsonNode response = rpc(request("tools/call", params));
        checkError(response);
        return response.path("result");
    }

    private void ensureInitialized() {
        if (!initialized) {
            initialize();
        }
    }

    /**
     * Builds a JSON-RPC request envelope with the next monotonic id.
     *
     * @param method the JSON-RPC method
     * @param params the params object
     * @return the request envelope
     */
    protected ObjectNode request(String method, ObjectNode params) {
        ObjectNode request = Json.newObject();
        request.put("jsonrpc", "2.0");
        request.put("id", nextId++);
        request.put("method", method);
        request.set("params", params);
        return request;
    }

    /**
     * Throws {@link McpException} if the response carries a JSON-RPC {@code error} object.
     *
     * @param response the response envelope
     */
    protected void checkError(JsonNode response) {
        if (response.has("error") && !response.path("error").isNull()) {
            JsonNode error = response.path("error");
            throw new McpException("MCP error " + error.path("code").asInt(0)
                    + ": " + error.path("message").asText("unknown error"));
        }
    }

    private String clientVersion() {
        String version = getClass().getPackage().getImplementationVersion();
        return version != null ? version : "development";
    }
}
