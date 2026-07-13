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
     * Memoized {@code tools/list} results for {@link #listTools()}/{@link #listToolsRaw()},
     * scoped to this connection. Each one-shot CLI invocation constructs a fresh client, so no
     * cross-invocation staleness is possible; within one invocation, {@code tools} and {@code call}
     * commands that would otherwise round-trip {@code tools/list} more than once (e.g. a future
     * interactive/session-mode caller reusing one client for several commands) now pay for it once.
     */
    private List<Tool> toolsCache;
    private JsonNode toolsRawCache;

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
        return listTools(false);
    }

    /**
     * Lists the tools advertised by the server via {@code tools/list}, serving the last successful
     * response from this connection's cache unless a refresh is requested.
     *
     * @param forceRefresh when {@code true}, bypasses the cache and issues a fresh {@code tools/list}
     * @return the advertised tools
     */
    public List<Tool> listTools(boolean forceRefresh) {
        if (!forceRefresh && toolsCache != null) {
            return toolsCache;
        }
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
        toolsCache = List.copyOf(tools);
        return toolsCache;
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
     * @return the raw {@code result} node of a {@code tools/list} call, for {@code --json} output;
     *         served from this connection's cache unless it has not been fetched yet
     */
    @Override
    public JsonNode listToolsRaw() {
        return listToolsRaw(false);
    }

    /**
     * Returns the raw {@code result} node of a {@code tools/list} call, for {@code --json} output,
     * serving the last successful response from this connection's cache unless a refresh is
     * requested.
     *
     * @param forceRefresh when {@code true}, bypasses the cache and issues a fresh {@code tools/list}
     * @return the raw {@code result} node
     */
    public JsonNode listToolsRaw(boolean forceRefresh) {
        if (!forceRefresh && toolsRawCache != null) {
            return toolsRawCache;
        }
        ensureInitialized();
        JsonNode response = rpc(request("tools/list", Json.newObject()));
        checkError(response);
        toolsRawCache = response.path("result");
        return toolsRawCache;
    }

    /**
     * Clears the memoized {@code tools/list} results, forcing the next {@link #listTools()} or
     * {@link #listToolsRaw()} call to round-trip. Subclasses that support reconnecting an existing
     * instance (rather than constructing a fresh one) should call this alongside resetting whatever
     * marks the connection as initialized.
     */
    protected void invalidateToolsCache() {
        toolsCache = null;
        toolsRawCache = null;
    }

    /**
     * Invokes a tool and returns the raw {@code result} node, for {@code --json} output.
     *
     * @param name      the tool name
     * @param arguments the arguments object
     * @return the raw JSON-RPC {@code result} node
     */
    @Override
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
