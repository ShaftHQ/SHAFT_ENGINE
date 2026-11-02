package com.shaft.commandline.mcp;

import com.shaft.commandline.util.Json;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * Pins {@link AbstractMcpClient}'s {@code tools/list} caching contract: a second
 * {@link AbstractMcpClient#listTools()}/{@link AbstractMcpClient#listToolsRaw()} call must be
 * served from the memoized result instead of round-tripping {@code rpc()} again, a
 * {@code forceRefresh} overload must bypass the cache, and {@link AbstractMcpClient#initialize()}
 * keeps running only once (pre-existing contract, re-pinned here to guard the cache changes
 * did not disturb it).
 *
 * <p>Uses a minimal in-memory {@link AbstractMcpClient} subclass that counts {@code rpc()} calls
 * per JSON-RPC method, needing no real transport or subprocess.</p>
 */
class AbstractMcpClientCacheTest {

    @Test
    void secondListToolsCallServesFromCacheWithoutARoundTrip() {
        CountingClient client = new CountingClient();

        List<Tool> first = client.listTools();
        List<Tool> second = client.listTools();

        assertEquals(1, client.toolsListCalls.get(), "the second listTools() call must not re-fetch");
        assertSame(first, second, "a cache hit must return the exact same list instance");
        assertEquals(List.of(CountingClient.TOOL_NAME), first.stream().map(Tool::name).toList());
    }

    @Test
    void listToolsForceRefreshBypassesTheCache() {
        CountingClient client = new CountingClient();

        List<Tool> first = client.listTools();
        List<Tool> refreshed = client.listTools(true);
        List<Tool> cachedAgain = client.listTools();

        assertEquals(2, client.toolsListCalls.get(), "forceRefresh must issue a fresh round-trip");
        assertNotSame(first, refreshed);
        assertSame(refreshed, cachedAgain, "the call after a refresh must serve the refreshed cache");
    }

    @Test
    void secondListToolsRawCallServesFromCacheWithoutARoundTrip() {
        CountingClient client = new CountingClient();

        JsonNode first = client.listToolsRaw();
        JsonNode second = client.listToolsRaw();

        assertEquals(1, client.toolsListCalls.get());
        assertSame(first, second);
    }

    @Test
    void listToolsRawForceRefreshBypassesTheCache() {
        CountingClient client = new CountingClient();

        JsonNode first = client.listToolsRaw();
        JsonNode refreshed = client.listToolsRaw(true);

        assertEquals(2, client.toolsListCalls.get());
        assertNotSame(first, refreshed);
    }

    @Test
    void invalidateToolsCacheForcesAFreshRoundTripOnTheNextCall() {
        CountingClient client = new CountingClient();
        client.listTools();

        client.invalidateToolsCache();
        client.listTools();

        assertEquals(2, client.toolsListCalls.get(),
                "a call after invalidateToolsCache() must round-trip again, not serve the stale cache");
    }

    @Test
    void initializeRunsOnlyOnceAcrossRepeatedCalls() {
        CountingClient client = new CountingClient();

        client.initialize();
        client.initialize();

        assertEquals(1, client.initializeCalls.get(), "initialize() must keep its existing once-only contract");
    }

    /** Minimal in-memory {@link AbstractMcpClient}: counts {@code rpc()} calls, no transport needed. */
    private static final class CountingClient extends AbstractMcpClient {
        static final String TOOL_NAME = "counting_tool";

        private final AtomicInteger toolsListCalls = new AtomicInteger();
        private final AtomicInteger initializeCalls = new AtomicInteger();

        @Override
        protected JsonNode rpc(ObjectNode request) {
            String method = request.path("method").asText("");
            ObjectNode response = Json.newObject();
            response.put("jsonrpc", "2.0");
            response.set("id", request.path("id"));
            ObjectNode result = Json.newObject();
            switch (method) {
                case "initialize" -> {
                    initializeCalls.incrementAndGet();
                    result.put("protocolVersion", "2025-03-26");
                    result.set("capabilities", Json.newObject());
                    ObjectNode serverInfo = Json.newObject();
                    serverInfo.put("name", "counting-fake");
                    serverInfo.put("version", "test");
                    result.set("serverInfo", serverInfo);
                }
                case "tools/list" -> {
                    int calls = toolsListCalls.incrementAndGet();
                    ArrayNode tools = Json.MAPPER.createArrayNode();
                    ObjectNode tool = Json.newObject();
                    tool.put("name", TOOL_NAME);
                    tool.put("description", "call #" + calls);
                    tools.add(tool);
                    result.set("tools", tools);
                }
                default -> throw new McpException("Unexpected method in test: " + method);
            }
            response.set("result", result);
            return response;
        }

        @Override
        protected void sendNotification(ObjectNode notification) {
            // no-op: nothing to notify in-memory
        }

        @Override
        public void close() {
            // nothing to release
        }
    }
}
