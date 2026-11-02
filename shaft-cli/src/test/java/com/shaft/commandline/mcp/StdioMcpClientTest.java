package com.shaft.commandline.mcp;

import com.shaft.commandline.testsupport.FakeMcpServer;
import com.shaft.commandline.testsupport.InProcessMcp;
import com.shaft.commandline.util.Json;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.node.ObjectNode;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class StdioMcpClientTest {

    @Test
    void initializeNegotiatesServerIdentity() throws Exception {
        try (StdioMcpClient client = InProcessMcp.start()) {
            InitializeResult result = client.initialize();
            assertEquals("shaft-mcp", result.serverName());
            assertEquals("2025-03-26", result.protocolVersion());
        }
    }

    @Test
    void listsToolsIncludingArbitraryName() throws Exception {
        try (StdioMcpClient client = InProcessMcp.start()) {
            List<String> names = client.listTools().stream().map(Tool::name).toList();
            assertTrue(names.contains(FakeMcpServer.ECHO_TOOL));
            assertTrue(names.contains(FakeMcpServer.ARBITRARY_TOOL));
        }
    }

    @Test
    void callsToolAndEchoesArguments() throws Exception {
        try (StdioMcpClient client = InProcessMcp.start()) {
            ObjectNode args = Json.newObject();
            args.put("value", "hi");
            CallToolResult result = client.callTool(FakeMcpServer.ECHO_TOOL, args);
            assertFalse(result.isError());
            assertTrue(result.text().contains("called " + FakeMcpServer.ECHO_TOOL));
            assertTrue(result.text().contains("hi"));
        }
    }

    @Test
    void surfacesToolLevelError() throws Exception {
        try (StdioMcpClient client = InProcessMcp.start()) {
            CallToolResult result = client.callTool(FakeMcpServer.FAILING_TOOL, Json.newObject());
            assertTrue(result.isError());
            assertEquals("boom", result.text());
        }
    }
}
