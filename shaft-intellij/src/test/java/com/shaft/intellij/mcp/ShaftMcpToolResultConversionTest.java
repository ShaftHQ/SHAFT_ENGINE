package com.shaft.intellij.mcp;

import com.google.gson.JsonElement;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins {@link ShaftMcpInvocationService#toolResult}: a successful JSON-RPC envelope can still carry
 * a failing tool call ({@code isError: true}) per the MCP protocol. Before this conversion existed,
 * every such failure (e.g. a doctor/capture tool failing in a project with no SHAFT setup) was
 * reported to the user as a success.
 */
class ShaftMcpToolResultConversionTest {
    @Test
    void toolErrorResultIsReportedAsFailureWithItsOwnText() {
        JsonElement result = JsonParser.parseString(
                "{\"content\":[{\"type\":\"text\",\"text\":\"Allure result path cannot be resolved.\"}],"
                        + "\"isError\":true}");

        ShaftMcpToolResult converted = ShaftMcpInvocationService.toolResult(result);

        assertFalse(converted.success());
        assertEquals("Allure result path cannot be resolved.", converted.output());
        assertEquals(McpInvocationError.TOOL_ERROR, converted.errorCategory());
    }

    @Test
    void toolErrorResultWithMultipleContentEntriesJoinsTheirText() {
        JsonElement result = JsonParser.parseString(
                "{\"content\":[{\"type\":\"text\",\"text\":\"first line\"},"
                        + "{\"type\":\"text\",\"text\":\"second line\"}],\"isError\":true}");

        ShaftMcpToolResult converted = ShaftMcpInvocationService.toolResult(result);

        assertFalse(converted.success());
        assertEquals("first line\nsecond line", converted.output());
    }

    @Test
    void toolErrorResultWithoutContentTextFallsBackToTheRawJson() {
        JsonElement result = JsonParser.parseString("{\"isError\":true}");

        ShaftMcpToolResult converted = ShaftMcpInvocationService.toolResult(result);

        assertFalse(converted.success());
        assertEquals("{\"isError\":true}", converted.output());
    }

    @Test
    void successfulToolResultIsReportedAsSuccess() {
        JsonElement result = JsonParser.parseString(
                "{\"content\":[{\"type\":\"text\",\"text\":\"ok\"}],\"isError\":false}");

        ShaftMcpToolResult converted = ShaftMcpInvocationService.toolResult(result);

        assertTrue(converted.success());
    }

    @Test
    void toolsListResultHasNoIsErrorFieldAndIsReportedAsSuccess() {
        JsonElement result = JsonParser.parseString("{\"tools\":[{\"name\":\"fake_tool\"}]}");

        ShaftMcpToolResult converted = ShaftMcpInvocationService.toolResult(result);

        assertTrue(converted.success());
        assertEquals(result.toString(), converted.output());
    }

    @Test
    void nullResultIsReportedAsSuccessWithAnEmptyObject() {
        ShaftMcpToolResult converted = ShaftMcpInvocationService.toolResult(null);

        assertTrue(converted.success());
        assertEquals("{}", converted.output());
    }
}
