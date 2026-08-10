package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.ShaftCliCommandIndex;
import com.shaft.intellij.mcp.ToolCatalogIndex;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AssistantCapabilityCatalogTest {

    @Test
    void liveCatalogIsAuthoritativeAndKeepsUnknownDiscoveredTools() throws Exception {
        Method live = assertDoesNotThrow(() -> AssistantCapabilityCatalog.class
                .getDeclaredMethod("live", String.class, String.class));
        live.setAccessible(true);
        String payload = """
                {"tools":[
                  {"name":"browser_navigate","description":"Live browser navigation"},
                  {"name":"future_live_tool","description":"A newly discovered capability"}
                ]}
                """;

        String output = (String) live.invoke(null, "", payload);

        assertTrue(output.contains("Live SHAFT MCP discovery succeeded."), output);
        assertTrue(output.contains("browser_navigate"), output);
        assertTrue(output.contains("Live browser navigation"), output);
        assertTrue(output.contains("future_live_tool"), output);
        assertTrue(output.contains("Other discovered tools"), output);
        assertFalse(output.contains("element_click"),
                "a successful live catalog must not advertise bundled-only tools as available now");
    }

    @Test
    void unusableLiveCatalogFallsBackAndExplainsTheSourceBeforeTheCatalog() {
        for (String payload : new String[]{"", "not-json", "{}", "{\"tools\":{}}"}) {
            String output = AssistantCapabilityCatalog.live("", payload);

            assertTrue(output.startsWith("# SHAFT Assistant capabilities\n\nLive discovery returned no usable MCP tools"), output);
            assertTrue(output.contains("element_click"), output);
            assertFalse(output.contains("Live SHAFT MCP discovery succeeded."), output);
        }
    }

    @Test
    void validEmptyLiveCatalogRemainsAuthoritative() {
        String output = AssistantCapabilityCatalog.live("", "{\"tools\":[]}");

        assertTrue(output.contains("Live SHAFT MCP discovery succeeded."), output);
        assertFalse(output.contains("element_click"), output);
        assertFalse(output.contains("bundled catalog"), output);
    }

    @Test
    void multiwordTopicsMatchAcrossToolNameSeparators() {
        String output = AssistantCapabilityCatalog.bundled("browser navigate");

        assertTrue(output.contains("browser_navigate"), output);
        assertTrue(output.contains("shaft-cli browser"), output);
        assertFalse(output.contains("No capability matched"), output);
    }

    @Test
    void bundledCatalogContainsEveryMcpAndCliEntryExactlyOnce() {
        String output = AssistantCapabilityCatalog.bundled("");

        for (ToolCatalogIndex.ToolMetadata tool : ToolCatalogIndex.tools()) {
            assertTrue(occurrences(output, "- `" + tool.name() + "` —") == 1, tool.name());
            if (!tool.slashAlias().isBlank()) {
                assertTrue(occurrences(output, "(alias: `/mcp " + tool.slashAlias() + "`)") == 1,
                        tool.slashAlias());
            }
        }
        for (ShaftCliCommandIndex.CommandMetadata command : ShaftCliCommandIndex.commands()) {
            assertTrue(occurrences(output, "- `shaft-cli " + command.name() + "` —") == 1, command.name());
        }
    }

    private static int occurrences(String text, String needle) {
        return (text.length() - text.replace(needle, "").length()) / needle.length();
    }
}
