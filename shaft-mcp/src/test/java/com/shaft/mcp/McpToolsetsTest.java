package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpToolsetsTest {

    @Test
    void unsetBlankAndAllKeepEverySuppliedService() {
        Map<String, List<String>> grouped = sample();
        assertEquals(List.of("driver_initialize", "browser_navigate", "element_click", "capture_start"),
                McpToolsets.filter(null, grouped));
        assertEquals(groupedValues(grouped), McpToolsets.filter("", grouped));
        assertEquals(groupedValues(grouped), McpToolsets.filter("all", grouped));
        assertEquals(groupedValues(grouped), McpToolsets.filter("  ALL  ", grouped));
    }

    @Test
    void namedGroupsAreCaseInsensitiveAndRejectUnknownNames() {
        Map<String, List<String>> grouped = sample();
        assertEquals(List.of("driver_initialize", "browser_navigate", "element_click"),
                McpToolsets.filter(" Browser, element ,engine ", grouped));
        assertEquals(List.of("capture_start"), McpToolsets.filter("heal,capture", grouped));

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> McpToolsets.filter("browser,not-a-group", grouped));
        assertTrue(failure.getMessage().contains("not-a-group"), failure.getMessage());
        assertTrue(failure.getMessage().contains("Valid groups:"), failure.getMessage());
        for (String group : List.of(
                "browser", "element", "engine", "capture", "mobile", "design", "trace", "doctor",
                "infrastructure", "setup", "heal", "healer", "project", "autobot", "guide",
                "planner", "coding-partner", "test-automation", "report")) {
            assertTrue(failure.getMessage().contains(group), failure.getMessage());
        }
    }

    @Test
    void blankPropertyFallsBackToTheEnvironmentValue() {
        assertEquals("", McpToolsets.resolve(null, null));
        assertEquals("", McpToolsets.resolve("   ", null));
        assertEquals("browser", McpToolsets.resolve(null, "browser"));
        assertEquals("element", McpToolsets.resolve("element", "browser"));
        assertEquals(List.of("browser_navigate"),
                McpToolsets.filter(McpToolsets.resolve(null, "browser"), sample()));
        assertEquals(groupedValues(sample()), McpToolsets.filter(McpToolsets.resolve("   ", "   "), sample()));
    }

    @Test
    void schemaStripRemovesOnlyTheTopLevelDialectUrl() {
        String stripped = McpToolsets.stripSchemaJson(
                "{\"$schema\":\"https://json-schema.org/draft/2020-12/schema\",\"type\":\"object\","
                        + "\"additionalProperties\":false,\"properties\":{\"url\":{\"type\":\"string\","
                        + "\"description\":\"page\"}},\"required\":[\"url\"]}");
        assertTrue(stripped.contains("\"additionalProperties\":false"), stripped);
        assertTrue(stripped.contains("\"description\":\"page\""), stripped);
        assertTrue(stripped.contains("\"required\":[\"url\"]"), stripped);
        assertFalse(stripped.contains("$schema"), stripped);
    }

    private static Map<String, List<String>> sample() {
        Map<String, List<String>> grouped = new LinkedHashMap<>();
        grouped.put("engine", List.of("driver_initialize"));
        grouped.put("browser", List.of("browser_navigate"));
        grouped.put("element", List.of("element_click"));
        grouped.put("capture", List.of("capture_start"));
        return grouped;
    }

    private static List<String> groupedValues(Map<String, List<String>> grouped) {
        return McpToolsets.filter("all", grouped);
    }
}
