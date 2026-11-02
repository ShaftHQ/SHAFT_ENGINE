package com.shaft.commandline.runtime;

import com.shaft.commandline.mcp.McpException;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.node.ObjectNode;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ArgumentParserTest {

    @Test
    void coercesKeyValueTypes() {
        ObjectNode node = ArgumentParser.parse(null, List.of("count=3", "ratio=1.5", "flag=true", "name=hello"));
        assertTrue(node.path("count").isIntegralNumber());
        assertEquals(3, node.path("count").asInt());
        assertEquals(1.5, node.path("ratio").asDouble());
        assertTrue(node.path("flag").isBoolean());
        assertTrue(node.path("flag").asBoolean());
        assertTrue(node.path("name").isTextual());
        assertEquals("hello", node.path("name").asText());
    }

    @Test
    void parsesArgsJsonObject() {
        ObjectNode node = ArgumentParser.parse("{\"url\":\"https://example.com\"}", null);
        assertEquals("https://example.com", node.path("url").asText());
    }

    @Test
    void keyValueOverridesArgsJson() {
        ObjectNode node = ArgumentParser.parse("{\"x\":1}", List.of("x=2"));
        assertEquals(2, node.path("x").asInt());
    }

    @Test
    void parsesNestedJsonValue() {
        ObjectNode node = ArgumentParser.parse(null, List.of("filter={\"k\":1}"));
        assertTrue(node.path("filter").isObject());
        assertEquals(1, node.path("filter").path("k").asInt());
    }

    @Test
    void emptyInputsYieldEmptyObject() {
        ObjectNode node = ArgumentParser.parse(null, List.of());
        assertTrue(node.isEmpty());
    }

    @Test
    void rejectsMissingEquals() {
        assertThrows(McpException.class, () -> ArgumentParser.parse(null, List.of("bogus")));
    }

    @Test
    void rejectsNonObjectArgsJson() {
        assertThrows(McpException.class, () -> ArgumentParser.parse("[1,2,3]", null));
    }
}
