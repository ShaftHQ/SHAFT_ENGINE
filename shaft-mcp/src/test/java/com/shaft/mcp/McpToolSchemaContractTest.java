package com.shaft.mcp;

import io.modelcontextprotocol.server.McpServerFeatures;
import org.junit.jupiter.api.Test;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContext;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.SerializationFeature;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ObjectNode;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * #7007: golden snapshot of the MCP tools/list contract (tool names and input schemas).
 * A schema or tool-set change fails until the snapshot is regenerated on purpose with
 * {@code -Dshaft.contract.update=true}, so agent clients never break silently.
 * Descriptions are excluded: they are guidance text, not the wire contract.
 */
@SpringBootTest(properties = "spring.ai.mcp.server.enabled=false")
class McpToolSchemaContractTest {
    static final Path SNAPSHOT = Path.of("src/test/resources/contracts/mcp-tool-schemas.json");
    private static final ObjectMapper MAPPER = JsonMapper.builder()
            .enable(SerializationFeature.INDENT_OUTPUT)
            .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS)
            .build();

    @Autowired
    private ApplicationContext context;

    @Test
    void toolSchemasMatchTheGoldenSnapshot() throws Exception {
        String actual = render(currentSchemas());
        if (Boolean.getBoolean("shaft.contract.update")) {
            Files.writeString(SNAPSHOT, actual, StandardCharsets.UTF_8);
        }
        assertTrue(Files.exists(SNAPSHOT), "missing " + SNAPSHOT + "; generate with -Dshaft.contract.update=true");
        assertEquals(Files.readString(SNAPSHOT, StandardCharsets.UTF_8), actual,
                "MCP tool contract changed. If intended, regenerate " + SNAPSHOT
                        + " with -Dshaft.contract.update=true and label a breaking change.");
    }

    @Test
    void renderingIsCanonical() throws Exception {
        Map<String, JsonNode> schemas = new TreeMap<>();
        schemas.put("b", MAPPER.readTree("{\"z\":1,\"a\":2}"));
        schemas.put("a", MAPPER.readTree("{}"));
        String rendered = render(schemas);
        assertTrue(rendered.indexOf("\"a\"") < rendered.indexOf("\"b\""));
        assertTrue(rendered.indexOf("\"a\" : 2") < rendered.indexOf("\"z\" : 1"));
        assertTrue(rendered.endsWith("\n"));
    }

    @SuppressWarnings("unchecked")
    private Map<String, JsonNode> currentSchemas() throws Exception {
        Map<String, JsonNode> schemas = new TreeMap<>();
        for (Object callback : context.getBean("shaftTools", List.class)) {
            var definition = ((ToolCallback) callback).getToolDefinition();
            schemas.put(definition.name(), MAPPER.readTree(definition.inputSchema()));
        }
        for (Object spec : context.getBean("toolSpecs", List.class)) {
            var tool = ((McpServerFeatures.SyncToolSpecification) spec).tool();
            schemas.put(tool.name(), MAPPER.valueToTree(tool.inputSchema()));
        }
        return schemas;
    }

    static String render(Map<String, JsonNode> schemas) throws Exception {
        ObjectNode root = MAPPER.createObjectNode();
        schemas.forEach((name, schema) -> root.set(name, sort(schema)));
        return MAPPER.writeValueAsString(root) + "\n";
    }

    private static JsonNode sort(JsonNode node) {
        if (node.isObject()) {
            ObjectNode sorted = MAPPER.createObjectNode();
            new TreeMap<String, JsonNode>(toMap(node)).forEach((k, v) -> sorted.set(k, sort(v)));
            return sorted;
        }
        if (node.isArray()) {
            var array = MAPPER.createArrayNode();
            node.forEach(item -> array.add(sort(item)));
            return array;
        }
        return node;
    }

    private static Map<String, JsonNode> toMap(JsonNode node) {
        Map<String, JsonNode> map = new TreeMap<>();
        node.properties().forEach(entry -> map.put(entry.getKey(), entry.getValue()));
        return map;
    }
}
