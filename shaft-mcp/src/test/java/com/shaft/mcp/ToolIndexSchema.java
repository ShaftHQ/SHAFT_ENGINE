package com.shaft.mcp;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

/**
 * Converts a live JSON-schema (either the raw JSON string {@code ToolDefinition.inputSchema()}
 * returns, or the already-parsed {@code Map<String,Object>} {@code McpSchema.Tool.inputSchema()}
 * returns) into the flat {@link ToolIndexParam} list the tool-index records. Only the top-level
 * {@code properties}/{@code required} keys are read -- nested object parameters (for example
 * {@code driver_initialize}'s {@code mobileOptions}) are recorded with their own {@code "object"}
 * type but not recursively expanded, matching design doc Decision 4's flat per-tool params list.
 */
final class ToolIndexSchema {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    private ToolIndexSchema() {
    }

    static List<ToolIndexParam> paramsFromJson(String schemaJson) {
        return paramsFromNode(MAPPER.readTree(schemaJson));
    }

    static List<ToolIndexParam> paramsFromMap(Map<String, Object> schemaMap) {
        return paramsFromNode(MAPPER.valueToTree(schemaMap));
    }

    private static List<ToolIndexParam> paramsFromNode(JsonNode schema) {
        JsonNode properties = schema.path("properties");
        if (!properties.isObject()) {
            return List.of();
        }
        List<String> required = new ArrayList<>();
        for (JsonNode requiredName : schema.path("required")) {
            required.add(requiredName.asText());
        }

        List<ToolIndexParam> params = new ArrayList<>();
        for (String paramName : properties.propertyNames()) {
            JsonNode propertySchema = properties.get(paramName);
            JsonNode typeNode = propertySchema.path("type");
            String type = typeNode.isMissingNode() ? "unknown" : typeNode.asText();
            JsonNode descriptionNode = propertySchema.path("description");
            String description = descriptionNode.isMissingNode() ? null : descriptionNode.asText();
            params.add(new ToolIndexParam(paramName, type, required.contains(paramName), description));
        }
        params.sort(Comparator.comparing(ToolIndexParam::name));
        return params;
    }
}
