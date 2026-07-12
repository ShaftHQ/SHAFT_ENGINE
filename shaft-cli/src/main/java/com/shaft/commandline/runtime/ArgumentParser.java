package com.shaft.commandline.runtime;

import com.shaft.commandline.mcp.McpException;
import com.shaft.commandline.util.Json;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;

import java.util.List;

/**
 * Builds a tool-arguments JSON object from either a {@code --args '{...}'} JSON string or repeated
 * {@code key=value} pairs. When both are supplied, {@code --args} provides the base and key=value
 * pairs override individual keys.
 */
public final class ArgumentParser {

    private ArgumentParser() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * @param argsJson  the {@code --args} JSON object string, or {@code null}
     * @param keyValues repeated {@code key=value} tokens, or {@code null}
     * @return the assembled arguments object (never {@code null})
     */
    public static ObjectNode parse(String argsJson, List<String> keyValues) {
        ObjectNode node = Json.newObject();
        if (argsJson != null && !argsJson.isBlank()) {
            JsonNode parsed;
            try {
                parsed = Json.MAPPER.readTree(argsJson);
            } catch (RuntimeException exception) {
                throw new McpException("--args is not valid JSON: " + exception.getMessage(), exception);
            }
            if (!(parsed instanceof ObjectNode object)) {
                throw new McpException("--args must be a JSON object, e.g. --args '{\"key\":\"value\"}'");
            }
            node = object;
        }
        if (keyValues != null) {
            for (String pair : keyValues) {
                int equals = pair.indexOf('=');
                if (equals < 0) {
                    throw new McpException("Expected key=value but got: " + pair);
                }
                String key = pair.substring(0, equals);
                String value = pair.substring(equals + 1);
                node.set(key, coerce(value));
            }
        }
        return node;
    }

    private static JsonNode coerce(String value) {
        if ("true".equals(value) || "false".equals(value)) {
            return Json.MAPPER.getNodeFactory().booleanNode(Boolean.parseBoolean(value));
        }
        if (isInteger(value)) {
            return Json.MAPPER.getNodeFactory().numberNode(Long.parseLong(value));
        }
        if (isDecimal(value)) {
            return Json.MAPPER.getNodeFactory().numberNode(Double.parseDouble(value));
        }
        String trimmed = value.stripLeading();
        if (trimmed.startsWith("{") || trimmed.startsWith("[")) {
            try {
                return Json.MAPPER.readTree(value);
            } catch (RuntimeException ignored) {
                // Fall through to a plain string.
            }
        }
        return Json.MAPPER.getNodeFactory().textNode(value);
    }

    private static boolean isInteger(String value) {
        if (value.isEmpty()) {
            return false;
        }
        try {
            Long.parseLong(value);
            return true;
        } catch (NumberFormatException exception) {
            return false;
        }
    }

    private static boolean isDecimal(String value) {
        if (value.isEmpty()) {
            return false;
        }
        try {
            Double.parseDouble(value);
            return true;
        } catch (NumberFormatException exception) {
            return false;
        }
    }
}
