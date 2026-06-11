package com.shaft.pilot.json;

import com.fasterxml.jackson.databind.JsonNode;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Deterministic validator for the JSON Schema subset shared by supported providers.
 */
public final class JsonSchemaValidator {
    private JsonSchemaValidator() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Validates a payload against the supported schema subset.
     *
     * @param schema JSON schema
     * @param payload candidate payload
     * @return validation errors; empty when valid
     */
    public static List<String> validate(JsonNode schema, JsonNode payload) {
        List<String> errors = new ArrayList<>();
        validateNode(schema, payload, "$", errors);
        return List.copyOf(errors);
    }

    private static void validateNode(JsonNode schema, JsonNode value, String path, List<String> errors) {
        if (schema == null || schema.isMissingNode() || schema.isNull() || !schema.isObject()) {
            return;
        }
        if (!matchesType(schema.get("type"), value)) {
            errors.add(path + " does not match the required type.");
            return;
        }
        JsonNode enumValues = schema.get("enum");
        if (enumValues != null && enumValues.isArray()) {
            boolean matched = false;
            for (JsonNode allowed : enumValues) {
                if (allowed.equals(value)) {
                    matched = true;
                    break;
                }
            }
            if (!matched) {
                errors.add(path + " is not an allowed enum value.");
            }
        }
        if (value != null && value.isObject()) {
            validateObject(schema, value, path, errors);
        } else if (value != null && value.isArray()) {
            validateArray(schema, value, path, errors);
        } else if (value != null && value.isNumber()) {
            validateNumber(schema, value, path, errors);
        }
    }

    private static boolean matchesType(JsonNode type, JsonNode value) {
        if (type == null || type.isNull()) {
            return true;
        }
        if (type.isArray()) {
            for (JsonNode candidate : type) {
                if (matchesType(candidate, value)) {
                    return true;
                }
            }
            return false;
        }
        return switch (type.asText()) {
            case "object" -> value != null && value.isObject();
            case "array" -> value != null && value.isArray();
            case "string" -> value != null && value.isTextual();
            case "number" -> value != null && value.isNumber();
            case "integer" -> value != null && value.isIntegralNumber();
            case "boolean" -> value != null && value.isBoolean();
            case "null" -> value == null || value.isNull();
            default -> true;
        };
    }

    private static void validateObject(JsonNode schema, JsonNode value, String path, List<String> errors) {
        Set<String> required = new HashSet<>();
        JsonNode requiredNode = schema.get("required");
        if (requiredNode != null && requiredNode.isArray()) {
            requiredNode.forEach(item -> required.add(item.asText()));
        }
        required.stream().filter(name -> !value.has(name))
                .forEach(name -> errors.add(path + "." + name + " is required."));

        JsonNode properties = schema.get("properties");
        if (properties != null && properties.isObject()) {
            for (Map.Entry<String, JsonNode> field : properties.properties()) {
                if (value.has(field.getKey())) {
                    validateNode(field.getValue(), value.get(field.getKey()), path + "." + field.getKey(), errors);
                }
            }
            if (schema.path("additionalProperties").isBoolean()
                    && !schema.path("additionalProperties").asBoolean()) {
                value.fieldNames().forEachRemaining(name -> {
                    if (!properties.has(name)) {
                        errors.add(path + "." + name + " is not allowed.");
                    }
                });
            }
        }
    }

    private static void validateArray(JsonNode schema, JsonNode value, String path, List<String> errors) {
        if (schema.has("minItems") && value.size() < schema.path("minItems").asInt()) {
            errors.add(path + " has fewer items than allowed.");
        }
        if (schema.has("maxItems") && value.size() > schema.path("maxItems").asInt()) {
            errors.add(path + " has more items than allowed.");
        }
        JsonNode itemSchema = schema.get("items");
        if (itemSchema != null) {
            for (int index = 0; index < value.size(); index++) {
                validateNode(itemSchema, value.get(index), path + "[" + index + "]", errors);
            }
        }
    }

    private static void validateNumber(JsonNode schema, JsonNode value, String path, List<String> errors) {
        BigDecimal number = value.decimalValue();
        if (schema.has("minimum") && number.compareTo(schema.path("minimum").decimalValue()) < 0) {
            errors.add(path + " is below the minimum.");
        }
        if (schema.has("maximum") && number.compareTo(schema.path("maximum").decimalValue()) > 0) {
            errors.add(path + " is above the maximum.");
        }
    }
}
