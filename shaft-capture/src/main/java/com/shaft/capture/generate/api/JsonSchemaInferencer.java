package com.shaft.capture.generate.api;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ObjectNode;

/**
 * Infers a standard (draft-04-compatible) JSON Schema document from one recorded sample response
 * body, for use with {@code SHAFT.API}'s {@code assertThatResponse().matchesSchema(path)}, which
 * is backed by REST-Assured's {@code io.restassured.module.jsv.JsonSchemaValidator}.
 *
 * <p>Since a schema is inferred from exactly one recorded sample, every key observed on an object
 * is marked {@code required} -- this is deliberately strict (a real endpoint may omit optional
 * keys on other calls) but matches what was actually recorded; nothing here invents fields that
 * were never observed.
 */
public final class JsonSchemaInferencer {
    private static final ObjectMapper MAPPER = new JsonMapper();

    private JsonSchemaInferencer() {
    }

    /**
     * Infers a JSON Schema document from a sample JSON body.
     *
     * @param sampleJson recorded response body text
     * @return pretty-printed JSON Schema text, or an empty-object schema ({@code "{}"}) if the
     *         sample is blank or not valid JSON
     */
    public static String infer(String sampleJson) {
        if (sampleJson == null || sampleJson.isBlank()) {
            return "{}";
        }
        JsonNode sample;
        try {
            sample = MAPPER.readTree(sampleJson);
        } catch (RuntimeException malformed) {
            return "{}";
        }
        ObjectNode schema = schemaFor(sample);
        return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(schema);
    }

    private static ObjectNode schemaFor(JsonNode node) {
        ObjectNode schema = MAPPER.createObjectNode();
        if (node == null || node.isNull() || node.isMissingNode()) {
            schema.put("type", "null");
            return schema;
        }
        if (node.isObject()) {
            schema.put("type", "object");
            ObjectNode properties = MAPPER.createObjectNode();
            var required = MAPPER.createArrayNode();
            node.forEachEntry((key, value) -> {
                properties.set(key, schemaFor(value));
                required.add(key);
            });
            schema.set("properties", properties);
            schema.set("required", required);
            return schema;
        }
        if (node.isArray()) {
            schema.put("type", "array");
            schema.set("items", node.isEmpty() ? MAPPER.createObjectNode() : schemaFor(node.get(0)));
            return schema;
        }
        if (node.isIntegralNumber()) {
            schema.put("type", "integer");
            return schema;
        }
        if (node.isNumber()) {
            schema.put("type", "number");
            return schema;
        }
        if (node.isBoolean()) {
            schema.put("type", "boolean");
            return schema;
        }
        schema.put("type", "string");
        return schema;
    }
}
