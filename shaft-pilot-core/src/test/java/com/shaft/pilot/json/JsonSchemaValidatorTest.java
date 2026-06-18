package com.shaft.pilot.json;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class JsonSchemaValidatorTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void acceptsValidObjectWithArrayAndNumberRules() throws Exception {
        JsonNode schema = JSON.readTree("""
                {
                  "type": "object",
                  "required": ["name", "scores"],
                  "additionalProperties": false,
                  "properties": {
                    "name": {"type": ["string", "null"], "enum": ["Sam", null]},
                    "scores": {
                      "type": "array",
                      "minItems": 1,
                      "maxItems": 2,
                      "items": {"type": "number", "minimum": 0, "maximum": 10}
                    }
                  }
                }
                """);
        JsonNode payload = JSON.readTree("{\"name\":\"Sam\",\"scores\":[3,10]}");

        assertTrue(JsonSchemaValidator.validate(schema, payload).isEmpty());
    }

    @Test
    void reportsRequiredAdditionalArrayNumberAndEnumErrors() throws Exception {
        JsonNode schema = JSON.readTree("""
                {
                  "type": "object",
                  "required": ["name", "scores"],
                  "additionalProperties": false,
                  "properties": {
                    "name": {"type": "string", "enum": ["Sam"]},
                    "scores": {
                      "type": "array",
                      "minItems": 1,
                      "maxItems": 2,
                      "items": {"type": "integer", "minimum": 0, "maximum": 10}
                    }
                  }
                }
                """);
        JsonNode payload = JSON.readTree("{\"name\":\"Ana\",\"scores\":[-1,11,1.5],\"extra\":true}");

        List<String> errors = JsonSchemaValidator.validate(schema, payload);

        assertEquals(List.of(
                "$.name is not an allowed enum value.",
                "$.scores has more items than allowed.",
                "$.scores[0] is below the minimum.",
                "$.scores[1] is above the maximum.",
                "$.scores[2] does not match the required type.",
                "$.extra is not allowed."), errors);
    }

    @Test
    void reportsMissingRequiredAndRootTypeMismatch() throws Exception {
        JsonNode schema = JSON.readTree("""
                {
                  "type": "object",
                  "required": ["name"],
                  "properties": {"name": {"type": "string"}}
                }
                """);

        assertEquals(List.of("$.name is required."),
                JsonSchemaValidator.validate(schema, JSON.readTree("{}")));
        assertEquals(List.of("$ does not match the required type."),
                JsonSchemaValidator.validate(schema, JSON.readTree("[]")));
    }

    @Test
    void ignoresUnsupportedOrMissingSchema() throws Exception {
        assertTrue(JsonSchemaValidator.validate(null, JSON.readTree("{}")).isEmpty());
        assertTrue(JsonSchemaValidator.validate(JSON.readTree("\"schema\""), JSON.readTree("{}")).isEmpty());
        assertTrue(JsonSchemaValidator.validate(JSON.readTree("{\"type\":\"custom\"}"), JSON.readTree("1")).isEmpty());
    }

    @Test
    void constructorRejectsInstantiation() throws Exception {
        Constructor<JsonSchemaValidator> constructor = JsonSchemaValidator.class.getDeclaredConstructor();
        constructor.setAccessible(true);

        InvocationTargetException thrown = assertThrows(InvocationTargetException.class, constructor::newInstance);

        assertEquals(IllegalStateException.class, thrown.getCause().getClass());
        assertEquals("Utility class", thrown.getCause().getMessage());
    }
}
