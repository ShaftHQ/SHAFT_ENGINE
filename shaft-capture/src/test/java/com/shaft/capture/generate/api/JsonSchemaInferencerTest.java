package com.shaft.capture.generate.api;

import io.restassured.module.jsv.JsonSchemaValidator;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class JsonSchemaInferencerTest {

    @Test
    void blankOrMalformedSampleYieldsEmptyObjectSchema() {
        assertEquals("{}", JsonSchemaInferencer.infer(null));
        assertEquals("{}", JsonSchemaInferencer.infer(""));
        assertEquals("{}", JsonSchemaInferencer.infer("not json"));
    }

    @Test
    void inferredSchemaActuallyValidatesTheSampleItWasInferredFrom() {
        // The real assertion: hand the inferred schema to REST-Assured's own JSON Schema
        // Validator (the same one SHAFT.API's matchesSchema() uses) and confirm it accepts the
        // exact sample it was inferred from -- not just that our code runs without throwing.
        String sample = "{\"id\":\"abc-123\",\"count\":3,\"active\":true,\"tags\":[\"a\",\"b\"],"
                + "\"nested\":{\"name\":\"Ada\"}}";

        String schema = JsonSchemaInferencer.infer(sample);

        assertThat(sample, JsonSchemaValidator.matchesJsonSchema(schema));
    }

    @Test
    void inferredSchemaRejectsASampleMissingARequiredField() {
        String schema = JsonSchemaInferencer.infer("{\"id\":\"abc-123\",\"count\":3}");

        assertThat("{\"id\":\"abc-123\"}", org.hamcrest.core.IsNot.not(JsonSchemaValidator.matchesJsonSchema(schema)));
    }

    @Test
    void inferredSchemaRejectsWrongType() {
        String schema = JsonSchemaInferencer.infer("{\"count\":3}");

        assertThat("{\"count\":\"not-a-number\"}",
                org.hamcrest.core.IsNot.not(JsonSchemaValidator.matchesJsonSchema(schema)));
    }

    @Test
    void emptyArrayInfersAPermissiveItemsSchema() {
        String schema = JsonSchemaInferencer.infer("{\"items\":[]}");

        assertThat(schema, org.hamcrest.core.StringContains.containsString("\"array\""));
    }

    @Test
    void everyObjectKeyIsMarkedRequiredSinceOnlyOneSampleWasObserved() {
        String schema = JsonSchemaInferencer.infer("{\"a\":1,\"b\":2}");

        assertTrue(schema.contains("\"a\""));
        assertTrue(schema.contains("\"b\""));
        assertTrue(schema.contains("\"required\""));
    }
}
