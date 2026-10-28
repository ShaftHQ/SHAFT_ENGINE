package io.restassured.module.jsv;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

/**
 * Coverage for the Jackson 2→3 bridge that lets networknt 3.x run under REST Assured 6.3.0 (#7423).
 */
public class NetworkntSchemaValidationTest {

    private static final ObjectMapper JACKSON2 = new ObjectMapper();

    @Test
    public void validateAcceptsDraft07SchemaWithJackson2Nodes() throws Exception {
        var schema = JACKSON2.readTree("""
                {
                  "$schema":"http://json-schema.org/draft-07/schema#",
                  "type":"object",
                  "required":["id"],
                  "properties":{"id":{"type":"integer"}}
                }
                """);
        var valid = JACKSON2.readTree("{\"id\":1}");
        var invalid = JACKSON2.readTree("{\"id\":\"x\"}");

        List<String> ok = NetworkntSchemaValidation.validate(JsonSchemaVersion.V7, schema, null, valid);
        Assert.assertTrue(ok.isEmpty(), ok.toString());

        List<String> bad = NetworkntSchemaValidation.validate(JsonSchemaVersion.V7, schema, null, invalid);
        Assert.assertFalse(bad.isEmpty(), "expected type error for id");
    }

    @Test
    public void validateWithSchemaLocationResolvesAbsoluteFileUri() throws Exception {
        var schema = JACKSON2.readTree("""
                {"$schema":"http://json-schema.org/draft-07/schema#","type":"string"}
                """);
        var content = JACKSON2.readTree("\"ok\"");
        List<String> messages = NetworkntSchemaValidation.validate(
                JsonSchemaVersion.V7, schema, "https://example.invalid/schema.json", content);
        Assert.assertTrue(messages.isEmpty(), messages.toString());
    }
}
