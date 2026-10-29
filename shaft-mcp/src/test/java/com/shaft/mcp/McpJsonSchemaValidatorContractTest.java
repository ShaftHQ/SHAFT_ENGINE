package com.shaft.mcp;

import com.networknt.schema.Schema;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.JsonNode;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * The live MCP server validates tool schemas with
 * {@code Schema.validate(tools.jackson.databind.JsonNode)}. networknt 2.0.8
 * does not have that method, and the stdio process then exits 1.
 *
 * <p>#7483: shaft-engine still brings Atlassian openapi-request-validator and
 * REST Assured json-schema-validator, which call the removed Jackson-2
 * {@code SchemaRegistry.getSchema(JsonNode)} overload. Those artifacts are
 * excluded from the mcp classpath so a latent {@code NoSuchMethodError} cannot
 * fire if swagger validation or {@code matchesJsonSchema} were ever enabled here.
 */
class McpJsonSchemaValidatorContractTest {
    @Test
    void schemaValidatesTheJackson3NodeTheMcpServerSends() throws Exception {
        var method = Schema.class.getMethod("validate", JsonNode.class);
        assertEquals(List.class, method.getReturnType());
    }

    @Test
    void openApiRequestValidatorIsNotOnTheMcpClasspath() {
        assertThrows(ClassNotFoundException.class,
                () -> Class.forName("com.atlassian.oai.validator.restassured.OpenApiValidationFilter"));
        assertThrows(ClassNotFoundException.class,
                () -> Class.forName("com.atlassian.oai.validator.interaction.ApiOperationResolver"));
    }

    @Test
    void restAssuredJsonSchemaValidatorIsNotOnTheMcpClasspath() {
        assertThrows(ClassNotFoundException.class,
                () -> Class.forName("io.restassured.module.jsv.JsonSchemaValidator"));
    }
}
