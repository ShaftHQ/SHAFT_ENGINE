package com.shaft.mcp;

import com.networknt.schema.Schema;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.JsonNode;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The live MCP server validates tool schemas with
 * {@code Schema.validate(tools.jackson.databind.JsonNode)}. networknt 2.0.8
 * does not have that method, and the stdio process then exits 1.
 */
class McpJsonSchemaValidatorContractTest {
    @Test
    void schemaValidatesTheJackson3NodeTheMcpServerSends() throws Exception {
        var method = Schema.class.getMethod("validate", JsonNode.class);
        assertEquals(List.class, method.getReturnType());
    }
}
