package com.shaft.tools.io;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.validation.Validations;
import io.swagger.parser.SwaggerParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.OpenAPIV3Parser;
import io.swagger.models.HttpMethod;
import io.swagger.models.Swagger;
import io.swagger.models.Path;
import java.io.IOException;
import java.net.URL;
import java.util.Map;

public class SwaggerManager {
    private static final String SWAGGER_JSON_URL = "https://petstore.swagger.io/v2/swagger.json";

    private SwaggerManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Loads the OpenAPI/Swagger specification dynamically.
     * Supports both OpenAPI 3.x & Swagger 2.x.
     *
     * @return Parsed OpenAPI/Swagger object.
     */
    public static Object getOpenAPI() {
        try {
            return new OpenAPIV3Parser().read(SWAGGER_JSON_URL);
        } catch (Exception e) {
            SHAFT.Report.log("⚠ Failed to load OpenAPI 3.x, trying Swagger 2.x...");
            return new SwaggerParser().read(SWAGGER_JSON_URL);
        }
    }

    /**
     * Fetches the response schema for an API endpoint and method.
     *
     * @param endpoint   The actual API endpoint hit (e.g., "/user/user1").
     * @param method     The HTTP method (e.g., "GET").
     * @param statusCode The expected response status code (e.g., 200).
     * @return Extracted schema as JSON.
     */
    public static JsonNode getResponseSchema(String endpoint, String method, int statusCode) {
        Object openAPI = getOpenAPI();
        String swaggerPath = findMatchingSwaggerEndpoint(endpoint);
        if (swaggerPath == null) {
            SHAFT.Report.log("❌ ERROR: API endpoint `" + endpoint + "` does not match any Swagger-defined endpoints!");
            return null;
        }

        // Handle OpenAPI 3.x
        if (openAPI instanceof OpenAPI openAPI3) {
            PathItem pathItem = openAPI3.getPaths().get(swaggerPath);
            if (pathItem == null) return null;

            var operation = pathItem.readOperationsMap().get(PathItem.HttpMethod.valueOf(method.toUpperCase()));
            if (operation == null || operation.getResponses() == null) return null;

            ApiResponse response = operation.getResponses().get(String.valueOf(statusCode));
            if (response != null && response.getContent() != null) {
                MediaType mediaType = response.getContent().get("application/json");
                if (mediaType != null && mediaType.getSchema() != null) {
                    return resolveSchemaReference(mediaType.getSchema());
                }
            }
        }

        SHAFT.Report.log("⚠ No schema found for: " + method + " " + swaggerPath + " [" + statusCode + "]");
        return null;
    }

    /**
     * Resolves `$ref` in JSON schemas by fetching the actual definition from `components/schemas`.
     *
     * @param schema OpenAPI 3.x schema object.
     * @return Fully resolved JSON schema.
     */
    private static JsonNode resolveSchemaReference(io.swagger.v3.oas.models.media.Schema<?> schema) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            JsonNode schemaNode = objectMapper.readTree(objectMapper.writeValueAsString(schema));

            if (schemaNode.has("$ref")) {
                String refPath = schemaNode.get("$ref").asText(); // e.g., "#/components/schemas/User"
                String schemaName = refPath.replace("#/components/schemas/", ""); // Extract "User"

                OpenAPI openAPI = (OpenAPI) getOpenAPI();
                if (openAPI.getComponents().getSchemas().containsKey(schemaName)) {
                    return objectMapper.readTree(objectMapper.writeValueAsString(openAPI.getComponents().getSchemas().get(schemaName)));
                } else {
                    SHAFT.Report.log("❌ ERROR: Schema reference `" + refPath + "` not found in OpenAPI specification.");
                }
            }
            return schemaNode;
        } catch (IOException e) {
            SHAFT.Report.log("⚠ Failed to resolve schema reference: " + e.getMessage());
            return null;
        }
    }


    /**
     * Finds the correct Swagger endpoint by replacing path parameters dynamically.
     *
     * @param actualPath The API path used in the test (e.g., "/user/user1").
     * @return The correct path from Swagger (e.g., "/user/{username}"), or null if not found.
     */
    private static String findMatchingSwaggerEndpoint(String actualPath) {
        Map<String, ?> swaggerEndpoints = getAllEndpoints();
        if (swaggerEndpoints == null) return null;

        for (String swaggerPath : swaggerEndpoints.keySet()) {
            // Convert "/user/{username}" into "/user/.*" and compare with actualPath
            String regexPath = swaggerPath.replaceAll("\\{.*?}", "[^/]+");
            if (actualPath.matches(regexPath)) {
                SHAFT.Report.log("✅ Matched actual API path: " + actualPath + " with Swagger path: " + swaggerPath);
                return swaggerPath;
            }
        }
        return null;
    }

    /**
     * Retrieves all available API endpoints.
     *
     * @return Map containing API paths.
     */
    private static Map<String, ?> getAllEndpoints() {
        Object openAPI = getOpenAPI();
        if (openAPI instanceof OpenAPI openAPI3) {
            return openAPI3.getPaths();
        } else if (openAPI instanceof Swagger swagger) {
            return swagger.getPaths();
        }
        return null;
    }

    /**
     * Converts a Swagger/OpenAPI schema to a JSON node.
     *
     * @param schema OpenAPI 3.x Schema object.
     * @return JSON representation of the schema.
     */
    private static JsonNode convertSchemaToJsonNode(Object schema) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            return objectMapper.readTree(objectMapper.writeValueAsString(schema));
        } catch (IOException e) {
            SHAFT.Report.log("⚠ Failed to convert schema to JSON: " + e.getMessage());
            return null;
        }
    }
}
