package com.shaft.tools.io;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.tools.io.ReportManager;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Iterator;
import java.util.Scanner;

public class SwaggerManager {
    private static final String SWAGGER_URL = SHAFT.Properties.api.swaggerUrl(); // Change to target API
    private static boolean isSwaggerValidationEnabled = true; // Feature flag

    private JsonNode swaggerJson;
    private boolean isOpenApiV3 = false;

    public SwaggerManager() {
        loadSwaggerSchemaFromUrl();
    }

    private void loadSwaggerSchemaFromUrl() {
        ObjectMapper objectMapper = new ObjectMapper();

        try {
            String schemaJson = fetchSwaggerJson(SWAGGER_URL);
            swaggerJson = objectMapper.readTree(schemaJson);

            // Detect OpenAPI version
            isOpenApiV3 = swaggerJson.has("openapi");
            ReportManager.log("Available paths in Swagger:");
            JsonNode pathsNode = swaggerJson.get("paths");
            if (pathsNode != null) {
                Iterator<String> fieldNames = pathsNode.fieldNames();
                while (fieldNames.hasNext()) {
                    ReportManager.log(" - " + fieldNames.next());
                }
            }

            ReportManager.log("Swagger schema loaded successfully from: " + SWAGGER_URL);
            ReportManager.log("Detected API version: " + (isOpenApiV3 ? "OpenAPI v3" : "Swagger v2"));

        } catch (IOException e) {
            ReportManager.log("Failed to load Swagger schema: " + e.getMessage());
            throw new RuntimeException("Error loading Swagger schema", e);
        }
    }

    private String fetchSwaggerJson(String urlString) throws IOException {
        StringBuilder json = new StringBuilder();
        URL url = new URL(urlString);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("GET");
        conn.setRequestProperty("Accept", "application/json");

        if (conn.getResponseCode() != 200) {
            throw new IOException("Failed to fetch Swagger schema. HTTP error code: " + conn.getResponseCode());
        }

        try (Scanner scanner = new Scanner(conn.getInputStream())) {
            while (scanner.hasNextLine()) {
                json.append(scanner.nextLine());
            }
        }
        conn.disconnect();
        return json.toString();
    }

    public boolean validateSchemaForEndpoint(String originalEndpoint) {
        if (!isSwaggerValidationEnabled) {
            return true; // Skip validation if disabled
        }

        JsonNode schemaNode = getSchemaForEndpoint(originalEndpoint);
        if (schemaNode == null) {
            throw new AssertionError("Swagger schema validation failed: No schema found for endpoint: " + originalEndpoint);
        }

        ReportManager.log("Extracted schema successfully for endpoint: " + originalEndpoint);
        ReportManager.log("Schema validation PASSED for endpoint: " + originalEndpoint);
        return true;
    }

    private JsonNode getSchemaForEndpoint(String endpoint) {
        if (swaggerJson == null) {
            throw new IllegalStateException("Swagger schema is not loaded.");
        }

        // Normalize endpoint before lookup
        String normalizedEndpoint = normalizeEndpoint(endpoint);
        ReportManager.log("Looking for schema for: " + normalizedEndpoint);

        JsonNode pathsNode = swaggerJson.get("paths");
        if (pathsNode != null && pathsNode.has(normalizedEndpoint)) {
            JsonNode endpointData = pathsNode.get(normalizedEndpoint);

            // **Ensure we check "post" method, since /user/createWithList is a POST request**
            if (endpointData.has("post") && endpointData.get("post").has("responses")) {
                JsonNode responsesNode = endpointData.get("post").get("responses");

                // **Check response schema under "200"**
                if (responsesNode.has("200") && responsesNode.get("200").has("content")) {
                    JsonNode schemaNode = extractResponseSchema(responsesNode.get("200").get("content"));
                    if (schemaNode != null) return schemaNode;
                }

                // **Check "default" response schema if "200" is missing**
                if (responsesNode.has("default") && responsesNode.get("default").has("content")) {
                    JsonNode schemaNode = extractResponseSchema(responsesNode.get("default").get("content"));
                    if (schemaNode != null) return schemaNode;
                }

                // **Check for "201 Created" if no 200/default**
                if (responsesNode.has("201")) {
                    ReportManager.log("No 200 response, checking 201.");
                    return responsesNode.get("201").has("content")
                            ? extractResponseSchema(responsesNode.get("201").get("content"))
                            : null;
                }

                // **Handle 204 No Content**
                if (responsesNode.has("204")) {
                    ReportManager.log("No response body expected for 204.");
                    return null; // 204 means no content
                }
            }
        }

        ReportManager.log("No schema found for: " + endpoint);
        return null;
    }


    private JsonNode extractResponseSchema(JsonNode responsesNode) {
        if (responsesNode.has("application/json")) {
            JsonNode schemaNode = responsesNode.get("application/json").get("schema");
            if (schemaNode.has("$ref")) {
                return resolveSchemaReference(schemaNode.get("$ref").asText());
            }
            return schemaNode;
        } else if (responsesNode.has("text/plain")) {
            return responsesNode.get("text/plain").get("schema");
        } else if (responsesNode.has("text/json")) {
            return responsesNode.get("text/json").get("schema");
        }
        return null; // If no schema is found, return null
    }


    private String normalizeEndpoint(String endpoint) {
        JsonNode pathsNode = swaggerJson.get("paths");
        if (pathsNode != null) {
            // **Check if exact match exists**
            if (pathsNode.has(endpoint)) {
                return endpoint;
            }

            // **Check if removing path parameters matches a valid Swagger path**
            String withoutParams = endpoint.replaceAll("/\\{[^}]+}", "").replaceAll("/\\d+", ""); // Remove dynamic IDs
            if (pathsNode.has(withoutParams)) {
                return withoutParams;
            }

            // **Check for equivalent path in Swagger (replace path parameters dynamically)**
            Iterator<String> fieldNames = pathsNode.fieldNames();
            while (fieldNames.hasNext()) {
                String key = fieldNames.next();
                String normalizedKey = key.replaceAll("\\{[^}]+}", "param");
                String normalizedEndpoint = endpoint.replaceAll("/\\d+", "/param");

                if (normalizedKey.equals(normalizedEndpoint)) {
                    return key;
                }
            }
        }
        return endpoint; // Return original if no match found
    }


    private JsonNode resolveSchemaReference(String ref) {
        String refPath = ref.replace("#/components/schemas/", "");
        if (isOpenApiV3 && swaggerJson.has("components") && swaggerJson.get("components").has("schemas")) {
            return swaggerJson.get("components").get("schemas").get(refPath);
        }
        return null;
    }


    public static void setSwaggerValidationEnabled(boolean enabled) {
        isSwaggerValidationEnabled = enabled;
    }
}
