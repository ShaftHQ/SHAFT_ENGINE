package com.shaft.tools.io;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.tools.io.ReportManager;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Scanner;

public class SwaggerV2Manager {
    private static final String SWAGGER_URL = "https://petstore.swagger.io/v2/swagger.json"; // Swagger v2 URL
    private JsonNode swaggerJson;

    public SwaggerV2Manager(String swaggerUrl) {  // Accepts Swagger URL
        loadSwaggerSchema(swaggerUrl);
    }

    private void loadSwaggerSchema(String swaggerUrl) {
        ObjectMapper objectMapper = new ObjectMapper();
        try {
            String schemaJson = fetchSwaggerJson(swaggerUrl);
            swaggerJson = objectMapper.readTree(schemaJson);
            ReportManager.log("Loaded Swagger v2 schema successfully.");
        } catch (IOException e) {
            throw new RuntimeException("Failed to load Swagger v2 schema", e);
        }
    }

    private String fetchSwaggerJson(String urlString) throws IOException {
        StringBuilder json = new StringBuilder();
        URL url = new URL(urlString);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("GET");
        conn.setRequestProperty("Accept", "application/json");

        if (conn.getResponseCode() != 200) {
            throw new IOException("Failed to fetch Swagger schema. HTTP error: " + conn.getResponseCode());
        }

        try (Scanner scanner = new Scanner(conn.getInputStream())) {
            while (scanner.hasNextLine()) {
                json.append(scanner.nextLine());
            }
        }
        conn.disconnect();
        return json.toString();
    }

    public JsonNode getSchemaForEndpoint(String endpoint) {
        if (swaggerJson == null) {
            throw new IllegalStateException("Swagger v2 schema is not loaded.");
        }

        JsonNode pathsNode = swaggerJson.get("paths");
        if (pathsNode != null && pathsNode.has(endpoint)) {
            JsonNode endpointData = pathsNode.get(endpoint);

            if (endpointData.has("post") && endpointData.get("post").has("responses")) {
                JsonNode responsesNode = endpointData.get("post").get("responses");

                // **Check for "200" response first**
                if (responsesNode.has("200")) {
                    JsonNode schemaNode = extractResponseSchema(responsesNode.get("200"));
                    if (schemaNode != null) {
                        return schemaNode;
                    }
                }

                // **Fallback: Check for "default" response**
                if (responsesNode.has("default")) {
                    JsonNode schemaNode = extractResponseSchema(responsesNode.get("default"));
                    if (schemaNode != null) {
                        return schemaNode;
                    }
                }
            }
        }

        ReportManager.log("No schema found for Swagger v2 endpoint: " + endpoint);
        return null;
    }

    private JsonNode extractResponseSchema(JsonNode responseNode) {
        if (responseNode.has("schema")) {
            JsonNode schemaNode = responseNode.get("schema");
            if (schemaNode.has("$ref")) {
                return resolveSchemaReference(schemaNode.get("$ref").asText());
            }
            return schemaNode;
        }
        return null;
    }

    private JsonNode resolveSchemaReference(String ref) {
        String refPath = ref.replace("#/definitions/", "");
        if (swaggerJson.has("definitions") && swaggerJson.get("definitions").has(refPath)) {
            return swaggerJson.get("definitions").get(refPath);
        }
        return null;
    }
}
