package com.shaft.tools.io;


import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.tools.io.ReportManager;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Iterator;
import java.util.Scanner;

public class OpenApiV3Manager {
    private static final String SWAGGER_URL = "https://fakerestapi.azurewebsites.net/swagger/v1/swagger.json"; // OpenAPI v3 URL
    private JsonNode swaggerJson;

    public OpenApiV3Manager(String swaggerUrl) {  // Accepts Swagger URL
        loadSwaggerSchema(swaggerUrl);
    }

    private void loadSwaggerSchema(String swaggerUrl) {
        ObjectMapper objectMapper = new ObjectMapper();
        try {
            String schemaJson = fetchSwaggerJson(swaggerUrl);
            swaggerJson = objectMapper.readTree(schemaJson);
            ReportManager.log("Loaded OpenAPI v3 schema successfully.");
        } catch (IOException e) {
            throw new RuntimeException("Failed to load OpenAPI v3 schema", e);
        }
    }

    private String fetchSwaggerJson(String urlString) throws IOException {
        StringBuilder json = new StringBuilder();
        URL url = new URL(urlString);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("GET");
        conn.setRequestProperty("Accept", "application/json");

        if (conn.getResponseCode() != 200) {
            throw new IOException("Failed to fetch OpenAPI v3 schema. HTTP error: " + conn.getResponseCode());
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
            throw new IllegalStateException("OpenAPI v3 schema is not loaded.");
        }

        JsonNode pathsNode = swaggerJson.get("paths");
        if (pathsNode != null && pathsNode.has(endpoint)) {
            JsonNode endpointSchema = pathsNode.get(endpoint).get("post").get("responses").get("200").get("content").get("application/json").get("schema");
            if (endpointSchema != null) {
                return resolveSchemaReference(endpointSchema);
            }
        }

        ReportManager.log("No schema found for OpenAPI v3 endpoint: " + endpoint);
        return null;
    }

    private JsonNode resolveSchemaReference(JsonNode schemaNode) {
        if (schemaNode.has("$ref")) {
            String ref = schemaNode.get("$ref").asText().replace("#/components/schemas/", "");
            return swaggerJson.get("components").get("schemas").get(ref);
        }
        return schemaNode;
    }
}
