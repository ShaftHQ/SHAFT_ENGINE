package com.shaft.api;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.net.URL;

public class SwaggerSchemaExtractor {

    private static final String SWAGGER_JSON_URL = "https://petstore.swagger.io/v2/swagger.json";
    private static final String SCHEMA_NAME = "User"; // Name of the schema to extract

    public static void main(String[] args) {
        try {
            // Fetch and parse Swagger JSON
            ObjectMapper objectMapper = new ObjectMapper();
            JsonNode rootNode = objectMapper.readTree(new URL(SWAGGER_JSON_URL));

            // Navigate to definitions -> User schema
            JsonNode userSchema = rootNode.path("definitions").path(SCHEMA_NAME);

            if (!userSchema.isMissingNode()) {
                // Print the extracted schema as formatted JSON
                System.out.println("Extracted User Schema:");
                System.out.println(objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(userSchema));
            } else {
                System.out.println("❌ ERROR: 'User' schema not found in Swagger JSON.");
            }

        } catch (IOException e) {
            System.out.println("❌ ERROR: Failed to fetch or parse Swagger JSON: " + e.getMessage());
        }
    }
}

