package com.shaft.tools.io;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import io.swagger.parser.SwaggerParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.OpenAPIV3Parser;
import io.swagger.models.Swagger;
import io.swagger.models.Path;
import io.swagger.models.HttpMethod;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Map;

public class SwaggerManager {
    private static final String DEFAULT_SWAGGER_URL = "https://fakerestapi.azurewebsites.net/swagger/v1/swagger.json";
    private static String schemaSource;
    private static boolean enableValidation;
    private static boolean strictValidation;
    private static final boolean enableDetailedLogs = Boolean.parseBoolean(System.getProperty("swagger.enableDetailedLogs", "false"));

    private static void log(String message) {
        if (enableDetailedLogs) {
            ReportManager.log(message);  // Assuming ReportManager handles logging
        }
    }

    static {
        schemaSource = System.getProperty("swagger.schemaSource", DEFAULT_SWAGGER_URL);
        enableValidation = Boolean.parseBoolean(System.getProperty("swagger.enableValidation", "true"));
        strictValidation = Boolean.parseBoolean(System.getProperty("swagger.strictValidation", "true"));
    }

    private SwaggerManager() {
        throw new IllegalStateException("Utility class");
    }

    public static Object getOpenAPI() {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            JsonNode rootNode;

            if (new File(schemaSource).exists()) {
                rootNode = objectMapper.readTree(new File(schemaSource));
            } else {
                rootNode = objectMapper.readTree(new URL(schemaSource));
            }

            if (rootNode.has("openapi")) {
                return new OpenAPIV3Parser().read(schemaSource);
            } else if (rootNode.has("swagger")) {
                return new SwaggerParser().read(schemaSource);
            }
        } catch (Exception e) {
            if (enableDetailedLogs) {
                SHAFT.Report.log("[ERROR] ❌ Failed to load Swagger Schema: " + e.getMessage());
            }
        }
        return null;
    }

    public static JsonNode getResponseSchema(String endpoint, String method, int statusCode) {
        if (!enableValidation) {
            return null;
        }

        Object apiSpec = getOpenAPI();
        if (apiSpec == null) return null;

        if (!validateEndpointExistence(endpoint, method)) {
            SHAFT.Report.log("❌ API endpoint `" + endpoint + "` does NOT exist in Swagger!");
            return null;
        }

        return resolveSchemaForMethod(apiSpec, endpoint, method, statusCode);
    }

    private static boolean validateEndpointExistence(String actualPath, String method) {
        Object openAPI = getOpenAPI();
        if (openAPI == null) return false;

        Map<String, ?> swaggerEndpoints = getAllEndpoints();
        if (swaggerEndpoints == null) return false;

        for (String swaggerPath : swaggerEndpoints.keySet()) {
            String regexPath = swaggerPath.replaceAll("\\{.*?}", "[^/]+");
            if (actualPath.matches(regexPath)) {
                if (enableDetailedLogs) {
                    SHAFT.Report.log("[INFO] ✅ Matched API Path: " + actualPath + " with Swagger path: " + swaggerPath);
                }
                return true;
            }
        }

        return false;
    }

    private static Map<String, ?> getAllEndpoints() {
        Object openAPI = getOpenAPI();
        if (openAPI instanceof OpenAPI openAPI3) {
            return openAPI3.getPaths();
        } else if (openAPI instanceof Swagger swagger) {
            return swagger.getPaths();
        }
        return null;
    }

    private static JsonNode resolveSchemaForMethod(Object apiSpec, String endpoint, String method, int statusCode) {
        if (apiSpec instanceof OpenAPI openAPI3) {
            PathItem pathItem = openAPI3.getPaths().get(endpoint);
            if (pathItem == null) return null;

            var operation = pathItem.readOperationsMap().get(PathItem.HttpMethod.valueOf(method.toUpperCase()));
            if (operation == null || operation.getResponses() == null) return null;

            ApiResponse response = operation.getResponses().get(String.valueOf(statusCode));
            if (response != null && response.getContent() != null) {
                MediaType mediaType = response.getContent().get("application/json");
                if (mediaType != null && mediaType.getSchema() != null) {
                    return resolveSchemaReference(mediaType.getSchema(), openAPI3);
                }
            }
        }

        return null;
    }

    private static JsonNode resolveSchemaReference(Object schema, Object apiSpec) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();
            JsonNode schemaNode = objectMapper.readTree(objectMapper.writeValueAsString(schema));

            if (schemaNode.has("$ref")) {
                String refPath = schemaNode.get("$ref").asText();
                String schemaName = refPath.replace("#/components/schemas/", "").replace("#/definitions/", "");

                JsonNode resolvedSchema = null;

                if (apiSpec instanceof OpenAPI openAPI3) {
                    resolvedSchema = objectMapper.readTree(objectMapper.writeValueAsString(
                            openAPI3.getComponents().getSchemas().getOrDefault(schemaName, null)
                    ));
                }

                if (resolvedSchema == null) {
                    if (enableDetailedLogs) {
                        SHAFT.Report.log("[ERROR] ❌ Missing Schema Reference: `" + refPath + "` in Swagger.");
                    }
                    return null;
                }

                return resolvedSchema;
            }
            return schemaNode;
        } catch (IOException e) {
            return null;
        }
    }
}
