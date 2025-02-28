package com.shaft.tools.io;

import com.shaft.properties.internal.Properties;
import io.swagger.models.HttpMethod;
import io.swagger.parser.SwaggerParser;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.parser.OpenAPIV3Parser;
import io.swagger.models.Swagger;
import io.swagger.models.Path;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;

import java.util.Map;

public class SwaggerManager {
    private static final String SWAGGER_URL = Properties.api.swaggerUrl();

    // Prevent instantiation
    private SwaggerManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Loads the OpenAPI specification dynamically (supports both OpenAPI 3.x & Swagger 2.x).
     *
     * @return Parsed OpenAPI/Swagger object.
     */
    public static Object getOpenAPI() {
        ReportManager.log("Fetching OpenAPI specification from: " + SWAGGER_URL);
        try {
            return new OpenAPIV3Parser().read(SWAGGER_URL);
        } catch (Exception e) {
            ReportManager.logDiscrete("Failed to parse as OpenAPI 3.x, trying Swagger 2.x...");
            return new SwaggerParser().read(SWAGGER_URL);
        }
    }

    /**
     * Retrieves all available API endpoints.
     *
     * @return Map containing API paths.
     */
    public static Map<String, ?> getAllEndpoints() {
        Object openAPI = getOpenAPI();

        if (openAPI instanceof OpenAPI openAPI3) {
            return openAPI3.getPaths();
        } else if (openAPI instanceof Swagger swagger) {
            return swagger.getPaths();
        }

        ReportManager.logDiscrete("⚠ No endpoints found in the API specification.");
        return null;
    }

    /**
     * Fetches the response schema for a given API endpoint, method, and status code.
     *
     * @param endpoint    API endpoint (e.g., "/users/{id}").
     * @param method      HTTP method (e.g., "GET").
     * @param statusCode  Expected response status code (e.g., 200).
     * @return The corresponding OpenAPI 3.x schema.
     */
    public static io.swagger.v3.oas.models.media.Schema<?> getResponseSchema(String endpoint, String method, int statusCode) {
        Object openAPI = getOpenAPI();
        ReportManager.log("Fetching response schema for: " + method + " " + endpoint + " [" + statusCode + "]");

        if (openAPI instanceof OpenAPI openAPI3) { // OpenAPI 3.x
            PathItem pathItem = openAPI3.getPaths().get(endpoint);
            if (pathItem == null) return null;

            Operation operation = pathItem.readOperationsMap().get(PathItem.HttpMethod.valueOf(method.toUpperCase()));
            if (operation == null || operation.getResponses() == null) return null;

            ApiResponse response = operation.getResponses().get(String.valueOf(statusCode));
            if (response != null && response.getContent() != null) {
                MediaType mediaType = response.getContent().get("application/json");
                return mediaType != null ? mediaType.getSchema() : null;
            }
        }

        else if (openAPI instanceof Swagger swagger) { // Swagger 2.x
            Path path = swagger.getPath(endpoint);
            if (path == null) return null;

            io.swagger.models.Operation operation = path.getOperationMap().get(HttpMethod.valueOf(method.toUpperCase()));
            if (operation == null || operation.getResponses() == null) return null;

            io.swagger.models.Response response = operation.getResponses().get(String.valueOf(statusCode));
            if (response != null && response.getSchema() != null) {
                return convertSwaggerSchemaToOpenAPISchema(response.getSchema());
            }
        }

        ReportManager.logDiscrete("⚠ No schema found for: " + method + " " + endpoint + " [" + statusCode + "]");
        return null;
    }

    /**
     * Converts Swagger 2.x schema to OpenAPI 3.x format.
     *
     * @param property Swagger 2.x property.
     * @return Converted OpenAPI 3.x schema.
     */
    private static io.swagger.v3.oas.models.media.Schema<?> convertSwaggerSchemaToOpenAPISchema(io.swagger.models.properties.Property property) {
        if (property == null) return null;

        io.swagger.v3.oas.models.media.Schema<Object> openAPISchema = new io.swagger.v3.oas.models.media.Schema<>();

        if (property instanceof io.swagger.models.properties.StringProperty) {
            openAPISchema.setType("string");
        } else if (property instanceof io.swagger.models.properties.IntegerProperty) {
            openAPISchema.setType("integer");
        } else if (property instanceof io.swagger.models.properties.BooleanProperty) {
            openAPISchema.setType("boolean");
        } else if (property instanceof io.swagger.models.properties.ArrayProperty arrayProperty) {
            io.swagger.v3.oas.models.media.ArraySchema arraySchema = new io.swagger.v3.oas.models.media.ArraySchema();
            arraySchema.setItems(convertSwaggerSchemaToOpenAPISchema(arrayProperty.getItems()));
            return arraySchema;
        } else if (property instanceof io.swagger.models.properties.ObjectProperty) {
            openAPISchema.setType("object");
        } else {
            openAPISchema.setType("unknown");
        }

        return openAPISchema;
    }
}
