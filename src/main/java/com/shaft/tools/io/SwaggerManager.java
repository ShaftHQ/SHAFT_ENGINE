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
    private static final String SWAGGER_URL = Properties.api.swaggerUrl();  // Dynamic Swagger URL from Properties

    public static Object getOpenAPI() {
        try {
            // Try parsing as OpenAPI 3.x
            return new OpenAPIV3Parser().read(SWAGGER_URL);
        } catch (Exception e) {
            // Fallback to Swagger 2.x
            return new SwaggerParser().read(SWAGGER_URL);
        }
    }

    public static Map<String, ?> getAllEndpoints() {
        Object openAPI = getOpenAPI();

        if (openAPI instanceof OpenAPI) {
            return ((OpenAPI) openAPI).getPaths();
        } else if (openAPI instanceof Swagger) {
            return ((Swagger) openAPI).getPaths();
        }
        return null;
    }

    public static io.swagger.v3.oas.models.media.Schema<?> getResponseSchema(String endpoint, String method, int statusCode) {
        Object openAPI = getOpenAPI(); // Get either OpenAPI 3.x or Swagger 2.x

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
                // Fix: Cast Swagger 2.x schema to OpenAPI 3.x Schema format
                return convertSwaggerSchemaToOpenAPISchema(response.getSchema());
            }
        }

        return null;
    }

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
            openAPISchema.setType("unknown"); // Default case
        }

        return openAPISchema;
    }


}

