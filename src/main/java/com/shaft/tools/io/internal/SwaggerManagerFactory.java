package com.shaft.tools.io.internal;

import com.fasterxml.jackson.databind.JsonNode;
import com.shaft.tools.io.OpenApiV3Manager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.SwaggerV2Manager;

public class SwaggerManagerFactory {
    private SwaggerV2Manager swaggerV2Manager;
    private OpenApiV3Manager openApiV3Manager;
    private boolean isOpenApiV3;

    public SwaggerManagerFactory(String swaggerVersion, String swaggerUrl) {
        if (swaggerVersion.equalsIgnoreCase("v3")) {
            openApiV3Manager = new OpenApiV3Manager(swaggerUrl);
            isOpenApiV3 = true;
        } else {
            swaggerV2Manager = new SwaggerV2Manager(swaggerUrl);
            isOpenApiV3 = false;
        }
    }

    public JsonNode getSchemaForEndpoint(String endpoint) {
        if (isOpenApiV3) {
            return openApiV3Manager.getSchemaForEndpoint(endpoint);
        } else {
            return swaggerV2Manager.getSchemaForEndpoint(endpoint);
        }
    }
}
