package com.shaft.api.internal;

import com.atlassian.oai.validator.report.ValidationReport;
import com.atlassian.oai.validator.restassured.OpenApiValidationFilter;
import com.shaft.api.RestActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.sun.net.httpserver.HttpServer;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertTrue;

public class OpenApiCoverageReporterUnitTest {
    private Path specFile;

    @AfterMethod(alwaysRun = true)
    public void afterMethod() throws IOException {
        OpenApiCoverageReporter.reset();
        Properties.clearForCurrentThread();
        if (specFile != null) {
            Files.deleteIfExists(specFile);
        }
    }

    @Test(description = "OpenAPI coverage summary groups exercised, untested, unmatched, and validation failures")
    public void testOpenApiCoverageSummary() throws IOException {
        String spec = writeSpec();
        OpenApiCoverageReporter.start(spec, 80);

        OpenApiCoverageReporter.recordInteraction(spec, RestActions.RequestType.GET, "/pets/7", validationFailure());
        OpenApiCoverageReporter.recordInteraction(spec, RestActions.RequestType.DELETE, "/missing", null);

        String summary = OpenApiCoverageReporter.buildSummaryText();
        AssertionError thresholdFailure = OpenApiCoverageReporter.thresholdFailure();

        assertTrue(summary.contains("Coverage: 1/3 operations (33.33%)"), summary);
        assertTrue(summary.contains("Pets"), summary);
        assertTrue(summary.contains("GET /pets/{petId} (getPet) hits=1 validationFailures=1"), summary);
        assertTrue(summary.contains("GET /pets (listPets)"), summary);
        assertTrue(summary.contains("POST /orders (createOrder)"), summary);
        assertTrue(summary.contains("DELETE /missing hits=1"), summary);
        assertNotNull(thresholdFailure);
        assertTrue(thresholdFailure.getMessage().contains("33.33% is below the configured 80% threshold"),
                thresholdFailure.getMessage());
    }

    @Test(description = "OpenAPI coverage fails fast when enabled without a configured spec")
    public void testOpenApiCoverageRequiresSpecUrl() {
        IllegalArgumentException exception = Assert.expectThrows(IllegalArgumentException.class,
                () -> OpenApiCoverageReporter.start("", 0));

        assertTrue(exception.getMessage().contains("swagger.validation.url"), exception.getMessage());
    }

    @Test(description = "OpenAPI coverage threshold rejects raw property values outside 0..100")
    public void testOpenApiCoverageThresholdBounds() throws IOException {
        String spec = writeSpec();

        Assert.assertThrows(IllegalArgumentException.class, () -> OpenApiCoverageReporter.start(spec, -1));
        Assert.assertThrows(IllegalArgumentException.class, () -> OpenApiCoverageReporter.start(spec, 101));
    }

    @Test(description = "OpenAPI coverage caches a file validation filter and ignores blank interactions")
    public void testOpenApiCoverageFilterAndIgnoredInteractions() throws IOException {
        String spec = writeSpec();
        OpenApiValidationFilter first = OpenApiCoverageReporter.validationFilter(spec);
        OpenApiValidationFilter second = OpenApiCoverageReporter.validationFilter(spec);
        Assert.assertSame(first, second);

        OpenApiCoverageReporter.start(spec, 80);
        OpenApiCoverageReporter.recordInteraction(" ", RestActions.RequestType.GET, "/pets", null);
        OpenApiCoverageReporter.recordInteraction(spec, null, "/pets", null);
        OpenApiCoverageReporter.recordInteraction(spec, RestActions.RequestType.GET, " ", null);
        OpenApiCoverageReporter.recordInteraction(spec, RestActions.RequestType.GET, "pets/7/?verbose=true", null);

        String summary = OpenApiCoverageReporter.buildSummaryText();
        assertTrue(summary.contains("GET /pets/{petId} (getPet) hits=1"), summary);
        assertTrue(summary.contains("Unmatched operations:"), summary);
        AssertionError reported = OpenApiCoverageReporter.reportAndGetThresholdFailure();
        assertNotNull(reported);
        assertTrue(reported.getMessage().contains("33.33% is below the configured 80% threshold"),
                reported.getMessage());
        Assert.assertNull(OpenApiCoverageReporter.reportAndGetThresholdFailure());
    }

    @Test(description = "OpenAPI coverage reports JSON, a met threshold, an empty document, and a missing spec")
    public void testOpenApiCoverageReportEdges() throws IOException {
        String spec = writeSpec();
        OpenApiCoverageReporter.start(spec, 50);
        OpenApiCoverageReporter.recordInteraction(spec, RestActions.RequestType.GET, "/pets", null);
        OpenApiCoverageReporter.recordInteraction(spec, RestActions.RequestType.GET, "/pets/7", validationFailure());
        OpenApiCoverageReporter.recordInteraction(spec, RestActions.RequestType.POST, "/orders", null);
        Assert.assertNull(OpenApiCoverageReporter.thresholdFailure());

        AssertionError reported = OpenApiCoverageReporter.reportAndGetThresholdFailure();
        Assert.assertNull(reported);
        Assert.assertNull(OpenApiCoverageReporter.reportAndGetThresholdFailure());

        String untagged = writeUntaggedSpec();
        OpenApiCoverageReporter.start(untagged, 100);
        String untaggedSummary = OpenApiCoverageReporter.buildSummaryText();
        assertTrue(untaggedSummary.contains("untagged"), untaggedSummary);
        assertTrue(untaggedSummary.contains("GET /plain (GET /plain)"), untaggedSummary);
        assertTrue(untaggedSummary.contains("Exercised operations: none"), untaggedSummary);
        AssertionError untaggedReport = OpenApiCoverageReporter.reportAndGetThresholdFailure();
        assertNotNull(untaggedReport);
        assertTrue(untaggedReport.getMessage().contains("0.00% is below the configured 100% threshold"),
                untaggedReport.getMessage());

        String empty = writeEmptyPathsSpec();
        OpenApiCoverageReporter.start(empty, 1);
        AssertionError emptyPaths = OpenApiCoverageReporter.thresholdFailure();
        assertNotNull(emptyPaths);
        assertTrue(emptyPaths.getMessage().contains("0.00%"), emptyPaths.getMessage());

        IllegalArgumentException missing = Assert.expectThrows(IllegalArgumentException.class,
                () -> OpenApiCoverageReporter.start(specFile.getParent().resolve("missing-openapi.yaml").toString(), 0));
        assertTrue(missing.getMessage().contains("Unable to load OpenAPI specification"), missing.getMessage());
    }

    @Test(description = "SHAFT API requests are recorded in OpenAPI coverage")
    public void testShaftApiRequestRecordsOpenApiCoverage() throws Exception {
        String spec = writeResourceSpec();
        HttpServer server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/resource", exchange -> {
            byte[] body = "{\"ok\":true}".getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, body.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(body);
            }
        });
        server.start();

        try {
            SHAFT.Properties.api.set()
                    .swaggerValidationEnabled(false)
                    .swaggerValidationUrl(spec)
                    .openApiCoverageReportEnabled(true)
                    .openApiCoverageThreshold(0);

            SHAFT.API api = new SHAFT.API("http://127.0.0.1:" + server.getAddress().getPort() + "/");
            api.get("resource").perform();

            String summary = OpenApiCoverageReporter.buildSummaryText();
            assertTrue(summary.contains("GET /resource (getResource) hits=1 validationFailures=0"), summary);
        } finally {
            server.stop(0);
        }
    }

    private String writeSpec() throws IOException {
        specFile = Files.createTempFile("shaft-openapi-coverage", ".yaml");
        Files.writeString(specFile, """
                openapi: 3.0.0
                info:
                  title: SHAFT Coverage Test API
                  version: 1.0.0
                paths:
                  /pets:
                    get:
                      tags:
                        - Pets
                      operationId: listPets
                      responses:
                        '200':
                          description: ok
                  /pets/{petId}:
                    parameters:
                      - name: petId
                        in: path
                        required: true
                        schema:
                          type: string
                    get:
                      tags:
                        - Pets
                      operationId: getPet
                      responses:
                        '200':
                          description: ok
                  /orders:
                    post:
                      tags:
                        - Orders
                      operationId: createOrder
                      responses:
                        '201':
                          description: created
                """);
        return specFile.toString().replace("\\", "/");
    }

    private String writeUntaggedSpec() throws IOException {
        return replaceSpec("""
                openapi: 3.0.0
                info:
                  title: SHAFT Untagged Coverage Test API
                  version: 1.0.0
                paths:
                  /plain:
                    get:
                      responses:
                        '200':
                          description: ok
                """);
    }

    private String writeEmptyPathsSpec() throws IOException {
        return replaceSpec("""
                openapi: 3.0.0
                info:
                  title: SHAFT Empty Coverage Test API
                  version: 1.0.0
                paths: {}
                """);
    }

    private String replaceSpec(String yaml) throws IOException {
        if (specFile != null) {
            Files.deleteIfExists(specFile);
        }
        specFile = Files.createTempFile("shaft-openapi-coverage", ".yaml");
        Files.writeString(specFile, yaml);
        return specFile.toString().replace("\\", "/");
    }

    private String writeResourceSpec() throws IOException {
        specFile = Files.createTempFile("shaft-openapi-resource-coverage", ".yaml");
        Files.writeString(specFile, """
                openapi: 3.0.0
                info:
                  title: SHAFT Resource Coverage Test API
                  version: 1.0.0
                paths:
                  /resource:
                    get:
                      tags:
                        - Resources
                      operationId: getResource
                      responses:
                        '200':
                          description: ok
                """);
        return specFile.toString().replace("\\", "/");
    }

    private OpenApiValidationFilter.OpenApiValidationException validationFailure() {
        ValidationReport.Message message = ValidationReport.Message
                .create("response.body.invalid", "response body did not match schema")
                .build();
        return new OpenApiValidationFilter.OpenApiValidationException(ValidationReport.singleton(message));
    }
}
