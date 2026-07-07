package com.shaft.capture.generate.api.internal;

import com.shaft.capture.generate.api.ApiTransaction;
import com.shaft.capture.generate.api.ResponseNormalizer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OpenApiCoverageReporterTest {

    @TempDir
    Path tempDir;

    @Test
    void jsonSpecReportsCoveredMissingAndUndeclaredOperations() throws Exception {
        Path specPath = tempDir.resolve("openapi.json");
        Files.writeString(specPath, """
                {
                  "openapi": "3.0.0",
                  "paths": {
                    "/orders/{orderId}": {
                      "get": {},
                      "delete": {}
                    },
                    "/health": {
                      "get": {}
                    }
                  }
                }
                """, StandardCharsets.UTF_8);

        List<ApiTransaction> transactions = List.of(
                transaction("GET", "https://api.example.test/orders/42"),
                transaction("POST", "https://api.example.test/orders/42/cancel"));

        OpenApiCoverageReporter.CoverageReport report = OpenApiCoverageReporter.report(specPath, transactions);

        assertTrue(report.loadable(), "Report load failure: " + report.loadFailureReason());
        assertEquals(List.of("GET /orders/{param}"), report.coveredOperations());
        assertEquals(List.of("DELETE /orders/{param}", "GET /health"), report.missingOperations());
        assertEquals(List.of("POST /orders/{param}/cancel"), report.undeclaredOperations());
        assertEquals(3, report.totalDeclaredOperations());
    }

    @Test
    void yamlSpecIsParsedTheSameAsJson() throws Exception {
        Path specPath = tempDir.resolve("openapi.yaml");
        Files.writeString(specPath, """
                openapi: "3.0.0"
                paths:
                  /health:
                    get: {}
                """, StandardCharsets.UTF_8);

        OpenApiCoverageReporter.CoverageReport report = OpenApiCoverageReporter.report(
                specPath, List.of(transaction("GET", "https://api.example.test/health")));

        assertTrue(report.loadable(), "Report load failure: " + report.loadFailureReason());
        assertEquals(List.of("GET /health"), report.coveredOperations());
        assertEquals(1.0, report.coverageRatio());
    }

    @Test
    void numericAndUuidSegmentsAreNormalizedToMatchTemplatedOperations() throws Exception {
        Path specPath = tempDir.resolve("openapi.json");
        Files.writeString(specPath, """
                {"paths": {"/orders/{id}/items/{itemId}": {"get": {}}}}
                """, StandardCharsets.UTF_8);

        OpenApiCoverageReporter.CoverageReport report = OpenApiCoverageReporter.report(specPath, List.of(
                transaction("GET", "https://api.example.test/orders/3fa85f64-5717-4562-b3fc-2c963f66afa6/items/7")));

        assertTrue(report.loadable());
        assertEquals(List.of("GET /orders/{param}/items/{param}"), report.coveredOperations());
        assertEquals(List.of(), report.missingOperations());
        assertEquals(List.of(), report.undeclaredOperations());
    }

    @Test
    void missingSpecFileIsReportedAsNotLoadableRatherThanThrowing() {
        OpenApiCoverageReporter.CoverageReport report = OpenApiCoverageReporter.report(
                tempDir.resolve("does-not-exist.json"), List.of());

        assertFalse(report.loadable());
        assertFalse(report.loadFailureReason().isBlank());
    }

    @Test
    void specWithNoPathsIsReportedAsNotLoadable() throws Exception {
        Path specPath = tempDir.resolve("openapi.json");
        Files.writeString(specPath, "{\"openapi\": \"3.0.0\"}", StandardCharsets.UTF_8);

        OpenApiCoverageReporter.CoverageReport report = OpenApiCoverageReporter.report(specPath, List.of());

        assertFalse(report.loadable());
    }

    private static ApiTransaction transaction(String method, String url) {
        return new ApiTransaction("tx", method, url, "https://api.example.test", Map.of(), "", 200, Map.of(), "{}",
                ResponseNormalizer.classify("{}"));
    }
}
