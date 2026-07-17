package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Path-extraction coverage for {@link DoctorEvidenceImageLocator} (issue #3642) against
 * representative fixture shapes for {@code McpAnalysisReport} (returned by
 * {@code doctor_analyze_failed_allure}/{@code doctor_suggest_fix}) and {@code McpHealerRunResult}
 * (returned by {@code healer_run_failed_test}, nesting an {@code McpAnalysisReport} under
 * {@code analysis}). Neither result type embeds evidence directly -- confirmed against
 * {@code McpDoctorRemediationService.build()} -- so every fixture here points {@code bundlePath}/
 * {@code jsonReportPath} at a real evidence-bundle/report JSON file on disk, matching the schema
 * in the golden fixture {@code shaft-doctor/src/test/resources/fixtures/golden/doctor-report.json}
 * ({@code evidence[].category} / {@code evidence[].relativePath}).
 */
class DoctorEvidenceImageLocatorTest {

    @Test
    void resolvesScreenshotAndPageSnapshotPathsFromABundlePathPointerExcludingNonImageEvidence(
            @TempDir Path directory) throws IOException {
        Files.createDirectories(directory.resolve("artifacts"));
        Files.writeString(directory.resolve("doctor-evidence.json"), """
                {
                  "schemaVersion": "1.0",
                  "bundleId": "bundle-abc",
                  "evidence": [
                    {
                      "id": "e-1",
                      "category": "SCREENSHOT",
                      "mediaType": "image/png",
                      "relativePath": "artifacts/e-1.png",
                      "sha256": "deadbeef",
                      "sizeBytes": 128,
                      "redacted": false,
                      "truncated": false,
                      "attributes": {},
                      "provenance": {"adapter": "allure-screenshot", "sourceReference": "attempt.png", "originalSha256": "deadbeef"}
                    },
                    {
                      "id": "e-2",
                      "category": "PAGE_SNAPSHOT",
                      "mediaType": "text/html",
                      "relativePath": "artifacts/e-2.html",
                      "sha256": "cafef00d",
                      "sizeBytes": 64,
                      "redacted": false,
                      "truncated": false,
                      "attributes": {},
                      "provenance": {"adapter": "allure-page-snapshot", "sourceReference": "attempt.html", "originalSha256": "cafef00d"}
                    },
                    {
                      "id": "e-3",
                      "category": "EXCEPTION_CHAIN",
                      "mediaType": "text/plain",
                      "relativePath": "",
                      "sha256": "0000",
                      "sizeBytes": 32,
                      "content": "NoSuchElementException",
                      "redacted": true,
                      "truncated": false,
                      "attributes": {},
                      "provenance": {"adapter": "allure-exception-chain", "sourceReference": "attempt.json", "originalSha256": "0000"}
                    }
                  ],
                  "redaction": {"appliedRules": [], "removedFieldNames": [], "omittedItems": 0},
                  "metadata": {}
                }
                """, StandardCharsets.UTF_8);
        String rawEvidence = """
                {
                  "schemaVersion": "1.0",
                  "status": "DETERMINISTIC",
                  "bundleId": "bundle-abc",
                  "bundlePath": "%s"
                }
                """.formatted(directory.resolve("doctor-evidence.json").toString().replace("\\", "\\\\"));

        List<Path> images = DoctorEvidenceImageLocator.resolveImagePaths(rawEvidence);

        assertEquals(2, images.size(), "Only the SCREENSHOT and PAGE_SNAPSHOT entries carry a renderable path");
        assertEquals(directory.resolve("artifacts/e-1.png").normalize(), images.get(0));
        assertEquals(directory.resolve("artifacts/e-2.html").normalize(), images.get(1));
    }

    @Test
    void resolvesImagePathsNestedUnderAHealerAnalysisFieldViaJsonReportPath(@TempDir Path directory)
            throws IOException {
        Files.writeString(directory.resolve("doctor-report.json"), """
                {
                  "bundle": {
                    "schemaVersion": "1.0",
                    "bundleId": "bundle-xyz",
                    "evidence": [
                      {
                        "id": "e-1",
                        "category": "SCREENSHOT",
                        "mediaType": "image/png",
                        "relativePath": "artifacts/e-1.png",
                        "sha256": "deadbeef",
                        "sizeBytes": 128,
                        "redacted": false,
                        "truncated": false,
                        "attributes": {},
                        "provenance": {"adapter": "allure-screenshot", "sourceReference": "attempt.png", "originalSha256": "deadbeef"}
                      }
                    ],
                    "redaction": {"appliedRules": [], "removedFieldNames": [], "omittedItems": 0},
                    "metadata": {}
                  },
                  "diagnosis": {"schemaVersion": "1.1", "primaryCause": "LOCATOR", "contributingCauses": [],
                    "confidence": "HIGH", "summary": "x", "rationale": "y", "findings": [], "remediations": [],
                    "missingEvidence": [], "rankedCauses": []}
                }
                """, StandardCharsets.UTF_8);
        // Mirrors McpHealerRunResult's actual nesting: analysis is an embedded McpAnalysisReport.
        String rawEvidence = """
                {
                  "schemaVersion": "1.0",
                  "status": "FAILED_WITH_SUGGESTIONS",
                  "attempts": [],
                  "analysis": {
                    "schemaVersion": "1.0",
                    "status": "DETERMINISTIC",
                    "jsonReportPath": "%s"
                  }
                }
                """.formatted(directory.resolve("doctor-report.json").toString().replace("\\", "\\\\"));

        List<Path> images = DoctorEvidenceImageLocator.resolveImagePaths(rawEvidence);

        assertEquals(1, images.size());
        assertEquals(directory.resolve("artifacts/e-1.png").normalize(), images.get(0));
    }

    @Test
    void returnsEmptyListWhenThePointerFileDoesNotExist(@TempDir Path directory) {
        String rawEvidence = """
                {"schemaVersion": "1.0", "bundlePath": "%s"}
                """.formatted(directory.resolve("missing-bundle.json").toString().replace("\\", "\\\\"));

        List<Path> images = DoctorEvidenceImageLocator.resolveImagePaths(rawEvidence);

        assertTrue(images.isEmpty(), "A missing evidence-bundle file must degrade to no resolved images");
    }

    @Test
    void returnsEmptyListForBlankOrMalformedRawEvidence() {
        assertTrue(DoctorEvidenceImageLocator.resolveImagePaths(null).isEmpty());
        assertTrue(DoctorEvidenceImageLocator.resolveImagePaths("").isEmpty());
        assertTrue(DoctorEvidenceImageLocator.resolveImagePaths("   ").isEmpty());
        assertTrue(DoctorEvidenceImageLocator.resolveImagePaths("not json {").isEmpty());
    }

    @Test
    void returnsEmptyListWhenRawEvidenceHasNoPointerFields() {
        String rawEvidence = "{\"schemaVersion\": \"1.0\", \"warnings\": [\"no evidence retained\"]}";

        assertTrue(DoctorEvidenceImageLocator.resolveImagePaths(rawEvidence).isEmpty());
    }
}
