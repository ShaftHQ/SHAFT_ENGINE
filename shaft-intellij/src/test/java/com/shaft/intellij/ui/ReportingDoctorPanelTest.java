package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportingDoctorPanelTest {
    @Test
    void retryFixtureShowsTimingSynchronizationAndRetryCorrelation() {
        // SC-001 / FR-001: retry-hidden Doctor output labels TIMING, not PRODUCT.
        ReportingDoctorPanel panel = new ReportingDoctorPanel();
        panel.applyDoctorJson("""
                {
                  "schemaVersion": "1.0",
                  "status": "DETERMINISTIC",
                  "bundleId": "retry-bundle",
                  "primaryCause": "TIMING_SYNCHRONIZATION",
                  "confidence": "MEDIUM",
                  "summary": "The final result is green, but earlier failed or broken attempts show flake evidence.",
                  "diagnosis": {
                    "schemaVersion": "1.1",
                    "primaryCause": "TIMING_SYNCHRONIZATION",
                    "contributingCauses": [],
                    "confidence": "MEDIUM",
                    "summary": "The final result is green, but earlier failed or broken attempts show flake evidence.",
                    "rationale": "Retry correlation found a non-final failure followed by a passed final attempt.",
                    "findings": [
                      {
                        "id": "f-retry-1",
                        "kind": "OBSERVATION",
                        "category": "TIMING_SYNCHRONIZATION",
                        "severity": "WARNING",
                        "title": "A retry hid an earlier failed or broken attempt",
                        "detail": "The final attempt passed, but Doctor retained and inspected all non-final attempts.",
                        "ruleId": "retry-correlation",
                        "evidenceIds": ["e-1", "e-2"]
                      }
                    ],
                    "remediations": [],
                    "missingEvidence": [],
                    "rankedCauses": []
                  },
                  "actions": [],
                  "codeBlocks": [],
                  "warnings": []
                }
                """);

        String card = panel.cardArea().getText();
        assertAll(
                () -> assertTrue(card.contains("TIMING_SYNCHRONIZATION"), card),
                () -> assertTrue(card.contains("Retry correlation") || card.contains("retry-correlation"), card),
                () -> assertTrue(card.contains("not a product defect") || card.contains("Diagnosis card"), card),
                () -> assertFalse(card.contains("**Primary cause:** PRODUCT"), card),
                () -> assertTrue(panel.statusLabel().getText().toLowerCase().contains("timing")
                        || panel.statusLabel().getText().toLowerCase().contains("diagnosis"), panel.statusLabel().getText()));
    }

    @Test
    void historicalSignatureFixtureShowsClusterKey() {
        // SC-002: historical-signature finding detail carries Cluster key from Doctor JSON.
        ReportingDoctorPanel panel = new ReportingDoctorPanel();
        panel.applyDoctorJson("""
                {
                  "schemaVersion": "1.0",
                  "status": "DETERMINISTIC",
                  "bundleId": "history-bundle",
                  "primaryCause": "LOCATOR",
                  "confidence": "HIGH",
                  "summary": "Locator did not resolve an element.",
                  "diagnosis": {
                    "schemaVersion": "1.1",
                    "primaryCause": "LOCATOR",
                    "contributingCauses": [],
                    "confidence": "HIGH",
                    "summary": "Locator did not resolve an element.",
                    "rationale": "The first matching rule was locator-not-found.",
                    "findings": [
                      {
                        "id": "f-loc-1",
                        "kind": "INFERENCE",
                        "category": "LOCATOR",
                        "severity": "ERROR",
                        "title": "Locator did not resolve an element",
                        "detail": "Rule locator-not-found matched.",
                        "ruleId": "locator-not-found",
                        "evidenceIds": ["e-now"]
                      },
                      {
                        "id": "f-hist-1",
                        "kind": "OBSERVATION",
                        "category": "UNKNOWN",
                        "severity": "WARNING",
                        "title": "Failure signature recurred across evidence bundles",
                        "detail": "A normalized current failure signature was also present in supplied historical bundles. Cluster key: fp-stable.",
                        "ruleId": "historical-signature-correlation",
                        "evidenceIds": ["e-now"]
                      }
                    ],
                    "remediations": [],
                    "missingEvidence": [],
                    "rankedCauses": []
                  },
                  "actions": [],
                  "codeBlocks": [],
                  "warnings": []
                }
                """);

        String card = panel.cardArea().getText();
        assertAll(
                () -> assertTrue(card.contains("Historical signature"), card),
                () -> assertTrue(card.contains("fp-stable"), card),
                () -> assertTrue(card.contains("Locator") || card.contains("LOCATOR"), card),
                () -> assertTrue(panel.statusLabel().getText().toLowerCase().contains("historical")
                        || panel.statusLabel().getText().toLowerCase().contains("cluster")
                        || panel.statusLabel().getText().toLowerCase().contains("diagnosis"),
                        panel.statusLabel().getText()));
    }
}
