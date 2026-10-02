package com.shaft.capture.privacy;

import com.shaft.capture.network.CaptureNetworkRecorder;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #6375: direct contract tests for capture redaction and URL glob filtering.
 */
class CapturePrivacyContractTest {
    @Test
    void sanitizeTextRedactsBearerTokensAndKeepsPlainText() {
        CapturePrivacyClassifier classifier = new CapturePrivacyClassifier();
        String sanitized = classifier.sanitizeText("Authorization: Bearer abc.def-123").value();
        assertFalse(sanitized.contains("abc.def-123"));
        assertEquals("plain checkout text", classifier.sanitizeText("plain checkout text").value());
    }

    @Test
    void onlyNonSecretClassifiedValuesArePersistable() {
        CapturePrivacyClassifier classifier = new CapturePrivacyClassifier();
        assertTrue(classifier.classifyValue("firstName", "Jane", "#first-name", Map.of()).persistable());
        assertFalse(classifier.classifyValue("password", "hunter2", "input[type=password]",
                Map.of("type", "password")).persistable());
    }

    @Test
    void globsMatchWildcardsAndBlankGlobsNeverMatch() {
        assertTrue(CaptureNetworkRecorder.matchesGlob("https://shop.test/api/cart", "**/api/*"));
        assertFalse(CaptureNetworkRecorder.matchesGlob("https://shop.test/static/app.js", "**/api/*"));
        assertFalse(CaptureNetworkRecorder.matchesGlob("https://shop.test/api/cart", " "));
        assertFalse(CaptureNetworkRecorder.matchesGlob("https://shop.test/api/cart", null));
    }

    @Test
    void transactionSummariesKeepOnlyMethodAndPath() {
        assertEquals("GET /api/cart", com.shaft.capture.generate.CaptureEnrichmentService
                .boundedTransactionSummary(" GET https://shop.test/api/cart?token=abc "));
        assertEquals("POST /", com.shaft.capture.generate.CaptureEnrichmentService
                .boundedTransactionSummary("POST https://shop.test"));
        assertEquals("", com.shaft.capture.generate.CaptureEnrichmentService.boundedTransactionSummary(" "));
        assertEquals("GET", com.shaft.capture.generate.CaptureEnrichmentService.boundedTransactionSummary("GET"));
    }
}
