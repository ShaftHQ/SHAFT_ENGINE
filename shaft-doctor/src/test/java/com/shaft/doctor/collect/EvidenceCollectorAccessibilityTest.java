package com.shaft.doctor.collect;

import com.shaft.doctor.DoctorAnalysisRequest;
import com.shaft.doctor.format.DoctorJsonCodec;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import com.shaft.doctor.model.EvidenceItem;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class EvidenceCollectorAccessibilityTest {

    @Test
    void accessibilityAuditJsonIsCollectedWithSummaryAttributes(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        Files.writeString(inputDir.resolve("AccessibilityJSON_HomePage_20260101_000000.json"),
                accessibilityAuditFixture(), StandardCharsets.UTF_8);
        EvidenceCollector collector = new EvidenceCollector();
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir),
                List.of(),
                List.of(temp),
                outputDir,
                true,
                true,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES,
                true);

        EvidenceBundle bundle = collector.collect(request);

        List<EvidenceItem> audits = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.ACCESSIBILITY_AUDIT)
                .toList();
        assertEquals(1, audits.size(), "Exactly one accessibility-audit evidence item should be collected");
        EvidenceItem audit = audits.getFirst();
        assertEquals("HomePage", audit.attributes().get("pageName"));
        assertEquals("2", audit.attributes().get("violationsCount"));
        assertEquals("1", audit.attributes().get("incompleteCount"));
        assertEquals("1", audit.attributes().get("passesCount"));
        assertEquals("1", audit.attributes().get("inapplicableCount"));
        assertEquals("1", audit.attributes().get("criticalCount"));
        assertEquals("0", audit.attributes().get("seriousCount"));
        assertEquals("1", audit.attributes().get("moderateCount"));
        assertEquals("0", audit.attributes().get("minorCount"));
        assertTrue(audit.attributes().get("topRuleIds").contains("color-contrast"));
        assertTrue(audit.attributes().get("topRuleIds").contains("label"));
        assertEquals("accessibility-audit-json", audit.provenance().adapter());

        assertFalse(audit.content().contains("SuperSecretCanary123"),
                "A canary secret embedded in a violation node's html must be redacted");
        assertTrue(audit.content().contains("[REDACTED]"));

        assertDoesNotThrow(() -> new DoctorJsonCodec().write(bundle),
                "Bundle with ACCESSIBILITY_AUDIT evidence must round-trip through the JSON schema");
    }

    @Test
    void malformedAccessibilityAuditJsonIsRetainedAsInvalidWithoutThrowing(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        Files.writeString(inputDir.resolve("AccessibilityJSON_BrokenPage_20260101_000000.json"),
                "{\"violations\": [ this is not valid json",
                StandardCharsets.UTF_8);
        EvidenceCollector collector = new EvidenceCollector();
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir),
                List.of(),
                List.of(temp),
                outputDir,
                true,
                true,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES,
                true);

        EvidenceBundle bundle = assertDoesNotThrow(() -> collector.collect(request),
                "Malformed accessibility audit JSON must never throw during collection");

        List<EvidenceItem> audits = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.ACCESSIBILITY_AUDIT)
                .toList();
        assertEquals(1, audits.size());
        assertEquals("true", audits.getFirst().attributes().get("invalid"));

        assertDoesNotThrow(() -> new DoctorJsonCodec().write(bundle));
    }

    private static String accessibilityAuditFixture() {
        return """
                {
                  "violations": [
                    {
                      "id": "color-contrast",
                      "category": "Violation",
                      "impact": "critical",
                      "description": "Elements must meet minimum color contrast ratio thresholds",
                      "helpUrl": "https://dequeuniversity.com/rules/axe/color-contrast",
                      "wcagModel": "WCAG21AA",
                      "tags": ["wcag2aa", "wcag143"],
                      "nodes": [
                        {
                          "html": "<input type=\\"text\\" data-value=\\"password=SuperSecretCanary123\\">",
                          "target": "#login-username",
                          "failureSummary": "Fix contrast on the username field"
                        }
                      ]
                    },
                    {
                      "id": "label",
                      "category": "Violation",
                      "impact": "moderate",
                      "description": "Form elements must have labels",
                      "helpUrl": "https://dequeuniversity.com/rules/axe/label",
                      "wcagModel": "WCAG20A",
                      "tags": ["wcag2a", "wcag412"],
                      "nodes": [
                        {
                          "html": "<input type=\\"text\\" id=\\"search\\">",
                          "target": "#search",
                          "failureSummary": "Add a label to the search field"
                        }
                      ]
                    }
                  ],
                  "incomplete": [
                    {
                      "id": "aria-hidden-focus",
                      "category": "Incomplete",
                      "impact": "serious",
                      "description": "ARIA hidden element must not contain focusable elements",
                      "helpUrl": "https://dequeuniversity.com/rules/axe/aria-hidden-focus",
                      "wcagModel": "WCAG21A",
                      "tags": ["wcag2a"],
                      "nodes": []
                    }
                  ],
                  "inapplicable": [
                    {
                      "id": "audio-caption",
                      "category": "Inapplicable",
                      "impact": null,
                      "description": "Audio elements must have a captions track",
                      "helpUrl": "https://dequeuniversity.com/rules/axe/audio-caption",
                      "wcagModel": "WCAG20A",
                      "tags": ["wcag2a"],
                      "nodes": []
                    }
                  ],
                  "passes": [
                    {
                      "id": "document-title",
                      "category": "Pass",
                      "impact": null,
                      "description": "Documents must have a title",
                      "helpUrl": "https://dequeuniversity.com/rules/axe/document-title",
                      "wcagModel": "WCAG20A",
                      "tags": ["wcag2a"],
                      "nodes": []
                    }
                  ],
                  "totalViolations": 2,
                  "totalIncomplete": 1,
                  "totalInapplicable": 1,
                  "totalPassed": 1
                }
                """;
    }
}
