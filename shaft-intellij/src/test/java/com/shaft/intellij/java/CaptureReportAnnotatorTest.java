package com.shaft.intellij.java;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureReportAnnotatorTest {
    @Test
    void flattensReportFindingsWithCategoryLabelsAndCap() {
        JsonObject report = JsonParser.parseString("""
                {
                  "sourcePath": "src/test/java/tests/generated/RecordedFlowTest.java",
                  "readinessWarnings": ["Step 8 needs a follow-up assertion after navigation."],
                  "warnings": ["Recording used a positional locator."],
                  "flakySteps": ["step-3"],
                  "unsupportedEvents": ["drag-and-drop on canvas"],
                  "requiredUserInputs": ["data.searchbox-input-4"],
                  "fallbackLocators": ["#searchbox_input"]
                }
                """).getAsJsonObject();

        List<String> findings = CaptureReportAnnotator.findings(report);

        assertAll(
                () -> assertEquals(6, findings.size()),
                () -> assertTrue(findings.contains("Step 8 needs a follow-up assertion after navigation.")),
                () -> assertTrue(findings.contains("potentially flaky — step-3")),
                () -> assertTrue(findings.contains("not converted to code — drag-and-drop on canvas")),
                () -> assertTrue(findings.contains("input required before running — data.searchbox-input-4")),
                () -> assertTrue(findings.contains("fallback locator — #searchbox_input")));
    }

    @Test
    void capsPathologicalReportsSoTheEditorIsNeverFlooded() {
        StringBuilder json = new StringBuilder("{\"warnings\": [");
        for (int index = 0; index < 40; index++) {
            json.append(index > 0 ? "," : "").append("\"warning ").append(index).append('"');
        }
        json.append("]}");

        List<String> findings = CaptureReportAnnotator.findings(
                JsonParser.parseString(json.toString()).getAsJsonObject());

        assertEquals(12, findings.size());
    }
}
