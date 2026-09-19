package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Issue #5962 / S2-06: UNCONFIRMED must not satisfy the production Keep gate.
 */
class CaptureReplayProofTest {
    @Test
    void successIsProductionReadyAndLabeled() {
        JsonObject raw = JsonParser.parseString("""
                {
                  "successful": true,
                  "report": {
                    "status": "SUCCESS",
                    "compilation": {"status": "PASSED", "diagnostics": []},
                    "replay": {"status": "PASSED", "diagnostics": []}
                  }
                }
                """).getAsJsonObject();
        assertAll(
                () -> assertTrue(CaptureReplayProof.isProductionReady(raw)),
                () -> assertFalse(CaptureReplayProof.isUnconfirmed(raw)),
                () -> assertTrue(CaptureReplayProof.canvasLabel(raw).startsWith("SUCCESS")),
                () -> assertTrue(CaptureReplayProof.evidenceSummary(raw).contains("compilation=PASSED")));
    }

    @Test
    void unconfirmedBlocksKeepAndSurfacesEvidence() {
        JsonObject raw = JsonParser.parseString("""
                {
                  "successful": false,
                  "report": {
                    "status": "UNCONFIRMED",
                    "compilation": {"status": "PASSED", "diagnostics": []},
                    "replay": {"status": "SKIPPED", "diagnostics": ["replay not requested"]}
                  }
                }
                """).getAsJsonObject();
        assertAll(
                () -> assertFalse(CaptureReplayProof.isProductionReady(raw)),
                () -> assertTrue(CaptureReplayProof.isUnconfirmed(raw)),
                () -> assertTrue(CaptureReplayProof.canvasLabel(raw).contains("UNCONFIRMED")),
                () -> assertTrue(CaptureReplayProof.canvasLabel(raw).contains("Keep/insert blocked")),
                () -> assertTrue(CaptureReplayProof.evidenceSummary(raw).contains("report.status=UNCONFIRMED")),
                () -> assertTrue(CaptureReplayProof.evidenceSummary(raw).contains("replay=SKIPPED")),
                () -> assertTrue(CaptureReplayProof.evidenceSummary(raw).contains("replay not requested")));
    }

    @Test
    void missingReportIsNotProductionReady() {
        JsonObject raw = JsonParser.parseString("""
                {"successful": true, "codeBlocks": []}
                """).getAsJsonObject();
        assertAll(
                () -> assertEquals("", CaptureReplayProof.reportStatus(raw)),
                () -> assertFalse(CaptureReplayProof.isProductionReady(raw)),
                () -> assertTrue(CaptureReplayProof.canvasLabel(raw).contains("unproven")));
    }
}
