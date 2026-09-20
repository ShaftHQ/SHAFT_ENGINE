package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportingLabelsPanelTest {
    @Test
    void suggestResultShowsAlias() {
        ReportingLabelsPanel panel = new ReportingLabelsPanel();
        panel.applyResultJson("""
                {
                  "action": "suggest",
                  "status": "ok",
                  "message": "Suggested confirmed cause ENVIRONMENT from prior user confirmation for this signature.",
                  "signature": "sig-grid",
                  "causeCategory": "ENVIRONMENT_CONFIGURATION",
                  "displayAlias": "ENVIRONMENT",
                  "matched": true,
                  "replaced": false
                }
                """);
        assertTrue(panel.statusLabel().getText().contains("ENVIRONMENT"));
        assertTrue(panel.statusLabel().getText().contains("suggest"));
    }

    @Test
    void confirmOverrideMentionsReplaced() {
        ReportingLabelsPanel panel = new ReportingLabelsPanel();
        panel.signatureField().setText("sig-login");
        panel.applyResultJson("""
                {
                  "action": "confirm",
                  "status": "ok",
                  "message": "Overrode confirmed cause TIMING for signature.",
                  "signature": "sig-login",
                  "causeCategory": "TIMING_SYNCHRONIZATION",
                  "displayAlias": "TIMING",
                  "matched": true,
                  "replaced": true
                }
                """);
        assertTrue(panel.statusLabel().getText().contains("replaced"));
        assertTrue(panel.statusLabel().getText().contains("TIMING"));
    }
}
