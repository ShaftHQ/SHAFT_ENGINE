package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import javax.swing.JTable;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportingHealPanelTest {
    @Test
    void emptyStateDoesNotCrash() {
        ReportingHealPanel panel = new ReportingHealPanel();
        panel.applyInsightsJson("""
                {
                  "empty": true,
                  "emptyMessage": "No heal insights yet.",
                  "statusCounts": [],
                  "insights": []
                }
                """);
        assertEquals(0, panel.insightsTable().getRowCount());
        assertTrue(panel.statusLabel().getText().contains("heal insights"));
    }

    @Test
    void recoveredAndAmbiguousCountsVisibleAndOnlyRecoveredOffersReview() {
        ReportingHealPanel panel = new ReportingHealPanel();
        panel.applyInsightsJson("""
                {
                  "empty": false,
                  "totalReports": 2,
                  "statusCounts": [
                    {"status": "RECOVERED", "count": 1},
                    {"status": "AMBIGUOUS", "count": 1},
                    {"status": "NO_CANDIDATES", "count": 0}
                  ],
                  "insights": [
                    {
                      "attemptId": "ok-1",
                      "status": "RECOVERED",
                      "originalLocator": "By.id: old",
                      "proposedLocator": "By.id: new",
                      "actionOutcome": "PASSED",
                      "confidence": 0.91,
                      "canProposeSourcePatch": true,
                      "primaryAction": "REVIEW_DIFF",
                      "primaryActionLabel": "Review patch",
                      "reportPath": "/tmp/ok-1.json"
                    },
                    {
                      "attemptId": "amb-1",
                      "status": "AMBIGUOUS",
                      "originalLocator": "By.id: other",
                      "proposedLocator": "",
                      "actionOutcome": "FAILED",
                      "confidence": 0.4,
                      "canProposeSourcePatch": false,
                      "primaryAction": "NONE",
                      "primaryActionLabel": "",
                      "reportPath": "/tmp/amb-1.json"
                    }
                  ]
                }
                """);
        JTable table = panel.insightsTable();
        assertEquals(2, table.getRowCount());
        assertTrue(panel.countsLabel().getText().contains("RECOVERED=1"));
        assertTrue(panel.countsLabel().getText().contains("AMBIGUOUS=1"));
        assertEquals("Review patch", table.getValueAt(0, 6));
        assertEquals("—", table.getValueAt(1, 6));
        assertFalse(panel.reviewPatchButton().isEnabled());
    }

    @Test
    void noCandidatesIsShown() {
        ReportingHealPanel panel = new ReportingHealPanel();
        panel.applyInsightsJson("""
                {
                  "empty": false,
                  "totalReports": 1,
                  "statusCounts": [{"status": "NO_CANDIDATES", "count": 1}],
                  "insights": [
                    {
                      "attemptId": "nc-1",
                      "status": "NO_CANDIDATES",
                      "originalLocator": "By.cssSelector: .gone",
                      "proposedLocator": "",
                      "actionOutcome": "PENDING",
                      "confidence": 0.0,
                      "canProposeSourcePatch": false,
                      "primaryAction": "NONE",
                      "primaryActionLabel": ""
                    }
                  ]
                }
                """);
        assertEquals("NO_CANDIDATES", panel.insightsTable().getValueAt(0, 0));
        assertTrue(panel.countsLabel().getText().contains("NO_CANDIDATES=1"));
    }

    @Test
    void proposalJsonShowsReviewableDiffWithoutApplying() {
        ReportingHealPanel panel = new ReportingHealPanel();
        panel.applyProposalJson("""
                {
                  "originalExpression": "By.id(\\"old-login\\")",
                  "proposedExpression": "By.id(\\"new-login\\")",
                  "manifestPath": "target/shaft-doctor/healing-proposals/healing-locator-proposal-x.json"
                }
                """);
        assertTrue(panel.reviewDiffArea().getText().contains("By.id(\"old-login\")"));
        assertTrue(panel.reviewDiffArea().getText().contains("By.id(\"new-login\")"));
        assertTrue(panel.reviewDiffArea().getText().contains("not applied"));
        assertTrue(panel.statusLabel().getText().contains("source unchanged"));
    }
}
