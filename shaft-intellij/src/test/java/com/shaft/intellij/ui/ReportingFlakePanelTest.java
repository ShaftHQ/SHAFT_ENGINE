package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import javax.swing.JTable;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportingFlakePanelTest {
    @Test
    void emptyStateDoesNotCrash() {
        ReportingFlakePanel panel = new ReportingFlakePanel();
        panel.applyFlakeJson("""
                {
                  "empty": true,
                  "emptyMessage": "No flake rows yet.",
                  "rows": []
                }
                """);
        assertEquals(0, panel.flakeTable().getRowCount());
        assertTrue(panel.statusLabel().getText().contains("No flake"));
    }

    @Test
    void twoFixtureTypesShowTwoDifferentTagsAndNoCombinedScore() {
        ReportingFlakePanel panel = new ReportingFlakePanel();
        panel.applyFlakeJson("""
                {
                  "empty": false,
                  "windowSize": 10,
                  "transitionThreshold": 3,
                  "rows": [
                    {
                      "historyId": "hist-retry-only",
                      "name": "retryOnly",
                      "retryHidden": true,
                      "retryHiddenTag": "retry-hidden",
                      "transitionAssessment": "always-passing",
                      "transitionCount": 0,
                      "launchCount": 2,
                      "sameShaAvailable": false,
                      "sameShaTransitionCount": null,
                      "tags": ["retry-hidden"]
                    },
                    {
                      "historyId": "hist-transitions",
                      "name": "flippy",
                      "retryHidden": false,
                      "retryHiddenTag": "",
                      "transitionAssessment": "transitions",
                      "transitionCount": 9,
                      "launchCount": 10,
                      "sameShaAvailable": false,
                      "sameShaTransitionCount": null,
                      "tags": ["transitions"]
                    }
                  ]
                }
                """);
        JTable table = panel.flakeTable();
        assertEquals(2, table.getRowCount());
        assertEquals("retry-hidden", table.getValueAt(0, 2));
        assertEquals("always-passing", table.getValueAt(0, 3));
        assertEquals("retry-hidden", table.getValueAt(0, 6));
        assertEquals("", table.getValueAt(1, 2));
        assertEquals("transitions", table.getValueAt(1, 3));
        assertEquals("transitions", table.getValueAt(1, 6));
        assertFalse(panel.statusLabel().getText().toLowerCase().contains("combined score only"));
        assertTrue(panel.statusLabel().getText().contains("no combined score"));
    }

    @Test
    void insufficientHistoryShowsUnknownNotZero() {
        ReportingFlakePanel panel = new ReportingFlakePanel();
        panel.applyFlakeJson("""
                {
                  "empty": false,
                  "rows": [
                    {
                      "historyId": "hist-insufficient",
                      "name": "once",
                      "retryHidden": false,
                      "retryHiddenTag": "",
                      "transitionAssessment": "unknown",
                      "transitionCount": null,
                      "launchCount": 1,
                      "sameShaAvailable": false,
                      "tags": []
                    }
                  ]
                }
                """);
        assertEquals("unknown", panel.flakeTable().getValueAt(0, 3));
        assertEquals("—", panel.flakeTable().getValueAt(0, 4));
    }
}
