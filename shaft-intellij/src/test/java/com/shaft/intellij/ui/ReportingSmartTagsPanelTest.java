package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import javax.swing.JTable;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportingSmartTagsPanelTest {
    @Test
    void emptyStateDoesNotCrash() {
        ReportingSmartTagsPanel panel = new ReportingSmartTagsPanel();
        panel.applyTagsJson("""
                {
                  "empty": true,
                  "emptyMessage": "No smart tags yet.",
                  "rows": []
                }
                """);
        assertEquals(0, panel.tagsTable().getRowCount());
        assertTrue(panel.statusLabel().getText().contains("No smart tags"));
    }

    @Test
    void regressedAndNewFixturesRenderDistinctTags() {
        ReportingSmartTagsPanel panel = new ReportingSmartTagsPanel();
        panel.applyTagsJson("""
                {
                  "empty": false,
                  "rows": [
                    {
                      "historyId": "hist-regressed",
                      "name": "regressed",
                      "primaryTag": "Regressed",
                      "tags": ["Regressed"],
                      "launchCount": 2,
                      "transitionCount": 1,
                      "newestStatus": "failed",
                      "previousStatus": "passed",
                      "durationAnomaly": false
                    },
                    {
                      "historyId": "hist-new",
                      "name": "firstSeen",
                      "primaryTag": "New",
                      "tags": ["New"],
                      "launchCount": 1,
                      "transitionCount": null,
                      "newestStatus": "failed",
                      "previousStatus": "",
                      "durationAnomaly": false
                    }
                  ]
                }
                """);
        JTable table = panel.tagsTable();
        assertEquals(2, table.getRowCount());
        assertEquals("Regressed", table.getValueAt(0, 2));
        assertEquals("New", table.getValueAt(1, 2));
        assertTrue(panel.statusLabel().getText().contains("never invents Flaky"));
    }
}
