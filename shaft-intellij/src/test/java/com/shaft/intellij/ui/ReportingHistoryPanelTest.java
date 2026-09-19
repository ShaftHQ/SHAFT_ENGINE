package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import javax.swing.JTable;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportingHistoryPanelTest {
    @Test
    void missingHistoryShowsEmptyStateNotCrash() {
        ReportingHistoryPanel panel = new ReportingHistoryPanel();
        panel.applyHistoryJson("""
                {
                  "empty": true,
                  "emptyMessage": "No Allure history yet.",
                  "tests": [],
                  "retries": [],
                  "allureResultsRootReplaced": false
                }
                """);
        assertEquals(0, panel.historyTable().getRowCount());
        assertTrue(panel.statusLabel().getText().contains("No Allure history"));
    }

    @Test
    void twoLaunchesPerHistoryIdAreVisibleAndRetriesMarked() {
        ReportingHistoryPanel panel = new ReportingHistoryPanel();
        panel.applyHistoryJson("""
                {
                  "empty": false,
                  "emptyMessage": "",
                  "allureResultsRootReplaced": false,
                  "tests": [
                    {
                      "historyId": "hist-login",
                      "name": "login",
                      "doctorCause": "LOCATOR",
                      "launches": [
                        {"launchUuid": "launch-2", "launchName": "Nightly #2", "status": "passed",
                         "timestamp": 2, "kind": "HISTORY"},
                        {"launchUuid": "launch-1", "launchName": "Nightly #1", "status": "failed",
                         "timestamp": 1, "kind": "HISTORY"}
                      ]
                    }
                  ],
                  "retries": [
                    {
                      "historyId": "hist-login",
                      "name": "login",
                      "attempts": [
                        {"resultUuid": "retry-2", "status": "passed", "start": 300, "kind": "RETRY"},
                        {"resultUuid": "retry-1", "status": "failed", "start": 100, "kind": "RETRY"}
                      ]
                    }
                  ]
                }
                """);
        JTable table = panel.historyTable();
        assertEquals(4, table.getRowCount());
        assertEquals("HISTORY", table.getValueAt(0, 2));
        assertEquals("RETRY", table.getValueAt(2, 2));
        assertTrue(panel.statusLabel().getText().contains("never replaced"));
    }
}
