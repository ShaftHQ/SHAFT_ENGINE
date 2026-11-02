package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import javax.swing.JTable;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ReportingClustersPanelTest {
    @Test
    void emptyStateDoesNotCrash() {
        ReportingClustersPanel panel = new ReportingClustersPanel();
        panel.applyClustersJson("""
                {
                  "empty": true,
                  "emptyMessage": "No unique-error clusters yet.",
                  "clusters": []
                }
                """);
        assertEquals(0, panel.clustersTable().getRowCount());
        assertTrue(panel.statusLabel().getText().contains("unique-error"));
    }

    @Test
    void sharedSignatureShowsImpactedTests() {
        ReportingClustersPanel panel = new ReportingClustersPanel();
        panel.applyClustersJson("""
                {
                  "empty": false,
                  "clusterCount": 1,
                  "impactedTestCount": 2,
                  "clusters": [
                    {
                      "signatureKey": "fp-shared",
                      "displayError": "Unable to locate element: #login-btn",
                      "impactedCount": 2,
                      "impactedTests": [
                        {"historyId": "hist-login", "name": "loginTest", "status": "failed"},
                        {"historyId": "hist-checkout", "name": "checkoutTest", "status": "failed"}
                      ]
                    }
                  ]
                }
                """);
        JTable table = panel.clustersTable();
        assertEquals(1, table.getRowCount());
        assertEquals("fp-shared", table.getValueAt(0, 0));
        assertEquals("2", table.getValueAt(0, 2));
        assertTrue(table.getValueAt(0, 3).toString().contains("loginTest"));
        assertTrue(table.getValueAt(0, 3).toString().contains("checkoutTest"));
        assertTrue(panel.statusLabel().getText().contains("1 signature"));
    }

    @Test
    void distinctSignaturesShowSeparateRows() {
        ReportingClustersPanel panel = new ReportingClustersPanel();
        panel.applyClustersJson("""
                {
                  "empty": false,
                  "clusterCount": 2,
                  "impactedTestCount": 2,
                  "clusters": [
                    {
                      "signatureKey": "fp-a",
                      "displayError": "locator",
                      "impactedCount": 1,
                      "impactedTests": [{"historyId": "hist-a", "name": "a"}]
                    },
                    {
                      "signatureKey": "fp-b",
                      "displayError": "timeout",
                      "impactedCount": 1,
                      "impactedTests": [{"historyId": "hist-b", "name": "b"}]
                    }
                  ]
                }
                """);
        assertEquals(2, panel.clustersTable().getRowCount());
        assertEquals("fp-a", panel.clustersTable().getValueAt(0, 0));
        assertEquals("fp-b", panel.clustersTable().getValueAt(1, 0));
    }
}
