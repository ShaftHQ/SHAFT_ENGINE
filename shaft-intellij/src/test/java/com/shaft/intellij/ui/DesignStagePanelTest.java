package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import javax.accessibility.AccessibleContext;
import javax.swing.JComponent;
import javax.swing.JTable;
import java.awt.Component;
import java.awt.Container;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignStagePanelTest {
    @Test
    void canvasExposesRegisterInsteadOfJsonDump() {
        DesignStagePanel panel = new DesignStagePanel();
        assertNotNull(panel.storyArea());
        assertNotNull(panel.ingestButton());
        assertNotNull(panel.analyzeButton());
        assertNotNull(panel.acceptRiskButton());
        assertNotNull(panel.gherkinButton());
        assertNotNull(findByAccessibleName(panel, "Gap register", JTable.class));
        assertNotNull(findByAccessibleName(panel, "Evidence oracles", JTable.class));
        assertNull(findByAccessibleName(panel, "Ingested design pack", JComponent.class));
        assertFalse(panel.gherkinButton().isEnabled());
        assertFalse(panel.ingestButton().isEnabled());
    }

    @Test
    void needsQuestionsKeepsGherkinDisabled() {
        DesignStagePanel panel = new DesignStagePanel();
        panel.applyAnalysisJson("""
                {
                  "status": "needs_questions",
                  "message": "Needs questions",
                  "blockingCount": 1,
                  "gherkinGenerationAllowed": false,
                  "residualRiskAccepted": false,
                  "pack": {
                    "actor": "shopper",
                    "outcome": "check out",
                    "acceptanceCriteria": [{"id": "AC-01", "text": "Then cart updates"}]
                  },
                  "gaps": [{
                    "id": "GAP-01",
                    "kind": "omission",
                    "severity": "blocking",
                    "rank": 1,
                    "tracedAcIds": ["AC-01"],
                    "question": "What happens on decline?",
                    "waivable": true,
                    "accepted": false
                  }],
                  "oracles": []
                }
                """);
        assertFalse(panel.gherkinButton().isEnabled());
        assertEquals(1, panel.gapTable().getRowCount());
        assertTrue(panel.statusBadge().getText().contains("needs questions"));
        assertFalse(panel.acceptRiskButton().isEnabled(), "headless panel has no project so MCP actions stay off");
    }

    @Test
    void analysisCompleteLeavesGherkinDisabledWithoutAProject() {
        DesignStagePanel panel = new DesignStagePanel();
        panel.applyAnalysisJson("""
                {
                  "status": "analysis_complete",
                  "message": "Analysis complete",
                  "blockingCount": 0,
                  "gherkinGenerationAllowed": true,
                  "residualRiskAccepted": false,
                  "pack": {
                    "actor": "shopper",
                    "outcome": "check out",
                    "acceptanceCriteria": [{"id": "AC-01", "text": "Then cart updates"}]
                  },
                  "gaps": [],
                  "oracles": [{"acId": "AC-01", "oracle": "Observe cart", "evidence": "assertion"}]
                }
                """);
        assertFalse(panel.gherkinButton().isEnabled());
        assertTrue(panel.statusBadge().getText().contains("analysis complete"));
        assertEquals(0, panel.gapTable().getRowCount());
    }

    @Test
    void examplesTableShowsRowsAndDeleteControl() {
        DesignStagePanel panel = new DesignStagePanel();
        assertNotNull(panel.examplesTable());
        assertNotNull(panel.deleteExampleButton());
        panel.applyExamplesJson("""
                {
                  "status": "drafted",
                  "rows": [
                    {"id": "EX-01", "kind": "valid", "cells": ["gold", "SAVE10", "100", "15"]},
                    {"id": "EX-02", "kind": "invalid", "cells": ["none", "SAVE10", "100", "0"]},
                    {"id": "EX-03", "kind": "boundary", "cells": ["gold", "none", "50", "10"]}
                  ]
                }
                """);
        assertEquals(3, panel.examplesTable().getRowCount());
        panel.applyExamplesJson("""
                {
                  "status": "drafted",
                  "rows": [
                    {"id": "EX-01", "kind": "valid", "cells": ["gold", "SAVE10", "100", "15"]},
                    {"id": "EX-03", "kind": "boundary", "cells": ["gold", "none", "50", "10"]}
                  ]
                }
                """);
        assertEquals(2, panel.examplesTable().getRowCount());
        assertFalse(panel.deleteExampleButton().isEnabled(), "headless panel has no project");
    }

    private static <T extends JComponent> T findByAccessibleName(
            Component component, String accessibleName, Class<T> type) {
        if (type.isInstance(component) && accessibleName.equals(accessibleName((JComponent) component))) {
            return type.cast(component);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                T found = findByAccessibleName(child, accessibleName, type);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static String accessibleName(JComponent component) {
        AccessibleContext context = component.getAccessibleContext();
        return context == null ? "" : context.getAccessibleName();
    }
}
