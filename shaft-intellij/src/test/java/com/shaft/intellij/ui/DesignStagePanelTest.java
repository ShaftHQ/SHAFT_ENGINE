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
        assertNotNull(panel.gherkinDraftArea());
        assertNotNull(findByAccessibleName(panel, "Gherkin draft", JComponent.class));
        assertNotNull(panel.lexiconSuggestions());
        assertNotNull(panel.suggestPhrasesButton());
        assertNotNull(findByAccessibleName(panel, "Lexicon suggestions", JComponent.class));
        assertNotNull(panel.coverageTable());
        assertNotNull(panel.coverageButton());
        assertNotNull(findByAccessibleName(panel, "AC coverage", JTable.class));
        assertNotNull(panel.lintTable());
        assertNotNull(panel.lintButton());
        assertNotNull(findByAccessibleName(panel, "Gherkin lint", JTable.class));
    }

    @Test
    void draftJsonFillsEditableGherkinArea() {
        DesignStagePanel panel = new DesignStagePanel();
        panel.applyDraftJson("""
                {
                  "status": "drafted",
                  "message": "Gherkin draft ready for review. Not written to the repository.",
                  "feature": "Feature: Check out",
                  "wroteFiles": false
                }
                """);
        assertTrue(panel.gherkinDraftArea().getText().contains("Feature:"));
        assertTrue(panel.gherkinDraftArea().isEditable());
        assertFalse(panel.gherkinDraftArea().getText().toLowerCase().contains("xpath"));
    }

    @Test
    void lexiconJsonFillsSuggestionList() {
        DesignStagePanel panel = new DesignStagePanel();
        panel.applyLexiconJson("""
                {
                  "status": "ok",
                  "suggestions": ["the shopper is authenticated"]
                }
                """);
        assertEquals(1, panel.lexiconSuggestions().getModel().getSize());
        assertEquals("the shopper is authenticated", panel.lexiconSuggestions().getModel().getElementAt(0));
        assertFalse(panel.suggestPhrasesButton().isEnabled(), "headless panel has no project");
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

    @Test
    void coverageTableShowsCoveredUncoveredAndWaived() {
        DesignStagePanel panel = new DesignStagePanel();
        panel.applyCoverageJson("""
                {
                  "status": "blocked",
                  "message": "Uncovered AC or untagged scenarios block Ready until resolved or waived with a reason.",
                  "covered": ["AC-01", "AC-02"],
                  "uncovered": ["AC-03"],
                  "waived": [{"id": "AC-04", "reason": "out of v1"}],
                  "readyBlocked": true
                }
                """);
        assertEquals(4, panel.coverageTable().getRowCount());
        assertEquals("uncovered", panel.coverageTable().getValueAt(2, 1));
        assertEquals("waived", panel.coverageTable().getValueAt(3, 1));
        assertTrue(panel.statusBadge().getText().contains("Ready"));
        assertFalse(panel.coverageButton().isEnabled(), "headless panel has no project");
    }

    @Test
    void xpathLintDisablesAcceptControl() {
        DesignStagePanel panel = new DesignStagePanel();
        panel.applyLintJson("""
                {
                  "status": "blocked",
                  "message": "Error-level lint findings block accept until resolved or waived with a reason.",
                  "findings": [
                    {"id": "LINT-01", "level": "error", "rule": "click_xpath", "message": "When step uses click and xpath"}
                  ],
                  "acceptBlocked": true
                }
                """);
        assertEquals(1, panel.lintTable().getRowCount());
        assertEquals("error", panel.lintTable().getValueAt(0, 1));
        assertTrue(panel.statusBadge().getText().contains("block accept"));
        assertTrue(panel.lintAcceptBlocked());
        assertFalse(panel.acceptRiskButton().isEnabled(), "accept stays disabled while errors remain");
        panel.lintWaivedField().setText("LINT-01:accepted residual click");
        assertEquals("LINT-01:accepted residual click", panel.lintWaivedField().getText());
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

    @Test
    void applyAnalysisJsonDoesNotFillFluentGapMap() {
        // Issue #6012: gap-map payloads must use applyGapMapJson; analysis apply is a different shape.
        DesignStagePanel panel = new DesignStagePanel();
        panel.applyAnalysisJson("""
                {
                  "schemaVersion": "1.0",
                  "status": "ok",
                  "message": "analysis only",
                  "steps": [{
                    "stepText": "Then total should equal 1",
                    "classification": "mapped",
                    "shaftType": "Validations",
                    "shaftMethod": "assertThat"
                  }]
                }
                """);
        assertEquals(0, panel.fluentMapTable().getRowCount(),
                "analysis apply must not paint the fluent gap-map table");
    }

    @Test
    void fluentGapMapFillsTable() {
        DesignStagePanel panel = new DesignStagePanel();
        assertNotNull(panel.fluentMapButton());
        assertNotNull(panel.fluentMapTable());
        panel.applyGapMapJson("""
                {
                  "schemaVersion": "1.0",
                  "status": "ok",
                  "message": "Mapped 1 step(s).",
                  "wroteFiles": false,
                  "steps": [{
                    "stepText": "Then total should equal 1",
                    "classification": "mapped",
                    "shaftType": "Validations",
                    "shaftMethod": "assertThat",
                    "candidates": [],
                    "note": "Validations.assertThat().object(...).isEqualTo(...)"
                  }]
                }
                """);
        assertEquals(1, panel.fluentMapTable().getRowCount());
        assertEquals("mapped", panel.fluentMapTable().getValueAt(0, 1));
        assertEquals("Validations", panel.fluentMapTable().getValueAt(0, 2));
    }

    @Test
    void readinessPopulatesTable() {
        DesignStagePanel panel = new DesignStagePanel();
        assertNotNull(panel.readinessButton());
        assertNotNull(panel.readinessTable());
        panel.applyReadinessJson("""
                {
                  "schemaVersion": "1.0",
                  "status": "draft",
                  "message": "Ready conditions are unmet.",
                  "unmetConditions": ["accept: user has not accepted the pack"],
                  "handoffAllowed": false,
                  "wroteFiles": false
                }
                """);
        assertTrue(panel.readinessTable().getRowCount() >= 1);
        assertEquals("draft", panel.readinessTable().getValueAt(0, 0));
    }


    @Test
    void handoffPopulatesTable() {
        DesignStagePanel panel = new DesignStagePanel();
        assertNotNull(panel.handoffButton());
        assertNotNull(panel.handoffTable());
        panel.applyHandoffJson("""
                {
                  "schemaVersion": "1.0",
                  "status": "blocked",
                  "message": "Pack is not Ready; Automation handoff is disabled.",
                  "unmetConditions": ["accept: user has not accepted the pack"],
                  "scenarios": [],
                  "acceptanceCriteria": [],
                  "examples": [],
                  "gapMap": [],
                  "oracles": [],
                  "optionalUrl": "",
                  "automationPrefill": {},
                  "wroteFiles": false
                }
                """);
        assertTrue(panel.handoffTable().getRowCount() >= 1);
        assertEquals("blocked", panel.handoffTable().getValueAt(0, 0));
    }

}
