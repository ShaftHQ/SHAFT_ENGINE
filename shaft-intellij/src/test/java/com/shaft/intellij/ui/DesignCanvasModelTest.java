package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignCanvasModelTest {

    @Test
    void fromJsonBlankAndNonJsonKeepsFallbackMessage() {
        DesignCanvasModel blank = DesignCanvasModel.fromJson(null);
        assertEquals("No analysis result.", blank.message());
        assertEquals("Draft", blank.badgeText());
        assertTrue(blank.packStrip().contains("Actor: —"));

        DesignCanvasModel plain = DesignCanvasModel.fromJson("not-json");
        assertEquals("not-json", plain.message());
        assertFalse(plain.acceptEnabled());
        assertEquals("", plain.acceptedGapIdsArgument());
    }

    @Test
    void fromJsonParsesPackGapsOraclesAndWaivableBlockingIds() {
        String raw = """
                {
                  "status": "needs_review",
                  "message": "Review gaps",
                  "blockingCount": 2,
                  "gherkinGenerationAllowed": true,
                  "pack": {
                    "actor": "Shopper",
                    "outcome": "Checkout",
                    "acceptanceCriteria": [
                      {"id": "AC-1", "text": "Pay succeeds"},
                      "skip-me",
                      {"id": "AC-2", "text": "Receipt email"}
                    ]
                  },
                  "gaps": [
                    {
                      "id": "G1",
                      "kind": "missing",
                      "severity": "blocking",
                      "rank": "1",
                      "tracedAcIds": ["AC-1", "AC-2"],
                      "question": "What payment?",
                      "waivable": true,
                      "accepted": false
                    },
                    {
                      "id": "G2",
                      "kind": "ambiguous",
                      "severity": "blocking",
                      "rank": "2",
                      "question": "Already accepted",
                      "waivable": true,
                      "accepted": true
                    },
                    {
                      "id": "G3",
                      "kind": "info",
                      "severity": "info",
                      "rank": "3",
                      "question": "Not blocking",
                      "waivable": true,
                      "accepted": false
                    },
                    "skip-gap"
                  ],
                  "oracles": [
                    {"acId": "AC-1", "oracle": "HTTP 200", "evidence": "response"},
                    "skip-oracle"
                  ]
                }
                """;
        DesignCanvasModel model = DesignCanvasModel.fromJson(raw);

        assertEquals("needs_review", model.status());
        assertEquals("Review gaps", model.message());
        assertEquals("Shopper", model.actor());
        assertEquals("Checkout", model.outcome());
        assertEquals(2, model.blockingCount());
        assertTrue(model.gherkinGenerationAllowed());
        assertEquals(2, model.acRows().size());
        assertEquals("AC-1", model.acRows().get(0)[0]);
        assertEquals(3, model.gapRows().size());
        assertEquals("yes", model.gapRows().get(1)[6]);
        assertEquals(1, model.oracleRows().size());
        assertEquals(List.of("G1"), model.waivableBlockingIds());
        assertTrue(model.acceptEnabled());
        assertEquals("G1", model.acceptedGapIdsArgument());
        assertEquals("needs review · blocking 2", model.badgeText());
        assertTrue(model.packStrip().contains("AC 2"));
        assertTrue(model.gapRows().get(0)[4].contains("AC-1"));
    }

    @Test
    void lexiconExampleLintGapMapAndCoverageParsers() {
        String analysis = """
                {
                  "status": "ready",
                  "suggestions": ["login", 3, "checkout"],
                  "rows": [
                    {"id": "E1", "kind": "positive", "cells": ["a", "b"]},
                    "skip",
                    {"id": "E2", "kind": "negative"}
                  ],
                  "findings": [
                    {"id": "L1", "level": "error", "rule": "R1", "message": "bad"},
                    "skip",
                    {"id": "L2", "level": "warn", "rule": "R2", "message": "soft"}
                  ],
                  "acceptBlocked": true,
                  "steps": [
                    {
                      "stepText": "When I pay",
                      "classification": "action",
                      "shaftType": "ElementActions",
                      "shaftMethod": "click",
                      "note": "ok"
                    },
                    "skip"
                  ],
                  "covered": ["AC-1", 9],
                  "uncovered": ["AC-2"],
                  "waived": [
                    {"id": "AC-3", "reason": "risk accepted"},
                    "skip"
                  ]
                }
                """;

        assertEquals(List.of("login", "checkout"), DesignCanvasModel.lexiconSuggestions(analysis));
        assertTrue(DesignCanvasModel.lexiconSuggestions("[]").isEmpty());
        assertTrue(DesignCanvasModel.lexiconSuggestions("{").isEmpty());

        List<String[]> examples = DesignCanvasModel.exampleRows(analysis);
        assertEquals(2, examples.size());
        assertEquals("E1", examples.get(0)[0]);
        assertTrue(examples.get(0)[2].contains("a"));
        assertEquals("", examples.get(1)[2]);
        assertTrue(DesignCanvasModel.exampleRows("{}").isEmpty());

        List<String[]> lint = DesignCanvasModel.lintRows(analysis);
        assertEquals(2, lint.size());
        assertEquals("L1", lint.get(0)[0]);
        assertTrue(DesignCanvasModel.lintAcceptBlocked(analysis));
        assertFalse(DesignCanvasModel.lintAcceptBlocked("{\"status\":\"x\"}"));

        List<String[]> gaps = DesignCanvasModel.gapMapRows(analysis);
        assertEquals(1, gaps.size());
        assertEquals("When I pay", gaps.get(0)[0]);
        assertEquals("click", gaps.get(0)[3]);
        assertTrue(DesignCanvasModel.gapMapRows("{}").isEmpty());

        List<String[]> coverage = DesignCanvasModel.coverageRows(analysis);
        assertEquals(3, coverage.size());
        assertEquals("covered", coverage.get(0)[1]);
        assertEquals("uncovered", coverage.get(1)[1]);
        assertEquals("waived", coverage.get(2)[1]);
        assertEquals("risk accepted", coverage.get(2)[2]);
        assertTrue(DesignCanvasModel.coverageRows("not-json").isEmpty());
    }

    @Test
    void handoffAndReadinessParsers() {
        String raw = """
                {
                  "status": "handoff_ready",
                  "message": "Ready",
                  "optionalUrl": "https://example.test/story",
                  "handoffAllowed": true,
                  "unmetConditions": ["need-login", 7],
                  "scenarios": ["happy path"],
                  "automationPrefill": {"browser": "chrome", "nested": {"x": 1}}
                }
                """;
        List<String[]> handoff = DesignCanvasModel.handoffRows(raw);
        assertTrue(handoff.stream().anyMatch(row -> "handoff_ready".equals(row[0])));
        assertTrue(handoff.stream().anyMatch(row -> "unmet".equals(row[0]) && "need-login".equals(row[1])));
        assertTrue(handoff.stream().anyMatch(row -> "scenario".equals(row[0])));
        assertTrue(handoff.stream().anyMatch(row -> "prefill".equals(row[0]) && "browser".equals(row[1])));
        assertTrue(DesignCanvasModel.handoffAllowed(raw));
        assertFalse(DesignCanvasModel.handoffAllowed("{\"status\":\"x\"}"));
        assertTrue(DesignCanvasModel.handoffRows("plain").isEmpty());

        List<String[]> readiness = DesignCanvasModel.readinessRows(raw);
        assertEquals("true", readiness.get(0)[2]);
        assertTrue(readiness.stream().anyMatch(row -> "unmet".equals(row[0])));
        assertTrue(DesignCanvasModel.readinessRows("[]").isEmpty());
    }
}
