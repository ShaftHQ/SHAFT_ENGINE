package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LocatorPlaygroundPanelTest {
    @Test
    void accessibleNameAndActionsPresent() {
        LocatorPlaygroundPanel panel = new LocatorPlaygroundPanel(null);
        assertEquals(LocatorPlaygroundPanel.ACCESSIBLE_NAME,
                panel.getAccessibleContext().getAccessibleName());
        assertEquals("Refresh pick", panel.refreshButton().getAccessibleContext().getAccessibleName());
        assertEquals("Copy SHAFT locator", panel.copyButton().getAccessibleContext().getAccessibleName());
    }

    @Test
    void roleRanksAboveXpathInDisplayedFixtureAndCopyIsShaftHasRole() {
        LocatorPlaygroundPanel panel = new LocatorPlaygroundPanel(null);
        boolean applied = panel.applyToolOutput("""
                {
                  "snippet": "SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasNormalizedText(\\"Pay now\\").build()",
                  "ranked": [
                    {
                      "strategy": "ROLE",
                      "expression": "button:Pay now",
                      "uniquenessCount": 1,
                      "score": 140,
                      "snippet": "SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasNormalizedText(\\"Pay now\\").build()"
                    },
                    {
                      "strategy": "XPATH",
                      "expression": "//button[normalize-space(.)=\\"Pay now\\"]",
                      "uniquenessCount": 1,
                      "score": 35,
                      "snippet": "By.xpath(\\"//button[normalize-space(.)=\\\\\\"Pay now\\\\\\"]\\")"
                    }
                  ]
                }
                """);
        assertTrue(applied);
        List<LocatorPlaygroundPanel.RankedRow> rows = panel.model().rows();
        assertEquals(2, rows.size());
        assertEquals("ROLE", rows.get(0).strategy());
        assertEquals("XPATH", rows.get(1).strategy());
        assertTrue(rows.get(0).unique());
        assertTrue(panel.selectedSnippet().startsWith("SHAFT.GUI.Locator.hasRole(Role.BUTTON)"));
        assertFalse(panel.selectedSnippet().contains("SHAFT.GUI.Locator.xpath"));
        panel.copyButton().doClick();
        assertTrue(panel.statusLabel().getAccessibleContext().getAccessibleDescription()
                .toLowerCase().contains("copied"));
    }

    @Test
    void nonUniqueCandidatesAreMarked() {
        LocatorPlaygroundPanel panel = new LocatorPlaygroundPanel(null);
        panel.applyPickResult("SHAFT.GUI.Locator.cssSelector(\"button\")", List.of(
                new LocatorPlaygroundPanel.RankedRow(
                        1, "CSS", "button", 4, 40, "SHAFT.GUI.Locator.cssSelector(\"button\")")));
        assertEquals("not unique (4)", panel.model().getValueAt(0, 3));
        assertFalse(panel.model().rowAt(0).unique());
    }

    @Test
    void emptyPickFailsClosedWithStatus() {
        LocatorPlaygroundPanel panel = new LocatorPlaygroundPanel(null);
        assertFalse(panel.applyToolOutput("{\"snippet\":\"\",\"ranked\":[]}"));
        assertTrue(panel.statusLabel().getText().contains("No live pick")
                || panel.statusLabel().getAccessibleContext().getAccessibleDescription()
                .contains("No live pick"));
    }

    @Test
    void unwrapsMcpContentEnvelope() {
        LocatorPlaygroundPanel panel = new LocatorPlaygroundPanel(null);
        String inner = "{"
                + "\"snippet\":\"SHAFT.GUI.Locator.id(\\\"x\\\")\","
                + "\"ranked\":[{"
                + "\"strategy\":\"ID\",\"expression\":\"x\",\"uniquenessCount\":1,\"score\":100,"
                + "\"snippet\":\"SHAFT.GUI.Locator.id(\\\"x\\\")\""
                + "}]}";
        com.google.gson.JsonObject textNode = new com.google.gson.JsonObject();
        textNode.addProperty("type", "text");
        textNode.addProperty("text", inner);
        com.google.gson.JsonArray content = new com.google.gson.JsonArray();
        content.add(textNode);
        com.google.gson.JsonObject envelope = new com.google.gson.JsonObject();
        envelope.add("content", content);
        assertTrue(panel.applyToolOutput(envelope.toString()));
        assertEquals("ID", panel.model().rowAt(0).strategy());
    }
}
