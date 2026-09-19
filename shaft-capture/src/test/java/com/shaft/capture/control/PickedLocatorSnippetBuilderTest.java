package com.shaft.capture.control;

import com.shaft.capture.model.LocatorCandidate;
import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PickedLocatorSnippetBuilderTest {
    @Test
    void roleExpressionBecomesHasRoleBuilder() {
        String snippet = PickedLocatorSnippetBuilder.snippet(candidate(
                LocatorCandidate.LocatorStrategy.ROLE, "button:Pay now"));
        assertEquals(
                "SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasNormalizedText(\"Pay now\").build()",
                snippet);
        assertFalse(snippet.contains("SHAFT.GUI.Locator.xpath"));
    }

    @Test
    void xpathStaysByXpathNotShaftLocatorXpath() {
        String snippet = PickedLocatorSnippetBuilder.snippet(candidate(
                LocatorCandidate.LocatorStrategy.XPATH, "//button[@id=\"pay\"]"));
        assertEquals("By.xpath(\"//button[@id=\\\"pay\\\"]\")", snippet);
        assertFalse(snippet.contains("SHAFT.GUI.Locator.xpath"));
    }

    @Test
    void idUsesShaftBuilder() {
        assertEquals(
                "SHAFT.GUI.Locator.id(\"submit\")",
                PickedLocatorSnippetBuilder.snippet(candidate(LocatorCandidate.LocatorStrategy.ID, "submit")));
    }

    @Test
    void roleSnippetHelperMapsKnownRoles() {
        assertTrue(PickedLocatorSnippetBuilder.roleSnippet("link:Docs")
                .startsWith("SHAFT.GUI.Locator.hasRole(Role.LINK)"));
        assertTrue(PickedLocatorSnippetBuilder.roleSnippet("unknown:X")
                .contains("containsText"));
    }

    private static LocatorCandidate candidate(LocatorCandidate.LocatorStrategy strategy, String expression) {
        return new LocatorCandidate(strategy, expression, 1, true, true, Set.of());
    }
}
