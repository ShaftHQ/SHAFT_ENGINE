package com.shaft.capture.control;

import com.shaft.capture.model.LocatorCandidate;
import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class PickedLocatorSnippetBuilderTest {

    @Test
    void idRendersThroughTheShaftLocatorBuilder() {
        assertEquals(
                "SHAFT.GUI.Locator.hasAnyTagName().hasId(\"username\").build()",
                PickedLocatorSnippetBuilder.snippet(candidate(LocatorCandidate.LocatorStrategy.ID, "username")));
    }

    @Test
    void nameRendersAsAStableAttributeOnTheBuilder() {
        assertEquals(
                "SHAFT.GUI.Locator.hasAnyTagName().hasAttribute(\"name\", \"email\").build()",
                PickedLocatorSnippetBuilder.snippet(candidate(LocatorCandidate.LocatorStrategy.NAME, "email")));
    }

    @Test
    void hashCssRendersAsAnIdBuilderCall() {
        assertEquals(
                "SHAFT.GUI.Locator.hasAnyTagName().hasId(\"login\").build()",
                PickedLocatorSnippetBuilder.snippet(candidate(LocatorCandidate.LocatorStrategy.CSS, "#login")));
    }

    @Test
    void classCssRendersAsAClassBuilderCall() {
        assertEquals(
                "SHAFT.GUI.Locator.hasAnyTagName().hasClass(\"mw-logo\").build()",
                PickedLocatorSnippetBuilder.snippet(candidate(LocatorCandidate.LocatorStrategy.CSS, ".mw-logo")));
    }

    @Test
    void xpathRendersAsNativeRelativeXpath() {
        assertEquals(
                "By.xpath(\"//button[@type='submit']\")",
                PickedLocatorSnippetBuilder.snippet(
                        candidate(LocatorCandidate.LocatorStrategy.XPATH, "//button[@type='submit']")));
    }

    @Test
    void neverEmitsBannedShaftLocatorFactories() {
        String snippet = PickedLocatorSnippetBuilder.snippet(
                candidate(LocatorCandidate.LocatorStrategy.ID, "username"));
        assertFalse(snippet.contains("SHAFT.GUI.Locator.id("));
        assertFalse(snippet.contains("SHAFT.GUI.Locator.name("));
        assertFalse(snippet.contains("SHAFT.GUI.Locator.cssSelector("));
        assertFalse(snippet.contains("SHAFT.GUI.Locator.xpath("));
    }

    private static LocatorCandidate candidate(LocatorCandidate.LocatorStrategy strategy, String expression) {
        return new LocatorCandidate(strategy, expression, 1, true, true, Set.of());
    }
}
