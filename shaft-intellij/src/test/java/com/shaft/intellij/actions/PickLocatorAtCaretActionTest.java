package com.shaft.intellij.actions;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Pins {@link PickLocatorAtCaretAction}'s pure logic: extracting a locator snippet out of a
 * {@code capture_pick_locator} tool result, classifying that result into an actionable outcome,
 * and clamping the insertion offset against a document that may have changed size while the async
 * MCP round trip was in flight. The IntelliJ platform pieces (PSI gating, {@code WriteCommandAction},
 * caret access) are not covered here; this module has no {@code BasePlatformTestCase} fixtures.
 */
class PickLocatorAtCaretActionTest {
    @Test
    void extractSnippetReadsADirectSnippetField() {
        String output = "{\"snippet\":\"SHAFT.GUI.Locator...byId(\\\"submit\\\")\",\"ranked\":[]}";

        assertEquals("SHAFT.GUI.Locator...byId(\"submit\")", PickLocatorAtCaretAction.extractSnippet(output));
    }

    @Test
    void extractSnippetUnwrapsAnMcpContentEnvelope() {
        // Built with Gson rather than a hand-escaped literal: the real payload is a JSON string
        // (the tool's raw record) nested inside a "text" field of the MCP content envelope, and
        // hand double-escaping that by hand is exactly the kind of mistake this test should catch.
        JsonObject innerRecord = new JsonObject();
        innerRecord.addProperty("snippet", "SHAFT.GUI.Locator...byId(\"submit\")");
        innerRecord.add("ranked", new JsonArray());

        JsonObject contentEntry = new JsonObject();
        contentEntry.addProperty("type", "text");
        contentEntry.addProperty("text", innerRecord.toString());

        JsonArray content = new JsonArray();
        content.add(contentEntry);

        JsonObject envelope = new JsonObject();
        envelope.add("content", content);
        envelope.addProperty("isError", false);

        assertEquals("SHAFT.GUI.Locator...byId(\"submit\")",
                PickLocatorAtCaretAction.extractSnippet(envelope.toString()));
    }

    @Test
    void extractSnippetReturnsNullForMalformedJson() {
        assertNull(PickLocatorAtCaretAction.extractSnippet("not json"));
    }

    @Test
    void extractSnippetReturnsNullForBlankOutput() {
        assertNull(PickLocatorAtCaretAction.extractSnippet(""));
        assertNull(PickLocatorAtCaretAction.extractSnippet(null));
    }

    @Test
    void extractSnippetReturnsNullWhenNoSnippetFieldIsPresent() {
        assertNull(PickLocatorAtCaretAction.extractSnippet("{\"ranked\":[]}"));
    }

    @Test
    void classifyReturnsSnippetOutcomeForANonBlankSnippet() {
        ShaftMcpToolResult result = ShaftMcpToolResult.success("{\"snippet\":\"SHAFT.GUI.Locator...byId(\\\"a\\\")\"}");

        PickLocatorAtCaretAction.PickOutcome outcome = PickLocatorAtCaretAction.classify(result);

        assertEquals(PickLocatorAtCaretAction.PickOutcomeKind.SNIPPET, outcome.kind());
        assertEquals("SHAFT.GUI.Locator...byId(\"a\")", outcome.detail());
    }

    @Test
    void classifyReturnsNoPickOutcomeForABlankSnippet() {
        ShaftMcpToolResult result = ShaftMcpToolResult.success("{\"snippet\":\"\",\"ranked\":[]}");

        PickLocatorAtCaretAction.PickOutcome outcome = PickLocatorAtCaretAction.classify(result);

        assertEquals(PickLocatorAtCaretAction.PickOutcomeKind.NO_PICK, outcome.kind());
    }

    @Test
    void classifyReturnsNoPickOutcomeWhenNoCandidatesWereEverPicked() {
        ShaftMcpToolResult result = ShaftMcpToolResult.success("{\"ranked\":[]}");

        PickLocatorAtCaretAction.PickOutcome outcome = PickLocatorAtCaretAction.classify(result);

        assertEquals(PickLocatorAtCaretAction.PickOutcomeKind.NO_PICK, outcome.kind());
    }

    @Test
    void classifyReturnsToolFailureOutcomeForAFailedResult() {
        ShaftMcpToolResult result = ShaftMcpToolResult.failure("MCP connection is disconnected.");

        PickLocatorAtCaretAction.PickOutcome outcome = PickLocatorAtCaretAction.classify(result);

        assertEquals(PickLocatorAtCaretAction.PickOutcomeKind.TOOL_FAILURE, outcome.kind());
        assertEquals("MCP connection is disconnected.", outcome.detail());
    }

    @Test
    void classifyReturnsToolFailureOutcomeForANullResult() {
        PickLocatorAtCaretAction.PickOutcome outcome = PickLocatorAtCaretAction.classify(null);

        assertEquals(PickLocatorAtCaretAction.PickOutcomeKind.TOOL_FAILURE, outcome.kind());
    }

    @Test
    void resolveInsertionOffsetKeepsAnInBoundsOffsetUnchanged() {
        assertEquals(5, PickLocatorAtCaretAction.resolveInsertionOffset(5, 10));
    }

    @Test
    void resolveInsertionOffsetClampsANegativeOffsetToZero() {
        assertEquals(0, PickLocatorAtCaretAction.resolveInsertionOffset(-3, 10));
    }

    @Test
    void resolveInsertionOffsetClampsAnOffsetPastTheDocumentEndToTheDocumentLength() {
        assertEquals(10, PickLocatorAtCaretAction.resolveInsertionOffset(42, 10));
    }
}
