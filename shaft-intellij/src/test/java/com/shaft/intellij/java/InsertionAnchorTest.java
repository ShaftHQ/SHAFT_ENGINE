package com.shaft.intellij.java;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Pins the shared "where does inserted SHAFT code go" mental model (issue #3662): the caret's
 * enclosing Java method when the caret resolves to one, otherwise the file open in the editor,
 * otherwise no anchor at all. {@code RecordShaftFlowHereAction}, {@code
 * ShaftAssistantPanel#insertReviewIntoOpenFile}, and {@code GuidedWorkflowPanel#insertCodeAtCaret}
 * previously computed three different, inconsistently-scoped answers to this question depending on
 * which tab the user started from -- this pure choice function is now the single source of truth.
 */
class InsertionAnchorTest {
    @Test
    void choosesTheCaretMethodWhenTheCaretResolvesToOne() {
        JavaTargetContext caretContext = new JavaTargetContext(
                "src/test/java/LoginTest.java", "tests", "LoginTest", "logsIn");

        InsertionAnchor anchor = InsertionAnchor.choose(caretContext, "src/test/java/OtherOpenFile.java");

        assertEquals("src/test/java/LoginTest.java", anchor.targetSourcePath());
        assertEquals("logsIn", anchor.insertAfter());
        assertEquals("tests.LoginTest#logsIn", anchor.displayName());
    }

    @Test
    void fallsBackToTheOpenEditorFileWhenTheCaretDoesNotResolve() {
        InsertionAnchor anchor = InsertionAnchor.choose(null, "src/test/java/OtherOpenFile.java");

        assertEquals("src/test/java/OtherOpenFile.java", anchor.targetSourcePath());
        assertEquals("", anchor.insertAfter());
        assertEquals("OtherOpenFile.java", anchor.displayName());
    }

    @Test
    void fallsBackToTheOpenEditorFileUsingBackslashPathsToo() {
        InsertionAnchor anchor = InsertionAnchor.choose(null, "C:\\project\\src\\OtherOpenFile.java");

        assertEquals("OtherOpenFile.java", anchor.displayName());
    }

    @Test
    void returnsNullWhenNeitherACaretNorAnOpenFileIsAvailable() {
        assertNull(InsertionAnchor.choose(null, null));
        assertNull(InsertionAnchor.choose(null, ""));
        assertNull(InsertionAnchor.choose(null, "   "));
    }
}
