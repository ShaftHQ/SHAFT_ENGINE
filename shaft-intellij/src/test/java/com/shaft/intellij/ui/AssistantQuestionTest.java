package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link AssistantQuestion#detect}, the pure detection/parsing step behind issue #3674's
 * selectable-answer clarifying-question card: a genuine multiple-choice clarifying question is
 * signaled by a trailing fenced {@code ```shaft-options} block containing a JSON array of short
 * option labels, distinguishing it from ordinary narrative prose that happens to end a turn.
 */
class AssistantQuestionTest {

    @Test
    void detectsAQuestionWithOptionsAndStripsTheFenceFromTheDisplayedMarkdown() {
        String markdown = """
                Want me to actually run through a recording now?

                ```shaft-options
                ["Use the sample page", "I'll give you a URL"]
                ```
                """;

        AssistantQuestion question = AssistantQuestion.detect(markdown);

        assertAll(
                () -> assertTrue(question != null, "a valid options fence should be detected"),
                () -> assertEquals(List.of("Use the sample page", "I'll give you a URL"), question.options()),
                () -> assertTrue(question.promptMarkdown().contains("Want me to actually run through a recording now?")),
                () -> assertFalse(question.promptMarkdown().contains("shaft-options"),
                        "the raw fence marker must not leak into the displayed markdown"),
                () -> assertFalse(question.promptMarkdown().contains("```"),
                        "the fenced block itself must be stripped, not just its language tag"));
    }

    @Test
    void returnsNullForOrdinaryProseWithNoOptionsFence() {
        String markdown = "Here is a plain narrative answer with no clarifying question at all.";

        assertNull(AssistantQuestion.detect(markdown));
    }

    @Test
    void returnsNullWhenTheFencedBlockIsNotValidJson() {
        String markdown = """
                Pick one:

                ```shaft-options
                not valid json
                ```
                """;

        assertNull(AssistantQuestion.detect(markdown));
    }

    @Test
    void returnsNullWhenFewerThanTwoOptionsAreOffered() {
        String markdown = """
                Only one choice, so this isn't really multiple choice:

                ```shaft-options
                ["Just this one"]
                ```
                """;

        assertNull(AssistantQuestion.detect(markdown));
    }

    @Test
    void returnsNullWhenMoreThanSixOptionsAreOffered() {
        String markdown = """
                Too many choices to render as chips:

                ```shaft-options
                ["A", "B", "C", "D", "E", "F", "G"]
                ```
                """;

        assertNull(AssistantQuestion.detect(markdown));
    }

    @Test
    void blankOptionsAreFilteredAndDoNotCountTowardTheMinimum() {
        String markdown = """
                Pick one:

                ```shaft-options
                ["Use the sample page", "  ", "", "I'll give you a URL"]
                ```
                """;

        AssistantQuestion question = AssistantQuestion.detect(markdown);

        assertAll(
                () -> assertTrue(question != null),
                () -> assertEquals(List.of("Use the sample page", "I'll give you a URL"), question.options()));
    }

    @Test
    void returnsNullForBlankInput() {
        assertAll(
                () -> assertNull(AssistantQuestion.detect(null)),
                () -> assertNull(AssistantQuestion.detect("")),
                () -> assertNull(AssistantQuestion.detect("   ")));
    }

    // -- detectStructuredLine: issue #3719's preferred structured protocol -----------------------
    //
    // A single trailing JSON object {"shaft-question": "...", "shaft-options": [...]} carries both
    // the question text and the options atomically, unlike the fence's two-part "prose before, JSON
    // array inside a separately-tagged fence after" convention -- one fewer place for a model's
    // output to drift out of shape. AssistantLocalAgentRunner's StructuredStreamParser is the
    // primary caller (recognizing it at the terminal-event boundary, before any SHAFT-appended
    // suffix lines); detect() (the fence) remains the documented fallback, unchanged, below.

    @Test
    void detectsAStructuredQuestionLineAndStripsItFromTheRemainingText() {
        String text = "Want me to actually run through a recording now?\n"
                + "{\"shaft-question\": \"Want me to actually run through a recording now?\", "
                + "\"shaft-options\": [\"Use the sample page\", \"I'll give you a URL\"]}";

        AssistantQuestion question = AssistantQuestion.detectStructuredLine(text);

        assertAll(
                () -> assertTrue(question != null, "a valid trailing structured line should be detected"),
                () -> assertEquals(List.of("Use the sample page", "I'll give you a URL"), question.options()),
                () -> assertEquals("Want me to actually run through a recording now?", question.promptMarkdown()),
                () -> assertFalse(question.promptMarkdown().contains("shaft-question"),
                        "the raw structured line must not leak into the remaining text"));
    }

    @Test
    void structuredLineFallsBackToItsOwnQuestionTextWhenThereIsNoLeadingProse() {
        // A chat bubble persisted as "" would strand ShaftAssistantPanel's streaming placeholder
        // (replaceLocalAgentStreamPlaceholder treats a blank message as "leave it untouched"), so a
        // model that skips leading prose still gets a real, readable bubble: the marker's own
        // question text.
        String text = "{\"shaft-question\": \"Pick one:\", \"shaft-options\": [\"Yes\", \"No\"]}";

        AssistantQuestion question = AssistantQuestion.detectStructuredLine(text);

        assertAll(
                () -> assertTrue(question != null),
                () -> assertEquals(List.of("Yes", "No"), question.options()),
                () -> assertEquals("Pick one:", question.promptMarkdown()));
    }

    @Test
    void structuredLineReturnsNullForOrdinaryProseWithNoTrailingJsonObject() {
        String text = "Here is a plain narrative answer with no clarifying question at all.";

        assertNull(AssistantQuestion.detectStructuredLine(text));
    }

    @Test
    void structuredLineReturnsNullWhenTheTrailingLineIsNotValidJson() {
        String text = "Pick one:\nnot valid json";

        assertNull(AssistantQuestion.detectStructuredLine(text));
    }

    @Test
    void structuredLineReturnsNullWhenTheQuestionKeyIsMissingOrBlank() {
        assertAll(
                () -> assertNull(AssistantQuestion.detectStructuredLine(
                        "{\"shaft-options\": [\"Yes\", \"No\"]}")),
                () -> assertNull(AssistantQuestion.detectStructuredLine(
                        "{\"shaft-question\": \"  \", \"shaft-options\": [\"Yes\", \"No\"]}")));
    }

    @Test
    void structuredLineReturnsNullWhenTheOptionsKeyIsMissingOrNotAnArray() {
        assertAll(
                () -> assertNull(AssistantQuestion.detectStructuredLine(
                        "{\"shaft-question\": \"Pick one:\"}")),
                () -> assertNull(AssistantQuestion.detectStructuredLine(
                        "{\"shaft-question\": \"Pick one:\", \"shaft-options\": \"Yes\"}")));
    }

    @Test
    void structuredLineReturnsNullWhenFewerThanTwoOptionsAreOffered() {
        String text = "{\"shaft-question\": \"Only one choice:\", \"shaft-options\": [\"Just this one\"]}";

        assertNull(AssistantQuestion.detectStructuredLine(text));
    }

    @Test
    void structuredLineReturnsNullWhenMoreThanSixOptionsAreOffered() {
        String text = "{\"shaft-question\": \"Too many:\", "
                + "\"shaft-options\": [\"A\", \"B\", \"C\", \"D\", \"E\", \"F\", \"G\"]}";

        assertNull(AssistantQuestion.detectStructuredLine(text));
    }

    @Test
    void structuredLineBlankOptionsAreFilteredAndDoNotCountTowardTheMinimum() {
        String text = "{\"shaft-question\": \"Pick one:\", "
                + "\"shaft-options\": [\"Use the sample page\", \"  \", \"\", \"I'll give you a URL\"]}";

        AssistantQuestion question = AssistantQuestion.detectStructuredLine(text);

        assertAll(
                () -> assertTrue(question != null),
                () -> assertEquals(List.of("Use the sample page", "I'll give you a URL"), question.options()));
    }

    @Test
    void structuredLineReturnsNullForBlankInput() {
        assertAll(
                () -> assertNull(AssistantQuestion.detectStructuredLine(null)),
                () -> assertNull(AssistantQuestion.detectStructuredLine("")),
                () -> assertNull(AssistantQuestion.detectStructuredLine("   ")));
    }
}
