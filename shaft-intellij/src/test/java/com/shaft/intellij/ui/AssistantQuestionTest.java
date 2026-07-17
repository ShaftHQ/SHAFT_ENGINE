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
}
