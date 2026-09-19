package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignExamplesTest {
    private final DesignService service = new DesignService(McpWorkspacePolicy.of(Path.of(".")));

    @Test
    void discountFixtureYieldsAtLeastThreeExampleRows() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/discount-rule.txt"));
        McpDesignExamples examples = service.examples(story, "", "", "", "");
        assertEquals(McpDesignExamples.STATUS_DRAFTED, examples.status(), examples.message());
        assertTrue(examples.rows().size() >= 3, examples.rows().toString());
        assertTrue(examples.outline().contains("Scenario Outline"));
        assertTrue(examples.outline().contains("Examples:"));
        assertFalse(examples.wroteFiles());
    }

    @Test
    void droppingARowUpdatesTheDraft() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/discount-rule.txt"));
        McpDesignExamples first = service.examples(story, "", "", "", "");
        String drop = first.rows().get(1).id();
        McpDesignExamples second = service.examples(story, "", "", "", drop);
        assertEquals(first.rows().size() - 1, second.rows().size());
        assertFalse(second.rows().stream().map(McpDesignExampleRow::id).collect(Collectors.toSet()).contains(drop));
        assertFalse(second.wroteFiles());
    }

    @Test
    void completeCheckoutStaysScenariosNotAnOutline() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/complete-checkout.txt"));
        McpDesignExamples examples = service.examples(story, "", "", "", "");
        assertEquals(McpDesignExamples.STATUS_SCENARIO, examples.status(), examples.message());
        assertTrue(examples.rows().isEmpty());
        assertFalse(examples.wroteFiles());
    }

    @Test
    void vagueStoryIsBlocked() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/vague-story.txt"));
        McpDesignExamples examples = service.examples(story, "", "", "", "");
        assertEquals(McpDesignExamples.STATUS_ERROR, examples.status());
        assertTrue(examples.rows().isEmpty());
        assertFalse(examples.wroteFiles());
    }
}
