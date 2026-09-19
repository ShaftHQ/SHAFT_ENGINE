package com.shaft.mcp;

import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DesignGherkinDraftTest {
    private final DesignService service = new DesignService(McpWorkspacePolicy.of(Path.of(".")));

    @Test
    void vagueStoryDoesNotEmitGherkin() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/vague-story.txt"));
        McpDesignGherkinDraft draft = service.gherkinDraft(story, "", "", "");
        assertEquals(McpDesignGherkinDraft.STATUS_ERROR, draft.status());
        assertTrue(draft.feature().isBlank(), draft.feature());
        assertFalse(draft.wroteFiles());
    }

    @Test
    void completeCheckoutDraftsDeclarativeScenariosWithoutLocators() throws Exception {
        String story = Files.readString(Path.of("src/test/resources/fixtures/design/complete-checkout.txt"));
        McpDesignGherkinDraft draft = service.gherkinDraft(story, "", "", "");
        assertEquals(McpDesignGherkinDraft.STATUS_DRAFTED, draft.status(), draft.message());
        assertTrue(draft.feature().contains("Feature:"));
        assertTrue(draft.feature().contains("Given"));
        assertTrue(draft.feature().contains("When"));
        assertTrue(draft.feature().contains("Then"));
        assertTrue(draft.feature().contains("@AC-01"));
        assertFalse(draft.feature().toLowerCase().contains("xpath"));
        assertFalse(draft.feature().contains("By.xpath"));
        assertFalse(draft.wroteFiles());
    }
}
