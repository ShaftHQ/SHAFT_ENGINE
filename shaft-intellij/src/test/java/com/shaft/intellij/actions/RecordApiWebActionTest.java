package com.shaft.intellij.actions;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Covers the default-mode (advancedUiEnabled=false) Assistant prompt {@code RecordApiWebAction}
 * builds instead of the old dead-end warning that discarded the typed URL (issue #3552).
 */
class RecordApiWebActionTest {
    @Test
    void recordApiPromptCarriesTheTypedUrl() {
        String prompt = RecordApiWebAction.recordApiPrompt("https://example.com/checkout");

        assertEquals("Record API traffic on https://example.com/checkout", prompt);
    }
}
