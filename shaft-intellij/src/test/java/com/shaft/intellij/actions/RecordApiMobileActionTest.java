package com.shaft.intellij.actions;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Covers the default-mode (advancedUiEnabled=false) Assistant prompt {@code RecordApiMobileAction}
 * builds, mirroring {@code RecordApiWebActionTest} (issue #3530 A2).
 */
class RecordApiMobileActionTest {
    @Test
    void recordApiMobilePromptCarriesTheTypedPlatform() {
        String prompt = RecordApiMobileAction.recordApiMobilePrompt("iOS");

        assertEquals("Record API traffic without a browser on iOS", prompt);
    }
}
