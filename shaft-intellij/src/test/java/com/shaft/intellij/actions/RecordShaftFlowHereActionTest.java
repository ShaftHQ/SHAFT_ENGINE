package com.shaft.intellij.actions;

import com.shaft.intellij.java.JavaTargetContext;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Covers the default-mode (advancedUiEnabled=false) Assistant prompt {@code
 * RecordShaftFlowHereAction} builds instead of the old copy-to-clipboard-and-warn no-op
 * (issue #3552).
 */
class RecordShaftFlowHereActionTest {
    @Test
    void recordFlowPromptNamesTheMethodAndClassInPlainLanguage() {
        JavaTargetContext context = new JavaTargetContext(
                "src/test/java/LoginTest.java", "tests", "LoginTest", "logsIn");

        String prompt = RecordShaftFlowHereAction.recordFlowPrompt(context);

        assertEquals("Record a SHAFT flow at logsIn in LoginTest", prompt);
    }
}
