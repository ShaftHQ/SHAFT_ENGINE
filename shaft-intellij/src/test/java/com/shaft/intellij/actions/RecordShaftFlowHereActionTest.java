package com.shaft.intellij.actions;

import com.shaft.intellij.java.JavaTargetContext;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Prompt text stays available for Assistant fallback; live recording is the default
 * action path after issue #5942 (Automation/Recorder is not expert-only).
 */
class RecordShaftFlowHereActionTest {
    @Test
    void recordFlowPromptNamesTheMethodAndClassInPlainLanguage() {
        JavaTargetContext context = new JavaTargetContext(
                "src/test/java/LoginTest.java", "tests", "LoginTest", "logsIn");

        String prompt = RecordShaftFlowHereAction.recordFlowPrompt(context);

        assertEquals("Record a SHAFT flow at logsIn in LoginTest", prompt);
    }

    @Test
    void logStatusDetailPassesThroughStatusText() {
        assertEquals(
                "Live SHAFT recording starting, anchored at LoginTest#logsIn.",
                RecordShaftFlowHereAction.logStatusDetail(
                        "Live SHAFT recording starting, anchored at LoginTest#logsIn."));
    }

    @Test
    void logStatusDetailTreatsNullAsEmpty() {
        assertEquals("", RecordShaftFlowHereAction.logStatusDetail(null));
    }
}
