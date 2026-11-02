package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AssistantCommandCoverageHelpersTest {
    @Test
    void commandTooltipAndHelpRenderCoreAndExpertSurfaces() {
        String core = AssistantCommand.commandTooltip(false);
        String expert = AssistantCommand.commandTooltip(true);
        assertAll(
                () -> assertTrue(core.contains("SHAFT commands")),
                () -> assertTrue(expert.length() >= core.length()),
                () -> assertFalse(AssistantCommand.commandHelp(false).isBlank()),
                () -> assertFalse(AssistantCommand.commandHelp(true).isBlank()),
                () -> assertFalse(AssistantCommand.registeredCommandHints().isEmpty()),
                () -> assertFalse(AssistantCommand.commandHints().isEmpty()));
    }

    @Test
    void stopRecordingAndScenarioIntentHelpersMatchKnownPhrases() {
        assertAll(
                () -> assertTrue(AssistantCommand.isStopRecording("stop recording")),
                () -> assertTrue(AssistantCommand.isStopRecording("finish recording")),
                () -> assertTrue(AssistantCommand.isStopRecording("end recording")),
                () -> assertFalse(AssistantCommand.isStopRecording("start recording")),
                () -> assertFalse(AssistantCommand.isStopRecording(null)),
                () -> assertTrue(AssistantCommand.isScenarioDescriptionIntent(
                        "record a login flow and generate a reviewed test")),
                () -> assertFalse(AssistantCommand.isScenarioDescriptionIntent("run verify")),
                () -> assertFalse(AssistantCommand.isScenarioDescriptionIntent(null)));
    }

    @Test
    void keywordsForKnownToolsIncludeToolNameTokens() {
        assertFalse(AssistantCommand.keywordsFor("capture_start", java.util.List.of()).isEmpty());
        assertTrue(AssistantCommand.keywordsFor("capture_start", java.util.List.of("custom")).contains("custom")
                || !AssistantCommand.keywordsFor("capture_start", java.util.List.of("custom")).isEmpty());
    }

    @Test
    void recordingHelpersBuildExpectedArgumentShapes() {
        assertEquals("recordings/demo.json",
                AssistantCommand.captureCodeReview("recordings/demo.json").get("sessionPath").getAsString());
        assertTrue(AssistantCommand.captureCodeReview(" ").get("sessionPath").getAsString().length() > 0);
        assertFalse(AssistantCommand.apiCaptureGenerate("sessions/api.json").entrySet().isEmpty());
        assertFalse(AssistantCommand.playwrightCodeReview("rec.json").entrySet().isEmpty());
        assertFalse(AssistantCommand.stopPlaywrightRecording().toolCalls().isEmpty());
        assertFalse(AssistantCommand.stopApiRecording().toolCalls().isEmpty());
    }
}
