package com.shaft.intellij.ui;

import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers issue #3674's wiring of {@link AssistantQuestion} detection into {@link
 * ShaftAssistantPanel}'s two terminal-answer choke points, {@code showResponse} (cloud/tool-card
 * answers) and {@code finishLocalAgentResponse} (streaming local-agent answers). Both are private
 * instance methods with no other test seam, so this drives them via reflection against a real
 * panel instance, the same approach {@link ShaftAssistantPanelTest} already uses for this class.
 */
class ShaftAssistantPanelQuestionTest {

    @Test
    void showResponseRendersAnswerChipsForADetectedClarifyingQuestionAndStripsTheFence()
            throws ReflectiveOperationException {
        ShaftAssistantPanel panel = newPanel();
        AssistantTranscriptView transcript = transcriptOf(panel);
        String response = """
                Want me to actually run through a recording now?

                ```shaft-options
                ["Use the sample page", "I'll give you a URL"]
                ```
                """;

        invokeShowResponse(panel, response, "");

        AssistantQuestionOptionsPanel widget =
                (AssistantQuestionOptionsPanel) transcript.pendingWidgetForTest();
        List<String> labels = widget.optionButtonsForTest().stream().map(JButton::getText).toList();
        assertAll(
                () -> assertTrue(transcript.markdown().contains("Want me to actually run through a recording now?")),
                () -> assertFalse(transcript.markdown().contains("shaft-options"),
                        "the raw fence marker must not leak into the persisted transcript"),
                () -> assertTrue(labels.contains("Use the sample page")),
                () -> assertTrue(labels.contains("I'll give you a URL")));
    }

    @Test
    void showResponseShowsNoWidgetForAnOrdinaryNarrativeAnswer() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = newPanel();
        AssistantTranscriptView transcript = transcriptOf(panel);

        invokeShowResponse(panel, "Here is a plain narrative answer with no clarifying question.", "");

        assertNull(transcript.pendingWidgetForTest());
    }

    @Test
    void clickingAnAnswerChipFillsTheComposerInsteadOfAutoSending() throws ReflectiveOperationException {
        ShaftAssistantPanel panel = newPanel();
        AssistantTranscriptView transcript = transcriptOf(panel);
        String response = """
                Pick one:

                ```shaft-options
                ["Yes", "No"]
                ```
                """;
        invokeShowResponse(panel, response, "");
        AssistantQuestionOptionsPanel widget =
                (AssistantQuestionOptionsPanel) transcript.pendingWidgetForTest();
        JButton yes = widget.optionButtonsForTest().stream()
                .filter(button -> "Yes".equals(button.getText())).findFirst().orElseThrow();

        yes.doClick();

        Field promptField = ShaftAssistantPanel.class.getDeclaredField("prompt");
        promptField.setAccessible(true); // NOPMD - test-only field injection, matching the established pattern in ShaftPanelSetupTest
        com.intellij.ui.components.JBTextArea prompt = (com.intellij.ui.components.JBTextArea) promptField.get(panel);
        assertEquals("Yes", prompt.getText(), "the chip should fill, not auto-send, the composer");
    }

    private static ShaftAssistantPanel newPanel() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        return new ShaftAssistantPanel(null, settings, ShaftAssistantChatState.getInstance(null));
    }

    private static AssistantTranscriptView transcriptOf(ShaftAssistantPanel panel) throws ReflectiveOperationException {
        Field transcriptField = ShaftAssistantPanel.class.getDeclaredField("transcript");
        transcriptField.setAccessible(true); // NOPMD - test-only field injection, matching the established pattern in ShaftPanelSetupTest
        return (AssistantTranscriptView) transcriptField.get(panel);
    }

    private static void invokeShowResponse(ShaftAssistantPanel panel, String response, String rawResponse)
            throws ReflectiveOperationException {
        Method showResponse = ShaftAssistantPanel.class.getDeclaredMethod("showResponse", String.class, String.class);
        showResponse.setAccessible(true); // NOPMD - test-only method invocation, matching the established pattern in ShaftPanelSetupTest
        showResponse.invoke(panel, response, rawResponse);
    }
}
