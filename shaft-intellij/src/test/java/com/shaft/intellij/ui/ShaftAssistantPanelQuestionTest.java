package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.TimeUnit;

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

    // -- issue #3719: the runner-recognized structured protocol, and its two documented fallbacks --
    //
    // resolveQuestion (ShaftAssistantPanel) tries, in order: (1) AssistantLocalAgentRunner.parseQuestion
    // on rawResponse -- the runner-envelope form only a StructuredStreamParser-backed local-agent run
    // (Claude Code, Codex) can populate; (2) AssistantQuestion.detectStructuredLine directly on the
    // displayed markdown -- covers paths with no runner envelope at all, e.g. cloud chat; (3)
    // AssistantQuestion.detect (the shaft-options fence) -- today's unchanged, universal fallback.
    // The three existing tests above already pin (3) working with an empty/no-runner rawResponse, so
    // they double as "fence still works" coverage for this refactor without any changes.

    @Test
    void finishLocalAgentResponseRendersAnswerChipsForARunnerRecognizedStructuredQuestion() throws Exception {
        ShaftAssistantPanel panel = newPanel();
        AssistantTranscriptView transcript = transcriptOf(panel);
        String resultText = "Want me to actually run through a recording now?\\n"
                + "{\\\"shaft-question\\\": \\\"Want me to actually run through a recording now?\\\", "
                + "\\\"shaft-options\\\": [\\\"Use the sample page\\\", \\\"I'll give you a URL\\\"]}";
        String stdout = "{\"type\":\"result\",\"result\":\"" + resultText
                + "\",\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}\n";
        String rawResponse = runClaudeStructuredStream(stdout).output();
        String response = AssistantMarkdown.normalizeMarkdown(
                AssistantLocalAgentRunner.stripTrailingUsageMetadata(rawResponse));

        invokeFinishLocalAgentResponse(panel, response, rawResponse);

        AssistantQuestionOptionsPanel widget =
                (AssistantQuestionOptionsPanel) transcript.pendingWidgetForTest();
        List<String> labels = widget.optionButtonsForTest().stream().map(JButton::getText).toList();
        assertAll(
                () -> assertTrue(transcript.markdown().contains("Want me to actually run through a recording now?")),
                () -> assertFalse(transcript.markdown().contains("shaft-question"),
                        "the raw structured marker must never leak into the persisted transcript: " + transcript.markdown()),
                () -> assertTrue(labels.contains("Use the sample page")),
                () -> assertTrue(labels.contains("I'll give you a URL")));
    }

    @Test
    void showResponseRendersAnswerChipsForABareStructuredLineWithNoRunnerEnvelope() throws Exception {
        // Mirrors a cloud-chat answer: no AssistantLocalAgentRunner envelope at all (rawResponse is
        // whatever autobot_provider_chat returned), just the model's own trailing structured line.
        ShaftAssistantPanel panel = newPanel();
        AssistantTranscriptView transcript = transcriptOf(panel);
        String response = "Want me to actually run through a recording now?\n"
                + "{\"shaft-question\": \"Want me to actually run through a recording now?\", "
                + "\"shaft-options\": [\"Use the sample page\", \"I'll give you a URL\"]}";

        invokeShowResponse(panel, response, "");

        AssistantQuestionOptionsPanel widget =
                (AssistantQuestionOptionsPanel) transcript.pendingWidgetForTest();
        List<String> labels = widget.optionButtonsForTest().stream().map(JButton::getText).toList();
        assertAll(
                () -> assertTrue(transcript.markdown().contains("Want me to actually run through a recording now?")),
                () -> assertFalse(transcript.markdown().contains("shaft-question"),
                        "the raw structured line must not leak into the persisted transcript: " + transcript.markdown()),
                () -> assertTrue(labels.contains("Use the sample page")),
                () -> assertTrue(labels.contains("I'll give you a URL")));
    }

    @Test
    void showResponseFallsBackToPlainTextWhenTheStructuredAttemptIsMalformedAndNoFenceIsPresent() throws Exception {
        // The model attempted the structured line but left it malformed (missing shaft-options) and
        // offered no fence either -- this must degrade to plain text, exactly like ordinary prose,
        // never crash and never show an empty/broken chip row.
        ShaftAssistantPanel panel = newPanel();
        AssistantTranscriptView transcript = transcriptOf(panel);
        String response = "Pick one:\n{\"shaft-question\": \"Pick one:\"}";

        invokeShowResponse(panel, response, "");

        assertAll(
                () -> assertNull(transcript.pendingWidgetForTest()),
                () -> assertTrue(transcript.markdown().contains("Pick one:")));
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

    private static void invokeFinishLocalAgentResponse(ShaftAssistantPanel panel, String response, String rawResponse)
            throws ReflectiveOperationException {
        Method finishLocalAgentResponse = ShaftAssistantPanel.class.getDeclaredMethod(
                "finishLocalAgentResponse", int.class, String.class, String.class);
        finishLocalAgentResponse.setAccessible(true); // NOPMD - test-only method invocation, matching the established pattern in ShaftPanelSetupTest
        // activeLocalAgentStreamToken defaults to -1 on a freshly constructed panel, which the guard
        // clause `streamToken != activeLocalAgentStreamToken && activeLocalAgentStreamToken != -1`
        // always lets through regardless of the token passed here, so any fixed value is safe.
        finishLocalAgentResponse.invoke(panel, 1, response, rawResponse);
    }

    /**
     * Runs a stubbed Claude Code structured-stream invocation through the real {@link
     * AssistantLocalAgentRunner}, so the resulting {@link ShaftMcpToolResult#output()} is a
     * genuine runner-composed trailing-metadata envelope rather than a hand-assembled one --
     * mirroring the proven pattern in {@code AssistantLocalAgentRunnerQuestionTest}'s {@code
     * finalOutput}.
     */
    private static ShaftMcpToolResult runClaudeStructuredStream(String stdout) throws Exception {
        StubProcess process = new StubProcess(stdout);
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Should I record a sample flow?", "CLAUDE_CODE", "ASK", ".", "", false);
        ShaftMcpInvocation running = AssistantLocalAgentRunner.start(
                invocation, line -> { }, (command, workingDirectory, environment) -> process, false);
        ShaftMcpToolResult result = running.future().get(5, TimeUnit.SECONDS);
        assertTrue(result.success(), "Expected the stub run to succeed: " + result.output());
        return result;
    }

    /**
     * Minimal stub {@link Process} that replays fixed stdout content and exits successfully as soon
     * as {@link Process#waitFor(long, TimeUnit)} is polled, mirroring the proven pattern in {@code
     * AssistantLocalAgentRunnerQuestionTest}'s {@code StubProcess}.
     */
    private static final class StubProcess extends Process {
        private final InputStream stdout;
        private final InputStream stderr;

        StubProcess(String stdoutContent) {
            this.stdout = new ByteArrayInputStream(stdoutContent.getBytes(StandardCharsets.UTF_8));
            this.stderr = new ByteArrayInputStream(new byte[0]);
        }

        @Override
        public OutputStream getOutputStream() {
            return new ByteArrayOutputStream();
        }

        @Override
        public InputStream getInputStream() {
            return stdout;
        }

        @Override
        public InputStream getErrorStream() {
            return stderr;
        }

        @Override
        public int waitFor() {
            return 0;
        }

        @Override
        public boolean waitFor(long timeout, TimeUnit unit) {
            return true;
        }

        @Override
        public int exitValue() {
            return 0;
        }

        @Override
        public void destroy() {
            // No-op: these tests never exercise cancellation/destroy, only completed-run streaming.
        }

        @Override
        public Process destroyForcibly() {
            return this;
        }

        @Override
        public boolean isAlive() {
            return false;
        }
    }
}
