package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.image.BufferedImage;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Epic coverage for {@link ShaftAssistantPanel}: exercises package-visible test seams and
 * constructor/layout paths under {@code java.awt.headless} without reflection.
 */
class ShaftAssistantPanelEpicCoverageTest {

    @BeforeAll
    static void forceHeadless() {
        System.setProperty("java.awt.headless", "true");
    }

    @Test
    void preferredFocusPrefillAndPromptTextCoverComposerSeams() throws Exception {
        ShaftAssistantPanel panel = newPanel(readySettings());
        runEdt(() -> {
            JComponent focus = panel.preferredFocusComponent();
            assertNotNull(focus);
            panel.prefillPrompt(null);
            panel.prefillPrompt("");
            panel.prefillPrompt("   ");
            assertEquals("", panel.promptText().trim());
            panel.prefillPrompt("Diagnose my last failed Allure run");
            assertEquals("Diagnose my last failed Allure run", panel.promptText());
            panel.submitReviewedPrompt("user reviewed this");
            assertFalse(panel.transcriptMarkdown().isBlank());
        });
    }

    @Test
    void contextSuggestionsCoverWorkflowProjectSlashAndUnknownTriggers() throws Exception {
        ShaftSettingsState.Settings expert = readySettings();
        expert.advancedUiEnabled = true;
        ShaftAssistantPanel expertPanel = newPanel(expert);
        ShaftAssistantPanel corePanel = newPanel(readySettings());

        runEdt(() -> {
            List<ShaftAssistantPanel.ContextSuggestion> workflows =
                    expertPanel.contextSuggestionsForTest('@');
            List<ShaftAssistantPanel.ContextSuggestion> project =
                    expertPanel.contextSuggestionsForTest('#');
            List<ShaftAssistantPanel.ContextSuggestion> slashExpert =
                    expertPanel.contextSuggestionsForTest('/');
            List<ShaftAssistantPanel.ContextSuggestion> slashCore =
                    corePanel.contextSuggestionsForTest('/');
            List<ShaftAssistantPanel.ContextSuggestion> unknown =
                    expertPanel.contextSuggestionsForTest('!');

            assertAll(
                    () -> assertFalse(workflows.isEmpty()),
                    () -> assertTrue(workflows.stream().anyMatch(s -> s.label().contains("@workflow:doctor"))),
                    () -> assertTrue(workflows.stream().anyMatch(s -> s.insertion().contains("Diagnose"))),
                    () -> assertNotNull(project),
                    () -> assertFalse(slashExpert.isEmpty()),
                    () -> assertTrue(slashExpert.size() >= slashCore.size()),
                    () -> assertTrue(unknown.isEmpty()));

            List<ShaftAssistantPanel.ContextSuggestion> filteredDoctor =
                    expertPanel.filteredContextSuggestions('@', "doctor");
            List<ShaftAssistantPanel.ContextSuggestion> filteredEmpty =
                    expertPanel.filteredContextSuggestions('@', "zzzz-no-match");
            List<ShaftAssistantPanel.ContextSuggestion> filteredBlank =
                    expertPanel.filteredContextSuggestions('/', "");
            assertAll(
                    () -> assertFalse(filteredDoctor.isEmpty()),
                    () -> assertTrue(filteredDoctor.stream().allMatch(
                            s -> s.matchText().toLowerCase().contains("doctor")
                                    || s.label().toLowerCase().contains("doctor")
                                    || s.insertion().toLowerCase().contains("diagnose"))),
                    () -> assertTrue(filteredEmpty.isEmpty()),
                    () -> assertFalse(filteredBlank.isEmpty()));
        });
    }

    @Test
    void simulateAppendOfferQuestionApprovalAndRunningToggleCoverTranscriptChrome() throws Exception {
        ShaftAssistantPanel panel = newPanel(readySettings());
        runEdt(() -> {
            panel.simulateAppendForTest("user", "hello", "");
            panel.simulateAppendForTest("assistant", "world", "raw", ShaftAssistantChatState.KIND_MILESTONE);
            panel.simulateAppendForTest("assistant", "tool card", "{}", ShaftAssistantChatState.KIND_TOOL_EVENT);
            panel.simulateAppendForTest("assistant", "error", "boom", ShaftAssistantChatState.KIND_ERROR);
            assertTrue(panel.transcriptMarkdown().contains("hello"));
            assertTrue(panel.transcriptMarkdown().contains("world"));

            panel.offerQuestion(List.of("Yes", "No", "Maybe later"));
            panel.offerApproval("capture_start");
            panel.setRunning(true, "Running: capture_start …");
            panel.setRunning(true, "still running");
            panel.setRunning(false, "Idle");
            panel.setRunning(false, "still idle");
        });
    }

    @Test
    void applyCapabilityHelpResultCoversSuccessAndFailureCards() throws Exception {
        ShaftAssistantPanel panel = newPanel(readySettings());
        runEdt(() -> {
            panel.applyCapabilityHelpResult("capture", ShaftMcpToolResult.success("{\"ok\":true}"), null);
            panel.applyCapabilityHelpResult("doctor", ShaftMcpToolResult.failure("missing allure"), null);
            panel.applyCapabilityHelpResult("heal", null, null);
            panel.applyCapabilityHelpResult("upgrade", null, new IllegalStateException("offline"));
            assertFalse(panel.transcriptMarkdown().isBlank());
        });
    }

    @Test
    void attachFileAndImagePathsCoverReadableAndUnreadableSeams(@TempDir Path temp) throws Exception {
        Path text = temp.resolve("snippet.java");
        Files.writeString(text, "public class Snippet {}");
        Path missing = temp.resolve("missing.txt");
        Path image = temp.resolve("shot.png");
        Files.write(image, new byte[] {(byte) 0x89, 0x50, 0x4E, 0x47});

        ShaftAssistantPanel panel = newPanel(readySettings());
        runEdt(() -> {
            panel.attachFileAtPath(text);
            panel.attachFileAtPath(missing);
            panel.attachImageAtPath(image);
        });
    }

    @Test
    void trimChatTitleForWidthCoversNullBlankShortAndOverflow() {
        BufferedImage image = new BufferedImage(200, 40, BufferedImage.TYPE_INT_ARGB);
        FontMetrics metrics = image.getGraphics().getFontMetrics(new Font(Font.SANS_SERIF, Font.PLAIN, 12));
        assertAll(
                () -> assertEquals("New chat", ShaftAssistantPanel.trimChatTitleForWidth(null, metrics, 80)),
                () -> assertEquals("New chat", ShaftAssistantPanel.trimChatTitleForWidth("   ", metrics, 80)),
                () -> assertEquals("Hi", ShaftAssistantPanel.trimChatTitleForWidth("Hi", metrics, 500)),
                () -> {
                    String longTitle = "Record a long browser workflow and generate Page Object code for checkout";
                    String trimmed = ShaftAssistantPanel.trimChatTitleForWidth(longTitle, metrics, 40);
                    assertTrue(trimmed.length() <= longTitle.length());
                    assertFalse(trimmed.isBlank());
                    assertTrue(trimmed.endsWith("...") || trimmed.equals(longTitle));
                },
                () -> assertEquals("...", ShaftAssistantPanel.trimChatTitleForWidth("X", metrics, 1)));
    }

    @Test
    void agentGuidanceOptimizationPromptCoversEverySupportedFamily() {
        for (String family : List.of("CLAUDE", "COPILOT", "CODEX", "GROK", "GEMINI", "ANTIGRAVITY", "", "UNKNOWN")) {
            ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
            settings.assistantFamily = family;
            String prompt = ShaftAssistantPanel.agentGuidanceOptimizationPrompt(settings);
            assertFalse(prompt.isBlank(), family);
            assertTrue(prompt.contains("shaft_guide_search") || prompt.toLowerCase().contains("guide"), family);
        }
    }

    @Test
    void toolCardMarkdownCoversStructuredDoctorAndVerboseShapes() {
        String doctorJson = "{\"summary\":\"3 failures\",\"clusters\":[{\"name\":\"timeout\"}]}";
        ShaftMcpToolResult ok = ShaftMcpToolResult.success(doctorJson);
        ShaftMcpToolResult empty = ShaftMcpToolResult.success("");
        ShaftMcpToolResult failed = ShaftMcpToolResult.failure("no evidence");

        assertAll(
                () -> assertFalse(ShaftAssistantPanel.toolCardMarkdown("doctor_analyze_failed_allure", ok, null).isBlank()),
                () -> assertFalse(ShaftAssistantPanel.toolCardMarkdown("capture_generate_replay", empty, null).isBlank()),
                () -> assertFalse(ShaftAssistantPanel.toolCardMarkdown("healer_run_failed_test", failed, null).isBlank()),
                () -> assertEquals(ShaftAssistantChatState.KIND_TOOL_EVENT,
                        ShaftAssistantPanel.toolCardKind(ok, null)),
                () -> assertEquals(ShaftAssistantChatState.KIND_ERROR,
                        ShaftAssistantPanel.toolCardKind(failed, null)));
    }

    @Test
    void requiresSourceEditApprovalCoversContinuationAndArtifactHints() {
        assertAll(
                () -> assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, false, "custom", "please edit OrdersPage.java", "")),
                () -> assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, false, "custom", "continue editing", "refactor the module")),
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, false, "custom", "open https://example.com", "")),
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, true, false, "custom", "edit file", "")),
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        false, false, false, "custom", "edit file", "")));
    }

    @Test
    void disposeLivePanelsAndConstructorVariantsCoverLifecycle() throws Exception {
        AtomicBoolean configured = new AtomicBoolean(false);
        ShaftAssistantPanel withCallback = new ShaftAssistantPanel(
                null, readySettings(), new ShaftAssistantChatState(), () -> configured.set(true));
        ShaftAssistantPanel plain = new ShaftAssistantPanel(null, readySettings());
        ShaftAssistantPanel withState = new ShaftAssistantPanel(null, readySettings(), new ShaftAssistantChatState());
        runEdt(() -> {
            assertNotNull(withCallback.preferredFocusComponent());
            assertNotNull(plain.preferredFocusComponent());
            assertNotNull(withState.preferredFocusComponent());
            withCallback.dispose();
            plain.dispose();
            withState.dispose();
            ShaftAssistantPanel.disposeLivePanels();
        });
        assertNotNull(withCallback);
        assertNotNull(plain);
        assertNotNull(withState);
    }

    @Test
    void cloudAndLocalSettingsConstructorsCoverProviderChrome() throws Exception {
        ShaftSettingsState.Settings cloud = readySettings();
        cloud.assistantProviderType = "CLOUD";
        cloud.cloudProvider = "openai";
        cloud.assistantFamily = "CODEX";
        cloud.advancedUiEnabled = true;
        cloud.passProviderApiKeysToMcp = true;

        ShaftSettingsState.Settings local = readySettings();
        local.assistantProviderType = "LOCAL";
        local.assistantFamily = "CLAUDE";
        local.advancedUiEnabled = true;

        ShaftSettingsState.Settings grok = readySettings();
        grok.assistantFamily = "GROK";
        grok.assistantProviderType = "LOCAL";

        runEdt(() -> {
            ShaftAssistantPanel cloudPanel = new ShaftAssistantPanel(null, cloud, new ShaftAssistantChatState());
            ShaftAssistantPanel localPanel = new ShaftAssistantPanel(null, local, new ShaftAssistantChatState());
            ShaftAssistantPanel grokPanel = new ShaftAssistantPanel(null, grok, new ShaftAssistantChatState());
            cloudPanel.prefillPrompt("Upgrade this project to the latest SHAFT");
            localPanel.prefillPrompt("Record my browser actions on https://example.com");
            grokPanel.prefillPrompt("/guide search swipe");
            assertFalse(cloudPanel.promptText().isBlank());
            assertFalse(localPanel.promptText().isBlank());
            assertFalse(grokPanel.promptText().isBlank());
            cloudPanel.dispose();
            localPanel.dispose();
            grokPanel.dispose();
        });
    }


    @Test
    void filteredSlashMcpToolArgumentSuggestionsCoverCatalogIndex() throws Exception {
        ShaftSettingsState.Settings settings = readySettings();
        settings.advancedUiEnabled = true;
        ShaftAssistantPanel panel = newPanel(settings);
        runEdt(() -> {
            var all = panel.filteredContextSuggestions('/', "mcp ");
            var click = panel.filteredContextSuggestions('/', "mcp cl");
            var tool = panel.filteredContextSuggestions('/', "tool cl");
            var call = panel.filteredContextSuggestions('/', "call cl");
            var none = panel.filteredContextSuggestions('/', "mcp zzz_not_a_real_tool");
            assertAll(
                    () -> assertFalse(all.isEmpty()),
                    () -> assertFalse(click.isEmpty() && tool.isEmpty() && call.isEmpty()),
                    () -> assertTrue(none.isEmpty() || none.stream().noneMatch(
                            s -> "zzz_not_a_real_tool".equals(s.matchText()))));
        });
    }

    @Test
    void applyCapabilityHelpResultCancellationFallsBackToBundledCatalog() throws Exception {
        ShaftAssistantPanel panel = newPanel(readySettings());
        runEdt(() -> {
            panel.applyCapabilityHelpResult(
                    "capture", null, new java.util.concurrent.CancellationException("cancelled"));
            assertTrue(panel.transcriptMarkdown().toLowerCase().contains("cancelled")
                    || panel.transcriptMarkdown().toLowerCase().contains("bundled")
                    || panel.transcriptMarkdown().toLowerCase().contains("capabilities"));
        });
    }

    private static ShaftAssistantPanel newPanel(ShaftSettingsState.Settings settings) {
        return new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState());
    }

    private static ShaftSettingsState.Settings readySettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        return settings;
    }

    private static void runEdt(ThrowingRunnable action) throws Exception {
        if (SwingUtilities.isEventDispatchThread()) {
            action.run();
            return;
        }
        Exception[] failure = new Exception[1];
        SwingUtilities.invokeAndWait(() -> {
            try {
                action.run();
            } catch (Exception exception) {
                failure[0] = exception;
            }
        });
        if (failure[0] != null) {
            throw failure[0];
        }
    }

    @FunctionalInterface
    private interface ThrowingRunnable {
        void run() throws Exception;
    }
}
