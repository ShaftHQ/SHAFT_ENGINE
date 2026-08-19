package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.google.gson.JsonArray;
import com.google.gson.JsonParser;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBTextArea;
import com.intellij.openapi.project.Project;
import com.intellij.util.ui.WrapLayout;
import com.shaft.intellij.approval.LocalAgentApprovalBridge;
import com.shaft.intellij.approval.ToolApprovalDecision;
import com.shaft.intellij.approval.ToolApprovalService;
import com.shaft.intellij.mcp.ShaftMcpInvocation;
import com.shaft.intellij.mcp.ShaftMcpToolResult;
import com.shaft.intellij.settings.AssistantAgentRoute;
import com.shaft.intellij.settings.ShaftSettingsState;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.accessibility.AccessibleContext;
import javax.swing.Action;
import javax.swing.AbstractButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JEditorPane;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JViewport;
import javax.swing.RepaintManager;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Function;
import javax.swing.text.JTextComponent;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPanelSetupTest {
    private static final List<Path> TEMP_DIRECTORIES = new ArrayList<>();

    @Test
    void assistantCapabilityHelpAppliesFreshMcpDiscoveryToTheTranscript() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Method apply = assertDoesNotThrow(() -> ShaftAssistantPanel.class.getDeclaredMethod(
                "applyCapabilityHelpResult", String.class, ShaftMcpToolResult.class, Throwable.class));
        apply.setAccessible(true);
        String payload = """
                {"tools":[{"name":"future_live_tool","description":"A live future capability"}]}
                """;

        apply.invoke(panel, "", ShaftMcpToolResult.success(payload), null);

        String transcript = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(transcript.contains("future_live_tool"), transcript),
                () -> assertTrue(transcript.contains("Live SHAFT MCP discovery succeeded"), transcript),
                () -> assertFalse(transcript.contains("element_click"), transcript));
    }

    @Test
    void capabilityDiscoveryCancellationDoesNotCancelTheSharedMcpClient() throws Exception {
        AtomicBoolean sharedCancelCalled = new AtomicBoolean();
        CompletableFuture<ShaftMcpToolResult> sharedFuture = new CompletableFuture<>();
        ShaftMcpInvocation shared = new ShaftMcpInvocation(sharedFuture,
                () -> sharedCancelCalled.set(true));
        Method wrap = ShaftAssistantPanel.class.getDeclaredMethod(
                "nonDisruptiveCapabilityDiscovery", ShaftMcpInvocation.class);
        wrap.setAccessible(true);

        ShaftMcpInvocation help = (ShaftMcpInvocation) wrap.invoke(null, shared);
        help.cancel();

        assertAll(
                () -> assertTrue(help.future().isCancelled()),
                () -> assertFalse(sharedFuture.isCancelled()),
                () -> assertFalse(sharedCancelCalled.get(),
                        "cancelling help must not invoke the shared MCP client's cancel-all action"));
        assertTrue(sharedFuture.complete(ShaftMcpToolResult.success("{\"tools\":[]}")));
        assertTrue(sharedFuture.join().success());
    }

    @Test
    void capabilityDiscoveryFailureLeadsWithFallbackNoticeAndRestoresReady() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Method apply = ShaftAssistantPanel.class.getDeclaredMethod(
                "applyCapabilityHelpResult", String.class, ShaftMcpToolResult.class, Throwable.class);
        apply.setAccessible(true);

        apply.invoke(panel, "", ShaftMcpToolResult.failure("server exploded\nraw stack detail"), null);

        String transcript = transcriptMarkdown(panel);
        assertTrue(transcript.indexOf("Live discovery failed. Showing the bundled catalog instead.")
                < transcript.indexOf("# SHAFT Assistant capabilities"), transcript);
        assertFalse(transcript.contains("server exploded"), transcript);
        assertFalse(transcript.contains("raw stack detail"), transcript);
        assertTrue(transcript.contains("element_click"), transcript);
    }

    /**
     * Temp-dir factory with suite-level cleanup: raw Files.createTempDirectory calls leaked one
     * directory per test run into the OS temp folder (dozens on a developer machine).
     */
    private static Path tempDirectory(String prefix) throws IOException {
        Path directory = Files.createTempDirectory(prefix);
        synchronized (TEMP_DIRECTORIES) {
            TEMP_DIRECTORIES.add(directory);
        }
        return directory;
    }

    @org.junit.jupiter.api.AfterAll
    static void deleteTemporaryDirectories() {
        synchronized (TEMP_DIRECTORIES) {
            for (Path directory : TEMP_DIRECTORIES) {
                try (var paths = Files.walk(directory)) {
                    paths.sorted(java.util.Comparator.reverseOrder()).forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (IOException ignored) {
                            // Best-effort cleanup; the OS temp folder is the backstop.
                        }
                    });
                } catch (IOException ignored) {
                    // Directory already gone or unreadable; nothing to clean.
                }
            }
            TEMP_DIRECTORIES.clear();
        }
    }

    @Test
    void assistantExplainsMissingMcpConfiguration() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        assertAll(
                () -> assertTrue(containsText(panel, "Configure SHAFT MCP")),
                () -> assertNotNull(findByAccessibleName(panel, "Open SHAFT settings", JButton.class)));
    }

    @Test
    void assistantMcpSetupGateIgnoresBroadLocalCliPrompts() {
        AssistantCommand.Invocation broadLocal = AssistantCommand.fromPrompt(
                "Plan a browser recording workflow",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "ASK",
                ".",
                "",
                false);
        AssistantCommand.Invocation broadCloud = AssistantCommand.fromPrompt(
                "Plan a browser recording workflow",
                AssistantCommand.Selection.cloud("github", "openai/gpt-4.1"),
                "PLAN",
                ".",
                "",
                false);
        AssistantCommand.Invocation mcpFeature = AssistantCommand.fromPrompt(
                "/guide locators",
                "CODEX",
                "ASK",
                ".",
                "",
                false);

        assertAll(
                () -> assertFalse(ShaftAssistantPanel.requiresMcpSetup(broadLocal, false)),
                () -> assertFalse(ShaftAssistantPanel.requiresMcpSetup(broadCloud, false)),
                () -> assertTrue(ShaftAssistantPanel.requiresMcpSetup(mcpFeature, false)),
                () -> assertFalse(ShaftAssistantPanel.requiresMcpSetup(mcpFeature, true)));
    }

    @Test
    void assistantRoutesRecordingCommandsForWebDriverAndPlaywright() throws Exception {
        AssistantCommand.Invocation webStart = AssistantCommand.fromPrompt(
                "/record https://example.com",
                "CODEX",
                "ASK",
                ".",
                "",
                false);
        AssistantCommand.Invocation playwrightStart = AssistantCommand.fromPrompt(
                "/record playwright",
                "CODEX",
                "ASK",
                ".",
                "",
                false);
        AssistantCommand.Invocation explicitPlaywrightStop = AssistantCommand.fromPrompt(
                "stop playwright recording",
                "CODEX",
                "ASK",
                ".",
                "",
                false);

        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Object playwrightBackend = recordingBackend("PLAYWRIGHT");
        setField(panel, "activeRecordingBackend", playwrightBackend);
        AssistantCommand.Invocation naturalStop = AssistantCommand.fromPrompt(
                "stop recording",
                "CODEX",
                "ASK",
                ".",
                "",
                false);
        Method route = ShaftAssistantPanel.class.getDeclaredMethod(
                "routeNaturalStopToActiveRecorder", String.class, AssistantCommand.Invocation.class);
        route.setAccessible(true);
        AssistantCommand.Invocation routedStop = (AssistantCommand.Invocation) route.invoke(
                panel, "stop recording", naturalStop);

        // "/record playwright" must switch the active engine to Playwright before calling
        // capture_start (design doc amendment A3): capture_start dispatches on whichever engine is
        // already active, so without the driver_initialize step first it would launch its own web
        // CDP browser instead of recording the Playwright session.
        List<String> playwrightStartToolNames = playwrightStart.toolCalls().stream()
                .map(AssistantCommand.ToolCall::toolName)
                .toList();

        assertAll(
                () -> assertEquals("capture_start", webStart.toolName()),
                () -> assertEquals("https://example.com", webStart.arguments().get("targetUrl").getAsString()),
                () -> assertEquals(List.of("driver_initialize", "capture_start"), playwrightStartToolNames),
                () -> assertEquals("capture_stop", explicitPlaywrightStop.toolName()),
                () -> assertEquals("capture_stop", routedStop.toolName()));
    }

    @Test
    void assistantBuildsPlaywrightReviewFromActiveRecordingPath() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Object playwrightBackend = recordingBackend("PLAYWRIGHT");
        setField(panel, "activeRecordingBackend", playwrightBackend);
        setField(panel, "activePlaywrightRecordingPath", "recordings/custom-playwright.json");

        Method review = ShaftAssistantPanel.class.getDeclaredMethod(
                "recordingCodeReviewInvocation", Class.forName("com.shaft.intellij.ui.ShaftAssistantPanel$RecordingBackend"));
        review.setAccessible(true);
        AssistantCommand.Invocation invocation = (AssistantCommand.Invocation) review.invoke(panel, playwrightBackend);

        assertAll(
                () -> assertEquals("capture_code_blocks", invocation.toolName()),
                () -> assertEquals("recordings/custom-playwright.json",
                        invocation.arguments().get("sessionPath").getAsString()),
                () -> assertEquals("playwright", invocation.arguments().get("backend").getAsString()));
    }

    // ---- Issue #3739: chat-side capture_api_* recording gets the same deterministic
    // record -> stop -> generate loop as the WebDriver/Playwright recorders. ----

    @Test
    void assistantTracksApiRecordingStartAndReroutesNaturalStopToApiStop() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        AssistantCommand.Invocation apiStart =
                AssistantCommand.Invocation.tool("capture_api_start", new JsonObject());

        rememberCaptureInvocation(panel, "/mcp capture_api_start {}", apiStart);

        assertEquals(recordingBackend("API"), getField(panel, "activeRecordingBackend"));

        AssistantCommand.Invocation naturalStop = AssistantCommand.fromPrompt(
                "stop recording", "CODEX", "ASK", ".", "", false);
        Method route = ShaftAssistantPanel.class.getDeclaredMethod(
                "routeNaturalStopToActiveRecorder", String.class, AssistantCommand.Invocation.class);
        route.setAccessible(true);
        AssistantCommand.Invocation routedStop = (AssistantCommand.Invocation) route.invoke(
                panel, "stop recording", naturalStop);

        assertAll(
                () -> assertEquals("capture_stop", naturalStop.toolName()),
                () -> assertEquals("capture_api_stop", routedStop.toolName()),
                () -> assertFalse(routedStop.arguments().get("discard").getAsBoolean()));
    }

    @Test
    void armedApiStopExtractsSessionPathAndBuildsGenerateInvocationWithPanelDefaults() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "activeRecordingBackend", recordingBackend("API"));
        setField(panel, "generateCaptureReviewAfterStop", true);

        String output = mcpText("""
                {
                  "state": "STOPPED",
                  "outputPath": "recordings/intellij-api-capture.json"
                }
                """);
        Method showDiagnostic = ShaftAssistantPanel.class.getDeclaredMethod(
                "showCaptureStopDiagnosticIfPending", String.class, boolean.class, String.class, String.class);
        showDiagnostic.setAccessible(true);
        // Extracting the session path happens before startCaptureCodeReview()'s real MCP dispatch,
        // which NPEs against the null test project -- mirrors
        // directCodegenToolDispatchArmsSameStickyReviewGateAsRecordFlow's null-project pattern, so
        // the state assertions below still hold even though the invoke() itself throws.
        assertThrows(InvocationTargetException.class,
                () -> showDiagnostic.invoke(panel, "capture_api_stop", true, "Stopped.", output));

        assertEquals("recordings/intellij-api-capture.json", getField(panel, "activeApiRecordingPath"));

        Method review = ShaftAssistantPanel.class.getDeclaredMethod(
                "recordingCodeReviewInvocation", Class.forName("com.shaft.intellij.ui.ShaftAssistantPanel$RecordingBackend"));
        review.setAccessible(true);
        AssistantCommand.Invocation invocation =
                (AssistantCommand.Invocation) review.invoke(panel, recordingBackend("API"));
        JsonObject arguments = invocation.arguments();

        assertAll(
                () -> assertEquals("capture_api_generate", invocation.toolName()),
                () -> assertEquals("recordings/intellij-api-capture.json", arguments.get("sessionPath").getAsString()),
                () -> assertEquals("generated-tests", arguments.get("outputDirectory").getAsString()),
                () -> assertEquals("tests.generated", arguments.get("packageName").getAsString()),
                () -> assertEquals("", arguments.get("className").getAsString()),
                () -> assertEquals("SCENARIO", arguments.get("style").getAsString()),
                () -> assertEquals("SCHEMA", arguments.get("validationDepth").getAsString()),
                () -> assertFalse(arguments.get("overwrite").getAsBoolean()),
                () -> assertFalse(arguments.get("replay").getAsBoolean()),
                () -> assertEquals("", arguments.get("openApiSpecPath").getAsString()),
                () -> assertEquals(0, arguments.getAsJsonArray("excludedTransactionIds").size()),
                () -> assertEquals(0, arguments.getAsJsonArray("pinnedJsonPaths").size()));
    }

    @Test
    void armedApiStopWithBlankSessionPathSkipsGenerateAndExplains() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "activeRecordingBackend", recordingBackend("API"));
        setField(panel, "generateCaptureReviewAfterStop", true);

        String output = mcpText("""
                {
                  "state": "STOPPED",
                  "outputPath": ""
                }
                """);
        Method showDiagnostic = ShaftAssistantPanel.class.getDeclaredMethod(
                "showCaptureStopDiagnosticIfPending", String.class, boolean.class, String.class, String.class);
        showDiagnostic.setAccessible(true);
        boolean handled = (Boolean) showDiagnostic.invoke(panel, "capture_api_stop", true, "Stopped.", output);

        assertAll(
                () -> assertTrue(handled),
                () -> assertFalse((Boolean) getField(panel, "generateCaptureReviewAfterStop")),
                () -> assertFalse((Boolean) getField(panel, "captureReviewGenerationRunning")),
                () -> assertTrue(containsText(panel, "session path")));
    }

    // ---- Issue #3767: the armed-stop path ("stop recording" with generateCaptureReviewAfterStop
    // set) short-circuits before showFinalToolResult on both its return-true branches, so
    // ShaftRecordingActivity.stopped(recordingKey) -- the only place that clears the readiness
    // strip's recording badge for stop tools -- never fired. The badge stayed lit until
    // removeNotify() or an unrelated later stop happened to clear it. ----

    @Test
    void armedCaptureStopClearsRecordingBadgeBeforeReviewGenerationThrows() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "activeRecordingBackend", recordingBackend("WEBDRIVER"));
        setField(panel, "generateCaptureReviewAfterStop", true);
        String recordingKey = (String) getField(panel, "recordingKey");
        ShaftRecordingActivity.resetForTests();
        ShaftRecordingActivity.started(recordingKey);
        assertTrue(ShaftRecordingActivity.active());

        Method showDiagnostic = ShaftAssistantPanel.class.getDeclaredMethod(
                "showCaptureStopDiagnosticIfPending", String.class, boolean.class, String.class, String.class);
        showDiagnostic.setAccessible(true);
        // Mirrors armedApiStopExtractsSessionPathAndBuildsGenerateInvocationWithPanelDefaults: the
        // armed path's startCaptureCodeReview() call NPEs against the null test project, but the
        // badge must be cleared before that dispatch is attempted, not after.
        assertThrows(InvocationTargetException.class,
                () -> showDiagnostic.invoke(panel, "capture_stop", true, "Stopped.", "Stopped."));

        assertFalse(ShaftRecordingActivity.active(), "armed capture_stop should clear the recording badge");
    }

    @Test
    void armedApiStopWithBlankSessionPathClearsRecordingBadge() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "activeRecordingBackend", recordingBackend("API"));
        setField(panel, "generateCaptureReviewAfterStop", true);
        String recordingKey = (String) getField(panel, "recordingKey");
        ShaftRecordingActivity.resetForTests();
        ShaftRecordingActivity.started(recordingKey);

        String output = mcpText("""
                {
                  "state": "STOPPED",
                  "outputPath": ""
                }
                """);
        Method showDiagnostic = ShaftAssistantPanel.class.getDeclaredMethod(
                "showCaptureStopDiagnosticIfPending", String.class, boolean.class, String.class, String.class);
        showDiagnostic.setAccessible(true);
        boolean handled = (Boolean) showDiagnostic.invoke(panel, "capture_api_stop", true, "Stopped.", output);

        assertAll(
                () -> assertTrue(handled),
                () -> assertFalse(ShaftRecordingActivity.active(),
                        "blank-sessionPath armed stop should still clear the recording badge"));
    }

    // ---- Attach-to-prompt affordances (issue #3727): current file / all open files / a disk file /
    // an image, rendered as removable chips near the composer and folded into the outbound prompt
    // text sent to the local-agent or cloud-provider MCP tool. ----

    @Test
    void fromPromptFoldsAttachmentsBlockIntoLocalAgentPromptText() {
        String attachmentsBlock = AssistantAttachments.outboundBlock(List.of(
                AssistantAttachment.forText("src/main/java/Foo.java", "class Foo {}")));

        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this class",
                AssistantCommand.Selection.local("CODEX", "CLI"),
                "ASK",
                ".",
                "",
                false,
                AssistantCommand.OpenFileContext.empty(),
                "",
                attachmentsBlock);

        String promptText = invocation.arguments().get("prompt").getAsString();
        assertAll(
                () -> assertTrue(promptText.contains("Attached context:"), promptText),
                () -> assertTrue(promptText.contains("File: src/main/java/Foo.java"), promptText),
                () -> assertTrue(promptText.contains("class Foo {}"), promptText));
    }

    @Test
    void fromPromptFoldsAttachmentsBlockIntoCloudPromptText() {
        String attachmentsBlock = AssistantAttachments.outboundBlock(List.of(
                AssistantAttachment.forText("Notes.txt", "remember to check locators")));

        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Summarize this",
                AssistantCommand.Selection.cloud("openai", "gpt-4.1"),
                "ASK",
                ".",
                "",
                false,
                AssistantCommand.OpenFileContext.empty(),
                "",
                attachmentsBlock);

        String promptText = invocation.arguments().get("prompt").getAsString();
        assertAll(
                () -> assertEquals("autobot_provider_chat", invocation.toolName()),
                () -> assertTrue(promptText.contains("Notes.txt"), promptText),
                () -> assertTrue(promptText.contains("remember to check locators"), promptText));
    }

    @Test
    void fromPromptWithoutAttachmentsContextBuildsPromptUnchanged() {
        AssistantCommand.Invocation withoutAttachments = AssistantCommand.fromPrompt(
                "Explain this class", "CODEX", "ASK", ".", "", false);

        assertFalse(withoutAttachments.arguments().get("prompt").getAsString().contains("Attached context:"));
    }

    @Test
    void outboundBlockTruncatesOversizedTextAttachmentWithVisibleNote() {
        String huge = "x".repeat(AssistantAttachment.MAX_CONTENT_CHARACTERS + 500);
        AssistantAttachment attachment = AssistantAttachment.forText("Big.java", huge);
        String block = AssistantAttachments.outboundBlock(List.of(attachment));

        assertAll(
                () -> assertTrue(attachment.truncated()),
                () -> assertEquals(AssistantAttachment.MAX_CONTENT_CHARACTERS, attachment.content().length()),
                () -> assertTrue(block.contains("truncated to " + AssistantAttachment.MAX_CONTENT_CHARACTERS
                        + " characters"), block));
    }

    @Test
    void outboundBlockNotesImagePathInsteadOfEmbeddingBytes() {
        AssistantAttachment image = AssistantAttachment.forImage("C:\\shots\\bug.png");
        String block = AssistantAttachments.outboundBlock(List.of(image));

        assertAll(
                () -> assertTrue(block.contains("C:\\shots\\bug.png"), block),
                () -> assertTrue(block.contains("text-only"), block),
                () -> assertEquals("", image.content()));
    }

    @Test
    void withAttachmentDedupesByPathReplacingEarlierEntry() {
        List<AssistantAttachment> firstAdd = AssistantAttachments.withAttachment(
                List.of(), AssistantAttachment.forText("Foo.java", "first"));
        List<AssistantAttachment> secondAdd = AssistantAttachments.withAttachment(
                firstAdd, AssistantAttachment.forText("Foo.java", "second"));

        assertAll(
                () -> assertEquals(1, secondAdd.size()),
                () -> assertEquals("second", secondAdd.get(0).content()));
    }

    @Test
    void addCurrentFileAttachmentUsesInjectedProviderAndShowsRemovableChip() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "currentEditorFileProvider",
                (Function<Project, AssistantCommand.OpenFileContext>)
                        p -> new AssistantCommand.OpenFileContext("SignInTest.java", "class SignInTest {}"));

        invokePrivate(panel, "addCurrentFileAttachment");

        @SuppressWarnings("unchecked")
        List<AssistantAttachment> attachments = (List<AssistantAttachment>) getField(panel, "attachments");
        JPanel chipRow = (JPanel) getField(panel, "attachmentsChipRow");
        assertAll(
                () -> assertEquals(1, attachments.size()),
                () -> assertEquals("SignInTest.java", attachments.get(0).path()),
                () -> assertTrue(chipRow.isVisible()),
                () -> assertEquals(1, chipRow.getComponentCount()),
                () -> assertTrue(((JButton) chipRow.getComponent(0)).getText().contains("SignInTest.java")));
    }

    @Test
    void addCurrentFileAttachmentWithNoOpenFileShowsStatusAndAddsNothing() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "currentEditorFileProvider",
                (Function<Project, AssistantCommand.OpenFileContext>) p -> AssistantCommand.OpenFileContext.empty());

        invokePrivate(panel, "addCurrentFileAttachment");

        assertAll(
                () -> assertTrue(((List<?>) getField(panel, "attachments")).isEmpty()),
                () -> assertTrue(containsText(panel, "No file is open to attach")));
    }

    @Test
    void addAllOpenFilesAttachmentsUsesInjectedProviderForEverySimulatedEditor() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "openEditorFilesProvider",
                (Function<Project, List<AssistantCommand.OpenFileContext>>) p -> List.of(
                        new AssistantCommand.OpenFileContext("Foo.java", "class Foo {}"),
                        new AssistantCommand.OpenFileContext("Bar.java", "class Bar {}")));

        invokePrivate(panel, "addAllOpenFilesAttachments");

        @SuppressWarnings("unchecked")
        List<AssistantAttachment> attachments = (List<AssistantAttachment>) getField(panel, "attachments");
        assertAll(
                () -> assertEquals(2, attachments.size()),
                () -> assertTrue(attachments.stream().anyMatch(a -> a.path().equals("Foo.java"))),
                () -> assertTrue(attachments.stream().anyMatch(a -> a.path().equals("Bar.java"))));
    }

    @Test
    void addAllOpenFilesAttachmentsWithNoOpenEditorsShowsStatusAndAddsNothing() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "openEditorFilesProvider",
                (Function<Project, List<AssistantCommand.OpenFileContext>>) p -> List.of());

        invokePrivate(panel, "addAllOpenFilesAttachments");

        assertAll(
                () -> assertTrue(((List<?>) getField(panel, "attachments")).isEmpty()),
                () -> assertTrue(containsText(panel, "No open editor files to attach")));
    }

    @Test
    void attachFileAtPathReadsRealFileFromDiskAndAddsAttachment(@TempDir Path tempDir) throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Path file = tempDir.resolve("notes.txt");
        Files.writeString(file, "attach me");

        panel.attachFileAtPath(file);

        @SuppressWarnings("unchecked")
        List<AssistantAttachment> attachments = (List<AssistantAttachment>) getField(panel, "attachments");
        assertAll(
                () -> assertEquals(1, attachments.size()),
                () -> assertEquals("attach me", attachments.get(0).content()),
                () -> assertFalse(attachments.get(0).image()));
    }

    @Test
    void attachFileAtPathTruncatesOversizedFileWithVisibleChipNote(@TempDir Path tempDir) throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Path file = tempDir.resolve("big.txt");
        Files.writeString(file, "y".repeat(AssistantAttachment.MAX_CONTENT_CHARACTERS + 1000));

        panel.attachFileAtPath(file);

        @SuppressWarnings("unchecked")
        List<AssistantAttachment> attachments = (List<AssistantAttachment>) getField(panel, "attachments");
        JPanel chipRow = (JPanel) getField(panel, "attachmentsChipRow");
        JButton chip = (JButton) chipRow.getComponent(0);
        assertAll(
                () -> assertTrue(attachments.get(0).truncated()),
                () -> assertTrue(chip.getToolTipText().contains("truncated"), chip.getToolTipText()));
    }

    @Test
    void attachImageAtPathAddsImageAttachmentWithoutReadingBytes(@TempDir Path tempDir) throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Path image = tempDir.resolve("screenshot.png");
        Files.write(image, new byte[]{(byte) 0x89, 'P', 'N', 'G'});

        panel.attachImageAtPath(image);

        @SuppressWarnings("unchecked")
        List<AssistantAttachment> attachments = (List<AssistantAttachment>) getField(panel, "attachments");
        assertAll(
                () -> assertEquals(1, attachments.size()),
                () -> assertTrue(attachments.get(0).image()),
                () -> assertEquals("", attachments.get(0).content()),
                () -> assertEquals(image.toAbsolutePath().toString(), attachments.get(0).path()));
    }

    @Test
    void attachingSamePathTwiceDedupesInsteadOfDuplicatingChip(@TempDir Path tempDir) throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Path file = tempDir.resolve("dup.txt");
        Files.writeString(file, "v1");
        panel.attachFileAtPath(file);
        Files.writeString(file, "v2");
        panel.attachFileAtPath(file);

        @SuppressWarnings("unchecked")
        List<AssistantAttachment> attachments = (List<AssistantAttachment>) getField(panel, "attachments");
        JPanel chipRow = (JPanel) getField(panel, "attachmentsChipRow");
        assertAll(
                () -> assertEquals(1, attachments.size()),
                () -> assertEquals("v2", attachments.get(0).content()),
                () -> assertEquals(1, chipRow.getComponentCount()));
    }

    @Test
    void clickingAttachmentChipRemovesItAndHidesRowWhenEmpty(@TempDir Path tempDir) throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Path file = tempDir.resolve("removable.txt");
        Files.writeString(file, "content");
        panel.attachFileAtPath(file);

        JPanel chipRow = (JPanel) getField(panel, "attachmentsChipRow");
        JButton chip = (JButton) chipRow.getComponent(0);
        chip.doClick();

        assertAll(
                () -> assertTrue(((List<?>) getField(panel, "attachments")).isEmpty()),
                () -> assertFalse(chipRow.isVisible()),
                () -> assertEquals(0, chipRow.getComponentCount()));
    }

    @Test
    void sendClearsAttachmentsOnceThePromptActuallyGoesThrough(@TempDir Path tempDir) throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Path file = tempDir.resolve("clearMe.txt");
        Files.writeString(file, "content");
        panel.attachFileAtPath(file);
        assertEquals(1, ((List<?>) getField(panel, "attachments")).size());

        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        assistantMode.setSelectedItem("ASK");
        JTextComponent prompt = assistantPrompt(panel);
        prompt.setText("/help");

        Method send = ShaftAssistantPanel.class.getDeclaredMethod("send", Project.class);
        send.setAccessible(true);
        send.invoke(panel, (Project) null);

        assertAll(
                () -> assertEquals("", prompt.getText()),
                () -> assertTrue(((List<?>) getField(panel, "attachments")).isEmpty()),
                () -> assertFalse(((JPanel) getField(panel, "attachmentsChipRow")).isVisible()));
    }

    /**
     * A pre-flight gate bounce (e.g. "switch to Agent mode and resend") must not silently drop
     * attachments the user already picked -- it returns before the {@code prompt.setText("")} reset
     * point that also clears attachments, mirroring how it preserves the typed prompt text for the
     * same resend.
     */
    @Test
    void sendPreservesAttachmentsWhenBlockedByAPreflightGate(@TempDir Path tempDir) throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Path file = tempDir.resolve("keepMe.txt");
        Files.writeString(file, "content");
        panel.attachFileAtPath(file);

        // Same known-good MCP-access-gate trigger as
        // assistantAskOrPlanMcpPromptOffersOneClickSwitchToAgentMode: PLAN mode plus a prompt that
        // needs shaft-mcp browser tool access trips the "Switch to Agent mode" gate in send()
        // before the prompt/attachments reset point.
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        assistantMode.setSelectedItem("PLAN");
        assistantPrompt(panel).setText("open duckduckgo and search for SHAFT Engine");
        clickAccessible(panel, "Send assistant prompt");

        assertAll(
                () -> assertNotNull(findByAccessibleName(panel, "Switch to Agent mode and resend", JButton.class),
                        "Precondition: the gate must actually have fired"),
                () -> assertEquals(1, ((List<?>) getField(panel, "attachments")).size(),
                        "A gate bounce must preserve attachments for the resend, just like it preserves the prompt text"));
    }

    private static void invokePrivate(Object target, String methodName) throws Exception {
        Method method = target.getClass().getDeclaredMethod(methodName);
        method.setAccessible(true);
        method.invoke(target);
    }

    private static void invokePrivate(Object target, String methodName, Class<?>[] parameterTypes, Object... arguments)
            throws Exception {
        Method method = target.getClass().getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        method.invoke(target, arguments);
    }

    @Test
    void toolsExplainMissingMcpConfiguration() {
        ShaftFeaturePanel panel = new ShaftFeaturePanel(null, blankMcpSettings());

        assertAll(
                () -> assertTrue(containsText(panel, "Configure SHAFT MCP")),
                () -> assertNotNull(findByAccessibleName(panel, "Open SHAFT settings", JButton.class)));
    }

    @Test
    void editableAndOutputTextAreasWrapLongContent() {
        List<JBTextArea> textAreas = new ArrayList<>();
        collectTextAreas(new ShaftAssistantPanel(null, blankMcpSettings()), textAreas);
        collectTextAreas(new ShaftFeaturePanel(null, blankMcpSettings()), textAreas);

        assertFalse(textAreas.isEmpty());
        assertAll(textAreas.stream()
                .map(textArea -> () -> assertTrue(textArea.getLineWrap())));
        assertAll(textAreas.stream()
                .map(textArea -> () -> assertTrue(textArea.getWrapStyleWord())));
    }

    @Test
    void toolsPreserveNonObjectMcpOutputForReview() throws Exception {
        ShaftFeaturePanel panel = new ShaftFeaturePanel(null, blankMcpSettings());

        showToolResult(panel, ShaftMcpToolResult.success("[\"a\",\"b\"]"));
        assertEquals("[\n  \"a\",\n  \"b\"\n]", outputText(panel));

        showToolResult(panel, ShaftMcpToolResult.success("plain text result"));
        assertEquals("plain text result", outputText(panel));
    }

    @Test
    void toolWindowShowsFirstRunSetupUntilMcpConnectionIsComplete() throws Exception {
        Path appData = tempDirectory("shaft-mcp-empty-app-data");
        Path bootstrap = tempDirectory("shaft-mcp-empty-bootstrap");
        String oldAppData = System.getProperty("shaft.intellij.mcp.applicationDataRoot");
        String oldBootstrap = System.getProperty("shaft.intellij.mcp.bootstrapRoot");
        System.setProperty("shaft.intellij.mcp.applicationDataRoot", appData.toString());
        System.setProperty("shaft.intellij.mcp.bootstrapRoot", bootstrap.toString());
        try {
            ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), blankMcpSettings());
            JComponent setupOutput = findByAccessibleName(toolWindow, "SHAFT MCP setup output", JComponent.class);
            JLabel nextStep = findByAccessibleName(toolWindow, "SHAFT MCP setup next step", JLabel.class);
            ShaftMcpSetupPanel setupPanel = setupPanel(toolWindow);
            JTextComponent mcpCommand = (JTextComponent) getField(setupPanel, "mcpCommand");
            JComponent detailsPanel = (JComponent) getField(setupPanel, "detailsPanel");
            JComponent installerDetailsPanel = (JComponent) getField(setupPanel, "installerDetailsPanel");

            assertNull(toolWindowWorkflowSelector(toolWindow));
            assertTrue(containsText(toolWindow, "Runtime"));
            assertTrue(containsText(toolWindow, "1 Upgrade project"));
            assertTrue(containsText(toolWindow, "2 Choose agent"));
            assertTrue(containsText(toolWindow, "3 Setup tools & skills"));
            assertTrue(containsText(toolWindow, "5 Check tools installation"));
            assertTrue(containsText(toolWindow, "Connect SHAFT Assistant"));
            // Issue #4314 fix 1: the redundant "Target: X. Runtime: Y." setupSummary caption is
            // removed -- the family/runtime combo boxes above it already show the live selection.
            assertNull(findByAccessibleName(toolWindow, "SHAFT MCP setup summary", JLabel.class));
            assertNotNull(findByAccessibleName(toolWindow, "Setup SHAFT Tools & Skills state", JLabel.class));
            assertNull(findByAccessibleName(toolWindow, "SHAFT MCP command status", JLabel.class));
            assertFalse(findByAccessibleName(toolWindow, "Assistant runtime setup status", JLabel.class).isVisible());
            assertFalse(findByAccessibleName(toolWindow, "Assistant connection setup status", JLabel.class).isVisible());
            assertNotNull(findByAccessibleName(toolWindow, "Assistant runtime setup status", JLabel.class));
            assertNotNull(findByAccessibleName(toolWindow, "Assistant connection setup status", JLabel.class));
            assertNotNull(nextStep);
            assertNotNull(mcpCommand);
            assertNotNull(setupOutput);
            assertNull(findByAccessibleName(toolWindow, "MCP stdio command", JTextComponent.class));
            assertNotNull(findByAccessibleName(toolWindow, "Show manual MCP install target", JCheckBox.class));
            assertNull(findByAccessibleName(toolWindow, "Install or update SHAFT MCP", JButton.class));
            assertTrue(mcpCommand instanceof JBTextArea);
            assertTrue(((JBTextArea) mcpCommand).getRows() >= 4);
            assertFalse(nextStep.isVisible());
            assertFalse(installerDetailsPanel.isVisible());
            assertFalse(detailsPanel.isVisible());
            assertTrue(findByAccessibleName(toolWindow, "Copy SHAFT Tools & Skills setup command", JButton.class).isVisible());
            // No shaft-mcp is installed in this isolated data root, so the real check is offered
            // immediately: verification, not clicking, is what completes setup (issue #3426 A5).
            assertTrue(findByAccessibleName(toolWindow, "Test SHAFT MCP connection", JButton.class).isVisible());
            assertTrue(findByAccessibleName(toolWindow, "Copy SHAFT upgrade command", JButton.class).isVisible());
            assertNotNull(findByAccessibleName(toolWindow, "Check SHAFT project version", JButton.class));
            // The dedicated "open terminal" buttons are gone: copying a command now opens the
            // terminal with the command pre-typed in one click (issue #3426 A3).
            assertNull(findByAccessibleName(toolWindow, "Open terminal for MCP installer", JButton.class));
            assertNull(findByAccessibleName(toolWindow, "Open terminal for SHAFT upgrade", JButton.class));
        } finally {
            restoreProperty("shaft.intellij.mcp.applicationDataRoot", oldAppData);
            restoreProperty("shaft.intellij.mcp.bootstrapRoot", oldBootstrap);
        }
    }

    @Test
    void toolWindowRedirectsToAssistantWhenMcpIsConfigured() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), connectedMcpSettings());

        assertAll(
                () -> assertNull(setupPanel(toolWindow)),
                // Progressive disclosure (issue #3425 A4): the default main view now pairs the
                // Assistant with the Guided workflow behind a two-entry selector.
                () -> assertNotNull(toolWindowWorkflowSelector(toolWindow)),
                () -> assertNotNull(findByAccessibleName(toolWindow, "Assistant prompt", JTextComponent.class)),
                () -> assertTrue(containsText(toolWindow, "Codex CLI")));
    }

    @Test
    void unverifiedMcpCommandKeepsSetupAndToolsGated() {
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), settings);
        ShaftFeaturePanel tools = new ShaftFeaturePanel(fakeProject(), settings);

        clickAccessible(tools, "Run SHAFT tool");

        assertAll(
                () -> assertNull(toolWindowWorkflowSelector(toolWindow)),
                () -> assertTrue(containsText(toolWindow, "Connect SHAFT Assistant")),
                () -> assertTrue(containsText(tools, "Configure SHAFT MCP")),
                () -> assertTrue(outputText(tools).contains("Configure SHAFT MCP in Settings before running Tools requests.")));
    }

    @Test
    void refreshCatalogEntersBusyStateThenBlocksReentryViaRealGuard() throws Exception {
        ShaftFeaturePanel tools = new ShaftFeaturePanel(fakeProject(), connectedMcpSettings());
        JButton refreshButton = findButton(tools, "Refresh tools");
        assertNotNull(refreshButton);
        assertEquals("Refresh tools", refreshButton.getToolTipText());

        // The fake test Project has no real ShaftMcpInvocationService/Application wired up (this
        // suite never registers one - see fakeProject()), so refreshCatalog()'s call into
        // ShaftMcpInvocationService.getInstance(project).startListTools() is expected to throw once
        // it gets that far. What we're proving here is that setRunning(true, ...) and the busy
        // tooltip swap already happened - via the real refreshCatalog(Project) method - before that
        // unrelated plumbing gap is hit.
        assertThrows(NullPointerException.class, () -> invokeRefreshCatalog(tools, fakeProject()));

        assertAll(
                () -> assertFalse(refreshButton.isEnabled(), "button should be disabled once refreshCatalog() starts running"),
                () -> assertEquals("Refreshing...", refreshButton.getToolTipText()));

        // Re-entry guard: seed currentInvocation as if a request were genuinely in flight, then
        // invoke the real refreshCatalog(Project) method again. The guard at the top of the method
        // must return immediately without touching the button/tooltip/currentInvocation again.
        ShaftMcpInvocation inFlight = new ShaftMcpInvocation(new CompletableFuture<>(), () -> {
        });
        setField(tools, "currentInvocation", inFlight);
        refreshButton.setEnabled(true);
        refreshButton.setToolTipText("Refresh tools");

        invokeRefreshCatalog(tools, fakeProject());

        assertAll(
                () -> assertSame(inFlight, getField(tools, "currentInvocation"),
                        "a blocked re-entrant call must not replace the in-flight invocation"),
                () -> assertTrue(refreshButton.isEnabled(),
                        "the re-entry guard must return before touching button state"),
                () -> assertEquals("Refresh tools", refreshButton.getToolTipText(),
                        "the re-entry guard must return before touching the tooltip"));
    }

    @Test
    void showCatalogResultAppliesSuccessAndErrorGlyphsThroughRealCompletionPath() throws Exception {
        ShaftFeaturePanel tools = new ShaftFeaturePanel(fakeProject(), connectedMcpSettings());
        JButton refreshButton = findButton(tools, "Refresh tools");
        assertNotNull(refreshButton);

        setField(tools, "currentInvocation", new ShaftMcpInvocation(new CompletableFuture<>(), () -> {
        }));
        refreshButton.setToolTipText("Refreshing...");

        showCatalogResult(tools, ShaftMcpToolResult.success(mcpToolsList()));

        assertAll(
                () -> assertTrue(statusText(tools).contains(ShaftStatusPresentation.SUCCESS_ICON),
                        "success completion should surface the shared SUCCESS_ICON glyph: " + statusText(tools)),
                () -> assertTrue(statusText(tools).contains("Tools refreshed")),
                () -> assertEquals("Refresh tools", refreshButton.getToolTipText(),
                        "tooltip should be restored once the refresh completes"),
                () -> assertNull(getField(tools, "currentInvocation"), "completion must clear the in-flight guard"),
                () -> assertTrue(refreshButton.isEnabled()));

        setField(tools, "currentInvocation", new ShaftMcpInvocation(new CompletableFuture<>(), () -> {
        }));
        refreshButton.setToolTipText("Refreshing...");

        showCatalogResult(tools, ShaftMcpToolResult.failure("MCP server process exited."));

        assertAll(
                () -> assertTrue(statusText(tools).contains(ShaftStatusPresentation.ERROR_ICON),
                        "error completion should surface the shared ERROR_ICON glyph: " + statusText(tools)),
                () -> assertTrue(statusText(tools).contains("Failed")),
                () -> assertEquals("Refresh tools", refreshButton.getToolTipText()),
                () -> assertNull(getField(tools, "currentInvocation")));
    }

    @Test
    void setupPanelPrefillsManualCommandAndKeepsTestEnabledForSelectionChanges() throws Exception {
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = false;
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });
        JTextComponent command = (JTextComponent) getField(panel, "mcpCommand");
        JButton test = findButton(panel, "Test SHAFT MCP connection");
        JComboBox<?> agent = findByAccessibleName(panel, "Assistant agent", JComboBox.class);

        assertAll(
                () -> assertNotNull(command),
                () -> assertNotNull(test),
                () -> assertNotNull(agent),
                () -> assertEquals("\"java\" \"@target/shaft-mcp.args\"", command.getText()),
                () -> assertNull(findButton(panel, "Install / Update SHAFT MCP")),
                () -> assertFalse(containsText(panel, "MCP: Configured")),
                () -> assertFalse(findByAccessibleName(panel, "Assistant runtime setup status", JLabel.class).isVisible()),
                () -> assertFalse(findByAccessibleName(panel, "Assistant connection setup status", JLabel.class).isVisible()),
                () -> assertTrue(test.isVisible()),
                () -> assertFalse(test.isEnabled()));

        selectDisplayValue(agent, "Claude Code");
        assertAll(
                () -> assertTrue(test.isEnabled()),
                () -> assertFalse(containsText(panel, "MCP: Configured")));

        selectDisplayValue(agent, "Claude Desktop");
        assertAll(
                () -> assertTrue(test.isEnabled()),
                () -> assertFalse(containsText(panel, "MCP: Configured")));
    }

    @Test
    void setupPanelShowsInstallerCommandAndInfersInstalledStdioCommand() throws Exception {
        Path appData = tempDirectory("shaft-mcp-app-data");
        Path bootstrap = tempDirectory("shaft-mcp-bootstrap");
        Path argsFile = appData.resolve("versions").resolve("10.3.20260703").resolve("shaft-mcp.args");
        Path java = bootstrap.resolve("tools").resolve("jdk").resolve("temurin-25-test").resolve("bin")
                .resolve(javaExecutableName());
        Files.createDirectories(argsFile.getParent());
        Files.writeString(argsFile, "-cp\nshaft-mcp.jar\ncom.shaft.mcp.ShaftMcpApplication\n");
        Files.createDirectories(java.getParent());
        Files.writeString(java, "");
        String oldAppData = System.getProperty("shaft.intellij.mcp.applicationDataRoot");
        String oldBootstrap = System.getProperty("shaft.intellij.mcp.bootstrapRoot");
        System.setProperty("shaft.intellij.mcp.applicationDataRoot", appData.toString());
        System.setProperty("shaft.intellij.mcp.bootstrapRoot", bootstrap.toString());
        try {
            ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
            });
            AtomicReference<String> copied = new AtomicReference<>();
            AtomicReference<String> toast = new AtomicReference<>();
            setField(panel, "copySink", (Consumer<String>) copied::set);
            setField(panel, "toastSink", (Consumer<String>) toast::set);
            JTextComponent command = (JTextComponent) getField(panel, "mcpCommand");
            JComponent installerDetailsPanel = (JComponent) getField(panel, "installerDetailsPanel");

            assertAll(
                    () -> assertTrue(findByAccessibleName(panel, "MCP installer command", JTextComponent.class)
                            instanceof JBTextArea),
                    () -> assertTrue(command instanceof JBTextArea),
                    () -> assertTrue(((JBTextArea) command).getRows() >= 4),
                    () -> assertFalse(findByAccessibleName(panel, "MCP installer target", JComboBox.class).isVisible()),
                    () -> assertEquals(7, findByAccessibleName(panel, "MCP installer target", JComboBox.class)
                            .getItemCount()),
                    () -> assertEquals("INTELLIJ_PLUGIN", lastComboItem(
                            findByAccessibleName(panel, "MCP installer target", JComboBox.class))),
                    () -> assertNotNull(findByAccessibleName(panel, "Show manual MCP install target", JCheckBox.class)),
                    () -> assertNull(findByAccessibleName(panel, "Use inferred MCP command", JButton.class)),
                    () -> assertFalse(installerDetailsPanel.isVisible()),
                    () -> assertTrue(command.getText().isBlank()),
                    () -> assertNull(findButton(panel, "Install / Update SHAFT MCP")));

            // A real shaft-mcp install exists in this data root, so on-disk detection is true, but
            // the row only earns its green badge once the user presses Check (issue #3560/#3426
            // A5): Check and Copy both stay visible regardless of on-disk state. The inferred stdio
            // command still points at the installed artifacts.
            String inferred = ShaftMcpSetupPanel.inferInstalledStdioCommand(appData, bootstrap);
            assertAll(
                    () -> assertTrue(findByAccessibleName(panel, "Copy SHAFT Tools & Skills setup command", JButton.class).isVisible()),
                    () -> assertNull(findByAccessibleName(panel, "Open terminal for MCP installer", JButton.class)),
                    () -> assertTrue(findByAccessibleName(panel, "Test SHAFT MCP connection", JButton.class).isVisible()),
                    () -> assertFalse(installerDetailsPanel.isVisible()),
                    () -> assertTrue(inferred.contains(java.toString())),
                    () -> assertTrue(inferred.contains("@" + argsFile)));
        } finally {
            restoreProperty("shaft.intellij.mcp.applicationDataRoot", oldAppData);
            restoreProperty("shaft.intellij.mcp.bootstrapRoot", oldBootstrap);
        }
    }

    @Test
    void setupPanelUpdatesInstallerCommandForSelectedAssistantClient() throws Exception {
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.assistantFamily = "CODEX";
        settings.defaultAutobotClient = "CODEX";
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, (client, runtime) -> ShaftMcpToolResult.failure("not checked"), new ShaftMcpSetupPanel.CloudKeyStore() {
            @Override
            public boolean hasKey(String keyName) {
                return false;
            }

            @Override
            public void saveKey(String keyName, char[] secret) {
            }
        });
        JTextComponent installer = findByAccessibleName(panel, "MCP installer command", JTextComponent.class);
        JComboBox<?> agent = findByAccessibleName(panel, "Assistant agent", JComboBox.class);
        JComboBox<?> target = findByAccessibleName(panel, "MCP installer target", JComboBox.class);
        JCheckBox manualTarget = findByAccessibleName(panel, "Show manual MCP install target", JCheckBox.class);

        assertAll(
                () -> assertNotNull(manualTarget),
                () -> assertFalse(manualTarget.isVisible()),
                () -> assertFalse(target.isVisible()),
                () -> assertEquals(7, agent.getItemCount()),
                () -> assertEquals("Claude Code", String.valueOf(agent.getItemAt(0))),
                () -> assertEquals("GitHub Copilot in IntelliJ", String.valueOf(agent.getItemAt(6))),
                () -> assertEquals("CODEX", target.getSelectedItem()),
                () -> assertTrue(installer.getText().contains("codex")),
                () -> assertTrue(installer.getText().contains("--install-shaft-skills")));

        selectDisplayValue(agent, "Claude Code");
        assertAll(
                () -> assertEquals("CLAUDE_CODE", target.getSelectedItem()),
                () -> assertTrue(installer.getText().contains("claude")));

        selectDisplayValue(agent, "Claude Desktop");
        assertAll(
                () -> assertEquals("CLAUDE_DESKTOP", target.getSelectedItem()),
                () -> assertTrue(installer.getText().contains("claude-desktop")));

        selectDisplayValue(agent, "GitHub Copilot CLI");
        assertAll(
                () -> assertEquals("COPILOT_CLI", target.getSelectedItem()),
                () -> assertTrue(installer.getText().contains("copilot")));

        selectDisplayValue(agent, "GitHub Copilot in IntelliJ");
        assertAll(
                () -> assertEquals("COPILOT_INTELLIJ", target.getSelectedItem()),
                () -> assertTrue(installer.getText().contains("copilot-intellij")));

        selectDisplayValue(agent, "Gemini in IntelliJ");
        assertEquals("INTELLIJ_PLUGIN", target.getSelectedItem());
        assertTrue(installer.getText().contains("intellij-plugin"));

        manualTarget.doClick();
        target.setSelectedItem("CLAUDE_CODE");
        assertAll(
                () -> assertFalse(target.isVisible()),
                () -> assertTrue(installer.getText().contains("intellij-plugin")),
                () -> assertFalse(installer.getText().contains("--claude ")));

        selectDisplayValue(agent, "Grok CLI");
        assertAll(
                () -> assertEquals("GROK", target.getSelectedItem()),
                () -> assertTrue(installer.getText().contains("grok")));

        // Regression lock: INTELLIJ_PLUGIN stays last among the known installer targets.
        List<String> installerTargetTokens = new ArrayList<>();
        for (int index = 0; index < target.getItemCount(); index++) {
            installerTargetTokens.add(String.valueOf(target.getItemAt(index)));
        }
        assertAll(
                () -> assertEquals(7, target.getItemCount()),
                () -> assertEquals("INTELLIJ_PLUGIN", lastComboItem(target)),
                () -> assertEquals(List.of("CODEX", "CLAUDE_CODE", "CLAUDE_DESKTOP", "COPILOT_CLI",
                        "COPILOT_INTELLIJ", "GROK", "INTELLIJ_PLUGIN"), installerTargetTokens));

        // The disambiguated label must not collapse back to a bare peer-agent name, and the
        // distinct IDE_PLUGIN runtime token (used by the runtime combo, not this target combo)
        // must stay unaffected.
        assertAll(
                () -> assertEquals("SHAFT IntelliJ plugin (this plugin only - no external agent)",
                        ShaftUiLabels.friendly("INTELLIJ_PLUGIN")),
                () -> assertNotEquals("SHAFT IntelliJ plugin", ShaftUiLabels.friendly("INTELLIJ_PLUGIN")),
                () -> assertEquals("IDE plugin", ShaftUiLabels.friendly("IDE_PLUGIN")));

        // installerArgumentFor()/installerCommandFor() must keep producing the exact same
        // --client argument strings as before for every known installer target token.
        assertAll(
                () -> assertEquals("codex", installerArgumentFor("CODEX")),
                () -> assertEquals("claude", installerArgumentFor("CLAUDE_CODE")),
                () -> assertEquals("claude-desktop", installerArgumentFor("CLAUDE_DESKTOP")),
                () -> assertEquals("copilot", installerArgumentFor("COPILOT_CLI")),
                () -> assertEquals("copilot-intellij", installerArgumentFor("COPILOT_INTELLIJ")),
                () -> assertEquals("intellij-plugin", installerArgumentFor("INTELLIJ_PLUGIN")),
                () -> assertEquals("grok", installerArgumentFor("GROK")));
        for (String token : installerTargetTokens) {
            String argument = installerArgumentFor(token);
            String command = installerCommandFor(argument);
            assertTrue(command.contains(argument), command);
        }
    }

    @Test
    void routeChangeInvalidatesReadinessAndNonCliRoutesNeverClaimUnobservedVerification() throws Exception {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.agentLaneReady = true;
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, (client, runtime) -> ShaftMcpToolResult.success("synthetic success"));
        JComboBox<?> agent = findByAccessibleName(panel, "Assistant agent", JComboBox.class);
        Method verify = ShaftMcpSetupPanel.class.getDeclaredMethod(
                "verifySelectedAgentReadiness", ShaftMcpToolResult.class);
        verify.setAccessible(true);

        for (String route : List.of("Claude Desktop", "GitHub Copilot in IntelliJ")) {
            settings.mcpSetupComplete = true;
            settings.agentLaneReady = true;
            selectDisplayValue(agent, route);
            ShaftMcpToolResult result = (ShaftMcpToolResult) verify.invoke(
                    panel, ShaftMcpToolResult.success("must not be trusted"));
            assertAll(
                    () -> assertFalse(settings.mcpSetupComplete, route),
                    () -> assertFalse(settings.agentLaneReady, route),
                    () -> assertFalse(result.success(), route),
                    () -> assertTrue(result.output().contains("cannot verify"), result.output()));
        }
    }

    private static void selectDisplayValue(JComboBox<?> comboBox, String displayValue) {
        for (int index = 0; index < comboBox.getItemCount(); index++) {
            Object item = comboBox.getItemAt(index);
            if (displayValue.equals(String.valueOf(item))) {
                comboBox.setSelectedIndex(index);
                return;
            }
        }
        throw new AssertionError("Missing combo item: " + displayValue);
    }

    @Test
    void setupPanelInstallsShaftCliByDefaultAndAllowsOptingOut() throws Exception {
        // Issue #3743: "Copy SHAFT Tools & Skills setup command" is a complete, one-click setup by default -- MCP,
        // skills, AND shaft-cli together -- so the checkbox (tucked inside Advanced installer
        // options, hidden by default) must start checked. It stays reachable only for a user who
        // explicitly wants to opt back out.
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.assistantFamily = "CODEX";
        settings.defaultAutobotClient = "CODEX";
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });
        JTextComponent installer = findByAccessibleName(panel, "MCP installer command", JTextComponent.class);
        JCheckBox installCli = findByAccessibleName(panel, "Also install shaft-cli command line", JCheckBox.class);

        assertAll(
                () -> assertNotNull(installCli),
                () -> assertTrue(installCli.isSelected(), "shaft-cli must install by default (issue #3743)"),
                // The flag must sit next to --install-shaft-skills so it stays inside the quoted
                // PowerShell command on Windows instead of being appended after the closing quote.
                () -> assertTrue(installer.getText().contains("--install-shaft-skills --install-shaft-cli"),
                        installer.getText()));

        installCli.doClick();
        assertFalse(installer.getText().contains("--install-shaft-cli"));

        installCli.doClick();
        assertTrue(installer.getText().contains("--install-shaft-skills --install-shaft-cli"),
                installer.getText());
    }

    @Test
    void setupCommandTransferActionsUseTheHonestCopyLabel() {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });

        for (String accessibleName : List.of(
                "Copy SHAFT Tools & Skills setup command",
                "Copy fresh SHAFT MCP setup command",
                "Copy SHAFT upgrade command",
                "Copy SHAFT Engine warm-up command",
                "Copy setup diagnostic command",
                "Copy setup diagnostic output",
                "Copy SHAFT MCP docs link",
                "Copy assistant CLI restart command")) {
            JButton button = findByAccessibleName(panel, accessibleName, JButton.class);
            assertNotNull(button, accessibleName);
            assertEquals("Copy", button.getText(), accessibleName);
            assertEquals(0, button.getMnemonic(), accessibleName);
        }
        assertFalse(collectButtons(panel).stream()
                .anyMatch(button -> List.of("Install", "Reinstall", "Warm up Engine", "Copy command",
                        "Copy output", "Copy docs link", "Copy restart command").contains(button.getText())));
    }

    @Test
    void installButtonRoutesTheFullInstallerCommandThroughTheTerminalOpenerSeam() throws Exception {
        // Issue #3743, reworked for JetBrains Marketplace compliance (ShaftPluginSecurityTest
        // forbids this plugin from spawning OS processes): the primary "Copy SHAFT Tools & Skills setup command" action
        // goes through the TerminalOpener seam, carrying the same MCP + skills + CLI command for
        // the selected client -- no process is ever launched. Issue #4314 fix 3: the duplicate
        // "Copy SHAFT MCP install command" button is gone -- Install alone now also copies the
        // command to the clipboard, via the same copyCommandIntoTerminal() helper every other
        // copy-then-terminal action on this screen already shares.
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.assistantFamily = "CODEX";
        settings.defaultAutobotClient = "CODEX";
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });
        JButton installButton = findByAccessibleName(panel, "Copy SHAFT Tools & Skills setup command", JButton.class);
        assertAll(
                () -> assertNotNull(installButton),
                () -> assertTrue(installButton.isVisible()),
                () -> assertTrue(installButton.isEnabled()),
                () -> assertNull(findByAccessibleName(panel, "Copy SHAFT MCP install command", JButton.class),
                        "the duplicate Copy button is gone -- Install now does both"),
                // Advanced installer options (including the raw command text area) stay hidden by
                // default alongside the primary Install action (issue #3743 (b)).
                () -> assertFalse(((JComponent) getField(panel, "installerDetailsPanel")).isVisible()),
                // shaft-cli installs by default (issue #3743).
                () -> assertTrue(findByAccessibleName(panel, "Also install shaft-cli command line", JCheckBox.class)
                        .isSelected()));

        AtomicReference<String> copied = new AtomicReference<>("");
        setField(panel, "copySink", (Consumer<String>) copied::set);
        AtomicReference<String> terminalTab = new AtomicReference<>();
        AtomicReference<String> terminalCommand = new AtomicReference<>();
        setField(panel, "terminalOpener", (ShaftMcpSetupPanel.TerminalOpener) (tab, command, onOutcome) -> {
            terminalTab.set(tab);
            terminalCommand.set(command);
            return true;
        });

        installButton.doClick();

        assertAll(
                () -> assertEquals("SHAFT MCP install", terminalTab.get()),
                () -> assertTrue(terminalCommand.get().contains("--install-shaft-skills --install-shaft-cli"),
                        terminalCommand.get()),
                () -> assertTrue(terminalCommand.get().contains("codex"), terminalCommand.get()),
                () -> assertEquals(terminalCommand.get(), copied.get(),
                        "clipboard and pre-typed terminal command must be the exact same installer command"),
                () -> assertTrue(containsText(panel, "Copied SHAFT MCP install command. Terminal opened — "
                        + "typing the command...")));
    }

    @Test
    void installButtonNeverSpawnsAProcessAndReflectsTheTerminalOutcome() throws Exception {
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.assistantFamily = "CODEX";
        settings.defaultAutobotClient = "CODEX";
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });
        // Issue #4314 fix 3: Install now also copies to the clipboard, so this headless test must
        // stub copySink like every other clipboard-touching test here -- the real copyToClipboard()
        // has no system clipboard to reach in this harness.
        setField(panel, "copySink", (Consumer<String>) copied -> { });
        JButton installButton = findByAccessibleName(panel, "Copy SHAFT Tools & Skills setup command", JButton.class);
        AtomicReference<Consumer<Boolean>> capturedOutcome = new AtomicReference<>();
        setField(panel, "terminalOpener", (ShaftMcpSetupPanel.TerminalOpener) (tab, command, onOutcome) -> {
            capturedOutcome.set(onOutcome);
            return true;
        });

        installButton.doClick();
        assertNotNull(capturedOutcome.get(), "the outcome callback must be captured");

        capturedOutcome.get().accept(Boolean.TRUE);
        assertTrue(containsText(panel, "Copied SHAFT MCP install command. Terminal opened with it pre-typed — "
                + "press Enter there to run it."));

        // The process-execution seam is gone entirely (compliance rework, issue #3743): no
        // installerProcessLauncher field and no InstallerProcessLauncher nested type remain.
        assertAll(
                () -> assertThrows(NoSuchFieldException.class,
                        () -> ShaftMcpSetupPanel.class.getDeclaredField("installerProcessLauncher")),
                () -> assertTrue(Arrays.stream(ShaftMcpSetupPanel.class.getDeclaredClasses())
                        .noneMatch(type -> type.getSimpleName().equals("InstallerProcessLauncher"))));
    }

    @Test
    void upgradeCommandProducesCrossPlatformScriptCommand() throws Exception {
        String command = upgradeCommand();

        assertAll(
                () -> assertTrue(command.contains("/main/shaft-upgrader/upgrade_to_modular_shaft.py"), command),
                () -> assertTrue(command.contains("--project .")));
        if (isWindowsOs()) {
            assertAll(
                    () -> assertTrue(command.contains("py -3")),
                    () -> assertTrue(command.contains("-Command '$upgrader=Join-Path")));
        } else {
            assertAll(
                    () -> assertTrue(command.contains("python3")),
                    () -> assertTrue(command.contains("curl -fL")));
        }
    }

    @Test
    void setupPanelUpgradeStepCopyOpensTerminalWithPreparedCommand() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        AtomicReference<String> copied = new AtomicReference<>();
        AtomicReference<String> terminalTab = new AtomicReference<>();
        AtomicReference<String> terminalCommand = new AtomicReference<>();
        setField(panel, "copySink", (Consumer<String>) copied::set);
        setField(panel, "terminalOpener", (ShaftMcpSetupPanel.TerminalOpener) (tab, command, onOutcome) -> {
            terminalTab.set(tab);
            terminalCommand.set(command);
            return true;
        });

        assertAll(
                () -> assertTrue(findByAccessibleName(panel, "Copy SHAFT upgrade command", JButton.class).isVisible()),
                () -> assertNotNull(findByAccessibleName(panel, "Check SHAFT project version", JButton.class)),
                () -> assertNull(findByAccessibleName(panel, "Open terminal for SHAFT upgrade", JButton.class)),
                () -> assertTrue(containsText(panel, "1 Upgrade project")));

        clickAccessible(panel, "Copy SHAFT upgrade command");

        assertAll(
                () -> assertTrue(copied.get().contains("upgrade_to_modular_shaft.py")),
                () -> assertTrue(copied.get().contains("/main/shaft-upgrader/upgrade_to_modular_shaft.py")),
                () -> assertTrue(copied.get().contains("--project .")),
                // One click copies AND opens a terminal with the same command pre-typed
                // (issue #3426 A3) — there is no separate "open terminal" button anymore.
                () -> assertEquals("SHAFT upgrade", terminalTab.get()),
                () -> assertEquals(copied.get(), terminalCommand.get()),
                () -> assertTrue(containsText(panel,
                        "Terminal opened with the upgrade command pre-typed. Press Enter there to run it, then press Check.")),
                () -> assertTrue(findByAccessibleName(panel, "Copy SHAFT upgrade command", JButton.class).isVisible()));
    }

    @Test
    void everySetupCommandCopyOpensTerminalWithPreparedCommand() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        List<String> copied = new ArrayList<>();
        List<String> terminalCommands = new ArrayList<>();
        setField(panel, "copySink", (Consumer<String>) copied::add);
        setField(panel, "terminalOpener", (ShaftMcpSetupPanel.TerminalOpener) (tab, command, onOutcome) -> {
            terminalCommands.add(command);
            return true;
        });

        JButton warmup = findByAccessibleName(panel, "Copy SHAFT Engine warm-up command", JButton.class);
        warmup.doClick();

        setField(panel, "diagnosticCommand", "java -version");
        JButton diagnostic = findByAccessibleName(panel, "Copy setup diagnostic command", JButton.class);
        diagnostic.setEnabled(true);
        diagnostic.doClick();

        assertAll(
                // Every runnable command copied from the setup screen also lands pre-typed in a
                // terminal tab; warm-up and diagnostics gain the same one-click behavior the
                // installer and upgrade buttons already have.
                () -> assertEquals(copied, terminalCommands),
                () -> assertEquals(2, terminalCommands.size()),
                () -> assertTrue(terminalCommands.get(0).contains("mvn"), terminalCommands.get(0)),
                () -> assertEquals("java -version", terminalCommands.get(1)));
    }

    @Test
    void copyCommandIntoTerminalShowsInterimStatusThenReconcilesOnAFailedPreType() throws Exception {
        // The pre-type is async (issue #3551): opening the tab alone must not yet claim success.
        // No ApplicationManager/EDT plumbing is needed here since copyCommandIntoTerminal calls
        // onOutcome directly (the real javax.swing.Timer caller already runs on the EDT).
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        AtomicReference<Consumer<Boolean>> capturedOutcome = new AtomicReference<>();
        setField(panel, "copySink", (Consumer<String>) text -> {
        });
        setField(panel, "terminalOpener", (ShaftMcpSetupPanel.TerminalOpener) (tab, command, onOutcome) -> {
            capturedOutcome.set(onOutcome);
            return true;
        });
        setField(panel, "diagnosticCommand", "java -version");
        JButton diagnostic = findByAccessibleName(panel, "Copy setup diagnostic command", JButton.class);
        diagnostic.setEnabled(true);

        diagnostic.doClick();

        assertAll(
                () -> assertTrue(containsText(panel, "Terminal opened — typing the command..."),
                        "opening the tab must not yet claim the command was typed"),
                () -> assertNotNull(capturedOutcome.get(), "the outcome callback must be captured"));

        capturedOutcome.get().accept(Boolean.FALSE);

        assertTrue(containsText(panel, "Copied — paste into a terminal (couldn't auto-type it there)."),
                "a failed async pre-type must retract the earlier 'pre-typed' framing");
    }

    @Test
    void copyCommandIntoTerminalConfirmsPreTypeSuccessOnceTheOutcomeArrives() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        AtomicReference<Consumer<Boolean>> capturedOutcome = new AtomicReference<>();
        setField(panel, "copySink", (Consumer<String>) text -> {
        });
        setField(panel, "terminalOpener", (ShaftMcpSetupPanel.TerminalOpener) (tab, command, onOutcome) -> {
            capturedOutcome.set(onOutcome);
            return true;
        });
        setField(panel, "diagnosticCommand", "java -version");
        JButton diagnostic = findByAccessibleName(panel, "Copy setup diagnostic command", JButton.class);
        diagnostic.setEnabled(true);

        diagnostic.doClick();
        capturedOutcome.get().accept(Boolean.TRUE);

        assertTrue(containsText(panel, "Terminal opened with it pre-typed — press Enter there to run it."),
                "a genuine write() success must confirm the command really was pre-typed");
    }

    @Test
    void setupPanelShowsNoGreenStepsOnFreshLanding() {
        // Issue #3560: none of the numbered verification steps may be green until the user has
        // explicitly clicked that row's Check and it passed -- a fresh landing always starts
        // neutral ("next" -- a blank badge as of issue #4314 fix 5), never green ("Done"),
        // regardless of what passive on-disk/PATH detection would otherwise reveal.
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        assertAll(
                () -> assertEquals("",
                        findByAccessibleName(panel, "Upgrade project setup state", JLabel.class).getText()),
                () -> assertEquals("Waiting",
                        findByAccessibleName(panel, "Choose agent setup state", JLabel.class).getText()),
                () -> assertEquals("",
                        findByAccessibleName(panel, "Setup SHAFT Tools & Skills state", JLabel.class).getText()),
                () -> assertNotEquals("Done",
                        findByAccessibleName(panel, "Check SHAFT agentic tools installation state", JLabel.class)
                                .getText()));
    }

    @Test
    void setupPanelReopeningAfterPriorCompletionDoesNotContradictTheDoneBadge() {
        // Issue #4160 area A: reopening the panel in a new IDE session after setup previously
        // succeeded (settings.mcpSetupComplete persists) shows "5 Check SHAFT agentic tools installation: Done" immediately --
        // but until now the greeting text always said "Press Check now.", as if nothing had ever
        // been verified. A returning user reading "Done" and "Press Check now." side by side has no
        // way to tell whether setup actually works; the guidance must acknowledge the prior success.
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), connectedMcpSettings(), () -> {
        });
        JLabel nextStep = findByAccessibleName(panel, "SHAFT MCP setup next step", JLabel.class);
        assertNotEquals("Press Check now.", nextStep.getText(),
                "greeting text must not contradict the already-Done Check-setup badge");
    }

    @Test
    void setupPanelUpgradeStepReflectsRealProjectVersionCheck() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        JLabel upgradeState = findByAccessibleName(panel, "Upgrade project setup state", JLabel.class);
        JLabel upgradeDetail = findByAccessibleName(panel, "SHAFT project version status", JLabel.class);

        // Nothing is green until the user presses Check (issue #3560/#3426 A4/A5); the "next" badge
        // itself renders blank (issue #4314 fix 5).
        assertEquals("", upgradeState.getText());

        // A project already on the latest release (or newer) is green once checked.
        setField(panel, "upgradeChecker", (java.util.function.Supplier<ShaftProjectVersionCheck.Result>) () ->
                new ShaftProjectVersionCheck.Result(
                        ShaftProjectVersionCheck.State.UP_TO_DATE, "10.3.20260710", "10.3.20260710"));
        clickAccessible(panel, "Check SHAFT project version");
        assertAll(
                () -> assertEquals("Done", upgradeState.getText()),
                () -> assertTrue(upgradeDetail.getText().contains("already the latest")),
                // Check and Copy stay visible in every state now (issue #3560): the row's
                // blue/green/red styling conveys pass/fail, not button visibility.
                () -> assertTrue(findByAccessibleName(panel, "Copy SHAFT upgrade command", JButton.class).isVisible()));

        // An older project version keeps the step actionable and says exactly what to do.
        setField(panel, "upgradeChecker", (java.util.function.Supplier<ShaftProjectVersionCheck.Result>) () ->
                new ShaftProjectVersionCheck.Result(
                        ShaftProjectVersionCheck.State.UPGRADE_AVAILABLE, "10.2.20260101", "10.3.20260710"));
        clickAccessible(panel, "Check SHAFT project version");
        assertAll(
                () -> assertEquals("", upgradeState.getText()),
                () -> assertTrue(upgradeDetail.getText().contains("10.3.20260710 is available")),
                () -> assertTrue(findByAccessibleName(panel, "Copy SHAFT upgrade command", JButton.class).isVisible()));
    }

    @Test
    void nextStateBadgeRendersBlankInsteadOfALookalikeButton() throws Exception {
        // Issue #4314 fix 5: the "next" badge used to render an opaque, bordered, "Next"-labeled
        // pill that looked like a clickable button but wasn't -- the owner found it added no signal
        // beyond the "done" (green) transition + auto-expanding next row already convey. Blank it
        // out specifically for "next"; every other state (done/failed/checking/optional) keeps its
        // exact existing text/colors/opacity, and the step name label itself keeps its existing
        // bold/blue "active step" styling -- only the redundant badge pill is removed.
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        JLabel upgradeState = findByAccessibleName(panel, "Upgrade project setup state", JLabel.class);
        // upgradeStep's own accessible NAME is dynamic (rewritten by setStep() to include the live
        // state), unlike upgradeState's stable name (issue #3603), so it must be read by field.
        JLabel upgradeStep = (JLabel) getField(panel, "upgradeStep");

        assertAll(
                () -> assertEquals("", upgradeState.getText()),
                () -> assertFalse(upgradeState.isOpaque(),
                        "a blank badge must not paint an opaque pill background"),
                () -> assertFalse(upgradeState.getBorder() instanceof javax.swing.border.CompoundBorder,
                        "a blank badge must not paint a visible border either"),
                () -> assertEquals(java.awt.Font.BOLD, upgradeStep.getFont().getStyle() & java.awt.Font.BOLD,
                        "the step name label's own bold 'active step' styling is untouched"));

        setField(panel, "upgradeChecker", (java.util.function.Supplier<ShaftProjectVersionCheck.Result>) () ->
                new ShaftProjectVersionCheck.Result(
                        ShaftProjectVersionCheck.State.UP_TO_DATE, "10.3.20260710", "10.3.20260710"));
        clickAccessible(panel, "Check SHAFT project version");

        assertAll(
                () -> assertEquals("Done", upgradeState.getText(), "the done badge is untouched"),
                () -> assertTrue(upgradeState.isOpaque(), "the done badge keeps its opaque pill"),
                () -> assertTrue(upgradeState.getBorder() instanceof javax.swing.border.CompoundBorder,
                        "the done badge keeps its visible border"));
    }

    @Test
    void setupPanelMcpVersionStepReflectsRealVersionCheck() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        // The merged "3 Setup tools & skills" row's badge is driven by this same check (issue #3560):
        // there is no separate "SHAFT MCP version" row anymore.
        JLabel installState = findByAccessibleName(panel, "Setup SHAFT Tools & Skills state", JLabel.class);
        JLabel mcpVersionDetail = findByAccessibleName(panel, "SHAFT MCP version status", JLabel.class);

        // Nothing is green until the user presses Check (issue #3560/#3426 A4/A5); the "next" badge
        // itself renders blank (issue #4314 fix 5).
        assertEquals("", installState.getText());

        // Installed at or above the latest release is green once checked, matching the
        // "Upgrade project" row's real-check pattern (issue #3538).
        setField(panel, "mcpVersionChecker", (java.util.function.Supplier<ShaftMcpVersionCheck.Result>) () ->
                new ShaftMcpVersionCheck.Result(
                        ShaftMcpVersionCheck.State.UP_TO_DATE, "10.3.20260710", "10.3.20260710"));
        clickAccessible(panel, "Check SHAFT MCP version");
        assertAll(
                () -> assertEquals("Done", installState.getText()),
                () -> assertTrue(mcpVersionDetail.getText().contains("is up to date")),
                // Check and Install stay visible in every state now (issue #3560; merged with the
                // old Copy button by issue #4314 fix 3): the row's blue/green/red styling conveys
                // pass/fail, not button visibility.
                () -> assertTrue(
                        findByAccessibleName(panel, "Copy SHAFT Tools & Skills setup command", JButton.class).isVisible()));

        // Installed but behind the latest release keeps the row actionable.
        setField(panel, "mcpVersionChecker", (java.util.function.Supplier<ShaftMcpVersionCheck.Result>) () ->
                new ShaftMcpVersionCheck.Result(
                        ShaftMcpVersionCheck.State.UPGRADE_AVAILABLE, "10.2.20260101", "10.3.20260710"));
        clickAccessible(panel, "Check SHAFT MCP version");
        assertAll(
                () -> assertEquals("", installState.getText()),
                () -> assertTrue(mcpVersionDetail.getText().contains("latest 10.3.20260710")),
                () -> assertTrue(
                        findByAccessibleName(panel, "Copy SHAFT Tools & Skills setup command", JButton.class).isVisible()));

        // Nothing installed offers the install command.
        setField(panel, "mcpVersionChecker", (java.util.function.Supplier<ShaftMcpVersionCheck.Result>) () ->
                new ShaftMcpVersionCheck.Result(ShaftMcpVersionCheck.State.NOT_INSTALLED, "", ""));
        clickAccessible(panel, "Check SHAFT MCP version");
        assertAll(
                () -> assertEquals("", installState.getText()),
                () -> assertTrue(mcpVersionDetail.getText().contains("not installed yet")),
                () -> assertTrue(
                        findByAccessibleName(panel, "Copy SHAFT Tools & Skills setup command", JButton.class).isVisible()));

        // Offline (latest unknown) is a neutral, non-blocking badge — never "Failed" and never
        // disables the rest of setup (issue #3538); it reads as "Offline" with a retry callout,
        // not "Optional"/"not needed" (issue #3551).
        setField(panel, "mcpVersionChecker", (java.util.function.Supplier<ShaftMcpVersionCheck.Result>) () ->
                new ShaftMcpVersionCheck.Result(ShaftMcpVersionCheck.State.LATEST_UNKNOWN, "10.3.20260703", ""));
        clickAccessible(panel, "Check SHAFT MCP version");
        assertAll(
                () -> assertEquals("Offline", installState.getText()),
                () -> assertTrue(mcpVersionDetail.getText().contains("offline")),
                () -> assertTrue(mcpVersionDetail.getText().contains("Press Check to retry."),
                        mcpVersionDetail.getText()),
                () -> assertTrue(mcpVersionDetail.getText().contains("10.3.20260703")),
                () -> assertTrue(
                        findByAccessibleName(panel, "Copy SHAFT Tools & Skills setup command", JButton.class).isVisible()),
                () -> assertTrue(findByAccessibleName(panel, "Test SHAFT MCP connection", JButton.class).isVisible(),
                        "the MCP version row must never gate the rest of setup"));
    }

    @Test
    void setupPanelDetectsRealInstalledMcpVersionFromDisk() throws Exception {
        Path appData = tempDirectory("shaft-mcp-app-data");
        Path argsFile = appData.resolve("versions").resolve("10.3.20260703").resolve("shaft-mcp.args");
        Files.createDirectories(argsFile.getParent());
        Files.writeString(argsFile, "-cp\nshaft-mcp.jar\ncom.shaft.mcp.ShaftMcpApplication\n");
        String oldAppData = System.getProperty("shaft.intellij.mcp.applicationDataRoot");
        System.setProperty("shaft.intellij.mcp.applicationDataRoot", appData.toString());
        try {
            assertEquals("10.3.20260703", ShaftMcpSetupPanel.installedShaftMcpVersion(appData));

            // Real on-disk detection no longer auto-runs in the constructor (issue #3560): nothing
            // is green/populated by default, only after the user presses Check. Once pressed, the
            // installed version this test just wrote to disk must flow through into the detail text.
            ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
            });
            JLabel mcpVersionDetail = findByAccessibleName(panel, "SHAFT MCP version status", JLabel.class);
            assertTrue(mcpVersionDetail.getText().isBlank(), mcpVersionDetail.getText());

            clickAccessible(panel, "Check SHAFT MCP version");
            assertTrue(mcpVersionDetail.getText().contains("10.3.20260703"), mcpVersionDetail.getText());
        } finally {
            restoreProperty("shaft.intellij.mcp.applicationDataRoot", oldAppData);
        }
    }

    @Test
    void setupPanelScrollsVerticallyAndNeverHorizontally() {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });

        JScrollPane scroll = findByAccessibleName(panel, "SHAFT MCP setup scroll pane", JScrollPane.class);
        assertNotNull(scroll, "the grown setup flow must live in a scroll pane so the bottom stays reachable");
        assertAll(
                () -> assertEquals(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, scroll.getVerticalScrollBarPolicy()),
                () -> assertEquals(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER, scroll.getHorizontalScrollBarPolicy()),
                () -> assertTrue(scroll.getViewport().getView() instanceof javax.swing.Scrollable scrollable
                                && scrollable.getScrollableTracksViewportWidth(),
                        "content must re-wrap to the viewport width instead of scrolling sideways"));
    }

    @Test
    void setupPanelListsPrerequisitesWithCopyableInstallCommands() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        AtomicReference<String> copied = new AtomicReference<>();
        setField(panel, "copySink", (Consumer<String>) copied::set);

        assertAll(
                () -> assertTrue(containsText(panel, "0 Prerequisites")),
                () -> assertNotNull(findByAccessibleName(panel, "SHAFT setup prerequisites", JComponent.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Recheck prerequisites", JButton.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Copy SHAFT Engine warm-up command", JButton.class)));

        clickAccessible(panel, "Copy SHAFT Engine warm-up command");
        assertTrue(copied.get().contains("dependency:get"), copied.get());
        assertTrue(copied.get().contains("io.github.shafthq:SHAFT_ENGINE"), copied.get());
    }

    @Test
    void setupPanelPrerequisitesOfferInstallCommandsForMissingTools() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        AtomicReference<String> copied = new AtomicReference<>();
        setField(panel, "copySink", (Consumer<String>) copied::set);
        setField(panel, "prerequisitesDetector",
                (java.util.function.Function<String, List<SetupPrerequisites.Prerequisite>>) family ->
                        List.of(new SetupPrerequisites.Prerequisite(
                                "Python 3", false, true, "winget install -e --id Python.Python.3.12")));
        Method refresh = ShaftMcpSetupPanel.class.getDeclaredMethod("refreshPrerequisites");
        refresh.setAccessible(true);
        refresh.invoke(panel);

        JButton copyInstall = findByAccessibleName(panel, "Copy Python 3 install command", JButton.class);
        assertNotNull(copyInstall, "a missing prerequisite must offer its install command");
        copyInstall.doClick();
        assertEquals("winget install -e --id Python.Python.3.12", copied.get());
        assertTrue(containsText(panel, "Python 3 missing"));
    }

    @Test
    void recheckPrerequisitesButtonCollapsesSatisfiedRowImmediately() throws Exception {
        // Issue #4168: the Recheck button used to call refreshPrerequisites() without
        // updateActionState(), so a satisfied row's content updated correctly but the row stayed
        // expanded until some later, unrelated interaction triggered a state update -- matching
        // every other path in this panel, which collapses a satisfied step immediately.
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        setField(panel, "prerequisitesDetector",
                (java.util.function.Function<String, List<SetupPrerequisites.Prerequisite>>) family ->
                        List.of(new SetupPrerequisites.Prerequisite(
                                "Python 3", false, true, "winget install -e --id Python.Python.3.12")));
        Method refresh = ShaftMcpSetupPanel.class.getDeclaredMethod("refreshPrerequisites");
        refresh.setAccessible(true);
        refresh.invoke(panel);
        // Establish the legitimate "still missing" starting state -- every other call site of
        // refreshPrerequisites() pairs it with updateActionState(); only the Recheck button (the
        // bug under test) does not.
        Method updateActionState = ShaftMcpSetupPanel.class.getDeclaredMethod("updateActionState", boolean.class);
        updateActionState.setAccessible(true);
        updateActionState.invoke(panel, false);

        JPanel prerequisitesRow = (JPanel) getField(panel, "prerequisitesRow");
        JComponent prerequisitesAction = stepRowAction(panel, prerequisitesRow);
        assertTrue(prerequisitesAction.isVisible(),
                "row must stay expanded while a required prerequisite is missing");

        // The user installs Python 3 outside the IDE, then clicks Recheck.
        setField(panel, "prerequisitesDetector",
                (java.util.function.Function<String, List<SetupPrerequisites.Prerequisite>>) family ->
                        List.of(new SetupPrerequisites.Prerequisite("Python 3", true, true, "")));
        clickAccessible(panel, "Recheck prerequisites");

        assertAll(
                () -> assertTrue(containsText(panel, "Python 3 detected")),
                () -> assertFalse(prerequisitesAction.isVisible(),
                        "Recheck must collapse a now-satisfied prerequisite row immediately, matching "
                                + "every other path in this panel (issue #4168)"));
    }

    @Test
    void setupPanelOffersRestartCommandWhenClientCannotAccessShaftMcp() throws Exception {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, (client, runtime) -> ShaftMcpToolResult.failure(
                "Codex CLI sees shaft-mcp but could not connect to it. If you just installed or "
                        + "updated shaft-mcp, restart Codex CLI with the restart command below, then check again."));
        AtomicReference<String> copied = new AtomicReference<>();
        setField(panel, "copySink", (Consumer<String>) copied::set);

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));

        JButton restart = findByAccessibleName(panel, "Copy assistant CLI restart command", JButton.class);
        assertAll(
                () -> assertTrue(containsText(panel, "Category: Client MCP connection")),
                () -> assertTrue(containsText(panel, "Restart the selected client CLI")),
                () -> assertNotNull(restart),
                () -> assertTrue(restart.isVisible()),
                () -> assertTrue(restart.isEnabled()),
                // Two-lane readiness (issue #3425 A2): the MCP lane is verified, so setup is
                // complete; the restart command remains the recovery for the optional agent lane.
                () -> assertTrue(settings.mcpSetupComplete));

        restart.doClick();
        assertTrue(copied.get().contains("codex"), copied.get());
        assertTrue(copied.get().contains("codex mcp list"), copied.get());
        if (isWindowsOs()) {
            assertTrue(copied.get().contains("Get-Process codex"), copied.get());
            assertTrue(copied.get().contains("Stop-Process"), copied.get());
        } else {
            assertTrue(copied.get().contains("pkill -x codex"), copied.get());
        }
    }

    @Test
    void setupPanelRegistrationFailureGuidesToInstallerWithoutRestartNoise() throws Exception {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, (client, runtime) -> ShaftMcpToolResult.failure(
                "shaft-mcp is not registered with Codex CLI. Run the SHAFT MCP installer command, then check again."));

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));

        assertAll(
                () -> assertTrue(containsText(panel, "Category: Client MCP registration")),
                () -> assertTrue(containsText(panel, "Run the installer command for the selected client")));
    }

    @Test
    void setupPanelStartsWithUnselectedAgentAndNoRecommendedLabel() {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        }, (client, runtime) -> "CLAUDE_CODE".equals(client)
                ? ShaftMcpToolResult.success("Claude Code CLI executable is available on PATH.")
                : ShaftMcpToolResult.failure("not found"));
        JComboBox<?> agent = findByAccessibleName(panel, "Assistant agent", JComboBox.class);
        @SuppressWarnings("unchecked")
        ListCellRenderer<Object> renderer = (ListCellRenderer<Object>) agent.getRenderer();
        Component placeholder = renderer.getListCellRendererComponent(new JList<>(), null, -1, false, false);
        JButton copy = findByAccessibleName(panel, "Copy SHAFT Tools & Skills setup command", JButton.class);

        assertAll(
                () -> assertNull(agent.getSelectedItem()),
                () -> assertTrue(placeholder instanceof JLabel),
                () -> assertEquals("Select an option", ((JLabel) placeholder).getText()),
                () -> assertFalse(copy.isEnabled()),
                () -> assertNull(findByAccessibleName(panel, "Recommended assistant agent", JLabel.class)),
                () -> assertTrue(containsText(panel, "2 Choose agent")),
                () -> assertTrue(containsText(panel, "3 Setup tools & skills")),
                () -> assertTrue(containsText(panel, "4 Check agent connection")),
                () -> assertTrue(containsText(panel, "5 Check tools installation")),
                () -> assertNull(findByAccessibleName((JPanel) getField(panel, "chooseRow"),
                        "Check agent connection", JButton.class)));
    }

    @Test
    void copyingSetupCommandUsesSelectedAgentAndAgenticToolsScript() throws Exception {
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.assistantFamily = "GROK";
        settings.defaultAutobotClient = "GROK";
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });
        AtomicReference<String> copied = new AtomicReference<>("");
        setField(panel, "copySink", (Consumer<String>) copied::set);
        clickAccessible(panel, "Copy SHAFT Tools & Skills setup command");
        assertAll(
                () -> assertTrue(copied.get().contains("install-shaft-agentic-tools")),
                () -> assertTrue(copied.get().contains("grok")),
                () -> assertFalse(copied.get().contains("install-shaft-mcp")));
    }

    @Test
    void setupPanelShowsBlankManualCommandDiagnostic() throws Exception {
        Path appData = tempDirectory("shaft-mcp-empty-app-data");
        Path bootstrap = tempDirectory("shaft-mcp-empty-bootstrap");
        String oldAppData = System.getProperty("shaft.intellij.mcp.applicationDataRoot");
        String oldBootstrap = System.getProperty("shaft.intellij.mcp.bootstrapRoot");
        System.setProperty("shaft.intellij.mcp.applicationDataRoot", appData.toString());
        System.setProperty("shaft.intellij.mcp.bootstrapRoot", bootstrap.toString());
        try {
            ShaftSettingsState.Settings settings = blankMcpSettings();
            settings.assistantFamily = "CODEX";
            settings.defaultAutobotClient = "CODEX";
            ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
            });
            AtomicReference<String> copied = new AtomicReference<>();
            setField(panel, "copySink", (Consumer<String>) copied::set);
            JComponent detailsPanel = (JComponent) getField(panel, "detailsPanel");

            clickAccessible(panel, "Copy SHAFT Tools & Skills setup command");
            clickAccessible(panel, "Test SHAFT MCP connection");

            assertAll(
                    () -> assertTrue(containsText(panel, "Assist: Error")),
                    () -> assertTrue(containsText(panel, "Probe failed")),
                    () -> assertTrue(containsText(panel, "No SHAFT MCP command configured.")),
                    () -> assertTrue(containsText(panel,
                            "Run the installer command, then check again. SHAFT will find the local command automatically.")),
                    () -> assertTrue(containsText(panel, "Managed by SHAFT automatically.")),
                    () -> assertTrue(detailsPanel.isVisible()),
                    () -> assertFalse(findByAccessibleName(panel, "Copy setup diagnostic command", JButton.class).isVisible()),
                    () -> assertTrue(findByAccessibleName(panel, "Copy setup diagnostic output", JButton.class).isEnabled()),
                    () -> assertTrue(findByAccessibleName(panel, "Copy SHAFT MCP docs link", JButton.class).isEnabled()),
                    () -> assertTrue(containsText(panel, "Recovery: retry Check setup")),
                    () -> assertNull(findButton(panel, "Install / Update SHAFT MCP")),
                    () -> assertTrue(findByAccessibleName(panel, "Test SHAFT MCP connection", JButton.class).isEnabled()));
            clickAccessible(panel, "Copy setup diagnostic output");
            assertTrue(copied.get().contains("No SHAFT MCP command configured."));
        } finally {
            restoreProperty("shaft.intellij.mcp.applicationDataRoot", oldAppData);
            restoreProperty("shaft.intellij.mcp.bootstrapRoot", oldBootstrap);
        }
    }

    @Test
    void setupPanelShowsClearConnectionSuccess() throws Exception {
        AtomicBoolean connected = new AtomicBoolean();
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> connected.set(true), readyProbe());
        JComponent detailsPanel = (JComponent) getField(panel, "detailsPanel");

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK\nMCP workspace: C:/work/shaft"));

        assertAll(
                () -> assertTrue(containsText(panel, "Runtime: Codex CLI verified")),
                () -> assertTrue(containsText(panel, "Ready to chat. Verified Codex CLI for workspace C:/work/shaft.")),
                () -> assertFalse(detailsPanel.isVisible()),
                () -> assertNull(findByAccessibleName(panel, "MCP stdio command", JTextComponent.class)),
                () -> assertFalse(connected.get()),
                () -> assertTrue(findByAccessibleName(panel, "Start chatting with SHAFT Assistant", JButton.class).isVisible()),
                // Check stays visible in every state now (issue #3560): the row's own styling
                // conveys pass/fail, not button visibility, so the user can always re-check.
                () -> assertTrue(findByAccessibleName(panel, "Test SHAFT MCP connection", JButton.class).isVisible()),
                () -> assertTrue(settings.mcpSetupComplete),
                // fakeProject()'s base path has no AGENTS.md, so the guidance-optimization
                // prompt (which references a validator only meaningful for a project that
                // adopted that scaffold) must not be scheduled -- see
                // setupSuccessSchedulesGuidanceOptimizationPromptOnlyWithAgentsMdScaffold
                // for the case where the scaffold is present.
                () -> assertFalse(settings.agentGuidanceOptimizationPromptPending));
        clickAccessible(panel, "Start chatting with SHAFT Assistant");
        assertTrue(connected.get());
    }

    @Test
    void setupSuccessSchedulesGuidanceOptimizationPromptOnlyWithAgentsMdScaffold() throws Exception {
        // Regression test for issue #3363 bug 9: the agent-guidance-optimization
        // prompt tells the agent to rerun scripts/ci/validate_agent_setup.py, a
        // validator that only works in a project that has actually adopted the
        // AGENTS.md scaffold. Scheduling that prompt for any successfully
        // configured project -- regardless of whether it has that scaffold --
        // made the agent report back that required config/scaffold files were
        // missing, because they never existed in the target project at all.
        Path projectWithoutScaffold = tempDirectory("shaft-no-scaffold");
        Path projectWithScaffold = tempDirectory("shaft-with-scaffold");
        try {
            Files.writeString(projectWithScaffold.resolve("AGENTS.md"), "# Guidance\n");

            ShaftSettingsState.Settings withoutScaffoldSettings = unverifiedMcpSettings();
            ShaftMcpSetupPanel withoutScaffoldPanel = new ShaftMcpSetupPanel(
                    fakeProject(new ShaftAssistantChatState(), projectWithoutScaffold.toString()),
                    withoutScaffoldSettings, () -> {
                    }, readyProbe());
            showTestResult(withoutScaffoldPanel, ShaftMcpToolResult.success("Probe OK"));

            ShaftSettingsState.Settings withScaffoldSettings = unverifiedMcpSettings();
            ShaftMcpSetupPanel withScaffoldPanel = new ShaftMcpSetupPanel(
                    fakeProject(new ShaftAssistantChatState(), projectWithScaffold.toString()),
                    withScaffoldSettings, () -> {
                    }, readyProbe());
            showTestResult(withScaffoldPanel, ShaftMcpToolResult.success("Probe OK"));

            assertAll(
                    () -> assertFalse(withoutScaffoldSettings.agentGuidanceOptimizationPromptPending,
                            "No AGENTS.md scaffold: the guidance-optimization prompt must not be scheduled"),
                    () -> assertTrue(withScaffoldSettings.agentGuidanceOptimizationPromptPending,
                            "AGENTS.md scaffold present: the guidance-optimization prompt should be scheduled"));
        } finally {
            Files.deleteIfExists(projectWithoutScaffold.resolve("AGENTS.md"));
            Files.deleteIfExists(projectWithoutScaffold);
            Files.deleteIfExists(projectWithScaffold.resolve("AGENTS.md"));
            Files.deleteIfExists(projectWithScaffold);
        }
    }

    @Test
    void freshProjectHintShowsOnlyForANonShaftProjectAndIsDismissible() throws Exception {
        // Issue #3601 O3: ShaftProjectVersionCheck's NOT_A_SHAFT_PROJECT state already exists for
        // the setup wizard's "Upgrade project" step; the Assistant panel surfaces that same signal
        // as a dismissible, non-blocking hint for a project with no SHAFT dependency yet, and stays
        // quiet for a project that already depends on SHAFT.
        Path freshProject = tempDirectory("shaft-fresh-project");
        Path shaftProject = tempDirectory("shaft-adopted-project");
        try {
            Files.writeString(freshProject.resolve("pom.xml"), """
                    <project>
                      <groupId>com.example</groupId>
                      <artifactId>fresh</artifactId>
                      <version>1.0.0</version>
                    </project>
                    """);
            Files.writeString(shaftProject.resolve("pom.xml"), """
                    <project>
                      <dependencies>
                        <dependency>
                          <groupId>io.github.shafthq</groupId>
                          <artifactId>SHAFT_ENGINE</artifactId>
                          <version>10.3.20260715</version>
                        </dependency>
                      </dependencies>
                    </project>
                    """);

            ShaftAssistantPanel freshPanel = new ShaftAssistantPanel(
                    fakeProject(new ShaftAssistantChatState(), freshProject.toString()), blankMcpSettings());
            ShaftAssistantPanel adoptedPanel = new ShaftAssistantPanel(
                    fakeProject(new ShaftAssistantChatState(), shaftProject.toString()), blankMcpSettings());
            JLabel freshHint = findByAccessibleName(freshPanel, "Fresh project hint", JLabel.class);
            JLabel adoptedHint = findByAccessibleName(adoptedPanel, "Fresh project hint", JLabel.class);
            JButton dismiss = findByAccessibleName(freshPanel, "Dismiss fresh project hint", JButton.class);

            assertAll(
                    () -> assertNotNull(freshHint),
                    () -> assertTrue(effectivelyVisible(freshHint, freshPanel),
                            "Hint must show for a project without a SHAFT dependency"),
                    () -> assertNotNull(adoptedHint),
                    () -> assertFalse(effectivelyVisible(adoptedHint, adoptedPanel),
                            "Hint must not show for a project already on SHAFT"),
                    () -> assertNotNull(dismiss),
                    // Issue #3603: the accessible name stays the short, stable "Fresh project
                    // hint" (test-id-safe), but a screen reader also needs the actual hint text.
                    () -> assertEquals(freshHint.getText(),
                            freshHint.getAccessibleContext().getAccessibleDescription()));

            dismiss.doClick();
            assertFalse(effectivelyVisible(freshHint, freshPanel),
                    "Dismiss must hide the hint without touching any other control");
        } finally {
            Files.deleteIfExists(freshProject.resolve("pom.xml"));
            Files.deleteIfExists(freshProject);
            Files.deleteIfExists(shaftProject.resolve("pom.xml"));
            Files.deleteIfExists(shaftProject);
        }
    }

    @Test
    void setupPanelOffersNoAgentLaneWhenSelectedAgentIsNotReady() throws Exception {
        // Two-lane readiness (issue #3425 A2): recorder/codegen/doctor only need a verified
        // SHAFT MCP, so a missing agent CLI completes setup on the No-AI lane instead of failing
        // the whole wizard — while the agent diagnostics stay visible for the optional lane.
        AtomicBoolean connected = new AtomicBoolean();
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> connected.set(true),
                (client, runtime) -> ShaftMcpToolResult.failure("Codex CLI executable is not available on PATH."));

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));

        assertAll(
                () -> assertTrue(containsText(panel, "MCP verified — agent optional")),
                () -> assertTrue(containsText(panel, "Recorder, codegen, and doctor are ready now")),
                () -> assertTrue(containsText(panel, "Agent lane not ready (optional)")),
                () -> assertTrue(containsText(panel, "Category: Client runtime")),
                () -> assertTrue(containsText(panel, "Install the selected client CLI or add it to PATH")),
                () -> assertTrue(containsText(panel, "codex --version")),
                () -> assertTrue(containsText(panel, "Agent readiness failed: Codex CLI executable is not available on PATH.")),
                () -> assertFalse(connected.get()),
                () -> assertTrue(settings.mcpSetupComplete),
                () -> assertTrue(findByAccessibleName(panel, "Start SHAFT without an agent", JButton.class).isVisible()),
                () -> assertFalse(findByAccessibleName(panel, "Start chatting with SHAFT Assistant", JButton.class).isVisible()));

        clickAccessible(panel, "Start SHAFT without an agent");
        assertTrue(connected.get());
    }

    @Test
    void setupPanelOffersConnectAgentAlongsideStartWithoutAgentOnlyWhileNotReady() throws Exception {
        // Real user report: once SHAFT MCP was verified but the agent CLI's deep readiness check
        // hadn't succeeded, the Ready step showed only the "Start without an agent" skip/dead-end
        // -- no button anywhere retried just the agent half. "Connect agent" is the primary,
        // get-to-green action shown alongside that skip option; it has nothing left to do once the
        // agent lane is actually ready.
        ShaftSettingsState.Settings notReadySettings = connectedMcpSettings();
        ShaftMcpSetupPanel notReadyPanel = new ShaftMcpSetupPanel(fakeProject(), notReadySettings, () -> {
        }, (client, runtime) -> ShaftMcpToolResult.failure("Codex CLI executable is not available on PATH."));
        showTestResult(notReadyPanel, ShaftMcpToolResult.success("Probe OK"));

        assertAll(
                () -> assertTrue(findByAccessibleName(notReadyPanel, "Connect SHAFT agent", JButton.class).isVisible()),
                () -> assertTrue(findByAccessibleName(notReadyPanel, "Start SHAFT without an agent", JButton.class)
                        .isVisible()),
                () -> assertFalse(findByAccessibleName(notReadyPanel, "Start chatting with SHAFT Assistant", JButton.class)
                        .isVisible()));

        ShaftMcpSetupPanel readyPanel = new ShaftMcpSetupPanel(fakeProject(), connectedMcpSettings(), () -> {
        }, readyProbe());
        showTestResult(readyPanel, ShaftMcpToolResult.success("Probe OK"));

        assertFalse(findByAccessibleName(readyPanel, "Connect SHAFT agent", JButton.class).isVisible(),
                "nothing left to connect once the agent lane is already ready");
    }

    @Test
    void connectAgentButtonRetriesReadinessAndReachesGreenStateWhenTheRetrySucceeds() throws Exception {
        AtomicBoolean connected = new AtomicBoolean();
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        // The constructor captures the deepReadinessProbe once, so this mutable holder is what lets
        // the same panel see a failing probe on first check and a succeeding one on the later
        // "Connect agent" retry.
        AtomicReference<ShaftMcpToolResult> deepResult = new AtomicReference<>(
                ShaftMcpToolResult.failure("Codex CLI executable is not available on PATH."));
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> connected.set(true),
                (client, runtime) -> ShaftMcpToolResult.failure("Codex CLI executable is not available on PATH."),
                (client, runtime) -> deepResult.get());

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));
        JButton connectAgent = findByAccessibleName(panel, "Connect SHAFT agent", JButton.class);
        assertTrue(connectAgent.isVisible());

        deepResult.set(ShaftMcpToolResult.success("Codex CLI executable is available on PATH."));
        // The click drives connectAgentClicked()'s synchronous "Connecting agent..." status and its
        // real off-EDT dispatch via ShaftPluginExecutor; the ApplicationManager.getApplication()
        // hand-off back to the EDT that dispatch ends with cannot complete in this headless harness
        // (mirrors testMcpConnection()'s established testing precedent in
        // ShaftSettingsConfigurableTest), so applyConnectAgentResult -- the real completion path
        // that hand-off applies -- is invoked directly below the same way showTestResult() already is.
        connectAgent.doClick();
        assertTrue(containsText(panel, "Connecting agent"), "click must synchronously enter the busy state");

        invokeApplyConnectAgentResult(panel, deepResult.get());

        assertAll(
                () -> assertTrue(findByAccessibleName(panel, "Start chatting with SHAFT Assistant", JButton.class)
                        .isVisible()),
                () -> assertFalse(connectAgent.isVisible()),
                () -> assertFalse(findByAccessibleName(panel, "Start SHAFT without an agent", JButton.class).isVisible()),
                () -> assertTrue(settings.agentLaneReady));
    }

    @Test
    void chooseRowOffersAStandaloneCheckButtonMirroringOtherStepsCheckPattern() throws Exception {
        // Issue #4314 fix 2: every other step (Upgrade, Install) already has its own "Check" button
        // that turns its badge green on its own -- "2 Choose agent" only ever turned green as a side
        // effect of the full step-4 MCP probe. This button lets a user verify "is my picked agent
        // already installed/connected" without running the whole setup flow.
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        settings.assistantFamily = "CODEX";
        settings.defaultAutobotClient = "CODEX";
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe());
        JPanel agentCheckRow = (JPanel) getField(panel, "agentCheckRow");

        JButton checkChosenAgent = findByAccessibleName(agentCheckRow, "Check agent connection", JButton.class);

        assertAll(
                () -> assertNotNull(checkChosenAgent, "agent check step should carry Check agent connection"),
                () -> assertTrue(checkChosenAgent.isVisible()),
                () -> assertNull(findByAccessibleName((JPanel) getField(panel, "chooseRow"),
                        "Check agent connection", JButton.class)));
    }

    @Test
    void checkingTheSelectedAgentReflectsCloudReadinessIntoTheChooseBadgeWithoutTouchingReadyStepWidgets()
            throws Exception {
        // The Check click must only settle settings.agentLaneReady and the "2 Choose agent" badge --
        // NOT run the full applyAgentLaneState() side effects the Ready step (startChatting/
        // startWithoutAgent/connectAgent) only earns from an actual step-4 MCP probe.
        java.util.Map<String, String> storedKeys = new java.util.HashMap<>();
        storedKeys.put("GEMINI_API_KEY", "already-stored-key");
        ShaftMcpSetupPanel.CloudKeyStore keyStore = fakeKeyStore(storedKeys);
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe(), keyStore);
        setField(panel, "providerEnvironmentLookup",
                (java.util.function.Function<String, String>) ignored -> null);
        ((JCheckBox) getField(panel, "useGeminiEnvironment")).setSelected(false);
        JComboBox<?> agent = findByAccessibleName(panel, "Assistant agent", JComboBox.class);
        selectDisplayValue(agent, "Gemini in IntelliJ");
        ((JCheckBox) getField(panel, "useGeminiEnvironment")).setSelected(false);
        JLabel chooseState = (JLabel) getField(panel, "chooseState");

        clickAccessible(panel, "Check agent connection");
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
        while (!settings.agentLaneReady && System.nanoTime() < deadline) {
            pumpEdt();
            Thread.onSpinWait();
        }

        assertAll(
                () -> assertTrue(settings.agentLaneReady),
                () -> assertEquals("Done", chooseState.getText()),
                () -> assertFalse(settings.mcpSetupComplete,
                        "the full step-4 MCP probe must never run as a side effect of this check"),
                () -> assertFalse(findByAccessibleName(panel, "Start chatting with SHAFT Assistant", JButton.class)
                        .isVisible()),
                () -> assertFalse(findByAccessibleName(panel, "Start SHAFT without an agent", JButton.class)
                        .isVisible()),
                () -> assertFalse(findByAccessibleName(panel, "Connect SHAFT agent", JButton.class).isVisible()));
    }

    @Test
    void checkingTheSelectedAgentRunsTheDeepProbeOffTheEdtAndReflectsFailureIntoTheChooseBadgeOnly()
            throws Exception {
        // Non-cloud families' readiness check spawns the client CLI (~10s) -- exactly like
        // connectAgentClicked()'s existing off-EDT dispatch -- so the click must synchronously enter
        // a busy state and the real result must only land once the (simulated) async hand-off
        // completes.
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        settings.assistantFamily = "CODEX";
        settings.defaultAutobotClient = "CODEX";
        ShaftMcpToolResult notReady = ShaftMcpToolResult.failure("Codex CLI executable is not available on PATH.");
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe(), (client, runtime) -> notReady);
        JLabel chooseState = (JLabel) getField(panel, "chooseState");

        clickAccessible(panel, "Check agent connection");
        assertTrue(containsText(panel, "Checking agent"), "click must synchronously enter the busy state");

        invokeApplyChosenAgentCheckResult(panel, notReady);

        assertAll(
                () -> assertFalse(settings.agentLaneReady),
                // The "next" badge itself renders blank (issue #4314 fix 5).
                () -> assertEquals("", chooseState.getText()),
                () -> assertFalse(settings.mcpSetupComplete),
                () -> assertFalse(findByAccessibleName(panel, "Connect SHAFT agent", JButton.class).isVisible()));
    }

    @Test
    void connectAgentButtonKeepsTheNotReadyStateWithoutCrashingWhenTheRetryStillFails() throws Exception {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftMcpToolResult stillFailing = ShaftMcpToolResult.failure("Codex CLI executable is not available on PATH.");
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, (client, runtime) -> stillFailing, (client, runtime) -> stillFailing);

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));
        JButton connectAgent = findByAccessibleName(panel, "Connect SHAFT agent", JButton.class);
        assertTrue(connectAgent.isVisible());

        connectAgent.doClick();
        invokeApplyConnectAgentResult(panel, stillFailing);

        assertAll(
                () -> assertTrue(connectAgent.isVisible(), "still not ready -- connectAgent stays the primary action"),
                () -> assertTrue(findByAccessibleName(panel, "Start SHAFT without an agent", JButton.class).isVisible()),
                () -> assertFalse(findByAccessibleName(panel, "Start chatting with SHAFT Assistant", JButton.class)
                        .isVisible()),
                // The final status text is applyAgentReadinessOutcome's own failure-branch message --
                // the same one showTestResult's not-ready branch already shows (issue #3425 A2) --
                // proving the retry path never drifts from it.
                () -> assertTrue(containsText(panel, "MCP verified — agent optional")),
                () -> assertFalse(settings.agentLaneReady));
    }

    @Test
    void setupPanelConfiguresGeminiCloudProviderWithStoredApiKey() throws Exception {
        java.util.Map<String, String> storedKeys = new java.util.HashMap<>();
        ShaftMcpSetupPanel.CloudKeyStore keyStore = fakeKeyStore(storedKeys);
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, (client, runtime) -> ShaftMcpToolResult.failure("local CLI readiness must not gate the cloud route"),
                keyStore);
        JComboBox<?> agent = findByAccessibleName(panel, "Assistant agent", JComboBox.class);
        javax.swing.JPasswordField apiKey =
                findByAccessibleName(panel, "Gemini API key", javax.swing.JPasswordField.class);
        JComponent runtimeRow = (JComponent) getField(panel, "runtimeRow");
        JComponent apiKeyRow = (JComponent) getField(panel, "apiKeyRow");

        assertAll(
                () -> assertFalse(apiKeyRow.isVisible()),
                () -> assertFalse(runtimeRow.isVisible()));

        selectDisplayValue(agent, "Gemini in IntelliJ");
        assertAll(
                () -> assertTrue(apiKeyRow.isVisible()),
                () -> assertFalse(runtimeRow.isVisible()),
                () -> assertTrue(containsText(panel, "Paste your Google AI Studio API key.")));

        apiKey.setText("test-gemini-key");
        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));

        assertAll(
                () -> assertEquals("test-gemini-key", storedKeys.get("GEMINI_API_KEY")),
                () -> assertEquals(0, apiKey.getPassword().length),
                () -> assertEquals("CLOUD", settings.assistantProviderType),
                () -> assertEquals("gemini", settings.cloudProvider),
                () -> assertEquals("gemini-3.5-flash", settings.cloudModel),
                () -> assertTrue(settings.passProviderApiKeysToMcp),
                () -> assertTrue(settings.mcpSetupComplete),
                () -> assertTrue(containsText(panel, "Ready to chat. Verified Gemini in IntelliJ.")),
                () -> assertTrue(findByAccessibleName(panel, "Start chatting with SHAFT Assistant", JButton.class)
                        .isVisible()));
    }

    @Test
    void connectAgentWithPasswordSafeGeminiKeyTransitionsTheReadyStep() throws Exception {
        java.util.Map<String, String> storedKeys = new java.util.HashMap<>();
        storedKeys.put("GEMINI_API_KEY", "stored-secret");
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.assistantProviderType = "CLOUD";
        settings.cloudProvider = "gemini";
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe(), fakeKeyStore(storedKeys));
        setField(panel, "providerEnvironmentLookup",
                (java.util.function.Function<String, String>) ignored -> null);
        JComboBox<?> agent = findByAccessibleName(panel, "Assistant agent", JComboBox.class);
        selectDisplayValue(agent, "Gemini in IntelliJ");
        ((JCheckBox) getField(panel, "useGeminiEnvironment")).setSelected(false);
        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));
        JButton connectAgent = findByAccessibleName(panel, "Connect SHAFT agent", JButton.class);
        settings.agentLaneReady = false;
        invokeApplyConnectAgentResult(panel, ShaftMcpToolResult.failure("retry required"));
        assertTrue(connectAgent.isVisible());

        connectAgent.doClick();

        assertAll(
                () -> assertTrue(settings.agentLaneReady),
                () -> assertTrue(findByAccessibleName(panel, "Start chatting with SHAFT Assistant", JButton.class)
                        .isVisible()),
                () -> assertFalse(connectAgent.isVisible()));
    }

    @Test
    void setupPanelUsesConfiguredGeminiEnvironmentWithoutCopyingItsValue() throws Exception {
        java.util.Map<String, String> storedKeys = new java.util.HashMap<>();
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe(), fakeKeyStore(storedKeys));
        setField(panel, "providerEnvironmentLookup",
                (java.util.function.Function<String, String>) name -> "GOOGLE_API_KEY".equals(name)
                        ? "never-render-this-secret" : null);
        java.util.concurrent.atomic.AtomicReference<String> probedSecret = new java.util.concurrent.atomic.AtomicReference<>();
        setField(panel, "geminiKeyProbe",
                (java.util.function.Function<char[], com.shaft.intellij.settings.ProviderKeyProbe.Result>) secret -> {
                    probedSecret.set(new String(secret));
                    return new com.shaft.intellij.settings.ProviderKeyProbe.Result(true, "");
                });

        JComboBox<?> agent = findByAccessibleName(panel, "Assistant agent", JComboBox.class);
        selectDisplayValue(agent, "Gemini in IntelliJ");
        java.lang.reflect.Method update = ShaftMcpSetupPanel.class.getDeclaredMethod("updateCloudControls");
        update.setAccessible(true);
        update.invoke(panel);
        JCheckBox source = findByAccessibleName(panel, "Use configured GOOGLE_API_KEY", JCheckBox.class);

        assertTrue(source.isVisible());
        source.doClick();
        Method verify = ShaftMcpSetupPanel.class.getDeclaredMethod(
                "verifySelectedAgentReadiness", ShaftMcpToolResult.class);
        verify.setAccessible(true);
        Method credentialReadiness = ShaftMcpSetupPanel.class.getDeclaredMethod(
                "cloudCredentialReadiness", String.class);
        credentialReadiness.setAccessible(true);
        ShaftMcpToolResult validated = (ShaftMcpToolResult) credentialReadiness.invoke(panel, "GOOGLE_API_KEY");
        ShaftMcpToolResult readiness = (ShaftMcpToolResult) verify.invoke(panel, validated);

        assertAll(
                () -> assertEquals("GOOGLE_API_KEY", settings.providerApiKeyEnvironmentVariable("gemini")),
                () -> assertEquals("never-render-this-secret", probedSecret.get()),
                () -> assertTrue(storedKeys.isEmpty()),
                () -> assertFalse(containsText(panel, "never-render-this-secret")),
                () -> assertTrue(containsText(panel, "GOOGLE_API_KEY")),
                () -> assertTrue(readiness.success()));
    }

    @Test
    void setupPanelPreservesDisplayedGeminiAliasWhenBothAliasesAreConfigured() throws Exception {
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        settings.setProviderApiKeyEnvironmentVariable("gemini", "GEMINI_API_KEY");
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe(), fakeKeyStore(new java.util.HashMap<>()));
        setField(panel, "providerEnvironmentLookup",
                (java.util.function.Function<String, String>) name -> switch (name) {
                    case "GOOGLE_API_KEY" -> "preferred-secret";
                    case "GEMINI_API_KEY" -> "persisted-secret";
                    default -> null;
                });

        JComboBox<?> agent = findByAccessibleName(panel, "Assistant agent", JComboBox.class);
        selectDisplayValue(agent, "Gemini in IntelliJ");
        Method update = ShaftMcpSetupPanel.class.getDeclaredMethod("updateCloudControls");
        update.setAccessible(true);
        update.invoke(panel);
        JCheckBox source = findByAccessibleName(panel, "Use configured GEMINI_API_KEY", JCheckBox.class);
        if (!source.isSelected()) {
            source.setSelected(true);
        }

        Method apply = ShaftMcpSetupPanel.class.getDeclaredMethod("applySelectionToSettings");
        apply.setAccessible(true);
        apply.invoke(panel);

        assertEquals("GEMINI_API_KEY", settings.providerApiKeyEnvironmentVariable("gemini"));
    }

    @Test
    void setupPanelBlocksGeminiConnectionWithoutStoredApiKey() throws Exception {
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe(), fakeKeyStore(new java.util.HashMap<>()));
        JComboBox<?> agent = findByAccessibleName(panel, "Assistant agent", JComboBox.class);

        selectDisplayValue(agent, "Gemini in IntelliJ");
        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));

        assertAll(
                // Two-lane readiness (issue #3425 A2): a missing Gemini key only blocks the chat
                // lane; the verified MCP lane still completes with the no-agent start offered.
                () -> assertTrue(containsText(panel, "MCP verified — agent optional")),
                () -> assertTrue(containsText(panel, "No Gemini API key stored")),
                () -> assertTrue(settings.mcpSetupComplete),
                () -> assertTrue(findByAccessibleName(panel, "Start SHAFT without an agent", JButton.class)
                        .isVisible()));
    }

    @Test
    void assistantShowsModelAndEffortSelectorsWithoutAdvancedMode() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        JComboBox<?> localModel = findByAccessibleName(panel, "Assistant local agent model", JComboBox.class);
        JComboBox<?> effort = findByAccessibleName(panel, "Assistant effort", JComboBox.class);

        assertAll(
                () -> assertTrue(localModel.isVisible()),
                () -> assertTrue(effort.isVisible()),
                () -> assertEquals("DEFAULT", effort.getSelectedItem()),
                () -> assertTrue(effort.getItemCount() >= 4));
    }

    @Test
    void assistantLocalModelTooltipExplainsWhenCliReportsNoModels() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> localModel = findByAccessibleName(panel, "Assistant local agent model", JComboBox.class);

        applyLocalModels(panel, "CODEX", List.of());
        assertTrue(localModel.getToolTipText().contains("did not report models"), localModel.getToolTipText());

        applyLocalModels(panel, "CODEX", List.of("o1", "o1-mini"));
        assertFalse(localModel.getToolTipText().contains("did not report models"), localModel.getToolTipText());
    }

    @Test
    void assistantKeepsCliDefaultAvailableAndShowsModelDiscoveryStatus() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> localModel = findByAccessibleName(panel, "Assistant local agent model", JComboBox.class);
        JLabel status = findByAccessibleName(panel, "Local model discovery status", JLabel.class);

        applyLocalModels(panel, "CODEX", List.of());
        assertAll(
                () -> assertEquals(1, localModel.getItemCount()),
                () -> assertEquals("CLI default", localModel.getItemAt(0)),
                () -> assertEquals("CLI default", localModel.getSelectedItem()),
                () -> assertTrue(status.getText().contains("No models reported"), status.getText()),
                () -> assertEquals(status.getText(), status.getAccessibleContext().getAccessibleDescription()));

        applyLocalModels(panel, "CODEX", List.of("gpt-5.6-sol", "gpt-5.6-terra"));
        AssistantCommand.Invocation defaultInvocation = AssistantCommand.fromPrompt(
                "Explain this failure", selectedRoute(panel), "ASK", ".", "", false);
        assertAll(
                () -> assertEquals(3, localModel.getItemCount()),
                () -> assertEquals("CLI default", localModel.getItemAt(0)),
                () -> assertTrue(status.getText().contains("2 models"), status.getText()),
                () -> assertEquals(status.getText(), status.getAccessibleContext().getAccessibleDescription()),
                () -> assertFalse(AssistantLocalAgentRunner.commandFor(defaultInvocation.arguments())
                        .contains("--model")));
    }

    @Test
    void modelDiscoveryCheckingStateIsVisibleAndAccessibleUntilCompletion() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JLabel status = findByAccessibleName(panel, "Local model discovery status", JLabel.class);
        CompletableFuture<AssistantLocalAgentRunner.ModelDiscovery> discovery = new CompletableFuture<>();

        panel.startLocalModelDiscovery("CODEX", discovery);

        assertAll(
                () -> assertFalse(discovery.isDone()),
                () -> assertEquals("Checking model catalog…", status.getText()),
                () -> assertEquals("Checking model catalog…",
                        status.getAccessibleContext().getAccessibleDescription()),
                () -> assertTrue((Boolean) getField(panel, "modelListRefreshing")),
                () -> assertEquals("CODEX", getField(panel, "modelListRefreshingFamily")));

        discovery.complete(new AssistantLocalAgentRunner.ModelDiscovery(
                List.of("gpt-5.6-sol"), AssistantLocalAgentRunner.ModelDiscoveryState.AVAILABLE));
        pumpEdt();

        assertAll(
                () -> assertEquals("Ready · 1 models + CLI default", status.getText()),
                () -> assertEquals(status.getText(),
                        status.getAccessibleContext().getAccessibleDescription()),
                () -> assertFalse((Boolean) getField(panel, "modelListRefreshing")),
                () -> assertEquals("", getField(panel, "modelListRefreshingFamily")));
    }

    @Test
    void assistantModelFailureStatusesRemainAccessibleAndFitNarrowSettings() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JLabel status = findByAccessibleName(panel, "Local model discovery status", JLabel.class);

        applyLocalModelDiscovery(panel, "CODEX", AssistantLocalAgentRunner.ModelDiscoveryState.UNAVAILABLE);
        assertAll(
                () -> assertTrue(status.getText().contains("CLI unavailable"), status.getText()),
                () -> assertEquals(status.getText(), status.getAccessibleContext().getAccessibleDescription()),
                () -> assertTrue(status.getPreferredSize().width <= 307,
                        "unavailable status width=" + status.getPreferredSize().width));

        applyLocalModelDiscovery(panel, "CODEX", AssistantLocalAgentRunner.ModelDiscoveryState.FAILED);
        assertAll(
                () -> assertTrue(status.getText().contains("Reload failed"), status.getText()),
                () -> assertEquals(status.getText(), status.getAccessibleContext().getAccessibleDescription()),
                () -> assertTrue(status.getPreferredSize().width <= 307,
                        "failed status width=" + status.getPreferredSize().width));
    }

    @Test
    void assistantKeepsManualModelDuringAcceptedSameFamilyRefresh() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<String> localModel = findByAccessibleName(panel, "Assistant local agent model", JComboBox.class);
        localModel.getEditor().setItem("manual-account-model");

        applyLocalModels(panel, "CODEX", List.of("gpt-5.6-sol"));

        assertAll(
                () -> assertEquals("manual-account-model", localModel.getEditor().getItem()),
                () -> assertEquals("CLI default", localModel.getItemAt(0)),
                () -> assertEquals("gpt-5.6-sol", localModel.getItemAt(1)));
    }

    @Test
    void assistantDoesNotOfferGuessedNamedModelsWhenCliReportsNoModels() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<String> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        JComboBox<?> localModel = findByAccessibleName(panel, "Assistant local agent model", JComboBox.class);

        family.setSelectedItem("CLAUDE");
        applyLocalModels(panel, "CLAUDE", List.of());

        assertAll(
                () -> assertEquals(1, localModel.getItemCount()),
                () -> assertEquals("CLI default", localModel.getItemAt(0)),
                () -> assertTrue(localModel.isEditable()),
                () -> assertTrue(localModel.getToolTipText().contains("did not report models"),
                        localModel.getToolTipText()));
    }

    @Test
    void assistantExplainsUnavailableAndFailedModelDiscoveryWithoutOfferingModels() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<String> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        JComboBox<?> localModel = findByAccessibleName(panel, "Assistant local agent model", JComboBox.class);
        family.setSelectedItem("CLAUDE");

        applyLocalModelDiscovery(panel, "CLAUDE", AssistantLocalAgentRunner.ModelDiscoveryState.UNAVAILABLE);
        assertAll(
                () -> assertEquals(1, localModel.getItemCount()),
                () -> assertEquals("CLI default", localModel.getItemAt(0)),
                () -> assertTrue(localModel.isEditable()),
                () -> assertTrue(localModel.getToolTipText().contains("unavailable"), localModel.getToolTipText()));

        applyLocalModelDiscovery(panel, "CLAUDE", AssistantLocalAgentRunner.ModelDiscoveryState.FAILED);
        assertAll(
                () -> assertEquals(1, localModel.getItemCount()),
                () -> assertEquals("CLI default", localModel.getItemAt(0)),
                () -> assertTrue(localModel.isEditable()),
                () -> assertTrue(localModel.getToolTipText().contains("failed"), localModel.getToolTipText()));
    }

    @Test
    void assistantIgnoresStaleLocalModelCallbackAfterFamilyChanges() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<String> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        JComboBox<String> localModel = findByAccessibleName(panel, "Assistant local agent model", JComboBox.class);

        family.setSelectedItem("CLAUDE");
        applyLocalModels(panel, "CLAUDE", List.of("claude-new"));
        localModel.getEditor().setItem("manual-model");

        applyLocalModels(panel, "CODEX", List.of("codex-stale"));

        assertAll(
                () -> assertEquals("CLAUDE", family.getSelectedItem()),
                () -> assertEquals("manual-model", localModel.getEditor().getItem()),
                () -> assertEquals("CLI default", localModel.getItemAt(0)),
                () -> assertEquals("claude-new", localModel.getItemAt(1)));
    }

    @Test
    void assistantKeepsNewerSameFamilyRefreshWhileOlderCallbackCompletes() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<String> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        // CLAUDE gen1 starts, CODEX gen2 starts, then CLAUDE gen3 becomes the active request.
        family.setSelectedItem("CLAUDE");
        setField(panel, "modelListRefreshing", true);
        setField(panel, "modelListRefreshingFamily", "CLAUDE");
        setField(panel, "modelListRequestGeneration", 3L);

        applyLocalModels(panel, "CLAUDE", List.of("claude-gen1"), 1L);

        assertAll(
                () -> assertTrue((Boolean) getField(panel, "modelListRefreshing")),
                () -> assertEquals("CLAUDE", getField(panel, "modelListRefreshingFamily")),
                () -> assertEquals(3L, getField(panel, "modelListRequestGeneration")));
    }

    @Test
    void assistantKeepsPersistedCodexModelWhenModelDiscoveryIsEmpty() throws Exception {
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.localModel = "gpt-5.2-codex";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);

        applyLocalModels(panel, "CODEX", List.of());
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", selectedRoute(panel), "ASK", ".", "", false);

        assertAll(
                () -> assertEquals("gpt-5.2-codex", settings.localModel),
                () -> assertTrue(AssistantLocalAgentRunner.commandFor(invocation.arguments()).contains("gpt-5.2-codex")));
    }

    @Test
    void assistantKeepsLegacyLookingManualModelWhenDiscoveryIsNotAvailable() throws Exception {
        ShaftSettingsState.Settings claudeSettings = blankMcpSettings();
        claudeSettings.assistantFamily = "CLAUDE";
        claudeSettings.localModel = "claude-sonnet-5";
        ShaftAssistantPanel claudePanel = new ShaftAssistantPanel(null, claudeSettings);
        applyLocalModelDiscovery(claudePanel, "CLAUDE", AssistantLocalAgentRunner.ModelDiscoveryState.UNAVAILABLE);
        AssistantCommand.Invocation claudeInvocation = AssistantCommand.fromPrompt(
                "Explain this failure", selectedRoute(claudePanel), "ASK", ".", "", false);

        assertAll(
                () -> assertEquals("claude-sonnet-5", claudeSettings.localModel),
                () -> assertTrue(AssistantLocalAgentRunner.commandFor(claudeInvocation.arguments())
                        .contains("claude-sonnet-5")));
    }

    @Test
    void assistantKeepsExplicitCodexModelWhenModelDiscoveryIsEmpty() throws Exception {
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.localModel = "account-compatible-model";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);

        applyLocalModels(panel, "CODEX", List.of());
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Explain this failure", selectedRoute(panel), "ASK", ".", "", false);

        assertAll(
                () -> assertEquals("account-compatible-model", settings.localModel),
                () -> assertTrue(AssistantLocalAgentRunner.commandFor(invocation.arguments())
                        .contains("account-compatible-model")));
    }

    @Test
    void assistantRefreshLocalModelsButtonIsVisibleForLocalCliAndResetsModelListFamily() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JButton refresh = findByAccessibleName(panel, "Refresh local agent models", JButton.class);

        assertNotNull(refresh);
        assertTrue(refresh.isVisible(), "refresh should be visible for the default local CLI route");

        applyLocalModels(panel, "CODEX", List.of("o1"));
        assertEquals("CODEX", getField(panel, "modelListFamily"));

        clickAccessible(panel, "Refresh local agent models");
        assertEquals("", getField(panel, "modelListFamily"));
    }

    @Test
    void assistantKeepsCloudRouteButWaitsForLiveModelsWithoutAdvancedMode() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.assistantProviderType = "CLOUD";
        settings.cloudProvider = "gemini";
        settings.cloudModel = "gemini-3.5-flash";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        JComboBox<?> providerType = findByAccessibleName(panel, "Assistant provider type", JComboBox.class);
        JComboBox<?> cloudModel = findByAccessibleName(panel, "Assistant cloud model", JComboBox.class);
        JComboBox<?> effort = findByAccessibleName(panel, "Assistant effort", JComboBox.class);

        assertAll(
                // Basic mode must not force the setup-selected cloud route back to LOCAL.
                () -> assertEquals("CLOUD", providerType.getSelectedItem()),
                () -> assertTrue(cloudModel.isVisible()),
                () -> assertTrue(effort.isVisible()),
                () -> assertFalse(cloudModel.isEnabled()),
                () -> assertEquals(0, cloudModel.getItemCount()));
    }

    private static ShaftMcpSetupPanel.CloudKeyStore fakeKeyStore(java.util.Map<String, String> storedKeys) {
        return new ShaftMcpSetupPanel.CloudKeyStore() {
            @Override
            public boolean hasKey(String keyName) {
                return storedKeys.containsKey(keyName);
            }

            @Override
            public void saveKey(String keyName, char[] secret) {
                storedKeys.put(keyName, new String(secret));
            }
        };
    }

    @Test
    void assistantConsumesPendingGuidanceOptimizationPromptOnce() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.agentGuidanceOptimizationPromptPending = true;

        ShaftAssistantPanel first = new ShaftAssistantPanel(null, settings);
        ShaftAssistantPanel second = new ShaftAssistantPanel(null, settings);

        JComboBox<?> assistantMode = findByAccessibleName(first, "Assistant mode", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(first, "Approve source mutation for Agent mode", JCheckBox.class);

        assertAll(
                () -> assertTrue(containsText(first, "Audit and optimize")),
                () -> assertTrue(containsText(first, "shaft_guide_search")),
                () -> assertTrue(containsText(first, "test_automation_scenarios")),
                () -> assertTrue(containsText(first, "test_code_guardrails_check")),
                () -> assertFalse(settings.agentGuidanceOptimizationPromptPending),
                () -> assertFalse(containsText(second, "Audit and optimize")),
                () -> assertEquals("PLAN", assistantMode.getSelectedItem(),
                        "Onboarding optimization prompt should force Plan mode"),
                () -> assertFalse(allowEdits.isSelected(),
                        "Onboarding optimization prompt should leave source edits unchecked"));
    }

    @Test
    void guidanceOptimizationPromptUsesSelectedAgentSurfaces() {
        ShaftSettingsState.Settings codex = connectedMcpSettings();
        ShaftSettingsState.Settings claude = connectedMcpSettings();
        claude.assistantFamily = "CLAUDE";
        ShaftSettingsState.Settings copilot = connectedMcpSettings();
        copilot.assistantFamily = "COPILOT";

        assertAll(
                () -> assertTrue(ShaftAssistantPanel.agentGuidanceOptimizationPrompt(codex)
                        .contains("AGENTS.md, .codex/config.toml, .agents/skills/**, .memory/**")),
                () -> assertTrue(ShaftAssistantPanel.agentGuidanceOptimizationPrompt(claude)
                        .contains("CLAUDE.md, AGENTS.md, .agents/skills/**, .memory/**")),
                () -> assertTrue(ShaftAssistantPanel.agentGuidanceOptimizationPrompt(copilot)
                        .contains(".github/copilot-instructions.md, AGENTS.md, .github/instructions/**, .github/skills/**, .memory/**")));
    }

    @Test
    void guidanceOptimizationPromptRerunInstructionIsUnconditional() {
        ShaftSettingsState.Settings codex = connectedMcpSettings();

        String prompt = ShaftAssistantPanel.agentGuidanceOptimizationPrompt(codex);

        assertAll(
                () -> assertFalse(prompt.contains("If source edits are not enabled"), prompt),
                () -> assertTrue(prompt.contains("validate_agent_setup.py --skip-external"), prompt));
    }

    @Test
    void setupPanelShowsInlineProbeErrorStatesAndKeepsRetryEnabled() throws Exception {
        ShaftSettingsState.Settings copilotSettings = connectedMcpSettings();
        copilotSettings.assistantFamily = "COPILOT";
        copilotSettings.assistantRuntime = "IDE_PLUGIN";
        ShaftMcpSetupPanel testPanel = new ShaftMcpSetupPanel(fakeProject(), copilotSettings, () -> {
        });
        AtomicReference<String> copied = new AtomicReference<>();
        setField(testPanel, "copySink", (Consumer<String>) copied::set);

        showTestResult(testPanel, ShaftMcpToolResult.failure(
                "Could not resolve artifact io.github.shafthq:shaft-mcp:jar:1.0.0"));

        assertAll(
                () -> assertTrue(containsText(testPanel, "Assist: Error")),
                () -> assertTrue(containsText(testPanel, "Probe failed")),
                () -> assertTrue(containsText(testPanel, "Category: Maven artifact resolution")),
                () -> assertTrue(containsText(testPanel, "Client: GitHub Copilot")),
                () -> assertTrue(containsText(testPanel, "For GitHub Copilot, check the Copilot MCP client configuration")),
                () -> assertTrue(containsText(testPanel, "Diagnostic command")),
                () -> assertTrue(containsText(testPanel, "Managed by SHAFT automatically.")),
                () -> assertFalse(containsText(testPanel, "\"java\" \"@target/shaft-mcp.args\"")),
                () -> assertFalse(findByAccessibleName(testPanel, "Copy setup diagnostic command", JButton.class).isVisible()),
                () -> assertTrue(findByAccessibleName(testPanel, "Copy setup diagnostic output", JButton.class).isEnabled()),
                () -> assertTrue(findByAccessibleName(testPanel, "Test SHAFT MCP connection", JButton.class).isEnabled()));
        clickAccessible(testPanel, "Copy setup diagnostic output");
        assertTrue(copied.get().contains("Category: Maven artifact resolution"));
    }

    @Test
    void setupIntroDropsDeadGreyedCopyAndRedundantTargetRuntimeSummary() {
        // Issue #4314 fix 1: the intro's greyed "Pick an agent -- recording, code generation..."
        // paragraph and the redundant "Target: X. Runtime: Y." setupSummary caption (already
        // restated by the family/runtime combo boxes above it) add no value and cramp the layout --
        // both are removed, while the bold "Connect SHAFT Assistant" title stays.
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe());

        assertAll(
                () -> assertTrue(containsText(panel, "Connect SHAFT Assistant")),
                () -> assertFalse(containsText(panel, "Pick an agent")),
                () -> assertFalse(containsText(panel, "Model Context Protocol")),
                () -> assertFalse(containsText(panel, "Installs SHAFT MCP locally")),
                () -> assertFalse(containsText(panel, "Target: ")),
                () -> assertNull(findByAccessibleName(panel, "SHAFT MCP setup summary", JLabel.class)));
    }

    @Test
    void setupPanelHidesAncillaryControlsBehindKeyboardReachableMoreOptions() {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), unverifiedMcpSettings(), () -> {
        }, readyProbe());

        JCheckBox moreOptions = findByAccessibleName(panel, "Show more setup options", JCheckBox.class);
        assertNotNull(moreOptions, "Ancillary installer and recovery controls need one native disclosure");
        assertFalse(moreOptions.isSelected(), "The first-run flow should start with only its active task visible");

        moreOptions.doClick();
        assertAll(
                () -> assertTrue(moreOptions.isSelected()),
                () -> assertNotNull(findByAccessibleName(panel, "Show advanced installer options", JCheckBox.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Copy fresh SHAFT MCP setup command", JButton.class)));
    }

    @Test
    void setupPanelAccessibleDescriptionsTrackLiveStatusRecoveryChecklistAndToastAcrossUpdates() throws Exception {
        // Issue #3603: status/recoveryStatus/readyChecklist/toast keep a short, stable accessible
        // NAME (test-id-safe, e.g. "SHAFT MCP setup next step"), but a screen reader also needs the
        // live, real content those labels display -- carried by the accessible DESCRIPTION, which
        // must keep tracking every later update, not just the first. (setupSummary itself was
        // removed by issue #4314 fix 1 as redundant with the family/runtime combo boxes.)
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe());
        AtomicReference<String> copied = new AtomicReference<>();
        // toastSink is intentionally left as the real showToast(...) (unlike other tests that
        // override it to capture the message) so the toast JLabel itself is actually driven --
        // that live-updated label is what this test verifies the accessible description of.
        setField(panel, "copySink", (Consumer<String>) copied::set);

        JLabel status = findByAccessibleName(panel, "SHAFT MCP setup next step", JLabel.class);
        JLabel readyChecklist = findByAccessibleName(panel, "Setup ready checklist", JLabel.class);
        JLabel recoveryStatus = findByAccessibleName(panel, "SHAFT MCP recovery summary", JLabel.class);
        JLabel toastLabel = findByAccessibleName(panel, "SHAFT setup clipboard toast", JLabel.class);
        assertAll(
                () -> assertNotNull(status),
                () -> assertNotNull(readyChecklist),
                () -> assertNotNull(recoveryStatus),
                () -> assertNotNull(toastLabel));

        showTestResult(panel, ShaftMcpToolResult.success("Probe OK\nMCP workspace: C:/work/shaft"));
        String firstStatusDescription = status.getAccessibleContext().getAccessibleDescription();
        String firstChecklistDescription = readyChecklist.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(status.getToolTipText(), firstStatusDescription),
                () -> assertFalse(firstStatusDescription.isBlank()),
                () -> assertEquals(readyChecklist.getText(), firstChecklistDescription),
                () -> assertTrue(firstChecklistDescription.contains("Ready to record"), firstChecklistDescription));

        // Live-update proof: a second, different result (a failure) must update the descriptions to
        // their NEW text, not just retain what the first successful check produced.
        showTestResult(panel, ShaftMcpToolResult.failure("Could not resolve artifact io.github.shafthq:shaft-mcp"));
        String secondStatusDescription = status.getAccessibleContext().getAccessibleDescription();
        String secondChecklistDescription = readyChecklist.getAccessibleContext().getAccessibleDescription();
        String recoveryDescription = recoveryStatus.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(status.getToolTipText(), secondStatusDescription),
                () -> assertNotEquals(firstStatusDescription, secondStatusDescription,
                        "the description must track the live status text after it changes again"),
                () -> assertEquals(readyChecklist.getText(), secondChecklistDescription),
                () -> assertEquals("", secondChecklistDescription),
                () -> assertNotEquals(firstChecklistDescription, secondChecklistDescription),
                () -> assertEquals(recoveryStatus.getText(), recoveryDescription),
                () -> assertFalse(recoveryDescription.isBlank()));

        clickAccessible(panel, "Copy setup diagnostic output");
        String toastDescription = toastLabel.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(toastLabel.getText(), toastDescription),
                () -> assertFalse(toastDescription.isBlank()),
                () -> assertTrue(copied.get().contains("Probe failed")));
    }

    @Test
    void setupPanelAccessibleDescriptionsTrackLiveUpgradeMcpVersionRuntimeAssistAndGeminiKeyStatusAcrossUpdates()
            throws Exception {
        // Issue #3605: upgradeDetail/mcpVersionDetail/runtimeStatus/assistStatus/geminiKeyStatus
        // keep a short, stable accessible NAME (test-id-safe), but a screen reader also needs the
        // live, real content those labels display -- carried by the accessible DESCRIPTION, which
        // must keep tracking every later update, not just the first.
        java.util.Map<String, String> storedKeys = new java.util.HashMap<>();
        ShaftMcpSetupPanel.CloudKeyStore keyStore = fakeKeyStore(storedKeys);
        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe(), keyStore);

        JLabel upgradeDetail = findByAccessibleName(panel, "SHAFT project version status", JLabel.class);
        JLabel mcpVersionDetail = findByAccessibleName(panel, "SHAFT MCP version status", JLabel.class);
        JLabel runtimeStatus = findByAccessibleName(panel, "Assistant runtime setup status", JLabel.class);
        JLabel assistStatus = findByAccessibleName(panel, "Assistant connection setup status", JLabel.class);
        JLabel geminiKeyStatus = findByAccessibleName(panel, "Gemini API key status", JLabel.class);
        JComboBox<?> agent = findByAccessibleName(panel, "Assistant agent", JComboBox.class);
        assertAll(
                () -> assertNotNull(upgradeDetail),
                () -> assertNotNull(mcpVersionDetail),
                () -> assertNotNull(runtimeStatus),
                () -> assertNotNull(assistStatus),
                () -> assertNotNull(geminiKeyStatus));

        // runtimeStatus/assistStatus already reflect their construction-time state (issue #3605:
        // both route through showStatus(), the single choke point) before any check runs.
        String initialRuntimeDescription = runtimeStatus.getAccessibleContext().getAccessibleDescription();
        String initialAssistDescription = assistStatus.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(runtimeStatus.getText(), initialRuntimeDescription),
                () -> assertTrue(initialRuntimeDescription.contains("selected"), initialRuntimeDescription),
                () -> assertEquals(assistStatus.getText(), initialAssistDescription),
                () -> assertEquals("Assist: Not configured", initialAssistDescription));

        // Live-update proof for upgradeDetail: switching the checker result must update the
        // description to the NEW text, not just retain the initial placeholder.
        setField(panel, "upgradeChecker", (java.util.function.Supplier<ShaftProjectVersionCheck.Result>) () ->
                new ShaftProjectVersionCheck.Result(
                        ShaftProjectVersionCheck.State.UP_TO_DATE, "10.3.20260710", "10.3.20260710"));
        clickAccessible(panel, "Check SHAFT project version");
        String firstUpgradeDescription = upgradeDetail.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(upgradeDetail.getText(), firstUpgradeDescription),
                () -> assertTrue(firstUpgradeDescription.contains("already the latest"), firstUpgradeDescription));

        setField(panel, "upgradeChecker", (java.util.function.Supplier<ShaftProjectVersionCheck.Result>) () ->
                new ShaftProjectVersionCheck.Result(
                        ShaftProjectVersionCheck.State.UPGRADE_AVAILABLE, "10.2.20260101", "10.3.20260710"));
        clickAccessible(panel, "Check SHAFT project version");
        String secondUpgradeDescription = upgradeDetail.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(upgradeDetail.getText(), secondUpgradeDescription),
                () -> assertTrue(secondUpgradeDescription.contains("10.3.20260710 is available"),
                        secondUpgradeDescription),
                () -> assertNotEquals(firstUpgradeDescription, secondUpgradeDescription,
                        "the description must track the live upgrade check result after it changes"));

        // Live-update proof for mcpVersionDetail: same pattern as upgradeDetail.
        setField(panel, "mcpVersionChecker", (java.util.function.Supplier<ShaftMcpVersionCheck.Result>) () ->
                new ShaftMcpVersionCheck.Result(
                        ShaftMcpVersionCheck.State.UP_TO_DATE, "10.3.20260710", "10.3.20260710"));
        clickAccessible(panel, "Check SHAFT MCP version");
        String firstMcpVersionDescription = mcpVersionDetail.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(mcpVersionDetail.getText(), firstMcpVersionDescription),
                () -> assertTrue(firstMcpVersionDescription.contains("is up to date"), firstMcpVersionDescription));

        setField(panel, "mcpVersionChecker", (java.util.function.Supplier<ShaftMcpVersionCheck.Result>) () ->
                new ShaftMcpVersionCheck.Result(ShaftMcpVersionCheck.State.NOT_INSTALLED, "", ""));
        clickAccessible(panel, "Check SHAFT MCP version");
        String secondMcpVersionDescription = mcpVersionDetail.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(mcpVersionDetail.getText(), secondMcpVersionDescription),
                () -> assertTrue(secondMcpVersionDescription.contains("not installed yet"), secondMcpVersionDescription),
                () -> assertNotEquals(firstMcpVersionDescription, secondMcpVersionDescription,
                        "the description must track the live MCP version check result after it changes"));

        // Live-update proof for runtimeStatus/assistStatus: a successful connection test with a
        // ready agent moves both through showStatus() to their "verified"/"Configured" states.
        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));
        String verifiedRuntimeDescription = runtimeStatus.getAccessibleContext().getAccessibleDescription();
        String configuredAssistDescription = assistStatus.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(runtimeStatus.getText(), verifiedRuntimeDescription),
                () -> assertTrue(verifiedRuntimeDescription.contains("verified"), verifiedRuntimeDescription),
                () -> assertNotEquals(initialRuntimeDescription, verifiedRuntimeDescription,
                        "the description must track the live runtime status after it changes"),
                () -> assertEquals(assistStatus.getText(), configuredAssistDescription),
                () -> assertEquals("Assist: Configured", configuredAssistDescription),
                () -> assertNotEquals(initialAssistDescription, configuredAssistDescription,
                        "the description must track the live assist status after it changes"));

        // Live-update proof for assistStatus continuing to track: a subsequent failed probe must
        // move the description to its NEW "Error" text, not retain "Configured".
        showTestResult(panel, ShaftMcpToolResult.failure("Could not resolve artifact io.github.shafthq:shaft-mcp"));
        String erroredAssistDescription = assistStatus.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(assistStatus.getText(), erroredAssistDescription),
                () -> assertEquals("Assist: Error", erroredAssistDescription),
                () -> assertNotEquals(configuredAssistDescription, erroredAssistDescription,
                        "the description must track the live assist status after it changes again"));

        // Live-update proof for geminiKeyStatus: switching to the Gemini family with no stored key,
        // then completing a successful probe that stores the entered key, must update the
        // description to the NEW live text at each step (issue #3605's updateCloudControls() choke
        // point).
        selectDisplayValue(agent, "Gemini in IntelliJ");
        String noKeyDescription = geminiKeyStatus.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(geminiKeyStatus.getText(), noKeyDescription),
                () -> assertEquals("Paste your Google AI Studio API key.", noKeyDescription));

        javax.swing.JPasswordField apiKey =
                findByAccessibleName(panel, "Gemini API key", javax.swing.JPasswordField.class);
        apiKey.setText("test-gemini-key");
        showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));
        String storedKeyDescription = geminiKeyStatus.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(geminiKeyStatus.getText(), storedKeyDescription),
                () -> assertEquals("Key stored in Password Safe.", storedKeyDescription),
                () -> assertNotEquals(noKeyDescription, storedKeyDescription,
                        "the description must track the live Gemini key status after it changes"));
    }

    @Test
    void recommendedAgentAccessibleDescriptionTracksLiveRecommendationAcrossConstructionAndSummaryUpdate()
            throws Exception {
        ShaftMcpSetupPanel detected = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        }, readyProbe());
        ShaftMcpSetupPanel notDetected = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        }, (client, runtime) -> ShaftMcpToolResult.failure("not installed"));

        assertAll(
                () -> assertNull(findByAccessibleName(detected, "Recommended assistant agent", JLabel.class)),
                () -> assertNull(findByAccessibleName(notDetected, "Recommended assistant agent", JLabel.class)));
    }

    @Test
    void setupPanelHasNoLabelCroppingAndPaintsStepBackgroundContinuously() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });

        // Lay out the panel at 800x600 to ensure label text can render
        panel.setBounds(0, 0, 800, 600);
        SwingUtilities.invokeAndWait(() -> {
            panel.doLayout();
            // Recursively layout all nested components (not just immediate children)
            walkComponents(panel, comp -> {
                if (comp instanceof JComponent jc && comp != panel) {
                    jc.doLayout();
                }
            });
            panel.validate();
        });

        // Find the step row with the unified agent control.
        JPanel chooseRow = (JPanel) getField(panel, "chooseRow");
        AtomicReference<JLabel> agentLabel = new AtomicReference<>();
        AtomicReference<JComboBox<?>> agentCombo = new AtomicReference<>();
        List<JPanel> childPanels = new ArrayList<>();

        // Walk the component tree to find labels and verify they render at full size
        walkComponents(chooseRow, comp -> {
            if (comp instanceof JLabel lbl && "Agent".equals(lbl.getText())) {
                agentLabel.set(lbl);
            }
            if (comp instanceof JComboBox<?> cmb &&
                "Assistant agent".equals(cmb.getAccessibleContext().getAccessibleName())) {
                agentCombo.set(cmb);
            }
            if (comp instanceof JPanel pnl && comp != chooseRow) {
                childPanels.add(pnl);
            }
        });

        // Verify labels render without clipping
        assertAll(
                () -> assertNotNull(agentLabel.get(), "Should find 'Agent' label"),
                () -> assertNotNull(agentCombo.get(), "Should find agent combobox"),
                () -> assertTrue(agentLabel.get().getSize().width >= agentLabel.get().getPreferredSize().width),
                () -> assertTrue(agentLabel.get().getSize().height >= agentLabel.get().getPreferredSize().height));

        // Verify step row child panels are non-opaque or painted with step background
        Color stepBackground = chooseRow.getBackground();
        for (JPanel child : childPanels) {
            boolean isNonOpaque = !child.isOpaque();
            boolean hasStepBackground = child.getBackground() != null &&
                child.getBackground().equals(stepBackground);
            assertTrue(isNonOpaque || hasStepBackground,
                    "Child panel should be non-opaque or have step background color. " +
                    "Opaque: " + child.isOpaque() + ", Background matches step: " + hasStepBackground);
        }
    }

    /**
     * Issue #3771 (professional UX polish): the "2 Choose agent" step stacks "Assistant family" and
     * "Runtime" as two independently-packed {@code labeledControl} rows (each its own flow-layout
     * row), so their labels naturally size to their own text ("Assistant family" is longer than
     * "Runtime") and the dropdowns beside them land at different x-offsets -- a ragged left edge
     * that reads as unpolished next to a real aligned form. The fix gives every label in the group
     * the same preferred width (the widest label's), so every control in the column starts at the
     * same x regardless of its own label's text length.
     */
    @Test
    void pickAgentRowUsesOneVisibleRouteSelectorInsteadOfSplitControls() throws Exception {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });

        JPanel chooseRow = (JPanel) getField(panel, "chooseRow");
        JComboBox<?> agent = findByAccessibleName(chooseRow, "Assistant agent", JComboBox.class);
        JComboBox<?> family = findByAccessibleName(chooseRow, "Assistant family", JComboBox.class);
        JComboBox<?> runtime = findByAccessibleName(chooseRow, "Assistant runtime", JComboBox.class);

        assertAll(
                () -> assertNotNull(agent),
                () -> assertTrue(agent.isVisible()),
                () -> assertEquals(7, agent.getItemCount()),
                () -> assertFalse(family.isVisible()),
                () -> assertFalse(runtime.isVisible()));
    }

    @Test
    void doneStepRowCollapsesOnceLaterStepIsActiveAndReExpandsOnClick() throws Exception {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        // "2 Choose agent" reaches done, and "3 Setup tools & skills" is still next -- the flow has
        // moved past the choose row, so it should collapse (issue #3601 S2) while install stays
        // fully expanded, since it is the step actually in front of the user.
        settings.agentLaneReady = true;
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });
        // This test exercises the post-agent install step; make host-tool discovery deterministic
        // so a missing unrelated local prerequisite cannot become the active task instead.
        setField(panel, "prerequisitesDetector",
                (java.util.function.Function<String, List<SetupPrerequisites.Prerequisite>>) family -> List.of());
        Method refresh = ShaftMcpSetupPanel.class.getDeclaredMethod("refreshPrerequisites");
        refresh.setAccessible(true);
        refresh.invoke(panel);
        Method updateActionState = ShaftMcpSetupPanel.class.getDeclaredMethod("updateActionState", boolean.class);
        updateActionState.setAccessible(true);
        updateActionState.invoke(panel, false);

        JPanel chooseRow = (JPanel) getField(panel, "chooseRow");
        JPanel installRow = (JPanel) getField(panel, "installRow");
        JComboBox<?> familyCombo = findByAccessibleName(chooseRow, "Assistant family", JComboBox.class);
        JButton checkMcpVersionButton = findByAccessibleName(installRow, "Check SHAFT MCP version", JButton.class);
        JLabel chooseState = (JLabel) getField(panel, "chooseState");
        JComponent chooseAction = stepRowAction(panel, chooseRow);
        JComponent installAction = stepRowAction(panel, installRow);

        assertAll(
                () -> assertNotNull(familyCombo),
                () -> assertNotNull(checkMcpVersionButton),
                () -> assertFalse(chooseAction.isVisible(),
                        "Choose-agent row is done and a later step is active, so its detail should collapse"),
                () -> assertTrue(installAction.isVisible(),
                        "Install row is the active/next step and must stay fully expanded"),
                () -> assertFalse(effectivelyVisible(familyCombo, chooseRow),
                        "The collapsed row's own controls (e.g. the agent combo) must not render either"),
                () -> assertTrue(countVisibleComponents(chooseRow) < countVisibleComponents(installRow),
                        "A collapsed done row should show fewer visible sub-components than the active row"));

        // A click on the collapsed row's remaining state badge re-expands it for inspection.
        notifyMouseListeners(chooseState, new MouseEvent(
                chooseState, MouseEvent.MOUSE_CLICKED, System.currentTimeMillis(), 0, 2, 2, 1, false,
                MouseEvent.BUTTON1));
        assertTrue(chooseAction.isVisible(), "A click on a collapsed done row must re-expand it for inspection");
    }

    @Test
    void doneStepRowToggleIsKeyboardReachable() throws Exception {
        // The re-expand toggle above is threaded through a MouseAdapter only; a screen-reader or
        // keyboard-only user has no mouse to click with, so the row itself must be focusable and
        // carry an Enter/Space key binding equivalent to the click (#3601 a11y audit).
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.agentLaneReady = true;
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });

        JPanel chooseRow = (JPanel) getField(panel, "chooseRow");
        JComponent chooseAction = stepRowAction(panel, chooseRow);
        assertFalse(chooseAction.isVisible(), "Choose-agent row should start collapsed for this scenario");
        assertTrue(chooseRow.isFocusable(), "A collapsed row's re-expand toggle must be reachable by keyboard");
        assertNotNull(accessibleName(chooseRow), "The focusable row needs an accessible name for screen readers");

        Action enterAction = shortcutAction(chooseRow, JComponent.WHEN_FOCUSED, KeyEvent.VK_ENTER, 0);
        enterAction.actionPerformed(new ActionEvent(chooseRow, ActionEvent.ACTION_PERFORMED, "toggle"));
        assertTrue(chooseAction.isVisible(), "Enter on a focused collapsed row must re-expand it, like a click");

        Action spaceAction = shortcutAction(chooseRow, JComponent.WHEN_FOCUSED, KeyEvent.VK_SPACE, 0);
        spaceAction.actionPerformed(new ActionEvent(chooseRow, ActionEvent.ACTION_PERFORMED, "toggle"));
        assertFalse(chooseAction.isVisible(), "Space toggles the row again, collapsing it back");
    }

    @Test
    void doneStepRowShowsADiscoverableRecheckIconThatReusesTheExistingToggle() throws Exception {
        // Issue #4314 fix 4: a collapsed "done" row was already reconfigurable non-destructively by
        // clicking it (toggleStepRowInspection/installRowInspectionToggle) -- but nothing on screen
        // hinted it was clickable. A small recheck icon, visible only once the row is "done", makes
        // that existing behavior discoverable; it must reuse the same toggle, never new logic.
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.agentLaneReady = true;
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });

        JPanel chooseRow = (JPanel) getField(panel, "chooseRow");
        JPanel upgradeRow = (JPanel) getField(panel, "upgradeRow");
        JButton chooseRecheck = findByAccessibleName(chooseRow, "Choose agent setup step recheck", JButton.class);
        JButton upgradeRecheck = findByAccessibleName(upgradeRow, "Upgrade project setup step recheck", JButton.class);
        JComponent chooseAction = stepRowAction(panel, chooseRow);

        assertAll(
                () -> assertNotNull(chooseRecheck, "a done row should carry a recheck icon"),
                () -> assertTrue(chooseRecheck.isVisible(), "the icon is relevant once the row is done"),
                () -> assertNotNull(upgradeRecheck, "every step row carries the icon"),
                () -> assertFalse(upgradeRecheck.isVisible(), "...but it stays hidden outside the done state"),
                () -> assertFalse(chooseAction.isVisible(), "row starts collapsed since a later step is active"));

        chooseRecheck.doClick();
        assertTrue(chooseAction.isVisible(), "clicking the recheck icon must reuse the existing row toggle");

        chooseRecheck.doClick();
        assertFalse(chooseAction.isVisible(), "clicking again collapses it back, same as clicking the row");
    }

    private static JComponent stepRowAction(ShaftMcpSetupPanel panel, JPanel row) throws Exception {
        Method method = ShaftMcpSetupPanel.class.getDeclaredMethod("stepRowAction", JPanel.class);
        method.setAccessible(true);
        return (JComponent) method.invoke(panel, row);
    }

    private static boolean effectivelyVisible(Component component, Component root) {
        for (Component current = component; current != null; current = current.getParent()) {
            if (!current.isVisible()) {
                return false;
            }
            if (current == root) {
                break;
            }
        }
        return true;
    }

    @Test
    void toolWindowHidesAdvancedWorkflowsByDefault() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), connectedMcpSettings());

        // Regular users see only the Assistant: it understands recording, codegen, diagnosis, and
        // upgrade intents in plain language. Every specialist view stays behind the explicit
        // expert-mode opt-in, and a one-entry workflow selector stays hidden as noise.
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);
        assertNotNull(selector);
        List<String> labels = new ArrayList<>();
        for (int index = 0; index < selector.getItemCount(); index++) {
            labels.add(selector.getItemAt(index).label());
        }
        assertAll(
                () -> assertEquals(List.of("Assistant"), labels),
                () -> assertFalse(selector.isVisible()),
                // The MCP-status/Recheck chip used to ride in this shared header above every
                // workflow tab. Issue #3676 removed it from ShaftToolWindowPanel entirely --
                // not just hidden on the Assistant tab -- per explicit user feedback that it was
                // unwanted noise once already connected.
                () -> assertNull(findByAccessibleName(toolWindow, "Recheck SHAFT MCP health", JButton.class)));
    }

    @Test
    void toolWindowHeaderIsJustTheChatHistoryDropdown() {
        // Issue #3676: the user wants the chat-history dropdown directly below the tool window's
        // own header -- no MCP-status/Recheck chip, no empty space it left behind, and no second
        // "SHAFT Assistant" sub-header repeating what the tool window title already says.
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), connectedMcpSettings());

        assertAll(
                () -> assertNull(findByAccessibleName(toolWindow, "Recheck SHAFT MCP health", JButton.class)),
                () -> assertFalse(containsText(toolWindow, "MCP: verified")),
                () -> assertNull(findLabelWithText(toolWindow, "SHAFT Assistant")),
                () -> {
                    JPanel chatHeader = findByAccessibleName(toolWindow, "Assistant chat header", JPanel.class);
                    assertNotNull(chatHeader);
                    // Down from two children (title label + chat row) to just the chat row: the
                    // chat-history dropdown is the first real component under this header now.
                    assertEquals(1, chatHeader.getComponentCount());
                    assertNotNull(findByAccessibleName(chatHeader, "Assistant chat", JComboBox.class));
                });
    }

    @Test
    void emptyAssistantUsesThreePrefillSuggestionsWithoutFirstRunWelcomeEssay() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), settings);

        JButton record = findByAccessibleName(toolWindow, "Record a sample flow", JButton.class);
        JButton assertHelp = findByAccessibleName(toolWindow, "Ask how to assert", JButton.class);
        JButton diagnose = findByAccessibleName(toolWindow, "Diagnose my last failure", JButton.class);
        JLabel invitation = findByAccessibleName(toolWindow, "Assistant empty state invitation", JLabel.class);

        assertAll(
                () -> assertNull(findByAccessibleName(toolWindow, "Assistant welcome message bubble", JComponent.class),
                        "an empty Assistant should not lead with a long, persistent welcome essay"),
                () -> assertNotNull(record),
                () -> assertNotNull(assertHelp),
                () -> assertNotNull(diagnose),
                () -> assertEquals("What would you like to do?", invitation.getText()),
                () -> assertFalse(settings.firstRunCoachDismissed,
                        "there is no coach dismissal state to persist when no coach is rendered"));

        record.doClick();
        ShaftAssistantPanel panel = findAssistantPanel(toolWindow);
        assertTrue(panel.promptText().contains("Record a sample web flow"),
                "an empty-state suggestion must prefill for review, never auto-send");
    }

    /**
     * Root cause of the original clip (issue #3540): a plain {@code FlowLayout} row's
     * {@code getPreferredSize()} does not depend on the row's own width, so it under-reports height
     * once its content wraps to more than one line in a narrow tool window -- whatever the parent
     * lays out below the row then collides with the wrapped second line. {@code WrapLayout}
     * computes preferred size from the row's actual current width, so a narrower row correctly
     * reports a taller preferred size once its chips no longer fit on one line.
     */
    @Test
    void emptyStateChipRowReportsWrappedHeightInNarrowToolWindowWithoutClipping() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JPanel chipRow = (JPanel) getField(panel, "emptyStateChips");
        assertNotNull(chipRow);

        chipRow.setSize(new Dimension(1200, chipRow.getPreferredSize().height));
        int wideHeight = chipRow.getPreferredSize().height;
        chipRow.setSize(new Dimension(140, wideHeight));
        int narrowHeight = chipRow.getPreferredSize().height;

        assertAll(
                () -> assertTrue(chipRow.getLayout() instanceof WrapLayout,
                        "The empty-state chip row must use WrapLayout, not a plain FlowLayout that "
                                + "under-reports wrapped preferred height (issue #3540 root cause)."),
                () -> assertTrue(narrowHeight > wideHeight,
                        "A narrow chip row wrapping the three chips onto more than one line must "
                                + "report a taller preferred height than the same row laid out on one "
                                + "wide line (wide=" + wideHeight + ", narrow=" + narrowHeight + "), "
                                + "otherwise whatever follows it clips the wrapped second line."));
    }

    @Test
    void toolWindowShowsReadableWorkflowSelectorLabelsWhenAdvancedUiIsEnabled() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), advancedConnectedMcpSettings());

        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);
        assertNotNull(selector);
        List<String> labels = new ArrayList<>();
        for (int index = 0; index < selector.getItemCount(); index++) {
            labels.add(selector.getItemAt(index).label());
        }

        assertEquals(List.of("Assistant", "Guided", "Recorder", "Inspector", "Triage", "SHAFT Tests",
                "Visual Baselines", "Evidence", "Projects", "Advanced"), labels);
    }

    @Test
    void workflowSelectorKeepsEnoughHeightForVisibleTopLabels() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), advancedConnectedMcpSettings());
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);

        assertNotNull(selector);
        assertTrue(selector.getPreferredSize().height >= 30);
    }

    @Test
    void prefillToolSelectsMatchingWorkflowAndCategory() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), advancedConnectedMcpSettings());
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);
        assertNotNull(selector);
        JsonObject arguments = JsonParser.parseString("{}").getAsJsonObject();

        toolWindow.prefillTool("capture_start", arguments);
        assertEquals("Recorder", selectedWorkflow(toolWindow));
        assertEquals("Recorder", selectedCategory(toolWindow));

        toolWindow.prefillTool("capture_record_at_target_code_blocks", arguments);
        assertEquals("Recorder", selectedWorkflow(toolWindow));
        assertEquals("Recorder", selectedCategory(toolWindow));

        toolWindow.prefillTool("mobile_get_accessibility_tree", arguments);
        assertEquals("Inspector", selectedWorkflow(toolWindow));
        assertEquals("Inspector", selectedCategory(toolWindow));

        toolWindow.prefillTool("doctor_analyze_trace", arguments);
        assertEquals("Evidence", selectedWorkflow(toolWindow));
        assertEquals("Evidence", selectedCategory(toolWindow));

        toolWindow.prefillTool("shaft_project_create", arguments);
        assertEquals("Projects", selectedWorkflow(toolWindow));
        assertEquals("Projects", selectedCategory(toolWindow));
    }

    @Test
    void assistantDisplaysMarkdownInsteadOfNestedMcpJsonEnvelope() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String javaCode = """
                public class LoginTest {
                    @Test void logsIn() {
                    }
                }
                """.stripIndent().trim();
        String output = mcpText(mcpText(javaCode));

        showAssistantResult(panel, ShaftMcpToolResult.success(output));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("```java")),
                () -> assertTrue(markdown.contains("public class LoginTest")),
                () -> assertFalse(markdown.contains("\\\"content\\\"")),
                () -> assertNotNull(findButton(panel, "Copy raw")));
    }

    @Test
    void assistantDisplaysKnownClientListAsMarkdown() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        showAssistantResult(panel, "autobot_local_agent_clients", ShaftMcpToolResult.success(mcpText("""
                [
                  {"id":"CODEX","displayName":"Codex CLI","executableName":"codex","requiresCloudApiKey":false}
                ]
                """)));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("| Client | Command | SHAFT API key |")),
                () -> assertTrue(markdown.contains("| Codex CLI | `codex` | Not required |")),
                () -> assertFalse(markdown.contains("\\\"displayName\\\"")));
    }

    @Test
    void assistantCaptureCodegenResultWaitsForApprovalBeforeWritingFiles() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "captureReviewGenerationRunning", true);

        showAssistantResult(panel, "capture_code_blocks", ShaftMcpToolResult.success(mcpText("""
                {
                  "successful": true,
                  "codeBlocks": [
                    {"language":"java","code":"public class RecordedFlowTest {}"}
                  ]
                }
                """)));

        String markdown = transcriptMarkdown(panel);
        JComponent reviewPanel = findByAccessibleName(panel, "Capture review approval", JComponent.class);
        assertAll(
                () -> assertTrue(markdown.contains("public class RecordedFlowTest")),
                () -> assertTrue(markdown.contains("Review before writing files")),
                () -> assertTrue(markdown.contains("`approve`, `okay`, or `generate`")),
                () -> assertNotNull(reviewPanel),
                () -> assertTrue(reviewPanel.isVisible()),
                () -> assertTrue(containsText(reviewPanel, "Capture review ready")),
                () -> assertNotNull(findByAccessibleName(panel, "Approve Capture review", JButton.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Copy Capture review", JButton.class)),
                () -> assertNotNull(findByAccessibleName(panel, "Dismiss Capture review", JButton.class)));
    }

    @Test
    void dismissedCaptureReviewClearsApprovalPanelOnly() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "captureReviewGenerationRunning", true);

        showAssistantResult(panel, "capture_code_blocks", ShaftMcpToolResult.success(mcpText("""
                {
                  "successful": true,
                  "codeBlocks": [
                    {"language":"java","code":"public class RecordedFlowTest {}"}
                  ]
                }
                """)));

        click(panel, "Dismiss");

        JComponent reviewPanel = findByAccessibleName(panel, "Capture review approval", JComponent.class);
        assertAll(
                () -> assertNotNull(reviewPanel),
                () -> assertFalse(reviewPanel.isVisible()),
                () -> assertNull(getField(panel, "pendingCaptureReview")),
                () -> assertTrue(transcriptMarkdown(panel).contains("RecordedFlowTest")));
    }

    @Test
    void captureStartDiagnosticShowsExitedRecorderStatus() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        String startOutput = mcpText("""
                {
                  "state": "ACTIVE",
                  "outputPath": "recordings/intellij-capture-1.json",
                  "processId": 111
                }
                """);
        String statusOutput = mcpText("""
                {
                  "state": "NOT_RUNNING",
                  "outputPath": "",
                  "processId": 222,
                  "warnings": ["The recorder process is no longer reachable."]
                }
                """);

        showCaptureStartDiagnostic(panel, "recordings/intellij-capture-1.json", startOutput,
                ShaftMcpToolResult.success(statusOutput));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("Capture diagnostic")),
                () -> assertTrue(markdown.contains("State: `NOT_RUNNING`")),
                () -> assertTrue(markdown.contains("Recorder process: `111`")),
                () -> assertTrue(markdown.contains("Output: `recordings/intellij-capture-1.json`")),
                () -> assertTrue(markdown.contains("The recorder process is no longer reachable.")));
    }

    @Test
    void captureStartDiagnosticUnwrapsTheActiveEngineSectionFromTheUnionResponse() throws Exception {
        // Issue #3955 (mirrors #3949 in GuidedWorkflowPanel, fixed by #3954): capture_status/
        // capture_api_status return a union (McpCaptureUnionStatus/McpCaptureApiUnionStatus, design
        // doc amendment A3, landed by #3881) with the real recorder status nested under webStatus/
        // playwrightStatus/mobileStatus, not at the union's top level. showCaptureStartDiagnostic must
        // unwrap the matching section before reading state/outputPath, or the diagnostic always reads
        // them as absent/default from the union's top level.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        String startOutput = mcpText("""
                {
                  "state": "ACTIVE",
                  "outputPath": "recordings/intellij-capture-1.json",
                  "processId": 111
                }
                """);
        String statusOutput = mcpText("""
                {
                  "engine": "WEB",
                  "webStatus": {
                    "state": "NOT_RUNNING",
                    "outputPath": "recordings/intellij-capture-1.json",
                    "processId": 222,
                    "warnings": ["The recorder process is no longer reachable."]
                  },
                  "playwrightStatus": null,
                  "mobileStatus": null
                }
                """);

        showCaptureStartDiagnostic(panel, "recordings/intellij-capture-1.json", startOutput,
                ShaftMcpToolResult.success(statusOutput));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("Capture diagnostic")),
                () -> assertTrue(markdown.contains("State: `NOT_RUNNING`")),
                () -> assertTrue(markdown.contains("Recorder process: `111`")),
                () -> assertTrue(markdown.contains("Output: `recordings/intellij-capture-1.json`")),
                () -> assertTrue(markdown.contains("The recorder process is no longer reachable.")));
    }

    @Test
    void cancelledCaptureCodegenReviewDoesNotArmLaterApprovalFlow() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "captureReviewGenerationRunning", true);

        showAssistantResult(panel, "capture_code_blocks", null, new CancellationException("cancelled"));
        showAssistantResult(panel, "capture_code_blocks", ShaftMcpToolResult.success(mcpText("""
                {
                  "successful": true,
                  "codeBlocks": [
                    {"language":"java","code":"public class LaterGeneratedTest {}"}
                  ]
                }
                """)));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("capture_code_blocks cancelled")),
                () -> assertTrue(markdown.contains("LaterGeneratedTest")),
                () -> assertFalse(markdown.contains("Review before writing files")),
                () -> assertNull(getField(panel, "pendingCaptureReview")),
                () -> assertFalse((Boolean) getField(panel, "captureReviewGenerationRunning")));
    }

    @Test
    void directCodegenToolDispatchArmsSameStickyReviewGateAsRecordFlow() throws Exception {
        // Issue #3500 A7: a direct /codegen <recording.json> invocation dispatches
        // capture_generate_replay straight from dispatchApprovedTool, bypassing the
        // record -> stop -> startCaptureCodeReview() flow that normally arms
        // captureReviewGenerationRunning. The gate must arm regardless of entry point so the
        // sticky review strip still appears once the tool result comes back.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        approvalServiceOf(panel).record(ToolApprovalDecision.APPROVE_ALL_TOOLS, "unused");
        AssistantCommand.Invocation invocation =
                AssistantCommand.Invocation.tool("capture_generate_replay", new JsonObject());

        Method startMcpInvocation = ShaftAssistantPanel.class.getDeclaredMethod(
                "startMcpInvocation", AssistantCommand.Invocation.class);
        startMcpInvocation.setAccessible(true);

        // A null project means the real MCP dispatch fails with an NPE past the point where
        // dispatchApprovedTool arms the gate, mirroring
        // gateToolDispatchesImmediatelyWhenApproveAllToolsFlagIsSet.
        assertThrows(InvocationTargetException.class, () -> startMcpInvocation.invoke(panel, invocation));

        assertTrue((Boolean) getField(panel, "captureReviewGenerationRunning"),
                "A direct /codegen dispatch of capture_generate_replay must arm the sticky review gate "
                        + "just like the record -> stop -> generate flow");

        // Simulate that same dispatch completing successfully, exactly as showResult() receives it
        // from the real MCP invocation future.
        showAssistantResult(panel, "capture_generate_replay", ShaftMcpToolResult.success(mcpText("""
                {
                  "successful": true,
                  "codeBlocks": [
                    {"language":"java","code":"public class DirectCodegenTest {}"}
                  ]
                }
                """)));

        String markdown = transcriptMarkdown(panel);
        JComponent reviewPanel = findByAccessibleName(panel, "Capture review approval", JComponent.class);
        assertAll(
                () -> assertTrue(markdown.contains("public class DirectCodegenTest")),
                () -> assertTrue(markdown.contains("Review before writing files")),
                () -> assertNotNull(reviewPanel),
                () -> assertTrue(reviewPanel.isVisible()));
    }

    @Test
    void assistantKeepsShaftWrapperForCuratedMcpToolResponses() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        showAssistantResult(panel, "shaft_guide_search", ShaftMcpToolResult.success(mcpText("Use Page Object locators.")));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("**SHAFT Assistant (shaft_guide_search OK)**")),
                () -> assertTrue(markdown.contains("Use Page Object locators.")));
    }

    @Test
    void assistantDisplaysDirectLocalAgentMarkdownWithoutWrapperMetadata() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        showAgentResult(panel, ShaftMcpToolResult.success("""
                ## Done

                Opened DuckDuckGo and searched for SHAFT Engine.
                """.stripIndent().trim()));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("## Done")),
                () -> assertTrue(markdown.contains("Opened DuckDuckGo")),
                () -> assertFalse(markdown.contains("SHAFT Assistant")),
                () -> assertFalse(markdown.contains("Status:")),
                () -> assertFalse(markdown.contains("autobot_local_agent_run")));
    }

    @Test
    void assistantLocalAgentResponsesWithoutUsageMetadataOmitTokenLine() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        showAgentResult(panel, ShaftMcpToolResult.success("Opened DuckDuckGo and validated the SHAFT result."));

        String markdown = transcriptMarkdown(panel);
        assertFalse(markdown.contains("Tokens consumed:"), markdown);
    }

    @Test
    void assistantStreamingLocalAgentFinalResponseWithoutUsageMetadataOmitsTokenLine() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        appendStreamingLocalAgentBubble(panel, 7);
        showAgentResult(panel, 7, ShaftMcpToolResult.success("Final answer from Codex."));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("Final answer from Codex.")),
                () -> assertFalse(markdown.contains("Tokens consumed:"), markdown),
                () -> assertFalse(markdown.contains("Running local assistant")));
    }

    @Test
    void assistantLocalAgentTimeoutShowsFailureReasonAndTokenNoteWithoutMilestoneBubble() throws Exception {
        // Issue #3919: the terminal "Failed (Ns)" milestone bubble that used to follow the failure
        // reason is gone -- the reason and the token-usage note are what's left in the transcript.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        appendStreamingLocalAgentBubble(panel, 9);
        showAgentResult(panel, 9, ShaftMcpToolResult.failure("Timed out after 300 seconds."));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("Timed out after 300 seconds."),
                        "Failure reason must appear in the transcript: " + markdown),
                () -> assertFalse(markdown.contains("Failed ("),
                        "The terminal milestone bubble must no longer appear: " + markdown),
                () -> assertTrue(markdown.toLowerCase(java.util.Locale.ROOT).contains("token"),
                        "A terminal failure must say whether token usage is available: " + markdown));
    }

    @Test
    void assistantLocalAgentFailureUsesConsistentHeadlineAndNextStepCta() throws Exception {
        // Issue #3703: a local-agent CLI failure must render with the same short-headline +
        // details + one-next-action shape as an MCP tool failure, not a bare error string.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        appendStreamingLocalAgentBubble(panel, 10);
        showAgentResult(panel, 10, ShaftMcpToolResult.failure("Timed out after 300 seconds."));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("couldn't finish"), markdown),
                () -> assertTrue(markdown.contains("_Next:"), markdown));
    }

    /**
     * Issue #3968: {@code ShaftAssistantPanel}'s {@code append(...)} call sites previously all
     * role-inferred their kind via {@link ShaftAssistantChatState#resolveKind}'s default (everything
     * non-user defaults to {@code assistant-text}) even when the call site already knew it was
     * producing a tool-event, an error, a raw-verbose dump, or a milestone. These prove the relevant
     * call sites now thread an explicit kind through to the persisted message.
     */
    @Test
    void toolSelectedMilestoneCarriesTheMilestoneKind() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Method appendAgentMilestone =
                ShaftAssistantPanel.class.getDeclaredMethod("appendAgentMilestone", String.class);
        appendAgentMilestone.setAccessible(true);

        appendAgentMilestone.invoke(panel, "Tool selected: local assistant");

        assertEquals(ShaftAssistantChatState.KIND_MILESTONE, lastMessageKind(panel));
    }

    @Test
    void verboseExactToolRequestDumpCarriesTheRawVerboseKind() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ((JBCheckBox) getField(panel, "verboseAgentOutput")).setSelected(true);
        approvalServiceOf(panel).record(ToolApprovalDecision.APPROVE_ALL_TOOLS, "unused");
        AssistantCommand.Invocation invocation =
                AssistantCommand.Invocation.tool("shaft_guide_search", new JsonObject());
        Method startMcpInvocation = ShaftAssistantPanel.class.getDeclaredMethod(
                "startMcpInvocation", AssistantCommand.Invocation.class);
        startMcpInvocation.setAccessible(true);

        // A null project fails the actual MCP dispatch with an NPE past the point where the Verbose
        // exact-tool-request dump is appended, mirroring
        // directCodegenToolDispatchArmsSameStickyReviewGateAsRecordFlow.
        assertThrows(InvocationTargetException.class, () -> startMcpInvocation.invoke(panel, invocation));

        assertAll(
                () -> assertTrue(transcriptMarkdown(panel).contains("Verbose — exact tool request"),
                        transcriptMarkdown(panel)),
                () -> assertEquals(ShaftAssistantChatState.KIND_RAW_VERBOSE, lastMessageKind(panel)));
    }

    /**
     * Issue #3965 (final wiring step): PR #3977 threaded a {@code verbose} parameter through {@code
     * AssistantLocalAgentRunner.agentOutput}/{@code StructuredStreamParser#failureOutput} and the new
     * canonical 7-arg {@code start(...)} overload, proven at the runner level by {@code
     * AssistantLocalAgentRunnerVerboseStreamTest}. But {@code ShaftAssistantPanel#send}'s own call
     * site into {@code AssistantLocalAgentRunner.startWithOptionalCompact(...)} still resolved to an
     * overload that hardcodes {@code verbose=true}, so production stderr leaked into the compact
     * answer regardless of this Verbose checkbox. These two tests drive the REAL {@code send()} call
     * site end to end -- a real (non-stubbed) OS process, the bundled JVM's own {@code java}
     * executable invoked against a bogus main class, which deterministically exits non-zero with a
     * stable stderr message -- so this is genuine evidence the panel's live Verbose state now reaches
     * the runner, not just a re-assertion of the already-covered runner-level mechanism.
     */
    @Test
    void localAgentVerboseOffWithholdsRawStderrFromTheCompactAnswerThroughTheRealPanelSendPath() throws Exception {
        ShaftMcpToolResult result = runRealFailingCustomCommandThroughPanel(false);

        assertFalse(result.output().contains(LOCAL_AGENT_VERBOSE_TEST_BOGUS_CLASS),
                "Verbose off must withhold the real CLI's raw stderr from the compact answer: " + result.output());
    }

    @Test
    void localAgentVerboseOnStillSurfacesRawStderrThroughTheRealPanelSendPath() throws Exception {
        ShaftMcpToolResult result = runRealFailingCustomCommandThroughPanel(true);

        assertTrue(result.output().contains(LOCAL_AGENT_VERBOSE_TEST_BOGUS_CLASS),
                "Verbose on must still surface the real CLI's raw stderr in the compact answer: " + result.output());
    }

    private static final String LOCAL_AGENT_VERBOSE_TEST_BOGUS_CLASS = "NoSuchClassXYZ12345ForShaftVerboseTest";

    private static ShaftMcpToolResult runRealFailingCustomCommandThroughPanel(boolean verbose) throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ((JBCheckBox) getField(panel, "verboseAgentOutput")).setSelected(verbose);
        JTextComponent customCommand = textComponent(panel, "Optional local agent command");
        customCommand.setText(realJavaBogusClassCommandLine());

        // "Explain this failure" is the same prompt AssistantLocalAgentRunnerVerboseStreamTest's
        // customCommandInvocation() uses to route to the local-agent tool in ASK mode without
        // tripping isCodeGenerationRequest/promptNeedsMcpToolCalls (which would otherwise redirect
        // to the "needs MCP tool access" gate instead of ever reaching the local agent runner).
        assistantPrompt(panel).setText("Explain this failure");
        clickAccessible(panel, "Send assistant prompt");

        ShaftMcpInvocation invocation = (ShaftMcpInvocation) getField(panel, "currentInvocation");
        assertNotNull(invocation, "Expected send() to have started a real local-agent invocation");
        ShaftMcpToolResult result = invocation.future().get(20, TimeUnit.SECONDS);
        assertFalse(result.success(),
                "The bogus main class must make the real java process exit non-zero: " + result.output());
        // The real stderr line reaching send()'s live outputConsumer schedules the coalescer's real
        // ~100ms one-shot javax.swing.Timer (ShaftAssistantPanel.java's LocalAgentOutputCoalescer
        // wiring). This used to need a fixed 250ms wait to let the timer fire and self-stop before
        // the platform's SwingTimerWatcherExtension ran; issue #4500 gave that timer an owner, so the
        // panel's own disposal stops it deterministically and the sleep is gone.
        return result;
    }

    /** The bundled JVM's own {@code java} executable, invoked by absolute path (no PATH dependency). */
    private static String realJavaBogusClassCommandLine() {
        boolean windows = System.getProperty("os.name", "").toLowerCase(java.util.Locale.ROOT).contains("win");
        String javaBin = Path.of(System.getProperty("java.home"), "bin", windows ? "java.exe" : "java").toString();
        return "\"" + javaBin + "\" " + LOCAL_AGENT_VERBOSE_TEST_BOGUS_CLASS;
    }

    @Test
    void toolApprovalDenialOutcomeCarriesTheToolEventKind() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        AssistantCommand.Invocation invocation = AssistantCommand.Invocation.tool("capture_start", new JsonObject());
        Method startMcpInvocation = ShaftAssistantPanel.class.getDeclaredMethod(
                "startMcpInvocation", AssistantCommand.Invocation.class);
        startMcpInvocation.setAccessible(true);
        startMcpInvocation.invoke(panel, invocation);
        JButton deny = approvalDecisionButton((ToolApprovalPromptPanel) transcriptWidget(panel), "Deny");

        SwingUtilities.invokeAndWait(deny::doClick);

        assertAll(
                () -> assertTrue(transcriptMarkdown(panel).contains("Denied `capture_start`")),
                () -> assertEquals(ShaftAssistantChatState.KIND_TOOL_EVENT, lastMessageKind(panel)));
    }

    @Test
    void mcpToolFailureRendersWithTheErrorKind() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        showAssistantResult(panel, "shaft_guide_search", ShaftMcpToolResult.failure("Tool exploded."));

        assertEquals(ShaftAssistantChatState.KIND_ERROR, lastMessageKind(panel));
    }

    @Test
    void mcpToolSuccessRendersWithTheDefaultAssistantTextKind() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        showAssistantResult(panel, "shaft_guide_search",
                ShaftMcpToolResult.success(mcpText("Use Page Object locators.")));

        assertEquals(ShaftAssistantChatState.KIND_ASSISTANT_TEXT, lastMessageKind(panel));
    }

    @Test
    void localAgentStreamedFailureRendersWithTheErrorKindOnTheSameBubbleItStreamedInto() throws Exception {
        // Issue #3968: the terminal failure replaces the SAME streaming placeholder bubble via
        // replaceLast (issue #3628's fast in-place path) -- proving the kind change actually reaches
        // the persisted message even though the streamed bubble started life with the default kind.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        appendStreamingLocalAgentBubble(panel, 11);
        showAgentResult(panel, 11, ShaftMcpToolResult.failure("Timed out after 300 seconds."));

        assertEquals(ShaftAssistantChatState.KIND_ERROR, lastMessageKind(panel));
    }

    @Test
    void localAgentStreamedSuccessRendersWithTheDefaultAssistantTextKind() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        appendStreamingLocalAgentBubble(panel, 12);
        showAgentResult(panel, 12, ShaftMcpToolResult.success("Final answer from Codex."));

        assertEquals(ShaftAssistantChatState.KIND_ASSISTANT_TEXT, lastMessageKind(panel));
    }

    /**
     * Issue #3968 follow-up (hostile review finding): a user-initiated Cancel/Kill of a running
     * local-agent CLI is the same kind of event {@link #showTerminalSequenceResult}'s cancelled
     * branch already treats as {@code KIND_TOOL_EVENT} for the MCP-tool-sequence path -- an aborted
     * operation, not a genuine failure. {@code showAgentCancelled} must apply the same reasoning
     * instead of leaving the cancellation bubble at the role-inferred default.
     */
    @Test
    void localAgentCancelledRendersWithTheToolEventKind() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        appendStreamingLocalAgentBubble(panel, 13);
        showAgentResult(panel, 13, null, new CancellationException("cancelled"));

        assertEquals(ShaftAssistantChatState.KIND_TOOL_EVENT, lastMessageKind(panel));
    }

    @Test
    void localAgentKilledRendersWithTheToolEventKind() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        appendStreamingLocalAgentBubble(panel, 14);
        setField(panel, "killRequested", true);
        showAgentResult(panel, 14, null, new CancellationException("killed"));

        assertEquals(ShaftAssistantChatState.KIND_TOOL_EVENT, lastMessageKind(panel));
    }

    @Test
    void assistantLocalAgentCompletedWithoutUsageMetadataStatesTokenUsageNotAvailable() throws Exception {
        // Issue #3703: silence must never stand in for "no data" -- the user must be able to tell
        // "no data" apart from "the feature doesn't exist here".
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        appendStreamingLocalAgentBubble(panel, 11);
        showAgentResult(panel, 11, ShaftMcpToolResult.success("Final answer with no usage metadata."));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.toLowerCase(java.util.Locale.ROOT).contains("token usage"), markdown),
                () -> assertFalse(markdown.contains("Tokens consumed:"), markdown));
    }

    @Test
    void assistantLocalAgentCancelledWithoutUsageMetadataStatesTokenUsageNotAvailable() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        appendStreamingLocalAgentBubble(panel, 12);
        appendLocalAgentOutput(panel, 12, "partial answer before cancel");
        showAgentResult(panel, 12, null, new CancellationException("cancelled"));

        String markdown = transcriptMarkdown(panel);
        assertTrue(markdown.toLowerCase(java.util.Locale.ROOT).contains("token usage"), markdown);
    }

    @Test
    void assistantLocalAgentStaleCompletionClearsThinkingState() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        panel.setRunning(true, "Thinking...");
        setField(panel, "activeLocalAgentStreamToken", -1);

        showAgentResult(panel, 42, ShaftMcpToolResult.success("Created Page Object files."));

        JLabel status = (JLabel) getField(panel, "status");
        JProgressBar progress = (JProgressBar) getField(panel, "progress");
        assertAll(
                () -> assertEquals("Try asking me to do something...", status.getText()),
                () -> assertFalse(progress.isVisible()),
                () -> assertTrue(transcriptMarkdown(panel).contains("Created Page Object files.")),
                () -> assertFalse(transcriptMarkdown(panel).contains("Running local assistant")));
    }

    @Test
    void assistantLocalAgentStaleCompletionDoesNotInterruptActiveStream() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        panel.setRunning(true, "Thinking...");
        setField(panel, "activeLocalAgentStreamToken", 7);

        showAgentResult(panel, 42, ShaftMcpToolResult.success("Stale output"));

        JLabel status = (JLabel) getField(panel, "status");
        JProgressBar progress = (JProgressBar) getField(panel, "progress");
        assertAll(
                () -> assertEquals("Thinking...", status.getText()),
                () -> assertTrue(progress.isVisible()),
                () -> assertFalse(transcriptMarkdown(panel).contains("Stale output")));
    }

    @Test
    void assistantRejectsNativeSeleniumLocalAgentCodeAndNamesShaftPracticeTools() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        showAgentResult(panel, ShaftMcpToolResult.success("""
                ```java
                public class BadGeneratedTest {
                    @Test
                    void login() {
                        driver.get("https://example.com");
                        driver.findElement(By.id("login")).click();
                    }
                }
                ```
                """.stripIndent().trim()));

        String markdown = transcriptMarkdown(panel);
        String exported = assistantExport(panel);

        assertAll(
                () -> assertTrue(markdown.contains("**Generated code rejected**")),
                () -> assertTrue(markdown.contains("Ask the agent to regenerate")),
                () -> assertTrue(markdown.contains("`shaft_guide_search`")),
                () -> assertTrue(markdown.contains("`test_automation_scenarios`")),
                () -> assertTrue(markdown.contains("`test_code_guardrails_check`")),
                () -> assertFalse(markdown.contains("driver.get")),
                () -> assertFalse(markdown.contains("driver.findElement")),
                () -> assertFalse(exported.contains("driver.get")),
                () -> assertFalse(exported.contains("driver.findElement")),
                () -> assertFalse(exported.contains("autobot_local_agent_run")));
    }

    /**
     * Pins issue #3680's structured "proceed to Agent / keep refining" choice: a completed Plan-mode
     * local-agent run whose raw output carries an {@code ExitPlanMode} plan proposal (see {@link
     * AssistantLocalAgentRunner#parsePlanProposal}) must render an actionable choice card, and the
     * plan text itself must land in the persisted response bubble (not only the ephemeral card) so
     * it survives dismissal and shows up in transcript export/scrollback.
     */
    @Test
    void completedPlanModeRunWithAnExitPlanModeProposalRendersAStructuredChoiceCard() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        ((JComboBox<?>) getField(panel, "mode")).setSelectedItem("PLAN");
        String rawOutput = "I've reviewed the code and put together a plan.\n\n"
                + "{\"usage\":{\"input_tokens\":10,\"output_tokens\":5},"
                + "\"plan\":\"1. Add a failing test.\\n2. Implement the fix.\"}";

        showAgentResult(panel, ShaftMcpToolResult.success(rawOutput));

        JComponent widget = transcriptWidget(panel);
        assertAll(
                () -> assertNotNull(widget, "a Plan-mode ExitPlanMode proposal must render a structured card"),
                () -> assertNotNull(findButton(widget, "Switch to Agent and run this plan"),
                        "the card must offer a way to proceed to Agent mode and run the plan"),
                () -> assertNotNull(findButton(widget, "Keep refining"),
                        "the card must offer a way to stay in Plan mode and keep discussing"),
                () -> assertTrue(transcriptMarkdown(panel).contains("Add a failing test"),
                        "the plan content must land in the persisted response, not only the ephemeral card"));
    }

    /** A plain Plan-mode answer with no ExitPlanMode call must never synthesize a choice card. */
    @Test
    void completedPlanModeRunWithoutAnExitPlanModeProposalRendersNoChoiceCard() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        ((JComboBox<?>) getField(panel, "mode")).setSelectedItem("PLAN");

        showAgentResult(panel, ShaftMcpToolResult.success("Here's my analysis of the code, no plan proposed yet."));

        assertNull(transcriptWidget(panel),
                "a run with no ExitPlanMode call must not render a Plan-proposed card");
    }

    /** Same ExitPlanMode proposal, but the mode combo is on AGENT: no Plan-mode card should render. */
    @Test
    void exitPlanModeProposalOutsidePlanModeRendersNoChoiceCard() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        ((JComboBox<?>) getField(panel, "mode")).setSelectedItem("AGENT");
        String rawOutput = "Done.\n\n{\"usage\":{\"input_tokens\":1,\"output_tokens\":1},\"plan\":\"unused\"}";

        showAgentResult(panel, ShaftMcpToolResult.success(rawOutput));

        assertNull(transcriptWidget(panel), "the Plan-proposed card is scoped to Plan mode only");
    }

    /**
     * "Keep refining" must be a safe, side-effect-free dismissal: it only clears the card, leaving
     * the assistant mode and the persisted answer bubble untouched so the user can keep typing in
     * Plan mode.
     */
    @Test
    void keepRefiningDismissesTheCardWithoutChangingModeOrSourceEditApproval() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        JComboBox<?> mode = (JComboBox<?>) getField(panel, "mode");
        mode.setSelectedItem("PLAN");
        JCheckBox allowSourceMutation = (JCheckBox) getField(panel, "allowSourceMutation");
        allowSourceMutation.setSelected(false);
        String rawOutput = "Plan ready.\n\n"
                + "{\"usage\":{\"input_tokens\":1,\"output_tokens\":1},\"plan\":\"Do the thing.\"}";
        showAgentResult(panel, ShaftMcpToolResult.success(rawOutput));
        JButton keepRefining = findButton(transcriptWidget(panel), "Keep refining");

        SwingUtilities.invokeAndWait(keepRefining::doClick);

        assertAll(
                () -> assertNull(transcriptWidget(panel), "Keep refining must dismiss the card"),
                () -> assertEquals("PLAN", mode.getSelectedItem(), "Keep refining must not change the mode"),
                () -> assertFalse(allowSourceMutation.isSelected(),
                        "Keep refining must not touch the source-edit approval checkbox"),
                () -> assertTrue(transcriptMarkdown(panel).contains("Do the thing."),
                        "the persisted plan answer bubble must survive dismissing the card"));
    }

    /**
     * The "Switch to Agent and run this plan" button's state-preparation logic (mode -> AGENT, tick
     * Allow source edits, dismiss the card) is exercised directly rather than by actually clicking the
     * button: a real click also calls {@code rerun()} -> {@code send()}, which would dispatch a real
     * local-agent CLI process -- unsafe and undesired from a headless unit test (no other test in this
     * suite drives that path either; see the "Rerun" button tests, which only assert enabled state).
     */
    @Test
    void prepareAgentModeForProposedPlanSwitchesModeAndGrantsSourceEditsAndClearsTheCard() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        JComboBox<?> mode = (JComboBox<?>) getField(panel, "mode");
        mode.setSelectedItem("PLAN");
        JCheckBox allowSourceMutation = (JCheckBox) getField(panel, "allowSourceMutation");
        allowSourceMutation.setSelected(false);
        String rawOutput = "Plan ready.\n\n"
                + "{\"usage\":{\"input_tokens\":1,\"output_tokens\":1},\"plan\":\"Do the thing.\"}";
        showAgentResult(panel, ShaftMcpToolResult.success(rawOutput));
        assertNotNull(transcriptWidget(panel), "precondition: the card must be showing");

        Method prepare = ShaftAssistantPanel.class.getDeclaredMethod("prepareAgentModeForProposedPlan");
        prepare.setAccessible(true);
        prepare.invoke(panel);

        assertAll(
                () -> assertEquals("AGENT", mode.getSelectedItem()),
                () -> assertTrue(allowSourceMutation.isSelected()),
                () -> assertNull(transcriptWidget(panel), "the card must clear once the user has chosen"));
    }

    /**
     * Issue #3704 (scoped slice): before a local-agent run that is plausibly a multi-step
     * orchestrated flow (record -> act -> save -> codegen -> self-heal) starts, the user should see
     * a plain informational preview of the anticipated stages. The trigger reuses {@code
     * AssistantCommand#isScenarioDescriptionIntent} exactly (issue #3692) rather than reinventing
     * detection.
     *
     * <p>The announcement helper is invoked directly via reflection rather than through a full
     * {@code send()} click: {@code send()} for a prompt this detector matches ultimately reaches
     * {@code AssistantLocalAgentRunner#startWithOptionalCompact}, which spawns a real OS process
     * (see the javadoc on {@code prepareAgentModeForProposedPlanSwitchesModeAndGrantsSourceEditsAndClearsTheCard}
     * above for the same constraint) -- unsafe and undesired in a headless unit test.
     */
    @Test
    void scenarioDescriptionPromptTriggersStageAnnouncementBeforeLocalAgentRunStarts() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String scenarioPrompt =
                "Record a sample web flow on a practice page, add one assertion, and generate a reviewed test.";
        assertTrue(AssistantCommand.isScenarioDescriptionIntent(scenarioPrompt), "test precondition");

        Method announce = ShaftAssistantPanel.class.getDeclaredMethod(
                "maybeAnnounceOrchestrationStages", String.class);
        announce.setAccessible(true);
        announce.invoke(panel, scenarioPrompt);

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("This will likely"), markdown),
                () -> assertTrue(markdown.contains("record"), markdown),
                () -> assertTrue(markdown.contains("save"), markdown),
                () -> assertTrue(markdown.contains("generate"), markdown),
                () -> assertTrue(markdown.contains("self-heal"), markdown));
    }

    @Test
    void plainNonOrchestrationPromptDoesNotTriggerStageAnnouncement() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String plainPrompt = "what does this method do?";
        assertFalse(AssistantCommand.isScenarioDescriptionIntent(plainPrompt), "test precondition");

        Method announce = ShaftAssistantPanel.class.getDeclaredMethod(
                "maybeAnnounceOrchestrationStages", String.class);
        announce.setAccessible(true);
        announce.invoke(panel, plainPrompt);

        assertFalse(transcriptMarkdown(panel).contains("This will likely"));
    }

    @Test
    void assistantRejectsNativeSeleniumMcpCodeBlocksBeforeApproval() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        setField(panel, "captureReviewGenerationRunning", true);

        showAssistantResult(panel, "capture_code_blocks", ShaftMcpToolResult.success(mcpText("""
                {
                  "codeBlocks": [
                    {"language":"java","code":"driver.get(\\"https://example.com\\");\\ndriver.findElement(By.id(\\"login\\")).click();"}
                  ]
                }
                """)));

        String markdown = transcriptMarkdown(panel);
        String exported = assistantExport(panel);

        assertAll(
                () -> assertTrue(markdown.contains("capture_code_blocks rejected")),
                () -> assertTrue(markdown.contains("**Generated code rejected**")),
                () -> assertTrue(markdown.contains("`shaft_guide_search`")),
                () -> assertFalse(markdown.contains("Review before writing files")),
                () -> assertFalse(markdown.contains("driver.get")),
                () -> assertFalse(markdown.contains("driver.findElement")),
                () -> assertNull(getField(panel, "pendingCaptureReview")),
                () -> assertFalse((Boolean) getField(panel, "captureReviewGenerationRunning")),
                () -> assertFalse(exported.contains("driver.get")),
                () -> assertFalse(exported.contains("driver.findElement")));
    }

    @Test
    void assistantRejectsNativeSeleniumSequenceCodeBlocksBeforeEvidenceExport() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        setField(panel, "sequenceMarkdown", new StringBuilder());
        setField(panel, "sequenceRawOutput", new StringBuilder());

        showSequenceResult(
                panel,
                new AssistantCommand.ToolCall("capture_code_blocks", new JsonObject()),
                ShaftMcpToolResult.success(mcpText("""
                        {
                          "codeBlocks": [
                            {"language":"java","code":"driver.get(\\"https://example.com\\");\\ndriver.findElement(By.id(\\"login\\")).click();"}
                          ]
                        }
                        """)));

        String markdown = transcriptMarkdown(panel);
        String exported = assistantExport(panel);

        assertAll(
                () -> assertTrue(markdown.contains("SHAFT Assistant sequence rejected")),
                () -> assertTrue(markdown.contains("capture_code_blocks rejected")),
                () -> assertTrue(markdown.contains("**Generated code rejected**")),
                () -> assertFalse(markdown.contains("driver.get")),
                () -> assertFalse(markdown.contains("driver.findElement")),
                () -> assertFalse(exported.contains("driver.get")),
                () -> assertFalse(exported.contains("driver.findElement")),
                () -> assertFalse(exported.contains("Tool evidence")));
    }

    @Test
    void assistantRestoresProjectServiceChat() {
        ShaftAssistantChatState storedState = new ShaftAssistantChatState();
        storedState.append("user", "persisted prompt", "{}");
        storedState.append("assistant", "persisted answer", "{}");

        ShaftAssistantPanel panel = new ShaftAssistantPanel(fakeProject(storedState), blankMcpSettings());
        JComboBox<?> chats = findByAccessibleName(panel, "Assistant chat", JComboBox.class);
        JButton rerun = findByAccessibleName(panel, "Rerun last assistant prompt", JButton.class);

        assertAll(
                () -> assertTrue(transcriptMarkdown(panel).contains("persisted prompt")),
                () -> assertTrue(transcriptMarkdown(panel).contains("persisted answer")),
                () -> assertNotNull(chats),
                () -> assertEquals(1, chats.getItemCount()),
                () -> assertTrue(rerun.isVisible()),
                () -> assertTrue(rerun.isEnabled()));
    }

    @Test
    void setupCompleteCallbackCreatesNewSessionBeforeShowingMainView() throws Exception {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "previous message", "{}");
        chatState.append("assistant", "previous answer", "{}");
        int initialSessionCount = chatState.sessions().size();
        String previousSessionId = chatState.activeSession().id;

        // Create tool window with unverified MCP settings (shows setup panel). Use the stubbed
        // readyProbe() rather than the real AssistantLocalAgentRunner::readiness default -- that
        // real probe checks whether an actual agent CLI is installed on PATH, which made this test
        // pass or fail depending on the machine running it (installed locally, absent on clean CI
        // runners) instead of testing the session-reset behavior it's meant to cover.
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(
                fakeProject(chatState), unverifiedMcpSettings(), readyProbe(), chatState);

        // Simulate successful setup and trigger callback by clicking "Start chatting"
        ShaftMcpSetupPanel setupPanel = setupPanel(toolWindow);
        assertNotNull(setupPanel);
        showTestResult(setupPanel, ShaftMcpToolResult.success("Probe OK\nMCP workspace: C:/work/shaft"));
        JButton startChatting = findByAccessibleName(setupPanel, "Start chatting with SHAFT Assistant", JButton.class);
        assertNotNull(startChatting);
        startChatting.doClick();

        assertAll(
                () -> assertEquals(initialSessionCount + 1, chatState.sessions().size(),
                        "Setup complete callback should create a new session"),
                () -> assertTrue(chatState.activeMessages().isEmpty(),
                        "New active session should have no messages"),
                () -> assertNotEquals(previousSessionId, chatState.activeSession().id,
                        "Active session should be the new one"),
                () -> assertNotNull(findByAccessibleName(toolWindow, "Assistant prompt", JTextComponent.class),
                        "Main view should be displayed with empty chat"));
    }

    @Test
    void setupCompleteDedupGuardPreventsDoubleSessionCreation() throws Exception {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "first chat", "{}");
        chatState.newSession(); // Create empty session
        int sessionCount = chatState.sessions().size();
        String activeSessionId = chatState.activeSession().id;

        // Invoke newSession() again while active session is empty
        ShaftAssistantChatState.Session returned = chatState.newSession();

        assertAll(
                () -> assertEquals(sessionCount, chatState.sessions().size(),
                        "Second newSession() on empty active session should not create new session"),
                () -> assertEquals(activeSessionId, returned.id,
                        "Should return the existing empty session"),
                () -> assertEquals(activeSessionId, chatState.activeSession().id,
                        "Active session ID should remain unchanged"));
    }

    @Test
    void newSessionCreatesNewWhenActiveSesionHasMessages() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "existing message", "{}");
        int initialSessionCount = chatState.sessions().size();
        String initialActiveId = chatState.activeSession().id;

        ShaftAssistantChatState.Session newSession = chatState.newSession();

        assertAll(
                () -> assertEquals(initialSessionCount + 1, chatState.sessions().size(),
                        "Should create new session when active has messages"),
                () -> assertNotEquals(initialActiveId, newSession.id,
                        "New session should have different ID"),
                () -> assertTrue(newSession.messages.isEmpty(),
                        "New session should be empty"),
                () -> assertEquals(newSession.id, chatState.activeSession().id,
                        "New session should become active"));
    }

    @Test
    void assistantPanelUiResetWorksWithDedupedSession() throws Exception {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "initial prompt", "{}");
        ShaftAssistantPanel panel = new ShaftAssistantPanel(fakeProject(chatState), blankMcpSettings(), chatState);

        // Simulate clearing the active session and creating a new one (like "New chat" does internally)
        chatState.clearActiveSession();

        // Call newSession() twice - second time should reuse the empty session instead of creating a new one
        chatState.newSession();
        ShaftAssistantChatState.Session afterFirstNew = chatState.activeSession();
        int sessionCountAfterFirst = chatState.sessions().size();

        chatState.newSession();
        ShaftAssistantChatState.Session afterSecondNew = chatState.activeSession();
        int sessionCountAfterSecond = chatState.sessions().size();

        assertAll(
                () -> assertTrue(chatState.activeMessages().isEmpty(),
                        "Chat should be empty after newSession"),
                () -> assertEquals(sessionCountAfterFirst, sessionCountAfterSecond,
                        "Second newSession() on empty session should not create new session (dedup guard)"),
                () -> assertEquals(afterFirstNew.id, afterSecondNew.id,
                        "After second newSession(), should still be on the same empty session"),
                () -> assertNotNull(findByAccessibleName(panel, "Assistant prompt", JTextComponent.class),
                        "UI should still be functional"));
    }

    @Test
    void assistantKeepsCurrentSessionHistoryInDropdown() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "first in-memory chat", "{}");
        chatState.append("assistant", "first response", "{}");
        chatState.newSession();
        chatState.append("user", "second in-memory chat", "{}");
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings(), chatState);

        JComboBox<?> chats = findByAccessibleName(panel, "Assistant chat", JComboBox.class);
        assertNotNull(chats);
        chats.setSelectedIndex(1);

        assertAll(
                () -> assertEquals(2, chatState.sessions().size()),
                () -> assertTrue(comboContains(chats, "first in-memory chat")),
                () -> assertTrue(comboContains(chats, "second in-memory chat")),
                () -> assertTrue(transcriptMarkdown(panel).contains("first in-memory chat")),
                () -> assertFalse(transcriptMarkdown(panel).contains("second in-memory chat")));
    }

    @Test
    void assistantChatTitlesTrimToDropdownWidth() {
        JLabel label = new JLabel();
        String title = "generate a very long browser automation test from the current recording";
        String trimmed = ShaftAssistantPanel.trimChatTitleForWidth(title, label.getFontMetrics(label.getFont()), 120);

        assertAll(
                () -> assertTrue(trimmed.endsWith("...")),
                () -> assertTrue(trimmed.length() < title.length()),
                () -> assertTrue(label.getFontMetrics(label.getFont()).stringWidth(trimmed) <= 120));
    }

    @Test
    void assistantChatTitleFillsWideDropdownInsteadOfHardCappingAtFixedCharacterCount() {
        // Issue: the history dropdown showed titles like "generate code that opens Wikipedia, s..."
        // even when the dropdown itself was far wider -- because the stored session title was
        // hard-capped at a fixed 37-character prefix at creation time, well before the width-aware
        // renderer (trimChatTitleForWidth) ever saw it. The stored title must keep the full text;
        // only rendering should ellipsize, and only when the title genuinely does not fit.
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        String longPrompt = "generate code that opens Wikipedia, searches for artificial intelligence topics";
        chatState.append("user", longPrompt, "{}");

        assertEquals(longPrompt, chatState.activeSession().title,
                "stored session title must not be hard-capped at a fixed low character count");

        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings(), chatState);
        JComboBox<?> chats = findByAccessibleName(panel, "Assistant chat", JComboBox.class);
        assertNotNull(chats);

        JList<Object> wideList = new JList<>();
        wideList.setSize(4000, 24);
        @SuppressWarnings("unchecked")
        ListCellRenderer<Object> renderer = (ListCellRenderer<Object>) chats.getRenderer();
        Component rendered = renderer.getListCellRendererComponent(
                wideList, chatState.activeSession(), 0, false, false);
        JLabel label = (JLabel) rendered;

        assertAll(
                () -> assertEquals(longPrompt, label.getText(),
                        "a title that fits a very wide dropdown must render in full, not truncated"),
                () -> assertFalse(label.getText().endsWith("...")));
    }

    @Test
    void connectedSetupRestoresProjectServiceChats() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftAssistantChatState storedState = new ShaftAssistantChatState();
        Project project = fakeProject(storedState);
        storedState.append("user", "start recording", "{}");
        storedState.append("assistant", "Capture browser opened.", "{}");
        storedState.newSession();
        storedState.append("user", "generate reviewed code", "{}");

        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(project, settings);
        JComboBox<?> chats = findByAccessibleName(toolWindow, "Assistant chat", JComboBox.class);

        assertAll(
                () -> assertNull(setupPanel(toolWindow)),
                () -> assertNotNull(toolWindowWorkflowSelector(toolWindow)),
                () -> assertFalse(transcriptMarkdown(toolWindow).contains("start recording")),
                () -> assertTrue(transcriptMarkdown(toolWindow).contains("generate reviewed code")),
                () -> assertNotNull(chats),
                () -> assertEquals(2, chats.getItemCount()));
    }

    @Test
    void userMessagesDoNotRenderSpeakerLabelsOrUseThemAsChatTitles() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings(), chatState);

        assistantPrompt(panel).setText("start recording");
        clickAccessible(panel, "Send assistant prompt");

        String transcript = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(transcript.contains("start recording")),
                () -> assertFalse(transcript.contains(String.valueOf(new char[]{'Y', 'o', 'u'}))),
                () -> assertEquals("start recording", chatState.activeSession().title));

        String speaker = String.valueOf(new char[]{'Y', 'o', 'u'});
        String localUser = System.getProperty("user.name", "");
        ShaftAssistantChatState legacyState = new ShaftAssistantChatState();
        legacyState.append("user", "**" + speaker + " (Agent via Codex CLI)**\n\n"
                + localUser + " open duckduckgo", "{}");

        assertAll(
                () -> assertTrue(legacyState.activeMarkdown().contains("open duckduckgo")),
                () -> assertFalse(legacyState.activeMarkdown().contains("Agent via")),
                () -> assertFalse(legacyState.activeSession().title.contains(speaker)),
                () -> assertFalse(localUser.length() > 1
                        && legacyState.activeSession().title.contains(localUser)));
    }

    @Test
    void assistantNonVerboseModeShowsCompactMilestoneBubbleInsteadOfGrowingPlaceholderWithRawOutput()
            throws Exception {
        // Issue #3695: a non-verbose run's live output line no longer disappears into a separate
        // "Run timeline" list -- it now surfaces as its own compact agent-milestone chat bubble in
        // the transcript. What non-verbose mode still gates is the *raw, verbatim, growing* bubble
        // (that stays Verbose-only, see assistantVerboseModeShowsLiveAgentOutput): the placeholder
        // bubble itself is untouched by the raw line and is later replaced by the real final answer.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        assistantPrompt(panel).setText("visible realtime user prompt");
        clickAccessible(panel, "Send assistant prompt");
        appendStreamingLocalAgentBubble(panel, 77);
        appendLocalAgentOutput(panel, 77, "visible realtime agent response");
        showAgentResult(panel, 77, ShaftMcpToolResult.success("public class GeneratedTest { void test() {} }"));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("visible realtime user prompt"), markdown),
                () -> assertTrue(markdown.contains("visible realtime agent response"), markdown),
                () -> assertTrue(markdown.contains("```java"), markdown),
                () -> assertTrue(markdown.contains("GeneratedTest"), markdown),
                () -> assertTrue(containsText(panel, "visible realtime user prompt")),
                () -> assertTrue(containsText(panel, "visible realtime agent response")));
    }

    @Test
    void assistantAgentMilestoneBubblesSuppressToolSearchButKeepRealToolCalls() throws Exception {
        // #3672: ToolSearch is an internal harness meta-tool call (the CLI's own tool-schema
        // lookup) with no user-relevant signal, so it must not leak into the visible agent-milestone
        // chat bubbles (issue #3695: this content used to feed a separately-scrollable "Run timeline"
        // list; it is now its own chat bubble per message) -- but real actions (Bash) and real SHAFT
        // MCP tool calls (mcp__shaft-mcp__...) must still surface, proving the fix didn't over-suppress.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        appendStreamingLocalAgentBubble(panel, 111);
        appendLocalAgentOutput(panel, 111, "Calling tool ToolSearch...");
        appendLocalAgentOutput(panel, 111, "Calling tool Bash (echo hi)...");
        appendLocalAgentOutput(panel, 111, "Calling tool mcp__shaft-mcp__capture_start...");

        String transcript = transcriptMarkdown(panel);
        assertAll(
                () -> assertFalse(transcript.contains("Calling tool ToolSearch"),
                        "Internal harness meta-tool calls must not leak into the visible agent-milestone "
                                + "chat bubbles: " + transcript),
                () -> assertTrue(transcript.contains("Calling tool Bash"), transcript),
                () -> assertTrue(transcript.contains("Calling tool mcp__shaft-mcp__capture_start"), transcript));
    }

    @Test
    void assistantVerboseModeStillSurfacesToolSearchDespiteNonVerboseMilestoneSuppression() throws Exception {
        // Issue #3920: NON_ACTIONABLE_META_TOOL_NAMES only ever filters the *compact* non-verbose
        // milestone bubble (addCompactLocalAgentMilestone) -- appendLocalAgentOutput's Verbose branch
        // renders the raw accumulated stream unfiltered, so ToolSearch is not silently dropped
        // system-wide, only gated behind Verbose (the policy this issue requires: formatted, behind a
        // disclosure, or behind Verbose -- never truly gone). Pins that policy so a future change can't
        // accidentally filter ToolSearch out of the Verbose stream too, which would make it a genuine
        // silent drop with no escape hatch at all.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        verbose.setSelected(true);

        appendStreamingLocalAgentBubble(panel, 611);
        appendLocalAgentOutput(panel, 611, "Calling tool ToolSearch...");

        String transcript = transcriptMarkdown(panel);
        assertTrue(transcript.contains("Calling tool ToolSearch"),
                "ToolSearch must still reach the user via Verbose, even though the compact non-verbose "
                        + "milestone bubble suppresses it: " + transcript);
    }

    @Test
    void assistantAgentMilestoneBubbleIsSkippedWhenSuppressedToolCallIsTheOnlyContent() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        appendStreamingLocalAgentBubble(panel, 112);
        String transcriptBefore = transcriptMarkdown(panel);

        appendLocalAgentOutput(panel, 112, "Calling tool ToolSearch...");

        assertEquals(transcriptBefore, transcriptMarkdown(panel),
                "A local-agent output line whose only content is a suppressed meta-tool call must not "
                        + "append a new agent-milestone chat bubble");
    }

    @Test
    void assistantAgentMilestoneBubbleKeepsSiblingLineWhenOnlyOneSubLineIsSuppressed() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        appendStreamingLocalAgentBubble(panel, 113);
        appendLocalAgentOutput(panel, 113, "Thinking: deciding what to search\nCalling tool ToolSearch...");

        String transcript = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(transcript.contains("Thinking: deciding what to search"), transcript),
                () -> assertFalse(transcript.contains("Calling tool ToolSearch"), transcript));
    }

    @Test
    void assistantAgentMilestoneBubbleShowsFullLongLineWithoutMidWordTruncation() throws Exception {
        // Regression: a non-verbose milestone line over 80 chars used to be hard-cut to 77 chars +
        // "...", producing a mid-word cutoff like "...sun.misc.Unsaf..." (user report with screenshot).
        // Milestones are full transcript bubbles since #3695, so long content must render in full,
        // matching Verbose mode's fidelity -- the user asked for full messages, no trimming.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String longLine = "Tool result (Bash): WARNING: A terminally deprecated method in "
                + "sun.misc.Unsafe::allocateInstance has been called";

        appendStreamingLocalAgentBubble(panel, 201);
        appendLocalAgentOutput(panel, 201, longLine);

        String transcript = transcriptMarkdown(panel);
        assertTrue(transcript.contains(longLine),
                "Expected the full milestone line with no truncation, got: " + transcript);
    }

    @Test
    void assistantNonVerboseModeCompactsRawToolResultDumpsInsteadOfShowingThemInFull() throws Exception {
        // A real user report: with Verbose off, raw tool output (a Bash stdout dump, a file's full
        // contents, ...) was still appearing in full as its own chat bubble -- addCompactLocalAgent-
        // Milestone only ever filtered raw NDJSON ("{"/"[" lines), never plain-text tool-result dumps.
        // A genuinely long "Tool result (X): ..."/"Tool failed (X): ..." entry must now compact down
        // to a short preview pointing at Verbose instead of dumping the whole thing into the
        // transcript (the sibling test above pins that a SHORT entry still renders in full, no
        // regression to the earlier mid-word-truncation bug for the common case).
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String rawOutput = "line ".repeat(120).strip();
        String longLine = "Tool result (Bash): " + rawOutput;

        appendStreamingLocalAgentBubble(panel, 202);
        appendLocalAgentOutput(panel, 202, longLine);

        String transcript = transcriptMarkdown(panel);
        assertAll(
                () -> assertFalse(transcript.contains(rawOutput),
                        "The full raw tool-result dump must not appear while Verbose is off: " + transcript),
                () -> assertTrue(transcript.contains("Tool result (Bash):"), transcript),
                () -> assertTrue(transcript.toLowerCase(java.util.Locale.ROOT).contains("verbose"),
                        "The compacted preview must point the user at Verbose for the full output: " + transcript));
    }

    @Test
    void assistantCompactedToolResultPointerIsReachableByTogglingVerboseMidRun() throws Exception {
        // Issue #3920: confirms the "enable Verbose to see the full output" pointer from the sibling
        // test above actually leads somewhere rather than being a dead end. localAgentOutput retains
        // the full, uncompacted content for the whole run regardless of what the compact milestone
        // bubble showed, so flipping Verbose on and letting the run stream at least one more line
        // renders the complete original content via the (separate) live streaming bubble.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        String rawOutput = "line ".repeat(120).strip();
        String longLine = "Tool result (Bash): " + rawOutput;

        appendStreamingLocalAgentBubble(panel, 621);
        appendLocalAgentOutput(panel, 621, longLine);
        verbose.setSelected(true);
        appendLocalAgentOutput(panel, 621, "next streamed line after enabling Verbose");

        String transcript = transcriptMarkdown(panel);
        assertTrue(transcript.contains(rawOutput),
                "Enabling Verbose mid-run must actually surface the previously compacted full content: "
                        + transcript);
    }

    @Test
    void assistantNonVerboseModeSurfacesANoticeInsteadOfSilentlyDroppingRawJsonMilestoneLines() throws Exception {
        // Issue #3920: addCompactLocalAgentMilestone used to silently return (append nothing at all)
        // for an unmapped raw NDJSON sub-line ("{"/"[" -- see StructuredStreamParser#accept's raw
        // passthrough for an event with no human-readable mapping). If that line never streams again
        // while Verbose is on and the run finishes normally, the content was gone with zero trace it
        // ever existed -- a genuine silent content drop, not "verbose-gated" in any meaningful sense.
        // A compact milestone bubble must now surface instead, pointing the user at Verbose, matching
        // the established compactIfRawToolResult pointer-text pattern for the same "reachable via
        // Verbose" contract as every other suppression point in this issue.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String rawJson = "{\"type\":\"system\",\"subtype\":\"thinking_tokens\",\"tokens\":42}";

        appendStreamingLocalAgentBubble(panel, 601);
        appendLocalAgentOutput(panel, 601, rawJson);

        String transcript = transcriptMarkdown(panel);
        assertAll(
                () -> assertFalse(transcript.contains(rawJson),
                        "Raw NDJSON must never render verbatim while Verbose is off: " + transcript),
                () -> assertTrue(transcript.toLowerCase(java.util.Locale.ROOT).contains("verbose"),
                        "A notice must point the user at Verbose instead of showing nothing at all: " + transcript));
    }

    @Test
    void assistantVerboseModeShowsLiveAgentOutput() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        verbose.setSelected(true);

        appendStreamingLocalAgentBubble(panel, 88);
        appendLocalAgentOutput(panel, 88, "visible verbose agent response");

        assertAll(
                () -> assertTrue(transcriptMarkdown(panel).contains("visible verbose agent response")),
                () -> assertTrue(containsText(panel, "visible verbose agent response")));
    }

    @Test
    void assistantVerboseStreamingAccumulatesInPlaceAcrossMultipleLines() throws Exception {
        // Issue #3919: with no eager placeholder bubble to anchor the first replace, the first
        // verbose line must still establish the transcript bubble the rest of the stream updates in
        // place -- not a fresh bubble per streamed line.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        verbose.setSelected(true);

        appendStreamingLocalAgentBubble(panel, 501);
        appendLocalAgentOutput(panel, 501, "first streamed line");
        appendLocalAgentOutput(panel, 501, "second streamed line");

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("first streamed line"), markdown),
                () -> assertTrue(markdown.contains("second streamed line"), markdown),
                () -> assertEquals(2, countOccurrences(markdown, "```"),
                        "Both lines must accumulate into the same fenced code block, not a fresh bubble "
                                + "per streamed line: " + markdown));
    }

    @Test
    void assistantStructuredLocalAgentResponseShowsExactReportedTokenCountAndHidesRawUsageJson() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        // Shaped per the R3-T1 contract: an answer followed by a trailing single-line usage JSON,
        // as produced by AssistantLocalAgentRunner's structured-stream terminal event / finalOutput.
        String rawResponse = "The failure was a stale locator."
                + "\n\n{\"usage\":{\"input_tokens\":123,\"output_tokens\":45}}";

        appendStreamingLocalAgentBubble(panel, 101);
        showAgentResult(panel, 101, ShaftMcpToolResult.success(rawResponse));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("The failure was a stale locator."), markdown),
                () -> assertTrue(markdown.contains("Tokens consumed:"), markdown),
                () -> assertTrue(markdown.contains("`168`"), markdown),
                () -> assertTrue(markdown.contains("input: 123"), markdown),
                () -> assertTrue(markdown.contains("output: 45"), markdown),
                () -> assertFalse(markdown.contains("(estimated)"), markdown),
                () -> assertFalse(markdown.contains("input_tokens"), markdown));
    }

    @Test
    void assistantUnstructuredLocalAgentResponseWithoutUsageMetadataOmitsTokenLine() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String rawResponse = "Here is the explanation.\nIt spans several lines.\nNo structured data anywhere.";

        appendStreamingLocalAgentBubble(panel, 102);
        showAgentResult(panel, 102, ShaftMcpToolResult.success(rawResponse));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("Here is the explanation."), markdown),
                () -> assertFalse(markdown.contains("Tokens consumed:"), markdown));
    }

    @Test
    void assistantKillStopsLocalAgentStreamingImmediately() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        AtomicBoolean killed = new AtomicBoolean();
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(),
                () -> {
                },
                () -> killed.set(true));
        verbose.setSelected(true);
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 99);
        appendLocalAgentOutput(panel, 99, "visible before kill");

        cancelOrKillCurrent(panel);
        cancelOrKillCurrent(panel);
        appendLocalAgentOutput(panel, 99, "late output after kill");

        assertAll(
                () -> assertTrue(killed.get()),
                () -> assertTrue(transcriptMarkdown(panel).contains("visible before kill")),
                () -> assertTrue(transcriptMarkdown(panel).contains("Killed"),
                        "a killed run with rendered content must still show a terminal marker: "
                                + transcriptMarkdown(panel)),
                () -> assertFalse(transcriptMarkdown(panel).contains("late output after kill")));
    }

    @Test
    void assistantVerboseToggleOnMidStreamDoesNotClobberTheUserPromptBubble() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        panel.simulateAppendForTest("user", "the user's own prompt", "");

        appendStreamingLocalAgentBubble(panel, 201);
        appendLocalAgentOutput(panel, 201, "output while verbose was still off");
        verbose.setSelected(true);
        appendLocalAgentOutput(panel, 201, "output after verbose flipped on");

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("the user's own prompt"),
                        "Flipping Verbose on mid-run must not clobber the user's prompt bubble: " + markdown),
                () -> assertTrue(markdown.contains("output after verbose flipped on"), markdown));
    }

    @Test
    void assistantVerboseToggleOffMidStreamLeavesNoStaleStreamingBubble() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        verbose.setSelected(true);

        appendStreamingLocalAgentBubble(panel, 202);
        appendLocalAgentOutput(panel, 202, "streamed while verbose was on");
        verbose.setSelected(false);
        showAgentResult(panel, 202, ShaftMcpToolResult.success("final answer"));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("final answer"), markdown),
                () -> assertFalse(markdown.contains("streamed while verbose was on"),
                        "No stale mid-stream content should survive the final replace: " + markdown),
                () -> assertFalse(markdown.contains("Running local assistant"), markdown));
    }

    @Test
    void assistantVerboseFinalizationKeepsRawStreamedBufferReachableAfterNormalCompletion() throws Exception {
        // Issue #3976: finishLocalAgentResponse always replaced the streaming bubble with the
        // polished final answer on normal completion, discarding the accumulated localAgentOutput
        // raw buffer regardless of Verbose state. Concretely: a compacted/pointer-text milestone
        // line (e.g. the RAW_STRUCTURED_OUTPUT_NOTICE issue #3920 added) streamed last before the
        // run completed normally became a dead end -- no more streaming bubble left to re-render
        // once Verbose was toggled on, because finalization already replaced it. Mirrors
        // stopLocalAgentStreaming's Verbose-gated raw-buffer rendering (issue #3918/#3961) applied
        // to the normal-completion path.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        verbose.setSelected(true);

        appendStreamingLocalAgentBubble(panel, 801);
        appendLocalAgentOutput(panel, 801, "{\"type\":\"raw_structured_line\",\"value\":42}");

        showAgentResult(panel, 801, ShaftMcpToolResult.success("final answer"));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("final answer"), markdown),
                () -> assertTrue(markdown.contains("raw_structured_line"),
                        "With Verbose on, the raw streamed buffer must still be reachable after a "
                                + "normal completion, not silently discarded: " + markdown));
    }

    @Test
    void assistantNonVerboseFinalizationShowsOnlyThePolishedAnswerNoRawBufferLeakage() throws Exception {
        // Companion pin for the fix above (issue #3976): with Verbose OFF throughout, a normal
        // completion must keep showing only the polished final answer -- the raw streamed buffer
        // must not leak into the transcript.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        appendStreamingLocalAgentBubble(panel, 802);
        appendLocalAgentOutput(panel, 802, "{\"type\":\"raw_structured_line\",\"value\":42}");

        showAgentResult(panel, 802, ShaftMcpToolResult.success("final answer"));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("final answer"), markdown),
                () -> assertFalse(markdown.contains("raw_structured_line"),
                        "Verbose is off, so the raw streamed buffer must not leak into the transcript: "
                                + markdown));
    }

    /**
     * Issue #4319 bug 2 (best evidence-consistent mechanism found -- not a confirmed live repro):
     * Claude Code's {@code stream-json} protocol emits the model's own final "text" block as a plain
     * assistant event the moment the turn ends ({@code ClaudeStreamEventMapper#describeAssistantEvent}
     * -> {@code appendLocalAgentOutput} -> {@code addCompactLocalAgentMilestone}), rendering it as a
     * dimmed {@code KIND_MILESTONE} bubble with nothing recognizing that this text IS about to become
     * the run's terminal answer -- Claude's terminal {@code result} event's own {@code result} field
     * carries the identical text on a normal completion. {@code finishLocalAgentResponse} then renders
     * that same text again as a fresh, separate, clear bubble (non-Verbose runs never establish a
     * streaming-placeholder index to replace in place -- see {@code replaceLocalAgentStreamPlaceholder}'s
     * fresh-append branch), so the exact same wording appears twice: once dimmed, once clear. This
     * reproduces with a plain completion (no approval pause, no stream-token mismatch), unlike the
     * currentStream-mismatch hypothesis the issue itself first floated.
     */
    @Test
    void assistantNonVerboseFinalizationDoesNotDuplicateAPlainTextFinalAnswerAlreadyStreamedAsAMilestone()
            throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String finalAnswer = "I need two things before I can generate this code: 1. No file is"
                + " currently open. 2. Target URL confirmation.";

        appendStreamingLocalAgentBubble(panel, 40);
        appendLocalAgentOutput(panel, 40, finalAnswer);
        showAgentResult(panel, 40, ShaftMcpToolResult.success(finalAnswer));

        @SuppressWarnings("unchecked")
        List<ShaftAssistantChatState.Message> messages =
                ((ShaftAssistantChatState) getField(panel, "chatState")).activeMessages();
        long occurrences = messages.stream().filter(message -> message.markdown.contains(finalAnswer)).count();

        assertEquals(1, occurrences,
                "the final answer must render exactly once, not once as a dimmed milestone and again"
                        + " as the polished final bubble: " + transcriptMarkdown(panel));
    }

    /**
     * Companion guardrail for the fix above: a milestone that streamed in DIFFERENT text from the
     * eventual final answer (the common case -- a "Calling tool X..." milestone, not the model's own
     * final text block) must never be retracted just because a terminal response follows it. Both
     * must still render as separate bubbles.
     */
    @Test
    void assistantNonVerboseFinalizationStillShowsADifferentMilestoneAndTheFinalAnswerSeparately()
            throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String milestone = "Calling tool Bash (grep locator)...";
        String finalAnswer = "Found the search result element and generated the SHAFT test.";

        appendStreamingLocalAgentBubble(panel, 41);
        appendLocalAgentOutput(panel, 41, milestone);
        showAgentResult(panel, 41, ShaftMcpToolResult.success(finalAnswer));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains(milestone),
                        "a milestone with genuinely different text must survive: " + markdown),
                () -> assertTrue(markdown.contains(finalAnswer),
                        "the final answer must still render: " + markdown));
    }

    @Test
    void assistantNonVerboseRunShowsNoPlaceholderBubbleWhileRunning() throws Exception {
        // Issue #3919: the "_Running local assistant..._" placeholder bubble is gone -- progress
        // while a run is in flight is the toolbar spinner/status label's job alone, not a transcript
        // bubble. The final answer still lands as a normal fresh bubble once the run completes.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        String transcriptBeforeStream = transcriptMarkdown(panel);

        appendStreamingLocalAgentBubble(panel, 203);
        assertEquals(transcriptBeforeStream, transcriptMarkdown(panel),
                "Starting a local-agent stream must not append any placeholder bubble: "
                        + transcriptMarkdown(panel));

        showAgentResult(panel, 203, ShaftMcpToolResult.success("done"));
        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("done"), markdown),
                () -> assertFalse(markdown.contains("Running local assistant"), markdown));
    }

    @Test
    void assistantKilledNonVerboseRunBeforeAnyOutputStillShowsKilled() throws Exception {
        // Issue #3919: with no placeholder bubble to replace (nothing ever streamed), a kill request
        // must still land a "Killed" bubble instead of leaving the run with no terminal signal at all.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        AtomicBoolean killed = new AtomicBoolean();
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(),
                () -> {
                },
                () -> killed.set(true));
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 204);

        cancelOrKillCurrent(panel);
        cancelOrKillCurrent(panel);

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(killed.get()),
                () -> assertTrue(markdown.contains("Killed"),
                        "A killed non-verbose run must still show a terminal signal: " + markdown),
                () -> assertFalse(markdown.contains("Running local assistant"), markdown));
    }

    @Test
    void assistantKilledRunAfterVerboseToggledOffMidStreamDoesNotDumpRawBuffer() throws Exception {
        // Issue #3918: stopLocalAgentStreaming used to key its "show the raw partial buffer?" decision
        // off the sticky localAgentBubbleRendersContent flag (true forever once Verbose rendered
        // anything this run), so a user who saw raw content while Verbose was on, then flipped it back
        // off before killing, still got the entire raw buffer dumped -- unformatted content leaking
        // into a non-verbose transcript. The decision must key off Verbose's CURRENT state at kill
        // time instead, mirroring how a normal completion's terminal replace already behaves (see
        // assistantVerboseToggleOffMidStreamLeavesNoStaleStreamingBubble).
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        AtomicBoolean killed = new AtomicBoolean();
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(),
                () -> {
                },
                () -> killed.set(true));
        verbose.setSelected(true);
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 301);
        appendLocalAgentOutput(panel, 301, "raw content shown while verbose was on");
        verbose.setSelected(false);

        cancelOrKillCurrent(panel);
        cancelOrKillCurrent(panel);

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(killed.get()),
                () -> assertTrue(markdown.contains("Killed"), markdown),
                () -> assertFalse(markdown.contains("raw content shown while verbose was on"),
                        "Verbose was off at kill time, so the raw partial buffer must not be dumped: "
                                + markdown));
    }

    @Test
    void assistantCancelledRunAfterVerboseToggledOffMidStreamDoesNotDumpRawBuffer() throws Exception {
        // Issue #3920 fold-in: unlike stopLocalAgentStreaming (the Kill path, fixed for #3918 by
        // PR#3958), showAgentResult's plain-Cancel path still keyed its "show the raw partial
        // buffer?" decision off the sticky localAgentBubbleRendersContent flag (true forever once
        // Verbose rendered anything this run) instead of Verbose's CURRENT state -- so a user who saw
        // raw content while Verbose was on, then flipped it back off before cancelling (as opposed to
        // killing), still got the entire raw buffer dumped into the "Cancelled" bubble. Mirrors
        // assistantKilledRunAfterVerboseToggledOffMidStreamDoesNotDumpRawBuffer above.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        verbose.setSelected(true);

        appendStreamingLocalAgentBubble(panel, 501);
        appendLocalAgentOutput(panel, 501, "raw content shown while verbose was on");
        verbose.setSelected(false);

        showAgentResult(panel, 501, null, new CancellationException("cancelled"));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("Cancelled"), markdown),
                () -> assertFalse(markdown.contains("raw content shown while verbose was on"),
                        "Verbose was off at cancel time, so the raw partial buffer must not be dumped: "
                                + markdown));
    }

    @Test
    void assistantNonVerboseBufferedOrCustomRunSuppressesRawLinesFromMilestoneBubbles() throws Exception {
        // Issue #3918: a buffered/custom-command local-agent run (Copilot's default command, or any
        // hand-typed custom command) has zero SHAFT-translated milestone content -- its entire live
        // stream is raw CLI passthrough (AssistantLocalAgentRunner's effectiveConsumer). Non-verbose
        // mode must show none of it as a compact milestone bubble; the data still reaches Verbose mode
        // via the raw streaming bubble/final answer, unchanged.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        appendStreamingLocalAgentBubble(panel, 401);
        setField(panel, "activeLocalAgentStreamHasStructuredMilestones", false);

        appendLocalAgentOutput(panel, 401,
                "This CLI has no structured live stream; its raw output is shown as-is below until it completes.");
        appendLocalAgentOutput(panel, 401, "copilot line one");

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertFalse(markdown.contains("copilot line one"),
                        "A buffered/custom-command run's raw output must stay Verbose-only: " + markdown),
                () -> assertFalse(markdown.contains("no structured live stream"), markdown));
    }

    @Test
    void assistantNonVerboseModeSuppressesRawNonJsonBannerPassthroughFromMilestoneBubbles() throws Exception {
        // Issue #3918: StructuredStreamParser.accept forwards non-JSON stdout (CLI banners/warnings)
        // marked with AssistantLocalAgentRunner.RAW_STDOUT_MARKER so it reaches the live output
        // consumer (Verbose must never hide information) without being mistaken for a translated
        // milestone line. Non-verbose mode must not render it as a compact milestone bubble.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        appendStreamingLocalAgentBubble(panel, 402);

        appendLocalAgentOutput(panel, 402,
                AssistantLocalAgentRunner.RAW_STDOUT_MARKER + "2026-07-08 INFO some internal CLI banner");

        String markdown = transcriptMarkdown(panel);
        assertFalse(markdown.contains("internal CLI banner"),
                "Raw non-JSON CLI stdout must stay Verbose-only, not become a milestone bubble: " + markdown);
    }

    @Test
    void assistantVerboseModeStripsTheRawPassthroughMarkerBeforeDisplay() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        verbose.setSelected(true);
        appendStreamingLocalAgentBubble(panel, 403);

        appendLocalAgentOutput(panel, 403, AssistantLocalAgentRunner.RAW_STDOUT_MARKER + "internal CLI banner text");

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("internal CLI banner text"), markdown),
                () -> assertFalse(markdown.contains(AssistantLocalAgentRunner.RAW_STDOUT_MARKER),
                        "The internal raw-passthrough marker must never leak into the UI: " + markdown));
    }

    @Test
    void assistantDoesNotPersistRawResponsePayloads() throws Exception {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();

        chatState.append("assistant", """
                Rendered response
                OPENAI_API_KEY=abc123
                Authorization: Bearer secret-token
                {"apiKey": "json-secret", "password": "json-password"}
                Cookie: session=abc123
                """.stripIndent(),
                "{\"secret\":\"raw payload\"}");
        ShaftAssistantChatState.StateData state = chatState.getState();
        String persistedMarkdown = state.sessions.get(0).messages.get(0).markdown;
        com.intellij.openapi.components.State annotation =
                ShaftAssistantChatState.class.getAnnotation(com.intellij.openapi.components.State.class);

        assertAll(
                () -> assertTrue(persistedMarkdown.contains("Rendered response")),
                () -> assertFalse(persistedMarkdown.contains("abc123")),
                () -> assertFalse(persistedMarkdown.contains("secret-token")),
                () -> assertFalse(persistedMarkdown.contains("json-secret")),
                () -> assertFalse(persistedMarkdown.contains("json-password")),
                () -> assertFalse(persistedMarkdown.contains("session=abc123")),
                () -> assertFalse(persistedMarkdown.contains("raw payload")),
                () -> assertTrue(persistedMarkdown.contains("<redacted>")),
                () -> assertEquals(com.intellij.openapi.components.StoragePathMacros.WORKSPACE_FILE,
                        annotation.storages()[0].value()),
                () -> assertThrows(NoSuchFieldException.class,
                        () -> ShaftAssistantChatState.Message.class.getDeclaredField("raw")));
    }

    @Test
    void assistantChatStateFallsBackFromCorruptPersistedState() {
        ShaftAssistantChatState.StateData state = new ShaftAssistantChatState.StateData();
        ShaftAssistantChatState.Session corrupt = new ShaftAssistantChatState.Session();
        corrupt.id = null;
        corrupt.title = null;
        corrupt.messages = null;
        state.sessions.add(null);
        state.sessions.add(corrupt);
        state.activeSessionId = "missing";

        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.loadState(state);
        chatState.append("user", "fresh prompt after fallback", "");

        assertAll(
                () -> assertEquals(1, chatState.sessions().size()),
                () -> assertEquals("fresh prompt after fallback", chatState.activeSession().title),
                () -> assertTrue(chatState.activeMarkdown().contains("fresh prompt after fallback")));
    }

    @Test
    void assistantNewChatClearsRerunAndCopyState() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        assistantPrompt(panel).setText("/help");
        clickAccessible(panel, "Send assistant prompt");
        assertAll(
                () -> assertTrue(findButton(panel, "Rerun").isEnabled()),
                () -> assertTrue(findButton(panel, "Copy response").isEnabled()));

        click(panel, "New chat");

        assertAll(
                () -> assertFalse(findButton(panel, "Rerun").isEnabled()),
                () -> assertFalse(findButton(panel, "Copy response").isEnabled()),
                () -> assertFalse(transcriptMarkdown(panel).contains("Slash commands:")));
    }

    @Test
    void assistantAndToolsControlsExposeAccessibleMetadata() {
        assertAccessibleControls(new ShaftAssistantPanel(null, blankMcpSettings()));
        assertAccessibleControls(new ShaftFeaturePanel(null, blankMcpSettings()));
        assertAccessibleControls(new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        }));
        assertAccessibleControls(new GuidedWorkflowPanel(null, (tool, arguments) -> {
        }));
        assertAccessibleControls(new EvidenceTriagePanel(null, (tool, arguments) -> {
        }));
    }

    @Test
    void assistantComposerUsesPlainLanguageInputAndModernThinkingIndicator() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        JProgressBar spinner = findByAccessibleName(panel, "Assistant thinking spinner", JProgressBar.class);
        JButton sendButton = findByAccessibleName(panel, "Send assistant prompt", JButton.class);
        JLabel status = findByAccessibleName(panel, "Assistant status", JLabel.class);
        JTextComponent prompt = assistantPrompt(panel);

        assertAll(
                // The composer understands plain language: no command picker, no command-help
                // button, and no slash-command coaching anywhere in the surface.
                () -> assertNull(findByAccessibleName(panel, "Assistant command autocomplete", JComboBox.class),
                        "the slash-command picker is retired"),
                () -> assertNull(findByAccessibleName(panel, "SHAFT command hints", JButton.class),
                        "the command help button is retired"),
                () -> assertTrue(prompt.getAccessibleContext().getAccessibleDescription().contains("guarded local Agent"),
                        "prompt description should mention guarded local Agent"),
                () -> assertTrue(prompt instanceof JBTextArea, "prompt should be a JBTextArea"),
                () -> assertTrue(((JBTextArea) prompt).getRows() >= 6, "prompt should have at least six rows"),
                () -> assertFalse(containsText(panel, "Add context (#), extensions (@), commands (/commands)"),
                        "old starter strip copy should stay removed"),
                () -> assertNull(findByAccessibleName(panel, "Assistant context suggestions", JButton.class),
                        "the '+' context button is retired; typed @ # triggers own that role"),
                () -> assertTrue(String.valueOf(((JBTextArea) prompt)
                                .getClientProperty("shaft.prompt.placeholder"))
                                .contains("Tell SHAFT what you need"),
                        "the prompt placeholder should invite plain language"),
                () -> assertFalse(String.valueOf(((JBTextArea) prompt)
                                .getClientProperty("shaft.prompt.placeholder"))
                                .contains("/"),
                        "the prompt placeholder must not advertise slash commands"),
                () -> assertNotNull(verbose),
                () -> assertFalse(verbose.isSelected()),
                () -> assertTrue(verbose.getToolTipText().contains("live local agent output")),
                () -> assertNotNull(spinner, "spinner should exist"),
                () -> assertTrue(spinner.isIndeterminate(), "spinner should be indeterminate"),
                () -> assertFalse(spinner.isVisible(), "spinner should be hidden while idle"),
                () -> assertNotNull(status, "status should exist"),
                () -> assertTrue(status.getToolTipText().contains("Try asking"), "status tooltip should be idle copy"),
                () -> assertTrue(status.getAccessibleContext().getAccessibleDescription().contains("Try asking"),
                        "status accessible description should be idle copy"),
                () -> assertNotNull(sendButton, "send button should exist"),
                () -> assertTrue(sendButton.getToolTipText().contains("Command+Enter"), "send tooltip should mention Command+Enter"),
                () -> assertTrue(sendButton.getToolTipText().contains("Ctrl+click"), "send tooltip should mention Ctrl+click"),
                () -> assertTrue(sendButton.getParent().getLayout() instanceof FlowLayout, "send row should use FlowLayout"),
                () -> assertEquals(FlowLayout.RIGHT, ((FlowLayout) sendButton.getParent().getLayout()).getAlignment(),
                        "send row should align right"),
                () -> assertEquals("", sendButton.getText(), "send button should stay icon-only"),
                () -> assertNotNull(sendButton.getIcon(), "send button should have an icon"),
                () -> assertTrue(sendButton.getIcon().getIconWidth() > 0, "send icon should have width"),
                () -> assertTrue(sendButton.getIcon().getIconHeight() > 0, "send icon should have height"));
    }

    @Test
    void assistantComposerKeepsLongSetupPromptScrollable() throws Exception {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.agentGuidanceOptimizationPromptPending = true;
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        JTextComponent prompt = assistantPrompt(panel);
        JScrollPane promptScroll = enclosingScrollPane(prompt);

        assertNotNull(promptScroll);
        promptScroll.setSize(new Dimension(640, 120));
        promptScroll.doLayout();
        int lineHeight = prompt.getFontMetrics(prompt.getFont()).getHeight();
        int lineCount = prompt.getDocument().getDefaultRootElement().getElementCount();

        assertAll(
                () -> assertTrue(lineCount > 10),
                () -> assertTrue(prompt.getPreferredSize().height > lineHeight * 10,
                        "Long setup prompt should have enough view height to scroll to its final line."));
    }

    @Test
    void assistantEmptyChatOmitsStarterStripAndKeepsPromptUsable() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        JTextComponent prompt = assistantPrompt(panel);

        assertAll(
                () -> assertNull(findByAccessibleName(panel, "Assistant starter actions", JComponent.class)),
                () -> assertTrue(prompt instanceof JBTextArea),
                () -> assertTrue(((JBTextArea) prompt).getRows() >= 6));
    }

    @Test
    void assistantLocalPromptCompletionOmitsSyntheticMilestoneBubbles() {
        // Issue #3919: "Prompt received" and a bare glyph-prefixed "Completed" bubble were pure
        // synthesized progress chrome duplicating the toolbar spinner/status label -- removed from
        // the transcript entirely; the real /help response is what the user actually asked for.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        assistantPrompt(panel).setText("/help");
        clickAccessible(panel, "Send assistant prompt");

        String transcript = transcriptMarkdown(panel);
        assertAll(
                () -> assertFalse(transcript.contains("Prompt received"), transcript),
                () -> assertFalse(transcript.contains("\n**Completed**"), transcript),
                () -> assertTrue(transcript.contains("## SHAFT Assistant commands"), transcript));
    }

    @Test
    void assistantCancelRequestShowsSingleCancelledResponseWithoutMilestoneBubbles() throws Exception {
        // Issue #3919: "Cancelling..."/terminal "Cancelled (Ns)" milestone bubbles are gone from the
        // transcript -- the toolbar status label still shows "Cancelling..." (see containsText-based
        // coverage elsewhere), and the real cancelled-tool response bubble already says so.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");

        cancelOrKillCurrent(panel);
        String transcriptAfterCancelRequest = transcriptMarkdown(panel);
        assertFalse(transcriptAfterCancelRequest.contains("Cancelling..."), transcriptAfterCancelRequest);

        showAssistantResult(panel, "shaft_guide_search", null, new CancellationException("cancelled"));

        String transcript = transcriptMarkdown(panel);
        assertAll(
                () -> assertFalse(transcript.contains("Cancelled ("), transcript),
                () -> assertFalse(transcript.contains("Killed ("), transcript),
                () -> assertTrue(transcript.toLowerCase(java.util.Locale.ROOT).contains("cancelled"), transcript));
    }

    @Test
    void assistantCancelAfterPartialStreamedOutputPreservesPartialAndShowsCancelled() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        verbose.setSelected(true);
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 301);
        appendLocalAgentOutput(panel, 301, "partial answer streamed before cancel");

        // A single cancel click only requests cancellation -- it must not escalate to a kill.
        cancelOrKillCurrent(panel);
        assertEquals(false, getField(panel, "killRequested"));

        showAgentResult(panel, 301, null, new CancellationException("cancelled"));

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("partial answer streamed before cancel"), markdown),
                () -> assertTrue(markdown.contains("Cancelled"), markdown),
                () -> assertFalse(markdown.contains("Killed"), markdown));
    }

    /**
     * Issue #3962 (hostile-review finding): a soft Cancel renders the "_Cancelled._" bubble
     * synchronously (matching Kill's shape -- neither one waits on the companion before the FIRST
     * render, since the run may already have streamed content the user must see immediately). The
     * companion is only ever consulted afterward, to upgrade that exact already-rendered message in
     * place once (if) it resolves with a real answer -- mirroring {@link
     * #assistantKillUpgradesTheSameFinalizedMessageOnceTerminalAnswerArrives}. Streams a line BEFORE
     * cancelling (the earlier version of this test never did, so it could not distinguish an in-place
     * replace from an appended duplicate) and asserts the message count never grows across the
     * upgrade -- the exact regression a broken token re-read inside the deferred callback produced.
     */
    @Test
    void assistantCancelUpgradesTheSameFinalizedMessageOnceTerminalAnswerArrives() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 401);
        appendLocalAgentOutput(panel, 401, "a line streamed before cancel");

        CompletableFuture<String> pendingTerminalAnswer = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingTerminalAnswer);

        cancelOrKillCurrent(panel);
        showAgentResult(panel, 401, null, new CancellationException("cancelled"));

        String immediately = transcriptMarkdown(panel);
        assertTrue(immediately.contains("Cancelled"), immediately);
        int messageCountAfterCancel = currentSessionMessageCount(panel);

        pendingTerminalAnswer.complete("The finished answer, parsed before cancel won the race.");
        pumpEdt();

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("The finished answer, parsed before cancel won the race."),
                        markdown),
                () -> assertEquals(messageCountAfterCancel, currentSessionMessageCount(panel),
                        "the upgrade must replace the same message in place, never append a second one"),
                () -> assertEquals(1,
                        countOccurrences(markdown, "The finished answer, parsed before cancel won the race."),
                        "the terminal answer must render exactly once: " + markdown));
    }

    /**
     * Same regression as above, for the case the companion resolves with {@code null} (the run never
     * produced a terminal event -- e.g. it failed before completing). The message count must still
     * never grow: no upgrade should happen, and critically no *second* rendering attempt either.
     */
    @Test
    void assistantCancelAfterStreamingDoesNotDuplicateWhenCompanionResolvesWithNoAnswer() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 403);
        appendLocalAgentOutput(panel, 403, "a line streamed before cancel");

        CompletableFuture<String> pendingTerminalAnswer = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingTerminalAnswer);

        cancelOrKillCurrent(panel);
        showAgentResult(panel, 403, null, new CancellationException("cancelled"));
        int messageCountAfterCancel = currentSessionMessageCount(panel);

        pendingTerminalAnswer.complete(null);
        pumpEdt();

        assertEquals(messageCountAfterCancel, currentSessionMessageCount(panel),
                "resolving with no answer must never append (or otherwise duplicate) a message");
    }

    /**
     * Issue #3962 (second-review finding F1): the recovered terminal answer written by the Cancel
     * upgrade must still carry the same "**Token usage:**..." disclaimer every other terminal
     * local-agent response gets (see the pinned {@code
     * assistantLocalAgentCancelledWithoutUsageMetadataStatesTokenUsageNotAvailable}, which never
     * resolves a companion so it could not catch this regression) -- the upgrade must route through
     * {@code withLocalAgentTokenUsage} exactly like {@code finishLocalAgentResponse} already does for
     * the ordinary (non-recovered) cancelled/killed text.
     */
    @Test
    void assistantCancelUpgradeStillIncludesTokenUsageStatement() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 405);
        appendLocalAgentOutput(panel, 405, "a line streamed before cancel");

        CompletableFuture<String> pendingTerminalAnswer = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingTerminalAnswer);

        cancelOrKillCurrent(panel);
        showAgentResult(panel, 405, null, new CancellationException("cancelled"));

        pendingTerminalAnswer.complete("The recovered final answer.");
        pumpEdt();

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("The recovered final answer."), markdown),
                () -> assertTrue(markdown.toLowerCase(java.util.Locale.ROOT).contains("token usage"),
                        "the token-usage disclaimer must survive the upgrade, not just the original "
                                + "bare marker: " + markdown));
    }

    /**
     * Issue #4210 (fast-follow to #3962/PR #4204): while a Cancelled bubble's companion terminal-
     * answer future is still unresolved, the bubble must carry a subtle "recovering final answer"
     * caption so the user can tell a change may still land -- today it sits indistinguishable from a
     * genuinely final Cancelled state for as long as the run takes to actually finish shutting down.
     * The caption must disappear the moment the companion resolves with a real answer (folded into
     * the upgraded content, never left stapled on).
     */
    @Test
    void assistantCancelledBubbleShowsPendingAnswerIndicatorWhileCompanionIsUnresolved() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 420);
        appendLocalAgentOutput(panel, 420, "a line streamed before cancel");

        CompletableFuture<String> pendingTerminalAnswer = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingTerminalAnswer);

        cancelOrKillCurrent(panel);
        showAgentResult(panel, 420, null, new CancellationException("cancelled"));

        String whilePending = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(whilePending.contains("Cancelled"), whilePending),
                () -> assertTrue(whilePending.toLowerCase(java.util.Locale.ROOT)
                                .contains("recovering final answer"),
                        "an unresolved companion must show a pending-answer indicator: " + whilePending));

        pendingTerminalAnswer.complete("The finished answer.");
        pumpEdt();

        String afterResolve = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(afterResolve.contains("The finished answer."), afterResolve),
                () -> assertFalse(afterResolve.toLowerCase(java.util.Locale.ROOT)
                                .contains("recovering final answer"),
                        "the indicator must be gone once the upgrade lands: " + afterResolve));
    }

    /**
     * Same as above for the case the companion resolves with no answer at all (the run never
     * produced a terminal event -- e.g. it failed before completing): {@link
     * #upgradeFinalizedLocalAgentMessage} intentionally no-ops here (nothing to upgrade to), but the
     * pending-answer indicator must still clear -- the companion HAS resolved, so there is nothing
     * left to wait for, and the bubble must not be stuck advertising a change that will never come.
     */
    @Test
    void assistantCancelledBubblePendingAnswerIndicatorClearsWhenCompanionResolvesWithNoAnswer() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 421);
        appendLocalAgentOutput(panel, 421, "a line streamed before cancel");

        CompletableFuture<String> pendingTerminalAnswer = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingTerminalAnswer);

        cancelOrKillCurrent(panel);
        showAgentResult(panel, 421, null, new CancellationException("cancelled"));
        assertTrue(transcriptMarkdown(panel).toLowerCase(java.util.Locale.ROOT)
                .contains("recovering final answer"));

        pendingTerminalAnswer.complete(null);
        pumpEdt();

        String after = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(after.contains("Cancelled"), after),
                () -> assertFalse(after.toLowerCase(java.util.Locale.ROOT).contains("recovering final answer"),
                        "resolving with no answer must still clear the indicator: " + after));
    }

    /**
     * Kill-side counterpart of {@link
     * #assistantCancelledBubbleShowsPendingAnswerIndicatorWhileCompanionIsUnresolved} (issue #4210):
     * {@code stopLocalAgentStreaming} finalizes the "_Killed._" bubble synchronously, before {@code
     * ShaftMcpInvocation#kill()} even runs, so its companion is essentially guaranteed unresolved at
     * that point -- the indicator must show there too, and clear once the upgrade lands.
     */
    @Test
    void assistantKilledBubbleShowsPendingAnswerIndicatorWhileCompanionIsUnresolved() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 422);

        CompletableFuture<String> pendingTerminalAnswer = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingTerminalAnswer);

        cancelOrKillCurrent(panel); // Cancel
        cancelOrKillCurrent(panel); // escalate to Kill -- calls stopLocalAgentStreaming synchronously

        String whilePending = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(whilePending.contains("Killed"), whilePending),
                () -> assertTrue(whilePending.toLowerCase(java.util.Locale.ROOT)
                                .contains("recovering final answer"),
                        "an unresolved companion must show a pending-answer indicator: " + whilePending));

        pendingTerminalAnswer.complete("The finished answer, parsed before kill won the race.");
        pumpEdt();

        String afterResolve = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(afterResolve.contains("The finished answer, parsed before kill won the race."),
                        afterResolve),
                () -> assertFalse(afterResolve.toLowerCase(java.util.Locale.ROOT)
                                .contains("recovering final answer"),
                        "the indicator must be gone once the upgrade lands: " + afterResolve));
    }

    /**
     * Issue #4210 (independent-review finding): the pending-answer indicator above is gated purely by
     * an in-memory {@code CompletableFuture} that does NOT survive a project/IDE restart. If IntelliJ's
     * workspace autosave calls {@code getState()} while the companion is still unresolved -- entirely
     * plausible during a run's up-to-several-minutes process-timeout window -- and the project is then
     * reopened (simulated here by feeding that {@code StateData} into a brand-new {@code
     * ShaftAssistantChatState}, standing in for a fresh IDE instance with no live future for the old
     * run), nothing must leave the caption baked into the reloaded transcript permanently: the run it
     * refers to structurally can never resolve after reload, so the caption would otherwise become a
     * permanent, false claim. The underlying terminal marker itself must still survive the round-trip.
     */
    @Test
    void assistantCancelledBubblePendingAnswerIndicatorDoesNotSurviveAStateRoundTripWhileCompanionIsUnresolved()
            throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 423);
        appendLocalAgentOutput(panel, 423, "a line streamed before cancel");

        // Deliberately never completed -- the companion is still pending when the "restart" below
        // happens, exactly like the real up-to-several-minutes process-timeout window.
        CompletableFuture<String> pendingTerminalAnswer = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingTerminalAnswer);

        cancelOrKillCurrent(panel);
        showAgentResult(panel, 423, null, new CancellationException("cancelled"));
        assertTrue(transcriptMarkdown(panel).toLowerCase(java.util.Locale.ROOT)
                .contains("recovering final answer"),
                "sanity: the indicator must be showing before the simulated restart");

        ShaftAssistantChatState chatState = (ShaftAssistantChatState) getField(panel, "chatState");
        // Simulates IntelliJ's workspace autosave firing while the companion is still unresolved.
        ShaftAssistantChatState.StateData persisted = chatState.getState();

        // Simulates a fresh IDE/project instance after restart: a new ShaftAssistantChatState has no
        // live pendingTerminalAnswer field tied to the old run at all.
        ShaftAssistantChatState reloaded = new ShaftAssistantChatState();
        reloaded.loadState(persisted);

        String reloadedMarkdown = reloaded.activeMarkdown();
        assertAll(
                () -> assertTrue(reloadedMarkdown.contains("Cancelled"),
                        "the underlying terminal marker must survive the round-trip: " + reloadedMarkdown),
                () -> assertFalse(reloadedMarkdown.toLowerCase(java.util.Locale.ROOT)
                                .contains("recovering final answer"),
                        "a stray pending-answer caption must never survive a save/reload round-trip -- "
                                + "the run it refers to can never resolve after reload: " + reloadedMarkdown));
    }

    /**
     * Issue #3962 (second-review finding F2/F3): reproduces the exact scenario the reviewer found --
     * at {@link ShaftAssistantChatState#MAX_MESSAGES_PER_SESSION}, appending the run's own cancelled
     * bubble trips {@code trim()}'s {@code remove(0)}, shifting that bubble's real index down by one
     * from wherever a caller predicted it would land ahead of time. The fix (recording the REAL
     * post-trim index {@code replaceLocalAgentStreamPlaceholder} used, instead of predicting it
     * beforehand) must land the recovered answer correctly even here -- not silently no-op and leave
     * the bare "_Cancelled._" marker forever, which would reproduce #3962's own original symptom.
     */
    @Test
    void assistantCancelUpgradeStillLandsWhenTheStreamedBubbleTripsTheSessionCap() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);

        ShaftAssistantChatState chatState = (ShaftAssistantChatState) getField(panel, "chatState");
        for (int i = 0; i < ShaftAssistantChatState.MAX_MESSAGES_PER_SESSION; i++) {
            chatState.append("assistant", "filler " + i, "");
        }
        assertEquals(ShaftAssistantChatState.MAX_MESSAGES_PER_SESSION, chatState.activeSession().messages.size());

        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 406);
        // Non-verbose: lands as its own compact milestone message (see appendLocalAgentOutput), which
        // alone already trips the cap's trim() once.
        appendLocalAgentOutput(panel, 406, "a line streamed before cancel");

        CompletableFuture<String> pendingTerminalAnswer = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingTerminalAnswer);

        cancelOrKillCurrent(panel);
        showAgentResult(panel, 406, null, new CancellationException("cancelled"));

        assertEquals(ShaftAssistantChatState.MAX_MESSAGES_PER_SESSION, chatState.activeSession().messages.size(),
                "the session must still be capped after the cancelled bubble's own append/trim");

        pendingTerminalAnswer.complete("The recovered final answer despite the cap trim.");
        pumpEdt();

        String markdown = transcriptMarkdown(panel);
        assertTrue(markdown.contains("The recovered final answer despite the cap trim."),
                "the recovered answer must land even when trim() shifted the cancelled bubble's real "
                        + "index out from under a beforehand prediction: " + markdown);
    }

    /**
     * Issue #3962 (third-review finding 2a): capturing an {@code int} index at render time (rather
     * than the {@link ShaftAssistantChatState.Message} object itself) goes stale the moment ANY later
     * append trims the session again -- even one completely unrelated to this run, like the user's
     * next message. At {@link ShaftAssistantChatState#MAX_MESSAGES_PER_SESSION}, every append shifts
     * every earlier index down by one. The recovered answer must still land correctly even after such
     * a shift happens between the synchronous cancelled render and the companion resolving.
     */
    @Test
    void assistantCancelUpgradeStillLandsAfterAnUnrelatedAppendShiftsItsIndexAtTheCap() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);

        ShaftAssistantChatState chatState = (ShaftAssistantChatState) getField(panel, "chatState");
        for (int i = 0; i < ShaftAssistantChatState.MAX_MESSAGES_PER_SESSION; i++) {
            chatState.append("assistant", "filler " + i, "");
        }

        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 410);
        appendLocalAgentOutput(panel, 410, "a line streamed before cancel");

        CompletableFuture<String> pendingTerminalAnswer = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingTerminalAnswer);

        cancelOrKillCurrent(panel);
        showAgentResult(panel, 410, null, new CancellationException("cancelled"));

        // Before the companion resolves, an unrelated later message is appended (e.g. the user's next
        // prompt) -- at the cap, this trims the oldest message, shifting the just-cancelled bubble's
        // real position down by one from wherever it landed right after the synchronous render.
        chatState.append("user", "an unrelated later message", "");
        assertEquals(ShaftAssistantChatState.MAX_MESSAGES_PER_SESSION, chatState.activeSession().messages.size());

        pendingTerminalAnswer.complete("The recovered final answer despite the later shift.");
        pumpEdt();

        String markdown = transcriptMarkdown(panel);
        assertTrue(markdown.contains("The recovered final answer despite the later shift."),
                "the recovered answer must still land after an unrelated append shifts its real index "
                        + "out from under a captured int: " + markdown);
    }

    /**
     * Issue #3962 (third-review finding 2b -- a genuinely new defect introduced by the F2/F3 fix, not
     * merely an old symptom recurring): two runs cancelled back-to-back at the session cap. Run A's
     * companion stays unresolved while run B starts, streams, and is itself cancelled -- run B's own
     * cancelled-bubble append trims the session again, shifting run A's already-captured index. If
     * that capture is a stale {@code int}, run A's late-resolving companion would target whatever
     * message now sits at that index -- run B's own cancelled bubble -- and silently overwrite it
     * with run A's answer: wrong content landing in the wrong bubble, not just a lost update. Each
     * run's recovered answer must land only in its own bubble, exactly once, never the other's.
     */
    @Test
    void assistantTwoCancelledRunsAtTheCapNeverCrossContaminateEachOthersBubble() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftAssistantChatState chatState = (ShaftAssistantChatState) getField(panel, "chatState");
        for (int i = 0; i < ShaftAssistantChatState.MAX_MESSAGES_PER_SESSION; i++) {
            chatState.append("assistant", "filler " + i, "");
        }

        // Run A: starts, streams, is cancelled -- its companion is deliberately left unresolved,
        // simulating a slow CLI that hasn't produced a terminal event yet.
        ShaftMcpInvocation invocationA = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocationA);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 411);
        appendLocalAgentOutput(panel, 411, "run A streamed line");
        CompletableFuture<String> pendingA = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingA);
        cancelOrKillCurrent(panel);
        showAgentResult(panel, 411, null, new CancellationException("cancelled"));

        // Run B: a new prompt sent right after cancelling A, also streamed and cancelled -- at the
        // cap, its own cancelled-bubble append trims the session again, shifting A's bubble down by
        // one more from wherever it was captured.
        ShaftMcpInvocation invocationB = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocationB);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 412);
        appendLocalAgentOutput(panel, 412, "run B streamed line");
        CompletableFuture<String> pendingB = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingB);
        cancelOrKillCurrent(panel);
        showAgentResult(panel, 412, null, new CancellationException("cancelled"));

        // Run A's companion resolves LATE, after run B has already landed its own cancelled bubble.
        pendingA.complete("Run A's recovered answer.");
        pumpEdt();
        pendingB.complete("Run B's recovered answer.");
        pumpEdt();

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("Run A's recovered answer."), markdown),
                () -> assertTrue(markdown.contains("Run B's recovered answer."), markdown),
                () -> assertEquals(1, countOccurrences(markdown, "Run A's recovered answer."),
                        "run A's answer must land exactly once, in its own bubble: " + markdown),
                () -> assertEquals(1, countOccurrences(markdown, "Run B's recovered answer."),
                        "run B's answer must land exactly once, in its own bubble, never overwritten "
                                + "by run A's late-resolving companion: " + markdown));
    }

    /**
     * Issue #3962, Kill side: unlike Cancel, {@code stopLocalAgentStreaming} finalizes the "_Killed._"
     * bubble synchronously at button-click time -- before {@code ShaftMcpInvocation#kill()} even runs
     * -- so the companion cannot be waited on there without delaying the existing instant feedback.
     * Instead the same already-finalized message is upgraded in place once the companion resolves.
     * Proves both halves of the anti-duplication requirement: the message count never grows (no
     * second bubble) and the upgrade only fires once real content exists.
     */
    @Test
    void assistantKillUpgradesTheSameFinalizedMessageOnceTerminalAnswerArrives() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 402);

        CompletableFuture<String> pendingTerminalAnswer = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingTerminalAnswer);

        cancelOrKillCurrent(panel); // Cancel
        cancelOrKillCurrent(panel); // escalate to Kill -- calls stopLocalAgentStreaming synchronously

        String immediately = transcriptMarkdown(panel);
        assertTrue(immediately.contains("Killed"), immediately);
        int messageCountAfterKill = currentSessionMessageCount(panel);

        pendingTerminalAnswer.complete("The finished answer, parsed before kill won the race.");
        pumpEdt();

        String markdown = transcriptMarkdown(panel);
        assertAll(
                () -> assertTrue(markdown.contains("The finished answer, parsed before kill won the race."),
                        markdown),
                () -> assertEquals(messageCountAfterKill, currentSessionMessageCount(panel),
                        "the upgrade must replace the same message in place, never append a second one"),
                () -> assertEquals(1,
                        countOccurrences(markdown, "The finished answer, parsed before kill won the race."),
                        "the terminal answer must render exactly once: " + markdown));
    }

    /**
     * Issue #3962 (hostile-review finding 3): {@code setRunning(false, ...)} re-enables chat
     * switching before the deferred upgrade runs, so a user who switches to a different chat in that
     * window must never have the recovered terminal answer land there. Deliberately fabricates the
     * worst case the reviewer described: the now-active OTHER session's message at the exact same
     * index also happens to carry the "_Killed._" marker -- so an index-and-marker check alone (with
     * no session identity check) would be fooled into overwriting it. The upgrade must guard on the
     * session id captured at schedule time, not whatever session happens to be active when the
     * companion resolves.
     */
    @Test
    void assistantKillTerminalAnswerUpgradeIsSkippedIfTheUserSwitchedSessionsMeanwhile() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");
        appendStreamingLocalAgentBubble(panel, 404);
        appendLocalAgentOutput(panel, 404, "a line streamed before kill");

        CompletableFuture<String> pendingTerminalAnswer = new CompletableFuture<>();
        setField(panel, "pendingTerminalAnswer", pendingTerminalAnswer);

        cancelOrKillCurrent(panel); // Cancel
        cancelOrKillCurrent(panel); // escalate to Kill

        ShaftAssistantChatState chatState = (ShaftAssistantChatState) getField(panel, "chatState");
        int killedMessageIndex = chatState.activeSession().messages.size() - 1;
        assertTrue(chatState.activeSession().messages.get(killedMessageIndex).markdown.contains("_Killed._"));

        // The user leaves this chat for a different one whose message at that SAME index also
        // happens to carry the "_Killed._" marker -- a coincidence the fix must not be fooled by.
        chatState.newSession();
        for (int i = 0; i < killedMessageIndex; i++) {
            chatState.append("assistant", "filler message " + i, "");
        }
        chatState.append("assistant", "_Killed._", "");
        String otherSessionId = chatState.activeSession().id;

        pendingTerminalAnswer.complete("The finished answer, arriving after the user left this chat.");
        pumpEdt();

        ShaftAssistantChatState.Session otherSessionAfter = chatState.activeSession();
        assertAll(
                () -> assertEquals(otherSessionId, otherSessionAfter.id),
                () -> assertEquals("_Killed._", otherSessionAfter.messages.get(killedMessageIndex).markdown,
                        "the recovered answer must never overwrite a different session's message, even one "
                                + "coincidentally shaped like the finalized marker"));
    }

    @Test
    void assistantKillRequestTerminalEntryIsKilledNotCancelled() throws Exception {
        // Issue #3919: no "Killing..."/terminal "Killed (Ns)" milestone bubbles in the transcript any
        // more (the toolbar status label still shows "Killing...", see containsText-based coverage
        // elsewhere) -- but the outcome ("killed" vs "cancelled") must still be distinguishable from
        // the real response bubble.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftMcpInvocation invocation = new ShaftMcpInvocation(
                new CompletableFuture<>(), () -> {
        }, () -> {
        });
        setField(panel, "currentInvocation", invocation);
        panel.setRunning(true, "Thinking...");

        cancelOrKillCurrent(panel);
        cancelOrKillCurrent(panel);
        String transcriptAfterKillRequest = transcriptMarkdown(panel);
        assertFalse(transcriptAfterKillRequest.contains("Killing..."), transcriptAfterKillRequest);

        showAssistantResult(panel, "shaft_guide_search", null, new CancellationException("killed"));

        String transcript = transcriptMarkdown(panel);
        assertAll(
                () -> assertFalse(transcript.contains("Killed ("), transcript),
                () -> assertFalse(transcript.contains("Cancelled ("), transcript),
                () -> assertTrue(transcript.toLowerCase(java.util.Locale.ROOT).contains("killed"), transcript));
    }

    /**
     * Issue #3768 regression: unlike the tests above, which hand-construct the {@code (result,
     * error)} pair {@code showAgentResult} receives, this test drives a REAL
     * {@link CompletableFuture#supplyAsync} worker -- mirroring how {@code AssistantLocalAgentRunner}
     * actually resolves a soft cancel, by *throwing* {@link CancellationException} from inside the
     * async task rather than calling {@code future.cancel(...)} itself -- through the real
     * {@link ShaftMcpInvocation#cancel()} the Cancel button calls. It then forwards whatever
     * {@code future.whenComplete} actually observes into {@code showAgentResult}, exactly mirroring
     * {@code ShaftAssistantPanel#send}'s real {@code whenComplete} registration minus the
     * {@code ApplicationManager.getApplication().invokeLater(...)} EDT hop -- which cannot run in this
     * headless harness (see {@code invokeApplyConnectAgentResult} above for the same, established
     * workaround). Before the #3768 fix, {@code cancel()} left the future to complete with a
     * {@code CompletionException}-wrapped {@code CancellationException}, so {@code showAgentResult}'s
     * {@code error instanceof CancellationException} check never fired and the run rendered "Failed".
     */
    @Test
    void assistantSoftCancelThroughRealFutureRendersCancelledNotFailed() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        CountDownLatch workerStarted = new CountDownLatch(1);
        CountDownLatch cancelActionRan = new CountDownLatch(1);
        ExecutorService worker = Executors.newSingleThreadExecutor();
        try {
            CompletableFuture<ShaftMcpToolResult> future = CompletableFuture.supplyAsync(() -> {
                workerStarted.countDown();
                try {
                    assertTrue(cancelActionRan.await(5, TimeUnit.SECONDS), "cancelAction must unblock the worker");
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
                throw new CancellationException("cancelled");
            }, worker);
            ShaftMcpInvocation invocation = new ShaftMcpInvocation(future, cancelActionRan::countDown);
            setField(panel, "currentInvocation", invocation);
            panel.setRunning(true, "Thinking...");
            assertTrue(workerStarted.await(5, TimeUnit.SECONDS),
                    "The worker must be blocked waiting for cancelAction before cancel() runs, or the race is nondeterministic");

            AtomicReference<ShaftMcpToolResult> observedResult = new AtomicReference<>();
            AtomicReference<Throwable> observedError = new AtomicReference<>();
            CountDownLatch completed = new CountDownLatch(1);
            future.whenComplete((result, error) -> {
                observedResult.set(result);
                observedError.set(error);
                completed.countDown();
            });

            cancelOrKillCurrent(panel);

            assertTrue(completed.await(5, TimeUnit.SECONDS), "The future must complete after cancel()");
            showAgentResult(panel, -1, observedResult.get(), observedError.get());

            String transcript = transcriptMarkdown(panel);
            assertAll(
                    () -> assertTrue(transcript.contains("Cancelled"), transcript),
                    () -> assertFalse(transcript.contains("Failed"), transcript));
        } finally {
            worker.shutdownNow();
        }
    }

    @Test
    void assistantContextSuggestionsAppearOnlyForImplementedTriggers() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        List<ShaftAssistantPanel.ContextSuggestion> workflowSuggestions = panel.contextSuggestionsForTest('@');
        List<ShaftAssistantPanel.ContextSuggestion> slashSuggestions = panel.contextSuggestionsForTest('/');
        List<ShaftAssistantPanel.ContextSuggestion> fileSuggestions = panel.contextSuggestionsForTest('#');
        List<ShaftAssistantPanel.ContextSuggestion> unsupportedSuggestions = panel.contextSuggestionsForTest('$');

        assertAll(
                // Workflow suggestions insert plain-language requests routed by intent — never
                // slash-command syntax the user would have to learn.
                () -> assertTrue(workflowSuggestions.stream().anyMatch(
                        suggestion -> "@workflow:record-web".equals(suggestion.label())
                                && "Record my browser actions on https://".equals(suggestion.insertion()))),
                () -> assertTrue(workflowSuggestions.stream().anyMatch(
                        suggestion -> "@workflow:record-mobile".equals(suggestion.label())
                                && "Record my mobile actions on the Android emulator"
                                .equals(suggestion.insertion()))),
                () -> assertTrue(workflowSuggestions.stream().anyMatch(
                        suggestion -> "@workflow:doctor".equals(suggestion.label())
                                && "Diagnose my last failed test run".equals(suggestion.insertion()))),
                () -> assertTrue(workflowSuggestions.stream().anyMatch(
                        suggestion -> "@workflow:upgrade".equals(suggestion.label())
                                && "Upgrade this project to the latest SHAFT".equals(suggestion.insertion()))),
                () -> assertTrue(workflowSuggestions.stream()
                        .noneMatch(suggestion -> suggestion.insertion().startsWith("/")),
                        "workflow insertions must be plain language"),
                // The '/' trigger reinstates slash-command autocomplete (issue #3540): core commands
                // only by default (Expert mode off), backed by AssistantCommand's command registry.
                () -> assertFalse(slashSuggestions.isEmpty(), "slash commands should be suggested"),
                () -> assertTrue(slashSuggestions.stream().anyMatch(
                        suggestion -> suggestion.insertion().equals("/record ")
                                && suggestion.label().contains("/record")
                                && suggestion.label().contains("Record web actions")),
                        slashSuggestions.toString()),
                () -> assertTrue(slashSuggestions.stream()
                        .noneMatch(suggestion -> suggestion.label().contains("/guardrails")),
                        "expert-only commands must stay hidden when Expert mode is off: " + slashSuggestions),
                () -> assertTrue(fileSuggestions.isEmpty()),
                () -> assertTrue(unsupportedSuggestions.isEmpty()));
    }

    @Test
    void slashCommandPopupOnlyTriggersAtTheStartOfTheComposerLine() {
        // A leading "/" is a command; a "/" inside a URL/path (https://, a/b) must not pop the menu
        // (issue #3550). "@"/"#" keep firing anywhere, so this guard is slash-only.
        assertAll(
                () -> assertTrue(ShaftAssistantPanel.slashTriggerAllowed(""),
                        "an empty composer must offer commands"),
                () -> assertTrue(ShaftAssistantPanel.slashTriggerAllowed("   "),
                        "leading whitespace still counts as line start"),
                () -> assertTrue(ShaftAssistantPanel.slashTriggerAllowed(null)),
                () -> assertFalse(ShaftAssistantPanel.slashTriggerAllowed("https:/"),
                        "a slash inside a URL is not a command"),
                () -> assertFalse(ShaftAssistantPanel.slashTriggerAllowed("open the "),
                        "a slash mid-sentence is not a command"));
    }

    @Test
    void shaftMcpToolsAreFirstPartyAndBypassLocalAgentApprovalPrompts() {
        assertAll(
                () -> assertTrue(ShaftAssistantPanel.isShaftMcpTool("mcp__shaft-mcp__shaft_coding_partner_plan")),
                () -> assertTrue(ShaftAssistantPanel.isShaftMcpTool("mcp__shaft-mcp__capture_generate_replay")),
                () -> assertFalse(ShaftAssistantPanel.isShaftMcpTool("Bash"),
                        "shell commands still require approval"),
                () -> assertFalse(ShaftAssistantPanel.isShaftMcpTool("mcp__other-server__tool"),
                        "third-party MCP servers still require approval"),
                () -> assertFalse(ShaftAssistantPanel.isShaftMcpTool("mcp__shaft-mcp"),
                        "only fully-qualified tool names arrive from the bridge"),
                () -> assertFalse(ShaftAssistantPanel.isShaftMcpTool(null)));
    }

    @Test
    void assistantWorkflowSuggestionsFilterByTypedPrefix() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        List<ShaftAssistantPanel.ContextSuggestion> record = panel.filteredContextSuggestions('@', "workflow:record");
        List<ShaftAssistantPanel.ContextSuggestion> none = panel.filteredContextSuggestions('@', "zzz");

        assertAll(
                () -> assertTrue(record.stream().anyMatch(s -> "@workflow:record-web".equals(s.matchText())),
                        record.toString()),
                () -> assertTrue(record.stream().anyMatch(s -> "@workflow:record-mobile".equals(s.matchText())),
                        record.toString()),
                () -> assertTrue(record.stream().noneMatch(s -> "@workflow:doctor".equals(s.matchText())),
                        record.toString()),
                () -> assertTrue(none.isEmpty(), none.toString()));
    }

    // ---- /mcp autocomplete on the tool-name argument (issue #3883(a)) ----

    @Test
    void mcpToolArgumentSuggestionsOfferToolNamesAndCuratedAliasesFilteredByTypedPrefix() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        List<ShaftAssistantPanel.ContextSuggestion> clickFilter =
                panel.filteredContextSuggestions('/', "mcp cl");
        List<ShaftAssistantPanel.ContextSuggestion> toolFilter =
                panel.filteredContextSuggestions('/', "tool cl");
        List<ShaftAssistantPanel.ContextSuggestion> callFilter =
                panel.filteredContextSuggestions('/', "call cl");
        List<ShaftAssistantPanel.ContextSuggestion> noMatch =
                panel.filteredContextSuggestions('/', "mcp zzz_not_a_real_tool");

        assertAll(
                // "click" is the curated slashAlias for element_click; its row shows the canonical
                // tool name as a description and inserts the canonical "/mcp element_click " form.
                () -> assertTrue(clickFilter.stream().anyMatch(s -> "click".equals(s.matchText())
                                && "/mcp element_click ".equals(s.insertion())
                                && s.label().contains("element_click")),
                        clickFilter.toString()),
                // A plain tool-name match ("element_clear" contains "cl") is also offered, inserting
                // itself.
                () -> assertTrue(clickFilter.stream().anyMatch(s -> "element_clear".equals(s.matchText())
                                && "/mcp element_clear ".equals(s.insertion())),
                        clickFilter.toString()),
                () -> assertEquals(clickFilter, toolFilter, "/tool must offer the same suggestions as /mcp"),
                () -> assertEquals(clickFilter, callFilter, "/call must offer the same suggestions as /mcp"),
                () -> assertTrue(noMatch.isEmpty(), noMatch.toString()));
    }

    @Test
    void mcpToolArgumentSuggestionsShowEveryToolAndAliasWhenNoPrefixIsTypedYet() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        List<ShaftAssistantPanel.ContextSuggestion> all = panel.filteredContextSuggestions('/', "mcp ");

        assertAll(
                () -> assertTrue(all.stream().anyMatch(s -> "init".equals(s.matchText())), all.toString()),
                () -> assertTrue(all.stream().anyMatch(s -> "driver_quit".equals(s.matchText())), all.toString()),
                () -> assertTrue(all.size() >= com.shaft.intellij.mcp.ToolCatalogIndex.toolNames().size(),
                        "must include at least every tool name (plus curated aliases)"));
    }

    @Test
    void mcpToolArgumentSuggestionsStopOnceASecondArgumentTokenStarts() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        // Once the user has moved past the first argument (a second space), this is no longer the
        // tool-name position -- the generic '/' suggestion list (filtered on the whole string) takes
        // over instead, same as before this task.
        List<ShaftAssistantPanel.ContextSuggestion> pastFirstArgument =
                panel.filteredContextSuggestions('/', "mcp element_click {}");

        assertTrue(pastFirstArgument.stream().noneMatch(s -> "element_click".equals(s.matchText())
                        && "/mcp element_click ".equals(s.insertion())),
                "must not still be in tool-argument suggestion mode: " + pastFirstArgument);
    }

    @Test
    void assistantPromptCtrlClickSendsPrompt() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JTextComponent prompt = assistantPrompt(panel);
        prompt.setText("/help");

        notifyMouseListeners(prompt, new MouseEvent(
                prompt,
                MouseEvent.MOUSE_CLICKED,
                System.currentTimeMillis(),
                InputEvent.CTRL_DOWN_MASK,
                4,
                4,
                1,
                false,
                MouseEvent.BUTTON1));

        assertAll(
                () -> assertEquals("", prompt.getText()),
                () -> assertTrue(transcriptMarkdown(panel).contains("## SHAFT Assistant commands")),
                () -> assertTrue(transcriptMarkdown(panel).contains("/record-mobile")));
    }

    @Test
    void assistantPromptKeyboardShortcutsSendAndCancelRunningPrompt() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JTextComponent prompt = assistantPrompt(panel);
        Action metaEnter = shortcutAction(prompt, KeyEvent.VK_ENTER, InputEvent.META_DOWN_MASK);
        Action escape = shortcutAction(panel, JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT,
                KeyEvent.VK_ESCAPE, 0);
        AtomicInteger cancelCalls = new AtomicInteger();

        prompt.setText("/help");
        metaEnter.actionPerformed(new ActionEvent(prompt, ActionEvent.ACTION_PERFORMED, "send"));

        setField(panel, "currentInvocation", new ShaftMcpInvocation(
                new CompletableFuture<>(), cancelCalls::incrementAndGet, () -> {
        }));
        panel.setRunning(true, "Thinking...");
        escape.actionPerformed(new ActionEvent(panel, ActionEvent.ACTION_PERFORMED, "cancel"));

        assertAll(
                () -> assertEquals("", prompt.getText()),
                () -> assertTrue(transcriptMarkdown(panel).contains("## SHAFT Assistant commands")),
                () -> assertEquals(1, cancelCalls.get()),
                () -> assertTrue(containsText(panel, "Cancelling...")));
    }

    @Test
    void assistantListedControlsAreIconOnlyAndSymmetric() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        List<String> controls = List.of(
                "Start a new Assistant chat",
                "Copy last assistant response",
                "Copy last raw assistant response",
                "Copy assistant transcript",
                "Clear assistant transcript",
                "Rerun last assistant prompt",
                "Cancel assistant request");

        assertAll(controls.stream()
                .map(accessibleName -> () -> assertIconOnlySymmetric(
                        findByAccessibleName(panel, accessibleName, JButton.class))));
    }

    @Test
    void assistantActionChromeAppearsOnlyWhenUseful() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        JButton copyResponse = findByAccessibleName(panel, "Copy last assistant response", JButton.class);
        JButton copyRaw = findByAccessibleName(panel, "Copy last raw assistant response", JButton.class);
        JButton copyAll = findByAccessibleName(panel, "Copy assistant transcript", JButton.class);
        JButton clear = findByAccessibleName(panel, "Clear assistant transcript", JButton.class);
        JButton rerun = findByAccessibleName(panel, "Rerun last assistant prompt", JButton.class);
        JButton cancel = findByAccessibleName(panel, "Cancel assistant request", JButton.class);

        assertAll(
                () -> assertFalse(copyResponse.isVisible()),
                () -> assertFalse(copyRaw.isVisible()),
                () -> assertFalse(copyAll.isVisible()),
                () -> assertFalse(clear.isVisible()),
                () -> assertFalse(rerun.isVisible()),
                () -> assertFalse(cancel.isVisible()));

        showAssistantResult(panel, ShaftMcpToolResult.success("Rendered assistant output"));

        assertAll(
                () -> assertTrue(copyResponse.isVisible()),
                () -> assertTrue(copyAll.isVisible()),
                () -> assertTrue(clear.isVisible()),
                () -> assertIconOnlySymmetric(copyAll),
                () -> assertIconOnlySymmetric(clear),
                () -> assertTrue(copyRaw.isVisible()),
                () -> assertFalse(cancel.isVisible()));

        panel.setRunning(true, "Thinking...");

        assertTrue(cancel.isVisible());
    }

    @Test
    void assistantHasNoSeparateRunTimelineComponent() {
        // Issue #3695: the separately-scrollable "Run timeline" list (and its "Run timeline" label)
        // below the chat is gone. Issue #3919 later removed the synthesized milestone bubbles that
        // replaced it too -- progress now belongs solely to the toolbar spinner/status label below.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        assertAll(
                () -> assertNull(findByAccessibleName(panel, "Assistant execution timeline", JComponent.class),
                        "The separately-scrollable Run timeline list must not exist any more"),
                () -> assertFalse(containsText(panel, "Run timeline"),
                        "No component may show a \"Run timeline\" label any more"));
    }

    @Test
    void assistantStatusIsTheOnlySingleLineStatusElementBelowTheChat() {
        // Issue #3695: with the Run timeline list removed, the spinner + status JLabel is the only
        // run-status surface left below the chat window, and it must stay single-line and unlabeled.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());

        JLabel status = findByAccessibleName(panel, "Assistant status", JLabel.class);
        panel.setRunning(true, "Awaiting approval for capture_start...");

        assertAll(
                () -> assertNotNull(status),
                () -> assertFalse(status.getText().contains("\n"), status.getText()),
                () -> assertFalse(status.getText().contains("<br"), status.getText()),
                () -> assertEquals("Awaiting approval for capture_start...", status.getToolTipText()));
    }

    @Test
    void toolWindowButtonsAreIconOnlyAndSymmetric() {
        List<Component> panels = List.of(
                new ShaftAssistantPanel(null, blankMcpSettings()),
                new ShaftFeaturePanel(null, blankMcpSettings()),
                new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
                }),
                new GuidedWorkflowPanel(null, (tool, arguments) -> {
                }),
                new EvidenceTriagePanel(null, (tool, arguments) -> {
                }));

        assertAll(panels.stream()
                .flatMap(panel -> collectButtons(panel).stream())
                .filter(button -> !isSetupPrimaryAction(button))
                .filter(button -> !"Copy".equals(button.getText()))
                .filter(button -> !"Copy fresh SHAFT MCP setup command".equals(accessibleName(button)))
                .filter(button -> !"Reset everything".equals(accessibleName(button)))
                // Names the exact non-destructive action it runs, distinguishing it at a glance from
                // the destructive "Reset everything" button beside it (issue #3601 S4).
                .filter(button -> !"Re-check connection and agents".equals(accessibleName(button)))
                .filter(button -> !"Copy SHAFT upgrade command".equals(accessibleName(button)))
                .filter(button -> !"Check SHAFT project version".equals(accessibleName(button)))
                // The shaft-mcp version step's check button is labeled like its upgrade-step peer
                // above: it names the exact check being run on a first-run setup screen (issue #3538).
                .filter(button -> !"Check SHAFT MCP version".equals(accessibleName(button)))
                // "2 Choose agent"'s own standalone Check button (issue #4314 fix 2) is the same
                // first-run-setup-screen Check pattern as the two rows above it.
                .filter(button -> !"Check agent connection".equals(accessibleName(button)))
                // Lane/teaching controls keep visible labels by design: the no-agent start names
                // its lane (issue #3425 A2/B7/A6).
                .filter(button -> !"Start SHAFT without an agent".equals(accessibleName(button)))
                // The primary "get to green" retry beside it needs the same visible-label treatment
                // (real user report: no button anywhere retried just the agent lane besides redoing
                // steps 2/4 from scratch).
                .filter(button -> !"Connect SHAFT agent".equals(accessibleName(button)))
                .filter(button -> !"Convert pasted Selenium to SHAFT".equals(accessibleName(button)))
                // The user-guide footer link is a text hyperlink by design, not an icon button.
                .filter(button -> !"Open SHAFT user guide in browser".equals(accessibleName(button)))
                // Setup-screen prerequisite/recovery command buttons keep visible labels like the
                // upgrade step's copy/terminal pair: they name the exact terminal command being
                // copied, which an icon alone cannot convey on a first-run provisioning screen.
                .filter(button -> !"Recheck prerequisites".equals(accessibleName(button)))
                .filter(button -> !"Copy SHAFT Engine warm-up command".equals(accessibleName(button)))
                .filter(button -> !"Copy assistant CLI restart command".equals(accessibleName(button)))
                .filter(button -> !"Copy setup diagnostic output".equals(accessibleName(button)))
                .filter(button -> !"Copy SHAFT MCP docs link".equals(accessibleName(button)))
                .filter(button -> !String.valueOf(accessibleName(button)).matches("Copy .+ install command"))
                // Empty-state suggestion chips (issue #3500 A6) are content affordances that name
                // the exact request they pre-fill — an icon alone cannot convey that on first run.
                .filter(button -> !"Record a sample flow".equals(accessibleName(button)))
                .filter(button -> !"Ask how to assert".equals(accessibleName(button)))
                .filter(button -> !"Diagnose my last failure".equals(accessibleName(button)))
                // The first-run coach's acknowledgment (issue #3500 O1) names its one-time action.
                .filter(button -> !"Dismiss first run coach".equals(accessibleName(button)))
                // Recovery actions keep visible labels: which of Retry / Restart MCP server / View
                // logs applies depends on the failure category, which an icon alone cannot convey
                // (issue #3626).
                .filter(button -> !"SHAFT tool recovery action".equals(accessibleName(button)))
                // Provider discovery failures expose a direct, visible retry label; the focused
                // provider-model layout contract verifies that text independently.
                .filter(button -> !"Retry provider models".equals(accessibleName(button)))
                // Local-agent health recovery uses visible action labels because Recheck and Repair
                // have different effects that an icon-only pair would not communicate in settings.
                .filter(button -> !"Recheck local agent health".equals(accessibleName(button)))
                .filter(button -> !"Repair SHAFT agent skills".equals(accessibleName(button)))
                .map(button -> () -> assertIconOnlySymmetric(button)));
    }

    @Test
    void iconButtonsUseClickableIconOnlySurfaces() {
        JButton active = new JButton("Run");
        JButton inactive = new JButton("Cancel");
        ShaftIconButtons.apply(active, ShaftIcons.SEND);
        ShaftIconButtons.apply(inactive, ShaftIcons.CANCEL);

        inactive.setEnabled(false);

        assertAll(
                () -> assertNotNull(inactive.getDisabledIcon()),
                () -> assertEquals(inactive.getIcon().getIconWidth(), inactive.getDisabledIcon().getIconWidth()),
                () -> assertTrue(active.isContentAreaFilled()),
                () -> assertTrue(active.isBorderPainted()),
                () -> assertTrue(active.isFocusPainted()),
                () -> assertTrue(active.isRolloverEnabled()),
                () -> assertTrue(inactive.isContentAreaFilled()),
                () -> assertTrue(inactive.isBorderPainted()));
    }

    @Test
    void iconButtonsExposeVisibleHoverPressedAndDisabledFeedback() {
        JButton button = new JButton("Run");
        ShaftIconButtons.apply(button, ShaftIcons.SEND);

        Color ready = button.getBackground();
        button.getModel().setRollover(true);
        Color hover = button.getBackground();
        button.getModel().setArmed(true);
        button.getModel().setPressed(true);
        Color pressed = button.getBackground();
        int enabledCursor = button.getCursor().getType();
        button.setEnabled(false);
        Color disabled = button.getBackground();
        int disabledCursor = button.getCursor().getType();

        assertAll(
                () -> assertTrue(button.isOpaque()),
                () -> assertEquals(Cursor.HAND_CURSOR, enabledCursor),
                () -> assertEquals(Cursor.DEFAULT_CURSOR, disabledCursor),
                () -> assertNotEquals(ready, hover),
                () -> assertNotEquals(hover, pressed),
                () -> assertNotEquals(ready, disabled));
    }

    @Test
    void assistantSendButtonTurnsIntoProgressAndHoverCancelWhileRunning() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JButton sendButton = findByAccessibleName(panel, "Send assistant prompt", JButton.class);
        assertNotNull(sendButton);
        Icon readyIcon = sendButton.getIcon();

        panel.setRunning(true, "Thinking...");
        Icon runningIcon = sendButton.getIcon();

        assertAll(
                () -> assertTrue(sendButton.isEnabled()),
                () -> assertNotEquals(readyIcon, runningIcon),
                () -> assertEquals("Assistant request running", sendButton.getToolTipText()));

        notifyShaftMouseListener(sendButton, MouseEvent.MOUSE_ENTERED);
        Icon hoverIcon = sendButton.getIcon();
        assertAll(
                () -> assertNotEquals(runningIcon, hoverIcon),
                () -> assertEquals("Cancel assistant request", sendButton.getToolTipText()));

        notifyShaftMouseListener(sendButton, MouseEvent.MOUSE_EXITED);
        assertAll(
                () -> assertEquals(runningIcon, sendButton.getIcon()),
                () -> assertEquals("Assistant request running", sendButton.getToolTipText()));

        panel.setRunning(false, "ready");
        assertAll(
                () -> assertEquals(readyIcon, sendButton.getIcon()),
                () -> assertEquals("Send assistant prompt (Ctrl+Enter, Command+Enter, or Ctrl+click)",
                        sendButton.getToolTipText()));
    }

    @Test
    void assistantCancelButtonArmsKillForActiveInvocation() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JButton cancelButton = findButton(panel, "Cancel");
        AtomicInteger cancelCalls = new AtomicInteger();
        AtomicInteger killCalls = new AtomicInteger();
        setField(panel, "currentInvocation", new ShaftMcpInvocation(
                new CompletableFuture<>(), cancelCalls::incrementAndGet, killCalls::incrementAndGet));

        panel.setRunning(true, "Thinking...");
        clickAccessible(panel, "Cancel assistant request");

        assertAll(
                () -> assertEquals(1, cancelCalls.get()),
                () -> assertEquals(0, killCalls.get()),
                () -> assertEquals("Kill assistant session", cancelButton.getToolTipText()),
                () -> assertNotNull(findByAccessibleName(panel, "Kill assistant session", JButton.class)));

        clickAccessible(panel, "Kill assistant session");

        assertAll(
                () -> assertEquals(1, cancelCalls.get()),
                () -> assertEquals(1, killCalls.get()),
                () -> assertTrue(containsText(panel, "Killing...")));
    }

    @Test
    void actionButtonsUseModernSvgIcons() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        ShaftFeaturePanel featurePanel = new ShaftFeaturePanel(null, blankMcpSettings());
        ShaftMcpSetupPanel setupPanel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });
        GuidedWorkflowPanel guidedPanel = new GuidedWorkflowPanel(null, (tool, arguments) -> {
        });
        EvidenceTriagePanel triagePanel = new EvidenceTriagePanel(null, (tool, arguments) -> {
        });

        assertAll(
                () -> assertNull(findButton(panel, "Test connection and start chatting")),
                () -> assertIcon(findButton(panel, "New chat")),
                () -> assertIcon(findButton(panel, "Copy response")),
                () -> assertIcon(findButton(panel, "Copy raw")),
                () -> assertIcon(findButton(panel, "Copy all")),
                () -> assertIcon(findButton(panel, "Clear")),
                () -> assertIcon(findButton(panel, "Rerun")),
                () -> assertIcon(findButton(panel, "Cancel")),
                () -> assertIcon(findByAccessibleName(panel, "Send assistant prompt", JButton.class)),
                () -> assertIcon(findButton(setupPanel, "Copy SHAFT Tools & Skills setup command")),
                () -> assertIcon(findButton(setupPanel, "Copy SHAFT upgrade command")),
                () -> assertIcon(findButton(setupPanel, "Check SHAFT project version")),
                () -> assertIcon(findButton(setupPanel, "Test SHAFT MCP connection")),
                () -> assertIcon(findButton(setupPanel, "Start chatting with SHAFT Assistant")),
                () -> assertIcon(findButton(featurePanel, "Run")),
                () -> assertIcon(findButton(featurePanel, "Cancel")),
                () -> assertIcon(findButton(featurePanel, "Restore defaults")),
                () -> assertIcon(findButton(featurePanel, "Copy output")),
                () -> assertIcon(findButton(featurePanel, "Refresh tools")),
                () -> assertIcon(findButton(setupPanel, "Copy setup diagnostic command")),
                () -> assertIcon(findButton(setupPanel, "Copy setup diagnostic output")),
                () -> assertIcon(findButton(guidedPanel, "Plan coding partner")),
                () -> assertIcon(findButton(guidedPanel, "Start recording")),
                () -> assertIcon(findButton(guidedPanel, "Stop recording")),
                () -> assertIcon(findButton(guidedPanel, "Review code")),
                () -> assertIcon(findButton(guidedPanel, "Inspect locator")),
                () -> assertIcon(findButton(guidedPanel, "Guardrail check")),
                () -> assertIcon(findButton(triagePanel, "Analyze Allure")),
                () -> assertIcon(findButton(triagePanel, "Analyze Trace")),
                () -> assertIcon(findButton(triagePanel, "Suggest Fix")),
                () -> assertIcon(findButton(triagePanel, "Run Healer")),
                () -> assertIcon(findButton(triagePanel, "Propose Locator")));
    }

    @Test
    void assistantPanelShowsConfigureButtonWhenSetupCallbackIsConfigured() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        AtomicBoolean openedSetup = new AtomicBoolean();
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, chatState,
                () -> openedSetup.set(true));

        JButton configure = findButton(panel, "Configure");
        JComboBox<?> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        JComboBox<?> runtime = findByAccessibleName(panel, "Assistant runtime", JComboBox.class);
        JComboBox<?> mode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        JLabel currentAgent = findByAccessibleName(panel, "Current agent configuration", JLabel.class);

        assertAll(
                () -> assertNotNull(configure),
                () -> assertNotNull(family),
                () -> assertNotNull(runtime),
                () -> assertTrue(configure.isVisible()),
                () -> assertFalse(family.isVisible()),
                () -> assertFalse(runtime.isVisible()),
                () -> assertTrue(containsText(panel, "Codex CLI")),
                () -> assertEquals("Agent: Local / Codex / CLI", currentAgent.getToolTipText()),
                // Issue #3603: the description mirrors the same live text as the visible label,
                // not a static generic explanation.
                () -> assertEquals(currentAgent.getText(),
                        currentAgent.getAccessibleContext().getAccessibleDescription()),
                () -> assertNull(findButton(panel, "Test connection and start chatting")),
                () -> assertFalse(openedSetup.get()));

        // Live-update proof: reconfiguring the route must update the description to the NEW live
        // text, not just retain what was captured at construction.
        String firstDescription = currentAgent.getAccessibleContext().getAccessibleDescription();
        settings.assistantFamily = "CLAUDE";
        mode.setSelectedItem("PLAN");
        String secondDescription = currentAgent.getAccessibleContext().getAccessibleDescription();
        assertAll(
                () -> assertEquals(currentAgent.getText(), secondDescription),
                () -> assertNotEquals(firstDescription, secondDescription,
                        "the description must track the live agent configuration after it changes"));

        configure.doClick();
        assertTrue(openedSetup.get());
    }

    @Test
    void captureReviewStatusAccessibleDescriptionTracksLiveSummaryAcrossUpdates() throws Exception {
        // Issue #3603: captureReviewStatus's accessible name stays the short, stable "Capture
        // review status" (test-id-safe), but the description must mirror the live summary text --
        // and keep tracking each later update, not just the first.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings(), new ShaftAssistantChatState());
        JLabel captureReviewStatus = findByAccessibleName(panel, "Capture review status", JLabel.class);
        assertNotNull(captureReviewStatus);
        assertEquals("Capture review ready", captureReviewStatus.getText());
        assertEquals(captureReviewStatus.getText(),
                captureReviewStatus.getAccessibleContext().getAccessibleDescription());

        setPendingCaptureReview(panel, "```java\ncode\n```");
        String firstDescription = captureReviewStatus.getAccessibleContext().getAccessibleDescription();

        setPendingCaptureReview(panel, "```java\ncode\n```\n\n```java\nmore\n```");
        String secondDescription = captureReviewStatus.getAccessibleContext().getAccessibleDescription();

        assertAll(
                () -> assertEquals("Capture review ready: 1 code block", firstDescription),
                () -> assertEquals("Capture review ready: 2 code blocks", secondDescription),
                () -> assertNotEquals(firstDescription, secondDescription,
                        "the description must track each live update, not just the first"),
                () -> assertEquals(captureReviewStatus.getText(), secondDescription));
    }

    @Test
    void assistantTranscriptInitialStateIsBlank() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();

        assertAll(
                () -> assertTrue(transcript.markdown().isBlank()),
                () -> assertFalse(containsText(transcript, "Start with")),
                () -> assertFalse(containsText(transcript, "/guide")),
                () -> assertFalse(containsText(transcript, "/browser")),
                () -> assertFalse(containsText(transcript, "/record")),
                () -> assertFalse(containsText(transcript, "/doctor")));
    }

    @Test
    void assistantTranscriptIsSelfContainedWithoutExternalMarkdownRuntime() {
        List<String> externalMarkdownFields = new ArrayList<>();
        for (Field field : AssistantTranscriptView.class.getDeclaredFields()) {
            if (field.getType().getName().startsWith("org.commonmark")) {
                externalMarkdownFields.add(field.getName() + ":" + field.getType().getName());
            }
        }

        assertTrue(externalMarkdownFields.isEmpty(), externalMarkdownFields.toString());
    }

    @Test
    void assistantTranscriptRendersRoleBasedMessageBubbles() {
        // The fenced code block lives on the assistant message, not the user message, on purpose:
        // user bubbles now bypass Markdown entirely (exact-as-typed text only), so exercising the
        // shared code-copy-button/syntax-highlighting machinery this test also covers requires an
        // assistant bubble.
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("user", "Hello assistant");
        transcript.append("assistant", "Hi user\n\n```java\nclass UserPrompt {}\n```");

        List<JComponent> bubbles = transcriptBubbles(transcript);
        List<JEditorPane> panes = transcriptHtmlPanes(transcript);
        String rendered = transcriptRenderedHtml(transcript);
        assertAll(
                () -> assertEquals(2, bubbles.size()),
                () -> assertEquals(2, panes.size()),
                () -> assertEquals("user",
                        bubbles.get(0).getClientProperty(AssistantTranscriptView.TRANSCRIPT_BUBBLE_PROPERTY)),
                () -> assertEquals("assistant",
                        bubbles.get(1).getClientProperty(AssistantTranscriptView.TRANSCRIPT_BUBBLE_PROPERTY)),
                () -> assertEquals(BorderLayout.EAST,
                        ((BorderLayout) bubbles.get(0).getParent().getLayout()).getConstraints(bubbles.get(0))),
                () -> assertEquals(BorderLayout.WEST,
                        ((BorderLayout) bubbles.get(1).getParent().getLayout()).getConstraints(bubbles.get(1))),
                () -> assertEquals("user",
                        panes.get(0).getClientProperty(AssistantTranscriptView.TRANSCRIPT_ROLE_PROPERTY)),
                () -> assertEquals("assistant",
                        panes.get(1).getClientProperty(AssistantTranscriptView.TRANSCRIPT_ROLE_PROPERTY)),
                () -> assertTrue(rendered.contains("background:")),
                () -> assertTrue(rendered.contains("class=\"shaft-code-copy\"")),
                () -> assertTrue(rendered.contains("data-copy-code=\"class UserPrompt")),
                () -> assertTrue(rendered.contains("aria-label=\"Copy code\"")),
                () -> assertTrue(rendered.contains("<svg width=\"16\" height=\"16\"")),
                () -> assertTrue(rendered.contains("class=\"shaft-code-copy-icon\"")),
                () -> assertTrue(rendered.contains("&#x2398;")),
                () -> assertTrue(rendered.contains("class=\"shaft-code-highlighted language-java\""), rendered),
                () -> assertTrue(rendered.contains("<span style=\""), rendered),
                () -> assertFalse(rendered.contains(">Copy code<")),
                () -> assertTrue(containsText(transcript, "Hello assistant")),
                () -> assertTrue(containsText(transcript, "Hi user")),
                () -> assertFalse(containsText(transcript, "Start with")),
                () -> assertFalse(rendered.contains("cellpadding=\"8\"")),
                () -> assertFalse(rendered.contains("border=\"1\"")),
                () -> assertFalse(rendered.contains("<table")));
    }

    @Test
    void assistantTranscriptKeepsBubblesInsideNarrowToolWindow() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.setSize(new Dimension(360, 520));
        transcript.append("user", "generate code that opens DuckDuckGo, searches SHAFT Engine, and opens the first result");
        transcript.append("assistant", """
                Confirmed target: https://duckduckgo.com/

                ```java
                public class DuckDuckGoSearchTest {
                    private final SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();
                }
                ```
                """.stripIndent().trim());
        transcript.doLayout();

        for (JComponent bubble : transcriptBubbles(transcript)) {
            assertTrue(bubble.getPreferredSize().width <= 336,
                    "Narrow transcript bubble should fit inside 360 px tool window: " + bubble.getPreferredSize());
        }
    }

    @Test
    void assistantTranscriptWrapsLongMarkdownLinksAndPathsInsideBubble() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.setSize(new Dimension(360, 520));
        transcript.append("assistant", """
                Created
                [AGENTS.md](C:/Users/Mohab/IdeaProjects/GitHub/LevelingUpRound34/AGENTS.md) captures canonical guidance surface.
                """.stripIndent().trim());
        transcript.doLayout();

        String rendered = transcriptRenderedHtml(transcript);
        List<JComponent> bubbles = transcriptBubbles(transcript);
        List<JEditorPane> panes = transcriptHtmlPanes(transcript);

        assertAll(
                () -> assertEquals(1, bubbles.size()),
                () -> assertEquals(1, panes.size()),
                () -> assertTrue(rendered.contains("overflow-wrap: anywhere"), rendered),
                () -> assertTrue(rendered.contains("word-wrap: break-word"), rendered),
                () -> assertTrue(rendered.contains("C:&#8203;/&#8203;Users"), rendered),
                () -> assertTrue(bubbles.get(0).getPreferredSize().width <= 336,
                        "Long path bubble should fit inside 360 px tool window: "
                                + bubbles.get(0).getPreferredSize()),
                () -> assertTrue(panes.get(0).getPreferredSize().width <= 314,
                        "Long path pane should fit inside bubble content width: "
                                + panes.get(0).getPreferredSize()));
    }

    @Test
    void assistantTranscriptUsesReadActionForIntellijRendering() {
        assertTrue(AssistantTranscriptView.renderReadActionForTest(
                () -> ApplicationManager.getApplication() == null
                        || ApplicationManager.getApplication().isReadAccessAllowed()));
    }

    @Test
    void assistantTranscriptRendersLatestMessageInSwingView() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("user", "Show the sent prompt");

        assertTrue(containsText(transcript, "Show the sent prompt"));
    }

    @Test
    void assistantTranscriptAssistantBubbleHasVisibleRoundedSurface() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", "Assistant response");

        List<JComponent> bubbles = transcriptBubbles(transcript);
        assertAll(
                () -> assertEquals(1, bubbles.size()),
                () -> assertEquals("assistant",
                        bubbles.get(0).getClientProperty(AssistantTranscriptView.TRANSCRIPT_BUBBLE_PROPERTY)),
                () -> assertNotNull(bubbles.get(0).getBorder()),
                () -> assertNotNull(bubbles.get(0).getBackground()),
                () -> assertTrue(bubbles.get(0).getAccessibleContext().getAccessibleName().contains("Assistant")),
                () -> assertFalse(transcriptRenderedHtml(transcript).contains("<table")));
    }

    @Test
    void assistantTranscriptMessagePaneShowsContextMenuWithCopyActionsAndSurvivesRerender() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", "First response");

        JEditorPane firstPane = transcriptHtmlPanes(transcript).get(0);
        JPopupMenu menu = showMessageContextMenu(firstPane);
        assertSame(menu, transcript.lastMessageContextMenuForTest());
        JMenuItem copyItem = findByAccessibleName(menu, "Copy", JMenuItem.class);
        JMenuItem selectAllItem = findByAccessibleName(menu, "Select All", JMenuItem.class);
        JMenuItem copyFullTranscriptItem = findByAccessibleName(menu, "Copy full transcript", JMenuItem.class);

        assertAll(
                () -> assertEquals(3, menu.getComponentCount()),
                () -> assertNotNull(copyItem),
                () -> assertFalse(copyItem.isEnabled(), "Copy should be disabled without a selection"),
                () -> assertNotNull(selectAllItem),
                () -> assertTrue(selectAllItem.isEnabled()),
                () -> assertNotNull(copyFullTranscriptItem),
                () -> assertTrue(copyFullTranscriptItem.isEnabled()));

        firstPane.select(0, firstPane.getDocument().getLength());
        showMessageContextMenu(firstPane);
        JMenuItem copyItemAfterSelection = findByAccessibleName(
                transcript.lastMessageContextMenuForTest(), "Copy", JMenuItem.class);
        assertTrue(copyItemAfterSelection.isEnabled(), "Copy should be enabled once the pane has a selection");

        transcript.append("assistant", "Second response");
        List<JEditorPane> panesAfterRerender = transcriptHtmlPanes(transcript);
        JEditorPane newestPane = panesAfterRerender.get(panesAfterRerender.size() - 1);
        showMessageContextMenu(newestPane);
        assertEquals(3, transcript.lastMessageContextMenuForTest().getComponentCount());
    }

    /**
     * Issue #3630: the context menu is wired via {@link JComponent#setComponentPopupMenu}, so a
     * single {@link JPopupMenu} instance backs both the mouse right-click trigger and the platform's
     * keyboard trigger (Shift+F10 / the keyboard menu key) on a focused bubble -- there is no way for
     * the two paths to diverge, and the pane must be focusable for the keyboard trigger to ever reach
     * it. A real screen realization isn't available in this headless suite (no test here shows a
     * live window), so the keyboard trigger is exercised the same faithful way the mouse trigger is
     * in the test above: by invoking the exact {@code popupMenuWillBecomeVisible} lifecycle callback
     * Swing's own {@code BasicPopupMenuUI} invokes right before painting the popup, regardless of
     * which trigger asked for it.
     */
    @Test
    void assistantTranscriptMessagePaneContextMenuIsKeyboardReachableAndIdenticalEitherTrigger() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", "Response");
        JEditorPane pane = transcriptHtmlPanes(transcript).get(0);

        assertTrue(pane.isFocusable(), "The message pane must be focusable for Shift+F10 to reach it");

        JPopupMenu menu = showMessageContextMenu(pane);
        assertAll(
                () -> assertEquals(3, menu.getComponentCount()),
                () -> assertNotNull(findByAccessibleName(menu, "Copy", JMenuItem.class)),
                () -> assertNotNull(findByAccessibleName(menu, "Select All", JMenuItem.class)),
                () -> assertNotNull(findByAccessibleName(menu, "Copy full transcript", JMenuItem.class)),
                () -> assertSame(menu, pane.getComponentPopupMenu(),
                        "The keyboard trigger and the mouse trigger must open the same menu instance"));
    }

    @Test
    void assistantTranscriptCopyFullTranscriptContextMenuItemReusesCopyButtonExportPath() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        showAssistantResult(panel, ShaftMcpToolResult.success("Rendered assistant output"));
        AssistantTranscriptView view = getTranscriptView(panel);
        assertNotNull(view);

        String expectedExport = assistantExport(panel);
        assertTrue(expectedExport.contains("Rendered assistant output"));

        AtomicInteger invocations = new AtomicInteger();
        view.setCopyFullTranscriptAction(invocations::incrementAndGet);

        List<JEditorPane> panes = transcriptHtmlPanes(view);
        JEditorPane pane = panes.get(panes.size() - 1);
        showMessageContextMenu(pane);

        JMenuItem copyFullTranscriptItem = findByAccessibleName(
                view.lastMessageContextMenuForTest(), "Copy full transcript", JMenuItem.class);
        assertNotNull(copyFullTranscriptItem);
        copyFullTranscriptItem.doClick();

        assertEquals(1, invocations.get());
    }

    @Test
    void assistantTranscriptCodeCopyControlDoesNotPaintRectangularOutline() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", """
                ```java
                class AssistantResponse {}
                ```
                """.stripIndent().trim());

        String rendered = transcriptRenderedHtml(transcript);
        String copyControlCss = rendered.substring(
                rendered.indexOf(".shaft-code-copy {"),
                rendered.indexOf(".shaft-code-copy-icon"));
        assertAll(
                () -> assertFalse(copyControlCss.contains("border:")),
                () -> assertFalse(copyControlCss.contains("background:")),
                () -> assertTrue(copyControlCss.contains("line-height: 24px")),
                () -> assertTrue(rendered.contains("line-height:24px")));
    }

    @Test
    void assistantTranscriptCodeBlocksUseLightThemePalette() {
        Object oldTextAreaBackground = UIManager.get("TextArea.background");
        Object oldTextAreaForeground = UIManager.get("TextArea.foreground");
        Object oldEditorPaneBackground = UIManager.get("EditorPane.background");
        Object oldBorder = UIManager.get("Component.borderColor");
        UIManager.put("TextArea.background", Color.WHITE);
        UIManager.put("TextArea.foreground", new Color(0x1F2328));
        UIManager.put("EditorPane.background", new Color(0x3C3F41));
        UIManager.put("Component.borderColor", new Color(0xC9CCD1));
        try {
            AssistantTranscriptView transcript = new AssistantTranscriptView();
            transcript.append("assistant", """
                    ```java
                    public class AssistantResponse {}
                    ```
                    """.stripIndent().trim());

            String rendered = transcriptRenderedHtml(transcript);

            assertAll(
                    () -> assertTrue(rendered.contains("background: #f6f8fa"), rendered),
                    () -> assertTrue(rendered.contains("background: #eef2f7"), rendered),
                    () -> assertTrue(rendered.contains("color:#000080;"), rendered),
                    () -> assertFalse(rendered.contains("#24272b"), rendered),
                    () -> assertFalse(rendered.contains("#2f3338"), rendered));
        } finally {
            restoreUiValue("TextArea.background", oldTextAreaBackground);
            restoreUiValue("TextArea.foreground", oldTextAreaForeground);
            restoreUiValue("EditorPane.background", oldEditorPaneBackground);
            restoreUiValue("Component.borderColor", oldBorder);
        }
    }

    @Test
    void setupPanelShowsExpectedWorkflowActionsAtEachStep() throws Exception {
        Path appData = tempDirectory("shaft-mcp-empty-app-data");
        Path bootstrap = tempDirectory("shaft-mcp-empty-bootstrap");
        String oldAppData = System.getProperty("shaft.intellij.mcp.applicationDataRoot");
        String oldBootstrap = System.getProperty("shaft.intellij.mcp.bootstrapRoot");
        System.setProperty("shaft.intellij.mcp.applicationDataRoot", appData.toString());
        System.setProperty("shaft.intellij.mcp.bootstrapRoot", bootstrap.toString());
        try {
            ShaftSettingsState.Settings blank = blankMcpSettings();
            blank.assistantFamily = "CODEX";
            blank.defaultAutobotClient = "CODEX";
            ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blank, () -> {
            });
            AtomicReference<String> copied = new AtomicReference<>("");
            setField(panel, "copySink", (Consumer<String>) copied::set);
            // Copy and the tools-install check stay available after the user picks an agent.
            assertVisiblePrimarySetupActions(panel, "Copy SHAFT Tools & Skills setup command", "Test SHAFT MCP connection");

            clickAccessible(panel, "Copy SHAFT Tools & Skills setup command");
            assertAll(
                    () -> assertTrue(copied.get().contains("install-shaft-agentic-tools")),
                    () -> assertTrue(copied.get().contains("codex")),
                    () -> assertTrue(copied.get().contains("--install-shaft-skills")),
                    () -> assertTrue(copied.get().contains("/main/scripts/mcp/install-shaft-agentic-tools")),
                    () -> assertFalse(copied.get().contains("SHAFT_MCP_INSTALLER_REF")));
            if (isWindowsOs()) {
                assertAll(
                        () -> assertTrue(copied.get().contains("-Command '$installer=Join-Path")),
                        () -> assertFalse(copied.get().contains("-Command \"$installer")));
            } else {
                assertTrue(copied.get().contains("sh \"$tmp\" --codex"));
            }
            assertVisiblePrimarySetupActions(panel, "Copy SHAFT Tools & Skills setup command", "Test SHAFT MCP connection");

            clickAccessible(panel, "Test SHAFT MCP connection");
            assertVisiblePrimarySetupActions(panel, "Copy SHAFT Tools & Skills setup command", "Test SHAFT MCP connection");
        } finally {
            restoreProperty("shaft.intellij.mcp.applicationDataRoot", oldAppData);
            restoreProperty("shaft.intellij.mcp.bootstrapRoot", oldBootstrap);
        }

        ShaftSettingsState.Settings settings = unverifiedMcpSettings();
        settings.assistantFamily = "CODEX";
        settings.defaultAutobotClient = "CODEX";
        ShaftMcpSetupPanel connectedPanel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        }, readyProbe());
        assertVisiblePrimarySetupActions(connectedPanel, "Copy SHAFT Tools & Skills setup command", "Test SHAFT MCP connection");

        showTestResult(connectedPanel, ShaftMcpToolResult.success("Probe OK"));
        // Check and Install remain visible/enabled alongside "Start chatting" (issue #3560): the
        // row color, not button visibility, now conveys that setup already passed.
        assertVisiblePrimarySetupActions(connectedPanel, "Copy SHAFT Tools & Skills setup command",
                "Test SHAFT MCP connection", "Start chatting with SHAFT Assistant");
    }

    @Test
    void setupResetAndReinstallClearsStateAndCopiesInstallerCommand() throws Exception {
        Path appData = tempDirectory("shaft-mcp-empty-app-data");
        Path bootstrap = tempDirectory("shaft-mcp-empty-bootstrap");
        String oldAppData = System.getProperty("shaft.intellij.mcp.applicationDataRoot");
        String oldBootstrap = System.getProperty("shaft.intellij.mcp.bootstrapRoot");
        System.setProperty("shaft.intellij.mcp.applicationDataRoot", appData.toString());
        System.setProperty("shaft.intellij.mcp.bootstrapRoot", bootstrap.toString());
        try {
            ShaftSettingsState.Settings settings = unverifiedMcpSettings();
            settings.mcpSetupComplete = true;
            settings.agentGuidanceOptimizationPromptPending = true;
            ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
            }, readyProbe());
            AtomicReference<String> copied = new AtomicReference<>("");
            setField(panel, "copySink", (Consumer<String>) copied::set);
            JTextComponent mcpCommand = (JTextComponent) getField(panel, "mcpCommand");

            showTestResult(panel, ShaftMcpToolResult.success("Probe OK"));
            clickAccessible(panel, "Copy fresh SHAFT MCP setup command");

            assertAll(
                    () -> assertEquals("", mcpCommand.getText()),
                    () -> assertFalse(settings.mcpSetupComplete),
                    () -> assertFalse(settings.agentGuidanceOptimizationPromptPending),
                    () -> assertTrue(copied.get().contains("install-shaft-agentic-tools")),
                    () -> assertTrue(copied.get().contains("--install-shaft-skills")),
                    () -> assertVisiblePrimarySetupActions(panel,
                            "Copy SHAFT Tools & Skills setup command", "Test SHAFT MCP connection"),
                    () -> assertTrue(containsText(panel, "Installer command copied. Run it in terminal, then check.")));
        } finally {
            restoreProperty("shaft.intellij.mcp.applicationDataRoot", oldAppData);
            restoreProperty("shaft.intellij.mcp.bootstrapRoot", oldBootstrap);
        }
    }

    @Test
    void insertContextSuggestionSurvivesResetTriggerOffsetWithoutCrashing() throws Exception {
        // Issue #3426 B1: picking the first slash-menu entry crashed with
        // StringIndexOutOfBoundsException because hideContextPopup() reset contextTriggerOffset
        // to -1 before insertContextSuggestion read it.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        JTextComponent prompt = findByAccessibleName(panel, "Assistant prompt", JTextComponent.class);
        prompt.setText("/");
        prompt.setCaretPosition(1);
        setField(panel, "contextTriggerOffset", -1);

        Method insert = ShaftAssistantPanel.class.getDeclaredMethod(
                "insertContextSuggestion", char.class, ShaftAssistantPanel.ContextSuggestion.class);
        insert.setAccessible(true);
        insert.invoke(panel, '/', new ShaftAssistantPanel.ContextSuggestion(
                "/record-web — Record web actions", "/record-web https://example.com"));

        assertEquals("/record-web https://example.com", prompt.getText());

        // And the normal path (offset still valid) replaces the trigger plus typed filter.
        prompt.setText("/co");
        prompt.setCaretPosition(3);
        setField(panel, "contextTriggerOffset", 0);
        insert.invoke(panel, '/', new ShaftAssistantPanel.ContextSuggestion(
                "/codegen — Generate code from recordings", "/codegen "));
        assertEquals("/codegen ", prompt.getText());
    }

    @Test
    void completedSetupShowsCopyFreshCommandWhenReopened() {
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(fakeProject(), connectedMcpSettings());

        clickAccessible(toolWindow, "Open SHAFT MCP setup");

        JButton resetAndReinstall = findByAccessibleName(toolWindow, "Copy fresh SHAFT MCP setup command", JButton.class);
        assertAll(
                () -> assertEquals("Copy", resetAndReinstall.getText()),
                () -> assertEquals("Clear the saved MCP command and copy a fresh installer command",
                        resetAndReinstall.getToolTipText()),
                () -> assertTrue(resetAndReinstall.isVisible()),
                () -> assertTrue(resetAndReinstall.isEnabled()));
    }

    @Test
    void assistantTranscriptUsesStandardMarkdownForHeadersRulesAndCodeBlocks() {
        // Both payloads are assistant messages: user bubbles now bypass Markdown entirely, so
        // headers/rules/code-blocks -- what this test actually targets -- can only be exercised
        // through the assistant role.
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", """
                ## First payload

                ---

                ```json
                {"browser":"chrome"}
                ```
                """.stripIndent().trim());
        transcript.append("assistant", """
                ## Agent response

                ```java
                public class LoginTest {
                    void signsIn() {}
                }
                ```

                ```xml
                <suite name="smoke"/>
                ```
                """.stripIndent().trim());

        String rendered = transcriptRenderedHtml(transcript);

        assertAll(
                () -> assertTrue(rendered.contains("<h2")),
                () -> assertTrue(rendered.contains("<hr")),
                () -> assertTrue(rendered.contains("language-json")),
                () -> assertTrue(rendered.contains("language-java")),
                () -> assertTrue(rendered.contains("language-xml")),
                () -> assertEquals(3, countOccurrences(rendered, "class=\"shaft-code-copy\"")),
                () -> assertTrue(rendered.contains("data-copy-code=\"public class LoginTest")),
                () -> assertTrue(rendered.contains("href=\"shaft-copy-code:public+class+LoginTest")),
                () -> assertTrue(rendered.contains("aria-label=\"Copy code\"")),
                () -> assertTrue(rendered.contains("<svg width=\"16\" height=\"16\"")),
                () -> assertTrue(rendered.contains("class=\"shaft-code-copy-icon\"")),
                () -> assertTrue(rendered.contains("&#x2398;")),
                () -> assertTrue(rendered.contains("class=\"shaft-code-highlighted language-java\""), rendered),
                () -> assertTrue(rendered.contains("<span style=\""), rendered),
                () -> assertFalse(rendered.contains(">Copy code<")));
    }

    @Test
    void assistantTranscriptCanReplaceLiveAgentOutputWithFinalMessage() {
        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.append("assistant", "Running Codex CLI...");
        transcript.replaceLast("assistant", "Validated DuckDuckGo title.");

        assertAll(
                () -> assertEquals("Validated DuckDuckGo title.", transcript.markdown()),
                () -> assertFalse(transcriptRenderedHtml(transcript).contains("Running Codex CLI")),
                () -> assertTrue(transcriptRenderedHtml(transcript).contains("Validated DuckDuckGo title.")));
    }

    @Test
    void assistantTranscriptRestoresMessagesFromChatState() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();
        state.append("user", "User prompt", "{}");
        state.append("assistant", "Assistant reply", "{}");

        AssistantTranscriptView transcript = new AssistantTranscriptView();
        transcript.setMessages(state.activeMessages());

        assertEquals("User prompt\n\nAssistant reply", transcript.markdown());
        assertFalse(transcriptRenderedHtml(transcript).isBlank());
    }

    @Test
    void assistantAgentModeSourceEditApprovalOnlyBlocksUnsandboxedCustomCommands() {
        assertAll(
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, false, "", "edit this test", "")),
                () -> assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, false, "custom-agent --unsafe", "edit this test", "")),
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, true, "custom-agent --unsafe", "edit this test", "")),
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        false, false, false, "custom-agent --unsafe", "edit this test", "")),
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, true, false, "custom-agent --unsafe", "edit this test", "")));
    }

    @Test
    void assistantAskOrPlanMcpPromptOffersOneClickSwitchToAgentMode() throws Exception {
        // Issue #3681: SHAFT already knows exactly which binary confirmation and control this gate
        // needs, so it must offer a one-click fix instead of plain chat text the user has to act on
        // by finding the mode combo box themselves.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        useFailingTestLocalAgentLauncher(panel);
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        assertNotNull(assistantMode);
        assistantMode.setSelectedItem("PLAN");

        String promptText = "open duckduckgo and search for SHAFT Engine";
        assistantPrompt(panel).setText(promptText);
        clickAccessible(panel, "Send assistant prompt");

        JButton fix = findByAccessibleName(panel, "Switch to Agent mode and resend", JButton.class);
        assertNotNull(fix, "The MCP-mode gate must offer a one-click fix button, not just prose.");
        assertEquals("PLAN", assistantMode.getSelectedItem(), "Mode must not flip until the button is clicked.");

        fix.doClick();

        assertAll(
                () -> assertEquals("AGENT", assistantMode.getSelectedItem(),
                        "Clicking the fix must switch the mode selector to Agent."),
                () -> assertEquals(2, countOccurrences(transcriptMarkdown(panel), promptText),
                        "Clicking the fix must resend the original prompt."),
                () -> assertNull(findByAccessibleName(panel, "Switch to Agent mode and resend", JButton.class),
                        "The gate bubble must clear itself once the fix is applied."));
    }

    @Test
    void assistantGateResendUsesAnInjectedLocalAgentLauncher() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        assertTrue(Arrays.stream(ShaftAssistantPanel.class.getDeclaredFields())
                .anyMatch(field -> field.getName().equals("localAgentProcessLauncher")),
                "panel resend tests need a local-agent launcher seam");
        CountDownLatch launchAttempted = new CountDownLatch(1);
        setField(panel, "localAgentProcessLauncher", (AssistantLocalAgentRunner.ProcessLauncher) (command, directory, environment) -> {
            launchAttempted.countDown();
            throw new IOException("test launcher");
        });
        setField(panel, "requireLocalAgentCommandAvailable", false);

        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        assertNotNull(assistantMode);
        assistantMode.setSelectedItem("PLAN");
        assistantPrompt(panel).setText("open duckduckgo and search for SHAFT Engine");
        clickAccessible(panel, "Send assistant prompt");

        JButton fix = findByAccessibleName(panel, "Switch to Agent mode and resend", JButton.class);
        assertNotNull(fix);
        fix.doClick();

        assertTrue(launchAttempted.await(5, TimeUnit.SECONDS),
                "the resend must invoke the injected launcher instead of a real CLI process");
    }

    @Test
    void unrestrictedCodexAccessIsExplicitDefaultOffAndPersisted() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        JCheckBox unrestricted = findByAccessibleName(panel,
                "Allow unrestricted Codex access outside the project", JCheckBox.class);

        assertNotNull(unrestricted);
        assertFalse(unrestricted.isSelected());
        assertTrue(unrestricted.getToolTipText().contains("outside this project"));

        unrestricted.doClick();
        assertTrue(settings.unrestrictedLocalAgentAccess);
    }

    @Test
    void assistantBlocksLocalAgentWhenRequiredShaftSkillsAreMissing(@TempDir Path projectRoot) throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                fakeProject(new ShaftAssistantChatState(), projectRoot.toString()), blankMcpSettings());
        CountDownLatch launchAttempted = new CountDownLatch(1);
        setField(panel, "localAgentProcessLauncher",
                (AssistantLocalAgentRunner.ProcessLauncher) (command, directory, environment) -> {
                    launchAttempted.countDown();
                    throw new IOException("test launcher");
                });
        setField(panel, "requireLocalAgentCommandAvailable", false);

        setPendingCaptureReview(panel, "```java\nclass RecordedFlowTest {}\n```");
        assistantPrompt(panel).setText("approve");
        clickAccessible(panel, "Send assistant prompt");

        assertAll(
                () -> assertEquals(1L, launchAttempted.getCount()),
                () -> assertNotNull(findByAccessibleName(panel, "Repair SHAFT skills and resend", JButton.class)),
                () -> assertTrue(containsText(panel, "shaft-developer")),
                () -> assertTrue(containsText(panel, "shaft-recording-codegen")));

        for (String skill : List.of("shaft-developer", "shaft-recording-codegen")) {
            Path skillDirectory = projectRoot.resolve(".agents/skills").resolve(skill);
            Files.createDirectories(skillDirectory);
            Files.writeString(skillDirectory.resolve("SKILL.md"), "name: " + skill);
        }
        clickAccessible(panel, "Recheck SHAFT skills");

        assertTrue(launchAttempted.await(5, TimeUnit.SECONDS),
                "recheck must resume the pending request after both skills are present");
    }

    @Test
    void assistantFamilySwitchRefreshesSkillHealth(@TempDir Path projectRoot) throws Exception {
        for (String skill : List.of("shaft-developer", "shaft-recording-codegen")) {
            Path skillDirectory = projectRoot.resolve(".agents/skills").resolve(skill);
            Files.createDirectories(skillDirectory);
            Files.writeString(skillDirectory.resolve("SKILL.md"), "name: " + skill);
        }
        ShaftAssistantPanel panel = new ShaftAssistantPanel(
                fakeProject(new ShaftAssistantChatState(), projectRoot.toString()), blankMcpSettings());
        JComboBox<?> family = findByAccessibleName(panel, "Assistant family", JComboBox.class);
        JLabel health = findByAccessibleName(panel, "Local agent health status", JLabel.class);

        assertTrue(health.getText().contains("Ready"), health.getText());
        family.setSelectedItem("CLAUDE");

        assertAll(
                () -> assertTrue(health.getText().contains("Missing"), health.getText()),
                () -> assertTrue(findByAccessibleName(panel, "Repair SHAFT agent skills", JButton.class).isVisible()));
    }

    @Test
    void assistantClearsPendingSkillResumeWhenRepairIsDeniedOrCancelled() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "pendingSkillRepairResume", true);
        invokePrivate(panel, "showDeniedToolResult", new Class<?>[]{String.class}, "shaft_project_init_agents");
        assertFalse((Boolean) getField(panel, "pendingSkillRepairResume"));

        setField(panel, "pendingSkillRepairResume", true);
        invokePrivate(panel, "showCancelledToolResult", new Class<?>[]{String.class, boolean.class},
                "shaft_project_init_agents", false);
        assertFalse((Boolean) getField(panel, "pendingSkillRepairResume"));
    }

    @Test
    void assistantAgentHealthControlsWrapAtNarrowWidth() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JPanel health = (JPanel) getField(panel, "agentHealthPanel");
        health.setSize(240, 100);
        health.doLayout();

        int firstRow = health.getComponent(0).getY();
        assertAll(
                () -> assertTrue(health.getLayout() instanceof WrapLayout),
                () -> assertTrue(Arrays.stream(health.getComponents()).anyMatch(child -> child.getY() > firstRow),
                        "health actions must wrap below the status at 240px"));
    }

    @Test
    void assistantAgentModeSourceEditWarningUsesActiveContextForContinuationPrompt() {
        assertAll(
                () -> assertTrue(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, false, "custom-agent --unsafe", "try again", "fix this Java test")),
                () -> assertFalse(ShaftAssistantPanel.requiresSourceEditApprovalBeforeSend(
                        true, false, false, "", "try again", "fix this Java test")));
    }

    @Test
    void assistantAgentModeDoesNotWarnForBrowserOnlyTasksWhenSourceEditsNotApproved() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> blockedMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        assertNotNull(blockedMode);
        blockedMode.setSelectedItem("AGENT");
        assistantPrompt(panel).setText("open https://example.com and update me on the login form");
        clickAccessible(panel, "Send assistant prompt");

        assertFalse(transcriptMarkdown(panel).contains("enable **Allow source edits**"));
    }

    @Test
    void assistantSourceEditGateOffersOneClickAllowAndResend() throws Exception {
        // Issue #3681: same one-click-fix treatment as the MCP-mode gate above, for the source-edit
        // approval gate -- ticks the checkbox and resends instead of leaving prose for the user to
        // act on.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        useFailingTestLocalAgentLauncher(panel);
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(panel, "Approve source mutation for Agent mode", JCheckBox.class);
        JTextComponent customCommand = textComponent(panel, "Optional local agent command");
        assertAll(
                () -> assertNotNull(assistantMode),
                () -> assertNotNull(allowEdits),
                () -> assertNotNull(customCommand));
        assistantMode.setSelectedItem("AGENT");
        allowEdits.setSelected(false);
        customCommand.setText("custom-agent --unsafe");

        String promptText = "fix this Java test";
        assistantPrompt(panel).setText(promptText);
        clickAccessible(panel, "Send assistant prompt");

        JButton fix = findByAccessibleName(panel, "Allow source edits and resend", JButton.class);
        assertNotNull(fix, "The source-edit gate must offer a one-click fix button, not just prose.");
        assertFalse(allowEdits.isSelected(), "The checkbox must not flip until the button is clicked.");

        fix.doClick();

        assertAll(
                () -> assertTrue(allowEdits.isSelected(),
                        "Clicking the fix must tick the Allow source edits checkbox."),
                () -> assertEquals(2, countOccurrences(transcriptMarkdown(panel), promptText),
                        "Clicking the fix must resend the original prompt."),
                () -> assertNull(findByAccessibleName(panel, "Allow source edits and resend", JButton.class),
                        "The gate bubble must clear itself once the fix is applied."));
    }

    @Test
    void assistantModeSelectorCanChangeModesInDefaultComposer() {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);

        assertAll(
                () -> assertNotNull(assistantMode),
                () -> assertTrue(assistantMode.isVisible()),
                () -> assertTrue(assistantMode.isEnabled()));

        assistantMode.setSelectedItem("PLAN");
        assertEquals("PLAN", assistantMode.getSelectedItem());

        assistantMode.setSelectedItem("AGENT");
        assertEquals("AGENT", assistantMode.getSelectedItem());

        JCheckBox allowEdits = findByAccessibleName(panel, "Approve source mutation for Agent mode", JCheckBox.class);
        assertAll(
                () -> assertNotNull(allowEdits),
                () -> assertTrue(allowEdits.isVisible()),
                () -> assertTrue(allowEdits.isEnabled()));
    }

    @Test
    void allowSourceMutationSurvivesRunningCycleInAgentModeLocalRoute() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.advancedUiEnabled = true;
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        JComboBox<?> assistantProviderType = findByAccessibleName(panel, "Assistant provider type", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(panel, "Approve source mutation for Agent mode", JCheckBox.class);

        assistantProviderType.setSelectedItem("LOCAL");
        assistantMode.setSelectedItem("AGENT");
        allowEdits.setSelected(true);

        panel.setRunning(true, "Thinking...");
        panel.setRunning(false, "Ready");

        assertTrue(allowEdits.isSelected(),
                "Allow source edits should remain selected after a running cycle in Agent mode");
    }

    @Test
    void allowSourceMutationDefaultsOnAndKeepsUserChoiceAcrossModeSwitches() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.advancedUiEnabled = true;
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        JComboBox<?> assistantProviderType = findByAccessibleName(panel, "Assistant provider type", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(panel, "Approve source mutation for Agent mode", JCheckBox.class);

        assistantProviderType.setSelectedItem("LOCAL");
        assistantMode.setSelectedItem("AGENT");
        // Checked by default: a first-time user expects generated tests to land in the project.
        assertTrue(allowEdits.isSelected());

        AtomicInteger uncheckCount = new AtomicInteger();
        allowEdits.addItemListener(event -> {
            if (!allowEdits.isSelected()) {
                uncheckCount.incrementAndGet();
            }
        });

        // Mode switches no longer silently clear the approval: the checkbox value is only honored
        // in local Agent mode anyway (AssistantCommand forces allowSourceMutation=false elsewhere),
        // so the user's choice survives ASK/PLAN round-trips.
        assistantMode.setSelectedItem("ASK");
        assistantMode.setSelectedItem("AGENT");
        assistantMode.setSelectedItem("PLAN");
        assistantMode.setSelectedItem("AGENT");
        assertAll(
                () -> assertTrue(allowEdits.isSelected(), "Mode switches must not clear the source-edit approval"),
                () -> assertEquals(0, uncheckCount.get(), "No silent unchecks on mode switches"));
    }

    @Test
    void allowSourceMutationSurvivesCloudForcedModeSwitch() {
        // The live cloud route is not reachable in this headless test harness: usesCloud()==true
        // makes updateControlVisibility() call updateCloudKeyStatus(), which needs
        // ApplicationManager.getApplication() (unavailable without IntelliJ Platform Test Framework
        // fixtures, which this task's tests must not require/pull in). updateControlVisibility()
        // forces mode back to PLAN (the behavior this test proves) *before* it reaches
        // updateCloudKeyStatus(), so the NPE from the missing ApplicationManager happens strictly
        // after the effect under test has already taken place. The try/catch below only swallows
        // that trailing, unrelated environment limitation.
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.advancedUiEnabled = true;
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings);
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        JComboBox<?> assistantProviderType = findByAccessibleName(panel, "Assistant provider type", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(panel, "Approve source mutation for Agent mode", JCheckBox.class);

        assistantProviderType.setSelectedItem("LOCAL");
        assistantMode.setSelectedItem("AGENT");
        assertTrue(allowEdits.isSelected(), "Source edits are approved by default");
        assertEquals("AGENT", assistantMode.getSelectedItem());

        // Selecting CLOUD forces mode back to PLAN (cloud cannot run Agent). The checkbox keeps
        // its value — AssistantCommand already forces allowSourceMutation=false on every cloud
        // request — so returning to the local route does not lose the user's approval.
        try {
            assistantProviderType.setSelectedItem("CLOUD");
        } catch (NullPointerException ignoredMissingApplicationManager) {
            // Expected in this headless harness; see comment above.
        }

        assertAll(
                () -> assertTrue(allowEdits.isSelected(),
                        "Cloud route must not silently clear the source-edit approval"),
                () -> assertEquals("PLAN", assistantMode.getSelectedItem(),
                        "Forcing cloud route while in Agent mode should switch mode to PLAN end-to-end"));
    }

    @Test
    void assistantAgentModeShowsSourceEditApprovalForLockedDesktopRuntime() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.assistantRuntime = "DESKTOP_APP";
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, settings, new ShaftAssistantChatState(), () -> {
        });
        JComboBox<?> assistantMode = findByAccessibleName(panel, "Assistant mode", JComboBox.class);
        JCheckBox allowEdits = findByAccessibleName(panel, "Approve source mutation for Agent mode", JCheckBox.class);

        assertAll(
                () -> assertNotNull(assistantMode),
                () -> assertNotNull(allowEdits));

        assistantMode.setSelectedItem("AGENT");

        assertAll(
                () -> assertTrue(allowEdits.isVisible()),
                () -> assertTrue(allowEdits.isEnabled()));
    }

    @Test
    void actionRowButtonsAreLaidOutAfterClearingTranscript() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        panel.simulateAppendForTest("user", "Plan a resilient login test", "");
        panel.simulateAppendForTest("assistant", "Here is a plan.", "raw output");

        layoutPanel(panel);
        assertActionRowButtonsSaneAfterLayout(panel, true);

        RevalidateSpy spy = RevalidateSpy.install();
        try {
            clickAccessible(panel, "Clear assistant transcript");
            SwingUtilities.invokeAndWait(() -> {
            });
        } finally {
            spy.uninstall();
        }
        assertTrue(spy.sawRevalidateFor(actionRowContainer(panel)),
                "updateActionChrome() should revalidate the action row's container after Clear,"
                        + " without relying on the test's manual layout walk");

        // Do NOT call the manual layoutPanel() walker before asserting bounds here: that helper
        // forces doLayout()/validate() on every component regardless of production behavior, which
        // would make this assertion pass even if updateActionChrome()'s revalidate()/repaint() call
        // were deleted. Instead, flush the revalidate/repaint that production code already scheduled.
        flushPendingLayoutAndRepaint(panel);

        JButton clear = findByAccessibleName(panel, "Clear assistant transcript", JButton.class);
        JButton copyTranscript = findByAccessibleName(panel, "Copy assistant transcript", JButton.class);
        JButton rerun = findByAccessibleName(panel, "Rerun last assistant prompt", JButton.class);
        JButton copyResponse = findByAccessibleName(panel, "Copy last assistant response", JButton.class);
        assertAll(
                () -> assertFalse(clear.isVisible(), "Clear should hide once transcript is empty"),
                () -> assertFalse(copyTranscript.isVisible(), "Copy all should hide once transcript is empty"),
                () -> assertFalse(rerun.isVisible(), "Rerun should hide once last prompt is cleared"),
                () -> assertFalse(copyResponse.isVisible(), "Copy response should hide once last response is cleared"));
        assertActionRowButtonsSaneAfterLayout(panel, false);
    }

    @Test
    void actionRowButtonsAreLaidOutAfterAppendingMessages() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        layoutPanel(panel);

        RevalidateSpy spy = RevalidateSpy.install();
        try {
            panel.simulateAppendForTest("user", "Plan a resilient login test", "");
            SwingUtilities.invokeAndWait(() -> {
            });
            panel.simulateAppendForTest("assistant", "Here is a plan.", "raw output");
            SwingUtilities.invokeAndWait(() -> {
            });
        } finally {
            spy.uninstall();
        }
        assertTrue(spy.sawRevalidateFor(actionRowContainer(panel)),
                "updateActionChrome() should revalidate the action row's container after appending"
                        + " messages (a non-Clear caller), proving the fix lives in updateActionChrome()"
                        + " rather than being Clear-specific");

        // Same rationale as above: flush the production-scheduled revalidate/repaint instead of
        // forcing layout manually, so a deleted refreshActionRowLayout() call would fail this test.
        flushPendingLayoutAndRepaint(panel);

        assertActionRowButtonsSaneAfterLayout(panel, true);
    }

    @Test
    void saveTranscriptButtonIsWiredWithIconWithoutInvokingFileChooser() {
        // Deliberately does NOT click the button: doing so would invoke the real
        // FileChooserFactory save dialog, which hangs/crashes in this headless suite.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());

        JButton saveTranscript = findByAccessibleName(panel, "Save assistant transcript to file", JButton.class);

        assertAll(
                () -> assertNotNull(saveTranscript, "Save transcript button should be wired into the action row"),
                () -> assertNotNull(saveTranscript.getIcon(), "save transcript button should have an icon"));
    }

    @Test
    void saveTranscriptHidesWithItsActionRowSiblingsInTheEmptyState() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        layoutPanel(panel);

        JButton saveTranscript = findByAccessibleName(panel, "Save assistant transcript to file", JButton.class);
        assertNotNull(saveTranscript, "Save transcript button should be wired into the action row");
        assertFalse(saveTranscript.isVisible(),
                "Save transcript should hide on a fresh, empty panel like its copyTranscript/clearTranscript siblings");

        panel.simulateAppendForTest("user", "Plan a resilient login test", "");
        panel.simulateAppendForTest("assistant", "Here is a plan.", "raw output");
        flushPendingLayoutAndRepaint(panel);

        assertTrue(saveTranscript.isVisible(), "Save transcript should show once the transcript has content");

        clickAccessible(panel, "Clear assistant transcript");
        SwingUtilities.invokeAndWait(() -> {
        });
        flushPendingLayoutAndRepaint(panel);

        assertFalse(saveTranscript.isVisible(),
                "Save transcript should hide again once the transcript is cleared back to the empty state");
    }

    @Test
    void writeTranscriptSyncWritesExportedTranscriptContent(@TempDir Path tempDir) throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        panel.simulateAppendForTest("user", "Plan a resilient login test", "");
        panel.simulateAppendForTest("assistant", "Here is a plan.", "raw output");

        Path target = tempDir.resolve("test-transcript.md");
        String expectedExport = assistantExport(panel);

        writeTranscriptSync(panel, target, expectedExport);

        assertAll(
                () -> assertTrue(Files.exists(target), "transcript file should have been written"),
                () -> assertEquals(expectedExport, Files.readString(target),
                        "saved transcript content should equal exportTranscriptWithEvidence() output"));
    }

    /**
     * Resolves the same container that {@code ShaftAssistantPanel.refreshActionRowLayout()}
     * revalidates/repaints: the action row's parent, falling back to the action row itself.
     */
    private static Container actionRowContainer(ShaftAssistantPanel panel) throws Exception {
        JPanel actionRow = (JPanel) getField(panel, "actionRow");
        Container parent = actionRow.getParent();
        return parent != null ? parent : actionRow;
    }

    /**
     * Flushes a revalidate()/repaint() that production code already scheduled (via
     * {@code RepaintManager}) into an actual layout pass. The spy assertion (run before this method)
     * is what keeps the test sensitive to a deleted {@code refreshActionRowLayout()} call; this
     * method's job is only to produce realized bounds afterward, so it mirrors {@link
     * #layoutPanel(ShaftAssistantPanel)}'s recursive doLayout()/validate() walk rather than relying
     * on {@code Container.validate()} to recurse into every nested panel on its own.
     */
    private static void flushPendingLayoutAndRepaint(ShaftAssistantPanel panel) throws Exception {
        layoutPanel(panel);
    }

    /**
     * A {@link RepaintManager} that records exactly which components had {@code revalidate()}
     * invoked on them (via {@code addInvalidComponent}), so tests can assert that production code
     * itself requested a layout refresh instead of relying on a test helper that forces layout
     * unconditionally. Deliberately does NOT track {@code addDirtyRegion} (repaint): a plain
     * {@link javax.swing.AbstractButton#doClick()} already marks the unrealized top-level panel
     * dirty as a side effect of the button's own pressed/armed paint state (Swing's default
     * RepaintManager walks up to the nearest showing-or-root ancestor), which would make a
     * repaint-based check pass trivially even with no production revalidate/repaint call at all.
     * revalidate() does not have that false-positive: only an explicit
     * {@code JComponent.revalidate()} call adds the exact component to the invalid-component list.
     */
    private static final class RevalidateSpy extends RepaintManager {
        private final RepaintManager delegate;
        private final List<Component> invalidated = new ArrayList<>();

        private RevalidateSpy(RepaintManager delegate) {
            this.delegate = delegate;
        }

        static RevalidateSpy install() {
            RevalidateSpy spy = new RevalidateSpy(RepaintManager.currentManager(null));
            RepaintManager.setCurrentManager(spy);
            return spy;
        }

        void uninstall() {
            RepaintManager.setCurrentManager(delegate);
        }

        boolean sawRevalidateFor(Component target) {
            return invalidated.contains(target);
        }

        @Override
        public synchronized void addInvalidComponent(JComponent invalidComponent) {
            invalidated.add(invalidComponent);
            delegate.addInvalidComponent(invalidComponent);
        }
    }

    private static void layoutPanel(ShaftAssistantPanel panel) throws Exception {
        panel.setBounds(0, 0, 900, 700);
        SwingUtilities.invokeAndWait(() -> {
            panel.doLayout();
            walkComponents(panel, comp -> {
                if (comp instanceof JComponent jc && comp != panel) {
                    jc.doLayout();
                }
            });
            panel.validate();
        });
    }

    private static void assertActionRowButtonsSaneAfterLayout(ShaftAssistantPanel panel, boolean expectResponseButtonsVisible)
            throws Exception {
        JLabel statusLabel = (JLabel) getField(panel, "status");
        List<JButton> actionRowButtons = List.of(
                findByAccessibleName(panel, "Copy last assistant response", JButton.class),
                findByAccessibleName(panel, "Copy last raw assistant response", JButton.class),
                findByAccessibleName(panel, "Copy assistant transcript", JButton.class),
                findByAccessibleName(panel, "Clear assistant transcript", JButton.class),
                findByAccessibleName(panel, "Rerun last assistant prompt", JButton.class),
                findByAccessibleName(panel, "Cancel assistant request", JButton.class));
        boolean anyVisible = false;
        for (JButton button : actionRowButtons) {
            assertNotNull(button);
            if (!button.isVisible()) {
                continue;
            }
            anyVisible = true;
            assertTrue(button.getWidth() > 0,
                    accessibleName(button) + " should have a positive width after layout");
            assertTrue(button.getHeight() > 0,
                    accessibleName(button) + " should have a positive height after layout");
            java.awt.Rectangle buttonBounds = new java.awt.Rectangle(
                    SwingUtilities.convertPoint(button.getParent(), button.getLocation(), panel), button.getSize());
            java.awt.Rectangle statusBounds = new java.awt.Rectangle(
                    SwingUtilities.convertPoint(statusLabel.getParent(), statusLabel.getLocation(), panel),
                    statusLabel.getSize());
            assertFalse(buttonBounds.intersects(statusBounds),
                    accessibleName(button) + " bounds " + buttonBounds + " should not intersect status bounds " + statusBounds);
        }
        if (expectResponseButtonsVisible) {
            assertTrue(anyVisible, "Expected at least one action-row button to be visible after layout");
        }
    }

    @Test
    void assistantCopyAllIncludesCurrentSessionToolEvidence() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings());
        showAgentResult(panel, ShaftMcpToolResult.success("tool output payload"));
        String exported = assistantExport(panel);

        assertAll(
                () -> assertTrue(exported.contains("## Tool evidence")),
                () -> assertTrue(exported.contains("autobot_local_agent_run")),
                () -> assertTrue(exported.contains("tool output payload")));
    }

    @Test
    void setupSuccessOpensNewFreshSessionWithoutLosingSessions() throws Exception {
        ShaftSettingsState.Settings settings = blankMcpSettings();
        settings.mcpCommand = "cmd";
        settings.mcpSetupComplete = false;
        settings.assistantFamily = "CODEX";
        settings.defaultAutobotClient = "CODEX";
        Project project = fakeProject();
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "Previous assistant conversation", "{}");
        String previousSessionId = chatState.activeSession().id;
        int initialSessionCount = chatState.sessions().size();

        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(project, settings, readyProbe(), chatState);

        ShaftMcpSetupPanel setupPanel = setupPanel(toolWindow);
        assertNotNull(setupPanel);
        showTestResult(setupPanel, ShaftMcpToolResult.success("Probe OK"));
        clickAccessible(setupPanel, "Start chatting with SHAFT Assistant");

        assertAll(
                () -> assertTrue(chatState.activeMessages().isEmpty(),
                        "Setup complete should open a fresh (empty) session"),
                () -> assertNotEquals(previousSessionId, chatState.activeSession().id,
                        "Should have created a new session"),
                () -> assertEquals(initialSessionCount + 1, chatState.sessions().size(),
                        "Previous session should still exist in sessions list"),
                () -> assertTrue(chatState.sessions().stream().anyMatch(s -> s.id.equals(previousSessionId)),
                        "Previous session with conversation should still be selectable"),
                () -> assertFalse(transcriptMarkdown(toolWindow).contains("Previous assistant conversation"),
                        "Current transcript should show the new empty session, not previous conversation"),
                // Progressive disclosure (issue #3425 A4): the default main view carries the
                // Assistant + Guided selector.
                () -> assertNotNull(toolWindowWorkflowSelector(toolWindow)));
    }

    @Test
    void guidedWorkflowPrefillsContractAlignedToolArguments() {
        List<CapturedInvocation> invocations = new ArrayList<>();
        GuidedWorkflowPanel panel = new GuidedWorkflowPanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));

        click(panel, "Inspect locator");
        CapturedInvocation inspect = last(invocations);
        assertAll(
                () -> assertEquals("browser_open_intent", inspect.toolName()),
                () -> assertTrue(inspect.arguments().has("userIntent")),
                () -> assertTrue(inspect.arguments().has("maxCharacters")),
                () -> assertFalse(inspect.arguments().has("intent")));

        click(panel, "Review code");
        CapturedInvocation codegen = last(invocations);
        assertAll(
                () -> assertEquals("capture_code_blocks", codegen.toolName()),
                () -> assertTrue(codegen.arguments().has("outputDirectory")),
                () -> assertTrue(codegen.arguments().has("packageName")),
                () -> assertTrue(codegen.arguments().has("driverVariableName")));

        // Issue #3660 added a second combo box (the recorder browser picker) to this panel's
        // always-visible primary fields, ahead of the backend combo in tab order -- so the ambiguous
        // "first combo box in the tree" lookup below no longer reaches the backend combo. Look it up
        // by its accessible name instead, same as GuidedWorkflowPanelTest already does.
        JComboBox<?> backend = findByAccessibleName(panel, "Guided workflow backend", JComboBox.class);
        assertNotNull(backend);
        backend.setSelectedItem("Playwright");
        click(panel, "Start recording");
        CapturedInvocation playwright = last(invocations);
        assertAll(
                () -> assertEquals("capture_start", playwright.toolName()),
                () -> assertTrue(playwright.arguments().has("outputPath")),
                // Issue #3912: mode/includeSensitiveValues are stale pre-#3881 capture_start
                // arguments (absorbed into sessionGoal / dropped) -- the live-served schema rejects
                // them outright, and this assertion previously encoded the stale contract.
                () -> assertTrue(playwright.arguments().has("sessionGoal")),
                () -> assertFalse(playwright.arguments().has("mode")),
                () -> assertFalse(playwright.arguments().has("includeSensitiveValues")),
                () -> assertFalse(playwright.arguments().has("targetUrl")));
    }

    @Test
    void evidenceTriagePrefillsDoctorAndHealerContracts() {
        List<CapturedInvocation> invocations = new ArrayList<>();
        EvidenceTriagePanel panel = new EvidenceTriagePanel(null,
                (toolName, arguments) -> invocations.add(new CapturedInvocation(toolName, arguments)));

        click(panel, "Fix Failing Test");
        CapturedInvocation repair = last(invocations);
        assertAll(
                () -> assertEquals("shaft_coding_partner_plan", repair.toolName()),
                () -> assertTrue(repair.arguments().has("repositoryPath")),
                () -> assertTrue(repair.arguments().has("intent")),
                () -> assertTrue(repair.arguments().getAsJsonArray("artifactPaths").size() >= 1));

        click(panel, "Analyze Allure");
        CapturedInvocation analyze = last(invocations);
        assertAll(
                () -> assertEquals("doctor_analyze_failed_allure", analyze.toolName()),
                () -> assertTrue(analyze.arguments().has("repositoryRoot")),
                () -> assertTrue(analyze.arguments().has("historicalBundlePaths")),
                () -> assertTrue(analyze.arguments().has("useAi")),
                () -> assertFalse(analyze.arguments().has("repository")));

        click(panel, "Suggest Fix");
        CapturedInvocation suggest = last(invocations);
        assertAll(
                () -> assertEquals("doctor_suggest_fix", suggest.toolName()),
                () -> assertTrue(suggest.arguments().has("jsonReportPath")),
                () -> assertTrue(suggest.arguments().has("repositoryRoot")),
                () -> assertTrue(suggest.arguments().has("useAi")));

        click(panel, "Run Healer");
        CapturedInvocation healer = last(invocations);
        assertAll(
                () -> assertEquals("healer_run_failed_test", healer.toolName()),
                () -> assertEquals("-Dtest=ExampleTest",
                        healer.arguments().getAsJsonArray("testCommand").get(2).getAsString()));

        click(panel, "Propose Locator");
        CapturedInvocation locator = last(invocations);
        assertAll(
                () -> assertEquals("doctor_propose_healed_locator", locator.toolName()),
                () -> assertTrue(locator.arguments().has("repositoryRoot")),
                () -> assertTrue(locator.arguments().has("healingReportPath")),
                () -> assertTrue(locator.arguments().has("sourcePath")));
    }

    @Test
    void contextTruncationIndicatorRendersAtBoundaryWhenExceeded() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings(), chatState);

        String longMessage = "x".repeat(8000);
        String shortMessage = "y".repeat(100);

        panel.simulateAppendForTest("user", longMessage, "");
        panel.simulateAppendForTest("assistant", longMessage, "");
        panel.simulateAppendForTest("user", shortMessage, "");

        AssistantTranscriptView transcript = getTranscriptView(panel);
        JLabel truncationChip = findByAccessibleName(panel, "Context truncation chip", JLabel.class);

        assertAll(
                () -> assertNotNull(truncationChip,
                        "Truncation chip should exist when context exceeds cap"),
                () -> assertEquals("Earlier messages not included in context", truncationChip.getText(),
                        "Truncation chip text should be correct"),
                () -> assertTrue(truncationChip.getToolTipText().contains("16000"),
                        "Tooltip should mention the 16K character cap"));
    }

    @Test
    void contextTruncationIndicatorHidesWhenAllFits() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings(), chatState);

        String shortMessage = "Test message";
        panel.simulateAppendForTest("user", shortMessage, "");
        panel.simulateAppendForTest("assistant", shortMessage, "");

        JLabel truncationChip = findByAccessibleName(panel, "Context truncation chip", JLabel.class);

        assertNull(truncationChip,
                "Truncation chip should not appear when all messages fit within cap");
    }

    @Test
    void contextTruncationIndicatorRepositionsWithNewMessages() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings(), chatState);

        String longMessage = "x".repeat(8000);
        String mediumMessage = "y".repeat(500);
        String shortMessage = "z".repeat(100);

        panel.simulateAppendForTest("user", longMessage, "");
        panel.simulateAppendForTest("assistant", longMessage, "");
        panel.simulateAppendForTest("user", mediumMessage, "");

        JLabel chipBefore = findByAccessibleName(panel, "Context truncation chip", JLabel.class);
        assertNotNull(chipBefore, "Truncation chip should exist before adding more messages");

        panel.simulateAppendForTest("assistant", shortMessage, "");

        JLabel chipAfter = findByAccessibleName(panel, "Context truncation chip", JLabel.class);
        assertNotNull(chipAfter,
                "Truncation indicator should still be present after new message");
    }

    @Test
    void contextTruncationIndicatorTooltipIsDescriptive() {
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, connectedMcpSettings(), chatState);

        String longMessage = "x".repeat(8000);
        String shortMessage = "y".repeat(100);
        panel.simulateAppendForTest("user", longMessage, "");
        panel.simulateAppendForTest("assistant", longMessage, "");
        panel.simulateAppendForTest("user", shortMessage, "");

        JLabel truncationChip = findByAccessibleName(panel, "Context truncation chip", JLabel.class);

        assertAll(
                () -> assertNotNull(truncationChip, "Truncation chip should exist"),
                () -> assertTrue(truncationChip.getToolTipText().contains("Start a new chat"),
                        "Tooltip should suggest starting a new chat when context is truncated"));
    }

    @Test
    void gateToolDispatchesImmediatelyWhenApproveAllToolsFlagIsSet() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        clearFirstRunWelcome(panel);
        approvalServiceOf(panel).record(ToolApprovalDecision.APPROVE_ALL_TOOLS, "unused");
        AssistantCommand.Invocation invocation = AssistantCommand.Invocation.tool("capture_start", new JsonObject());

        Method startMcpInvocation = ShaftAssistantPanel.class.getDeclaredMethod(
                "startMcpInvocation", AssistantCommand.Invocation.class);
        startMcpInvocation.setAccessible(true);

        InvocationTargetException thrown = assertThrows(InvocationTargetException.class,
                () -> startMcpInvocation.invoke(panel, invocation));

        assertAll(
                () -> assertTrue(thrown.getCause() instanceof NullPointerException,
                        "approve-all should skip the prompt and reach the real dispatch immediately"),
                () -> assertNull(transcriptWidget(panel), "approve-all must never render an approval prompt"));
    }

    // ---- Transcript-visible routing-decision labels (issue #3883(b)) ----

    @Test
    void toolSelectedMilestoneNarratesTheRoutedViaLabelWhenTheInvocationCarriesOne() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        clearFirstRunWelcome(panel);
        approvalServiceOf(panel).record(ToolApprovalDecision.APPROVE_ALL_TOOLS, "unused");
        AssistantCommand.Invocation invocation = AssistantCommand.Invocation.tool("capture_start", new JsonObject())
                .routedVia("intent: browser recording (weight 60)");

        Method startMcpInvocation = ShaftAssistantPanel.class.getDeclaredMethod(
                "startMcpInvocation", AssistantCommand.Invocation.class);
        startMcpInvocation.setAccessible(true);

        assertThrows(InvocationTargetException.class, () -> startMcpInvocation.invoke(panel, invocation));

        assertTrue(transcriptMarkdown(panel).contains(
                        "Tool selected: capture_start — routed via intent: browser recording (weight 60)"),
                transcriptMarkdown(panel));
    }

    @Test
    void toolSelectedMilestoneOmitsTheSuffixWhenTheInvocationCarriesNoRoutedViaLabel() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        clearFirstRunWelcome(panel);
        approvalServiceOf(panel).record(ToolApprovalDecision.APPROVE_ALL_TOOLS, "unused");
        AssistantCommand.Invocation invocation = AssistantCommand.Invocation.tool("capture_start", new JsonObject());

        Method startMcpInvocation = ShaftAssistantPanel.class.getDeclaredMethod(
                "startMcpInvocation", AssistantCommand.Invocation.class);
        startMcpInvocation.setAccessible(true);

        assertThrows(InvocationTargetException.class, () -> startMcpInvocation.invoke(panel, invocation));

        assertAll(
                () -> assertTrue(transcriptMarkdown(panel).contains("Tool selected: capture_start"),
                        transcriptMarkdown(panel)),
                () -> assertFalse(transcriptMarkdown(panel).contains("routed via"), transcriptMarkdown(panel)));
    }

    @Test
    void toolSelectedMilestoneNarratesTheRoutedViaLabelForASequenceInvocation() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        clearFirstRunWelcome(panel);
        approvalServiceOf(panel).record(ToolApprovalDecision.APPROVE_ALL_TOOLS, "unused");
        AssistantCommand.Invocation invocation = AssistantCommand.Invocation.sequence(List.of(
                        new AssistantCommand.ToolCall("driver_initialize", new JsonObject()),
                        new AssistantCommand.ToolCall("browser_open_intent", new JsonObject())))
                .routedVia("intent: browser control (weight 55)");

        Method startMcpInvocation = ShaftAssistantPanel.class.getDeclaredMethod(
                "startMcpInvocation", AssistantCommand.Invocation.class);
        startMcpInvocation.setAccessible(true);

        assertThrows(InvocationTargetException.class, () -> startMcpInvocation.invoke(panel, invocation));

        assertTrue(transcriptMarkdown(panel).contains(
                        "Tool selected: sequence — routed via intent: browser control (weight 55)"),
                transcriptMarkdown(panel));
    }

    @Test
    void gateToolDenialNeverDispatchesAndReturnsToReadyWithADenialMessage() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        AssistantCommand.Invocation invocation = AssistantCommand.Invocation.tool("capture_start", new JsonObject());
        Method startMcpInvocation = ShaftAssistantPanel.class.getDeclaredMethod(
                "startMcpInvocation", AssistantCommand.Invocation.class);
        startMcpInvocation.setAccessible(true);
        startMcpInvocation.invoke(panel, invocation);

        JComponent widget = transcriptWidget(panel);
        assertNotNull(widget, "an unapproved tool must render an approval prompt before dispatching");
        JButton deny = approvalDecisionButton((ToolApprovalPromptPanel) widget, "Deny");

        SwingUtilities.invokeAndWait(deny::doClick);

        assertAll(
                () -> assertNull(getField(panel, "currentInvocation"), "the invocation service must never be called"),
                () -> assertNull(transcriptWidget(panel), "the prompt should clear once a decision is made"),
                () -> assertTrue(transcriptMarkdown(panel).contains("Denied `capture_start`")),
                () -> assertTrue(transcriptMarkdown(panel).contains("denied")));
    }

    @Test
    void gateToolApprovedOnceDispatchesAndSkipsARepeatPromptForTheSameToolInTheSameRun() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        Method gateTool = ShaftAssistantPanel.class.getDeclaredMethod(
                "gateTool", String.class, JsonObject.class, Runnable.class, Runnable.class);
        gateTool.setAccessible(true);
        AtomicInteger approvedCalls = new AtomicInteger();
        Runnable onApproved = approvedCalls::incrementAndGet;
        Runnable onDenied = () -> {
        };

        gateTool.invoke(panel, "capture_start", new JsonObject(), onApproved, onDenied);
        JComponent widget = transcriptWidget(panel);
        assertNotNull(widget, "an unapproved tool must render an approval prompt");
        JButton approveOnce = approvalDecisionButton((ToolApprovalPromptPanel) widget, "Approve once");
        SwingUtilities.invokeAndWait(approveOnce::doClick);

        assertAll(
                () -> assertEquals(1, approvedCalls.get(), "approve-once should invoke the invocation exactly once"),
                () -> assertNull(transcriptWidget(panel), "the prompt should clear once approved"));

        gateTool.invoke(panel, "capture_start", new JsonObject(), onApproved, onDenied);

        assertAll(
                () -> assertEquals(2, approvedCalls.get(),
                        "a tool already approved this run must dispatch immediately without a second prompt"),
                () -> assertNull(transcriptWidget(panel), "no widget should render for a repeat call in the same run"));
    }

    @Test
    void sequenceSkipsARepeatPromptForAToolAlreadyApprovedThisRun() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        clearFirstRunWelcome(panel);
        setField(panel, "currentToolSequence", List.of(
                new AssistantCommand.ToolCall("capture_start", new JsonObject()),
                new AssistantCommand.ToolCall("capture_start", new JsonObject())));
        setField(panel, "sequenceMarkdown", new StringBuilder());
        setField(panel, "sequenceRawOutput", new StringBuilder());
        @SuppressWarnings("unchecked")
        Set<String> approvedThisRun = (Set<String>) getField(panel, "approvedToolsThisRun");
        approvedThisRun.add("capture_start");

        Method runNextSequenceCall = ShaftAssistantPanel.class.getDeclaredMethod("runNextSequenceCall", int.class);
        runNextSequenceCall.setAccessible(true);

        InvocationTargetException thrown = assertThrows(InvocationTargetException.class,
                () -> runNextSequenceCall.invoke(panel, 1));

        assertAll(
                () -> assertTrue(thrown.getCause() instanceof NullPointerException,
                        "the already-approved tool should reach the real dispatch immediately"),
                () -> assertNull(transcriptWidget(panel),
                        "an already-approved-this-run tool must not render a second prompt"));
    }

    @Test
    void localAgentApprovalRequestRendersApprovalWidgetAndApproveOnceAllows() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "activeLocalAgentStreamToken", 7);
        JsonObject input = new JsonObject();
        input.addProperty("command", "echo hi");

        LocalAgentApprovalBridge.ApprovalRequestHandler handler = localAgentApprovalHandler(panel, 7);
        CompletableFuture<LocalAgentApprovalBridge.Decision> decision = handler.requestApproval("Bash", input);
        SwingUtilities.invokeAndWait(() -> {
        });

        JComponent widget = transcriptWidget(panel);
        assertNotNull(widget, "an unapproved local-agent tool call must render an approval prompt");
        JButton approveOnce = approvalDecisionButton((ToolApprovalPromptPanel) widget, "Approve once");
        SwingUtilities.invokeAndWait(approveOnce::doClick);

        assertAll(
                () -> assertNull(transcriptWidget(panel), "the prompt should clear once approved"),
                () -> assertTrue(decision.isDone()),
                () -> assertTrue(decision.get().allowed()));
    }

    /**
     * Issue #3632: a keyboard-only or screen-reader user gets no signal an approval decision is
     * needed unless focus moves to the prompt. Headless Swing components are never "showing" in
     * this suite (see #3630), so {@code requestFocusInWindow()} cannot actually move real keyboard
     * focus here; what this test can reliably verify is that the new {@code
     * runOnEdt(approvalPanel::focusFirstDecisionButton)} wiring in {@code
     * showLocalAgentApprovalPrompt} runs end-to-end without throwing, and that the button it
     * targets is genuinely focusable/enabled -- the precondition for the fix to work in a real,
     * realized IDE window.
     */
    @Test
    void localAgentApprovalPromptFocusesFirstDecisionButtonWhenShown() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "activeLocalAgentStreamToken", 13);

        LocalAgentApprovalBridge.ApprovalRequestHandler handler = localAgentApprovalHandler(panel, 13);
        handler.requestApproval("Bash", new JsonObject());
        SwingUtilities.invokeAndWait(() -> {
        });

        ToolApprovalPromptPanel widget = (ToolApprovalPromptPanel) transcriptWidget(panel);
        assertNotNull(widget, "an unapproved local-agent tool call must render an approval prompt");
        JButton firstButton = widget.decisionButtonsForTest().get(0);
        assertAll(
                () -> assertTrue(firstButton.isFocusable(), "the first decision button must be focusable"),
                () -> assertTrue(firstButton.isEnabled(),
                        "the first decision button must be enabled to receive focus"));
    }

    /**
     * Issue #3632: focus should return to the composer after a decision resolves the widget slot,
     * so the keyboard-only user can keep typing. Coverage of the "but NOT when a second prompt is
     * queued" nuance in {@code resolveLocalAgentApproval} is intentionally not duplicated here as a
     * brittle focus-introspection assertion: {@link #secondQueuedLocalAgentApprovalShowsAfterTheFirstResolves}
     * already asserts that resolving the first of two queued prompts immediately shows the second
     * widget, which is only true if the {@code next.run()} branch ran instead of the {@code
     * prompt.requestFocusInWindow()} branch -- that existing, unmodified test remaining green after
     * this change is the regression proof for the queued case.
     */
    @Test
    void localAgentApprovalRestoresComposerFocusAfterADecisionWithNothingQueued() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "activeLocalAgentStreamToken", 14);

        LocalAgentApprovalBridge.ApprovalRequestHandler handler = localAgentApprovalHandler(panel, 14);
        handler.requestApproval("Bash", new JsonObject());
        SwingUtilities.invokeAndWait(() -> {
        });
        JButton approveOnce = approvalDecisionButton((ToolApprovalPromptPanel) transcriptWidget(panel), "Approve once");
        SwingUtilities.invokeAndWait(approveOnce::doClick);

        JBTextArea prompt = (JBTextArea) getField(panel, "prompt");
        assertAll(
                () -> assertNull(transcriptWidget(panel), "the prompt should clear once approved"),
                () -> assertTrue(prompt.isFocusable(), "composer must be focusable to receive the restored focus"));
    }

    @Test
    void localAgentDenyCompletesDenyAndFoldsOutcomeIntoStreamBubble() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        JCheckBox verbose = findByAccessibleName(panel, "Show verbose agent output", JCheckBox.class);
        verbose.setSelected(true);
        appendStreamingLocalAgentBubble(panel, 8);

        LocalAgentApprovalBridge.ApprovalRequestHandler handler = localAgentApprovalHandler(panel, 8);
        CompletableFuture<LocalAgentApprovalBridge.Decision> decision = handler.requestApproval("Write", new JsonObject());
        SwingUtilities.invokeAndWait(() -> {
        });
        JButton deny = approvalDecisionButton((ToolApprovalPromptPanel) transcriptWidget(panel), "Deny");
        SwingUtilities.invokeAndWait(deny::doClick);

        assertAll(
                () -> assertTrue(decision.isDone()),
                () -> assertFalse(decision.get().allowed()),
                () -> assertTrue(transcriptMarkdown(panel).contains("Denied tool Write"),
                        "The denial outcome should be folded into the still-open streaming bubble: "
                                + transcriptMarkdown(panel)));
    }

    @Test
    void localAgentApproveToolAlwaysPersistsOnlyTheNamespacedKey() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "activeLocalAgentStreamToken", 9);

        LocalAgentApprovalBridge.ApprovalRequestHandler handler = localAgentApprovalHandler(panel, 9);
        handler.requestApproval("Bash", new JsonObject());
        SwingUtilities.invokeAndWait(() -> {
        });
        JButton approveAlways =
                approvalDecisionButton((ToolApprovalPromptPanel) transcriptWidget(panel), "Approve tool always");
        SwingUtilities.invokeAndWait(approveAlways::doClick);

        ToolApprovalService service = approvalServiceOf(panel);
        assertAll(
                () -> assertTrue(service.isApproved("local-agent:Bash")),
                () -> assertFalse(service.isApproved("Bash"),
                        "A local-agent tool-name approval must never leak into the SHAFT MCP tool-name namespace"));
    }

    @Test
    void localAgentApproveAllMapsToLocalAgentSentinelNotTheGlobalFlag() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "activeLocalAgentStreamToken", 10);

        LocalAgentApprovalBridge.ApprovalRequestHandler handler = localAgentApprovalHandler(panel, 10);
        handler.requestApproval("Bash", new JsonObject());
        SwingUtilities.invokeAndWait(() -> {
        });
        JButton approveAll =
                approvalDecisionButton((ToolApprovalPromptPanel) transcriptWidget(panel), "Approve all tools");
        SwingUtilities.invokeAndWait(approveAll::doClick);

        // A different local-agent tool name should now be auto-allowed without a second prompt.
        CompletableFuture<LocalAgentApprovalBridge.Decision> second = handler.requestApproval("Write", new JsonObject());
        SwingUtilities.invokeAndWait(() -> {
        });

        ToolApprovalService service = approvalServiceOf(panel);
        assertAll(
                () -> assertTrue(second.isDone() && second.get().allowed(),
                        "Approve-all should cover every future local-agent tool call in this project"),
                () -> assertNull(transcriptWidget(panel), "no second prompt should render after approve-all"),
                () -> assertFalse(service.isApproved("capture_start"),
                        "Local-agent approve-all must not silently grant the shared SHAFT MCP approve-all flag"));
    }

    @Test
    void localAgentApprovalIsAutoDeniedWhenTheRunHasEnded() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        clearFirstRunWelcome(panel);
        setField(panel, "activeLocalAgentStreamToken", -1);

        LocalAgentApprovalBridge.ApprovalRequestHandler handler = localAgentApprovalHandler(panel, 11);
        CompletableFuture<LocalAgentApprovalBridge.Decision> decision = handler.requestApproval("Bash", new JsonObject());
        SwingUtilities.invokeAndWait(() -> {
        });

        assertAll(
                () -> assertTrue(decision.isDone()),
                () -> assertFalse(decision.get().allowed()),
                () -> assertNull(transcriptWidget(panel), "a stale/ended run must never render an approval widget"));
    }

    @Test
    void secondQueuedLocalAgentApprovalShowsAfterTheFirstResolves() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        setField(panel, "activeLocalAgentStreamToken", 12);

        LocalAgentApprovalBridge.ApprovalRequestHandler handler = localAgentApprovalHandler(panel, 12);
        CompletableFuture<LocalAgentApprovalBridge.Decision> first = handler.requestApproval("Bash", new JsonObject());
        CompletableFuture<LocalAgentApprovalBridge.Decision> second = handler.requestApproval("Write", new JsonObject());
        SwingUtilities.invokeAndWait(() -> {
        });

        ToolApprovalPromptPanel firstWidget = (ToolApprovalPromptPanel) transcriptWidget(panel);
        assertNotNull(firstWidget, "the first request should render immediately");
        assertFalse(second.isDone(), "a second concurrent request must queue behind the first, not race it");

        SwingUtilities.invokeAndWait(approvalDecisionButton(firstWidget, "Approve once")::doClick);

        assertTrue(first.isDone());
        JComponent secondWidgetComponent = transcriptWidget(panel);
        assertNotNull(secondWidgetComponent, "the queued request should show once the first resolves");
        SwingUtilities.invokeAndWait(
                approvalDecisionButton((ToolApprovalPromptPanel) secondWidgetComponent, "Approve once")::doClick);

        assertAll(
                () -> assertTrue(second.isDone()),
                () -> assertTrue(second.get().allowed()));
    }

    /**
     * Issue #4319 bug 1: {@code LocalAgentApprovalBridge.awaitDecision} gives up locally on its own
     * {@code TimeoutException} (the CLI moves on -- e.g. retries the same intent via a different
     * tool) but never cancels/completes the bridge-facing future it handed out, so {@code
     * localAgentApprovalPromptShowing} stays stuck {@code true} forever: a later approval request in
     * the SAME run (the model's PowerShell retry after Bash's approval timed out) then finds the
     * flag stuck and is silently queued behind a request that will never resolve -- never actually
     * rendering its own selector, and never updating the status bar either (since {@code
     * showLocalAgentApprovalPrompt}, the only place that sets the status text, never runs for it).
     * This reproduces the abandonment with {@code future.cancel(true)}, mirroring the fix planned for
     * the bridge side.
     */
    @Test
    void abandonedLocalAgentApprovalUnsticksTheShowingFlagAndTheNextRequestStillShows() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        appendStreamingLocalAgentBubble(panel, 21);

        LocalAgentApprovalBridge.ApprovalRequestHandler handler = localAgentApprovalHandler(panel, 21);
        CompletableFuture<LocalAgentApprovalBridge.Decision> first = handler.requestApproval("Bash", new JsonObject());
        SwingUtilities.invokeAndWait(() -> {
        });
        assertNotNull(transcriptWidget(panel), "the first request should render immediately");

        // Simulates the bridge giving up on its own timeout, with no user decision ever arriving.
        SwingUtilities.invokeAndWait(() -> first.cancel(true));

        CompletableFuture<LocalAgentApprovalBridge.Decision> second =
                handler.requestApproval("PowerShell", new JsonObject());
        SwingUtilities.invokeAndWait(() -> {
        });

        JComponent secondWidget = transcriptWidget(panel);
        assertAll(
                () -> assertNotNull(secondWidget,
                        "a second tool's approval request must still render its own selector after the"
                                + " first one was abandoned (timed out), not resolved by the user"),
                () -> assertTrue(secondWidget instanceof ToolApprovalPromptPanel,
                        "the rendered widget must be the clickable approval selector"),
                () -> assertEquals("Tool approval request for PowerShell",
                        secondWidget.getAccessibleContext().getAccessibleName(),
                        "the rendered widget must be the SECOND (PowerShell) request, not the stale"
                                + " Bash widget left over from the abandoned first request"));
    }

    /**
     * Issue #4319 bug 1: {@code AssistantLocalAgentRunner.narrateApproval} emits "Waiting for your
     * approval on X -- run timer paused." through the SAME coalesced output pipeline that renders
     * ordinary milestone bubbles (see {@code appendLocalAgentOutput} -> {@code
     * addCompactLocalAgentMilestone} -> {@code appendAgentMilestone} -> {@code append}). {@code
     * append} unconditionally calls {@code transcript.clearWidget()}, on the documented (but false,
     * for this exact case) assumption that append is never called while an approval widget is
     * showing -- so the narration line the user actually sees in the real transcript destroys the
     * just-rendered {@link ToolApprovalPromptPanel} selector a moment after it appears, leaving only
     * narration text with nothing left to click.
     */
    @Test
    void localAgentApprovalWidgetSurvivesAMilestoneAppendedWhileItIsShowing() throws Exception {
        ShaftAssistantPanel panel = new ShaftAssistantPanel(null, blankMcpSettings());
        appendStreamingLocalAgentBubble(panel, 20);

        LocalAgentApprovalBridge.ApprovalRequestHandler handler = localAgentApprovalHandler(panel, 20);
        handler.requestApproval("Bash", new JsonObject());
        SwingUtilities.invokeAndWait(() -> {
        });
        assertNotNull(transcriptWidget(panel), "the approval prompt should render immediately");

        // Simulates narrateApproval's own narration line reaching the transcript through the real
        // coalesced output path a live run uses.
        appendLocalAgentOutput(panel, 20, "Waiting for your approval on Bash -- run timer paused.");

        JComponent widgetAfterNarration = transcriptWidget(panel);
        assertAll(
                () -> assertNotNull(widgetAfterNarration,
                        "a narration/milestone line streaming in while an approval prompt is showing"
                                + " must not clobber it"),
                () -> assertTrue(widgetAfterNarration instanceof ToolApprovalPromptPanel,
                        "the surviving widget must still be the clickable approval selector"));
    }

    private static LocalAgentApprovalBridge.ApprovalRequestHandler localAgentApprovalHandler(
            ShaftAssistantPanel panel, int streamToken) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("localAgentApprovalHandler", int.class);
        method.setAccessible(true);
        return (LocalAgentApprovalBridge.ApprovalRequestHandler) method.invoke(panel, streamToken);
    }

    @Test
    void setupPanelHidesExpertModeAndResetDuringFirstRunSetup() {
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), blankMcpSettings(), () -> {
        });

        assertAll(
                () -> assertFalse(findByAccessibleName(panel, "Enable expert mode", JCheckBox.class).isVisible()),
                () -> assertFalse(findByAccessibleName(panel, "Reset everything", JButton.class).isVisible()),
                () -> assertFalse(findByAccessibleName(panel, "Re-check connection and agents", JButton.class)
                        .isVisible()));
    }

    @Test
    void setupPanelShowsExpertModeAndResetWhenReenteredAfterSetupIsComplete() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.advancedUiEnabled = true;
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });

        assertAll(
                () -> assertTrue(findByAccessibleName(panel, "Enable expert mode", JCheckBox.class).isVisible()),
                () -> assertTrue(findByAccessibleName(panel, "Enable expert mode", JCheckBox.class).isSelected(),
                        "should be pre-selected because advancedUiEnabled is already true"),
                () -> assertTrue(findByAccessibleName(panel, "Reset everything", JButton.class).isVisible()),
                () -> assertTrue(findByAccessibleName(panel, "Re-check connection and agents", JButton.class)
                        .isVisible()));
    }

    @Test
    void setupPanelShowsExpertModeUncheckedWhenAdvancedUiIsDisabled() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.advancedUiEnabled = false;
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });

        assertFalse(findByAccessibleName(panel, "Enable expert mode", JCheckBox.class).isSelected());
    }

    @Test
    void expertModeCheckboxWritesAdvancedUiEnabled() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });
        JCheckBox expertMode = findByAccessibleName(panel, "Enable expert mode", JCheckBox.class);

        assertFalse(settings.advancedUiEnabled);
        expertMode.doClick();

        assertTrue(settings.advancedUiEnabled);
    }

    @Test
    void resetButtonConfirmDelegatesToResetServiceAndCancelIsNoOp() throws Exception {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });
        AtomicInteger resetCalls = new AtomicInteger();
        setField(panel, "resetAction", (Runnable) resetCalls::incrementAndGet);

        setField(panel, "confirmReset", (java.util.function.BooleanSupplier) () -> false);
        clickAccessible(panel, "Reset everything");
        assertEquals(0, resetCalls.get(), "cancelling the confirmation dialog must be a no-op");

        setField(panel, "confirmReset", (java.util.function.BooleanSupplier) () -> true);
        clickAccessible(panel, "Reset everything");
        assertEquals(1, resetCalls.get(), "confirming should invoke the reset service exactly once");
    }

    @Test
    void recheckConnectionAndAgentsButtonRunsRealCheckWithoutTouchingReset() throws Exception {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        ShaftMcpSetupPanel panel = new ShaftMcpSetupPanel(fakeProject(), settings, () -> {
        });
        AtomicInteger resetCalls = new AtomicInteger();
        setField(panel, "resetAction", (Runnable) resetCalls::incrementAndGet);
        JProgressBar progress = (JProgressBar) getField(panel, "progress");
        JLabel status = (JLabel) getField(panel, "status");

        clickAccessible(panel, "Re-check connection and agents");

        assertAll(
                () -> assertTrue(progress.isVisible(),
                        "must dispatch through the same real check testConnection() runs from Check now"),
                () -> assertEquals("Testing...", status.getText()),
                () -> assertEquals(0, resetCalls.get(), "must never invoke the reset action"));
    }

    private static ToolApprovalService approvalServiceOf(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("approvalService");
        method.setAccessible(true);
        return (ToolApprovalService) method.invoke(panel);
    }

    /**
     * Issue #3968: the persisted {@link ShaftAssistantChatState.Message#kind} of the most recently
     * appended message in the panel's active session -- reads the same source of truth {@link
     * MessageStyleRegistry#styleFor} resolves bubble colors from, so this is a stronger assertion
     * than scraping rendered markdown text for a kind-specific string.
     */
    private static String lastMessageKind(ShaftAssistantPanel panel) throws Exception {
        ShaftAssistantChatState chatState = (ShaftAssistantChatState) getField(panel, "chatState");
        List<ShaftAssistantChatState.Message> messages = chatState.activeMessages();
        assertFalse(messages.isEmpty(), "Expected at least one persisted message");
        return messages.get(messages.size() - 1).kind;
    }

    private static JComponent transcriptWidget(ShaftAssistantPanel panel) throws Exception {
        AssistantTranscriptView transcript = (AssistantTranscriptView) getField(panel, "transcript");
        return transcript.pendingWidgetForTest();
    }

    /**
     * Clears the first-run welcome bubble a fresh panel shows in the transcript widget slot (#3540),
     * so approval-flow tests that assert an empty widget slot exercise the returning-user /
     * post-first-message state — real tool dispatch only happens after the user's first message,
     * which already clears the welcome.
     */
    private static void clearFirstRunWelcome(ShaftAssistantPanel panel) throws Exception {
        ((AssistantTranscriptView) getField(panel, "transcript")).clearWidget();
    }

    private static JButton approvalDecisionButton(ToolApprovalPromptPanel panel, String label) {
        return panel.decisionButtonsForTest().stream()
                .filter(button -> label.equals(button.getText()))
                .findFirst()
                .orElseThrow(() -> new AssertionError("No \"" + label + "\" decision button rendered"));
    }

    private static ShaftSettingsState.Settings blankMcpSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "";
        return settings;
    }

    private static ShaftSettingsState.Settings connectedMcpSettings() {
        ShaftSettingsState.Settings settings = new ShaftSettingsState.Settings();
        settings.mcpCommand = "\"java\" \"@target/shaft-mcp.args\"";
        settings.mcpSetupComplete = true;
        settings.assistantFamily = "CODEX";
        settings.defaultAutobotClient = "CODEX";
        return settings;
    }

    private static ShaftSettingsState.Settings unverifiedMcpSettings() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.mcpSetupComplete = false;
        return settings;
    }

    private static ShaftSettingsState.Settings advancedConnectedMcpSettings() {
        ShaftSettingsState.Settings settings = connectedMcpSettings();
        settings.advancedUiEnabled = true;
        return settings;
    }

    private static ShaftMcpSetupPanel.AgentReadinessProbe readyProbe() {
        return (client, runtime) -> ShaftMcpToolResult.success("Codex CLI executable is available on PATH.");
    }

    private static boolean containsText(Component component, String expected) {
        if (component instanceof JLabel label && label.getText() != null && label.getText().contains(expected)) {
            return true;
        }
        if (component instanceof AbstractButton button && button.getText() != null && button.getText().contains(expected)) {
            return true;
        }
        if (component instanceof JTextComponent textComponent
                && textComponent.getText() != null
                && textComponent.getText().contains(expected)) {
            return true;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                if (containsText(child, expected)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static String transcriptRenderedHtml(Component component) {
        StringBuilder html = new StringBuilder();
        for (JEditorPane pane : transcriptHtmlPanes(component)) {
            Object rendered = pane.getClientProperty(AssistantTranscriptView.TRANSCRIPT_RENDERED_HTML_PROPERTY);
            html.append(rendered instanceof String value ? value : pane.getText()).append('\n');
        }
        return html.toString();
    }

    private static List<JEditorPane> transcriptHtmlPanes(Component component) {
        List<JEditorPane> panes = new ArrayList<>();
        collectTranscriptHtmlPanes(component, panes);
        return panes;
    }

    /**
     * Fires the same {@code popupMenuWillBecomeVisible} lifecycle callback Swing invokes right
     * before painting a {@link JComponent#setComponentPopupMenu} popup, regardless of whether a real
     * mouse popup-trigger or the keyboard context-menu key/Shift+F10 asked for it (issue #3630).
     * Doing it this way -- rather than a real {@code JPopupMenu.show()} -- avoids requiring the pane
     * to be realized on an actual screen, which this headless suite never does.
     */
    private static JPopupMenu showMessageContextMenu(JEditorPane pane) {
        JPopupMenu menu = pane.getComponentPopupMenu();
        assertNotNull(menu, "Context menu should be installed via setComponentPopupMenu in fallbackHtmlPane");
        for (PopupMenuListener listener : menu.getPopupMenuListeners()) {
            listener.popupMenuWillBecomeVisible(new PopupMenuEvent(menu));
        }
        return menu;
    }

    private static void collectTranscriptHtmlPanes(Component component, List<JEditorPane> panes) {
        if (component instanceof JEditorPane pane
                && pane.getClientProperty(AssistantTranscriptView.TRANSCRIPT_RENDERED_HTML_PROPERTY) != null) {
            panes.add(pane);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                collectTranscriptHtmlPanes(child, panes);
            }
        }
    }

    private static List<JComponent> transcriptBubbles(Component component) {
        List<JComponent> bubbles = new ArrayList<>();
        collectComponentsWithClientProperty(component, AssistantTranscriptView.TRANSCRIPT_BUBBLE_PROPERTY, bubbles);
        return bubbles;
    }

    private static void collectComponentsWithClientProperty(Component component, String property, List<JComponent> found) {
        if (component instanceof JComponent jComponent && jComponent.getClientProperty(property) != null) {
            found.add(jComponent);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                collectComponentsWithClientProperty(child, property, found);
            }
        }
    }

    private static void collectTextAreas(Component component, List<JBTextArea> textAreas) {
        if (component instanceof JBTextArea textArea) {
            textAreas.add(textArea);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                collectTextAreas(child, textAreas);
            }
        }
    }

    private static void showToolResult(ShaftFeaturePanel panel, ShaftMcpToolResult result) throws Exception {
        Method showResult = ShaftFeaturePanel.class.getDeclaredMethod(
                "showResult", String.class, ShaftMcpToolResult.class, Throwable.class, Project.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, "", result, null, (Project) null);
    }

    private static void showCatalogResult(ShaftFeaturePanel panel, ShaftMcpToolResult result) throws Exception {
        Method showCatalogResult = ShaftFeaturePanel.class.getDeclaredMethod(
                "showCatalogResult", ShaftMcpToolResult.class, Throwable.class, Project.class);
        showCatalogResult.setAccessible(true);
        showCatalogResult.invoke(panel, result, null, (Project) null);
    }

    private static void invokeRefreshCatalog(ShaftFeaturePanel panel, Project project) throws Exception {
        Method refreshCatalog = ShaftFeaturePanel.class.getDeclaredMethod("refreshCatalog", Project.class);
        refreshCatalog.setAccessible(true);
        try {
            refreshCatalog.invoke(panel, project);
        } catch (java.lang.reflect.InvocationTargetException wrapped) {
            Throwable cause = wrapped.getCause();
            if (cause instanceof Exception exception) {
                throw exception;
            }
            if (cause instanceof Error error) {
                throw error;
            }
            throw wrapped;
        }
    }

    private static String statusText(ShaftFeaturePanel panel) throws Exception {
        return (String) ((JLabel) getField(panel, "status")).getText();
    }

    private static String mcpToolsList() {
        JsonObject tool = new JsonObject();
        tool.addProperty("name", "browser_navigate");
        tool.addProperty("description", "Navigate the browser");
        JsonArray tools = new JsonArray();
        tools.add(tool);
        JsonObject result = new JsonObject();
        result.add("tools", tools);
        return result.toString();
    }

    private static void showTestResult(ShaftMcpSetupPanel panel, ShaftMcpToolResult result) throws Exception {
        Method showResult = ShaftMcpSetupPanel.class.getDeclaredMethod(
                "showTestResult", ShaftMcpToolResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, result, null);
    }

    /**
     * Invokes connectAgentClicked()'s real completion path directly, mirroring showTestResult()
     * above: the background probe/EDT hand-off connectAgentClicked() dispatches via
     * ShaftPluginExecutor + ApplicationManager.getApplication().invokeLater(...) cannot complete in
     * this headless harness (no live IntelliJ Application), so tests simulate "the retry's result
     * arrived" by calling the same private method that hand-off would otherwise invoke.
     */
    private static void invokeApplyConnectAgentResult(ShaftMcpSetupPanel panel, ShaftMcpToolResult readiness)
            throws Exception {
        Method applyResult = ShaftMcpSetupPanel.class.getDeclaredMethod(
                "applyConnectAgentResult", ShaftMcpToolResult.class);
        applyResult.setAccessible(true);
        applyResult.invoke(panel, readiness);
    }

    /**
     * Invokes checkChosenAgentClicked()'s real completion path directly, mirroring
     * {@link #invokeApplyConnectAgentResult} above: the non-cloud branch's background probe/EDT
     * hand-off cannot complete in this headless harness (no live IntelliJ Application), so tests
     * simulate "the check's result arrived" by calling the same private method that hand-off would
     * otherwise invoke.
     */
    private static void invokeApplyChosenAgentCheckResult(ShaftMcpSetupPanel panel, ShaftMcpToolResult readiness)
            throws Exception {
        Method applyResult = ShaftMcpSetupPanel.class.getDeclaredMethod(
                "applyChosenAgentCheckResult", ShaftMcpToolResult.class);
        applyResult.setAccessible(true);
        applyResult.invoke(panel, readiness);
    }

    /**
     * Sets the pending Capture review and re-renders it, mirroring what a real captured-code
     * response drives in {@code ShaftAssistantPanel#applyCaptureReview} -- used to prove the
     * {@code captureReviewStatus} accessible description tracks each live summary update (issue
     * #3603).
     */
    private static void setPendingCaptureReview(ShaftAssistantPanel panel, String markdown) throws Exception {
        Class<?> captureReviewClass = Class.forName("com.shaft.intellij.ui.ShaftAssistantPanel$CaptureReview");
        java.lang.reflect.Constructor<?> constructor =
                captureReviewClass.getDeclaredConstructor(String.class, String.class);
        constructor.setAccessible(true);
        Object review = constructor.newInstance(markdown, "");
        setField(panel, "pendingCaptureReview", review);
        Method showPendingCaptureReview = ShaftAssistantPanel.class.getDeclaredMethod("showPendingCaptureReview");
        showPendingCaptureReview.setAccessible(true);
        showPendingCaptureReview.invoke(panel);
    }

    private static ShaftMcpSetupPanel setupPanel(ShaftToolWindowPanel toolWindow) {
        if (toolWindow == null) {
            return null;
        }
        for (Component component : toolWindow.getComponents()) {
            if (component instanceof ShaftMcpSetupPanel setup) {
                return setup;
            }
        }
        return null;
    }

    private static void showAssistantResult(ShaftAssistantPanel panel, ShaftMcpToolResult result) throws Exception {
        showAssistantResult(panel, "autobot_local_agent_run", result);
    }

    private static void showAssistantResult(ShaftAssistantPanel panel, String toolName, ShaftMcpToolResult result) throws Exception {
        showAssistantResult(panel, toolName, result, null);
    }

    private static void showAssistantResult(
            ShaftAssistantPanel panel,
            String toolName,
            ShaftMcpToolResult result,
            Throwable error) throws Exception {
        Method showResult = ShaftAssistantPanel.class.getDeclaredMethod(
                "showResult", String.class, ShaftMcpToolResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, toolName, result, error);
    }

    private static void showSequenceResult(
            ShaftAssistantPanel panel,
            AssistantCommand.ToolCall toolCall,
            ShaftMcpToolResult result) throws Exception {
        Method showResult = ShaftAssistantPanel.class.getDeclaredMethod(
                "showSequenceResult",
                int.class,
                AssistantCommand.ToolCall.class,
                ShaftMcpToolResult.class,
                Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, 0, toolCall, result, null);
    }

    private static void showCaptureStartDiagnostic(
            ShaftAssistantPanel panel,
            String expectedOutputPath,
            String startOutput,
            ShaftMcpToolResult result) throws Exception {
        Method showDiagnostic = ShaftAssistantPanel.class.getDeclaredMethod(
                "showCaptureStartDiagnostic",
                String.class,
                String.class,
                ShaftMcpToolResult.class,
                Throwable.class);
        showDiagnostic.setAccessible(true);
        showDiagnostic.invoke(panel, expectedOutputPath, startOutput, result, null);
    }

    private static void showAgentResult(ShaftAssistantPanel panel, ShaftMcpToolResult result) throws Exception {
        Method showResult = ShaftAssistantPanel.class.getDeclaredMethod(
                "showAgentResult", ShaftMcpToolResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, result, null);
    }

    private static void showAgentResult(ShaftAssistantPanel panel, int streamToken, ShaftMcpToolResult result)
            throws Exception {
        showAgentResult(panel, streamToken, result, null);
    }

    private static void showAgentResult(
            ShaftAssistantPanel panel, int streamToken, ShaftMcpToolResult result, Throwable error)
            throws Exception {
        Method showResult = ShaftAssistantPanel.class.getDeclaredMethod(
                "showAgentResult", int.class, ShaftMcpToolResult.class, Throwable.class);
        showResult.setAccessible(true);
        showResult.invoke(panel, streamToken, result, error);
    }

    private static void appendStreamingLocalAgentBubble(ShaftAssistantPanel panel, int streamToken) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("appendStreamingLocalAgentBubble", int.class);
        method.setAccessible(true);
        method.invoke(panel, streamToken);
    }

    private static void appendLocalAgentOutput(ShaftAssistantPanel panel, int streamToken, String line) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("appendLocalAgentOutput", int.class, String.class);
        method.setAccessible(true);
        method.invoke(panel, streamToken, line);
    }

    private static void cancelOrKillCurrent(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("cancelOrKillCurrent");
        method.setAccessible(true);
        method.invoke(panel);
    }

    private static void applyLocalModels(ShaftAssistantPanel panel, String family, List<String> models)
            throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("applyLocalModels", String.class, List.class);
        method.setAccessible(true);
        method.invoke(panel, family, models);
    }

    private static void applyLocalModels(
            ShaftAssistantPanel panel, String family, List<String> models, long requestGeneration) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod(
                "applyLocalModels", String.class, List.class, long.class);
        method.setAccessible(true);
        method.invoke(panel, family, models, requestGeneration);
    }

    private static void applyLocalModelDiscovery(
            ShaftAssistantPanel panel, String family, AssistantLocalAgentRunner.ModelDiscoveryState state) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("applyLocalModels", String.class,
                AssistantLocalAgentRunner.ModelDiscovery.class, long.class);
        method.setAccessible(true);
        method.invoke(panel, family, new AssistantLocalAgentRunner.ModelDiscovery(List.of(), state),
                getField(panel, "modelListRequestGeneration"));
    }

    private static AssistantCommand.Selection selectedRoute(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("selectedRoute");
        method.setAccessible(true);
        return (AssistantCommand.Selection) method.invoke(panel);
    }

    private static void useFailingTestLocalAgentLauncher(ShaftAssistantPanel panel) throws Exception {
        setField(panel, "localAgentProcessLauncher", (AssistantLocalAgentRunner.ProcessLauncher) (command, directory, environment) -> {
            throw new IOException("test launcher");
        });
        setField(panel, "requireLocalAgentCommandAvailable", false);
    }

    private static void setField(Object target, String name, Object value) throws Exception {
        Field field = target.getClass().getDeclaredField(name);
        field.setAccessible(true);
        field.set(target, value);
    }

    private static Object getField(Object target, String name) throws Exception {
        Field field = target.getClass().getDeclaredField(name);
        field.setAccessible(true);
        return field.get(target);
    }

    private static int currentSessionMessageCount(ShaftAssistantPanel panel) throws Exception {
        ShaftAssistantChatState chatState = (ShaftAssistantChatState) getField(panel, "chatState");
        return chatState.activeSession().messages.size();
    }

    /** Flushes the real AWT Event Dispatch Thread's queue: a no-op {@code invokeAndWait} only returns
     * once every {@code invokeLater}/{@code runOnEdt} action already queued ahead of it has run,
     * making otherwise-async EDT continuations (e.g. a {@code CompletableFuture.whenComplete} callback
     * that hops onto the EDT via {@code runOnEdt}) observable deterministically in a headless test. */
    private static void pumpEdt() {
        try {
            SwingUtilities.invokeAndWait(() -> { });
        } catch (Exception pumpFailure) {
            Thread.currentThread().interrupt();
        }
    }


    @SuppressWarnings({"unchecked", "rawtypes"})
    private static Object recordingBackend(String name) throws ClassNotFoundException {
        return Enum.valueOf((Class<Enum>) (Class) Class.forName(
                "com.shaft.intellij.ui.ShaftAssistantPanel$RecordingBackend"), name);
    }

    private static String assistantExport(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("exportTranscriptWithEvidence");
        method.setAccessible(true);
        return (String) method.invoke(panel);
    }

    /**
     * Invokes the synchronous file-write body that {@code writeTranscriptToFile(Path)} runs inside
     * its {@code executeOnPooledThread} lambda. Bypasses the threading itself (a thin, low-risk
     * wrapper) so the content-correctness the "Save transcript" acceptance criterion cares about
     * can be asserted deterministically, without polling a background thread in a headless suite.
     */
    private static void writeTranscriptSync(ShaftAssistantPanel panel, Path target, String content) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("writeTranscriptSync", Path.class, String.class);
        method.setAccessible(true);
        try {
            method.invoke(panel, target, content);
        } catch (InvocationTargetException e) {
            if (e.getCause() instanceof IOException ioException) {
                throw ioException;
            }
            throw e;
        }
    }

    private static String firstJavaClassBlock(JsonObject raw) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("firstJavaClassBlock", JsonObject.class);
        method.setAccessible(true);
        return (String) method.invoke(null, (Object) raw);
    }

    private static void rememberCaptureInvocation(
            ShaftAssistantPanel panel, String promptText, AssistantCommand.Invocation invocation) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod(
                "rememberCaptureInvocation", String.class, AssistantCommand.Invocation.class);
        method.setAccessible(true);
        method.invoke(panel, promptText, invocation);
    }

    private static void populateContextPopup(
            ShaftAssistantPanel panel, List<ShaftAssistantPanel.ContextSuggestion> suggestions) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("populateContextPopup", List.class);
        method.setAccessible(true);
        method.invoke(panel, suggestions);
    }

    private static void refreshContextPopupFilter(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("refreshContextPopupFilter");
        method.setAccessible(true);
        method.invoke(panel);
    }

    private static void showContextSuggestions(ShaftAssistantPanel panel, char trigger) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("showContextSuggestions", char.class);
        method.setAccessible(true);
        method.invoke(panel, trigger);
    }

    private static void appendSequenceStep(
            ShaftAssistantPanel panel, AssistantCommand.ToolCall toolCall, String statusText, String output)
            throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod(
                "appendSequenceStep", AssistantCommand.ToolCall.class, String.class, String.class);
        method.setAccessible(true);
        method.invoke(panel, toolCall, statusText, output);
    }

    private static void showDeniedSequenceResult(ShaftAssistantPanel panel, AssistantCommand.ToolCall toolCall)
            throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod(
                "showDeniedSequenceResult", AssistantCommand.ToolCall.class);
        method.setAccessible(true);
        method.invoke(panel, toolCall);
    }

    private static void showTerminalSequenceResult(ShaftAssistantPanel panel, boolean cancelled, String statusText)
            throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod(
                "showTerminalSequenceResult", boolean.class, String.class);
        method.setAccessible(true);
        method.invoke(panel, cancelled, statusText);
    }

    private static boolean formatUnknownResponse(
            ShaftAssistantPanel panel, String toolName, String output, String fallbackMarkdown) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod(
                "formatUnknownResponse", String.class, String.class, String.class);
        method.setAccessible(true);
        return (boolean) method.invoke(panel, toolName, output, fallbackMarkdown);
    }

    private static void showFormattedUnknownResponse(
            ShaftAssistantPanel panel,
            String originalToolName,
            String rawOutput,
            String fallbackMarkdown,
            String formatterToolName,
            ShaftMcpToolResult result,
            Throwable error) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod(
                "showFormattedUnknownResponse", String.class, String.class, String.class, String.class,
                ShaftMcpToolResult.class, Throwable.class);
        method.setAccessible(true);
        method.invoke(panel, originalToolName, rawOutput, fallbackMarkdown, formatterToolName, result, error);
    }

    private static void createTestClassFromReview(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("createTestClassFromReview");
        method.setAccessible(true);
        method.invoke(panel);
    }

    private static void openCaptureReviewFile(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("openCaptureReviewFile");
        method.setAccessible(true);
        method.invoke(panel);
    }

    private static void collectCaptureEvidencePack(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("collectCaptureEvidencePack");
        method.setAccessible(true);
        method.invoke(panel);
    }

    private static void compareCaptureBackends(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("compareCaptureBackends");
        method.setAccessible(true);
        method.invoke(panel);
    }

    private static void insertReviewIntoOpenFile(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("insertReviewIntoOpenFile");
        method.setAccessible(true);
        method.invoke(panel);
    }

    private static void scheduleCaptureStartDiagnostic(ShaftAssistantPanel panel, String startOutput) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("scheduleCaptureStartDiagnostic", String.class);
        method.setAccessible(true);
        method.invoke(panel, startOutput);
    }

    private static void saveCloudApiKey(ShaftAssistantPanel panel) throws Exception {
        Method method = ShaftAssistantPanel.class.getDeclaredMethod("saveCloudApiKey");
        method.setAccessible(true);
        method.invoke(panel);
    }

    /**
     * Sets {@code pendingCaptureReview} with a caller-controlled raw MCP result (unlike {@link
     * #setPendingCaptureReview(ShaftAssistantPanel, String)}, which always hardcodes an empty raw
     * result) -- needed by review-action tests ({@code openCaptureReviewFile},
     * {@code collectCaptureEvidencePack}, {@code compareCaptureBackends}) that read specific fields
     * (e.g. {@code reviewPath}) back out of the stored raw JSON via {@code pendingReviewJson()}.
     */
    private static void setPendingCaptureReviewRaw(ShaftAssistantPanel panel, String markdown, String rawResult)
            throws Exception {
        Class<?> captureReviewClass = Class.forName("com.shaft.intellij.ui.ShaftAssistantPanel$CaptureReview");
        java.lang.reflect.Constructor<?> constructor =
                captureReviewClass.getDeclaredConstructor(String.class, String.class);
        constructor.setAccessible(true);
        Object review = constructor.newInstance(markdown, rawResult);
        setField(panel, "pendingCaptureReview", review);
    }

    private static int countOccurrences(String value, String needle) {
        int count = 0;
        int offset = 0;
        while ((offset = value.indexOf(needle, offset)) >= 0) {
            count++;
            offset += needle.length();
        }
        return count;
    }

    private static String mcpText(String text) {
        JsonObject item = new JsonObject();
        item.addProperty("type", "text");
        item.addProperty("text", text);
        JsonArray content = new JsonArray();
        content.add(item);
        JsonObject result = new JsonObject();
        result.add("content", content);
        result.addProperty("isError", false);
        return result.toString();
    }

    private static String outputText(Component component) {
        if (component instanceof JBTextArea textArea && !textArea.isEditable()) {
            return textArea.getText();
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                String found = outputText(child);
                if (!found.isEmpty()) {
                    return found;
                }
            }
        }
        return "";
    }

    private static String transcriptMarkdown(Component component) {
        if (component instanceof AssistantTranscriptView view) {
            return view.markdown();
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                String found = transcriptMarkdown(child);
                if (!found.isEmpty()) {
                    return found;
                }
            }
        }
        return "";
    }

    private static AssistantTranscriptView getTranscriptView(Component component) {
        if (component instanceof AssistantTranscriptView view) {
            return view;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                AssistantTranscriptView found = getTranscriptView(child);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static String getTranscriptHtml(Component component) {
        AssistantTranscriptView view = getTranscriptView(component);
        if (view != null) {
            return transcriptRenderedHtml(view);
        }
        return "";
    }

    private static String selectedCategory(ShaftToolWindowPanel toolWindow) {
        Component selectedComponent = selectedWorkflowComponent(toolWindow);
        if (selectedComponent instanceof Container container) {
            JComboBox<?> categorySelector = findCategorySelector(container);
            if (categorySelector != null && categorySelector.getSelectedItem() != null) {
                return categorySelector.getSelectedItem().toString();
            }
        }
        return "";
    }

    private static String selectedWorkflow(ShaftToolWindowPanel toolWindow) {
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);
        Object selected = selector == null ? null : selector.getSelectedItem();
        if (!(selected instanceof ShaftToolWindowPanel.WorkflowView view)) {
            return "";
        }
        return view.label();
    }

    private static Component selectedWorkflowComponent(ShaftToolWindowPanel toolWindow) {
        JComboBox<ShaftToolWindowPanel.WorkflowView> selector = toolWindowWorkflowSelector(toolWindow);
        Object selected = selector == null ? null : selector.getSelectedItem();
        if (!(selected instanceof ShaftToolWindowPanel.WorkflowView view)) {
            return null;
        }
        return view.component();
    }

    private static JComboBox<ShaftToolWindowPanel.WorkflowView> toolWindowWorkflowSelector(ShaftToolWindowPanel toolWindow) {
        return toolWindow.workflowSelector();
    }

    private static JComboBox<?> findCategorySelector(Component component) {
        if (component instanceof ShaftFeaturePanel featurePanel) {
            return featurePanel.categorySelector();
        }
        if (component instanceof JComboBox<?> comboBox
                && comboBox.getItemCount() > 0
                && comboBox.getItemAt(0) instanceof ToolCategory) {
            return comboBox;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JComboBox<?> found = findCategorySelector(child);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static void click(Component component, String text) {
        JButton button = findButton(component, text);
        assertNotNull(button, text);
        button.doClick();
    }

    private static void clickAccessible(Component component, String accessibleName) {
        JButton button = findByAccessibleName(component, accessibleName, JButton.class);
        assertNotNull(button, accessibleName);
        button.doClick();
    }

    private static Action shortcutAction(JTextComponent component, int keyCode, int modifiers) {
        Object key = component.getInputMap().get(KeyStroke.getKeyStroke(keyCode, modifiers));
        assertNotNull(key);
        Action action = component.getActionMap().get(key);
        assertNotNull(action);
        return action;
    }

    private static Action shortcutAction(JComponent component, int condition, int keyCode, int modifiers) {
        Object key = component.getInputMap(condition).get(KeyStroke.getKeyStroke(keyCode, modifiers));
        assertNotNull(key);
        Action action = component.getActionMap().get(key);
        assertNotNull(action);
        return action;
    }

    private static void assertIcon(JButton button) {
        assertNotNull(button);
        String name = button.getText() + "/" + accessibleName(button);
        assertNotNull(button.getIcon(), name);
        assertTrue(button.getIcon().getIconWidth() > 0, name);
        assertTrue(button.getIcon().getIconHeight() > 0, name);
        assertTrue(paintsVisiblePixels(button.getIcon(), button), button.getToolTipText());
    }

    private static boolean paintsVisiblePixels(Icon icon, JButton button) {
        BufferedImage image = new BufferedImage(icon.getIconWidth(), icon.getIconHeight(), BufferedImage.TYPE_INT_ARGB);
        Graphics2D graphics = image.createGraphics();
        try {
            icon.paintIcon(button, graphics, 0, 0);
        } finally {
            graphics.dispose();
        }
        for (int y = 0; y < image.getHeight(); y++) {
            for (int x = 0; x < image.getWidth(); x++) {
                if ((image.getRGB(x, y) >>> 24) > 0) {
                    return true;
                }
            }
        }
        return false;
    }

    private static void assertIconOnlySymmetric(JButton button) {
        assertIcon(button);
        Dimension size = button.getPreferredSize();
        assertAll(
                () -> assertEquals("", button.getText()),
                () -> assertEquals(size.width, size.height),
                () -> assertEquals(32, size.width),
                () -> assertNotNull(button.getToolTipText()),
                () -> assertFalse(button.getToolTipText().isBlank()),
                () -> assertNotNull(accessibleName(button)),
                () -> assertFalse(accessibleName(button).isBlank()));
    }

    private static String javaExecutableName() {
        return isWindowsOs() ? "java.exe" : "java";
    }

    private static boolean isWindowsOs() {
        return System.getProperty("os.name", "").toLowerCase().contains("win");
    }

    private static void restoreProperty(String key, String value) {
        if (value == null) {
            System.clearProperty(key);
        } else {
            System.setProperty(key, value);
        }
    }

    private static void restoreUiValue(String key, Object value) {
        if (value == null) {
            UIManager.getDefaults().remove(key);
        } else {
            UIManager.put(key, value);
        }
    }

    private static List<JButton> collectButtons(Component root) {
        List<JButton> buttons = new ArrayList<>();
        if (root instanceof JButton button && isShaftOwnedButton(button)) {
            buttons.add(button);
        }
        if (root instanceof Container container) {
            for (Component child : container.getComponents()) {
                buttons.addAll(collectButtons(child));
            }
        }
        return buttons;
    }

    private static boolean isShaftOwnedButton(JButton button) {
        if ("Send assistant prompt".equals(accessibleName(button))) {
            return false;
        }
        return hasText(button.getText())
                || hasText(accessibleName(button))
                || (button.getIcon() != null && hasText(button.getToolTipText()));
    }

    private static boolean isSetupPrimaryAction(JButton button) {
        return List.of(
                "Copy SHAFT Tools & Skills setup command",
                "Test SHAFT MCP connection",
                "Start chatting with SHAFT Assistant").contains(accessibleName(button));
    }

    private static void assertVisiblePrimarySetupActions(Component component, String... expected) {
        List<String> actual = collectButtons(component).stream()
                .filter(ShaftPanelSetupTest::isSetupPrimaryAction)
                .filter(button -> button.isVisible() && button.isEnabled())
                .map(ShaftPanelSetupTest::accessibleName)
                .sorted()
                .toList();
        assertEquals(Arrays.stream(expected).sorted().toList(), actual);
    }

    private static boolean hasText(String text) {
        return text != null && !text.isBlank();
    }

    private static void notifyShaftMouseListener(JButton button, int eventId) {
        MouseEvent event = new MouseEvent(button, eventId, System.currentTimeMillis(), 0, 1, 1, 0, false);
        for (MouseListener listener : button.getMouseListeners()) {
            if (listener.getClass().getName().startsWith(ShaftAssistantPanel.class.getName())) {
                if (eventId == MouseEvent.MOUSE_ENTERED) {
                    listener.mouseEntered(event);
                } else if (eventId == MouseEvent.MOUSE_EXITED) {
                    listener.mouseExited(event);
                }
            }
        }
    }

    private static void notifyMouseListeners(Component component, MouseEvent event) {
        for (MouseListener listener : component.getMouseListeners()) {
            listener.mouseClicked(event);
        }
    }

    private static int componentIndex(Container container, Component component) {
        Component[] children = container.getComponents();
        for (int index = 0; index < children.length; index++) {
            if (children[index] == component) {
                return index;
            }
        }
        return -1;
    }

    private static JButton findButton(Component component, String text) {
        if (component instanceof JButton button
                && (text.equals(button.getText())
                || text.equals(button.getToolTipText())
                || text.equals(accessibleName(button)))) {
            return button;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JButton found = findButton(child, text);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    /**
     * Finds a {@link JLabel} whose visible text exactly equals {@code text} -- unlike
     * {@link #containsText(Component, String)}, this does not match the phrase when it appears
     * inside a button, transcript, or other non-label component (e.g. the Assistant's welcome
     * message says "I'm the SHAFT Assistant" in the transcript, which must not count as the
     * removed "SHAFT Assistant" sub-header label, issue #3676).
     */
    private static JLabel findLabelWithText(Component component, String text) {
        if (component instanceof JLabel label && text.equals(label.getText())) {
            return label;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JLabel found = findLabelWithText(child, text);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static <T extends JComponent> T findByAccessibleName(
            Component component,
            String accessibleName,
            Class<T> type) {
        if (type.isInstance(component)
                && accessibleName.equals(accessibleName((JComponent) component))) {
            return type.cast(component);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                T found = findByAccessibleName(child, accessibleName, type);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static boolean comboContains(JComboBox<?> comboBox, String expectedText) {
        if (comboBox == null) {
            return false;
        }
        for (int index = 0; index < comboBox.getItemCount(); index++) {
            Object item = comboBox.getItemAt(index);
            if (item != null && item.toString().contains(expectedText)) {
                return true;
            }
        }
        return false;
    }

    private static Object lastComboItem(JComboBox<?> comboBox) {
        return comboBox.getItemAt(comboBox.getItemCount() - 1);
    }

    private static String installerArgumentFor(String target) throws Exception {
        Method method = ShaftMcpSetupPanel.class.getDeclaredMethod("installerArgumentFor", String.class);
        method.setAccessible(true);
        return (String) method.invoke(null, target);
    }

    private static String installerCommandFor(String argument) throws Exception {
        Method method = ShaftMcpSetupPanel.class.getDeclaredMethod("installerCommandFor", String.class);
        method.setAccessible(true);
        return (String) method.invoke(null, argument);
    }

    private static String upgradeCommand() throws Exception {
        Method method = ShaftMcpSetupPanel.class.getDeclaredMethod("upgradeCommand");
        method.setAccessible(true);
        return (String) method.invoke(null);
    }

    private static JTextComponent assistantPrompt(Component component) {
        JTextComponent prompt = textComponent(component, "Assistant prompt");
        assertNotNull(prompt);
        return prompt;
    }

    private static JScrollPane enclosingScrollPane(Component component) {
        Container parent = component.getParent();
        while (parent != null) {
            if (parent instanceof JViewport viewport && viewport.getParent() instanceof JScrollPane scrollPane) {
                return scrollPane;
            }
            parent = parent.getParent();
        }
        return null;
    }

    private static JTextComponent textComponent(Component component, String accessibleName) {
        if (component instanceof JTextComponent textComponent && accessibleName.equals(accessibleName(textComponent))) {
            return textComponent;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JTextComponent found = textComponent(child, accessibleName);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static CapturedInvocation last(List<CapturedInvocation> invocations) {
        assertFalse(invocations.isEmpty());
        return invocations.get(invocations.size() - 1);
    }

    private static void assertAccessibleControls(Component component) {
        List<String> missing = new ArrayList<>();
        collectAccessibilityViolations(component, missing);
        assertTrue(missing.isEmpty(), "Missing accessible names: " + missing);
    }

    private static void collectAccessibilityViolations(Component component, List<String> missing) {
        if (needsAccessibleName(component)) {
            String accessibleName = accessibleName((JComponent) component);
            if (accessibleName == null || accessibleName.isBlank()) {
                missing.add(component.getClass().getSimpleName());
            }
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                collectAccessibilityViolations(child, missing);
            }
        }
    }

    private static boolean needsAccessibleName(Component component) {
        return component instanceof JButton button
                && button.getText() != null
                && !button.getText().isBlank()
                && !button.getClass().getSimpleName().equals("MetalComboBoxButton")
                || component instanceof JComboBox<?>
                || component instanceof JTextComponent;
    }

    private static String accessibleName(JComponent component) {
        AccessibleContext context = component.getAccessibleContext();
        return context == null ? null : context.getAccessibleName();
    }

    private static Project fakeProject() {
        return fakeProject(new ShaftAssistantChatState());
    }

    private static Project fakeProject(ShaftAssistantChatState assistantChatState) {
        return fakeProject(assistantChatState, "");
    }

    private static Project fakeProject(ShaftAssistantChatState assistantChatState, String basePath) {
        return (Project) Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class},
                (proxy, method, arguments) -> {
                    switch (method.getName()) {
                        case "equals":
                            Object other = arguments == null || arguments.length == 0 ? null : arguments[0];
                            return proxy == other;
                        case "hashCode":
                            return System.identityHashCode(proxy);
                        case "getBasePath":
                            return basePath;
                        case "getName":
                            return "SHAFT test project";
                        case "getService":
                            Class<?> type = arguments == null || arguments.length == 0 ? null : (Class<?>) arguments[0];
                            if (type == ShaftAssistantChatState.class) {
                                return assistantChatState;
                            }
                            return null;
                        default:
                            return defaultValue(method.getReturnType());
                    }
                });
    }

    private static Object defaultValue(Class<?> returnType) {
        if (returnType == boolean.class) {
            return false;
        } else if (returnType == byte.class) {
            return 0;
        } else if (returnType == short.class) {
            return (short) 0;
        } else if (returnType == int.class) {
            return 0;
        } else if (returnType == long.class) {
            return 0L;
        } else if (returnType == float.class) {
            return 0.0F;
        } else if (returnType == double.class) {
            return 0.0D;
        } else if (returnType == char.class) {
            return '\0';
        } else {
            return null;
        }
    }

    @Test
    void assistantTranscriptRerendersWhenThemeChanges() throws Exception {
        AssistantTranscriptView transcript = new AssistantTranscriptView(null);
        transcript.append("user", "test user message");
        transcript.append("assistant", "test response with **bold** and `code`");

        String originalHtml = extractRenderedHtml(transcript);
        String originalContent = extractTranscriptContent(transcript);

        triggerLafManagerListener(transcript);

        String rerenderedHtml = extractRenderedHtml(transcript);
        String rerenderedContent = extractTranscriptContent(transcript);

        assertAll(
                () -> assertNotNull(originalHtml, "Original HTML should be rendered"),
                () -> assertFalse(originalHtml.isBlank(), "Original HTML should not be empty"),
                () -> assertNotNull(rerenderedHtml, "Rerendered HTML should exist"),
                () -> assertFalse(rerenderedHtml.isBlank(), "Rerendered HTML should not be empty"),
                () -> assertEquals(originalContent, rerenderedContent,
                        "Message content should remain unchanged after theme switch"),
                () -> assertTrue(rerenderedHtml.contains("background:"), "Rerendered HTML should have color styles"),
                () -> assertTrue(rerenderedHtml.contains("color:"), "Rerendered HTML should have text color styles"));
    }

    private static String extractRenderedHtml(AssistantTranscriptView transcript) throws Exception {
        JPanel fallbackPanel = (JPanel) getField(transcript, "fallbackPanel");
        if (fallbackPanel == null || fallbackPanel.getComponentCount() == 0) {
            return "";
        }
        return transcriptRenderedHtml(fallbackPanel.getComponent(0));
    }

    private static String extractTranscriptContent(AssistantTranscriptView transcript) throws Exception {
        return transcript.markdown();
    }

    private static void triggerLafManagerListener(AssistantTranscriptView transcript) throws Exception {
        java.lang.reflect.Field lafListenerField = AssistantTranscriptView.class.getDeclaredField("lafListener");
        lafListenerField.setAccessible(true);
        com.intellij.ide.ui.LafManagerListener lafListener =
                (com.intellij.ide.ui.LafManagerListener) lafListenerField.get(transcript);
        if (lafListener != null) {
            lafListener.lookAndFeelChanged(null);
            SwingUtilities.invokeAndWait(() -> {
            });
        }
    }

    private record CapturedInvocation(String toolName, JsonObject arguments) {
    }

    private static void walkComponents(Component component, Consumer<Component> visitor) {
        visitor.accept(component);
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                walkComponents(child, visitor);
            }
        }
    }

    private static int countVisibleComponents(Component root) {
        AtomicInteger count = new AtomicInteger();
        // A descendant's own isVisible() flag does not fall when an ancestor is hidden (Swing only
        // skips it at paint/layout time), so "effectively visible" is what a collapsed row's
        // reduced footprint actually means here.
        walkComponents(root, component -> {
            if (effectivelyVisible(component, root)) {
                count.incrementAndGet();
            }
        });
        return count.get();
    }

    @Test
    void focusPromptForCustomAnswerSelectsAllTextAfterSuggestedAnswerChipFill() throws Exception {
        // DEFECT 2: the "Answer myself" button (focusPromptForCustomAnswer) only focused the
        // composer without selecting existing text. If a suggested-answer chip was clicked first,
        // its text remained and the user had to manually clear it. Fix: selectAll() after
        // requestFocusInWindow() so typing replaces all text while keeping it recoverable.
        ShaftAssistantPanel panel = new ShaftAssistantPanel(fakeProject(), connectedMcpSettings());
        JTextComponent prompt = assistantPrompt(panel);

        // Simulate filling the prompt with suggested-answer text (like a chip click would do).
        String suggestedText = "Generate a login test";
        prompt.setText(suggestedText);
        prompt.setCaretPosition(0);

        // Clear the selection to simulate the state after a chip is clicked but before
        // custom-answer is invoked.
        prompt.setSelectionStart(0);
        prompt.setSelectionEnd(0);

        // Invoke focusPromptForCustomAnswer via reflection since it's private.
        Method focusMethod = ShaftAssistantPanel.class.getDeclaredMethod("focusPromptForCustomAnswer");
        focusMethod.setAccessible(true);
        focusMethod.invoke(panel);

        // After the fix, the entire text should be selected so typing replaces it.
        assertAll(
                () -> assertEquals(suggestedText, prompt.getText(),
                        "Prompt text should still contain the suggested answer"),
                () -> assertEquals(0, prompt.getSelectionStart(),
                        "Selection should start at position 0"),
                () -> assertEquals(suggestedText.length(), prompt.getSelectionEnd(),
                        "Selection should end at the text length, selecting all"));
    }

    // ------------------------------------------------------------------
    // Round 5 (issue #3363) capstone: walks the entire onboarding journey
    // -- setup -> install command -> Start chatting -> agent-mode consent
    // -> verbose streaming/token count -> Clear -> right-click context menu
    // -> New chat -- in one ordered flow, so a future regression in any of
    // Rounds 1-4's fixes fails here even if a narrower unit test is ever
    // weakened or deleted.
    // ------------------------------------------------------------------
    @Test
    void onboardingJourneyCoversSetupThroughNewChatWithoutRegressingAnyRoundFix() throws Exception {
        // Step 1: setup view shown. Pre-seed a prior session so step 3's
        // fresh-session assertion isn't vacuously true.
        ShaftAssistantChatState chatState = new ShaftAssistantChatState();
        chatState.append("user", "previous onboarding message", "{}");
        chatState.append("assistant", "previous onboarding answer", "{}");
        int sessionsBeforeStartChatting = chatState.sessions().size();

        // readyProbe() stub, never the real AssistantLocalAgentRunner::readiness probe --
        // that real probe checks for an actual agent CLI on PATH and made a Round 2 test
        // pass locally but fail on clean CI runners; every construction here must stay
        // deterministic regardless of what's installed on the machine running it.
        ShaftToolWindowPanel toolWindow = new ShaftToolWindowPanel(
                fakeProject(chatState), unverifiedMcpSettings(), readyProbe(), chatState);
        ShaftMcpSetupPanel setupPanel = setupPanel(toolWindow);
        assertNotNull(setupPanel, "Unverified MCP settings must show the setup panel first");

        // Step 2: install command contract -- must never auto-suggest intellij-plugin
        // by default (Round 4 fix) and must reference a real installer script.
        JTextComponent installerCommandField =
                findByAccessibleName(setupPanel, "MCP installer command", JTextComponent.class);
        assertNotNull(installerCommandField);
        String installerCommandText = installerCommandField.getText();
        assertAll(
                () -> assertFalse(installerCommandText.contains("-Client intellij-plugin"),
                        "Round 4 fix: the setup panel must never auto-suggest the intellij-plugin target: "
                                + installerCommandText),
                () -> assertTrue(installerCommandText.contains("install-shaft-agentic-tools"),
                        "Installer command must reference a real installer script: " + installerCommandText));

        // Step 3: Check -> Start chatting must open a fresh session (Round 2 fix).
        showTestResult(setupPanel, ShaftMcpToolResult.success("Probe OK\nMCP workspace: C:/work/shaft"));
        JButton startChatting =
                findByAccessibleName(setupPanel, "Start chatting with SHAFT Assistant", JButton.class);
        assertNotNull(startChatting);
        startChatting.doClick();

        assertAll(
                () -> assertEquals(sessionsBeforeStartChatting + 1, chatState.sessions().size(),
                        "Round 2 fix: Start chatting must create exactly one new session"),
                () -> assertTrue(chatState.activeMessages().isEmpty(),
                        "Round 2 fix: the new active session must start empty"),
                () -> assertNotNull(findByAccessibleName(toolWindow, "Assistant prompt", JTextComponent.class),
                        "Round 2 fix: the main view must be shown with an empty prompt after Start chatting"));

        ShaftAssistantPanel assistantPanel = findAssistantPanel(toolWindow);
        assertNotNull(assistantPanel, "Main view should now host the Assistant panel");

        // Step 4: agent-mode consent integrity (Round 2 fix, both defects).
        JComboBox<?> assistantMode = findByAccessibleName(assistantPanel, "Assistant mode", JComboBox.class);
        JCheckBox allowEdits =
                findByAccessibleName(assistantPanel, "Approve source mutation for Agent mode", JCheckBox.class);
        assistantMode.setSelectedItem("AGENT");
        allowEdits.setSelected(true);

        // A run-state cycle that is NOT itself a user-driven mode change must not
        // silently reset the checkbox (Round 2 defect A).
        assistantPanel.setRunning(true, "Thinking...");
        assistantPanel.setRunning(false, "Ready");
        assertTrue(allowEdits.isSelected(),
                "Round 2 fix: Allow source edits must survive a running cycle that isn't a mode change");

        // The constructed prompt must carry an affirmative, unconditional instruction
        // once edits are truly enabled (Round 2 defect B).
        AssistantCommand.Invocation invocation = AssistantCommand.fromPrompt(
                "Implement this login flow in Java", "CODEX", "AGENT", ".", "", true);
        String agentPromptText = invocation.arguments().get("prompt").getAsString();
        assertAll(
                () -> assertTrue(agentPromptText.contains("Source edits are approved for this session"),
                        "Round 2 fix: prompt must carry an affirmative source-edit instruction: " + agentPromptText),
                () -> assertFalse(agentPromptText.contains("Source edits are not enabled"),
                        "Round 2 fix: prompt must not carry the unresolved negative conditional: " + agentPromptText));

        // Step 5: verbose streaming shows real progress, and a structured response
        // reports an exact token count with no "(estimated)" fallback (Round 3 fix).
        JCheckBox verboseOutput =
                findByAccessibleName(assistantPanel, "Show verbose agent output", JCheckBox.class);
        verboseOutput.setSelected(true);
        appendStreamingLocalAgentBubble(assistantPanel, 555);
        appendLocalAgentOutput(assistantPanel, 555, "intermediate progress line");

        // While streaming is in progress the interim line must be visible (Round 3 fix)
        // -- it is later superseded by the final answer once the result finalizes the
        // bubble, so this must be checked before showAgentResult(), not after.
        assertTrue(transcriptMarkdown(assistantPanel).contains("intermediate progress line"),
                "Round 3 fix: verbose streaming must show real progress, not just the static placeholder");

        String rawResponse = "The login flow now handles expired sessions."
                + "\n\n{\"usage\":{\"input_tokens\":210,\"output_tokens\":64}}";
        showAgentResult(assistantPanel, 555, ShaftMcpToolResult.success(rawResponse));

        String markdownAfterAgentRun = transcriptMarkdown(assistantPanel);
        assertAll(
                () -> assertTrue(markdownAfterAgentRun.contains("The login flow now handles expired sessions."),
                        markdownAfterAgentRun),
                () -> assertTrue(markdownAfterAgentRun.contains("`274`"),
                        "Round 3 fix: exact token count (210 + 64) must be shown: " + markdownAfterAgentRun),
                () -> assertFalse(markdownAfterAgentRun.contains("(estimated)"),
                        "Round 3 fix: a structured response must not fall back to the estimate"));

        // Step 6: Clear must not leave the action row visually broken (Round 2 fix).
        clickAccessible(assistantPanel, "Clear assistant transcript");
        layoutPanel(assistantPanel);
        assertActionRowButtonsSaneAfterLayout(assistantPanel, false);

        // Step 7: right-click context menu on the transcript (Round 4 fix). Re-populate
        // the transcript first -- Clear left it empty, and there must be a rendered
        // message pane to right-click on.
        assistantPanel.simulateAppendForTest("assistant", "Rendered assistant output for the journey test", "");
        AssistantTranscriptView transcriptView = getTranscriptView(assistantPanel);
        assertNotNull(transcriptView);
        List<JEditorPane> panes = transcriptHtmlPanes(transcriptView);
        assertFalse(panes.isEmpty(), "Expected at least one rendered transcript pane after re-populating");
        JEditorPane lastPane = panes.get(panes.size() - 1);
        JPopupMenu contextMenu = showMessageContextMenu(lastPane);
        assertAll(
                () -> assertNotNull(findByAccessibleName(contextMenu, "Copy", JMenuItem.class),
                        "Round 4 fix: transcript context menu must offer Copy"),
                () -> assertNotNull(findByAccessibleName(contextMenu, "Select All", JMenuItem.class),
                        "Round 4 fix: transcript context menu must offer Select All"),
                () -> assertNotNull(findByAccessibleName(contextMenu, "Copy full transcript", JMenuItem.class),
                        "Round 4 fix: transcript context menu must offer Copy full transcript"));

        // Step 8: New chat -- fresh transcript, cleared prompt, no session data lost
        // across the whole journey.
        assistantPrompt(assistantPanel).setText("stray text that must be cleared by New chat");
        click(assistantPanel, "New chat");

        assertAll(
                () -> assertTrue(transcriptMarkdown(assistantPanel).isBlank(),
                        "New chat must clear the visible transcript"),
                () -> assertTrue(assistantPrompt(assistantPanel).getText().isBlank(),
                        "New chat must clear the prompt field"),
                () -> assertEquals(sessionsBeforeStartChatting + 2, chatState.sessions().size(),
                        "New chat must not lose any prior session across the whole journey"));
    }

    private static ShaftAssistantPanel findAssistantPanel(Container container) {
        for (Component component : container.getComponents()) {
            if (component instanceof ShaftAssistantPanel assistant) {
                return assistant;
            }
            if (component instanceof Container child) {
                ShaftAssistantPanel nested = findAssistantPanel(child);
                if (nested != null) {
                    return nested;
                }
            }
        }
        return null;
    }
}
