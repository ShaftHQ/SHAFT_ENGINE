package com.shaft.mcp;

import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureReadiness;
import com.shaft.capture.model.CaptureSession;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PlaywrightServiceTest {
    @TempDir
    Path temp;

    @Test
    void recordingCodeBlocksGenerateCaptureArtifactsAndReportMetadata() throws Exception {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright-capture.json");

        recorder.start(recording.toString(), "playwright", true);
        recorder.record(
                "navigate",
                null,
                "",
                Map.of("url", "https://example.test/login"),
                "driver.browser().navigateToURL(\"https://example.test/login\");",
                "driver.browser().navigateToURL(\"https://example.test/login\");",
                false);
        recorder.record(
                "type",
                locatorStrategy.ID,
                "email",
                Map.of("value", "alice@example.test"),
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"alice@example.test\");",
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"<redacted>\");",
                true);
        recorder.stop(false);

        McpMobileReplayResult result = recorder.codeBlocks(recording.toString(), "page");

        assertTrue(Files.isRegularFile(result.captureSessionPath()));
        assertTrue(Files.isRegularFile(result.sourcePath()));
        assertTrue(Files.isRegularFile(result.reportPath()));
        assertTrue(Files.isRegularFile(result.reviewPath()));
        assertTrue(result.codeBlocks().stream().anyMatch(block -> block.id().equals("capture-full-class")));
        assertTrue(result.report().warnings().stream()
                .anyMatch(warning -> warning.contains("No recorded SHAFT assertion-builder verification")));
        CaptureSession session = new CaptureJsonCodec().read(result.captureSessionPath());
        assertTrue(session.events().getFirst() instanceof CaptureEvent.NavigationEvent);
        assertTrue(session.events().get(1) instanceof CaptureEvent.TypeEvent);
        assertTrue(Files.readString(result.sourcePath()).contains("SHAFT.GUI.Playwright"));
        assertTrue(Files.readString(result.sourcePath()).contains("driver.element().type("));
        assertFalse(Files.readString(result.sourcePath()).contains("alice@example.test"));
        assertTrue(Files.readString(result.testDataPath()).contains("alice@example.test"));
    }

    @Test
    void recordingCodeBlocksGeneratePlaywrightReplayWithoutLeakingRedactedValues() throws Exception {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright.json");

        recorder.start(recording.toString(), "playwright", false);
        recorder.record(
                "type",
                locatorStrategy.ID,
                "email",
                Map.of("value", "alice@example.test"),
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"alice@example.test\");",
                "driver.element().type(SHAFT.GUI.Locator.id(\"email\"), \"<redacted>\");",
                true);
        recorder.stop(false);

        McpMobileReplayResult result = recorder.codeBlocks(recording.toString(), "page");

        assertTrue(Files.isRegularFile(recording));
        String code = result.codeBlocks().getFirst().code();
        assertTrue(code.contains("SHAFT.GUI.Playwright page"));
        assertTrue(code.contains("<redacted>"));
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("redacted")));
        assertFalse(code.contains("alice@example.test"));
        assertTrue(Files.isRegularFile(result.captureSessionPath()));
        assertTrue(Files.isRegularFile(result.sourcePath()));
        assertTrue(Files.isRegularFile(result.testDataPath()));
        assertTrue(Files.isRegularFile(result.reportPath()));
        assertArtifactDoesNotContain(result.captureSessionPath(), "alice@example.test");
        assertArtifactDoesNotContain(result.sourcePath(), "alice@example.test");
        assertArtifactDoesNotContain(result.testDataPath(), "alice@example.test");
        assertArtifactDoesNotContain(result.reportPath(), "alice@example.test");
        assertFalse(result.report().toString().contains("alice@example.test"));
    }

    @Test
    void semanticTypeRecordingCodeBlocksRedactTypedValues() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright-semantic.json");

        recorder.start(recording.toString(), "playwright", false);
        recorder.record(
                "type_semantic",
                null,
                "Password",
                Map.of("value", "super-secret"),
                PlaywrightService.semanticTypeCode("Password", "super-secret"),
                PlaywrightService.semanticTypeCode("Password", "<redacted>"),
                true);
        recorder.stop(false);

        McpMobileReplayResult result = recorder.codeBlocks(recording.toString(), "page");

        String code = result.codeBlocks().getFirst().code();
        assertFalse(result.codeBlocks().getFirst().imports().contains("com.shaft.gui.driver.ShaftLocator"));
        assertTrue(code.contains("SHAFT.GUI.Locator.inputField(\"Password\")"));
        assertTrue(code.contains("Password"));
        assertTrue(code.contains("<redacted>"));
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("redacted")));
        assertFalse(code.contains("super-secret"));
    }

    @Test
    void coordinateFallbackRecordingCarriesReplayWarning() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright-coordinates.json");

        recorder.start(recording.toString(), "playwright", true);
        recorder.record(
                "clickCoordinates",
                null,
                "",
                Map.of("x", "10", "y", "20"),
                "driver.mouse().click(10, 20);",
                "driver.mouse().click(10, 20);",
                false);
        recorder.stop(false);

        McpMobileReplayResult result = recorder.codeBlocks(recording.toString(), "page");

        assertTrue(result.codeBlocks().getFirst().code().contains("probably fail"));
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("probably fail")));
    }

    @Test
    void statusSpeaksSharedReadinessVocabularyLikeWebAndMobile() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));

        // Idle (never started) is READY.
        assertEquals(CaptureReadiness.State.READY, recorder.status().readiness());

        Path recording = temp.resolve("recordings/playwright-readiness.json");
        recorder.start(recording.toString(), "playwright", false);
        recorder.record(
                "click",
                locatorStrategy.CSS,
                "#login",
                Map.of(),
                "driver.element().click(SHAFT.GUI.Locator.hasTagName(\"button\"));",
                "driver.element().click(SHAFT.GUI.Locator.hasTagName(\"button\"));",
                false);
        McpMobileRecordingStatus active = recorder.status();
        assertTrue(active.active());
        assertEquals(1, active.actionCount());
        assertEquals(CaptureReadiness.State.READY, active.readiness());
        assertEquals(CaptureReadiness.State.READY, recorder.stop(false).readiness());
    }

    @Test
    void coordinateFallbackStepsMakeReadinessRisky() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright-risky.json");

        recorder.start(recording.toString(), "playwright", true);
        recorder.record(
                "clickCoordinates",
                null,
                "",
                Map.of("x", "10", "y", "20"),
                "driver.mouse().click(10, 20);",
                "driver.mouse().click(10, 20);",
                false);

        assertEquals(CaptureReadiness.State.RISKY, recorder.status().readiness());
        assertEquals(CaptureReadiness.State.RISKY, recorder.stop(false).readiness());
    }

    @Test
    void stoppingWithNoStepsIsBlocked() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright-empty.json");

        recorder.start(recording.toString(), "playwright", false);
        McpMobileRecordingStatus stopped = recorder.stop(false);

        assertEquals(0, stopped.actionCount());
        assertEquals(CaptureReadiness.State.BLOCKED, stopped.readiness());
        assertEquals(CaptureReadiness.State.BLOCKED, recorder.stop(false).readiness());
    }

    @Test
    void recordedStepsGetDistinctStableIdsThatSurviveDeleteAndAreNeverReused() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright-stable-ids.json");

        recorder.start(recording.toString(), "playwright", false);
        McpMobileRecordedAction first = click(recorder, "login");
        McpMobileRecordedAction second = click(recorder, "next");
        McpMobileRecordedAction third = click(recorder, "submit");

        assertEquals("m1", first.stepId());
        assertEquals("m2", second.stepId());
        assertEquals("m3", third.stepId());

        recorder.deleteStep("m2");
        McpMobileRecording afterDelete = recorder.readRecording(recording.toString());
        assertEquals(List.of("m1", "m3"),
                afterDelete.actions().stream().map(McpMobileRecordedAction::stepId).toList());
        assertEquals(List.of(1L, 2L),
                afterDelete.actions().stream().map(McpMobileRecordedAction::sequence).toList());

        McpMobileRecordedAction fourth = click(recorder, "confirm");
        assertEquals("m4", fourth.stepId(), "A new step must never reuse a deleted step's id");
    }

    @Test
    void deleteStepRemovesTheTargetedStepAndRenumbersRemainingSequences() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        recorder.start(temp.resolve("recordings/playwright-delete.json").toString(), "playwright", false);
        click(recorder, "a");
        click(recorder, "b");
        click(recorder, "c");

        McpMobileRecordingStatus status = recorder.deleteStep("m2");

        assertEquals(2, status.actionCount());
        List<McpMobileStepSummary> steps = status.steps();
        assertEquals(List.of("m1", "m3"), steps.stream().map(McpMobileStepSummary::stepId).toList());
        assertEquals(List.of(1L, 2L), steps.stream().map(McpMobileStepSummary::sequence).toList());
        assertEquals(List.of("a", "c"), steps.stream().map(McpMobileStepSummary::locatorValue).toList());
    }

    @Test
    void deleteStepIsANoOpWhenInactiveOrForAnUnknownStepId() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));

        McpMobileRecordingStatus inactive = recorder.deleteStep("m1");
        assertFalse(inactive.active());
        assertTrue(inactive.warnings().stream().anyMatch(warning -> warning.contains("Ignored")));

        recorder.start(temp.resolve("recordings/playwright-delete-noop.json").toString(), "playwright", false);
        click(recorder, "a");

        McpMobileRecordingStatus unknown = recorder.deleteStep("m99");
        assertEquals(1, unknown.actionCount());
        assertEquals(List.of("m1"), unknown.steps().stream().map(McpMobileStepSummary::stepId).toList());
    }

    @Test
    void reorderStepMovesAStepUpOrDownAndRenumbersSequences() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        recorder.start(temp.resolve("recordings/playwright-reorder.json").toString(), "playwright", false);
        click(recorder, "a");
        click(recorder, "b");
        click(recorder, "c");

        McpMobileRecordingStatus movedUp = recorder.reorderStep("m3", "up");
        assertEquals(List.of("m1", "m3", "m2"),
                movedUp.steps().stream().map(McpMobileStepSummary::stepId).toList());
        assertEquals(List.of(1L, 2L, 3L),
                movedUp.steps().stream().map(McpMobileStepSummary::sequence).toList());

        McpMobileRecordingStatus movedDown = recorder.reorderStep("m1", "down");
        assertEquals(List.of("m3", "m1", "m2"),
                movedDown.steps().stream().map(McpMobileStepSummary::stepId).toList());
    }

    @Test
    void reorderStepIsANoOpAtListBoundariesAndForUnknownStepIdOrDirection() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        recorder.start(temp.resolve("recordings/playwright-reorder-noop.json").toString(), "playwright", false);
        click(recorder, "a");
        click(recorder, "b");
        List<String> original = List.of("m1", "m2");

        assertEquals(original, recorder.reorderStep("m1", "up").steps()
                .stream().map(McpMobileStepSummary::stepId).toList());
        assertEquals(original, recorder.reorderStep("m2", "down").steps()
                .stream().map(McpMobileStepSummary::stepId).toList());
        assertEquals(original, recorder.reorderStep("m99", "up").steps()
                .stream().map(McpMobileStepSummary::stepId).toList());
        assertEquals(original, recorder.reorderStep("m1", "sideways").steps()
                .stream().map(McpMobileStepSummary::stepId).toList());
    }

    @Test
    void statusExposesStepsWithStepIdSequenceActionAndRiskyWarnings() {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        recorder.start(temp.resolve("recordings/playwright-steps-status.json").toString(), "playwright", true);
        click(recorder, "login");
        recorder.record(
                "clickCoordinates",
                null,
                "",
                Map.of("x", "10", "y", "20"),
                "driver.mouse().click(10, 20);",
                "driver.mouse().click(10, 20);",
                false);

        List<McpMobileStepSummary> steps = recorder.status().steps();

        assertEquals(2, steps.size());
        assertEquals("m1", steps.get(0).stepId());
        assertEquals(1L, steps.get(0).sequence());
        assertEquals("click", steps.get(0).action());
        assertFalse(steps.get(0).risky());
        assertTrue(steps.get(0).warnings().isEmpty());

        assertEquals("m2", steps.get(1).stepId());
        assertEquals("clickCoordinates", steps.get(1).action());
        assertTrue(steps.get(1).risky());
        assertTrue(steps.get(1).warnings().stream().anyMatch(warning -> warning.contains("probably fail")));
    }

    @Test
    void recordAtTargetCodeBlocksAddFocusedPlaywrightInsertionSnippets() throws Exception {
        McpPlaywrightRecordingService recorder = new McpPlaywrightRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("recordings/playwright-target.json");
        Path target = temp.resolve("LoginPage.java");
        Files.writeString(target, """
                package pages;

                import com.shaft.driver.SHAFT;

                public class LoginPage {
                    private final SHAFT.GUI.Playwright driver;

                    public LoginPage(SHAFT.GUI.Playwright driver) {
                        this.driver = driver;
                    }

                    public LoginPage open() {
                        return this;
                    }
                }
                """);

        recorder.start(recording.toString(), "playwright", true);
        recorder.record(
                "click",
                locatorStrategy.ID,
                "login",
                Map.of(),
                "driver.element().click(SHAFT.GUI.Locator.id(\"login\"));",
                "driver.element().click(SHAFT.GUI.Locator.id(\"login\"));",
                false);
        recorder.stop(false);

        McpMobileReplayResult result = recorder.codeBlocks(recording.toString(), "driver", target, "open");

        McpCodeBlock snippet = block(result.codeBlocks(), "capture-target-action-snippet");
        assertEquals(McpCodeBlock.Kind.ACTION, snippet.kind());
        assertTrue(snippet.placement().contains("open"));

        McpCodeBlock preview = block(result.codeBlocks(), "capture-target-patch-preview");
        assertEquals("PATCH_PREVIEW", preview.kind().name());
        assertTrue(preview.code().contains("LoginPage.java"));
        assertTrue(preview.code().contains("open"));
    }

    private static McpMobileRecordedAction click(McpPlaywrightRecordingService recorder, String locatorValue) {
        return recorder.record(
                "click",
                locatorStrategy.ID,
                locatorValue,
                Map.of(),
                "driver.element().click(SHAFT.GUI.Locator.id(\"" + locatorValue + "\"));",
                "driver.element().click(SHAFT.GUI.Locator.id(\"" + locatorValue + "\"));",
                false);
    }

    private static McpCodeBlock block(List<McpCodeBlock> blocks, String id) {
        return blocks.stream()
                .filter(block -> block.id().equals(id))
                .findFirst()
                .orElseThrow(() -> new AssertionError("Missing block " + id));
    }

    private static void assertArtifactDoesNotContain(Path path, String rawValue) throws Exception {
        assertFalse(Files.readString(path).contains(rawValue));
    }
}
