package com.shaft.mcp;

import com.shaft.capture.model.CaptureReadiness;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MobileRecordingServiceTest {
    @TempDir
    Path temp;

    @Test
    void recordingGeneratesRedactedReplayCodeInsideWorkspace() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("mobile.json");

        service.start(recording.toString(), "native", false);
        service.record(
                "tap",
                locatorStrategy.ACCESSIBILITY_ID,
                "login",
                Map.of(),
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                false);
        service.record(
                "type",
                locatorStrategy.ACCESSIBILITY_ID,
                "username",
                Map.of("value", "secret"),
                "driver.element().type(SHAFT.GUI.Locator.accessibilityId(\"username\"), \"secret\");",
                "driver.element().type(SHAFT.GUI.Locator.accessibilityId(\"username\"), \"<redacted>\");",
                true);

        McpMobileRecordingStatus stopped = service.stop(false);
        McpMobileReplayResult result = service.codeBlocks(recording.toString(), "mobileDriver");
        String code = result.codeBlocks().getFirst().code();

        assertEquals(2, stopped.actionCount());
        assertTrue(Files.isRegularFile(recording));
        assertTrue(code.contains("mobileDriver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));"));
        assertTrue(code.contains("mobileDriver.element().type(SHAFT.GUI.Locator.accessibilityId(\"username\"), \"<redacted>\");"));
        assertFalse(code.contains("\"secret\""));
        assertFalse(result.codeBlocks().getFirst().copyPasteReady());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("redacted")));
    }

    @Test
    void typedValueSensitivityIsClassifiedPerFieldLikeWebCapture() {
        assertTrue(MobileService.isSensitiveTypedValue("password_input", "hunter2"),
                "Password-looking fields must stay redacted");
        assertTrue(MobileService.isSensitiveTypedValue("com.example:id/api_token", "value"),
                "Token-looking fields must stay redacted");
        assertFalse(MobileService.isSensitiveTypedValue("search", "SHAFT Engine"),
                "Ordinary search fields must not be redacted");
        assertFalse(MobileService.isSensitiveTypedValue("username", "shaft.user"),
                "Username fields externalize as ordinary test data, matching web capture");
    }

    @Test
    void recordingCodeBlocksAddMobilePomHandoffBlocks() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("pom-handoff.json");

        service.start(recording.toString(), "native", true);
        service.record(
                "tap",
                locatorStrategy.ACCESSIBILITY_ID,
                "login",
                Map.of(),
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                false);
        service.record(
                "type",
                locatorStrategy.ID,
                "com.example:id/username",
                Map.of("value", "demo"),
                "driver.element().type(SHAFT.GUI.Locator.id(\"com.example:id/username\"), \"demo\");",
                "driver.element().type(SHAFT.GUI.Locator.id(\"com.example:id/username\"), \"<redacted>\");",
                true);
        service.stop(false);

        McpMobileReplayResult result = service.codeBlocks(recording.toString(), "mobileDriver");

        assertEquals(List.of(
                "mobile-replay-method",
                "mobile-pom-locator-inventory",
                "mobile-pom-action-sequence",
                "mobile-page-object-draft"
        ), result.codeBlocks().stream().map(McpCodeBlock::id).toList());

        McpCodeBlock locators = block(result.codeBlocks(), "mobile-pom-locator-inventory");
        assertEquals(McpCodeBlock.Kind.LOCATOR, locators.kind());
        assertTrue(locators.code().contains(
                "// #1 loginLocator -> SHAFT.GUI.Locator.accessibilityId(\"login\")"));
        assertTrue(locators.code().contains(
                "// #2 usernameLocator -> SHAFT.GUI.Locator.id(\"com.example:id/username\")"));
        assertTrue(locators.placement().contains("mobile Page Object"));

        McpCodeBlock actions = block(result.codeBlocks(), "mobile-pom-action-sequence");
        assertEquals(McpCodeBlock.Kind.ACTION, actions.kind());
        assertTrue(actions.code().contains("// Flow: replayMobileJourney"));
        assertTrue(actions.code().contains("mobileDriver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));"));
        assertTrue(actions.placement().contains("mobile page methods"));

        McpCodeBlock pageObject = block(result.codeBlocks(), "mobile-page-object-draft");
        assertEquals(McpCodeBlock.Kind.ACTION, pageObject.kind());
        assertTrue(pageObject.code().contains("public final class MobileJourneyPage"));
        assertTrue(pageObject.code().contains("private final SHAFT.GUI.WebDriver mobileDriver;"));
        assertTrue(pageObject.code().contains("private final By loginLocator"));
        assertTrue(pageObject.code().contains("public MobileJourneyPage replayMobileJourney()"));
        assertTrue(pageObject.code().contains("mobileDriver.element().touch().tap(loginLocator);"));
    }

    @Test
    void recordAtTargetCodeBlocksAddFocusedMobileInsertionSnippets() throws Exception {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("target.json");
        Path target = temp.resolve("LoginPage.java");
        Files.writeString(target, """
                package pages;

                import com.shaft.driver.SHAFT;

                public class LoginPage {
                    private final SHAFT.GUI.WebDriver driver;

                    public LoginPage(SHAFT.GUI.WebDriver driver) {
                        this.driver = driver;
                    }

                    public LoginPage open() {
                        return this;
                    }
                }
                """);

        service.start(recording.toString(), "native", true);
        service.record(
                "tap",
                locatorStrategy.ACCESSIBILITY_ID,
                "login",
                Map.of(),
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                false);
        service.stop(false);

        McpMobileReplayResult result = service.codeBlocks(recording.toString(), "driver", target, "open");

        McpCodeBlock fields = block(result.codeBlocks(), "mobile-target-locator-fields");
        assertEquals(McpCodeBlock.Kind.LOCATOR, fields.kind());
        assertTrue(fields.code().contains("private final By loginLocator = SHAFT.GUI.Locator.accessibilityId(\"login\");"));
        assertTrue(fields.placement().contains("LoginPage.java"));
        assertTrue(fields.copyPasteReady());

        McpCodeBlock snippet = block(result.codeBlocks(), "mobile-target-action-snippet");
        assertEquals(McpCodeBlock.Kind.ACTION, snippet.kind());
        assertTrue(snippet.code().contains("driver.element().touch().tap(loginLocator);"));
        assertTrue(snippet.placement().contains("after open"));
        assertTrue(snippet.copyPasteReady());

        McpCodeBlock preview = block(result.codeBlocks(), "mobile-target-patch-preview");
        assertEquals("PATCH_PREVIEW", preview.kind().name());
        assertTrue(preview.code().contains("LoginPage.java"));
        assertTrue(preview.code().contains("open"));
        assertTrue(preview.code().contains("import org.openqa.selenium.By;"));
        assertTrue(preview.code().contains("private final By loginLocator"));
        assertTrue(preview.code().contains("driver.element().touch().tap(loginLocator);"));
    }

    @Test
    void recordingCodeBlocksRejectPathOutsideWorkspace() throws Exception {
        Path outside = Files.createTempFile("mobile-recording", ".json");
        Files.writeString(outside, "{}");
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service.codeBlocks(outside.toString(), "driver"));

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void coordinateFallbackActionsCarryReplayWarning() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("coordinates.json");

        service.start(recording.toString(), "native", true);
        service.record(
                "tapCoordinates",
                null,
                "",
                Map.of("x", "10", "y", "20"),
                "driver.element().touch().tapByCoordinates(10, 20);",
                "driver.element().touch().tapByCoordinates(10, 20);",
                false);
        service.stop(false);

        McpMobileRecording stored = service.readRecording(recording.toString());
        McpMobileReplayResult replay = service.codeBlocks(recording.toString(), "driver");

        assertTrue(stored.actions().getFirst().warnings().stream()
                .anyMatch(warning -> warning.contains("probably fail")));
        assertTrue(replay.codeBlocks().getFirst().code().contains("probably fail"));
        assertTrue(replay.warnings().stream()
                .anyMatch(warning -> warning.contains("probably fail")));
        McpCodeBlock queue = block(replay.codeBlocks(), "mobile-locator-confidence-queue");
        assertEquals(McpCodeBlock.Kind.INVESTIGATION, queue.kind());
        assertTrue(queue.code().contains("action-1"));
        assertTrue(queue.code().contains("probably fail"));
    }

    @Test
    void statusSpeaksSharedReadinessVocabularyLikeWebCapture() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));

        // Idle (never started) is READY, matching CaptureReadiness.ready().
        assertEquals(CaptureReadiness.State.READY, service.status().readiness());

        Path recording = temp.resolve("readiness.json");
        service.start(recording.toString(), "native", false);
        // A clean recorded step keeps the session READY.
        service.record(
                "tap",
                locatorStrategy.ACCESSIBILITY_ID,
                "login",
                Map.of(),
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));",
                false);
        McpMobileRecordingStatus active = service.status();
        assertTrue(active.active());
        assertEquals(1, active.actionCount());
        assertEquals(CaptureReadiness.State.READY, active.readiness());

        McpMobileRecordingStatus stopped = service.stop(false);
        assertEquals(CaptureReadiness.State.READY, stopped.readiness());
    }

    @Test
    void coordinateFallbackStepsMakeReadinessRisky() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("risky.json");

        service.start(recording.toString(), "native", true);
        service.record(
                "tapCoordinates",
                null,
                "",
                Map.of("x", "10", "y", "20"),
                "driver.element().touch().tapByCoordinates(10, 20);",
                "driver.element().touch().tapByCoordinates(10, 20);",
                false);

        assertEquals(CaptureReadiness.State.RISKY, service.status().readiness());
        assertEquals(CaptureReadiness.State.RISKY, service.stop(false).readiness());
    }

    @Test
    void stoppingWithNoStepsIsBlocked() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("empty.json");

        service.start(recording.toString(), "native", false);
        McpMobileRecordingStatus stopped = service.stop(false);

        assertEquals(0, stopped.actionCount());
        assertEquals(CaptureReadiness.State.BLOCKED, stopped.readiness());

        // Stopping when nothing is active is also BLOCKED (nothing to review).
        assertEquals(CaptureReadiness.State.BLOCKED, service.stop(false).readiness());
    }

    @Test
    void backCompatConstructorDefaultsReadinessToReady() {
        McpMobileRecordingStatus status = new McpMobileRecordingStatus(
                false, temp.resolve("legacy.json"), "native", 3, false, List.of());
        assertEquals(CaptureReadiness.State.READY, status.readiness());
    }

    @Test
    void recordedStepsGetDistinctStableIdsThatSurviveDeleteAndAreNeverReused() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        Path recording = temp.resolve("stable-ids.json");

        service.start(recording.toString(), "native", false);
        McpMobileRecordedAction first = tap(service, "login");
        McpMobileRecordedAction second = tap(service, "next");
        McpMobileRecordedAction third = tap(service, "submit");

        assertEquals("m1", first.stepId());
        assertEquals("m2", second.stepId());
        assertEquals("m3", third.stepId());

        service.deleteStep("m2");
        McpMobileRecording afterDelete = service.readRecording(recording.toString());
        assertEquals(List.of("m1", "m3"),
                afterDelete.actions().stream().map(McpMobileRecordedAction::stepId).toList());
        assertEquals(List.of(1L, 2L),
                afterDelete.actions().stream().map(McpMobileRecordedAction::sequence).toList());

        McpMobileRecordedAction fourth = tap(service, "confirm");
        assertEquals("m4", fourth.stepId(), "A new step must never reuse a deleted step's id");
    }

    @Test
    void deleteStepRemovesTheTargetedStepAndRenumbersRemainingSequences() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        service.start(temp.resolve("delete.json").toString(), "native", false);
        tap(service, "a");
        tap(service, "b");
        tap(service, "c");

        McpMobileRecordingStatus status = service.deleteStep("m2");

        assertEquals(2, status.actionCount());
        List<McpMobileStepSummary> steps = status.steps();
        assertEquals(List.of("m1", "m3"), steps.stream().map(McpMobileStepSummary::stepId).toList());
        assertEquals(List.of(1L, 2L), steps.stream().map(McpMobileStepSummary::sequence).toList());
        assertEquals(List.of("a", "c"), steps.stream().map(McpMobileStepSummary::locatorValue).toList());
    }

    @Test
    void deleteStepIsANoOpWhenInactiveOrForAnUnknownStepId() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));

        McpMobileRecordingStatus inactive = service.deleteStep("m1");
        assertFalse(inactive.active());
        assertTrue(inactive.warnings().stream().anyMatch(warning -> warning.contains("Ignored")));

        service.start(temp.resolve("delete-noop.json").toString(), "native", false);
        tap(service, "a");

        McpMobileRecordingStatus unknown = service.deleteStep("m99");
        assertEquals(1, unknown.actionCount());
        assertEquals(List.of("m1"), unknown.steps().stream().map(McpMobileStepSummary::stepId).toList());
    }

    @Test
    void reorderStepMovesAStepUpOrDownAndRenumbersSequences() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        service.start(temp.resolve("reorder.json").toString(), "native", false);
        tap(service, "a");
        tap(service, "b");
        tap(service, "c");

        McpMobileRecordingStatus movedUp = service.reorderStep("m3", "up");
        assertEquals(List.of("m1", "m3", "m2"),
                movedUp.steps().stream().map(McpMobileStepSummary::stepId).toList());
        assertEquals(List.of(1L, 2L, 3L),
                movedUp.steps().stream().map(McpMobileStepSummary::sequence).toList());

        McpMobileRecordingStatus movedDown = service.reorderStep("m1", "down");
        assertEquals(List.of("m3", "m1", "m2"),
                movedDown.steps().stream().map(McpMobileStepSummary::stepId).toList());
    }

    @Test
    void reorderStepIsANoOpAtListBoundariesAndForUnknownStepIdOrDirection() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        service.start(temp.resolve("reorder-noop.json").toString(), "native", false);
        tap(service, "a");
        tap(service, "b");
        List<String> original = List.of("m1", "m2");

        assertEquals(original, service.reorderStep("m1", "up").steps()
                .stream().map(McpMobileStepSummary::stepId).toList());
        assertEquals(original, service.reorderStep("m2", "down").steps()
                .stream().map(McpMobileStepSummary::stepId).toList());
        assertEquals(original, service.reorderStep("m99", "up").steps()
                .stream().map(McpMobileStepSummary::stepId).toList());
        assertEquals(original, service.reorderStep("m1", "sideways").steps()
                .stream().map(McpMobileStepSummary::stepId).toList());
    }

    @Test
    void statusExposesStepsWithStepIdSequenceActionAndRiskyWarnings() {
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));
        service.start(temp.resolve("steps-status.json").toString(), "native", true);
        tap(service, "login");
        service.record(
                "tapCoordinates",
                null,
                "",
                Map.of("x", "10", "y", "20"),
                "driver.element().touch().tapByCoordinates(10, 20);",
                "driver.element().touch().tapByCoordinates(10, 20);",
                false);

        List<McpMobileStepSummary> steps = service.status().steps();

        assertEquals(2, steps.size());
        assertEquals("m1", steps.get(0).stepId());
        assertEquals(1L, steps.get(0).sequence());
        assertEquals("tap", steps.get(0).action());
        assertFalse(steps.get(0).risky());
        assertTrue(steps.get(0).warnings().isEmpty());

        assertEquals("m2", steps.get(1).stepId());
        assertEquals("tapCoordinates", steps.get(1).action());
        assertTrue(steps.get(1).risky());
        assertTrue(steps.get(1).warnings().stream().anyMatch(warning -> warning.contains("probably fail")));
    }

    @Test
    void preSchema11RecordingsAreBackfilledOnReadAndSupportCodegen() throws Exception {
        Path legacy = temp.resolve("legacy.json");
        Files.writeString(legacy, """
                {
                  "schemaVersion": "1.0",
                  "mode": "native",
                  "startedAt": "2026-01-01T00:00:00Z",
                  "stoppedAt": "",
                  "includeSensitiveValues": false,
                  "actions": [
                    {
                      "sequence": 1,
                      "timestamp": "2026-01-01T00:00:01Z",
                      "action": "tap",
                      "locatorStrategy": "ACCESSIBILITY_ID",
                      "locatorValue": "login",
                      "parameters": {},
                      "javaCode": "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\\"login\\"));",
                      "sensitiveValueStored": true,
                      "warnings": []
                    },
                    {
                      "sequence": 2,
                      "timestamp": "2026-01-01T00:00:02Z",
                      "action": "tap",
                      "locatorStrategy": "ACCESSIBILITY_ID",
                      "locatorValue": "next",
                      "parameters": {},
                      "javaCode": "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\\"next\\"));",
                      "sensitiveValueStored": true,
                      "warnings": []
                    }
                  ],
                  "warnings": []
                }
                """);
        McpMobileRecordingService service = new McpMobileRecordingService(McpWorkspacePolicy.of(temp));

        McpMobileRecording backfilled = service.readRecording(legacy.toString());

        assertEquals(List.of("m1", "m2"),
                backfilled.actions().stream().map(McpMobileRecordedAction::stepId).toList());
        assertEquals(3L, backfilled.nextStepId(),
                "nextStepId must be raised above every backfilled stepId so it is never reused");

        McpMobileReplayResult replay = service.codeBlocks(legacy.toString(), "driver");
        String code = replay.codeBlocks().getFirst().code();
        assertTrue(code.contains("driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"login\"));"));
        assertTrue(code.contains("driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"next\"));"));
    }

    private static McpMobileRecordedAction tap(McpMobileRecordingService service, String locatorValue) {
        return service.record(
                "tap",
                locatorStrategy.ACCESSIBILITY_ID,
                locatorValue,
                Map.of(),
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"" + locatorValue + "\"));",
                "driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId(\"" + locatorValue + "\"));",
                false);
    }

    private static McpCodeBlock block(List<McpCodeBlock> blocks, String id) {
        return blocks.stream()
                .filter(block -> block.id().equals(id))
                .findFirst()
                .orElseThrow(() -> new AssertionError("Missing block " + id));
    }
}
