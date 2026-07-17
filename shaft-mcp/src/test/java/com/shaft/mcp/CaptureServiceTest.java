package com.shaft.mcp;

import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.model.PageContext;
import com.shaft.capture.model.RedactionSummary;
import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStatus;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureServiceTest {
    @TempDir
    Path temp;

    @Test
    void codeBlocksToolGeneratesReusableReplaySnippetsInsideWorkspace() throws Exception {
        Path session = temp.resolve("capture.json");
        Files.copy(repositoryRoot().resolve(
                "shaft-capture/src/test/resources/fixtures/golden-session-1.0.json"), session);

        CaptureService service = service();
        McpCaptureReplayResult result;
        try {
            result = service.codeBlocks(
                    session.toString(),
                    temp.resolve("generated").toString(),
                    "generated.capture",
                    "GoldenSessionTest",
                    false,
                    "browser");
        } finally {
            service.close();
        }

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        assertTrue(Files.isRegularFile(result.sourcePath()));
        assertTrue(Files.isRegularFile(result.reviewUiPath()));
        assertTrue(result.codeBlocks().stream()
                .anyMatch(block -> block.kind() == McpCodeBlock.Kind.FULL_CLASS
                        && block.code().contains("class GoldenSessionTest")));
        assertTrue(result.codeBlocks().stream()
                .anyMatch(block -> block.kind() == McpCodeBlock.Kind.TEST_METHOD
                        && block.placement().contains("browser")));
        assertFalse(result.codeBlocks().stream()
                .anyMatch(block -> block.kind() == McpCodeBlock.Kind.PROVIDER_ADVISORY));
        assertTrue(Files.readString(result.reviewUiPath()).contains("Playwright Codegen Feature Map"));
    }

    @Test
    void recordAtTargetCodeBlocksReturnFocusedInsertionBlocksInsideWorkspace() throws Exception {
        Path session = temp.resolve("capture.json");
        new CaptureJsonCodec().write(session, reviewWarningSession());
        Path target = temp.resolve("CheckoutTest.java");
        Files.writeString(target, """
                package tests;

                import com.shaft.driver.SHAFT;

                public class CheckoutTest {
                    private SHAFT.GUI.WebDriver browser;

                    public void replayCheckout() {
                        browser.browser().navigateToURL("https://example.test");
                    }
                }
                """);

        CaptureService service = service();
        McpCaptureReplayResult result;
        try {
            result = service.recordAtTargetCodeBlocks(
                    session.toString(),
                    temp.resolve("generated-target").toString(),
                    "generated.capture",
                    "GoldenSessionTest",
                    false,
                    target.toString(),
                    "replayCheckout",
                    "browser");
        } finally {
            service.close();
        }

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        assertFalse(result.codeBlocks().stream()
                .anyMatch(block -> block.kind() == McpCodeBlock.Kind.PROVIDER_ADVISORY));
        assertTrue(result.codeBlocks().stream()
                .anyMatch(block -> block.id().equals("capture-target-action-snippet")
                        && block.placement().contains("after replayCheckout")));
        McpCodeBlock preview = block(result.codeBlocks(), "capture-target-patch-preview");
        assertEquals("PATCH_PREVIEW", preview.kind().name());
        assertTrue(preview.code().contains("CheckoutTest.java"));
        assertTrue(preview.code().contains("replayCheckout"));
        assertTrue(preview.code().contains("import org.openqa.selenium.By;"));
        assertTrue(preview.code().contains("private final By"));
        assertTrue(preview.code().contains("browser.element().click"));
    }

    @Test
    void playwrightCodeBlocksToolGeneratesPlaywrightPomGuidanceInsideWorkspace() throws Exception {
        Path session = temp.resolve("capture.json");
        Files.copy(repositoryRoot().resolve(
                "shaft-capture/src/test/resources/fixtures/golden-session-1.0.json"), session);

        CaptureService service = service();
        McpCaptureReplayResult result;
        try {
            result = service.playwrightCodeBlocks(
                    session.toString(),
                    temp.resolve("generated-playwright").toString(),
                    "generated.capture",
                    "PlaywrightGoldenSessionTest",
                    false,
                    "page");
        } finally {
            service.close();
        }

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        assertTrue(Files.readString(result.sourcePath()).contains("SHAFT.GUI.Playwright"));
        assertTrue(result.codeBlocks().stream()
                .anyMatch(block -> block.id().equals("capture-test-method")
                        && block.placement().contains("SHAFT.GUI.Playwright")
                        && block.placement().contains("page")));
        assertFalse(result.codeBlocks().stream()
                .anyMatch(block -> block.kind() == McpCodeBlock.Kind.PROVIDER_ADVISORY));
    }

    @Test
    void codeBlocksToolReturnsDeterministicReviewWarnings() throws Exception {
        Path session = temp.resolve("capture-review.json");
        new CaptureJsonCodec().write(session, reviewWarningSession());

        CaptureService service = service();
        McpCaptureReplayResult result;
        try {
            result = service.codeBlocks(
                    session.toString(),
                    temp.resolve("generated-review").toString(),
                    "generated.capture",
                    "ReviewWarningTest",
                    false,
                    "driver");
        } finally {
            service.close();
        }

        assertTrue(result.successful(), result.report().unsupportedEvents().toString());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("review/LOCATOR")),
                result.warnings().toString());
        assertTrue(result.warnings().stream().anyMatch(warning -> warning.contains("review/ASSERTION")),
                result.warnings().toString());
    }

    @Test
    void targetCandidatesToolFindsExistingSuiteAnchorsInsideWorkspace() throws Exception {
        Path page = temp.resolve("src/test/java/pages/LoginPage.java");
        Files.createDirectories(page.getParent());
        Files.writeString(page, """
                package pages;

                import com.shaft.driver.SHAFT;

                public class LoginPage {
                    private final SHAFT.GUI.WebDriver browser;

                    public LoginPage(SHAFT.GUI.WebDriver browser) {
                        this.browser = browser;
                    }

                    public LoginPage open() {
                        return this;
                    }
                }
                """);
        Path test = temp.resolve("src/test/java/tests/CheckoutTest.java");
        Files.createDirectories(test.getParent());
        Files.writeString(test, """
                package tests;

                import com.shaft.driver.SHAFT;

                public class CheckoutTest {
                    private SHAFT.GUI.WebDriver driver;

                    public void replayCheckout() {
                    }
                }
                """);

        CaptureService service = service();
        try {
            var candidates = service.targetCandidates(temp.toString(), 10);

            assertTrue(candidates.stream().anyMatch(candidate -> candidate.className().equals("LoginPage")
                    && candidate.packageName().equals("pages")
                    && candidate.driverVariableName().equals("browser")
                    && candidate.insertionAnchors().contains("open")));
            assertTrue(candidates.stream().anyMatch(candidate -> candidate.className().equals("CheckoutTest")
                    && candidate.insertionAnchors().contains("replayCheckout")));
        } finally {
            service.close();
        }
    }

    @Test
    void backendComparisonToolComposesExistingWebDriverAndPlaywrightBlocks() throws Exception {
        Path session = temp.resolve("capture.json");
        Files.copy(repositoryRoot().resolve(
                "shaft-capture/src/test/resources/fixtures/golden-session-1.0.json"), session);

        CaptureService service = service();
        try {
            var comparison = service.compareCodeBlocks(
                    session.toString(),
                    temp.resolve("generated-comparison").toString(),
                    "generated.capture",
                    "ComparedSessionTest",
                    false,
                    "driver");

            assertEquals(List.of("WEBDRIVER", "PLAYWRIGHT"),
                    comparison.backends().stream().map(CaptureService.CaptureBackendBlocks::backend).toList());
            assertTrue(comparison.backends().stream()
                    .allMatch(backend -> backend.blockIds().contains("capture-full-class")));
            assertTrue(comparison.backends().stream()
                    .anyMatch(backend -> backend.backend().equals("PLAYWRIGHT")
                            && backend.sourcePath().toString().contains("PlaywrightComparedSessionTest")));
        } finally {
            service.close();
        }
    }

    @Test
    void evidencePackToolReturnsReviewManifestWithoutUploadingArtifacts() throws Exception {
        Path session = temp.resolve("capture.json");
        Files.copy(repositoryRoot().resolve(
                "shaft-capture/src/test/resources/fixtures/golden-session-1.0.json"), session);
        CaptureService service = service();
        McpCaptureReplayResult generated;
        try {
            generated = service.codeBlocks(
                    session.toString(),
                    temp.resolve("generated-evidence").toString(),
                    "generated.capture",
                    "EvidenceSessionTest",
                    false,
                    "driver");
            Path screenshot = temp.resolve("screenshots/capture.png");
            Files.createDirectories(screenshot.getParent());
            Files.writeString(screenshot, "png");

            McpEvidencePack pack = service.evidencePack(
                    generated.sourcePath().toString(),
                    generated.reportPath().toString(),
                    generated.reviewUiPath().toString(),
                    List.of(screenshot.toString()));

            assertEquals("1.0", pack.schemaVersion());
            assertTrue(pack.artifactPaths().stream().anyMatch(path -> path.endsWith("EvidenceSessionTest.java")));
            assertTrue(pack.artifactPaths().stream().anyMatch(path -> path.endsWith("generation-report.json")));
            assertTrue(pack.artifactPaths().stream().anyMatch(path -> path.endsWith("capture.png")));
            assertTrue(pack.validationCommands().stream().anyMatch(command -> command.contains("mvn -pl")));
            assertTrue(pack.warnings().stream().noneMatch(warning -> warning.toLowerCase().contains("upload")));
        } finally {
            service.close();
        }
    }

    @Test
    void checkpointToolRequiresActiveCaptureSession() {
        CaptureService service = service();
        IllegalStateException failure;
        try {
            failure = assertThrows(IllegalStateException.class,
                    () -> service.checkpoint("review this state", "USER_MARKER"));
        } finally {
            service.close();
        }

        assertTrue(failure.getMessage().contains("Capture"));
    }

    @Test
    void statusAndStopReturnIdleCaptureStateWhenNoSessionIsActive() {
        CaptureService service = service();
        try {
            assertEquals(CaptureStatus.State.NOT_RUNNING, service.status().state());
            assertEquals(CaptureStatus.State.NOT_RUNNING, service.stop(false).state());
        } finally {
            service.close();
        }
    }

    @Test
    void codeBlocksRejectSessionOutsideWorkspace() throws Exception {
        Path outside = Files.createTempFile("outside-capture", ".json");
        Files.writeString(outside, "{}");

        CaptureService service = service();
        IllegalArgumentException failure;
        try {
            failure = assertThrows(IllegalArgumentException.class,
                    () -> service.codeBlocks(
                            outside.toString(),
                            temp.resolve("generated").toString(),
                            "generated.capture",
                            "OutsideTest",
                            false,
                            "driver"));
        } finally {
            service.close();
        }

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void startRejectsOutputPathOutsideWorkspaceBeforeLaunchingBrowser() throws Exception {
        Path outside = Files.createTempFile("outside-capture-output", ".json");

        CaptureService service = service();
        IllegalArgumentException failure;
        try {
            failure = assertThrows(IllegalArgumentException.class,
                    () -> service.start("https://example.test", "chrome", outside.toString(), true, ""));
        } finally {
            service.close();
        }

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void startWithOptionsRejectsUserDataDirectoryOutsideWorkspaceBeforeLaunchingBrowser() throws Exception {
        Path outside = Files.createTempDirectory("outside-capture-profile");

        CaptureService service = service();
        CaptureService.CaptureCodegenStartRequest request = new CaptureService.CaptureCodegenStartRequest();
        request.targetUrl = "https://example.test";
        request.browser = "chromium";
        request.outputPath = temp.resolve("capture.json").toString();
        request.headless = true;
        request.targetLanguage = "java";
        request.testIdAttribute = "data-pw";
        request.viewportSize = "800,600";
        request.ignoreHttpsErrors = true;
        request.language = "en-US";
        request.timeoutMillis = 1000;
        request.userAgent = "agent";
        request.userDataDirectory = outside.toString();
        IllegalArgumentException failure;
        try {
            failure = assertThrows(IllegalArgumentException.class,
                    () -> service.startWithOptions(request));
        } finally {
            service.close();
        }

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void codegenFeaturesExposePlaywrightInventory() {
        CaptureService service = service();
        try {
            assertTrue(service.codegenFeatures().stream()
                    .anyMatch(feature -> feature.playwrightControl().equals("--test-id-attribute")
                            && feature.shaftSupport().equals("SUPPORTED")));
        } finally {
            service.close();
        }
    }

    @Test
    void replayGenerationRejectsSessionOutsideWorkspaceBeforeRunningGenerator() throws Exception {
        Path outside = Files.createTempFile("outside-capture-session", ".json");
        Files.writeString(outside, "{}");

        CaptureService service = service();
        IllegalArgumentException replayFailure;
        IllegalArgumentException generateFailure;
        try {
            replayFailure = assertThrows(IllegalArgumentException.class,
                    () -> service.generateReplay(
                            outside.toString(),
                            temp.resolve("generated-replay").toString(),
                            "generated.capture",
                            "ReplayTest",
                            false,
                            false,
                            false,
                            false,
                            false,
                            "driver"));
            generateFailure = assertThrows(IllegalArgumentException.class,
                    () -> service.generate(
                            outside.toString(),
                            temp.resolve("generated").toString(),
                            "generated.capture",
                            "GeneratedTest",
                            false,
                            false,
                            false,
                            "",
                            false,
                            false,
                            false,
                            false));
        } finally {
            service.close();
        }

        assertTrue(replayFailure.getMessage().contains("workspace"));
        assertTrue(generateFailure.getMessage().contains("workspace"));
    }

    @Test
    void statusWithoutASessionPointsAtRecentRecordingsInsteadOfDeadEnding() throws Exception {
        Path recordings = Files.createDirectories(temp.resolve("recordings"));
        Path recording = recordings.resolve("intellij-capture-123.json");
        Files.writeString(recording, "{}");

        CaptureService service = service();
        CaptureStatus status;
        try {
            status = service.status();
        } finally {
            service.close();
        }

        assertEquals(CaptureStatus.State.NOT_RUNNING, status.state());
        assertTrue(status.warnings().stream().anyMatch(warning ->
                warning.contains("none is needed to generate code")
                        && warning.contains(recording.toString())));
    }

    @Test
    void statusWithoutASessionOrRecordingsExplainsTheScenarioDrivenPath() {
        CaptureService service = service();
        CaptureStatus status;
        try {
            status = service.status();
        } finally {
            service.close();
        }

        assertEquals(CaptureStatus.State.NOT_RUNNING, status.state());
        assertTrue(status.warnings().stream().anyMatch(warning ->
                warning.contains("capture_start_codegen")
                        && warning.contains("perform the described actions")));
    }

    @Test
    void startAcceptsNullHeadlessAndResolvesItBeforeAnyWorkspaceOrBrowserWork() throws Exception {
        // Issue #3692: capture_start's headless parameter was the last field still marked required
        // in the tool's JSON schema after sessionGoal/targetUrl/browser/outputPath were fixed (a
        // client omitting it hits the same "required property ... not found" failure class the user
        // reported). Boxing it to Boolean and resolving through the existing resolveHeadless helper
        // (already used by apiStart) must not NPE when the caller omits it; the workspace-boundary
        // check below still fires afterward, proving start(...) got past the null-to-boolean
        // resolution safely before ever touching a real browser.
        Path outside = Files.createTempFile("outside-capture-output-null-headless", ".json");

        CaptureService service = service();
        IllegalArgumentException failure;
        try {
            failure = assertThrows(IllegalArgumentException.class,
                    () -> service.start("https://example.test", "chrome", outside.toString(), null, ""));
        } finally {
            service.close();
        }

        assertTrue(failure.getMessage().contains("workspace"));
    }

    @Test
    void codeBlocksToolDefaultsToLatestRecordingWhenSessionPathIsBlank() throws Exception {
        // Issue #3692 item 2: capture_code_blocks previously required an explicit sessionPath; a
        // blank value must now resolve to the most recently modified recording under recordings/,
        // reusing the same lookup status() already uses for its idle guidance.
        Path recordings = Files.createDirectories(temp.resolve("recordings"));
        Files.copy(repositoryRoot().resolve(
                        "shaft-capture/src/test/resources/fixtures/golden-session-1.0.json"),
                recordings.resolve("capture-latest.json"));

        CaptureService service = service();
        McpCaptureReplayResult result;
        try {
            result = service.codeBlocks(
                    "",
                    temp.resolve("generated-default-session").toString(),
                    "generated.capture",
                    "DefaultSessionTest",
                    false,
                    "browser");
        } finally {
            service.close();
        }

        assertTrue(result.successful(),
                result.report() == null ? "no report" : result.report().unsupportedEvents().toString());
        assertTrue(result.codeBlocks().stream()
                .anyMatch(block -> block.kind() == McpCodeBlock.Kind.FULL_CLASS
                        && block.code().contains("class DefaultSessionTest")));
    }

    @Test
    void codeBlocksToolStillRaisesAClearErrorWhenSessionPathIsBlankAndNoRecordingsExist() {
        // The fallback must not silently swallow the "nothing to generate from" case: with no
        // recordings/ directory at all, McpWorkspacePolicy's existing "is required" error still
        // surfaces, unchanged.
        CaptureService service = service();
        IllegalArgumentException failure;
        try {
            failure = assertThrows(IllegalArgumentException.class,
                    () -> service.codeBlocks("", temp.resolve("generated-none").toString(),
                            "generated.capture", "NoRecordingTest", false, "browser"));
        } finally {
            service.close();
        }

        assertTrue(failure.getMessage().contains("required"));
    }

    private CaptureService service() {
        return new CaptureService(
                new CaptureManager(),
                McpWorkspacePolicy.of(temp),
                new McpCaptureCodeBlockService());
    }

    private static Path repositoryRoot() {
        Path current = Path.of("").toAbsolutePath().normalize();
        while (current != null) {
            if (Files.isRegularFile(current.resolve("pom.xml"))
                    && Files.isDirectory(current.resolve("shaft-mcp"))
                    && Files.isDirectory(current.resolve("shaft-capture"))) {
                return current;
            }
            current = current.getParent();
        }
        throw new IllegalStateException("Repository root could not be resolved.");
    }

    private static CaptureSession reviewWarningSession() {
        Instant started = Instant.parse("2026-01-02T03:04:05Z");
        BrowserMetadata browser = new BrowserMetadata("chrome", "137", "Windows 11", "browser-1", Map.of());
        PageContext page = new PageContext("https://shop.example/checkout", "Checkout", "window-1",
                List.of(), 1280, 720);
        ElementSnapshot pay = new ElementSnapshot(
                "pay-button",
                "button",
                "button",
                "Pay now",
                "",
                Map.of("type", "submit"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.XPATH,
                        "/html/body/div[3]/form/button[2]", 1, true, false,
                        Set.of(LocatorCandidate.LocatorSignal.POSITIONAL))),
                true,
                true,
                false);
        EventContext navigation = new EventContext(1, started.plusSeconds(1), page,
                EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of());
        EventContext click = new EventContext(2, started.plusSeconds(2), page,
                EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of());
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "mcp-review-session",
                CaptureSession.SessionStatus.COMPLETED,
                started,
                started.plusSeconds(3),
                browser,
                List.of(
                        new CaptureEvent.NavigationEvent(navigation,
                                CaptureEvent.NavigationAction.OPEN, "https://shop.example/checkout"),
                        new CaptureEvent.ClickEvent(click, pay, CaptureEvent.MouseButton.PRIMARY, 1)),
                List.of(),
                List.of(),
                RedactionSummary.empty(),
                Map.of());
    }

    private static McpCodeBlock block(List<McpCodeBlock> blocks, String id) {
        return blocks.stream()
                .filter(block -> block.id().equals(id))
                .findFirst()
                .orElseThrow(() -> new AssertionError("Missing block " + id));
    }

}
