package com.shaft.mcp;

import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStatus;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
        assertTrue(result.codeBlocks().stream()
                .anyMatch(block -> block.id().equals("capture-agent-integration")
                        && block.code().contains("Page Object")));
        assertTrue(Files.readString(result.reviewUiPath()).contains("Playwright Codegen Feature Map"));
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
                    () -> service.start("https://example.test", "chrome", outside.toString(), true));
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
}
