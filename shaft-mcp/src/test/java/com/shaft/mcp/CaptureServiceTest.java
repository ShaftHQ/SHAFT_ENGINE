package com.shaft.mcp;

import com.shaft.capture.runtime.CaptureManager;
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
        assertTrue(result.codeBlocks().stream()
                .anyMatch(block -> block.kind() == McpCodeBlock.Kind.FULL_CLASS
                        && block.code().contains("class GoldenSessionTest")));
        assertTrue(result.codeBlocks().stream()
                .anyMatch(block -> block.kind() == McpCodeBlock.Kind.TEST_METHOD
                        && block.placement().contains("browser")));
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
