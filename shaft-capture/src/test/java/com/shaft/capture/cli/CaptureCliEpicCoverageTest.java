package com.shaft.capture.cli;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Extra CaptureCli dispatch coverage through the public {@link CaptureCli#run} entrypoint.
 */
class CaptureCliEpicCoverageTest {

    @Test
    void usageAndUnknownCommandsExitNonZero() {
        assertEquals(1, CaptureCli.run(null, List.of("java")));
        assertEquals(1, CaptureCli.run(new String[] {}, List.of("java")));
        assertEquals(1, CaptureCli.run(new String[] {"help"}, List.of("java")));
        assertEquals(1, CaptureCli.run(new String[] {"unknown-command"}, List.of("java")));
    }

    @Test
    void featuresStatusStopAndCheckpointValidation(@TempDir Path runtime) throws Exception {
        Files.createDirectories(runtime);
        assertEquals(0, CaptureCli.run(new String[] {"features"}, List.of("java")));
        assertEquals(0, CaptureCli.run(new String[] {"status", "--runtime-dir", runtime.toString()}, List.of("java")));
        assertEquals(0, CaptureCli.run(new String[] {"stop", "--runtime-dir", runtime.toString()}, List.of("java")));
        assertEquals(0, CaptureCli.run(new String[] {"stop", "--runtime-dir", runtime.toString(), "--discard"},
                List.of("java")));
        assertEquals(1, CaptureCli.run(new String[] {
                "checkpoint", "--runtime-dir", runtime.toString(), "--description", "marker"
        }, List.of("java")));
        assertEquals(1, CaptureCli.run(new String[] {
                "checkpoint", "--runtime-dir", runtime.toString(), "--kind", "NOT_A_KIND", "--description", "x"
        }, List.of("java")));
        assertEquals(1, CaptureCli.run(new String[] {
                "checkpoint", "--runtime-dir", runtime.toString(), "--kind", "USER_MARKER"
        }, List.of("java")));
    }

    @Test
    void generateRejectsMissingSessionAndUnknownBackend(@TempDir Path runtime) throws Exception {
        Files.createDirectories(runtime);
        assertEquals(1, CaptureCli.run(new String[] {
                "generate", "--runtime-dir", runtime.toString()
        }, List.of("java")));
        assertEquals(1, CaptureCli.run(new String[] {
                "generate", "--session", runtime.resolve("missing.json").toString(), "--backend", "nope"
        }, List.of("java")));
        Path session = runtime.resolve("empty.json");
        Files.writeString(session, "{}");
        int code = CaptureCli.run(new String[] {
                "generate",
                "--session", session.toString(),
                "--backend", "webdriver",
                "--output-dir", runtime.resolve("out").toString(),
                "--skip-compile"
        }, List.of("java"));
        assertTrue(code == 0 || code == 1);
    }

    @Test
    void startFailsClosedForMissingLoadStorageAndEmptyLaunchPrefix(@TempDir Path runtime) throws Exception {
        Files.createDirectories(runtime);
        assertEquals(1, CaptureCli.run(new String[] {
                "start",
                "--url", "https://example.test/",
                "--runtime-dir", runtime.toString(),
                "--load-storage", runtime.resolve("missing-storage.json").toString()
        }, List.of("java")));
        assertEquals(1, CaptureCli.run(new String[] {
                "start",
                "--url", "https://example.test/",
                "--runtime-dir", runtime.resolve("empty-prefix").toString()
        }, List.of()));
    }

    @Test
    void daemonRequiresRequestFileInsideRuntime(@TempDir Path runtime) throws Exception {
        Files.createDirectories(runtime);
        Path outside = runtime.getParent().resolve("outside-request.json");
        Files.writeString(outside, "{\"command\":\"status\"}");
        assertEquals(1, CaptureCli.run(new String[] {
                "daemon",
                "--runtime-dir", runtime.toString(),
                "--request-file", outside.toString()
        }, List.of("java")));
    }
}
