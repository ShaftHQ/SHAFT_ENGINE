package com.shaft.commandline.command;

import com.shaft.commandline.runtime.CaptureDelegate;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import picocli.CommandLine;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * {@code shaft-cli codegen} delegates to the standalone, deterministic {@code CaptureCli generate}
 * engine (Decision 6: CLI-first for single-session actions, no MCP server). Covers only the new
 * dispatch wiring this command adds -- {@code CaptureGenerator}'s own behavior is shaft-capture's
 * test responsibility.
 */
class CodegenCommandTest {

    @TempDir
    Path temp;

    @Test
    void explicitSessionIsForwardedUnchanged() {
        RecordingDelegate delegate = new RecordingDelegate(0);
        int exit = new CommandLine(new CodegenCommand(delegate, temp.resolve("recordings")))
                .execute("--session", "capture.json", "--backend", "playwright");

        assertEquals(0, exit);
        assertEquals(
                List.of("generate", "--session", "capture.json", "--backend", "playwright"),
                delegate.lastArgs);
    }

    @Test
    void missingSessionDefaultsToTheNewestRecording() throws Exception {
        Path recordings = Files.createDirectories(temp.resolve("recordings"));
        Path older = recordings.resolve("capture-1.json");
        Path newer = recordings.resolve("capture-2.json");
        Files.writeString(older, "{}");
        Files.setLastModifiedTime(older, java.nio.file.attribute.FileTime.fromMillis(1_000));
        Files.writeString(newer, "{}");
        Files.setLastModifiedTime(newer, java.nio.file.attribute.FileTime.fromMillis(2_000));

        RecordingDelegate delegate = new RecordingDelegate(0);
        int exit = new CommandLine(new CodegenCommand(delegate, recordings))
                .execute("--backend", "webdriver");

        assertEquals(0, exit);
        assertEquals(
                List.of("generate", "--backend", "webdriver", "--session", newer.toString()),
                delegate.lastArgs);
    }

    @Test
    void missingSessionAndNoRecordingsFailsFastWithoutDelegating() {
        Path recordings = temp.resolve("recordings-that-do-not-exist");
        CaptureDelegate refusesToRun = (args, launchPrefix) -> {
            fail("codegen must not delegate when no --session can be resolved.");
            return 1;
        };

        StringWriter err = new StringWriter();
        int exit = new CommandLine(new CodegenCommand(refusesToRun, recordings))
                .setErr(new PrintWriter(err, true))
                .execute();

        assertEquals(2, exit);
        assertTrue(err.toString().contains("--session"), "error should mention --session");
        assertFalse(err.toString().isBlank());
    }

    @Test
    void exitCodePassesThroughFromTheDelegate() {
        RecordingDelegate delegate = new RecordingDelegate(1);
        int exit = new CommandLine(new CodegenCommand(delegate, temp.resolve("recordings")))
                .execute("--session", "capture.json");

        assertEquals(1, exit);
    }

    private static final class RecordingDelegate implements CaptureDelegate {
        private final int exitCode;
        private List<String> lastArgs;

        private RecordingDelegate(int exitCode) {
            this.exitCode = exitCode;
        }

        @Override
        public int run(String[] args, List<String> launchPrefix) {
            this.lastArgs = new ArrayList<>(List.of(args));
            return exitCode;
        }
    }
}
