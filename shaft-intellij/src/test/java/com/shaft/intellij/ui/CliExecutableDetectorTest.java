package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pure PATH/PATHEXT lookup used to decide whether {@code claude}/{@code codex} are actually
 * installed before the new CLI terminal actions (issue #3959) try to launch one -- injected
 * PATH/PATHEXT/OS state, same DI shape as {@link SetupPrerequisitesTest}, so this runs headless
 * with no real process spawned.
 */
class CliExecutableDetectorTest {
    @TempDir
    Path binDir;

    @Test
    void bareExecutableOnPosixPathIsFound() throws IOException {
        Files.createFile(binDir.resolve("claude"));

        assertTrue(CliExecutableDetector.isOnPath("claude", binDir.toString(), null, false));
    }

    @Test
    void windowsExecutableNeedsAPathextExtensionMatch() throws IOException {
        Files.createFile(binDir.resolve("claude.cmd"));

        assertTrue(CliExecutableDetector.isOnPath("claude", binDir.toString(), ".COM;.EXE;.CMD", true));
    }

    @Test
    void windowsLookupFallsBackToDefaultExtensionsWhenPathextIsBlank() throws IOException {
        Files.createFile(binDir.resolve("codex.exe"));

        assertTrue(CliExecutableDetector.isOnPath("codex", binDir.toString(), "", true));
    }

    @Test
    void missingExecutableIsNotFound() {
        assertFalse(CliExecutableDetector.isOnPath("claude", binDir.toString(), null, false));
    }

    @Test
    void blankPathEnvIsNotFound() {
        assertFalse(CliExecutableDetector.isOnPath("claude", "", null, false));
    }

    @Test
    void blankExecutableIsNotFound() {
        assertFalse(CliExecutableDetector.isOnPath("", binDir.toString(), null, false));
    }
}
