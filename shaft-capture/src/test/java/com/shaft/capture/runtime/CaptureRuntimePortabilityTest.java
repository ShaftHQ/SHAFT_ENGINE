package com.shaft.capture.runtime;

import com.shaft.capture.control.CaptureControlFiles;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureRuntimePortabilityTest {
    @Test
    void roundTripsPortablePathsWithSpacesAndRestrictedControlFiles(@TempDir Path temp) {
        Path runtime = temp.resolve("runtime with spaces");
        Path output = temp.resolve("recordings with spaces").resolve("capture.json");
        CaptureStartRequest request = new CaptureStartRequest(
                "https://example.test/path?token=secret",
                CaptureBrowser.CHROME,
                output,
                runtime,
                false);
        CaptureControlFiles files = new CaptureControlFiles(runtime);

        Path launchRequest = files.writeLaunchRequest(request);
        CaptureStartRequest restored = files.consumeLaunchRequest(launchRequest);
        files.writeToken("local-token");
        files.writeDescriptor(new CaptureControlFiles.ControlDescriptor(43123, ProcessHandle.current().pid()));
        files.writeStatus(new CaptureStatus(
                CaptureStatus.State.ACTIVE,
                "portable",
                "chrome",
                "https://example.test/path?token=%5Bdata%3Atoken%5D",
                3,
                List.of(),
                output.toAbsolutePath().normalize().toString(),
                false,
                ProcessHandle.current().pid(),
                Instant.parse("2026-06-11T10:00:00Z")));

        assertEquals(request, restored);
        assertEquals("local-token", files.readToken());
        assertEquals(43123, files.readDescriptor().port());
        assertEquals(output.toAbsolutePath().normalize().toString(), files.readStatus().outputPath());
        assertTrue(runtime.resolve("status.json").toFile().isFile());
    }

    @Test
    void singleSessionLockWorksWithoutPlatformSpecificShells(@TempDir Path temp) {
        Path runtime = temp.resolve("runtime with spaces");
        try (CaptureSingleSessionLock ignored = CaptureSingleSessionLock.acquire(runtime)) {
            assertThrows(IllegalStateException.class, () -> CaptureSingleSessionLock.acquire(runtime));
        }
        try (CaptureSingleSessionLock ignored = CaptureSingleSessionLock.acquire(runtime)) {
            assertTrue(runtime.resolve("capture.lock").toFile().isFile());
        }
    }

    @Test
    void rejectsUnsupportedFirefoxWithExplicitMessage() {
        IllegalArgumentException exception = assertThrows(
                IllegalArgumentException.class,
                () -> CaptureBrowser.parse("firefox"));

        assertTrue(exception.getMessage().contains("not supported"));
    }

    @Test
    void acceptsPlaywrightBrowserAliasesAndNormalizesCodegenOptions(@TempDir Path temp) {
        CaptureStartOptions options = new CaptureStartOptions(
                "python",
                "data-pw",
                "chrome-beta",
                "Pixel 7",
                "390,844",
                "dark",
                "30.0,31.0",
                true,
                true,
                "state.json",
                "state-out.json",
                "fr-FR",
                "Africa/Cairo",
                "http://proxy.example:8080",
                "localhost,127.0.0.1",
                "target/capture.har",
                "**/*.js",
                Duration.ofSeconds(5),
                "agent-user-agent",
                temp.resolve("profile"));

        assertEquals(CaptureBrowser.CHROME, CaptureBrowser.parse("chromium"));
        assertEquals(CaptureBrowser.CHROME, CaptureBrowser.parse("cr"));
        assertEquals(CaptureBrowser.EDGE, CaptureBrowser.parse("msedge"));
        assertEquals(390, options.viewport().width());
        assertEquals(844, options.viewport().height());
        assertEquals(Duration.ofSeconds(5), options.timeout());
        assertEquals(temp.resolve("profile").toAbsolutePath().normalize(), options.userDataDirectory());
        assertEquals(List.of("data-pw", "data-testid", "data-test", "data-qa"), options.testIdAttributes());
        assertTrue(options.warnings().stream().anyMatch(warning -> warning.contains("SHAFT generates Java TestNG")));
        assertTrue(options.warnings().stream().anyMatch(warning -> warning.contains("HAR capture")));
        assertTrue(options.warnings().stream().anyMatch(warning -> warning.contains("Service-worker")));

        assertNull(CaptureStartOptions.defaults().viewport());
        assertThrows(IllegalArgumentException.class, () -> new CaptureStartOptions(
                "", "", "", "", "wide", "", "", false, false,
                "", "", "", "", "", "", "", "", Duration.ZERO, "", null));
        assertThrows(IllegalArgumentException.class, () -> new CaptureStartOptions(
                "", "", "", "", "", "", "", false, false,
                "", "", "", "", "", "", "", "", Duration.ofMillis(-1), "", null));
    }
}
