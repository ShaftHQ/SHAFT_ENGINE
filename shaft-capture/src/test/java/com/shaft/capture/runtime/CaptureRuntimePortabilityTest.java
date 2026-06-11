package com.shaft.capture.runtime;

import com.shaft.capture.control.CaptureControlFiles;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.time.Instant;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
}
