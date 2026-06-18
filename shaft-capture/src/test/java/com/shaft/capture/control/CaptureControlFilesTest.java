package com.shaft.capture.control;

import com.shaft.capture.runtime.CaptureBrowser;
import com.shaft.capture.runtime.CaptureStartRequest;
import com.shaft.capture.runtime.CaptureStatus;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureControlFilesTest {
    @TempDir
    Path temp;

    @Test
    void controlFilesRoundTripStatusDescriptorTokenAndLaunchRequest() {
        CaptureControlFiles files = new CaptureControlFiles(temp);
        CaptureStatus status = new CaptureStatus(
                CaptureStatus.State.STARTING,
                "session",
                "chrome",
                "https://example.test",
                3,
                List.of("warning"),
                temp.resolve("capture.json").toString(),
                false,
                ProcessHandle.current().pid(),
                Instant.parse("2026-01-02T03:04:05Z"));
        CaptureControlFiles.ControlDescriptor descriptor =
                new CaptureControlFiles.ControlDescriptor(12345, ProcessHandle.current().pid());
        CaptureStartRequest request = new CaptureStartRequest(
                "https://example.test",
                CaptureBrowser.CHROME,
                temp.resolve("recordings/capture.json"),
                temp,
                true);

        files.writeStatus(status);
        files.writeDescriptor(descriptor);
        files.writeToken("secret-token");
        Path launchRequest = files.writeLaunchRequest(request);

        assertEquals(temp.toAbsolutePath().normalize(), files.runtimeDirectory());
        assertEquals(status, files.readStatus());
        assertEquals(descriptor, files.readDescriptor());
        assertEquals("secret-token", files.readToken());
        assertTrue(files.hasActiveControl());
        assertEquals(request, files.consumeLaunchRequest(launchRequest));
        assertFalse(Files.exists(launchRequest));

        files.clearActiveControl();
        assertFalse(files.hasActiveControl());
        assertEquals(CaptureStatus.State.STARTING, files.readStatus().state());
    }

    @Test
    void controlFilesRejectInvalidRuntimeAndLaunchMetadata() throws Exception {
        CaptureControlFiles files = new CaptureControlFiles(temp);
        Path outside = Files.createTempFile("capture-launch", ".json");

        assertThrows(IllegalArgumentException.class, () -> new CaptureControlFiles(null));
        assertThrows(IllegalArgumentException.class,
                () -> new CaptureControlFiles.ControlDescriptor(0, ProcessHandle.current().pid()));
        assertThrows(IllegalArgumentException.class, () -> files.consumeLaunchRequest(outside));
        assertThrows(IllegalStateException.class, files::readToken);
    }
}
