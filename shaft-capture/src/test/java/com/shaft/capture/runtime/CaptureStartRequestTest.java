package com.shaft.capture.runtime;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureStartRequestTest {
    @Test
    void nullTargetUrlNormalizesToAboutBlankInsteadOfRejectingTheRequest(@TempDir Path temp) {
        CaptureStartRequest request = new CaptureStartRequest(
                null,
                CaptureBrowser.CHROME,
                temp.resolve("capture.json"),
                temp.resolve("runtime"),
                true);

        assertEquals("about:blank", request.targetUrl());
    }

    @Test
    void blankTargetUrlNormalizesToAboutBlankInsteadOfRejectingTheRequest(@TempDir Path temp) {
        CaptureStartRequest request = new CaptureStartRequest(
                "   ",
                CaptureBrowser.CHROME,
                temp.resolve("capture.json"),
                temp.resolve("runtime"),
                true);

        assertEquals("about:blank", request.targetUrl());
    }

    @Test
    void explicitGarbageUrlIsStillRejected(@TempDir Path temp) {
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> new CaptureStartRequest(
                        "ftp://example.test",
                        CaptureBrowser.CHROME,
                        temp.resolve("capture.json"),
                        temp.resolve("runtime"),
                        true));

        assertTrue(exception.getMessage().contains("http, https, or file"), exception.getMessage());
    }
}
