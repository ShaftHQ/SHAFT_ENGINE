package com.shaft.capture.runtime;

import tools.jackson.databind.ObjectMapper;
import com.shaft.capture.model.CaptureReadiness;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureStatusTest {
    @Test
    void exposesReadinessWhileKeepingPreviousConstructorUsable() throws Exception {
        CaptureStatus oldStyle = new CaptureStatus(
                CaptureStatus.State.ACTIVE,
                "session",
                "chrome",
                "https://example.test",
                1,
                List.of(),
                "capture.json",
                false,
                123,
                Instant.parse("2026-01-02T03:04:05Z"));
        CaptureStatus risky = new CaptureStatus(
                CaptureStatus.State.ACTIVE,
                "session",
                "chrome",
                "https://example.test",
                1,
                CaptureReadiness.State.RISKY,
                List.of("Step 1 uses generated positional CSS."),
                "capture.json",
                false,
                123,
                Instant.parse("2026-01-02T03:04:05Z"));

        String json = new ObjectMapper().writeValueAsString(risky);

        assertEquals(CaptureReadiness.State.READY, oldStyle.readiness());
        assertEquals(CaptureReadiness.State.RISKY, risky.readiness());
        assertTrue(json.contains("\"readiness\":\"RISKY\""));
    }
}
