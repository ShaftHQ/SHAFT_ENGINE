package com.shaft.pilot.audit;

import com.shaft.pilot.ai.AiResponseStatus;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.time.Instant;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ShaftAiAuditSinkTest {

    @Test
    void recordAcceptsNormalizedEventsWithoutThrowing() {
        ShaftAiAuditSink sink = new ShaftAiAuditSink();
        AiAuditEvent event = new AiAuditEvent(
                Instant.parse("2026-11-01T00:00:00Z"),
                "req-1",
                "coverage",
                "local",
                "model-x",
                "redacted=1",
                Duration.ofMillis(12),
                AiResponseStatus.SUCCESS,
                1);
        sink.record(event);
        assertEquals(1, event.attemptNumber());
        assertEquals("local", event.provider());
    }

    @Test
    void auditEventRejectsNonPositiveAttemptsAndTruncatesFields() {
        assertThrows(IllegalArgumentException.class,
                () -> new AiAuditEvent(
                        Instant.parse("2026-11-01T00:00:00Z"),
                        "r",
                        "p",
                        "prov",
                        "m",
                        "sum",
                        Duration.ZERO,
                        AiResponseStatus.SUCCESS,
                        0));
        String longId = "x".repeat(200);
        AiAuditEvent event = new AiAuditEvent(
                Instant.parse("2026-11-01T00:00:00Z"),
                longId,
                "purpose",
                "provider",
                "model",
                "summary",
                Duration.ofSeconds(1),
                AiResponseStatus.ERROR);
        assertEquals(128, event.requestId().length());
        assertEquals(1, event.attemptNumber());
    }
}
