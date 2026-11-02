package com.shaft.pilot.audit;

/**
 * Receives safe local audit metadata.
 */
@FunctionalInterface
public interface AiAuditSink {
    /**
     * Records one safe event.
     *
     * @param event audit event
     */
    void record(AiAuditEvent event);
}
