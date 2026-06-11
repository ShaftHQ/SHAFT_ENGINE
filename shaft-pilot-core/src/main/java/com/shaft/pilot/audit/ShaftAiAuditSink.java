package com.shaft.pilot.audit;

import com.shaft.driver.SHAFT;

/**
 * Writes safe audit metadata through SHAFT logging.
 */
public final class ShaftAiAuditSink implements AiAuditSink {
    /**
     * Records an event without request or response content.
     *
     * @param event audit event
     */
    @Override
    public void record(AiAuditEvent event) {
        SHAFT.Report.log("SHAFT Pilot AI audit: requestId=" + event.requestId()
                + ", purpose=" + event.purpose()
                + ", provider=" + event.provider()
                + ", model=" + event.model()
                + ", status=" + event.status()
                + ", durationMs=" + event.duration().toMillis()
                + ", redaction=\"" + event.redactionSummary() + "\"");
    }
}
