package com.shaft.capture.model;

import com.fasterxml.jackson.databind.JsonNode;

import java.time.Instant;
import java.util.List;
import java.util.Map;

/**
 * Fields shared by every captured event.
 *
 * @param sequence stable one-based event order
 * @param timestamp event timestamp
 * @param page page context
 * @param replayStatus replay result
 * @param evidence optional safe evidence references
 * @param extensions forward-compatible extension data
 */
public record EventContext(
        long sequence,
        Instant timestamp,
        PageContext page,
        ReplayStatus replayStatus,
        List<EvidenceReference> evidence,
        Map<String, JsonNode> extensions) {
    /**
     * Replay lifecycle status.
     */
    public enum ReplayStatus {
        NOT_REPLAYED,
        PASSED,
        FAILED,
        SKIPPED,
        UNSUPPORTED
    }

    /**
     * Creates immutable common event context.
     */
    public EventContext {
        if (sequence < 1) {
            throw new IllegalArgumentException("Event sequence must be positive.");
        }
        if (timestamp == null) {
            throw new IllegalArgumentException("Event timestamp is required.");
        }
        if (page == null) {
            throw new IllegalArgumentException("Event page context is required.");
        }
        replayStatus = replayStatus == null ? ReplayStatus.NOT_REPLAYED : replayStatus;
        evidence = ModelSupport.list(evidence);
        extensions = ModelSupport.extensions(extensions);
    }
}
