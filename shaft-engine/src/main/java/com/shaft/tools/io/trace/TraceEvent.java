package com.shaft.tools.io.trace;

import com.shaft.gui.capabilities.AutomationBackend;

import java.time.Instant;
import java.util.List;
import java.util.Map;

/**
 * One ordered SHAFT operation expressed independently from Selenium, Appium, or Playwright APIs.
 */
public record TraceEvent(String id, AutomationBackend backend, String category, String name,
                         TraceEventStatus status, Instant startedAt, long durationMs, String source,
                         String target, String message, Map<String, String> metadata,
                         List<String> artifactIds) {
    public TraceEvent(String id, AutomationBackend backend, String category, String name,
                      TraceEventStatus status, Instant startedAt, long durationMs, String source,
                      String target, String message, Map<String, String> metadata,
                      List<String> artifactIds) {
        this.id = required(id, "id");
        this.backend = java.util.Objects.requireNonNull(backend, "backend");
        this.category = required(category, "category");
        this.name = required(name, "name");
        this.status = java.util.Objects.requireNonNull(status, "status");
        this.startedAt = java.util.Objects.requireNonNull(startedAt, "startedAt");
        if (durationMs < 0) {
            throw new IllegalArgumentException("durationMs must not be negative");
        }
        this.durationMs = durationMs;
        this.source = source == null ? "" : source;
        this.target = target == null ? "" : target;
        this.message = message == null ? "" : message;
        this.metadata = metadata == null ? Map.of() : Map.copyOf(metadata);
        this.artifactIds = artifactIds == null ? List.of() : List.copyOf(artifactIds);
    }

    @Override
    public Map<String, String> metadata() {
        return Map.copyOf(metadata);
    }

    @Override
    public List<String> artifactIds() {
        return List.copyOf(artifactIds);
    }

    private static String required(String value, String name) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(name + " must not be blank");
        }
        return value;
    }
}
