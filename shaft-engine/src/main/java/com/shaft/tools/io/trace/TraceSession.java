package com.shaft.tools.io.trace;

import com.shaft.gui.capabilities.AutomationBackend;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.HashSet;
import java.util.Set;

/**
 * Immutable schema-v2 trace session shared by all SHAFT automation backends.
 * Consumers should use {@link #SCHEMA_VERSION} for compatibility negotiation and must tolerate
 * additional metadata, artifact kinds, and event categories in later compatible revisions.
 */
public record TraceSession(String id, AutomationBackend backend, Instant generatedAt, String testId, int attempt,
                           List<TraceEvent> events, List<TraceArtifactReference> artifacts,
                           Map<String, String> metadata) {
    public static final String SCHEMA_VERSION = "2.0";

    public TraceSession {
        if (id == null || id.isBlank()) {
            throw new IllegalArgumentException("id must not be blank");
        }
        backend = java.util.Objects.requireNonNull(backend, "backend");
        generatedAt = java.util.Objects.requireNonNull(generatedAt, "generatedAt");
        testId = testId == null ? "" : testId;
        if (attempt < 1) {
            throw new IllegalArgumentException("attempt must be positive");
        }
        events = events == null ? List.of() : List.copyOf(events);
        artifacts = artifacts == null ? List.of() : List.copyOf(artifacts);
        Set<String> artifactIds = new HashSet<>();
        for (TraceArtifactReference artifact : artifacts) {
            if (!artifactIds.add(artifact.id())) {
                throw new IllegalArgumentException("artifact ids must be unique: " + artifact.id());
            }
        }
        for (TraceEvent event : events) {
            for (String artifactId : event.artifactIds()) {
                if (!artifactIds.contains(artifactId)) {
                    throw new IllegalArgumentException("event artifact id does not resolve: " + artifactId);
                }
            }
        }
        metadata = metadata == null ? Map.of() : Map.copyOf(metadata);
    }
}
