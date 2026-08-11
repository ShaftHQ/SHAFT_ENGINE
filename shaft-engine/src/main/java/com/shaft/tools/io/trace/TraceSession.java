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

    public TraceSession(String id, AutomationBackend backend, Instant generatedAt, String testId, int attempt,
                        List<TraceEvent> events, List<TraceArtifactReference> artifacts,
                        Map<String, String> metadata) {
        if (id == null || id.isBlank()) {
            throw new IllegalArgumentException("id must not be blank");
        }
        AutomationBackend checkedBackend = java.util.Objects.requireNonNull(backend, "backend");
        Instant checkedGeneratedAt = java.util.Objects.requireNonNull(generatedAt, "generatedAt");
        if (attempt < 1) {
            throw new IllegalArgumentException("attempt must be positive");
        }
        List<TraceEvent> copiedEvents = events == null ? List.of() : List.copyOf(events);
        List<TraceArtifactReference> copiedArtifacts = artifacts == null ? List.of() : List.copyOf(artifacts);
        Set<String> artifactIds = new HashSet<>();
        for (TraceArtifactReference artifact : copiedArtifacts) {
            if (!artifactIds.add(artifact.id())) {
                throw new IllegalArgumentException("artifact ids must be unique: " + artifact.id());
            }
        }
        for (TraceEvent event : copiedEvents) {
            for (String artifactId : event.artifactIds()) {
                if (!artifactIds.contains(artifactId)) {
                    throw new IllegalArgumentException("event artifact id does not resolve: " + artifactId);
                }
            }
        }
        this.id = id;
        this.backend = checkedBackend;
        this.generatedAt = checkedGeneratedAt;
        this.testId = testId == null ? "" : testId;
        this.attempt = attempt;
        this.events = copiedEvents;
        this.artifacts = copiedArtifacts;
        this.metadata = metadata == null ? Map.of() : Map.copyOf(metadata);
    }

    @Override
    public List<TraceEvent> events() {
        return List.copyOf(events);
    }

    @Override
    public List<TraceArtifactReference> artifacts() {
        return List.copyOf(artifacts);
    }

    @Override
    public Map<String, String> metadata() {
        return Map.copyOf(metadata);
    }
}
