package com.shaft.tools.io.trace;

import com.shaft.gui.capabilities.AutomationBackend;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
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
        String checkedId = requiredId(id);
        AutomationBackend checkedBackend = java.util.Objects.requireNonNull(backend, "backend");
        Instant checkedGeneratedAt = java.util.Objects.requireNonNull(generatedAt, "generatedAt");
        int checkedAttempt = positiveAttempt(attempt);
        List<TraceEvent> copiedEvents = events == null
                ? List.of()
                : Collections.unmodifiableList(new ArrayList<>(List.copyOf(events)));
        List<TraceArtifactReference> copiedArtifacts = artifacts == null
                ? List.of()
                : Collections.unmodifiableList(new ArrayList<>(List.copyOf(artifacts)));
        validateArtifactReferences(copiedEvents, copiedArtifacts);
        this.id = checkedId;
        this.backend = checkedBackend;
        this.generatedAt = checkedGeneratedAt;
        this.testId = testId == null ? "" : testId;
        this.attempt = checkedAttempt;
        this.events = copiedEvents;
        this.artifacts = copiedArtifacts;
        this.metadata = metadata == null
                ? Map.of()
                : Collections.unmodifiableMap(new HashMap<>(Map.copyOf(metadata)));
    }

    private static String requiredId(String id) {
        if (id == null || id.isBlank()) {
            throw new IllegalArgumentException("id must not be blank");
        }
        return id;
    }

    private static int positiveAttempt(int attempt) {
        if (attempt < 1) {
            throw new IllegalArgumentException("attempt must be positive");
        }
        return attempt;
    }

    private static void validateArtifactReferences(List<TraceEvent> events,
                                                   List<TraceArtifactReference> artifacts) {
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
