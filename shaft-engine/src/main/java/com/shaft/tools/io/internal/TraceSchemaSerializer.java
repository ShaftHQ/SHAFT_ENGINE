package com.shaft.tools.io.internal;

import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.tools.io.trace.TraceArtifactReference;
import com.shaft.tools.io.trace.TraceEvent;
import com.shaft.tools.io.trace.TraceEventStatus;
import com.shaft.tools.io.trace.TraceSession;
import tools.jackson.databind.ObjectMapper;

import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

/** Maps recorder events into the stable backend-neutral v2 trace contract. */
final class TraceSchemaSerializer {
    private static final ObjectMapper JSON = new ObjectMapper();

    private TraceSchemaSerializer() {
        throw new IllegalStateException("Utility class");
    }

    static TraceSession create(String testId, int attempt, List<TraceEventRecorder.ActionEvent> actions,
                               List<TraceArtifactReference> artifacts) {
        AutomationBackend backend = sessionBackend(actions);
        Instant generatedAt = Instant.now();
        String sessionId = testId + "-attempt-" + attempt + "-" + UUID.randomUUID();
        List<TraceEvent> events = actions.stream()
                .map(action -> event(sessionId, generatedAt, action, artifacts))
                .toList();
        String eventBackends = actions.stream().map(TraceEventRecorder.ActionEvent::backend).distinct()
                .map(Enum::name).sorted().collect(Collectors.joining(","));
        return new TraceSession(sessionId, backend, generatedAt, testId, attempt, events, artifacts,
                Map.of("producer", "SHAFT Engine", "eventBackends", eventBackends));
    }

    static String toJson(TraceSession session) {
        Map<String, Object> json = new LinkedHashMap<>();
        json.put("schemaVersion", TraceSession.SCHEMA_VERSION);
        json.put("id", safe(session.id()));
        json.put("backend", session.backend().name());
        json.put("generatedAt", session.generatedAt().toString());
        json.put("testId", safe(session.testId()));
        json.put("attempt", session.attempt());
        json.put("events", session.events().stream().map(TraceSchemaSerializer::eventMap).toList());
        json.put("artifacts", session.artifacts().stream().map(TraceSchemaSerializer::artifactMap).toList());
        json.put("metadata", safeMetadata(session.metadata()));
        return JSON.writeValueAsString(json);
    }

    static String artifactsToJson(List<TraceArtifactReference> artifacts) {
        List<TraceArtifactReference> safeArtifacts = artifacts == null ? List.of() : List.copyOf(artifacts);
        return JSON.writeValueAsString(safeArtifacts.stream().map(TraceSchemaSerializer::artifactMap).toList());
    }

    private static TraceEvent event(String sessionId, Instant generatedAt, TraceEventRecorder.ActionEvent action,
                                    List<TraceArtifactReference> artifacts) {
        Map<String, String> metadata = new LinkedHashMap<>(action.metadata());
        if (!action.url().isBlank()) {
            metadata.put("url", action.url());
        }
        if (!action.exceptionType().isBlank()) {
            metadata.put("exceptionType", action.exceptionType());
            metadata.put("exceptionMessage", action.exceptionMessage());
        }
        if (!action.attachments().isEmpty()) {
            metadata.put("attachmentSummaries", String.join(" | ", action.attachments()));
        }
        String screenshotId = "screenshot-" + action.id();
        List<String> artifactIds = artifacts.stream().anyMatch(artifact -> artifact.id().equals(screenshotId))
                ? List.of(screenshotId) : List.of();
        return new TraceEvent(sessionId + "/" + action.id(), action.backend(), nonBlank(action.category(), "action"),
                nonBlank(action.name(), "unnamed"), status(action.status()), instant(action.startTime(), generatedAt),
                action.durationMs(), action.caller(), action.locator(), action.message(), metadata, artifactIds);
    }

    private static Map<String, Object> eventMap(TraceEvent event) {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("id", safe(event.id()));
        map.put("backend", event.backend().name());
        map.put("category", safe(event.category()));
        map.put("name", safe(event.name()));
        map.put("status", event.status().name());
        map.put("startedAt", event.startedAt().toString());
        map.put("durationMs", event.durationMs());
        map.put("source", safe(event.source()));
        map.put("target", safe(event.target()));
        map.put("message", safe(event.message()));
        map.put("metadata", safeMetadata(event.metadata()));
        map.put("artifactIds", event.artifactIds().stream().map(TraceSchemaSerializer::safe).toList());
        return map;
    }

    private static Map<String, Object> artifactMap(TraceArtifactReference artifact) {
        Map<String, Object> map = new LinkedHashMap<>();
        map.put("id", safe(artifact.id()));
        map.put("kind", safe(artifact.kind()));
        map.put("path", safe(artifact.path()));
        map.put("mimeType", safe(artifact.mimeType()));
        map.put("omitted", artifact.omitted());
        map.put("metadata", safeMetadata(artifact.metadata()));
        return map;
    }

    private static Map<String, String> safeMetadata(Map<String, String> metadata) {
        Map<String, String> safe = new LinkedHashMap<>();
        metadata.forEach((key, value) -> safe.put(safe(key), isSensitiveKey(key) ? "********" : safe(value)));
        return safe;
    }

    private static boolean isSensitiveKey(String key) {
        String normalized = key == null ? "" : key.toLowerCase(java.util.Locale.ROOT);
        return normalized.contains("password") || normalized.contains("passwd") || normalized.contains("pwd")
                || normalized.contains("secret") || normalized.contains("token") || normalized.contains("api_key")
                || normalized.contains("apikey") || normalized.contains("accesskey")
                || normalized.contains("access_key") || normalized.contains("authorization")
                || normalized.contains("cookie");
    }

    private static AutomationBackend sessionBackend(List<TraceEventRecorder.ActionEvent> actions) {
        List<AutomationBackend> backends = actions.stream().map(TraceEventRecorder.ActionEvent::backend)
                .filter(backend -> backend != AutomationBackend.UNKNOWN).distinct().toList();
        return backends.size() == 1 ? backends.getFirst() : AutomationBackend.UNKNOWN;
    }

    private static TraceEventStatus status(String status) {
        return switch (status == null ? "" : status.toLowerCase(java.util.Locale.ROOT)) {
            case "passed" -> TraceEventStatus.PASSED;
            case "skipped" -> TraceEventStatus.SKIPPED;
            default -> TraceEventStatus.FAILED;
        };
    }

    private static Instant instant(String value, Instant fallback) {
        try {
            return Instant.parse(value);
        } catch (DateTimeParseException | NullPointerException ignored) {
            return fallback;
        }
    }

    private static String nonBlank(String value, String fallback) {
        return value == null || value.isBlank() ? fallback : value;
    }

    private static String safe(String value) {
        return FailureTraceReporter.redact(value == null ? "" : value);
    }
}
