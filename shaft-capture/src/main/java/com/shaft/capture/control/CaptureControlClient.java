package com.shaft.capture.control;

import tools.jackson.core.JacksonException;
import tools.jackson.core.type.TypeReference;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import com.shaft.capture.model.CaptureStep;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.runtime.CaptureStatus;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.List;
import java.util.Map;

/**
 * Authenticated client for a detached local capture recorder.
 */
public final class CaptureControlClient {
    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private final CaptureControlFiles files;
    private final HttpClient client = HttpClient.newBuilder()
            .connectTimeout(TIMEOUT)
            .build();
    private final ObjectMapper mapper = JsonMapper.builder().build();

    /**
     * Creates a client for one runtime directory.
     *
     * @param runtimeDirectory local runtime directory
     */
    public CaptureControlClient(java.nio.file.Path runtimeDirectory) {
        files = new CaptureControlFiles(runtimeDirectory);
    }

    /**
     * Returns active endpoint status or the final local status snapshot.
     *
     * @return safe status
     */
    public CaptureStatus status() {
        if (!files.hasActiveControl()) {
            CaptureStatus last = files.readStatus();
            if (last.state() == CaptureStatus.State.ACTIVE
                    || last.state() == CaptureStatus.State.STARTING
                    || last.state() == CaptureStatus.State.STOPPING) {
                return new CaptureStatus(
                        CaptureStatus.State.INCOMPLETE,
                        last.sessionId(),
                        last.browser(),
                        last.currentUrl(),
                        last.eventCount(),
                        last.readiness(),
                        append(last.warnings(), "The recorder process is no longer reachable."),
                        last.outputPath(),
                        false,
                        last.processId(),
                        last.startedAt(),
                        last.networkTransactionCount(),
                        last.lastEndpoints());
            }
            return last;
        }
        try {
            return send("GET", "/status", "", CaptureStatus.class);
        } catch (RuntimeException exception) {
            files.clearActiveControl();
            CaptureStatus last = files.readStatus();
            if (last.state() == CaptureStatus.State.ACTIVE
                    || last.state() == CaptureStatus.State.STARTING
                    || last.state() == CaptureStatus.State.STOPPING) {
                return new CaptureStatus(
                        CaptureStatus.State.INCOMPLETE,
                        last.sessionId(),
                        last.browser(),
                        last.currentUrl(),
                        last.eventCount(),
                        last.readiness(),
                        append(last.warnings(), "The recorder process is no longer reachable."),
                        last.outputPath(),
                        false,
                        last.processId(),
                        last.startedAt(),
                        last.networkTransactionCount(),
                        last.lastEndpoints());
            }
            return last;
        }
    }

    /**
     * Adds a checkpoint to the active recorder.
     *
     * @param description checkpoint description
     * @param kind checkpoint kind
     * @return updated status
     */
    public CaptureStatus checkpoint(String description, Checkpoint.CheckpointKind kind) {
        return sendJson("POST", "/checkpoint", Map.of(
                "description", description == null ? "" : description,
                "kind", kind == null ? Checkpoint.CheckpointKind.USER_MARKER.name() : kind.name()),
                CaptureStatus.class);
    }

    /**
     * Stops the active recorder.
     *
     * @param discard whether to delete capture artifacts
     * @return final status
     */
    public CaptureStatus stop(boolean discard) {
        return sendJson("POST", "/stop", Map.of("discard", discard), CaptureStatus.class);
    }

    /**
     * Returns the current server-side step list for the active session, so a recorder UI or
     * control client can source its step list from the session store instead of page-scoped
     * browser storage.
     *
     * @return ordered safe step summaries, or an empty list when no session is reachable
     */
    public List<CaptureStep> steps() {
        if (!files.hasActiveControl()) {
            return List.of();
        }
        try {
            return send("GET", "/steps", "", new TypeReference<List<CaptureStep>>() {
            });
        } catch (RuntimeException exception) {
            return List.of();
        }
    }

    private <T> T sendJson(String method, String path, Object body, Class<T> type) {
        try {
            return send(method, path, mapper.writeValueAsString(body), type);
        } catch (JacksonException exception) {
            throw new IllegalStateException("SHAFT Capture control request could not be serialized.", exception);
        }
    }

    private <T> T send(String method, String path, String body, Class<T> type) {
        String responseBody = exchange(method, path, body);
        return mapper.readValue(responseBody, type);
    }

    private <T> T send(String method, String path, String body, TypeReference<T> type) {
        String responseBody = exchange(method, path, body);
        return mapper.readValue(responseBody, type);
    }

    private String exchange(String method, String path, String body) {
        CaptureControlFiles.ControlDescriptor descriptor = files.readDescriptor();
        if (ProcessHandle.of(descriptor.processId()).isEmpty()) {
            throw new IllegalStateException("SHAFT Capture recorder process is not running.");
        }
        HttpRequest.Builder request = HttpRequest.newBuilder()
                .uri(URI.create("http://127.0.0.1:" + descriptor.port() + path))
                .timeout(TIMEOUT)
                .header("Authorization", "Bearer " + files.readToken())
                .header("Content-Type", "application/json");
        if ("GET".equals(method)) {
            request.GET();
        } else {
            request.method(method, HttpRequest.BodyPublishers.ofString(body));
        }
        try {
            HttpResponse<String> response = client.send(
                    request.build(),
                    HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() != 200) {
                throw new IllegalStateException("SHAFT Capture control endpoint rejected the request.");
            }
            return response.body();
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("SHAFT Capture control request was interrupted.", exception);
        } catch (IOException exception) {
            throw new IllegalStateException("SHAFT Capture control endpoint is unavailable.", exception);
        }
    }

    private static java.util.List<String> append(java.util.List<String> warnings, String warning) {
        java.util.List<String> result = new java.util.ArrayList<>(warnings);
        result.add(warning);
        return List.copyOf(result);
    }
}
