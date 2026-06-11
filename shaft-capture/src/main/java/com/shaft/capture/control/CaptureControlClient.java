package com.shaft.capture.control;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
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
    private final ObjectMapper mapper = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

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
                        append(last.warnings(), "The recorder process is no longer reachable."),
                        last.outputPath(),
                        false,
                        last.processId(),
                        last.startedAt());
            }
            return last;
        }
        try {
            return send("GET", "/status", "");
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
                        append(last.warnings(), "The recorder process is no longer reachable."),
                        last.outputPath(),
                        false,
                        last.processId(),
                        last.startedAt());
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
                "kind", kind == null ? Checkpoint.CheckpointKind.USER_MARKER.name() : kind.name()));
    }

    /**
     * Stops the active recorder.
     *
     * @param discard whether to delete capture artifacts
     * @return final status
     */
    public CaptureStatus stop(boolean discard) {
        return sendJson("POST", "/stop", Map.of("discard", discard));
    }

    private CaptureStatus sendJson(String method, String path, Object body) {
        try {
            return send(method, path, mapper.writeValueAsString(body));
        } catch (JsonProcessingException exception) {
            throw new IllegalStateException("SHAFT Capture control request could not be serialized.", exception);
        }
    }

    private CaptureStatus send(String method, String path, String body) {
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
            return mapper.readValue(response.body(), CaptureStatus.class);
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
