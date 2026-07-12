package com.shaft.capture.control;

import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStatus;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureControlServerClientTest {
    @TempDir
    Path temp;

    @Test
    void clientTalksToAuthorizedLoopbackServerAndFallsBackToLocalStatus() throws Exception {
        CaptureManager manager = new CaptureManager();
        CaptureControlFiles files = new CaptureControlFiles(temp);
        AtomicBoolean stopped = new AtomicBoolean(false);
        CaptureControlServer server = new CaptureControlServer(manager, files, "token", () -> stopped.set(true));
        int port = server.start();
        files.writeToken("token");
        files.writeDescriptor(new CaptureControlFiles.ControlDescriptor(port, ProcessHandle.current().pid()));

        try {
            CaptureControlClient client = new CaptureControlClient(temp);

            assertEquals(CaptureStatus.State.NOT_RUNNING, client.status().state());
            assertEquals(CaptureStatus.State.NOT_RUNNING, client.stop(false).state());
            assertTrue(stopped.get());
            assertThrows(IllegalStateException.class,
                    () -> client.checkpoint("mark", Checkpoint.CheckpointKind.ASSERTION));

            HttpClient http = HttpClient.newHttpClient();
            HttpResponse<String> unauthorized = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/status"))
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            HttpResponse<String> badMethod = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/stop"))
                    .header("Authorization", "Bearer token")
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            HttpResponse<String> badCheckpoint = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/checkpoint"))
                    .header("Authorization", "Bearer token")
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString("{\"description\":\"mark\",\"kind\":\"bad\"}"))
                    .build(), HttpResponse.BodyHandlers.ofString());
            HttpResponse<String> steps = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/steps"))
                    .header("Authorization", "Bearer token")
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            HttpResponse<String> unauthorizedSteps = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/steps"))
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());

            assertEquals(401, unauthorized.statusCode());
            assertEquals(400, badMethod.statusCode());
            assertEquals(400, badCheckpoint.statusCode());
            assertEquals(200, steps.statusCode());
            assertEquals("[]", steps.body());
            assertEquals(401, unauthorizedSteps.statusCode());
            assertTrue(new CaptureControlClient(temp).steps().isEmpty());
        } finally {
            server.close();
            manager.close();
        }

        files.clearActiveControl();
        files.writeStatus(new CaptureStatus(
                CaptureStatus.State.ACTIVE,
                "session",
                "chrome",
                "",
                0,
                java.util.List.of(),
                "",
                false,
                ProcessHandle.current().pid(),
                null));

        CaptureStatus incomplete = new CaptureControlClient(temp).status();
        assertEquals(CaptureStatus.State.INCOMPLETE, incomplete.state());
        assertTrue(incomplete.warnings().stream().anyMatch(warning -> warning.contains("no longer reachable")));
    }

    @Test
    void modeEndpointDefaultsToRecordAndCanBeToggledToInspect() throws Exception {
        CaptureManager manager = new CaptureManager();
        CaptureControlFiles files = new CaptureControlFiles(temp);
        CaptureControlServer server = new CaptureControlServer(manager, files, "token", () -> { });
        int port = server.start();

        try {
            HttpClient http = HttpClient.newHttpClient();
            HttpResponse<String> initial = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/mode"))
                    .header("Authorization", "Bearer token")
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            HttpResponse<String> toggled = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/mode"))
                    .header("Authorization", "Bearer token")
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString("{\"mode\":\"inspect\"}"))
                    .build(), HttpResponse.BodyHandlers.ofString());
            HttpResponse<String> confirmed = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/mode"))
                    .header("Authorization", "Bearer token")
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            HttpResponse<String> invalid = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/mode"))
                    .header("Authorization", "Bearer token")
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString("{\"mode\":\"not-a-mode\"}"))
                    .build(), HttpResponse.BodyHandlers.ofString());

            assertEquals(200, initial.statusCode());
            assertTrue(initial.body().contains("\"record\""));
            assertEquals(200, toggled.statusCode());
            assertTrue(toggled.body().contains("\"inspect\""));
            assertEquals(200, confirmed.statusCode());
            assertTrue(confirmed.body().contains("\"inspect\""));
            assertEquals(400, invalid.statusCode());
        } finally {
            server.close();
            manager.close();
        }
    }

    @Test
    void pickLocatorRanksCandidatesBestFirstAndRendersASnippet() throws Exception {
        CaptureManager manager = new CaptureManager();
        CaptureControlFiles files = new CaptureControlFiles(temp);
        CaptureControlServer server = new CaptureControlServer(manager, files, "token", () -> { });
        int port = server.start();

        try {
            HttpClient http = HttpClient.newHttpClient();
            String body = """
                    {"candidates": [
                      {"strategy": "CSS", "expression": "div.form > input", "uniquenessCount": 1, "visible": true, "stable": false},
                      {"strategy": "ID", "expression": "username", "uniquenessCount": 1, "visible": true, "stable": true}
                    ]}
                    """;
            HttpResponse<String> response = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/locator/pick"))
                    .header("Authorization", "Bearer token")
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString(body))
                    .build(), HttpResponse.BodyHandlers.ofString());

            assertEquals(200, response.statusCode());
            assertTrue(response.body().contains("SHAFT.GUI.Locator.id(\\\"username\\\")"),
                    "The ID strategy should outrank CSS here, got: " + response.body());
            assertTrue(response.body().contains("\"ranked\""));

            CaptureControlFiles.LastPick lastPick = files.readLastPick();
            assertEquals("SHAFT.GUI.Locator.id(\"username\")", lastPick.snippet(),
                    "/locator/pick must persist the winning snippet so a later empty-candidates "
                            + "caller (e.g. the MCP capture_pick_locator tool) can recover it.");
            assertEquals(2, lastPick.candidates().size());
            assertEquals("ID", lastPick.candidates().getFirst().strategy());
            assertTrue(lastPick.capturedAtMillis() > 0);
        } finally {
            server.close();
            manager.close();
        }
    }

    @Test
    void pickLocatorRejectsUnsupportedStrategyAndEmptyCandidateList() throws Exception {
        CaptureManager manager = new CaptureManager();
        CaptureControlFiles files = new CaptureControlFiles(temp);
        CaptureControlServer server = new CaptureControlServer(manager, files, "token", () -> { });
        int port = server.start();

        try {
            HttpClient http = HttpClient.newHttpClient();
            HttpResponse<String> badStrategy = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/locator/pick"))
                    .header("Authorization", "Bearer token")
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString(
                            "{\"candidates\": [{\"strategy\": \"NOT_A_STRATEGY\", \"expression\": \"x\"}]}"))
                    .build(), HttpResponse.BodyHandlers.ofString());
            HttpResponse<String> empty = http.send(HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/locator/pick"))
                    .header("Authorization", "Bearer token")
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString("{\"candidates\": []}"))
                    .build(), HttpResponse.BodyHandlers.ofString());

            assertEquals(400, badStrategy.statusCode());
            assertEquals(400, empty.statusCode());
        } finally {
            server.close();
            manager.close();
        }
    }

    @Test
    void serverRejectsMissingDependencies() {
        CaptureControlFiles files = new CaptureControlFiles(temp);

        assertThrows(IllegalArgumentException.class,
                () -> new CaptureControlServer(null, files, "token", () -> { }));
        assertThrows(IllegalArgumentException.class,
                () -> new CaptureControlServer(new CaptureManager(), files, "", () -> { }));
    }
}
