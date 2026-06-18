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

            assertEquals(401, unauthorized.statusCode());
            assertEquals(400, badMethod.statusCode());
            assertEquals(400, badCheckpoint.statusCode());
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
    void serverRejectsMissingDependencies() {
        CaptureControlFiles files = new CaptureControlFiles(temp);

        assertThrows(IllegalArgumentException.class,
                () -> new CaptureControlServer(null, files, "token", () -> { }));
        assertThrows(IllegalArgumentException.class,
                () -> new CaptureControlServer(new CaptureManager(), files, "", () -> { }));
    }
}
