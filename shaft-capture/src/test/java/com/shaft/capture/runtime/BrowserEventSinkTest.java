package com.shaft.capture.runtime;

import com.shaft.capture.collector.BrowserSignal;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BrowserEventSinkTest {
    @Test
    void authorizedGetReturnsSuppliedStateWhileUnauthorizedGetIsRejectedAndPostStillWorks() throws Exception {
        List<BrowserSignal> received = new CopyOnWriteArrayList<>();
        List<String> warnings = new CopyOnWriteArrayList<>();
        BrowserEventSink sink = new BrowserEventSink(
                received::add,
                warnings::add,
                () -> Map.of("instanceId", "session-42", "eventCount", 7, "readinessState", "RISKY"));
        String endpoint = sink.start();
        String token = sink.eventToken();

        try {
            HttpClient http = HttpClient.newHttpClient();

            HttpResponse<String> missingToken = http.send(HttpRequest.newBuilder()
                    .uri(URI.create(endpoint))
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            HttpResponse<String> wrongToken = http.send(HttpRequest.newBuilder()
                    .uri(URI.create(endpoint + "?token=wrong"))
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            HttpResponse<String> authorized = http.send(HttpRequest.newBuilder()
                    .uri(URI.create(endpoint + "?token=" + token))
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());

            assertEquals(403, missingToken.statusCode());
            assertEquals(403, wrongToken.statusCode());
            assertEquals(200, authorized.statusCode());
            assertEquals("*", authorized.headers().firstValue("Access-Control-Allow-Origin").orElse(""));
            assertTrue(authorized.body().contains("\"instanceId\":\"session-42\""));
            assertTrue(authorized.body().contains("\"eventCount\":7"));
            assertTrue(authorized.body().contains("\"readinessState\":\"RISKY\""));

            HttpResponse<String> posted = http.send(HttpRequest.newBuilder()
                    .uri(URI.create(endpoint))
                    .POST(HttpRequest.BodyPublishers.ofString(
                            "{\"token\":\"" + token + "\",\"payload\":{\"kind\":\"click\"}}"))
                    .build(), HttpResponse.BodyHandlers.ofString());

            assertEquals(204, posted.statusCode());
            assertEquals(1, received.size());
            assertEquals("click", received.getFirst().kind());
        } finally {
            sink.close();
        }
    }

    @Test
    void constructorRejectsMissingCollaborators() {
        assertThrows(IllegalArgumentException.class,
                () -> new BrowserEventSink(null, ignored -> { }, Map::of));
        assertThrows(IllegalArgumentException.class,
                () -> new BrowserEventSink(ignored -> { }, null, Map::of));
        assertThrows(IllegalArgumentException.class,
                () -> new BrowserEventSink(ignored -> { }, ignored -> { }, null));
    }
}
