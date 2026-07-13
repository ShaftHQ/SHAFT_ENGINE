package com.shaft.capture.runtime;

import com.shaft.capture.model.CaptureStep;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BrowserEventSinkTest {
    @Test
    void stepsEndpointServesAuthoritativeStepsAndRejectsBadToken() throws Exception {
        BrowserEventSink sink = new BrowserEventSink(signal -> { }, warning -> { });
        sink.stepsSupplier(() -> List.of(
                new CaptureStep("instance-a-1", 1, "Open https://a.test/"),
                new CaptureStep("instance-a-2", 2, "Click submit")));
        sink.start();
        try {
            HttpClient client = HttpClient.newHttpClient();

            HttpResponse<String> authorized = client.send(HttpRequest.newBuilder()
                    .uri(URI.create(sink.stepsEndpoint() + "?token=" + sink.eventToken()))
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(200, authorized.statusCode());
            assertTrue(authorized.body().contains("instance-a-1"));
            assertTrue(authorized.body().contains("Click submit"));

            HttpResponse<String> unauthorized = client.send(HttpRequest.newBuilder()
                    .uri(URI.create(sink.stepsEndpoint() + "?token=wrong-token"))
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(401, unauthorized.statusCode());

            HttpResponse<String> badMethod = client.send(HttpRequest.newBuilder()
                    .uri(URI.create(sink.stepsEndpoint() + "?token=" + sink.eventToken()))
                    .POST(HttpRequest.BodyPublishers.noBody())
                    .build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(405, badMethod.statusCode());
        } finally {
            sink.close();
        }
    }

    @Test
    void sessionEndpointServesSessionInfoAndRejectsBadToken() throws Exception {
        BrowserEventSink sink = new BrowserEventSink(signal -> { }, warning -> { });
        sink.sessionSupplier(() -> new BrowserEventSink.SessionInfo(
                "ACTIVE", "target/recordings/login.json"));
        sink.start();
        try {
            HttpClient client = HttpClient.newHttpClient();

            HttpResponse<String> authorized = client.send(HttpRequest.newBuilder()
                    .uri(URI.create(sink.sessionEndpoint() + "?token=" + sink.eventToken()))
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(200, authorized.statusCode());
            assertTrue(authorized.body().contains("\"state\":\"ACTIVE\""));
            assertTrue(authorized.body().contains("target/recordings/login.json"));

            HttpResponse<String> unauthorized = client.send(HttpRequest.newBuilder()
                    .uri(URI.create(sink.sessionEndpoint() + "?token=wrong-token"))
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(401, unauthorized.statusCode());
        } finally {
            sink.close();
        }
    }

    @Test
    void sessionEndpointReturnsBlankInfoBeforeSupplierIsWired() throws Exception {
        BrowserEventSink sink = new BrowserEventSink(signal -> { }, warning -> { });
        sink.start();
        try {
            HttpClient client = HttpClient.newHttpClient();
            HttpResponse<String> response = client.send(HttpRequest.newBuilder()
                    .uri(URI.create(sink.sessionEndpoint() + "?token=" + sink.eventToken()))
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(200, response.statusCode());
            assertTrue(response.body().contains("\"outputPath\":\"\""));
        } finally {
            sink.close();
        }
    }

    @Test
    void stepsEndpointReturnsEmptyListBeforeSupplierIsWired() throws Exception {
        BrowserEventSink sink = new BrowserEventSink(signal -> { }, warning -> { });
        sink.start();
        try {
            HttpClient client = HttpClient.newHttpClient();
            HttpResponse<String> response = client.send(HttpRequest.newBuilder()
                    .uri(URI.create(sink.stepsEndpoint() + "?token=" + sink.eventToken()))
                    .GET()
                    .build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(200, response.statusCode());
            assertEquals("[]", response.body());
        } finally {
            sink.close();
        }
    }

    @Test
    void stepsAndEventEndpointsAnswerCorsPreflightsWithPrivateNetworkAccessHeaders() throws Exception {
        BrowserEventSink sink = new BrowserEventSink(signal -> { }, warning -> { });
        sink.start();
        try {
            HttpClient client = HttpClient.newHttpClient();

            HttpResponse<String> stepsPreflight = client.send(HttpRequest.newBuilder()
                    .uri(URI.create(sink.stepsEndpoint()))
                    .method("OPTIONS", HttpRequest.BodyPublishers.noBody())
                    .build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(204, stepsPreflight.statusCode());
            assertEquals("true", stepsPreflight.headers().firstValue("Access-Control-Allow-Private-Network")
                    .orElse(""));
            assertEquals("*", stepsPreflight.headers().firstValue("Access-Control-Allow-Origin").orElse(""));

            HttpResponse<String> eventPreflight = client.send(HttpRequest.newBuilder()
                    .uri(URI.create(sink.endpoint()))
                    .method("OPTIONS", HttpRequest.BodyPublishers.noBody())
                    .build(), HttpResponse.BodyHandlers.ofString());
            assertEquals(204, eventPreflight.statusCode());
            assertEquals("true", eventPreflight.headers().firstValue("Access-Control-Allow-Private-Network")
                    .orElse(""));
        } finally {
            sink.close();
        }
    }
}
