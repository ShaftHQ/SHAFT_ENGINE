package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class VerifiedArtifactMirrorTest {
    @Test
    void servesOnlyExactGetRequestsFromLoopbackAndStopsOnClose(@TempDir Path temp) throws Exception {
        byte[] expected = "verified archive".getBytes(StandardCharsets.UTF_8);
        Path artifact = Files.write(temp.resolve("browser.zip"), expected);
        URI base;
        try (VerifiedArtifactMirror mirror = VerifiedArtifactMirror.open(Map.of(
                "/builds/firefox/1538/firefox-win64.zip", artifact))) {
            base = mirror.baseUri();
            assertTrue(base.getHost().equals("127.0.0.1"));
            assertTrue(!base.toString().endsWith("/"));
            assertArrayEquals(expected, request(URI.create(base + "//builds/firefox/1538/firefox-win64.zip"),
                    "GET").body());
            assertEquals(404, request(base.resolve("builds/firefox/1538/missing.zip"), "GET").status());
            assertEquals(405, request(base.resolve("builds/firefox/1538/firefox-win64.zip"), "POST").status());
            assertEquals(404, request(base.resolve("builds/firefox/1538/%2e%2e/browser.zip"), "GET").status());
        }

        URI closedMirror = base.resolve("builds/firefox/1538/firefox-win64.zip");
        byte[] replacementBody = "replacement listener".getBytes(StandardCharsets.UTF_8);
        Response reusedPort = requestAfterPortReuse(closedMirror, replacementBody);
        assertEquals(200, reusedPort.status());
        assertArrayEquals(replacementBody, reusedPort.body());
        assertFalse(java.util.Arrays.equals(expected, reusedPort.body()));
    }

    @Test
    void canonicalizesOnlyPlaywrightsExactDuplicateLeadingSlashForms() {
        assertEquals("/builds/cft/browser.zip", VerifiedArtifactMirror.canonicalRequestPath(
                URI.create("//builds/cft/browser.zip")));
        assertEquals("/builds/cft/browser.zip", VerifiedArtifactMirror.canonicalRequestPath(
                URI.create("http://127.0.0.1:1234//builds/cft/browser.zip")));
        assertEquals("///builds/cft/browser.zip", VerifiedArtifactMirror.canonicalRequestPath(
                URI.create("http://127.0.0.1:1234///builds/cft/browser.zip")));
    }

    @Test
    void acceptsTheHeaderLimitAndRejectsOnlyRequestsBeyondIt(@TempDir Path temp) throws Exception {
        Path artifact = Files.writeString(temp.resolve("browser.zip"), "verified archive");
        try (VerifiedArtifactMirror mirror = VerifiedArtifactMirror.open(Map.of("/browser.zip", artifact))) {
            assertEquals(200, rawRequestStatus(mirror.baseUri(), 100));
            assertEquals(431, rawRequestStatus(mirror.baseUri(), 101));
        }
    }

    private static int rawRequestStatus(URI base, int headerCount) throws Exception {
        try (Socket socket = new Socket(base.getHost(), base.getPort())) {
            StringBuilder request = new StringBuilder("GET /browser.zip HTTP/1.1\r\n");
            for (int index = 0; index < headerCount; index++) {
                request.append("X-Test-").append(index).append(": value\r\n");
            }
            request.append("\r\n");
            socket.getOutputStream().write(request.toString().getBytes(StandardCharsets.US_ASCII));
            String statusLine = new String(socket.getInputStream().readNBytes(32), StandardCharsets.US_ASCII);
            return Integer.parseInt(statusLine.split(" ", 3)[1]);
        }
    }

    private static Response request(URI uri, String method) throws Exception {
        HttpURLConnection connection = (HttpURLConnection) uri.toURL().openConnection(Proxy.NO_PROXY);
        connection.setRequestMethod(method);
        connection.setConnectTimeout(2_000);
        connection.setReadTimeout(2_000);
        int status = connection.getResponseCode();
        byte[] body = status >= 400
                ? connection.getErrorStream() == null ? new byte[0] : connection.getErrorStream().readAllBytes()
                : connection.getInputStream().readAllBytes();
        connection.disconnect();
        return new Response(status, body);
    }

    private static Response requestAfterPortReuse(URI uri, byte[] replacementBody) throws Exception {
        try (ServerSocket replacement = new ServerSocket()) {
            replacement.setReuseAddress(true);
            replacement.bind(new InetSocketAddress(uri.getHost(), uri.getPort()));
            ExecutorService responder = Executors.newSingleThreadExecutor();
            Future<?> response = responder.submit(() -> {
                try (Socket connection = replacement.accept()) {
                    byte[] headers = ("HTTP/1.1 200 OK\r\nContent-Length: " + replacementBody.length
                            + "\r\nConnection: close\r\n\r\n").getBytes(StandardCharsets.US_ASCII);
                    connection.getOutputStream().write(headers);
                    connection.getOutputStream().write(replacementBody);
                    connection.getOutputStream().flush();
                } catch (java.io.IOException failure) {
                    throw new java.io.UncheckedIOException(failure);
                }
            });
            try {
                return request(uri, "GET");
            } finally {
                try {
                    response.get(2, TimeUnit.SECONDS);
                } finally {
                    responder.shutdownNow();
                }
            }
        }
    }

    private record Response(int status, byte[] body) { }
}
