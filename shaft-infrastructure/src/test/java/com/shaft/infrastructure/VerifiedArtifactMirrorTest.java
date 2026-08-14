package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.HttpURLConnection;
import java.net.Proxy;
import java.net.Socket;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
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

        try {
            request(base.resolve("builds/firefox/1538/firefox-win64.zip"), "GET");
            throw new AssertionError("Closed artifact mirror still accepted a connection.");
        } catch (java.net.ConnectException expectedFailure) {
            assertTrue(true);
        }
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

    private record Response(int status, byte[] body) { }
}
