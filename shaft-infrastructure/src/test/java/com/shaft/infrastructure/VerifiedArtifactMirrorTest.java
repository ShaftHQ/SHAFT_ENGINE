package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.HttpURLConnection;
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

    private static Response request(URI uri, String method) throws Exception {
        HttpURLConnection connection = (HttpURLConnection) uri.toURL().openConnection();
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
