package com.shaft.infrastructure;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;

/** Ephemeral loopback-only HTTP mirror for already verified setup artifacts. */
final class VerifiedArtifactMirror implements AutoCloseable {
    private final HttpServer server;
    private final ExecutorService executor;
    private final Map<String, Path> artifacts;
    private final AtomicBoolean closed = new AtomicBoolean();

    private VerifiedArtifactMirror(HttpServer server, ExecutorService executor, Map<String, Path> artifacts) {
        this.server = server;
        this.executor = executor;
        this.artifacts = artifacts;
    }

    static VerifiedArtifactMirror open(Map<String, Path> artifacts) throws IOException {
        Objects.requireNonNull(artifacts, "artifacts");
        Map<String, Path> verified = new LinkedHashMap<>();
        for (Map.Entry<String, Path> entry : artifacts.entrySet()) {
            String requestPath = Objects.requireNonNull(entry.getKey(), "request path");
            if (!requestPath.startsWith("/") || requestPath.contains("%") || requestPath.contains("?")
                    || requestPath.contains("#") || requestPath.contains("..")) {
                throw new IllegalArgumentException("Unsafe artifact mirror request path: " + requestPath);
            }
            Path file = Objects.requireNonNull(entry.getValue(), "artifact path").toAbsolutePath().normalize();
            VerifiedArtifactStore.requireUnlinkedAncestors(file);
            if (!Files.isRegularFile(file, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
                throw new IOException("Verified artifact mirror input is not a regular file: " + file);
            }
            if (verified.put(requestPath, file) != null) {
                throw new IllegalArgumentException("Duplicate artifact mirror request path: " + requestPath);
            }
        }
        if (verified.isEmpty()) throw new IllegalArgumentException("Artifact mirror must not be empty.");

        InetAddress loopback = InetAddress.getByAddress(new byte[]{127, 0, 0, 1});
        HttpServer server = HttpServer.create(new InetSocketAddress(loopback, 0), 0);
        ExecutorService executor = Executors.newSingleThreadExecutor(runnable -> {
            Thread thread = new Thread(runnable, "shaft-playwright-artifact-mirror");
            thread.setDaemon(true);
            return thread;
        });
        VerifiedArtifactMirror mirror = new VerifiedArtifactMirror(server, executor, Map.copyOf(verified));
        server.createContext("/", mirror::handle);
        server.createContext("//", mirror::handle);
        server.setExecutor(executor);
        server.start();
        return mirror;
    }

    URI baseUri() {
        return URI.create("http://127.0.0.1:" + server.getAddress().getPort());
    }

    private void handle(HttpExchange exchange) throws IOException {
        try (exchange) {
            if (!"GET".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, -1);
                return;
            }
            URI request = exchange.getRequestURI();
            if (request.getRawQuery() != null || request.getRawFragment() != null
                    || request.getRawPath().contains("%")) {
                exchange.sendResponseHeaders(404, -1);
                return;
            }
            String rawPath = canonicalRequestPath(request);
            Path artifact = artifacts.get(rawPath);
            if (artifact == null) {
                exchange.sendResponseHeaders(404, -1);
                return;
            }
            VerifiedArtifactStore.requireUnlinkedAncestors(artifact);
            if (!Files.isRegularFile(artifact, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
                exchange.sendResponseHeaders(404, -1);
                return;
            }
            long size = Files.size(artifact);
            exchange.getResponseHeaders().set("Content-Type", "application/octet-stream");
            exchange.getResponseHeaders().set("Cache-Control", "no-store");
            exchange.sendResponseHeaders(200, size);
            try (var input = Files.newInputStream(artifact); var output = exchange.getResponseBody()) {
                input.transferTo(output);
            }
        }
    }

    static String canonicalRequestPath(URI request) {
        String rawPath = request.getRawPath();
        if (request.getScheme() == null && request.getRawAuthority() != null) {
            rawPath = '/' + request.getRawAuthority() + rawPath;
        }
        if (rawPath.startsWith("//") && !rawPath.startsWith("///")) return rawPath.substring(1);
        return rawPath;
    }

    @Override
    public void close() {
        if (!closed.compareAndSet(false, true)) return;
        server.stop(0);
        executor.shutdownNow();
    }
}
