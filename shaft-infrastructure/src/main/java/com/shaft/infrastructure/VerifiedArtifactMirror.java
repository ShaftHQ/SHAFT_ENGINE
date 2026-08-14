package com.shaft.infrastructure;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
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
    private static final int MAXIMUM_HTTP_LINE_BYTES = 8 * 1024;
    private static final int MAXIMUM_HTTP_HEADERS = 100;
    private final ServerSocket server;
    private final ExecutorService executor;
    private final Map<String, Path> artifacts;
    private final AtomicBoolean closed = new AtomicBoolean();

    private VerifiedArtifactMirror(ServerSocket server, ExecutorService executor, Map<String, Path> artifacts) {
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
        ServerSocket server = new ServerSocket();
        server.bind(new InetSocketAddress(loopback, 0));
        ExecutorService executor = Executors.newSingleThreadExecutor(runnable -> {
            Thread thread = new Thread(runnable, "shaft-playwright-artifact-mirror");
            thread.setDaemon(true);
            return thread;
        });
        VerifiedArtifactMirror mirror = new VerifiedArtifactMirror(server, executor, Map.copyOf(verified));
        executor.submit(mirror::serve);
        return mirror;
    }

    URI baseUri() {
        return URI.create("http://127.0.0.1:" + server.getLocalPort());
    }

    private void serve() {
        while (!closed.get()) {
            try (Socket connection = server.accept()) {
                connection.setSoTimeout(30_000);
                handle(connection);
            } catch (SocketException stopped) {
                if (!closed.get()) throw new IllegalStateException("Playwright artifact mirror socket failed.", stopped);
            } catch (IOException failure) {
                if (!closed.get()) throw new IllegalStateException("Playwright artifact mirror request failed.", failure);
            }
        }
    }

    private void handle(Socket connection) throws IOException {
        BufferedInputStream input = new BufferedInputStream(connection.getInputStream());
        BufferedOutputStream output = new BufferedOutputStream(connection.getOutputStream());
        String requestLine = readLine(input);
        String[] request = requestLine.split(" ", 3);
        if (request.length != 3 || !request[2].startsWith("HTTP/1.")) {
            respond(output, 400, null);
            return;
        }
        for (int count = 0; count < MAXIMUM_HTTP_HEADERS; count++) {
            if (readLine(input).isEmpty()) break;
            if (count == MAXIMUM_HTTP_HEADERS - 1) {
                respond(output, 431, null);
                return;
            }
        }
        if (!"GET".equals(request[0])) {
            respond(output, 405, null);
            return;
        }
        URI target;
        try {
            target = URI.create(request[1]);
        } catch (IllegalArgumentException invalid) {
            respond(output, 404, null);
            return;
        }
        if (target.getRawQuery() != null || target.getRawFragment() != null
                || target.getRawPath() == null || target.getRawPath().contains("%")) {
            respond(output, 404, null);
            return;
        }
        Path artifact = artifacts.get(canonicalRequestPath(target));
        if (artifact == null) {
            respond(output, 404, null);
            return;
        }
        VerifiedArtifactStore.requireUnlinkedAncestors(artifact);
        if (!Files.isRegularFile(artifact, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
            respond(output, 404, null);
            return;
        }
        respond(output, 200, artifact);
    }

    private static String readLine(BufferedInputStream input) throws IOException {
        byte[] line = new byte[MAXIMUM_HTTP_LINE_BYTES];
        int length = 0;
        while (length < line.length) {
            int value = input.read();
            if (value < 0) throw new IOException("Unexpected end of HTTP request.");
            if (value == '\n') {
                if (length > 0 && line[length - 1] == '\r') length--;
                return new String(line, 0, length, StandardCharsets.US_ASCII);
            }
            line[length++] = (byte) value;
        }
        throw new IOException("Playwright artifact mirror HTTP line is too long.");
    }

    private static void respond(BufferedOutputStream output, int status, Path artifact) throws IOException {
        long size = artifact == null ? 0 : Files.size(artifact);
        String reason = switch (status) {
            case 200 -> "OK";
            case 400 -> "Bad Request";
            case 405 -> "Method Not Allowed";
            case 431 -> "Request Header Fields Too Large";
            default -> "Not Found";
        };
        String headers = "HTTP/1.1 " + status + ' ' + reason + "\r\nContent-Length: " + size
                + "\r\nConnection: close\r\n" + (artifact == null ? "" :
                "Content-Type: application/octet-stream\r\nCache-Control: no-store\r\n") + "\r\n";
        output.write(headers.getBytes(StandardCharsets.US_ASCII));
        if (artifact != null) {
            try (var file = Files.newInputStream(artifact)) {
                file.transferTo(output);
            }
        }
        output.flush();
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
        try {
            server.close();
        } catch (IOException ignored) {
            // Closing an already failed ephemeral mirror is best-effort.
        }
        executor.shutdownNow();
    }
}
