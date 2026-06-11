package com.shaft.capture.control;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStatus;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Locale;
import java.util.concurrent.Executors;

/**
 * Loopback-only authenticated control endpoint for a detached recorder.
 */
public final class CaptureControlServer implements AutoCloseable {
    private static final int MAX_REQUEST_BYTES = 8192;

    private final CaptureManager manager;
    private final CaptureControlFiles files;
    private final String token;
    private final Runnable stoppedCallback;
    private final ObjectMapper mapper = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    private final HttpServer server;

    /**
     * Creates the loopback control endpoint.
     *
     * @param manager capture lifecycle
     * @param files local state files
     * @param token authorization token
     * @param stoppedCallback callback after stop
     */
    public CaptureControlServer(
            CaptureManager manager,
            CaptureControlFiles files,
            String token,
            Runnable stoppedCallback) {
        if (manager == null || files == null || token == null || token.isBlank() || stoppedCallback == null) {
            throw new IllegalArgumentException("Capture control server dependencies are required.");
        }
        this.manager = manager;
        this.files = files;
        this.token = token;
        this.stoppedCallback = stoppedCallback;
        try {
            server = HttpServer.create(
                    new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 0),
                    0);
        } catch (IOException exception) {
            throw new IllegalStateException("SHAFT Capture loopback control endpoint could not start.", exception);
        }
        server.createContext("/status", exchange -> handle(exchange, this::status));
        server.createContext("/checkpoint", exchange -> handle(exchange, this::checkpoint));
        server.createContext("/stop", exchange -> handle(exchange, this::stop));
        server.setExecutor(Executors.newFixedThreadPool(2, runnable -> {
            Thread thread = new Thread(runnable, "shaft-capture-control");
            thread.setDaemon(true);
            return thread;
        }));
    }

    /**
     * Starts accepting local control requests.
     *
     * @return assigned loopback port
     */
    public int start() {
        server.start();
        return server.getAddress().getPort();
    }

    @Override
    public void close() {
        server.stop(0);
    }

    private CaptureStatus status(HttpExchange exchange) {
        requireMethod(exchange, "GET");
        CaptureStatus status = manager.status();
        files.writeStatus(status);
        return status;
    }

    private CaptureStatus checkpoint(HttpExchange exchange) {
        requireMethod(exchange, "POST");
        CheckpointRequest request = read(exchange, CheckpointRequest.class);
        Checkpoint.CheckpointKind kind;
        try {
            kind = Checkpoint.CheckpointKind.valueOf(
                    request.kind() == null
                            ? "USER_MARKER"
                            : request.kind().trim().toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException exception) {
            throw new BadRequestException("Unsupported checkpoint kind.");
        }
        CaptureStatus status = manager.checkpoint(request.description(), kind);
        files.writeStatus(status);
        return status;
    }

    private CaptureStatus stop(HttpExchange exchange) {
        requireMethod(exchange, "POST");
        StopRequest request = read(exchange, StopRequest.class);
        CaptureStatus status = manager.stop(request.discard());
        files.writeStatus(status);
        return status;
    }

    private void handle(HttpExchange exchange, Handler handler) throws IOException {
        try (exchange) {
            try {
                if (!authorized(exchange)) {
                    send(exchange, 401, new ErrorResponse("Unauthorized local capture control request."));
                    return;
                }
                send(exchange, 200, handler.handle(exchange));
                if ("/stop".equals(exchange.getHttpContext().getPath())) {
                    stoppedCallback.run();
                }
            } catch (BadRequestException exception) {
                send(exchange, 400, new ErrorResponse(exception.getMessage()));
            } catch (RuntimeException exception) {
                send(exchange, 409, new ErrorResponse("SHAFT Capture control operation failed."));
            }
        }
    }

    private boolean authorized(HttpExchange exchange) {
        String header = exchange.getRequestHeaders().getFirst("Authorization");
        String expected = "Bearer " + token;
        return header != null && MessageDigest.isEqual(
                header.getBytes(StandardCharsets.UTF_8),
                expected.getBytes(StandardCharsets.UTF_8));
    }

    private <T> T read(HttpExchange exchange, Class<T> type) {
        try {
            byte[] body = exchange.getRequestBody().readNBytes(MAX_REQUEST_BYTES + 1);
            if (body.length > MAX_REQUEST_BYTES) {
                throw new BadRequestException("Capture control request is too large.");
            }
            return mapper.readValue(body, type);
        } catch (JsonProcessingException exception) {
            throw new BadRequestException("Capture control request is invalid.");
        } catch (IOException exception) {
            throw new BadRequestException("Capture control request could not be read.");
        }
    }

    private void send(HttpExchange exchange, int status, Object value) throws IOException {
        byte[] body = mapper.writeValueAsBytes(value);
        exchange.getResponseHeaders().set("Content-Type", "application/json; charset=utf-8");
        exchange.sendResponseHeaders(status, body.length);
        exchange.getResponseBody().write(body);
    }

    private static void requireMethod(HttpExchange exchange, String expected) {
        if (!expected.equalsIgnoreCase(exchange.getRequestMethod())) {
            throw new BadRequestException("Unsupported capture control method.");
        }
    }

    @FunctionalInterface
    private interface Handler {
        CaptureStatus handle(HttpExchange exchange);
    }

    private record CheckpointRequest(String description, String kind) {
    }

    private record StopRequest(boolean discard) {
    }

    private record ErrorResponse(String error) {
    }

    private static final class BadRequestException extends RuntimeException {
        private BadRequestException(String message) {
            super(message);
        }
    }
}
