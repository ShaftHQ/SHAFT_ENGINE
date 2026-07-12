package com.shaft.capture.control;

import tools.jackson.core.JacksonException;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import com.shaft.capture.model.CaptureStep;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStatus;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
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
    private final ObjectMapper mapper = JsonMapper.builder().build();
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
        server.createContext("/steps", exchange -> handle(exchange, this::steps));
        server.createContext("/mode", exchange -> handle(exchange, this::mode));
        server.createContext("/locator/pick", exchange -> handle(exchange, this::pickLocator));
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

    private List<CaptureStep> steps(HttpExchange exchange) {
        requireMethod(exchange, "GET");
        return manager.steps();
    }

    /**
     * Reads (GET) or toggles (POST) the recorder's live authoring mode: {@code record} (default;
     * the recorder captures interactions as replayable steps) or {@code inspect} (the recorder
     * overlay highlights hovered elements and reports the clicked element's locator candidates to
     * {@link #pickLocator} instead of recording an interaction step). Delegates to
     * {@link CaptureManager#mode()}/{@link CaptureManager#setMode(String)} so the same live mode is
     * visible whether it's read/toggled in-process (MCP) or over this loopback endpoint (a
     * detached recorder UI).
     */
    private ModeResponse mode(HttpExchange exchange) {
        if ("POST".equalsIgnoreCase(exchange.getRequestMethod())) {
            ModeRequest request = read(exchange, ModeRequest.class);
            try {
                manager.setMode(request.mode());
            } catch (IllegalArgumentException invalidMode) {
                throw new BadRequestException(invalidMode.getMessage());
            }
        } else {
            requireMethod(exchange, "GET");
        }
        return new ModeResponse(manager.mode());
    }

    /**
     * Ranks caller-supplied locator candidates for one picked element (best-first, via
     * {@link LocatorCandidate#BEST_FIRST}) and renders a copy-paste Java snippet for the winner.
     * The recorder JS computes candidates client-side (it already has the live DOM) and posts them
     * here rather than this server re-deriving them, since deriving locator evidence requires the
     * live page the browser has and this control server does not.
     *
     * <p>The ranked result is also persisted via {@link CaptureControlFiles#writeLastPick} so a
     * later caller with no candidates of its own (the SHAFT MCP {@code capture_pick_locator} tool,
     * serving an IntelliJ Pick-Locator action) can still recover the user's most recent pick.
     */
    private PickLocatorResponse pickLocator(HttpExchange exchange) {
        requireMethod(exchange, "POST");
        PickLocatorRequest request = read(exchange, PickLocatorRequest.class);
        List<LocatorCandidate> candidates = toCandidates(request.candidates());
        if (candidates.isEmpty()) {
            throw new BadRequestException("At least one locator candidate is required.");
        }
        List<LocatorCandidate> ranked = candidates.stream().sorted(LocatorCandidate.BEST_FIRST).toList();
        LocatorCandidate winner = ranked.getFirst();
        List<RankedCandidate> rankedResponse = ranked.stream()
                .map(candidate -> new RankedCandidate(
                        candidate.strategy().name(), candidate.expression(), candidate.score(),
                        PickedLocatorSnippetBuilder.snippet(candidate)))
                .toList();
        String winnerSnippet = PickedLocatorSnippetBuilder.snippet(winner);
        files.writeLastPick(new CaptureControlFiles.LastPick(
                winnerSnippet, rankedResponse, System.currentTimeMillis()));
        return new PickLocatorResponse(winnerSnippet, rankedResponse);
    }

    private static List<LocatorCandidate> toCandidates(List<CandidateRequest> requested) {
        if (requested == null) {
            return List.of();
        }
        List<LocatorCandidate> candidates = new ArrayList<>();
        for (CandidateRequest candidate : requested) {
            LocatorCandidate.LocatorStrategy strategy;
            try {
                strategy = LocatorCandidate.LocatorStrategy.valueOf(
                        (candidate.strategy() == null ? "" : candidate.strategy()).trim().toUpperCase(Locale.ROOT));
            } catch (IllegalArgumentException invalidStrategy) {
                throw new BadRequestException("Unsupported locator strategy: " + candidate.strategy());
            }
            candidates.add(new LocatorCandidate(
                    strategy, candidate.expression(), candidate.uniquenessCount(),
                    candidate.visible(), candidate.stable(), Set.of()));
        }
        return candidates;
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
        } catch (JacksonException exception) {
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
        Object handle(HttpExchange exchange);
    }

    private record CheckpointRequest(String description, String kind) {
    }

    private record StopRequest(boolean discard) {
    }

    private record ModeRequest(String mode) {
    }

    /**
     * Current recorder authoring mode.
     *
     * @param mode {@code record} or {@code inspect}
     */
    public record ModeResponse(String mode) {
    }

    private record CandidateRequest(
            String strategy, String expression, int uniquenessCount, boolean visible, boolean stable) {
    }

    private record PickLocatorRequest(List<CandidateRequest> candidates) {
    }

    /**
     * One ranked locator candidate.
     *
     * @param strategy locator strategy name
     * @param expression raw locator expression
     * @param score deterministic {@link LocatorCandidate#score()}
     * @param snippet copy-paste Java locator expression
     */
    public record RankedCandidate(String strategy, String expression, int score, String snippet) {
    }

    /**
     * Pick-locator result: the best candidate's snippet plus every candidate ranked best-first.
     *
     * @param snippet winning candidate's copy-paste Java locator expression
     * @param ranked every supplied candidate, ranked best-first
     */
    public record PickLocatorResponse(String snippet, List<RankedCandidate> ranked) {
    }

    private record ErrorResponse(String error) {
    }

    private static final class BadRequestException extends RuntimeException {
        private BadRequestException(String message) {
            super(message);
        }
    }
}
