package com.shaft.commandline.session;

import com.shaft.commandline.mcp.McpException;
import com.shaft.commandline.util.Json;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Orchestrates the shaft-cli HTTP daemon session lifecycle: starting shaft-mcp in its HTTP
 * profile, tracking whether it is still alive, and stopping it.
 */
public final class SessionManager {

    private static final Duration READINESS_TIMEOUT = Duration.ofSeconds(60);
    private static final Duration READINESS_POLL_INTERVAL = Duration.ofMillis(500);
    private static final Duration STOP_GRACE_PERIOD = Duration.ofSeconds(10);
    private static final int LOG_TAIL_LINES = 20;

    private final McpLauncherLocator locator;
    private final SessionStore store;
    private final HttpClient httpClient;

    /**
     * Creates a manager backed by a real launcher locator and the default session store.
     */
    public SessionManager() {
        this(new McpLauncherLocator(), new SessionStore());
    }

    /**
     * Creates a manager with injectable collaborators, for tests.
     *
     * @param locator resolves how to launch shaft-mcp
     * @param store   persists session metadata
     */
    public SessionManager(McpLauncherLocator locator, SessionStore store) {
        this.locator = locator;
        this.store = store;
        this.httpClient = HttpClient.newBuilder()
                .connectTimeout(Duration.ofSeconds(5))
                .build();
    }

    /**
     * Starts the shaft-mcp HTTP daemon, waits for it to become ready, and persists its session
     * metadata.
     *
     * @return the newly started session's metadata
     * @throws McpException if a session is already running, the process fails to launch, or it
     *                       does not become ready within the readiness timeout
     */
    public SessionInfo start() {
        Optional<SessionInfo> existing = liveSession();
        if (existing.isPresent()) {
            throw new McpException("a shaft-cli session is already running on port "
                    + existing.get().port() + "; run `shaft-cli session stop` first");
        }

        int port = FreePort.find();
        LaunchSpec spec = locator.locate();
        List<String> command = new ArrayList<>(spec.baseCommand());
        command.add("--spring.profiles.active=http");
        command.add("--server.port=" + port);

        Path logFile = logFile(port);
        Process process = launch(command, logFile);

        waitForReadiness(process, port, logFile);

        SessionInfo info = new SessionInfo(port, process.pid(), spec.jarPath(), Instant.now().toString());
        store.save(info);
        return info;
    }

    /**
     * Reports whether the persisted session's process is still alive. Does not probe the daemon
     * over HTTP, so this is cheap and deterministic.
     *
     * @return the current session status
     */
    public SessionStatus status() {
        Optional<SessionInfo> info = store.load();
        if (info.isEmpty()) {
            return new SessionStatus(false, null);
        }
        boolean running = ProcessHandle.of(info.get().pid()).map(ProcessHandle::isAlive).orElse(false);
        return new SessionStatus(running, info.get());
    }

    /**
     * Stops the persisted session's process, if any, and clears the session file.
     *
     * @return {@code true} if a session file was present, regardless of whether the process was
     *         still alive
     */
    public boolean stop() {
        Optional<SessionInfo> info = store.load();
        if (info.isEmpty()) {
            return false;
        }
        try {
            ProcessHandle.of(info.get().pid()).ifPresent(SessionManager::terminate);
        } finally {
            store.clear();
        }
        return true;
    }

    /**
     * @return the persisted session, only if its process is still alive; used by the routing
     *         layer to decide between the HTTP daemon and a fresh stdio child
     */
    public Optional<SessionInfo> liveSession() {
        return store.load().filter(info -> ProcessHandle.of(info.pid()).map(ProcessHandle::isAlive).orElse(false));
    }

    private static void terminate(ProcessHandle handle) {
        if (!handle.isAlive()) {
            return;
        }
        handle.destroy();
        try {
            handle.onExit().get(STOP_GRACE_PERIOD.toMillis(), java.util.concurrent.TimeUnit.MILLISECONDS);
        } catch (Exception exception) {
            // Timed out, interrupted, or failed while waiting for a graceful exit; force it below.
        }
        if (handle.isAlive()) {
            handle.destroyForcibly();
        }
    }

    private Process launch(List<String> command, Path logFile) {
        try {
            Files.createDirectories(logFile.getParent());
            ProcessBuilder builder = new ProcessBuilder(command)
                    .redirectOutput(ProcessBuilder.Redirect.appendTo(logFile.toFile()))
                    .redirectError(ProcessBuilder.Redirect.appendTo(logFile.toFile()));
            return builder.start();
        } catch (IOException exception) {
            throw new McpException("Failed to launch shaft-mcp daemon: " + exception.getMessage(), exception);
        }
    }

    private void waitForReadiness(Process process, int port, Path logFile) {
        Instant deadline = Instant.now().plus(READINESS_TIMEOUT);
        while (Instant.now().isBefore(deadline)) {
            if (!process.isAlive()) {
                throw new McpException("shaft-mcp daemon exited during startup (exit code "
                        + process.exitValue() + "). Last log lines from '" + logFile + "':\n"
                        + tailLog(logFile));
            }
            if (isReady(port)) {
                return;
            }
            sleep(READINESS_POLL_INTERVAL);
        }
        process.destroyForcibly();
        throw new McpException("shaft-mcp daemon did not become ready on port " + port + " within "
                + READINESS_TIMEOUT.getSeconds() + "s. Last log lines from '" + logFile + "':\n" + tailLog(logFile));
    }

    private boolean isReady(int port) {
        if (!canConnect(port)) {
            return false;
        }
        try {
            ObjectNode params = Json.newObject();
            params.putObject("capabilities");
            ObjectNode clientInfo = params.putObject("clientInfo");
            clientInfo.put("name", "shaft-cli");
            clientInfo.put("version", "1.0");
            params.put("protocolVersion", "2025-03-26");

            ObjectNode request = Json.newObject();
            request.put("jsonrpc", "2.0");
            request.put("id", 1);
            request.put("method", "initialize");
            request.set("params", params);

            HttpRequest httpRequest = HttpRequest.newBuilder()
                    .uri(URI.create("http://127.0.0.1:" + port + "/mcp"))
                    .header("Content-Type", "application/json")
                    .header("Accept", "application/json, text/event-stream")
                    .timeout(Duration.ofSeconds(5))
                    .POST(HttpRequest.BodyPublishers.ofString(
                            Json.MAPPER.writeValueAsString(request), StandardCharsets.UTF_8))
                    .build();
            HttpResponse<Void> response = httpClient.send(httpRequest, HttpResponse.BodyHandlers.discarding());
            return response.statusCode() == 200;
        } catch (IOException | RuntimeException exception) {
            return false;
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            return false;
        }
    }

    private static boolean canConnect(int port) {
        try (Socket socket = new Socket()) {
            socket.connect(new InetSocketAddress("127.0.0.1", port), 500);
            return true;
        } catch (IOException exception) {
            return false;
        }
    }

    private static void sleep(Duration duration) {
        try {
            Thread.sleep(duration.toMillis());
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new McpException("Interrupted while waiting for shaft-mcp daemon readiness.", exception);
        }
    }

    private static Path logFile(int port) {
        return Path.of(System.getProperty("user.home"), ".shaft", "logs", "daemon-" + port + ".log");
    }

    private static String tailLog(Path logFile) {
        try {
            List<String> lines = Files.readAllLines(logFile, StandardCharsets.UTF_8);
            int fromIndex = Math.max(0, lines.size() - LOG_TAIL_LINES);
            return String.join("\n", lines.subList(fromIndex, lines.size()));
        } catch (IOException exception) {
            return "(log file unavailable: " + exception.getMessage() + ")";
        }
    }
}
