package com.shaft.commandline.mcp;

import com.shaft.commandline.util.Json;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * MCP client speaking newline-delimited JSON-RPC over a child process's stdio (one-shot mode).
 * The server's stdout carries only JSON-RPC messages; its stderr (logs) is drained separately to
 * avoid the classic pipe-buffer deadlock.
 */
public final class StdioMcpClient extends AbstractMcpClient {

    private static final long RESPONSE_TIMEOUT_SECONDS = 120;

    private final Writer toServer;
    private final BufferedReader fromServer;
    private final BlockingQueue<String> inbound = new LinkedBlockingQueue<>();
    private final Process process;
    private final Thread readerThread;
    private volatile boolean closed;

    /**
     * Wraps an already-connected pair of streams. Used for in-process testing.
     *
     * @param serverStdout the stream carrying the server's JSON-RPC output
     * @param serverStdin  the stream to write JSON-RPC requests to
     */
    public StdioMcpClient(InputStream serverStdout, OutputStream serverStdin) {
        this(serverStdout, serverStdin, null);
    }

    private StdioMcpClient(InputStream serverStdout, OutputStream serverStdin, Process process) {
        this.process = process;
        this.toServer = new BufferedWriter(new OutputStreamWriter(serverStdin, StandardCharsets.UTF_8));
        this.fromServer = new BufferedReader(new java.io.InputStreamReader(serverStdout, StandardCharsets.UTF_8));
        this.readerThread = new Thread(this::readLoop, "shaft-cli-mcp-stdio-reader");
        this.readerThread.setDaemon(true);
        this.readerThread.start();
    }

    /**
     * Spawns a shaft-mcp stdio child from the given command and connects to it. The child's stderr
     * is redirected away from this process so log output cannot block the protocol channel.
     *
     * @param command the process command (e.g. {@code [java, @shaft-mcp.args]})
     * @return a connected client owning the child process
     */
    public static StdioMcpClient spawn(List<String> command) {
        ProcessBuilder builder = new ProcessBuilder(command);
        builder.redirectError(ProcessBuilder.Redirect.DISCARD);
        Process process;
        try {
            process = builder.start();
        } catch (IOException exception) {
            throw new McpException("Failed to launch shaft-mcp: " + String.join(" ", command), exception);
        }
        return new StdioMcpClient(process.getInputStream(), process.getOutputStream(), process);
    }

    private void readLoop() {
        try {
            String line;
            while ((line = fromServer.readLine()) != null) {
                if (!line.isBlank()) {
                    inbound.add(line);
                }
            }
        } catch (IOException ignored) {
            // Stream closed on shutdown; nothing to do.
        }
    }

    @Override
    protected JsonNode rpc(ObjectNode request) {
        long id = request.path("id").asLong();
        writeLine(request);
        long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(RESPONSE_TIMEOUT_SECONDS);
        while (System.nanoTime() < deadline) {
            String line;
            try {
                line = inbound.poll(200, TimeUnit.MILLISECONDS);
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                throw new McpException("Interrupted while awaiting MCP response", exception);
            }
            if (line == null) {
                if (process != null && !process.isAlive()) {
                    throw new McpException("shaft-mcp child exited before responding (exit "
                            + process.exitValue() + ")");
                }
                continue;
            }
            JsonNode message = Json.MAPPER.readTree(line);
            if (message.path("id").asLong(-1) == id) {
                return message;
            }
            // Otherwise it is a server notification/log frame; ignore and keep reading.
        }
        throw new McpException("Timed out after " + RESPONSE_TIMEOUT_SECONDS
                + "s awaiting MCP response id " + id);
    }

    @Override
    protected void sendNotification(ObjectNode notification) {
        writeLine(notification);
    }

    private synchronized void writeLine(ObjectNode message) {
        try {
            toServer.write(Json.MAPPER.writeValueAsString(message));
            toServer.write("\n");
            toServer.flush();
        } catch (IOException exception) {
            throw new McpException("Failed to write to shaft-mcp child", exception);
        }
    }

    @Override
    public void close() {
        if (closed) {
            return;
        }
        closed = true;
        try {
            toServer.close();
        } catch (IOException ignored) {
            // best effort
        }
        try {
            fromServer.close();
        } catch (IOException ignored) {
            // best effort
        }
        readerThread.interrupt();
        if (process != null && process.isAlive()) {
            process.destroy();
            try {
                if (!process.waitFor(5, TimeUnit.SECONDS)) {
                    process.destroyForcibly();
                }
            } catch (InterruptedException exception) {
                Thread.currentThread().interrupt();
                process.destroyForcibly();
            }
        }
    }
}
