package com.shaft.commandline.testsupport;

import com.shaft.commandline.mcp.McpException;
import com.shaft.commandline.mcp.StdioMcpClient;
import com.shaft.commandline.runtime.McpConnector;

import java.io.Closeable;
import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

/**
 * Wires a {@link FakeMcpServer} to a {@link StdioMcpClient} over in-memory pipes on a daemon thread,
 * with no subprocess. Deterministic and Windows-safe.
 */
public final class InProcessMcp {

    private static final int PIPE_BUFFER = 1 << 20;

    private InProcessMcp() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * @return a fresh client connected to a fresh in-process fake server
     * @throws IOException if the pipes cannot be created
     */
    public static StdioMcpClient start() throws IOException {
        PipedInputStream serverIn = null;
        PipedOutputStream clientToServer = null;
        PipedInputStream clientIn = null;
        PipedOutputStream serverToClient = null;
        boolean started = false;
        try {
            serverIn = new PipedInputStream(PIPE_BUFFER);
            clientToServer = new PipedOutputStream(serverIn);
            clientIn = new PipedInputStream(PIPE_BUFFER);
            serverToClient = new PipedOutputStream(clientIn);
            final PipedInputStream serverInput = serverIn;
            final PipedOutputStream serverOutput = serverToClient;
            Thread serverThread = new Thread(() -> {
                try {
                    FakeMcpServer.serve(serverInput, serverOutput);
                } catch (IOException ignored) {
                    // Pipe closed when the client closes; normal shutdown.
                }
            }, "fake-mcp-server");
            serverThread.setDaemon(true);
            serverThread.start();
            StdioMcpClient client = new StdioMcpClient(clientIn, clientToServer);
            started = true;
            return client;
        } finally {
            if (!started) {
                closeQuietly(serverToClient);
                closeQuietly(clientIn);
                closeQuietly(clientToServer);
                closeQuietly(serverIn);
            }
        }
    }

    private static void closeQuietly(Closeable closeable) {
        if (closeable != null) {
            try {
                closeable.close();
            } catch (IOException ignored) {
                // Best-effort cleanup on a failed pipe setup.
            }
        }
    }

    /**
     * @return an {@link McpConnector} that hands out a fresh in-process client per connect
     */
    public static McpConnector connector() {
        return (requiresSession, stdioOk) -> {
            try {
                return start();
            } catch (IOException exception) {
                throw new McpException("Failed to start in-process fake MCP server", exception);
            }
        };
    }
}
