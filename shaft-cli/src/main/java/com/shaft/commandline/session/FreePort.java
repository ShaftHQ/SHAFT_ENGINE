package com.shaft.commandline.session;

import com.shaft.commandline.mcp.McpException;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;

/**
 * Allocates an ephemeral loopback TCP port for the daemon to bind.
 */
public final class FreePort {

    private FreePort() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Binds a transient {@link ServerSocket} to port 0 on {@code 127.0.0.1}, reads back the port
     * the OS assigned, and closes the socket so the daemon can bind it.
     *
     * @return a currently-free TCP port on the loopback interface
     */
    public static int find() {
        try (ServerSocket socket = new ServerSocket()) {
            socket.bind(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 0));
            return socket.getLocalPort();
        } catch (IOException exception) {
            throw new McpException("Failed to allocate a free TCP port: " + exception.getMessage(), exception);
        }
    }
}
