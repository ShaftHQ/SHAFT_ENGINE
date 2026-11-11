package com.shaft.commandline.runtime;

import com.shaft.commandline.mcp.HttpMcpClient;
import com.shaft.commandline.mcp.McpClient;
import com.shaft.commandline.mcp.McpException;
import com.shaft.commandline.session.McpLauncherLocator;
import com.shaft.commandline.session.SessionInfo;
import com.shaft.commandline.session.SessionManager;
import com.shaft.commandline.session.SessionStore;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Routing only: a live session record selects HTTP, and a stateful command without one fails
 * before any stdio child is spawned.
 */
class ConnectionFactoryTest {

    @Test
    void liveSessionRoutesToHttpEvenWhenStdioWouldOtherwiseBeAllowed(@TempDir Path dir) throws Exception {
        SessionStore store = new SessionStore(dir);
        store.save(new SessionInfo(1, ProcessHandle.current().pid(), "not-a-real-jar", "2026-11-11T00:00:00Z"));
        ConnectionFactory factory = factory(store);

        try (McpClient client = factory.connect(false, true)) {
            assertInstanceOf(HttpMcpClient.class, client);
        }
    }

    @Test
    void deadSessionDoesNotSatisfyAStatefulCommand(@TempDir Path dir) {
        SessionStore store = new SessionStore(dir);
        store.save(new SessionInfo(1, Long.MAX_VALUE, "not-a-real-jar", "2026-11-11T00:00:00Z"));
        ConnectionFactory factory = factory(store);

        McpException failure = assertThrows(McpException.class, () -> factory.connect(true, false));

        assertTrue(failure.getMessage().contains("shaft-cli session start"), failure.getMessage());
        assertTrue(failure.getMessage().contains("--stdio-ok"), failure.getMessage());
    }

    @Test
    void missingSessionRejectsAStatefulCommandBeforeSpawningStdio(@TempDir Path dir) {
        ConnectionFactory factory = factory(new SessionStore(dir));

        McpException failure = assertThrows(McpException.class, () -> factory.connect(true, false));

        assertTrue(failure.getMessage().contains("needs a live shaft-mcp session"), failure.getMessage());
    }

    private static ConnectionFactory factory(SessionStore store) {
        return new ConnectionFactory(new SessionManager(new McpLauncherLocator(), store), new McpLauncherLocator());
    }
}
