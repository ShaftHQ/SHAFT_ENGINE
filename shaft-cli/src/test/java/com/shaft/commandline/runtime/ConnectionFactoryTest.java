package com.shaft.commandline.runtime;

import com.shaft.commandline.command.BrowserCommand;
import com.shaft.commandline.command.ElementCommand;
import com.shaft.commandline.command.GuideCommand;
import com.shaft.commandline.mcp.HttpMcpClient;
import com.shaft.commandline.mcp.McpClient;
import com.shaft.commandline.mcp.McpException;
import com.shaft.commandline.session.McpLauncherLocator;
import com.shaft.commandline.session.SessionInfo;
import com.shaft.commandline.session.SessionManager;
import com.shaft.commandline.session.SessionStore;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.lang.reflect.Field;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
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
    void noArgConstructorsWireTheRealSessionManager() throws Exception {
        assertRealSessionManager(new ConnectionFactory());
        assertRealSessionManager(factoryOf(new BrowserCommand()));
        assertRealSessionManager(factoryOf(new ElementCommand()));
        assertRealSessionManager(factoryOf(new GuideCommand()));
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

    private static ConnectionFactory factoryOf(Object command) throws Exception {
        Object wired = read(command, "factory");
        assertInstanceOf(ConnectionFactory.class, wired);
        return (ConnectionFactory) wired;
    }

    private static void assertRealSessionManager(ConnectionFactory factory) throws Exception {
        SessionManager manager = assertInstanceOf(SessionManager.class, read(factory, "sessionManager"));
        SessionStore store = assertInstanceOf(SessionStore.class, read(manager, "store"));
        assertEquals(Path.of(System.getProperty("user.home"), ".shaft", "cli-session.json"), store.path());
        McpLauncherLocator locator = assertInstanceOf(McpLauncherLocator.class, read(factory, "locator"));
        assertSame(System.getenv(), read(locator, "env"));
        assertNull(read(locator, "versionsRootOverride"));
        assertNull(read(locator, "devRepoRootOverride"));
    }

    private static Object read(Object target, String name) throws Exception {
        Field field = target.getClass().getDeclaredField(name);
        field.setAccessible(true);
        return field.get(target);
    }
}
