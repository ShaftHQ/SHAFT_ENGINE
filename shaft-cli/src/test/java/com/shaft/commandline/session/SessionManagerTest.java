package com.shaft.commandline.session;

import com.shaft.commandline.mcp.McpException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Session lifecycle against a temp store. The ready path is a local JDK HTTP server compiled in
 * the test, not a browser. Daemon logs still follow the production {@code ~/.shaft/logs} path and
 * are removed by port when the assertion can see that port.
 */
class SessionManagerTest {
    private static final Pattern DAEMON_LOG = Pattern.compile("daemon-(\\d+)\\.log");

    @TempDir
    Path temp;

    @Test
    void statusAndStopReportADeadSessionWithoutLaunching() throws Exception {
        SessionStore store = new SessionStore(temp);
        SessionManager manager = new SessionManager(locatorForMissingMain(), store);
        Process exited = new ProcessBuilder("true").start();
        assertTrue(exited.waitFor(5, TimeUnit.SECONDS));
        store.save(new SessionInfo(9, exited.pid(), "missing.jar", "2026-10-28T00:00:00Z"));

        SessionStatus status = manager.status();

        assertFalse(status.running());
        assertEquals(exited.pid(), status.info().pid());
        assertTrue(manager.liveSession().isEmpty());
        assertTrue(manager.stop());
        assertFalse(Files.exists(store.path()));
        assertFalse(manager.stop());
        assertFalse(manager.status().running());
        assertNull(manager.status().info());
    }

    @Test
    void startRejectsASessionWhoseProcessIsStillAlive() throws IOException {
        SessionStore store = new SessionStore(temp);
        SessionManager manager = new SessionManager(locatorForMissingMain(), store);
        store.save(new SessionInfo(9, ProcessHandle.current().pid(), "jar", "2026-10-28T00:00:00Z"));

        McpException failure = assertThrows(McpException.class, manager::start);

        assertTrue(failure.getMessage().contains("already running"));
        assertTrue(manager.liveSession().isPresent());
        assertEquals(ProcessHandle.current().pid(), manager.status().info().pid());
    }

    @Test
    void startReportsTheLogTailWhenTheDaemonExitsImmediately() throws IOException {
        SessionStore store = new SessionStore(temp);
        SessionManager manager = new SessionManager(locatorForMissingMain(), store);

        McpException failure = assertThrows(McpException.class, manager::start);

        assertTrue(failure.getMessage().contains("exited during startup"));
        assertTrue(failure.getMessage().contains("daemon-"));
        assertTrue(store.load().isEmpty());
        deleteDaemonLog(failure.getMessage());
    }

    @Test
    void startBecomesReadyThenStopClearsTheLiveProcess() throws Exception {
        Path classes = compileReadyServer();
        Path jar = temp.resolve("shaft-mcp.jar");
        Files.writeString(jar, "placeholder");
        Files.writeString(jar.resolveSibling("shaft-mcp.args"),
                "-cp" + System.lineSeparator()
                        + '"' + classes.toAbsolutePath().normalize().toString().replace('\\', '/') + '"'
                        + System.lineSeparator()
                        + "com.shaft.mcp.ShaftMcpApplication" + System.lineSeparator());
        SessionStore store = new SessionStore(temp.resolve("store"));
        McpLauncherLocator locator = new McpLauncherLocator(
                Map.of("SHAFT_MCP_JAR", jar.toString()), temp.resolve("no-versions"), temp.resolve("no-dev"));
        SessionManager manager = new SessionManager(locator, store);
        SessionInfo started = null;
        try {
            started = manager.start();
            assertTrue(started.port() > 0);
            assertEquals(jar.toString(), started.jarPath());
            assertTrue(manager.status().running());
            assertEquals(started.pid(), manager.liveSession().orElseThrow().pid());
            assertTrue(ProcessHandle.of(started.pid()).orElseThrow().isAlive());
            McpException duplicate = assertThrows(McpException.class, manager::start);
            assertTrue(duplicate.getMessage().contains("already running"));

            assertTrue(manager.stop());
            assertFalse(Files.exists(store.path()));
            long pid = started.pid();
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
            while (ProcessHandle.of(pid).map(ProcessHandle::isAlive).orElse(false)
                    && System.nanoTime() < deadline) {
                Thread.sleep(20);
            }
            assertFalse(ProcessHandle.of(pid).map(ProcessHandle::isAlive).orElse(false));
            assertFalse(manager.stop());
        } finally {
            if (started != null) {
                manager.stop();
                deleteDaemonLog("daemon-" + started.port() + ".log");
            }
        }
    }

    private McpLauncherLocator locatorForMissingMain() throws IOException {
        Path jar = temp.resolve("standalone-shaft-mcp.jar");
        Files.writeString(jar, "not-a-jar");
        return new McpLauncherLocator(
                Map.of("SHAFT_MCP_JAR", jar.toString()), temp.resolve("no-versions"), temp.resolve("no-dev"));
    }

    private Path compileReadyServer() throws Exception {
        Path source = temp.resolve("src/com/shaft/mcp/ShaftMcpApplication.java");
        Files.createDirectories(source.getParent());
        Files.writeString(source, """
                package com.shaft.mcp;
                import com.sun.net.httpserver.HttpServer;
                import java.net.InetSocketAddress;
                public final class ShaftMcpApplication {
                    public static void main(String[] args) throws Exception {
                        int port = -1;
                        for (String arg : args) {
                            if (arg.startsWith("--server.port=")) {
                                port = Integer.parseInt(arg.substring("--server.port=".length()));
                            }
                        }
                        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", port), 0);
                        server.createContext("/mcp", exchange -> {
                            exchange.getRequestBody().readAllBytes();
                            exchange.sendResponseHeaders(200, -1);
                            exchange.close();
                        });
                        server.start();
                        Thread.currentThread().join();
                    }
                }
                """);
        Path classes = temp.resolve("classes");
        Files.createDirectories(classes);
        Process compile = new ProcessBuilder(
                Path.of(System.getProperty("java.home"), "bin", "javac").toString(),
                "-d", classes.toString(),
                source.toString()).redirectErrorStream(true).start();
        String compilerOutput = new String(compile.getInputStream().readAllBytes());
        assertEquals(0, compile.waitFor(), compilerOutput);
        return classes;
    }

    private static void deleteDaemonLog(String message) {
        Matcher matcher = DAEMON_LOG.matcher(message);
        if (!matcher.find()) {
            return;
        }
        try {
            Files.deleteIfExists(Path.of(System.getProperty("user.home"), ".shaft", "logs", matcher.group()));
        } catch (IOException ignored) {
            // Best-effort removal of this test's daemon log only.
        }
    }
}
