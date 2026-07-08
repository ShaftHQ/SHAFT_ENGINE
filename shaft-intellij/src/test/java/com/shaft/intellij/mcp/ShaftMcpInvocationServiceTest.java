package com.shaft.intellij.mcp;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises {@link ShaftMcpInvocationService#acquireClient} directly, bypassing the IntelliJ
 * platform via the package-private client-factory seam, to pin the "one shared MCP process across
 * tool calls" contract documented on the class. Without it, a scoped command that is not
 * value-stable across calls (see {@link ShaftMcpProjectScopeTest}) silently defeats process reuse.
 */
class ShaftMcpInvocationServiceTest {
    private final List<ShaftMcpStdioClient> spawned = new ArrayList<>();

    @AfterEach
    void closeSpawnedClients() {
        spawned.forEach(ShaftMcpStdioClient::close);
        spawned.clear();
    }

    @Test
    void identicalCommandReusesTheSameClientInstance() throws IOException {
        RecordingFactory factory = new RecordingFactory();
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(null, factory);
        List<String> command = echoToolCommand();

        ShaftMcpStdioClient first = service.acquireClient(command, Path.of("."), Map.of());
        ShaftMcpStdioClient second = service.acquireClient(command, Path.of("."), Map.of());

        assertSame(first, second,
                "Two calls with an unchanged scoped command must reuse the shared MCP process so a live "
                        + "session (e.g. a SHAFT Capture recording) is not torn down between tool calls.");
        assertEquals(1, factory.created.size());
    }

    @Test
    void changedCommandClosesOldClientAndCreatesNew() throws IOException {
        RecordingFactory factory = new RecordingFactory();
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(null, factory);
        List<String> commandA = echoToolCommand();
        List<String> commandB = new ArrayList<>(commandA);
        commandB.add("--unused-marker");

        ShaftMcpStdioClient first = service.acquireClient(commandA, Path.of("."), Map.of());
        ShaftMcpStdioClient second = service.acquireClient(commandB, Path.of("."), Map.of());

        assertNotSame(first, second,
                "A changed scoped command (e.g. from a genuinely changed source argfile) must respawn.");
        assertEquals(2, factory.created.size());
        assertFalse(first.isAlive(), "The superseded process must be closed once the new one is acquired.");
    }

    @Test
    void deadClientIsRecreated() throws IOException {
        RecordingFactory factory = new RecordingFactory();
        ShaftMcpInvocationService service = new ShaftMcpInvocationService(null, factory);
        List<String> command = echoToolCommand();

        ShaftMcpStdioClient first = service.acquireClient(command, Path.of("."), Map.of());
        first.kill();
        ShaftMcpStdioClient second = service.acquireClient(command, Path.of("."), Map.of());

        assertNotSame(first, second, "A dead shared client must be replaced even when the command is unchanged.");
        assertEquals(2, factory.created.size());
        assertTrue(second.isAlive());
    }

    private static List<String> echoToolCommand() {
        return List.of(javaExecutable(), "-cp", System.getProperty("java.class.path"),
                FakeMcpServer.class.getName(), "echoTool");
    }

    private static String javaExecutable() {
        String javaHome = System.getProperty("java.home");
        boolean windows = System.getProperty("os.name").toLowerCase(Locale.ROOT).contains("win");
        return Paths.get(javaHome, "bin", windows ? "java.exe" : "java").toString();
    }

    private final class RecordingFactory implements ShaftMcpInvocationService.ShaftMcpClientFactory {
        private final List<ShaftMcpStdioClient> created = new ArrayList<>();

        @Override
        public ShaftMcpStdioClient create(List<String> command, Path workingDirectory, Map<String, String> environment)
                throws IOException {
            ShaftMcpStdioClient client = new ShaftMcpStdioClient(command, workingDirectory, environment);
            created.add(client);
            spawned.add(client);
            return client;
        }
    }
}
