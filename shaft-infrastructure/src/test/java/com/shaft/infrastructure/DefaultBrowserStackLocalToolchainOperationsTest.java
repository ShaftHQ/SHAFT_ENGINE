package com.shaft.infrastructure;

import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.net.BindException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DefaultBrowserStackLocalToolchainOperationsTest {
    @Test
    void startRefusesAnUnownedBinary(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        DefaultBrowserStackLocalToolchainOperations operations = new DefaultBrowserStackLocalToolchainOperations(
                paths, plan(), false);

        IOException failure = assertThrows(IOException.class,
                () -> operations.startTunnel(temp.resolve("BrowserStackLocal"), "key", paths.state().resolve("x.log")));
        assertTrue(failure.getMessage().contains("unowned"));
    }

    @Test
    void stopRefusesAnUnownedBinary(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        DefaultBrowserStackLocalToolchainOperations operations = new DefaultBrowserStackLocalToolchainOperations(
                paths, plan(), false);

        IOException failure = assertThrows(IOException.class,
                () -> operations.stopProcess(1, temp.resolve("BrowserStackLocal"), java.time.Duration.ofSeconds(1)));
        assertTrue(failure.getMessage().contains("unowned"));
    }

    @Test
    void startCommandNeverUsesDaemonMode(@TempDir Path temp) {
        List<String> command = DefaultBrowserStackLocalToolchainOperations.startCommand(
                temp.resolve("BrowserStackLocal"), "secret-key");
        assertFalse(command.contains("--daemon"));
        assertFalse(command.contains("stop"));
        assertEquals(List.of(temp.resolve("BrowserStackLocal").toString(), "--key", "secret-key"), command);
    }

    @Test
    void mutatedBinaryDegradesStatus(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        DefaultBrowserStackLocalToolchainOperations operations = new DefaultBrowserStackLocalToolchainOperations(
                paths, plan(), false);
        Path binary = paths.tools().resolve("browserstack-local").resolve("BrowserStackLocal");
        Files.createDirectories(binary.getParent());
        Files.writeString(binary, "not-the-official-binary");
        Files.writeString(Path.of(binary + ".sha256"), "deadbeef");

        assertEquals(SetupReadiness.DEGRADED, operations.status(plan().actions().getFirst()).readiness());
    }

    @Test
    void offlineInstallAndForeignTargetFailBeforeADownload(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        SetupAction action = plan().actions().getFirst();
        DefaultBrowserStackLocalToolchainOperations operations = new DefaultBrowserStackLocalToolchainOperations(
                paths, plan(), true);

        operations.hostPreflight(List.of());
        IOException offline = assertThrows(IOException.class, () -> operations.install(action));
        assertTrue(offline.getMessage().contains("offline cache"));
        SetupAction foreign = new SetupAction(SetupTarget.DOCKER, action.kind(), action.version(), action.source(),
                action.checksum(), false, Set.of());
        assertThrows(IllegalArgumentException.class, () -> operations.install(foreign));
        assertThrows(NullPointerException.class, () -> operations.hostPreflight(null));
    }

    @Test
    void lockedPreflightAndStatusDistinguishMissingDigestAndOfflineCache(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        SetupAction action = plan.actions().getFirst();
        DefaultBrowserStackLocalToolchainOperations offline = new DefaultBrowserStackLocalToolchainOperations(
                paths, plan, true);
        DefaultBrowserStackLocalToolchainOperations online = new DefaultBrowserStackLocalToolchainOperations(
                paths, plan, false);

        assertThrows(IOException.class, () -> offline.lockedPreflight(List.of(action), false));
        online.lockedPreflight(List.of(action), false);
        assertEquals(SetupReadiness.MISSING, offline.status(action).readiness());

        Path binary = paths.tools().resolve("browserstack-local").resolve("BrowserStackLocal");
        Files.createDirectories(binary.getParent());
        Files.writeString(binary, "bytes");
        Files.writeString(Path.of(binary + ".sha256"), sha256(binary));

        SetupStatus status = offline.status(action);
        assertEquals(SetupReadiness.DEGRADED, status.readiness());
        assertTrue(status.detail().contains("offline cache"));
    }

    @Test
    void startStopAndReadinessStayOnTheOwnedBinary(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        DefaultBrowserStackLocalToolchainOperations operations = new DefaultBrowserStackLocalToolchainOperations(
                paths, plan(), true);
        Path binary = paths.tools().resolve("browserstack-local").resolve("BrowserStackLocal");
        Files.createDirectories(binary.getParent());
        Path source = temp.resolve("owned.c");
        Files.writeString(source, """
                #include <unistd.h>
                int main(void) { for (;;) pause(); }
                """);
        Process compiled = new ProcessBuilder("gcc", "-s", "-o", binary.toString(), source.toString())
                .redirectErrorStream(true).start();
        String compilerOutput = new String(compiled.getInputStream().readAllBytes());
        org.junit.jupiter.api.Assumptions.assumeTrue(compiled.waitFor() == 0 && Files.isExecutable(binary),
                compilerOutput);
        Path log = paths.state().resolve("browserstack.log");

        assertThrows(IOException.class, () -> operations.startTunnel(binary, "  ", log));
        assertThrows(IOException.class, () -> operations.processRunning(1, temp.resolve("other")));
        assertFalse(operations.processRunning(ProcessHandle.current().pid(), binary));
        assertFalse(operations.processRunning(Long.MAX_VALUE, binary));
        operations.stopProcess(Long.MAX_VALUE, binary, Duration.ofMillis(50));

        long pid = operations.startTunnel(binary, "secret-key", log);
        try {
            assertTrue(pid > 0);
            assertTrue(Files.isRegularFile(log));
            assertTrue(operations.processRunning(pid, binary));
        } finally {
            operations.stopProcess(pid, binary, Duration.ofSeconds(3));
        }
        assertFalse(ProcessHandle.of(pid).map(ProcessHandle::isAlive).orElse(false));

        AtomicInteger readinessHits = new AtomicInteger();
        HttpServer listener = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        listener.createContext("/", exchange -> {
            readinessHits.incrementAndGet();
            exchange.sendResponseHeaders(204, -1);
            exchange.close();
        });
        listener.start();
        try {
            operations.awaitReady(URI.create("http://127.0.0.1:" + listener.getAddress().getPort() + "/"),
                    Duration.ofSeconds(2));
        } finally {
            listener.stop(0);
        }
        assertTrue(readinessHits.get() > 0);
    }

    @Test
    void publicAwaitReadyUsesTheFixedLocalProbe(@TempDir Path temp) throws Exception {
        DefaultBrowserStackLocalToolchainOperations operations =
                new DefaultBrowserStackLocalToolchainOperations(paths(temp), plan(), true);
        try (ServerSocket probe = new ServerSocket()) {
            probe.bind(new InetSocketAddress("127.0.0.1", 45691));
        } catch (BindException occupied) {
            operations.awaitReady(Duration.ofSeconds(3));
            return;
        }
        HttpServer listener = HttpServer.create(new InetSocketAddress("127.0.0.1", 45691), 0);
        listener.createContext("/", exchange -> {
            exchange.sendResponseHeaders(200, -1);
            exchange.close();
        });
        try {
            listener.start();
            operations.awaitReady(Duration.ofSeconds(3));
        } finally {
            listener.stop(0);
        }
    }

    private static String sha256(Path file) throws Exception {
        return java.util.HexFormat.of().formatHex(
                java.security.MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(file)));
    }

    private static SetupPlan plan() {
        return BrowserStackLocalSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64, SetupMode.MANAGED);
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache");
        Path data = temp.resolve("data");
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }
}
