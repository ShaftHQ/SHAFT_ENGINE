package com.shaft.infrastructure;

import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DefaultHealeniumToolchainOperationsTest {
    @Test
    void dockerProbeDoesNotCreateStateOrRequireALogFile(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        AtomicReference<Path> logged = new AtomicReference<>();
        DefaultHealeniumToolchainOperations operations = new DefaultHealeniumToolchainOperations(
                paths, plan(), (command, workingDirectory, environment, removed, stdin, log, timeout) -> {
                    logged.set(log);
                    return new ReportingSetupService.ProcessResult(0, "Docker Compose version v2.34.0");
                }, false);

        operations.hostPreflight(plan().actions());

        assertFalse(Files.exists(paths.state()));
        assertTrue(logged.get() == null);
    }

    @Test
    void composeInspectFailureIsNotTreatedAsStopped(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        DefaultHealeniumToolchainOperations operations = new DefaultHealeniumToolchainOperations(
                paths, plan(), (command, workingDirectory, environment, removed, stdin, log, timeout) -> {
                    throw new java.io.IOException("docker daemon unavailable");
                }, false);

        assertThrows(java.io.IOException.class, () -> operations.composeRunning(
                paths.tools().resolve("healenium").resolve("docker-compose.yml"),
                HealeniumSetupPlanner.PROJECT));
    }

    @Test
    void installThenStatusIsReadyAndInitMutationDegrades(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        DefaultHealeniumToolchainOperations operations = new DefaultHealeniumToolchainOperations(
                paths, plan, (command, workingDirectory, environment, removed, stdin, log, timeout) ->
                new ReportingSetupService.ProcessResult(0, "Docker Compose version v2.34.0"), false);
        SetupAction healenium = plan.actions().stream()
                .filter(action -> action.target() == SetupTarget.HEALENIUM).findFirst().orElseThrow();

        operations.install(healenium);

        assertEquals(SetupReadiness.READY, operations.status(healenium).readiness());
        assertTrue(Files.isRegularFile(paths.tools().resolve("healenium").resolve("docker-compose.yml")));
        assertTrue(Files.isRegularFile(paths.tools().resolve("healenium").resolve("init.sql")));

        Files.writeString(paths.tools().resolve("healenium").resolve("init.sql"), "DROP SCHEMA healenium;\n");
        assertEquals(SetupReadiness.DEGRADED, operations.status(healenium).readiness());
    }

    @Test
    void composeLifecycleRejectsForeignProjectsAndABadRender(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        AtomicReference<List<String>> seen = new AtomicReference<>();
        DefaultHealeniumToolchainOperations operations = new DefaultHealeniumToolchainOperations(
                paths, plan, (command, workingDirectory, environment, removed, stdin, log, timeout) -> {
                    seen.set(List.copyOf(command));
                    return new ReportingSetupService.ProcessResult(0, "abc");
                }, false);
        SetupAction docker = action(plan, SetupTarget.DOCKER);
        SetupAction healenium = action(plan, SetupTarget.HEALENIUM);
        Path compose = paths.tools().resolve("healenium").resolve("docker-compose.yml");

        assertEquals(SetupReadiness.READY, operations.status(docker).readiness());
        assertEquals(SetupReadiness.MISSING, operations.status(healenium).readiness());
        operations.install(docker);
        assertThrows(java.io.IOException.class, () -> operations.lockedPreflight(plan.actions(), true));
        java.io.IOException mismatch = assertThrows(java.io.IOException.class, () -> operations.install(
                new SetupAction(SetupTarget.HEALENIUM, healenium.kind(), healenium.version(), healenium.source(),
                        "sha256:" + "cd".repeat(32), false, Set.of())));
        assertTrue(mismatch.getMessage().contains("does not match"));
        assertThrows(IllegalArgumentException.class, () -> operations.install(
                new SetupAction(SetupTarget.SELENIUM_GRID, healenium.kind(), healenium.version(),
                        healenium.source(), healenium.checksum(), false, Set.of())));
        assertThrows(NullPointerException.class, () -> operations.hostPreflight(null));

        operations.install(healenium);
        operations.lockedPreflight(plan.actions(), true);
        assertTrue(operations.composeRunning(compose, HealeniumSetupPlanner.PROJECT));
        operations.composeUp(compose, HealeniumSetupPlanner.PROJECT);
        operations.composeDown(compose, HealeniumSetupPlanner.PROJECT);
        assertTrue(seen.get().contains("--remove-orphans"));
        java.io.IOException unowned = assertThrows(java.io.IOException.class,
                () -> operations.composeDown(compose, "other"));
        assertTrue(unowned.getMessage().contains("unowned"));

        DefaultHealeniumToolchainOperations failing = new DefaultHealeniumToolchainOperations(
                paths, plan, (command, workingDirectory, environment, removed, stdin, log, timeout) ->
                new ReportingSetupService.ProcessResult(7, "down failed"), false);
        java.io.IOException stopped = assertThrows(java.io.IOException.class,
                () -> failing.composeDown(compose, HealeniumSetupPlanner.PROJECT));
        assertTrue(stopped.getMessage().contains("Failed to stop"));
        assertEquals(SetupReadiness.MISSING,
                new DefaultHealeniumToolchainOperations(paths, plan,
                        (command, workingDirectory, environment, removed, stdin, log, timeout) -> {
                            throw new java.io.IOException("docker daemon unavailable");
                        }, true).status(docker).readiness());

        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/backend", exchange -> {
            exchange.sendResponseHeaders(204, -1);
            exchange.close();
        });
        server.createContext("/imitate", exchange -> {
            byte[] body = "ok".getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        server.start();
        try {
            int port = server.getAddress().getPort();
            operations.awaitReady(
                    URI.create("http://127.0.0.1:" + port + "/backend"),
                    URI.create("http://127.0.0.1:" + port + "/imitate"),
                    Duration.ofSeconds(2));
        } finally {
            server.stop(0);
        }
        assertThrows(java.io.IOException.class, () -> operations.awaitReady(
                URI.create("http://127.0.0.1:9/backend"),
                URI.create("http://127.0.0.1:9/imitate"),
                Duration.ofMillis(200)));

        HttpServer imitatorDown = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        imitatorDown.createContext("/backend", exchange -> {
            exchange.sendResponseHeaders(204, -1);
            exchange.close();
        });
        imitatorDown.createContext("/imitate", exchange -> {
            exchange.sendResponseHeaders(500, -1);
            exchange.close();
        });
        imitatorDown.start();
        try {
            int port = imitatorDown.getAddress().getPort();
            java.io.IOException imitator = assertThrows(java.io.IOException.class, () -> operations.awaitReady(
                    URI.create("http://127.0.0.1:" + port + "/backend"),
                    URI.create("http://127.0.0.1:" + port + "/imitate"),
                    Duration.ofMillis(400)));
            assertEquals("Healenium selector imitator status returned HTTP 500.", imitator.getMessage());
        } finally {
            imitatorDown.stop(0);
        }
    }

    private static SetupAction action(SetupPlan plan, SetupTarget target) {
        return plan.actions().stream().filter(candidate -> candidate.target() == target).findFirst().orElseThrow();
    }

    private static SetupPlan plan() {
        return HealeniumSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64, SetupMode.MANAGED,
                SetupSelection.defaults());
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache");
        Path data = temp.resolve("data");
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }
}
