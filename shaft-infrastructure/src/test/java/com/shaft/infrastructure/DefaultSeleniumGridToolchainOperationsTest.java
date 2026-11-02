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

class DefaultSeleniumGridToolchainOperationsTest {
    @Test
    void dockerProbeDoesNotCreateStateOrRequireALogFile(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        AtomicReference<Path> logged = new AtomicReference<>();
        DefaultSeleniumGridToolchainOperations operations = new DefaultSeleniumGridToolchainOperations(
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
        DefaultSeleniumGridToolchainOperations operations = new DefaultSeleniumGridToolchainOperations(
                paths, plan(), (command, workingDirectory, environment, removed, stdin, log, timeout) -> {
                    throw new java.io.IOException("docker daemon unavailable");
                }, false);

        assertThrows(java.io.IOException.class, () -> operations.composeRunning(
                paths.tools().resolve("selenium-grid").resolve("docker-compose.yml"),
                SeleniumGridSetupPlanner.PROJECT));
    }

    @Test
    void installStatusAndComposeStayOnTheOwnedProject(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        AtomicReference<List<String>> seen = new AtomicReference<>();
        DefaultSeleniumGridToolchainOperations operations = new DefaultSeleniumGridToolchainOperations(
                paths, plan, (command, workingDirectory, environment, removed, stdin, log, timeout) -> {
                    seen.set(List.copyOf(command));
                    return new ReportingSetupService.ProcessResult(0, "");
                }, false);
        SetupAction docker = action(plan, SetupTarget.DOCKER);
        SetupAction grid = action(plan, SetupTarget.SELENIUM_GRID);
        Path compose = paths.tools().resolve("selenium-grid").resolve("docker-compose.yml");

        assertEquals(SetupReadiness.READY, operations.status(docker).readiness());
        assertEquals(SetupReadiness.MISSING, operations.status(grid).readiness());
        operations.install(docker);
        assertFalse(Files.exists(compose));
        java.io.IOException mismatch = assertThrows(java.io.IOException.class, () -> operations.install(
                new SetupAction(SetupTarget.SELENIUM_GRID, grid.kind(), grid.version(), grid.source(),
                        "sha256:" + "ab".repeat(32), false, Set.of())));
        assertTrue(mismatch.getMessage().contains("does not match"));
        assertThrows(IllegalArgumentException.class, () -> operations.install(
                new SetupAction(SetupTarget.HEALENIUM, grid.kind(), grid.version(), grid.source(),
                        grid.checksum(), false, Set.of())));
        assertThrows(java.io.IOException.class, () -> operations.lockedPreflight(plan.actions(), true));

        operations.install(grid);
        operations.lockedPreflight(plan.actions(), true);
        assertEquals(SetupReadiness.READY, operations.status(grid).readiness());
        assertFalse(operations.composeRunning(compose, SeleniumGridSetupPlanner.PROJECT));
        operations.composeUp(compose, SeleniumGridSetupPlanner.PROJECT,
                SeleniumGridSetupPlanner.scaleFromPlan(plan));
        operations.composeDown(compose, SeleniumGridSetupPlanner.PROJECT);
        assertTrue(seen.get().contains("down"));
        assertTrue(Files.isDirectory(paths.state().resolve("logs")));

        Files.writeString(compose, Files.readString(compose) + "\n# mutated\n");
        assertEquals(SetupReadiness.DEGRADED, operations.status(grid).readiness());

        java.io.IOException unowned = assertThrows(java.io.IOException.class,
                () -> operations.composeRunning(compose, "not-shaft"));
        assertTrue(unowned.getMessage().contains("unowned"));
    }

    @Test
    void composeFailureAndClosedReadinessDoNotTreatTheGridAsReady(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        DefaultSeleniumGridToolchainOperations failing = new DefaultSeleniumGridToolchainOperations(
                paths, plan, (command, workingDirectory, environment, removed, stdin, log, timeout) ->
                new ReportingSetupService.ProcessResult(1, "cannot connect to docker"), false);
        Path compose = paths.tools().resolve("selenium-grid").resolve("docker-compose.yml");
        java.io.IOException startup = assertThrows(java.io.IOException.class, () -> failing.composeUp(
                compose, SeleniumGridSetupPlanner.PROJECT, SeleniumGridSetupPlanner.scaleFromPlan(plan)));
        assertTrue(startup.getMessage().contains("Failed to start"));
        assertThrows(java.io.IOException.class, () -> failing.composeRunning(compose, SeleniumGridSetupPlanner.PROJECT));
        assertEquals(SetupReadiness.MISSING, failing.status(action(plan, SetupTarget.DOCKER)).readiness());

        DefaultSeleniumGridToolchainOperations windows = new DefaultSeleniumGridToolchainOperations(
                paths, SeleniumGridSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64, SetupMode.MANAGED,
                        SetupSelection.defaults()),
                (command, workingDirectory, environment, removed, stdin, log, timeout) -> {
                    assertEquals("docker.exe", command.getFirst());
                    return new ReportingSetupService.ProcessResult(0, "container");
                }, false);
        assertTrue(windows.composeRunning(compose, SeleniumGridSetupPlanner.PROJECT));

        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/wd/hub/status", exchange -> {
            byte[] body = "ok".getBytes(StandardCharsets.UTF_8);
            exchange.sendResponseHeaders(200, body.length);
            exchange.getResponseBody().write(body);
            exchange.close();
        });
        server.start();
        try {
            windows.awaitReady(URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/"),
                    Duration.ofSeconds(2));
        } finally {
            server.stop(0);
        }
        assertThrows(java.io.IOException.class,
                () -> windows.awaitReady(URI.create("http://127.0.0.1:9/"), Duration.ofMillis(200)));
    }

    private static SetupAction action(SetupPlan plan, SetupTarget target) {
        return plan.actions().stream().filter(candidate -> candidate.target() == target).findFirst().orElseThrow();
    }

    private static SetupPlan plan() {
        return SeleniumGridSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64, SetupMode.MANAGED,
                SetupSelection.defaults());
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache");
        Path data = temp.resolve("data");
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }
}
