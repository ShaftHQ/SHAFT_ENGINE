package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicReference;

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
