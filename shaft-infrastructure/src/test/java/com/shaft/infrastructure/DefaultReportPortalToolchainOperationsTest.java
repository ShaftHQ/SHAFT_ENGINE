package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DefaultReportPortalToolchainOperationsTest {
    @Test
    void dockerProbeDoesNotCreateStateOrRequireALogFile(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        AtomicReference<Path> logged = new AtomicReference<>();
        DefaultReportPortalToolchainOperations operations = new DefaultReportPortalToolchainOperations(
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
        DefaultReportPortalToolchainOperations operations = new DefaultReportPortalToolchainOperations(
                paths, plan(), (command, workingDirectory, environment, removed, stdin, log, timeout) -> {
                    throw new java.io.IOException("docker daemon unavailable");
                }, false);

        assertThrows(java.io.IOException.class, () -> operations.composeRunning(
                paths.tools().resolve("reportportal").resolve("docker-compose.yml"),
                ReportPortalSetupPlanner.PROJECT));
    }

    @Test
    void composeInspectNonZeroExitIsNotTreatedAsStopped(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        DefaultReportPortalToolchainOperations operations = new DefaultReportPortalToolchainOperations(
                paths, plan(), (command, workingDirectory, environment, removed, stdin, log, timeout) ->
                new ReportingSetupService.ProcessResult(1, "cannot connect to docker"), false);

        assertThrows(java.io.IOException.class, () -> operations.composeRunning(
                paths.tools().resolve("reportportal").resolve("docker-compose.yml"),
                ReportPortalSetupPlanner.PROJECT));
    }

    @Test
    void composeInspectRefusesAnUnownedProject(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        DefaultReportPortalToolchainOperations operations = new DefaultReportPortalToolchainOperations(
                paths, plan(), (command, workingDirectory, environment, removed, stdin, log, timeout) ->
                new ReportingSetupService.ProcessResult(0, "abc123"), false);

        IOException failure = assertThrows(java.io.IOException.class, () -> operations.composeRunning(
                paths.tools().resolve("reportportal").resolve("docker-compose.yml"), "reportportal"));
        assertTrue(failure.getMessage().contains("unowned"));
    }

    @Test
    void installThenStatusIsReadyAndComposeMutationDegrades(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        DefaultReportPortalToolchainOperations operations = new DefaultReportPortalToolchainOperations(
                paths, plan, (command, workingDirectory, environment, removed, stdin, log, timeout) ->
                new ReportingSetupService.ProcessResult(0, "Docker Compose version v2.34.0"), false);
        SetupAction reportPortal = plan.actions().stream()
                .filter(action -> action.target() == SetupTarget.REPORT_PORTAL).findFirst().orElseThrow();

        operations.install(reportPortal);

        assertEquals(SetupReadiness.READY, operations.status(reportPortal).readiness());
        Path compose = paths.tools().resolve("reportportal").resolve("docker-compose.yml");
        assertTrue(Files.isRegularFile(compose));

        Files.writeString(compose, Files.readString(compose) + "\n# mutated\n");
        assertEquals(SetupReadiness.DEGRADED, operations.status(reportPortal).readiness());
    }

    private static SetupPlan plan() {
        return ReportPortalSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64, SetupMode.MANAGED,
                SetupSelection.defaults());
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache");
        Path data = temp.resolve("data");
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }
}
