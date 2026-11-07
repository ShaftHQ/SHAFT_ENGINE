package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import org.junit.jupiter.api.Assumptions;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
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

    @Test
    void lockedPreflightRequiresAStagedComposeFileWhenOffline(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        DefaultReportPortalToolchainOperations offline = operations(paths, plan, 0, "", true);

        IOException missing = assertThrows(IOException.class, () -> offline.lockedPreflight(plan.actions(), false));
        assertTrue(missing.getMessage().contains("staged compose"));

        Path compose = paths.tools().resolve("reportportal").resolve("docker-compose.yml");
        Files.createDirectories(compose.getParent());
        Files.writeString(compose, "services: {}\n");
        offline.lockedPreflight(plan.actions(), false);
        operations(paths, plan, 0, "", false).lockedPreflight(plan.actions(), true);
        assertTrue(Files.isRegularFile(compose));
    }

    @Test
    void installRejectsAForeignTargetAndARenderedChecksumMismatch(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        SetupAction reportPortal = reportPortal(plan);
        DefaultReportPortalToolchainOperations operations = operations(paths, plan, 0, "", false);
        SetupAction foreign = new SetupAction(SetupTarget.HEALENIUM, reportPortal.kind(), reportPortal.version(),
                reportPortal.source(), reportPortal.checksum(), reportPortal.artifactBytes(), false, Set.of());

        IllegalArgumentException rejected = assertThrows(IllegalArgumentException.class,
                () -> operations.install(foreign));
        assertTrue(rejected.getMessage().contains("HEALENIUM"));

        SetupAction mismatched = new SetupAction(reportPortal.target(), reportPortal.kind(), reportPortal.version(),
                reportPortal.source(), "sha256:" + "ab".repeat(32), reportPortal.artifactBytes(), false, Set.of());
        IOException checksum = assertThrows(IOException.class, () -> operations.install(mismatched));
        assertTrue(checksum.getMessage().contains("does not match the approved plan"));
        assertFalse(Files.exists(paths.tools().resolve("reportportal").resolve("docker-compose.yml")));
    }

    @Test
    void dockerProbeFailureIsMissingAndHostPreflightFailsClosed(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        DefaultReportPortalToolchainOperations operations = operations(paths, plan, 1, "daemon down", false);
        SetupAction docker = plan.actions().stream()
                .filter(action -> action.target() == SetupTarget.DOCKER).findFirst().orElseThrow();

        SetupStatus status = operations.status(docker);
        assertEquals(SetupReadiness.MISSING, status.readiness());
        assertTrue(status.detail().contains("Docker Engine"));

        IOException failure = assertThrows(IOException.class, () -> operations.hostPreflight(plan.actions()));
        assertTrue(failure.getMessage().contains("Docker Engine"));
        assertTrue(failure.getCause().getMessage().contains("Docker Engine"));
    }

    @Test
    void missingComposeIsNotReadyAndALinkedAncestorDegradesStatus(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        DefaultReportPortalToolchainOperations operations = operations(paths, plan, 0, "", false);

        SetupStatus missing = operations.status(reportPortal(plan));
        assertEquals(SetupReadiness.MISSING, missing.readiness());
        assertTrue(missing.detail().contains("missing"));

        Path tools = paths.tools();
        Files.createDirectories(tools.getParent());
        Files.createDirectories(temp.resolve("actual-tools"));
        try {
            Files.createSymbolicLink(tools, temp.resolve("actual-tools"));
        } catch (UnsupportedOperationException | IOException unsupported) {
            Assumptions.abort("Directory links unavailable: " + unsupported.getMessage());
        }

        SetupStatus linked = operations.status(reportPortal(plan));
        assertEquals(SetupReadiness.DEGRADED, linked.readiness());
        assertTrue(linked.detail().contains("symbolic links"));
    }

    @Test
    void composeMutationsStayOnTheOwnedProjectAndSurfaceRunnerFailure(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        List<List<String>> commands = new ArrayList<>();
        AtomicReference<Path> log = new AtomicReference<>();
        AtomicReference<Path> working = new AtomicReference<>();
        AtomicInteger exit = new AtomicInteger();
        DefaultReportPortalToolchainOperations operations = new DefaultReportPortalToolchainOperations(
                paths, plan, (command, workingDirectory, environment, removed, stdin, logged, timeout) -> {
                    commands.add(List.copyOf(command));
                    log.set(logged);
                    working.set(workingDirectory);
                    return new ReportingSetupService.ProcessResult(exit.get(), "runner-output");
                }, false);
        Path compose = paths.tools().resolve("reportportal").resolve("docker-compose.yml");

        IOException unowned = assertThrows(IOException.class, () -> operations.composeUp(compose, "reportportal"));
        assertTrue(unowned.getMessage().contains("unowned"));
        assertTrue(commands.isEmpty());
        assertThrows(IOException.class, () -> operations.composeDown(compose, "reportportal"));
        assertTrue(commands.isEmpty());

        operations.composeUp(compose, ReportPortalSetupPlanner.PROJECT);
        assertEquals(List.of("docker", "compose", "-p", ReportPortalSetupPlanner.PROJECT, "-f", compose.toString(),
                "up", "-d"), commands.getFirst());
        assertEquals(paths.state().resolve("logs").resolve("reportportal.log"), log.get());
        assertEquals(compose.getParent(), working.get());

        operations.composeDown(compose, ReportPortalSetupPlanner.PROJECT);
        assertEquals(List.of("docker", "compose", "-p", ReportPortalSetupPlanner.PROJECT, "-f", compose.toString(),
                "down", "--remove-orphans"), commands.get(1));

        exit.set(1);
        IOException failed = assertThrows(IOException.class,
                () -> operations.composeUp(compose, ReportPortalSetupPlanner.PROJECT));
        assertTrue(failed.getMessage().contains("Failed to start"));
        assertTrue(failed.getMessage().contains("runner-output"));
    }

    @Test
    void composeInspectTreatsBlankOutputAsStopped(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        AtomicReference<String> output = new AtomicReference<>("   ");
        DefaultReportPortalToolchainOperations operations = new DefaultReportPortalToolchainOperations(
                paths, plan(), (command, workingDirectory, environment, removed, stdin, log, timeout) ->
                new ReportingSetupService.ProcessResult(0, output.get()), false);
        Path compose = paths.tools().resolve("reportportal").resolve("docker-compose.yml");

        assertFalse(operations.composeRunning(compose, ReportPortalSetupPlanner.PROJECT));
        output.set("cid-1\n");
        assertTrue(operations.composeRunning(compose, ReportPortalSetupPlanner.PROJECT));
    }

    @Test
    void awaitReadyWithNoTimeRemainingFailsBeforeProbing(@TempDir Path temp) {
        DefaultReportPortalToolchainOperations operations = operations(paths(temp), plan(), 0, "", false);

        IOException failure = assertThrows(IOException.class,
                () -> operations.awaitReady(URI.create("http://127.0.0.1:9/ui"), Duration.ZERO));

        assertEquals("ReportPortal did not become ready.", failure.getMessage());
    }

    private static DefaultReportPortalToolchainOperations operations(ShaftCachePaths paths, SetupPlan plan, int exit,
                                                                     String output, boolean offline) {
        return new DefaultReportPortalToolchainOperations(paths, plan,
                (command, workingDirectory, environment, removed, stdin, log, timeout) ->
                        new ReportingSetupService.ProcessResult(exit, output), offline);
    }

    private static SetupAction reportPortal(SetupPlan plan) {
        return plan.actions().stream()
                .filter(action -> action.target() == SetupTarget.REPORT_PORTAL).findFirst().orElseThrow();
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
