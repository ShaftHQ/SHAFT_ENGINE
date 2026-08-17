package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DefaultAgentToolsToolchainOperationsTest {
    @Test
    void hostProbeDoesNotCreateStateOrRequireALogFile(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        AtomicReference<Path> logged = new AtomicReference<>();
        DefaultAgentToolsToolchainOperations operations = new DefaultAgentToolsToolchainOperations(
                paths, plan(), (command, workingDirectory, environment, removed, stdin, log, timeout) -> {
                    logged.set(log);
                    return new ReportingSetupService.ProcessResult(0, "openjdk 25");
                }, false);

        operations.hostPreflight(plan().actions());

        assertFalse(Files.exists(paths.state()));
        assertTrue(logged.get() == null);
    }

    @Test
    void installThenStatusIsReadyAndCatalogMutationDegrades(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        SetupPlan plan = plan();
        DefaultAgentToolsToolchainOperations operations = new DefaultAgentToolsToolchainOperations(
                paths, plan, (command, workingDirectory, environment, removed, stdin, log, timeout) ->
                new ReportingSetupService.ProcessResult(0, "ok"), false);
        SetupAction catalog = plan.actions().stream()
                .filter(action -> action.target() == SetupTarget.AGENT_CLI).findFirst().orElseThrow();

        operations.install(catalog);

        assertEquals(SetupReadiness.READY, operations.status(catalog).readiness());
        Path file = paths.tools().resolve("agent-tools").resolve("agent-clients.json");
        assertTrue(Files.isRegularFile(file));

        Files.writeString(file, Files.readString(file) + "\n");
        assertEquals(SetupReadiness.DEGRADED, operations.status(catalog).readiness());
    }

    @Test
    void installingAHostPrerequisiteWritesNothing(@TempDir Path temp) throws Exception {
        ShaftCachePaths paths = paths(temp);
        DefaultAgentToolsToolchainOperations operations = new DefaultAgentToolsToolchainOperations(
                paths, plan(), (command, workingDirectory, environment, removed, stdin, log, timeout) ->
                new ReportingSetupService.ProcessResult(0, "ok"), false);
        SetupAction java = plan().actions().stream()
                .filter(action -> action.target() == SetupTarget.JAVA).findFirst().orElseThrow();

        operations.install(java);

        assertFalse(Files.exists(paths.tools()));
        assertFalse(Files.exists(paths.state()));
    }

    private static SetupPlan plan() {
        return AgentToolsSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64, SetupMode.MANAGED);
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache");
        Path data = temp.resolve("data");
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }
}
