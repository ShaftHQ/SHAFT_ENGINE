package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AgentToolsSetupProviderTest {
    @Test
    void builtInCoordinatorProvidesAgentToolsPlan(@TempDir Path temp) {
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupOptions options = managed(temp);

        assertTrue(service.supports(SetupProfile.AGENT_TOOLS));
        SetupPlan plan = service.plan(options);

        assertEquals(List.of(SetupTarget.JAVA, SetupTarget.MAVEN, SetupTarget.PYTHON, SetupTarget.NODE,
                SetupTarget.AGENT_CLI), plan.actions().stream().map(SetupAction::target).toList());
        assertTrue(plan.actions().stream()
                .filter(action -> action.target() != SetupTarget.AGENT_CLI)
                .allMatch(action -> action.kind() == SetupActionKind.DIAGNOSE));
        assertEquals(SetupActionKind.INSTALL, plan.actions().getLast().kind());
        assertEquals("1", plan.actions().getLast().version());
    }

    @Test
    void hostPrerequisitesAreNeverInstallActions() {
        SetupPlan plan = AgentToolsSetupPlanner.plan(SetupPlatform.WINDOWS, SetupArchitecture.X64, SetupMode.MANAGED);
        assertFalse(plan.actions().stream()
                .filter(action -> action.target() != SetupTarget.AGENT_CLI)
                .anyMatch(action -> action.kind() == SetupActionKind.INSTALL));
    }

    @Test
    void managedProviderInstallsTheClientCatalog(@TempDir Path temp) throws Exception {
        RecordingOperations operations = new RecordingOperations();
        InfrastructureSetupService service = coordinator(operations);
        SetupOptions options = managed(temp);
        SetupPlan plan = service.plan(options);

        SetupReceipt receipt = service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()),
                options);

        assertEquals(plan.digest(), receipt.planDigest());
        assertTrue(operations.events.contains("install:" + SetupTarget.AGENT_CLI));
    }

    @Test
    void externalModeCannotInstall(@TempDir Path temp) {
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.LINUX, SetupArchitecture.X64);
        Path cache = temp.resolve("cache");
        Path data = temp.resolve("data");
        SetupOptions options = SetupOptions.defaults(SetupProfile.AGENT_TOOLS, new ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts")));
        SetupPlan plan = service.plan(options);

        assertThrows(IllegalArgumentException.class,
                () -> service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), options));
        assertFalse(Files.exists(options.paths().receipts().resolve("agent-tools.json")));
    }

    @Test
    void externalStatusStillProbesHostTools(@TempDir Path temp) {
        RecordingOperations operations = new RecordingOperations();
        InfrastructureSetupService service = coordinator(operations);
        Path cache = temp.resolve("cache");
        Path data = temp.resolve("data");
        SetupOptions options = SetupOptions.defaults(SetupProfile.AGENT_TOOLS, new ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts")));

        service.status(options);

        assertTrue(operations.events.contains("status:" + SetupTarget.JAVA));
        assertTrue(operations.events.contains("status:" + SetupTarget.AGENT_CLI));
    }

    private static InfrastructureSetupService coordinator(RecordingOperations operations) {
        return new InfrastructureSetupService(new SetupProviderRegistry(
                List.of(new AgentToolsSetupProvider((paths, plan, offline) -> operations))),
                SetupPlatform.LINUX, SetupArchitecture.X64);
    }

    private static SetupOptions managed(Path root) {
        Path cache = root.resolve("cache");
        Path data = root.resolve("data");
        return SetupOptions.defaults(SetupProfile.AGENT_TOOLS, new ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts")))
                .withMode(SetupMode.MANAGED);
    }

    private static final class RecordingOperations implements AgentToolsToolchainOperations {
        private final List<String> events = new ArrayList<>();
        private boolean installed;

        @Override
        public void hostPreflight(List<SetupAction> actions) {
            events.add("host-preflight");
        }

        @Override
        public void lockedPreflight(List<SetupAction> actions, boolean offline) {
            events.add("locked-preflight");
        }

        @Override
        public void install(SetupAction action) {
            events.add("install:" + action.target());
            if (action.target() == SetupTarget.AGENT_CLI) installed = true;
        }

        @Override
        public SetupStatus status(SetupAction action) {
            events.add("status:" + action.target());
            if (action.target() == SetupTarget.AGENT_CLI) {
                return new SetupStatus(action.target(),
                        installed ? SetupReadiness.READY : SetupReadiness.MISSING,
                        installed ? action.version() : "", installed ? "fixture" : "missing");
            }
            return new SetupStatus(action.target(), SetupReadiness.READY, action.version(), "fixture");
        }
    }
}
