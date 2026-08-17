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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SeleniumGridSetupProviderTest {
    @Test
    void builtInCoordinatorProvidesSeleniumGridPlan(@TempDir Path temp) {
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupOptions options = managed(temp);

        assertTrue(service.supports(SetupProfile.SELENIUM_GRID));
        SetupPlan plan = service.plan(options);

        assertEquals(List.of(SetupTarget.DOCKER, SetupTarget.SELENIUM_GRID),
                plan.actions().stream().map(SetupAction::target).toList());
        assertEquals(SetupActionKind.DIAGNOSE, plan.actions().getFirst().kind());
        assertEquals(SetupActionKind.INSTALL, plan.actions().getLast().kind());
        assertEquals("4.47.0-20260808,port=4444,chrome=1,edge=0,firefox=0",
                plan.actions().getLast().version());
        assertEquals(SetupSelection.defaults(), service.selectionFromPlan(plan));
    }

    @Test
    void selectedScaleIsBoundAndReconstructed(@TempDir Path temp) {
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.WINDOWS, SetupArchitecture.X64);
        SetupOptions options = managed(temp);
        SetupSelection selection = new SetupSelection(List.of("port_5555", "chrome_2", "firefox_1"));

        SetupPlan plan = service.plan(options, selection);

        assertEquals(selection, service.selectionFromPlan(plan));
        assertTrue(plan.actions().getLast().version().contains("port=5555"));
        assertThrows(IllegalArgumentException.class,
                () -> service.plan(options, new SetupSelection(List.of("chrome_0", "edge_0", "firefox_0"))));
    }

    @Test
    void missingDockerPerformsNoInstallWrites(@TempDir Path temp) {
        RecordingOperations operations = new RecordingOperations();
        operations.dockerReady = false;
        InfrastructureSetupService service = coordinator(operations);
        SetupOptions options = managed(temp);
        SetupPlan plan = service.plan(options);

        IOException failure = assertThrows(IOException.class,
                () -> service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), options));

        assertTrue(failure.getMessage().contains("Docker"));
        assertTrue(operations.events.contains("host-preflight"));
        assertFalse(operations.events.stream().anyMatch(event -> event.startsWith("install:")));
        assertFalse(Files.exists(options.paths().receipts().resolve("selenium-grid.json")));
    }

    @Test
    void managedProviderInstallsTheExactComposeReceipt(@TempDir Path temp) throws Exception {
        RecordingOperations operations = new RecordingOperations();
        InfrastructureSetupService service = coordinator(operations);
        SetupOptions options = managed(temp);
        SetupPlan plan = service.plan(options);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH, Set.of());

        assertEquals(SetupReadiness.MISSING, service.status(options).readiness());
        SetupReceipt receipt = service.install(plan, approval, options);

        assertEquals(plan.digest(), receipt.planDigest());
        assertEquals(plan.actions(), receipt.completedActions());
        assertEquals(SetupReadiness.READY, service.status(options).readiness());
        assertTrue(operations.events.contains("host-preflight"));
        assertTrue(operations.events.contains("install:" + SetupTarget.SELENIUM_GRID));
    }

    @Test
    void managedComposePinsMatchSelenium4AndEachOther() throws Exception {
        String compose = SeleniumGridSetupPlanner.compose(4444);
        assertFalse(compose.contains("container_name:"));
        assertEquals(Set.of("4.47.0-20260808"), imageTags(compose));
        Path engineCompose = Path.of("").toAbsolutePath().resolve(
                "../shaft-engine/src/main/resources/docker-compose/selenium4.yml");
        assertTrue(Files.isRegularFile(engineCompose), engineCompose.toString());
        assertEquals(Set.of("4.47.0-20260808"), imageTags(Files.readString(engineCompose)));
    }

    private static Set<String> imageTags(String compose) {
        Matcher matcher = Pattern.compile("selenium/(?:hub|node-chrome|node-edge|node-firefox):(\\S+)")
                .matcher(compose);
        java.util.LinkedHashSet<String> tags = new java.util.LinkedHashSet<>();
        while (matcher.find()) tags.add(matcher.group(1));
        return tags;
    }

    private static InfrastructureSetupService coordinator(RecordingOperations operations) {
        return new InfrastructureSetupService(new SetupProviderRegistry(
                List.of(new SeleniumGridSetupProvider((paths, plan, offline) -> operations))),
                SetupPlatform.LINUX, SetupArchitecture.X64);
    }

    private static SetupOptions managed(Path root) {
        Path cache = root.resolve("cache");
        Path data = root.resolve("data");
        return SetupOptions.defaults(SetupProfile.SELENIUM_GRID, new ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts")))
                .withMode(SetupMode.MANAGED);
    }

    private static final class RecordingOperations implements SeleniumGridToolchainOperations {
        private final List<String> events = new ArrayList<>();
        private boolean dockerReady = true;
        private boolean installed;

        @Override
        public void hostPreflight(List<SetupAction> actions) throws IOException {
            events.add("host-preflight");
            if (!dockerReady) throw new IOException("Docker Engine 26.1.4+ and Compose v2 are required.");
        }

        @Override
        public void lockedPreflight(List<SetupAction> actions, boolean offline) {
            events.add("locked-preflight");
        }

        @Override
        public void install(SetupAction action) {
            events.add("install:" + action.target());
            if (action.target() == SetupTarget.SELENIUM_GRID) installed = true;
        }

        @Override
        public SetupStatus status(SetupAction action) {
            events.add("status:" + action.target());
            if (action.target() == SetupTarget.DOCKER) {
                return new SetupStatus(action.target(),
                        dockerReady ? SetupReadiness.READY : SetupReadiness.MISSING,
                        dockerReady ? "26.1.4+" : "", dockerReady ? "fixture" : "Docker is missing.");
            }
            return new SetupStatus(action.target(),
                    installed ? SetupReadiness.READY : SetupReadiness.MISSING,
                    installed ? action.version() : "", installed ? "fixture" : "Compose is missing.");
        }
    }
}
