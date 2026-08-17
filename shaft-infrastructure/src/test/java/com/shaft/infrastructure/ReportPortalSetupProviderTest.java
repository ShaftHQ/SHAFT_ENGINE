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

class ReportPortalSetupProviderTest {
    @Test
    void builtInCoordinatorProvidesReportPortalPlan(@TempDir Path temp) {
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupOptions options = managed(temp);

        assertTrue(service.supports(SetupProfile.REPORT_PORTAL));
        SetupPlan plan = service.plan(options);

        assertEquals(List.of(SetupTarget.DOCKER, SetupTarget.REPORT_PORTAL),
                plan.actions().stream().map(SetupAction::target).toList());
        assertEquals(SetupActionKind.DIAGNOSE, plan.actions().getFirst().kind());
        assertEquals(SetupActionKind.INSTALL, plan.actions().getLast().kind());
        assertEquals("5.15.5+5.15.4+18.4,ui=8080", plan.actions().getLast().version());
        assertEquals(SetupSelection.defaults(), service.selectionFromPlan(plan));
    }

    @Test
    void selectedUiPortIsBoundAndReconstructed(@TempDir Path temp) {
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.WINDOWS, SetupArchitecture.X64);
        SetupOptions options = managed(temp);
        SetupSelection selection = new SetupSelection(List.of("ui_9080"));

        SetupPlan plan = service.plan(options, selection);

        assertEquals(selection, service.selectionFromPlan(plan));
        assertTrue(plan.actions().getLast().version().contains("ui=9080"));
        assertThrows(IllegalArgumentException.class,
                () -> service.plan(options, new SetupSelection(List.of("backend_8080"))));
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
        assertFalse(Files.exists(options.paths().receipts().resolve("reportportal.json")));
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
        assertTrue(operations.events.contains("install:" + SetupTarget.REPORT_PORTAL));
    }

    @Test
    void managedComposeUsesOfficialPinsWithoutHostDatabaseDashboardOrContainerNames() throws Exception {
        String compose = ReportPortalSetupPlanner.compose(new ReportPortalSetupPlanner.ReportPortalScale(8080));
        assertFalse(compose.contains("container_name:"));
        assertFalse(compose.contains("5432:5432"));
        assertFalse(compose.contains("8081:8081"));
        assertFalse(compose.contains("443:443"));
        assertEquals(Set.of("traefik:v2.11.54", "postgres:18.4", "rabbitmq:4.3.4-management",
                "reportportal/migrations:5.15.4", "reportportal/service-index:5.15.1",
                "reportportal/service-ui:5.15.5", "reportportal/service-api:5.15.4",
                "reportportal/service-authorization:5.15.1", "reportportal/service-jobs:5.15.2"),
                images(compose));
        assertTrue(everyHealthyDependencyHasAHealthcheck(compose));
        Path engineCompose = Path.of("").toAbsolutePath().resolve(
                "../shaft-engine/src/main/resources/docker-compose/report-portal.yml");
        assertTrue(Files.isRegularFile(engineCompose), engineCompose.toString());
        String documented = Files.readString(engineCompose);
        assertTrue(documented.contains("reportportal/service-ui:5.15.5"));
        assertTrue(documented.contains("reportportal/service-api:5.15.4"));
        assertTrue(documented.contains("reportportal/migrations:5.15.4"));
        assertTrue(documented.contains("traefik:v2.11.54"));
    }

    @Test
    void externalModeCannotInstall(@TempDir Path temp) {
        InfrastructureSetupService service = InfrastructureSetupService.builtIn(
                SetupPlatform.LINUX, SetupArchitecture.X64);
        Path cache = temp.resolve("cache");
        Path data = temp.resolve("data");
        SetupOptions options = SetupOptions.defaults(SetupProfile.REPORT_PORTAL, new ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts")));
        SetupPlan plan = service.plan(options);

        assertThrows(IllegalArgumentException.class,
                () -> service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), options));
    }

    private static boolean everyHealthyDependencyHasAHealthcheck(String compose) {
        Matcher depends = Pattern.compile("(?m)^ {6}([A-Za-z0-9_-]+):\\s*$\\n {8}condition: service_healthy")
                .matcher(compose);
        boolean found = false;
        while (depends.find()) {
            found = true;
            String service = depends.group(1);
            Matcher block = Pattern.compile("(?ms)^ {2}" + Pattern.quote(service) + ":\\n(.*?)(?=^ {2}\\S|\\Z)")
                    .matcher(compose);
            if (!block.find() || !block.group(1).contains("healthcheck:")) return false;
        }
        return found;
    }

    private static Set<String> images(String compose) {
        Matcher matcher = Pattern.compile("image:\\s*(\\S+)").matcher(compose);
        java.util.LinkedHashSet<String> images = new java.util.LinkedHashSet<>();
        while (matcher.find()) images.add(matcher.group(1));
        return images;
    }

    private static InfrastructureSetupService coordinator(RecordingOperations operations) {
        return new InfrastructureSetupService(new SetupProviderRegistry(
                List.of(new ReportPortalSetupProvider((paths, plan, offline) -> operations))),
                SetupPlatform.LINUX, SetupArchitecture.X64);
    }

    private static SetupOptions managed(Path root) {
        Path cache = root.resolve("cache");
        Path data = root.resolve("data");
        return SetupOptions.defaults(SetupProfile.REPORT_PORTAL, new ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts")))
                .withMode(SetupMode.MANAGED);
    }

    private static final class RecordingOperations implements ReportPortalToolchainOperations {
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
            if (action.target() == SetupTarget.REPORT_PORTAL) installed = true;
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
