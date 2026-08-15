package com.shaft.mcp;

import com.shaft.infrastructure.InfrastructureSetupService;
import com.shaft.infrastructure.ManagedEnvironment;
import com.shaft.infrastructure.SetupAction;
import com.shaft.infrastructure.SetupActionKind;
import com.shaft.infrastructure.SetupApproval;
import com.shaft.infrastructure.SetupArchitecture;
import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.infrastructure.SetupOperation;
import com.shaft.infrastructure.SetupPlan;
import com.shaft.infrastructure.SetupPlatform;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.SetupReceipt;
import com.shaft.infrastructure.SetupSelection;
import com.shaft.infrastructure.SetupTarget;
import com.shaft.infrastructure.ShaftCachePaths;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

import java.net.URI;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class InfrastructureMcpServiceTest {
    @Test
    void setupRequestExposesAnExplicitMaintenanceOperation() {
        assertTrue(java.util.Arrays.stream(McpSetupRequest.class.getRecordComponents())
                .anyMatch(component -> component.getName().equals("operation")));
    }

    private static final String CHECKSUM = "sha256:" + "a".repeat(64);

    @TempDir
    Path temp;

    @Test
    void planReturnsTheExactCoordinatorPlanAndPolicy() {
        InfrastructureSetupService coordinator = mock(InfrastructureSetupService.class);
        SetupPlan plan = plan(SetupProfile.OCR);
        when(coordinator.plan(any(SetupOptions.class), any(SetupSelection.class))).thenReturn(plan);
        InfrastructureMcpService service = new InfrastructureMcpService(coordinator);

        McpSetupPlanResult result = service.setupPlan(request("OCR", "MANAGED", List.of("fra", "deu")));

        assertEquals(plan, result.plan());
        assertEquals(plan.digest(), result.digest());
        assertEquals(plan, com.shaft.infrastructure.SetupPlanJson.read(result.planJson()));
        ArgumentCaptor<SetupOptions> options = ArgumentCaptor.forClass(SetupOptions.class);
        ArgumentCaptor<SetupSelection> selection = ArgumentCaptor.forClass(SetupSelection.class);
        verify(coordinator).plan(options.capture(), selection.capture());
        assertEquals(SetupMode.MANAGED, options.getValue().mode());
        assertEquals(List.of("deu", "fra"), selection.getValue().components());
    }

    @Test
    void installRecoversOcrSelectionFromTheReviewedPlan() throws Exception {
        InfrastructureSetupService coordinator = mock(InfrastructureSetupService.class);
        SetupPlan plan = plan(SetupProfile.OCR);
        SetupReceipt receipt = new SetupReceipt(plan.digest(), Instant.EPOCH, plan.actions());
        when(coordinator.install(any(SetupPlan.class), any(SetupApproval.class), any(SetupOptions.class),
                any(SetupSelection.class))).thenReturn(receipt);
        InfrastructureMcpService service = new InfrastructureMcpService(coordinator);

        SetupReceipt result = service.setupInstall(
                com.shaft.infrastructure.SetupPlanJson.write(plan), plan.digest(), List.of(),
                request("OCR", "MANAGED", List.of()));

        assertEquals(receipt, result);
        ArgumentCaptor<SetupApproval> approval = ArgumentCaptor.forClass(SetupApproval.class);
        ArgumentCaptor<SetupSelection> selection = ArgumentCaptor.forClass(SetupSelection.class);
        verify(coordinator).install(any(), approval.capture(), any(), selection.capture());
        assertEquals(plan.digest(), approval.getValue().planDigest());
        assertEquals(List.of("fra"), selection.getValue().components());
    }

    @Test
    void componentSelectionForAnUnrelatedProfileFailsBeforeCoordinatorCallback() {
        InfrastructureSetupService coordinator = mock(InfrastructureSetupService.class);
        InfrastructureMcpService service = new InfrastructureMcpService(coordinator);

        assertThrows(IllegalArgumentException.class,
                () -> service.setupPlan(request("REPORTING", "MANAGED", List.of("fra"))));

        verify(coordinator, never()).plan(any(SetupOptions.class), any(SetupSelection.class));
    }

    @Test
    void lifecycleToolsDelegateExactPlansAndOwnedLogsToTheCoordinator() throws Exception {
        InfrastructureSetupService coordinator = mock(InfrastructureSetupService.class);
        SetupPlan plan = plan(SetupProfile.REPORTING);
        SetupReceipt receipt = new SetupReceipt(plan.digest(), Instant.EPOCH, plan.actions());
        ManagedEnvironment environment = new ManagedEnvironment(plan.profile(), receipt,
                Optional.of(URI.create("http://127.0.0.1:4723/")), Map.of("appium", "ready"), () -> { });
        when(coordinator.start(any(), any(), any(), any(SetupSelection.class))).thenReturn(environment);
        when(coordinator.stop(any(), any(), any(), any(SetupSelection.class))).thenReturn(true);
        when(coordinator.logs(any(), any(SetupSelection.class))).thenReturn("owned logs");
        InfrastructureMcpService service = new InfrastructureMcpService(coordinator);
        McpSetupRequest request = request("REPORTING", "MANAGED", List.of());
        String json = com.shaft.infrastructure.SetupPlanJson.write(plan);

        McpSetupLifecycleResult started = service.setupStart(json, plan.digest(), List.of(), request);
        McpSetupLifecycleResult stopped = service.setupStop(json, plan.digest(), List.of(), request);
        McpSetupLifecycleResult logs = service.setupLogs(request);

        assertTrue(started.supported());
        assertEquals("http://127.0.0.1:4723/", started.endpoint());
        assertEquals(plan.digest(), started.planDigest());
        assertTrue(stopped.supported());
        assertEquals("owned logs", logs.logs());
        verify(coordinator).start(any(), any(), any(), any(SetupSelection.class));
        verify(coordinator).stop(any(), any(), any(), any(SetupSelection.class));
        verify(coordinator).logs(any(), any(SetupSelection.class));
    }

    @Test
    @SuppressWarnings("deprecation")
    void legacyProfileOnlyLifecycleOverloadsRemainExplicitlyUnsupported() {
        InfrastructureMcpService service = new InfrastructureMcpService(mock(InfrastructureSetupService.class));

        assertFalse(service.setupStart("REPORTING").supported());
        assertFalse(service.setupStop("REPORTING").supported());
        assertFalse(service.setupLogs("REPORTING").supported());
    }

    @Test
    void externalReadOnlyFlowAndRejectedInstallCreateNoRoots() {
        InfrastructureMcpService service = new InfrastructureMcpService();
        McpSetupRequest request = request("REPORTING", "EXTERNAL", List.of());

        McpSetupPlanResult result = service.setupPlan(request);
        service.setupStatus(request);
        service.setupDoctor(request);
        service.setupVerify(request);
        assertThrows(IllegalArgumentException.class, () -> service.setupInstall(
                result.planJson(), result.digest(), List.of(), request));

        assertFalse(java.nio.file.Files.exists(temp.resolve("cache")));
        assertFalse(java.nio.file.Files.exists(temp.resolve("data")));
    }

    @Test
    void malformedPlanAndSelectionDriftFailBeforeInstallCallback() throws Exception {
        InfrastructureSetupService coordinator = mock(InfrastructureSetupService.class);
        InfrastructureMcpService service = new InfrastructureMcpService(coordinator);
        SetupPlan plan = plan(SetupProfile.OCR);
        String unknownField = com.shaft.infrastructure.SetupPlanJson.write(plan)
                .replaceFirst("\\{", "{\\\"unknown\\\":true,");

        IllegalArgumentException invalidPlan = assertThrows(IllegalArgumentException.class, () -> service.setupInstall(
                unknownField, plan.digest(), List.of(), request("OCR", "MANAGED", List.of())));
        assertTrue(invalidPlan.getMessage().contains("strict setup plan"));
        assertThrows(IllegalArgumentException.class, () -> service.setupInstall(
                com.shaft.infrastructure.SetupPlanJson.write(plan), plan.digest(), List.of(),
                request("OCR", "MANAGED", List.of("deu"))));

        verify(coordinator, never()).install(any(SetupPlan.class), any(SetupApproval.class), any(SetupOptions.class),
                any(SetupSelection.class));
    }

    @Test
    void invalidRootPairFailsBeforePlanningCallback() {
        InfrastructureSetupService coordinator = mock(InfrastructureSetupService.class);
        InfrastructureMcpService service = new InfrastructureMcpService(coordinator);
        McpSetupRequest request = new McpSetupRequest("REPORTING", "MANAGED",
                temp.resolve("cache").toString(), "", false, false, true, true,
                "PT2M", "PT30S", "", List.of());

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class,
                () -> service.setupPlan(request));

        assertTrue(failure.getMessage().contains("supplied together"));
        verify(coordinator, never()).plan(any(SetupOptions.class), any(SetupSelection.class));
    }

    @Test
    void explicitRemoteEndpointForcesExternalModeAndDefaultsRemainStable() {
        McpSetupRequest request = new McpSetupRequest("REPORTING", "MANAGED",
                temp.resolve("cache").toString(), temp.resolve("data").toString(),
                null, null, null, null, null, null, "https://grid.example.test/wd/hub", null);

        SetupOptions options = request.options();

        assertEquals(SetupMode.MANAGED, options.mode());
        assertEquals(SetupMode.EXTERNAL, options.effectiveMode());
        assertFalse(options.offline());
        assertFalse(options.autoStart());
        assertTrue(options.preferSystemTools());
        assertTrue(options.reuseOwnedProcesses());
    }

    @Test
    void maintenancePlanRoutesTheExplicitOperationAndInstallRejectsAMismatch() throws Exception {
        InfrastructureSetupService coordinator = mock(InfrastructureSetupService.class);
        SetupPlan clean = SetupPlan.create(SetupProfile.LOCAL_AI, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, SetupMode.MANAGED, List.of(
                        new SetupAction(SetupTarget.MANAGED_LOCAL_AI_RUNTIME, SetupActionKind.CLEAN,
                                "reviewed", URI.create("https://example.invalid/runtime"), CHECKSUM,
                                false, Set.of())));
        when(coordinator.plan(any(SetupOptions.class), any(SetupSelection.class), eq(SetupOperation.CLEAN)))
                .thenReturn(clean);
        InfrastructureMcpService service = new InfrastructureMcpService(coordinator);
        McpSetupRequest request = new McpSetupRequest("LOCAL_AI", "MANAGED", temp.resolve("cache").toString(),
                temp.resolve("data").toString(), false, false, true, true, "PT2M", "PT30S", "",
                "CLEAN", List.of());

        assertEquals(clean, service.setupPlan(request).plan());
        verify(coordinator).plan(any(SetupOptions.class), any(SetupSelection.class), eq(SetupOperation.CLEAN));
        McpSetupRequest installRequest = new McpSetupRequest("LOCAL_AI", "MANAGED",
                temp.resolve("cache").toString(), temp.resolve("data").toString(), false, false, true, true,
                "PT2M", "PT30S", "", "INSTALL", List.of());
        assertThrows(IllegalArgumentException.class, () -> service.setupInstall(
                com.shaft.infrastructure.SetupPlanJson.write(clean), clean.digest(), List.of(), installRequest));
        verify(coordinator, never()).install(any(), any(), any(), any(SetupSelection.class));
    }

    @Test
    void localAiRollbackRequestRoutesTheExplicitOperation() {
        InfrastructureSetupService coordinator = mock(InfrastructureSetupService.class);
        SetupPlan rollback = SetupPlan.create(SetupProfile.LOCAL_AI, SetupPlatform.WINDOWS,
                SetupArchitecture.X64, SetupMode.MANAGED, List.of(
                        new SetupAction(SetupTarget.MANAGED_LOCAL_AI_RUNTIME, SetupActionKind.ROLLBACK,
                                "reviewed", URI.create("https://example.invalid/runtime"), CHECKSUM,
                                false, Set.of("MIT"))));
        when(coordinator.plan(any(SetupOptions.class), any(SetupSelection.class), eq(SetupOperation.ROLLBACK)))
                .thenReturn(rollback);
        InfrastructureMcpService service = new InfrastructureMcpService(coordinator);
        McpSetupRequest request = new McpSetupRequest("LOCAL_AI", "MANAGED", temp.resolve("cache").toString(),
                temp.resolve("data").toString(), false, false, true, true, "PT2M", "PT30S", "",
                "ROLLBACK", List.of());

        assertEquals(rollback, service.setupPlan(request).plan());
        verify(coordinator).plan(any(SetupOptions.class), any(SetupSelection.class), eq(SetupOperation.ROLLBACK));
    }

    @Test
    void omittedLocalAiRootsUseInferenceCacheButExplicitDefaultRootsRemainExact() {
        Path inferenceCache = temp.resolve("managed-inference-cache").toAbsolutePath();
        com.shaft.driver.SHAFT.Properties.managedLocalAi.set().cacheDirectory(inferenceCache.toString());
        try {
            McpSetupRequest omitted = new McpSetupRequest("LOCAL_AI", "MANAGED", "", "",
                    false, false, true, true, "PT2M", "PT30S", "", List.of());
            ShaftCachePaths defaults = ShaftCachePaths.current();
            McpSetupRequest explicit = new McpSetupRequest("LOCAL_AI", "MANAGED",
                    defaults.cacheRoot().toString(), defaults.dataRoot().toString(),
                    false, false, true, true, "PT2M", "PT30S", "", List.of());

            assertEquals(inferenceCache, omitted.options().paths().cacheRoot());
            assertEquals(defaults.cacheRoot(), explicit.options().paths().cacheRoot());
        } finally {
            com.shaft.properties.internal.Properties.clearForCurrentThread();
        }
    }

    @Test
    void defaultServiceLoaderRunsLocalAiStatusPlanAndCleanInstall() throws Exception {
        Path cache = temp.resolve("mcp-managed-ai-cache").toAbsolutePath();
        Path data = temp.resolve("mcp-managed-ai-data").toAbsolutePath();
        com.shaft.driver.SHAFT.Properties.managedLocalAi.set().enabled(true)
                .model("qwen3-0.6b-q8_0").cacheDirectory(cache.toString());
        try {
            InfrastructureMcpService service = new InfrastructureMcpService();
            McpSetupRequest request = new McpSetupRequest("LOCAL_AI", "MANAGED",
                    cache.toString(), data.toString(), true, false, true, true,
                    "PT2M", "PT30S", "", "CLEAN", List.of());

            service.setupStatus(request);
            McpSetupPlanResult planned = service.setupPlan(request);
            assertTrue(planned.plan().actions().stream()
                    .allMatch(action -> action.kind() == SetupActionKind.CLEAN));
            assertTrue(planned.plan().actions().stream()
                    .anyMatch(action -> action.target() == SetupTarget.MANAGED_LOCAL_AI_RUNTIME));
            assertTrue(planned.plan().actions().stream()
                    .anyMatch(action -> action.target() == SetupTarget.MANAGED_LOCAL_AI_MODEL));
            assertFalse(java.nio.file.Files.exists(cache));
            assertFalse(java.nio.file.Files.exists(data));
            List<String> licenses = planned.plan().actions().stream()
                    .flatMap(action -> action.requiredLicenses().stream()).distinct().toList();

            SetupReceipt receipt = service.setupInstall(
                    planned.planJson(), planned.digest(), licenses, request);

            assertEquals(planned.digest(), receipt.planDigest());
            assertEquals(planned.plan().actions(), receipt.completedActions());
        } finally {
            com.shaft.properties.internal.Properties.clearForCurrentThread();
        }
    }

    @Test
    void malformedTimeoutNamesTheRejectedField() {
        McpSetupRequest request = new McpSetupRequest("REPORTING", "MANAGED",
                temp.resolve("cache").toString(), temp.resolve("data").toString(),
                false, false, true, true, "soon", "PT30S", "", List.of());

        IllegalArgumentException failure = assertThrows(IllegalArgumentException.class, request::options);

        assertTrue(failure.getMessage().contains("startupTimeout"));
    }

    private McpSetupRequest request(String profile, String mode, List<String> components) {
        return new McpSetupRequest(profile, mode,
                temp.resolve("cache").toString(), temp.resolve("data").toString(),
                false, false, true, true, "PT2M", "PT30S", "", components);
    }

    private static SetupPlan plan(SetupProfile profile) {
        SetupAction action = new SetupAction(SetupTarget.OCR_TESSDATA, SetupActionKind.INSTALL,
                "tessdata:fra", URI.create("https://example.invalid/fra.traineddata"), CHECKSUM, "", false, Set.of());
        return SetupPlan.bind(SetupPlan.create(profile, SetupPlatform.WINDOWS, SetupArchitecture.X64,
                SetupMode.MANAGED, List.of(action)), CHECKSUM);
    }
}
