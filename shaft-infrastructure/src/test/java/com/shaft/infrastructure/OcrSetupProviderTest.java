package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.HexFormat;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OcrSetupProviderTest {
    @Test
    void managedPlanInstallsAndVerifiesPinnedModels(@TempDir Path temp) throws Exception {
        byte[] model = "verified-ocr-model".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        Path source = temp.resolve("eng.traineddata");
        Files.write(source, model);
        SetupAction action = action(source.toUri(), sha256(model));
        OcrSetupProvider provider = new OcrSetupProvider(List.of(action));
        SetupOptions options = SetupOptions.defaults(SetupProfile.OCR, paths(temp)).withMode(SetupMode.MANAGED);
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), SetupPlatform.LINUX, SetupArchitecture.X64);

        SetupPlan plan = service.plan(options);
        assertEquals(List.of(action), plan.actions());
        assertEquals(SetupReadiness.MISSING, service.status(options).readiness());

        SetupReceipt receipt = service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of(OcrSetupManifest.LICENSE_ID)), options);

        assertEquals(plan.actions(), receipt.completedActions());
        Path installed = OcrSetupManifest.modelsDirectory(options.paths()).resolve("eng.traineddata");
        assertEquals("verified-ocr-model", Files.readString(installed));
        assertTrue(Files.isRegularFile(options.paths().receipts().resolve("ocr.json")));
        assertEquals(SetupReadiness.READY, service.verify(options).readiness());

        Files.writeString(installed, "corrupt");
        assertEquals(SetupReadiness.DEGRADED, service.status(options).readiness());
        assertTrue(service.status(options).diagnostics().getFirst().contains("SHA-256"));
    }

    @Test
    void missingLicenseAndOfflineCacheFailBeforeToolPublication(@TempDir Path temp) throws Exception {
        byte[] model = "verified-ocr-model".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        Path source = temp.resolve("eng.traineddata");
        Files.write(source, model);
        OcrSetupProvider provider = new OcrSetupProvider(List.of(action(source.toUri(), sha256(model))));
        SetupOptions options = SetupOptions.defaults(SetupProfile.OCR, paths(temp)).withMode(SetupMode.MANAGED);
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupPlan plan = service.plan(options);

        assertThrows(IllegalArgumentException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), options));
        assertNoSetupMutation(options);

        SetupOptions offline = options.withOffline(true);
        SetupPlan offlinePlan = service.plan(offline);
        assertThrows(java.io.IOException.class, () -> service.install(offlinePlan,
                new SetupApproval(offlinePlan.digest(), Instant.EPOCH, Set.of(OcrSetupManifest.LICENSE_ID)), offline));
        assertNoSetupMutation(options);
    }

    @Test
    void releaseManifestPinsEnglishAndOrientationWithSha256() {
        List<SetupAction> actions = OcrSetupManifest.actions(SetupMode.MANAGED);

        assertEquals(List.of("eng", "ara"), OcrSetupManifest.baselineLanguages());
        assertEquals(2, actions.size());
        assertTrue(actions.stream().allMatch(action -> action.checksum().matches("sha256:[0-9a-f]{64}")));
        assertTrue(actions.stream().allMatch(action -> action.requiredLicenses().isEmpty()));
        assertTrue(actions.stream().allMatch(action -> action.source().toString()
                .contains(OcrSetupManifest.TESSDATA_REVISION)));
        List<SetupAction> selected = OcrSetupManifest.actions(SetupMode.MANAGED, List.of("deu", "fra"));
        assertEquals(List.of("deu", "fra"), selected.stream()
                .map(action -> action.version().substring(action.version().indexOf(':') + 1)).toList());
    }

    @Test
    void statusIsReadOnlyAndExternalPlanCannotMutate(@TempDir Path temp) {
        SetupOptions options = SetupOptions.defaults(SetupProfile.OCR, paths(temp));
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(new OcrSetupProvider())),
                SetupPlatform.LINUX, SetupArchitecture.X64);

        assertEquals(SetupReadiness.MISSING, service.status(options).readiness());
        assertFalse(Files.exists(options.paths().cacheRoot()));
        SetupPlan plan = service.plan(options);
        assertTrue(plan.actions().stream().allMatch(action -> action.kind() == SetupActionKind.DIAGNOSE));
        assertThrows(IllegalArgumentException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of()), options));
        assertFalse(Files.exists(options.paths().cacheRoot()));
        assertFalse(Files.exists(options.paths().dataRoot()));
    }

    private static void assertNoSetupMutation(SetupOptions options) {
        assertFalse(Files.exists(options.paths().cacheRoot()));
        assertFalse(Files.exists(options.paths().dataRoot()));
        assertFalse(Files.exists(options.paths().downloads()));
        assertFalse(Files.exists(options.paths().state()));
        assertFalse(Files.exists(options.paths().receipts()));
        assertFalse(Files.exists(OcrSetupManifest.modelsDirectory(options.paths())));
    }

    @Test
    void approvedOfflineInstallAdoptsOnlyVerifiedLegacyModel(@TempDir Path temp) throws Exception {
        byte[] model = "verified-legacy-model".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        Path source = temp.resolve("unused.traineddata");
        SetupAction action = action(source.toUri(), sha256(model));
        Path legacy = temp.resolve("legacy");
        Files.createDirectories(legacy);
        Files.write(legacy.resolve("unused.traineddata"), model);
        OcrSetupProvider provider = new OcrSetupProvider(List.of(action), legacy);
        SetupOptions options = SetupOptions.defaults(SetupProfile.OCR, paths(temp))
                .withMode(SetupMode.MANAGED).withOffline(true);
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupPlan plan = service.plan(options);

        service.install(plan, new SetupApproval(plan.digest(), Instant.EPOCH,
                Set.of(OcrSetupManifest.LICENSE_ID)), options);

        assertEquals("verified-legacy-model", Files.readString(
                OcrSetupManifest.modelsDirectory(options.paths()).resolve("unused.traineddata")));
        assertFalse(Files.exists(options.paths().downloads()));
    }

    @Test
    void laterModelFailurePreservesPartialReceiptAndNoCompleteReceipt(@TempDir Path temp) throws Exception {
        byte[] model = "first-model".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        Path first = temp.resolve("eng.traineddata");
        Files.write(first, model);
        SetupAction firstAction = action(first.toUri(), sha256(model));
        SetupAction secondAction = new SetupAction(SetupTarget.OCR_TESSDATA, SetupActionKind.INSTALL,
                OcrSetupManifest.TESSDATA_REVISION + ":ara", temp.resolve("missing.traineddata").toUri(),
                "sha256:" + "0".repeat(64), false, Set.of(OcrSetupManifest.LICENSE_ID));
        OcrSetupProvider provider = new OcrSetupProvider(List.of(firstAction, secondAction));
        SetupOptions options = SetupOptions.defaults(SetupProfile.OCR, paths(temp)).withMode(SetupMode.MANAGED);
        InfrastructureSetupService service = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(provider)), SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupPlan plan = service.plan(options);

        SetupExecutionException failure = assertThrows(SetupExecutionException.class, () -> service.install(plan,
                new SetupApproval(plan.digest(), Instant.EPOCH, Set.of(OcrSetupManifest.LICENSE_ID)), options));

        assertEquals(secondAction, failure.failedAction());
        assertEquals(List.of(firstAction), failure.partialReceipt().completedActions());
        assertTrue(Files.isRegularFile(OcrSetupManifest.modelsDirectory(options.paths())
                .resolve("eng.traineddata")));
        assertFalse(Files.exists(options.paths().receipts().resolve("ocr.json")));
    }

    @Test
    void concurrentProviderInstancesConvergeOnOneVerifiedInstallation(@TempDir Path temp) throws Exception {
        byte[] model = "concurrent-model".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        Path source = temp.resolve("eng.traineddata");
        Files.write(source, model);
        SetupAction action = action(source.toUri(), sha256(model));
        SetupOptions options = SetupOptions.defaults(SetupProfile.OCR, paths(temp)).withMode(SetupMode.MANAGED);
        InfrastructureSetupService first = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(new OcrSetupProvider(List.of(action)))),
                SetupPlatform.LINUX, SetupArchitecture.X64);
        InfrastructureSetupService second = new InfrastructureSetupService(
                new SetupProviderRegistry(List.of(new OcrSetupProvider(List.of(action)))),
                SetupPlatform.LINUX, SetupArchitecture.X64);
        SetupPlan plan = first.plan(options);
        SetupApproval approval = new SetupApproval(plan.digest(), Instant.EPOCH,
                Set.of(OcrSetupManifest.LICENSE_ID));

        try (var executor = Executors.newFixedThreadPool(2)) {
            var firstInstall = executor.submit(() -> first.install(plan, approval, options));
            var secondInstall = executor.submit(() -> second.install(plan, approval, options));
            assertEquals(plan.actions(), firstInstall.get().completedActions());
            assertEquals(plan.actions(), secondInstall.get().completedActions());
        }
        assertEquals("concurrent-model", Files.readString(
                OcrSetupManifest.modelsDirectory(options.paths()).resolve("eng.traineddata")));
        assertEquals(SetupReadiness.READY, first.verify(options).readiness());
    }

    private static SetupAction action(URI source, String checksum) {
        return new SetupAction(SetupTarget.OCR_TESSDATA, SetupActionKind.INSTALL,
                OcrSetupManifest.TESSDATA_REVISION + ":eng", source, checksum, false,
                Set.of(OcrSetupManifest.LICENSE_ID));
    }

    private static String sha256(byte[] bytes) throws Exception {
        return "sha256:" + HexFormat.of().formatHex(
                java.security.MessageDigest.getInstance("SHA-256").digest(bytes));
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }
}
