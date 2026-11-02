package com.shaft.ai.local;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedLocalAiStatusTest {
    @TempDir
    Path temp;

    @Test
    void inspectionIsTypedActionableAndReadOnly() {
        Path cache = temp.resolve("absent-cache");
        assertEquals(ManagedLocalAiStatus.State.DISABLED,
                ManagedLocalAiStatus.inspect(cache, false, "Windows 11", "amd64", "windows-msvc", "", null).state());
        assertEquals(ManagedLocalAiStatus.State.UNSUPPORTED,
                ManagedLocalAiStatus.inspect(cache, true, "Plan 9", "mips", "unknown", "", null).state());
        ManagedLocalAiStatus missing = ManagedLocalAiStatus.inspect(cache, true, "Windows 11", "amd64",
                "windows-msvc", "", "runtime-b10400-windows-x86_64");
        assertEquals(ManagedLocalAiStatus.State.NOT_PROVISIONED, missing.state());
        assertReviewedInventory(missing.action(), "windows-x86_64");
        assertFalse(Files.exists(cache));
    }

    @Test
    void disabledInspectionStillListsReviewedInventoryWithoutCreatingCache() {
        Path cache = temp.resolve("disabled-cache");
        ManagedLocalAiStatus disabled = ManagedLocalAiStatus.inspect(cache, false, "Windows 11", "amd64",
                "windows-msvc", "", null);
        assertEquals(ManagedLocalAiStatus.State.DISABLED, disabled.state());
        assertReviewedInventory(disabled.action(), "windows-x86_64");
        assertFalse(Files.exists(cache));
    }

    @Test
    void anotherOwnedInstallationDoesNotMakeMissingSelectionCorrupt() throws Exception {
        Path cache = temp.resolve("mixed-cache");
        Path stage = cache.resolve("staging/runtime.extract-test");
        Files.createDirectories(stage);
        Files.writeString(stage.resolve("server"), "binary");
        Files.writeString(stage.resolve(".shaft-ready"), "");
        ManagedLocalAiCache.withLock(cache, java.time.Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "installed", stage));

        ManagedLocalAiStatus status = ManagedLocalAiStatus.inspect(cache, true, "Windows 11", "amd64",
                "windows-msvc", "", "missing");
        assertEquals(ManagedLocalAiStatus.State.NOT_PROVISIONED, status.state());
        assertReviewedInventory(status.action(), "windows-x86_64");
    }

    @Test
    void revokedOwnedBytesAreNotReportedReady() throws Exception {
        Path cache = temp.resolve("revoked-cache");
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        ManagedLocalAiManifest.RuntimeAsset runtime = manifest.runtime().assets().stream()
                .filter(asset -> asset.platform().equals("windows-x86_64")).findFirst().orElseThrow();
        String pinId = ManagedLocalAiService.runtimeInstallationId(manifest, "windows-x86_64");
        Path stage = cache.resolve("staging/revoked.extract-test");
        Files.createDirectories(stage);
        Files.writeString(stage.resolve(runtime.file()), "revoked-bytes");
        Files.writeString(stage.resolve(".shaft-ready"), "");
        ManagedLocalAiCache.withLock(cache, java.time.Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, pinId, stage));

        ManagedLocalAiStatus status = ManagedLocalAiStatus.inspect(cache, true, "Windows 11", "amd64",
                "windows-msvc", "", pinId);

        assertEquals(ManagedLocalAiStatus.State.CORRUPT, status.state());
        assertTrue(status.action().contains("Rebuild"));
        assertReviewedInventory(status.action(), "windows-x86_64");
        assertTrue(ManagedLocalAiCache.ownsInstallation(cache, pinId));
    }

    static void assertReviewedInventory(String listed, String platform) {
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        ManagedLocalAiManifest.RuntimeAsset runtime = manifest.runtime().assets().stream()
                .filter(asset -> asset.platform().equals(platform)).findFirst().orElseThrow();
        ManagedLocalAiManifest.ModelManifest model = manifest.models().stream()
                .filter(candidate -> candidate.id().equals("qwen3-0.6b-q8_0")).findFirst().orElseThrow();
        assertTrue(listed.contains(manifest.runtime().version()), listed);
        assertTrue(listed.contains(model.revision()), listed);
        assertTrue(listed.contains(manifest.runtime().license()), listed);
        assertTrue(listed.contains(model.license()), listed);
        assertTrue(listed.contains("github.com/ggml-org/llama.cpp"), listed);
        assertTrue(listed.contains("huggingface.co/" + model.source()), listed);
        assertTrue(listed.contains(Long.toString(runtime.size())), listed);
        assertTrue(listed.contains(Long.toString(model.size())), listed);
        assertTrue(listed.contains("SHAFT_USER_CACHE"), listed);
        assertTrue(listed.contains(trimNumber(model.minimumRamGb())), listed);
        assertTrue(listed.contains(Integer.toString(model.minimumCpuCount())), listed);
        assertTrue(listed.contains(trimNumber(model.minimumFreeDiskGb())), listed);
        assertTrue(listed.contains("explicit reviewed plan"), listed);
        assertTrue(listed.contains("pin-bound"), listed);
        assertTrue(listed.contains("no silent float"), listed);
        assertTrue(listed.contains("owner-manifest only"), listed);
        assertTrue(listed.contains("unknown siblings preserved"), listed);
        assertTrue(listed.contains("deterministic SHAFT result remains authoritative"), listed);
        assertFalse(listed.contains(platform + "/installations"), listed);
    }

    private static String trimNumber(double value) {
        return value == Math.rint(value) ? Long.toString(Math.round(value)) : Double.toString(value);
    }
}
