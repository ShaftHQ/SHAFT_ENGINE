package com.shaft.ai.local;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.HexFormat;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ManagedLocalAiServiceTest {
    private static final long GIB = 1024L * 1024 * 1024;

    @TempDir
    Path temp;

    @Test
    void inspectionIsReadOnlyAndSurfacesDisabledUnsupportedExcludedAndMissingInventory() throws Exception {
        Path absentCache = temp.resolve("absent/cache");
        ManagedLocalAiService disabledService = service(absentCache, false, "auto", new ThrowingHost());
        ManagedLocalAiSnapshot disabled = disabledService.inspect();
        assertEquals(ManagedLocalAiSnapshot.State.DISABLED, disabled.state());
        assertFalse(Files.exists(temp.resolve("absent")));

        ManagedLocalAiSnapshot unsupported = service(absentCache, true, "auto",
                host("Plan 9", "mips", "unsupported", "", 32 * GIB, 16, 64 * GIB)).inspect();
        assertEquals(ManagedLocalAiSnapshot.State.UNSUPPORTED, unsupported.state());
        assertNull(unsupported.selectedModelId());

        ManagedLocalAiSnapshot excluded = service(absentCache, true, "auto",
                host("Windows 11", "amd64", "windows-msvc", "", 3 * GIB, 2, 64 * GIB)).inspect();
        assertEquals(ManagedLocalAiSnapshot.State.EXCLUDED, excluded.state());
        assertNull(excluded.selectedModelId());
        assertEquals("INSUFFICIENT_MEMORY", excluded.models().get("qwen3-1.7b-q8_0").reasons().getFirst());

        ManagedLocalAiSnapshot missing = service(absentCache, true, "qwen3-1.7b-q8_0",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB)).inspect();
        assertEquals(ManagedLocalAiSnapshot.State.NOT_PROVISIONED, missing.state());
        assertEquals("qwen3-1.7b-q8_0", missing.selectedModelId());
        assertEquals("qwen3-1.7b-q8_0", missing.requestedModelId());
        assertEquals("llama.cpp", missing.runtimeId());
        assertEquals("windows-x86_64", missing.platform());
        assertEquals(ManagedLocalAiSnapshot.CacheHealth.MISSING, missing.runtimeCacheHealth());
        assertEquals(ManagedLocalAiSnapshot.CacheHealth.MISSING, missing.modelCacheHealth());
        assertEquals(ManagedLocalAiSnapshot.Phase.IDLE, missing.phase());
        assertEquals(absentCache.toAbsolutePath().normalize(), missing.cacheDirectory());
        assertFalse(Files.exists(temp.resolve("absent")), "inspection must not create the cache or an ancestor");
    }

    @Test
    void inspectionReportsReadyThenCorruptWithoutChangingTheOwnedCache() throws Exception {
        Path cache = temp.resolve("cache");
        byte[] runtimeArchive = "reviewed-runtime-archive".getBytes();
        byte[] runtimeExecutable = "reviewed-runtime-executable".getBytes();
        byte[] modelPayload = "reviewed-model".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtimeArchive, modelPayload);
        String platform = "windows-x86_64";
        ManagedLocalAiManifest.ModelManifest modelManifest = manifest.models().getFirst();
        String modelId = modelManifest.id();
        ManagedLocalAiCache.Installation runtime = adopt(cache,
                ManagedLocalAiService.runtimeInstallationId(manifest, platform),
                new Payload("runtime.zip", runtimeArchive), new Payload("llama-server.exe", runtimeExecutable));
        ManagedLocalAiService service = service(cache, true, modelId,
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB), manifest);

        ManagedLocalAiSnapshot runtimeOnly = service.inspect();
        assertEquals(ManagedLocalAiSnapshot.State.NOT_PROVISIONED, runtimeOnly.state());
        assertEquals(ManagedLocalAiSnapshot.CacheHealth.READY, runtimeOnly.runtimeCacheHealth());
        assertEquals(ManagedLocalAiSnapshot.CacheHealth.MISSING, runtimeOnly.modelCacheHealth());

        ManagedLocalAiCache.Installation model = adopt(cache,
                ManagedLocalAiService.modelInstallationId(modelManifest), new Payload("model.gguf", modelPayload));

        List<String> before = tree(cache);
        ManagedLocalAiSnapshot ready = service.inspect();
        assertEquals(ManagedLocalAiSnapshot.State.READY, ready.state());
        assertEquals(ManagedLocalAiSnapshot.CacheHealth.READY, ready.runtimeCacheHealth());
        assertEquals(ManagedLocalAiSnapshot.CacheHealth.READY, ready.modelCacheHealth());
        assertEquals(modelManifest.revision(), ready.models().get(modelId).revision());
        assertEquals(modelManifest.file(), ready.models().get(modelId).file());
        assertEquals(before, tree(cache), "ready inspection must be read-only");
        assertThrows(UnsupportedOperationException.class, () -> ready.models().clear());
        assertThrows(UnsupportedOperationException.class,
                () -> ready.models().get(modelId).reasons().add("MUTATED"));

        Path modelFile = model.files().stream()
                .map(file -> cache.resolve(file.path()))
                .filter(path -> path.getFileName().toString().equals("model.gguf"))
                .findFirst().orElseThrow();
        Files.writeString(modelFile, "changed");
        List<String> changed = tree(cache);
        ManagedLocalAiSnapshot corrupt = service.inspect();
        assertEquals(ManagedLocalAiSnapshot.State.CORRUPT, corrupt.state());
        assertEquals(changed, tree(cache), "corrupt inspection must not repair or delete owned paths");
        assertFalse(runtime.files().isEmpty());
    }

    private ManagedLocalAiCache.Installation adopt(Path cache, String id, Payload... payloads) throws Exception {
        Path stage = cache.resolve("staging").resolve(id + ".extract-test");
        Files.createDirectories(stage);
        for (Payload payload : payloads) {
            Files.write(stage.resolve(payload.name()), payload.bytes());
        }
        Files.writeString(stage.resolve(".shaft-ready"), "ready");
        return ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, id, stage));
    }

    private List<String> tree(Path root) throws IOException {
        try (var paths = Files.walk(root)) {
            return paths.map(path -> root.relativize(path).toString())
                    .sorted().toList();
        }
    }

    private ManagedLocalAiService service(Path cache, boolean enabled, String model,
                                           ManagedLocalAiHardware.HostAccess host) {
        return new ManagedLocalAiService(() -> new ManagedLocalAiService.Settings(
                enabled, true, model, cache.toString()), host);
    }

    private ManagedLocalAiService service(Path cache, boolean enabled, String model,
                                           ManagedLocalAiHardware.HostAccess host,
                                           ManagedLocalAiManifest manifest) {
        return new ManagedLocalAiService(() -> new ManagedLocalAiService.Settings(
                enabled, true, model, cache.toString()), host, () -> manifest);
    }

    private ManagedLocalAiManifest manifest(byte[] runtimeArchive, byte[] modelPayload) {
        var asset = new ManagedLocalAiManifest.RuntimeAsset("windows-x86_64", "runtime.zip",
                URI.create("https://github.com/ggml-org/llama.cpp/releases/download/test/runtime.zip"),
                runtimeArchive.length, sha256(runtimeArchive), "llama-server.exe", "windows-msvc", "");
        var runtime = new ManagedLocalAiManifest.RuntimeManifest("llama.cpp", "test", "MIT",
                URI.create("https://github.com/ggml-org/llama.cpp/releases/tag/test"), List.of(asset));
        var model = new ManagedLocalAiManifest.ModelManifest("test-model", "Test model", "lite", true, true,
                "Apache-2.0", "owner/repo", "0123456789012345678901234567890123456789", "model.gguf",
                URI.create("https://huggingface.co/owner/repo/resolve/0123456789012345678901234567890123456789/model.gguf"),
                modelPayload.length, sha256(modelPayload), 1, 1, 1);
        return new ManagedLocalAiManifest(1, runtime, List.of(model));
    }

    private String sha256(byte[] bytes) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (NoSuchAlgorithmException impossible) {
            throw new AssertionError(impossible);
        }
    }

    private ManagedLocalAiHardware.HostAccess host(String os, String arch, String abi, String abiVersion,
                                                    long memory, int processors, long disk) {
        return new ManagedLocalAiHardware.HostAccess() {
            public String osName() { return os; }
            public String architecture() { return arch; }
            public String abi() { return abi; }
            public String abiVersion() { return abiVersion; }
            public long availableMemoryBytes() { return memory; }
            public int availableProcessors() { return processors; }
            public long usableSpace(Path ignored) { return disk; }
            public String read(String ignored) { return null; }
        };
    }

    private static final class ThrowingHost implements ManagedLocalAiHardware.HostAccess {
        private AssertionError unexpected() { return new AssertionError("disabled inspection touched hardware"); }
        public String osName() { throw unexpected(); }
        public String architecture() { throw unexpected(); }
        public String abi() { throw unexpected(); }
        public String abiVersion() { throw unexpected(); }
        public long availableMemoryBytes() { throw unexpected(); }
        public int availableProcessors() { throw unexpected(); }
        public long usableSpace(Path ignored) { throw unexpected(); }
        public String read(String ignored) throws IOException { throw unexpected(); }
    }

    private record Payload(String name, byte[] bytes) {
    }
}
