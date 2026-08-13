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
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

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

    @Test
    void provisionPublishesOrderedProgressReusesReadyCacheAndCanBeCancelledAndRetried() throws Exception {
        Path cache = temp.resolve("provision-cache");
        byte[] runtimeArchive = "runtime".getBytes();
        byte[] modelPayload = "model".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtimeArchive, modelPayload);
        FakeProvisioning provisioning = new FakeProvisioning(cache, manifest, runtimeArchive, modelPayload);
        ManagedLocalAiService service = service(cache, true, "test-model",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB),
                manifest, provisioning);

        List<ManagedLocalAiSnapshot.Phase> phases = new CopyOnWriteArrayList<>();
        ManagedLocalAiOperation first = service.provision(snapshot -> phases.add(snapshot.phase()));
        ManagedLocalAiSnapshot ready = first.completion().get(5, TimeUnit.SECONDS);
        assertEquals(ManagedLocalAiSnapshot.State.READY, ready.state());
        assertEquals(List.of(ManagedLocalAiSnapshot.Phase.DOWNLOADING_RUNTIME,
                ManagedLocalAiSnapshot.Phase.EXTRACTING_RUNTIME,
                ManagedLocalAiSnapshot.Phase.ADOPTING,
                ManagedLocalAiSnapshot.Phase.DOWNLOADING_MODEL,
                ManagedLocalAiSnapshot.Phase.ADOPTING,
                ManagedLocalAiSnapshot.Phase.IDLE), phases);
        assertEquals(1, provisioning.runtimeDownloads);
        assertEquals(1, provisioning.modelDownloads);

        service.provision(ignored -> { throw new IllegalStateException("observer failure"); })
                .completion().get(5, TimeUnit.SECONDS);
        assertEquals(1, provisioning.runtimeDownloads, "a ready owned runtime must be reused");
        assertEquals(1, provisioning.modelDownloads, "a ready owned model must be reused");

        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1), () -> ManagedLocalAiCache.clean(cache));
        ManagedLocalAiCache.Installation prior = adopt(cache, "prior-owned",
                new Payload("prior.bin", "prior".getBytes()));
        Path unknown = cache.resolve("user-note.txt");
        Files.writeString(unknown, "mine");
        provisioning.blockModel.set(true);
        provisioning.modelStarted = new CountDownLatch(1);
        ManagedLocalAiOperation cancelled = service.provision(ignored -> { });
        assertTrue(provisioning.modelStarted.await(5, TimeUnit.SECONDS));
        assertTrue(cancelled.cancel());
        assertThrows(java.util.concurrent.CancellationException.class,
                () -> cancelled.completion().get(5, TimeUnit.SECONDS));
        assertFalse(ManagedLocalAiCache.ownsInstallation(cache,
                ManagedLocalAiService.runtimeInstallationId(manifest, "windows-x86_64")));
        assertFalse(ManagedLocalAiCache.ownsInstallation(cache,
                ManagedLocalAiService.modelInstallationId(manifest.models().getFirst())));
        assertEquals(prior, ManagedLocalAiCache.verify(cache, "prior-owned"));
        assertEquals("mine", Files.readString(unknown));

        provisioning.blockModel.set(false);
        ManagedLocalAiSnapshot retried = service.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS);
        assertEquals(ManagedLocalAiSnapshot.State.READY, retried.state());
    }

    @Test
    void defaultProvisioningRunsTheRealCompositionAndRollsBackLateCancellationWithoutDeletingUnknowns()
            throws Exception {
        Path cache = temp.resolve("default-provision-cache");
        byte[] executable = "executable".getBytes();
        byte[] runtimeArchive = zip("bin/llama-server.exe", executable);
        byte[] modelPayload = "model-payload".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtimeArchive, modelPayload);
        AtomicBoolean addUnknown = new AtomicBoolean();
        ManagedLocalAiService.ArtifactAccess artifacts = new ManagedLocalAiService.ArtifactAccess() {
            public void download(ManagedLocalAiManifest.RuntimeAsset asset, Path target, Duration timeout,
                                 java.util.function.BooleanSupplier cancelled)
                    throws IOException, InterruptedException {
                ManagedLocalAiArtifacts.download(new ByteArrayInputStream(runtimeArchive), asset.size(),
                        asset.sha256(), target, cancelled);
            }
            public void download(ManagedLocalAiManifest.ModelManifest model, Path target, Duration timeout,
                                 java.util.function.BooleanSupplier cancelled)
                    throws IOException, InterruptedException {
                ManagedLocalAiArtifacts.download(new ByteArrayInputStream(modelPayload), model.size(),
                        model.sha256(), target, cancelled);
            }
            public ManagedLocalAiArtifacts.Extraction extract(Path archive, Path destination,
                                                               java.util.function.BooleanSupplier cancelled)
                    throws IOException, InterruptedException {
                ManagedLocalAiArtifacts.Extraction extraction = ManagedLocalAiArtifacts.extractStage(
                        archive, destination, cancelled);
                if (addUnknown.get()) {
                    Files.writeString(extraction.root().resolve("concurrent-user.txt"), "mine");
                }
                return extraction;
            }
        };
        ManagedLocalAiService.DefaultProvisioning defaultProvisioning =
                new ManagedLocalAiService.DefaultProvisioning(artifacts);
        ManagedLocalAiService service = service(cache, true, "test-model",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB), manifest,
                defaultProvisioning);

        assertEquals(ManagedLocalAiSnapshot.State.READY,
                service.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS).state());
        service.clean();

        AtomicReference<ManagedLocalAiOperation> operation = new AtomicReference<>();
        ManagedLocalAiOperation late = service.provision(snapshot -> {
            if (snapshot.phase() == ManagedLocalAiSnapshot.Phase.ADOPTING
                    && snapshot.completedBytes() == snapshot.totalBytes()) {
                while (operation.get() == null) Thread.onSpinWait();
                operation.get().cancel();
            }
        });
        operation.set(late);
        assertThrows(java.util.concurrent.CancellationException.class,
                () -> late.completion().get(5, TimeUnit.SECONDS));
        assertFalse(ManagedLocalAiCache.ownsInstallation(cache,
                ManagedLocalAiService.runtimeInstallationId(manifest, "windows-x86_64")));
        assertFalse(ManagedLocalAiCache.ownsInstallation(cache,
                ManagedLocalAiService.modelInstallationId(manifest.models().getFirst())));

        addUnknown.set(true);
        assertThrows(java.util.concurrent.ExecutionException.class,
                () -> service.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS));
        try (var paths = Files.walk(cache.resolve("staging"))) {
            Path unknown = paths.filter(path -> path.getFileName().toString().equals("concurrent-user.txt"))
                    .findFirst().orElseThrow();
            assertEquals("mine", Files.readString(unknown));
        }
    }

    @Test
    void cancellationAlwaysWinsReadyAndFailureRaces() throws Exception {
        Path cache = temp.resolve("cancel-races");
        byte[] runtime = "runtime".getBytes();
        byte[] model = "model".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtime, model);
        ManagedLocalAiService readyService = service(cache, true, "test-model",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB), manifest,
                new FakeProvisioning(cache, manifest, runtime, model));
        readyService.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS);

        AtomicReference<ManagedLocalAiOperation> readyOperation = new AtomicReference<>();
        ManagedLocalAiOperation readyCancel = readyService.provision(snapshot -> {
            while (readyOperation.get() == null) Thread.onSpinWait();
            readyOperation.get().cancel();
        });
        readyOperation.set(readyCancel);
        assertThrows(java.util.concurrent.CancellationException.class,
                () -> readyCancel.completion().get(5, TimeUnit.SECONDS));

        ManagedLocalAiService.Provisioning failing = (ignoredCache, ignoredManifest, profile, selected, settings,
                                                       host, cancelled, progress) -> {
            while (!cancelled.getAsBoolean()) Thread.onSpinWait();
            throw new IOException("failure after cancel");
        };
        ManagedLocalAiService missingService = service(temp.resolve("failure-race"), true, "test-model",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB), manifest, failing);
        ManagedLocalAiOperation failureRace = missingService.provision(ignored -> { });
        assertTrue(failureRace.cancel());
        assertThrows(java.util.concurrent.CancellationException.class,
                () -> failureRace.completion().get(5, TimeUnit.SECONDS));
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
                                           ManagedLocalAiManifest manifest,
                                           ManagedLocalAiService.Provisioning provisioning) {
        return new ManagedLocalAiService(() -> new ManagedLocalAiService.Settings(
                enabled, true, model, cache.toString()), host, () -> manifest, provisioning);
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

    private byte[] zip(String name, byte[] content) throws IOException {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        try (ZipOutputStream zip = new ZipOutputStream(output)) {
            zip.putNextEntry(new ZipEntry(name));
            zip.write(content);
            zip.closeEntry();
        }
        return output.toByteArray();
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

    private final class FakeProvisioning implements ManagedLocalAiService.Provisioning {
        private final Path cache;
        private final ManagedLocalAiManifest manifest;
        private final byte[] runtime;
        private final byte[] model;
        private final AtomicBoolean blockModel = new AtomicBoolean();
        private volatile CountDownLatch modelStarted = new CountDownLatch(1);
        private int runtimeDownloads;
        private int modelDownloads;

        private FakeProvisioning(Path cache, ManagedLocalAiManifest manifest, byte[] runtime, byte[] model) {
            this.cache = cache;
            this.manifest = manifest;
            this.runtime = runtime;
            this.model = model;
        }

        public ManagedLocalAiService.ProvisionResult provision(Path ignoredCache,
                              ManagedLocalAiManifest ignoredManifest,
                              ManagedLocalAiHardware.Profile profile,
                              ManagedLocalAiManifest.ModelManifest selected,
                              ManagedLocalAiService.Settings settings,
                              ManagedLocalAiHardware.HostAccess host,
                              java.util.function.BooleanSupplier cancelled,
                              java.util.function.Consumer<ManagedLocalAiService.Progress> progress) throws Exception {
            long total = runtime.length + model.length;
            String runtimeId = ManagedLocalAiService.runtimeInstallationId(manifest, profile.platform());
            try {
                ManagedLocalAiCache.verify(cache, runtimeId);
            } catch (IllegalStateException missing) {
                runtimeDownloads++;
                progress.accept(new ManagedLocalAiService.Progress(
                        ManagedLocalAiSnapshot.Phase.DOWNLOADING_RUNTIME, 0, total));
                progress.accept(new ManagedLocalAiService.Progress(
                        ManagedLocalAiSnapshot.Phase.EXTRACTING_RUNTIME, runtime.length, total));
                progress.accept(new ManagedLocalAiService.Progress(
                        ManagedLocalAiSnapshot.Phase.ADOPTING, runtime.length, total));
                adopt(cache, runtimeId, new Payload(manifest.runtime().assets().getFirst().file(), runtime),
                        new Payload(manifest.runtime().assets().getFirst().executable(), "executable".getBytes()));
            }
            String modelId = ManagedLocalAiService.modelInstallationId(selected);
            try {
                ManagedLocalAiCache.verify(cache, modelId);
            } catch (IllegalStateException missing) {
                modelDownloads++;
                progress.accept(new ManagedLocalAiService.Progress(
                        ManagedLocalAiSnapshot.Phase.DOWNLOADING_MODEL, runtime.length, total));
                modelStarted.countDown();
                try {
                    while (blockModel.get() && !cancelled.getAsBoolean()) {
                        Thread.sleep(10);
                    }
                    if (!cancelled.getAsBoolean()) {
                        progress.accept(new ManagedLocalAiService.Progress(
                                ManagedLocalAiSnapshot.Phase.ADOPTING, total, total));
                        adopt(cache, modelId, new Payload(selected.file(), model));
                        return new ManagedLocalAiService.ProvisionResult(java.util.Set.of(runtimeId, modelId));
                    }
                } catch (InterruptedException interrupted) {
                    Thread.interrupted();
                    try {
                        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                                () -> ManagedLocalAiCache.clean(cache, java.util.Set.of(runtimeId)));
                    } finally {
                        Thread.currentThread().interrupt();
                    }
                    throw interrupted;
                }
                ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                        () -> ManagedLocalAiCache.clean(cache, java.util.Set.of(runtimeId)));
                throw new InterruptedException("cancelled");
            }
            return new ManagedLocalAiService.ProvisionResult(java.util.Set.of());
        }
    }
}
