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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
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
    void eachRuntimeLaunchUsesANewContainedLogLeaf() {
        Path cache = temp.resolve("cache").toAbsolutePath();

        Path first = ManagedLocalAiService.inferenceLog(cache);
        Path second = ManagedLocalAiService.inferenceLog(cache);

        assertTrue(first.startsWith(cache.resolve("staging/logs")));
        assertTrue(second.startsWith(cache.resolve("staging/logs")));
        assertFalse(first.equals(second));
    }

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
    void inspectionReportsCorruptWhenAnActivatedArtifactChanges() throws Exception {
        Path cache = temp.resolve("activated-corrupt-cache");
        byte[] runtimeArchive = "runtime".getBytes();
        byte[] modelPayload = "model".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtimeArchive, modelPayload);
        ManagedLocalAiService service = service(cache, true, "test-model",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB),
                manifest, new FakeProvisioning(cache, manifest, runtimeArchive, modelPayload));

        assertEquals(ManagedLocalAiSnapshot.State.READY,
                service.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS).state());
        ManagedLocalAiCache.Installation model = ManagedLocalAiCache.verify(cache,
                ManagedLocalAiService.modelInstallationId(manifest.models().getFirst()));
        Path modelFile = model.files().stream()
                .map(file -> cache.resolve(file.path()))
                .filter(path -> path.getFileName().toString().equals("model.gguf"))
                .findFirst().orElseThrow();
        Files.writeString(modelFile, "changed");
        List<String> changed = tree(cache);

        assertEquals(ManagedLocalAiSnapshot.State.CORRUPT, service.inspect().state());
        assertEquals(changed, tree(cache), "corrupt activated inspection must remain read-only");
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

        service.clean();
        ManagedLocalAiCache.Installation prior = adopt(cache, "prior-owned",
                new Payload("prior.bin", "prior".getBytes()));
        Path unknown = cache.resolve("user-note.txt");
        Files.writeString(unknown, "mine");
        provisioning.blockModel.set(true);
        provisioning.modelWaitingForCancellation = new CountDownLatch(1);
        ManagedLocalAiOperation cancelled = service.provision(ignored -> { });
        assertTrue(provisioning.modelWaitingForCancellation.await(5, TimeUnit.SECONDS));
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
    void provisioningRetainsTheCallingThreadsEffectiveSettings() throws Exception {
        Path cache = temp.resolve("thread-local-provision-cache");
        byte[] runtimeArchive = "thread-local-runtime".getBytes();
        byte[] modelPayload = "thread-local-model".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtimeArchive, modelPayload);
        FakeProvisioning provisioning = new FakeProvisioning(cache, manifest, runtimeArchive, modelPayload);
        ManagedLocalAiService.Settings configured = new ManagedLocalAiService.Settings(
                true, true, "test-model", cache.toString());
        ThreadLocal<ManagedLocalAiService.Settings> local = new ThreadLocal<>();
        local.set(configured);
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> local.get() == null
                        ? new ManagedLocalAiService.Settings(false, false, "auto", temp.resolve("wrong").toString())
                        : local.get(),
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB),
                () -> manifest, provisioning);

        ManagedLocalAiSnapshot ready = service.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS);

        assertEquals(ManagedLocalAiSnapshot.State.READY, ready.state());
        assertEquals(cache.toAbsolutePath().normalize(), ready.cacheDirectory());
        assertEquals(1, provisioning.runtimeDownloads);
        assertEquals(1, provisioning.modelDownloads);
    }

    @Test
    void offlineProvisioningRechecksReadinessWithoutCallingArtifactTransport() throws Exception {
        Path cache = temp.resolve("offline-race-cache");
        byte[] runtimeArchive = "offline-runtime".getBytes();
        byte[] modelPayload = "offline-model".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtimeArchive, modelPayload);
        FakeProvisioning provisioning = new FakeProvisioning(cache, manifest, runtimeArchive, modelPayload);
        ManagedLocalAiService service = service(cache, true, "test-model",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB),
                manifest, provisioning);

        var failure = assertThrows(java.util.concurrent.ExecutionException.class,
                () -> service.provision(ignored -> { }, false).completion().get(5, TimeUnit.SECONDS));

        assertTrue(failure.getCause() instanceof IOException);
        assertEquals(0, provisioning.runtimeDownloads);
        assertEquals(0, provisioning.modelDownloads);
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
            @Override
            public void download(ManagedLocalAiManifest.RuntimeAsset asset, Path target, Duration timeout,
                                 java.util.function.BooleanSupplier cancelled)
                    throws IOException, InterruptedException {
                ManagedLocalAiArtifacts.download(new ByteArrayInputStream(runtimeArchive), asset.size(),
                        asset.sha256(), target, cancelled);
            }
            @Override
            public void download(ManagedLocalAiManifest.ModelManifest model, Path target, Duration timeout,
                                 java.util.function.BooleanSupplier cancelled)
                    throws IOException, InterruptedException {
                ManagedLocalAiArtifacts.download(new ByteArrayInputStream(modelPayload), model.size(),
                        model.sha256(), target, cancelled);
            }
            @Override
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
        assertFalse(Files.exists(cache.resolve("activation-history.json")));

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
            @Override
            public String osName() { return os; }
            @Override
            public String architecture() { return arch; }
            @Override
            public String abi() { return abi; }
            @Override
            public String abiVersion() { return abiVersion; }
            @Override
            public long availableMemoryBytes() { return memory; }
            @Override
            public int availableProcessors() { return processors; }
            @Override
            public long usableSpace(Path ignored) { return disk; }
            @Override
            public String read(String ignored) { return null; }
        };
    }

    @Test
    void reviewedCleanPreservesOtherOwnedInstallations(@TempDir Path cache) throws Exception {
        byte[] runtimeArchive = "runtime".getBytes();
        byte[] modelPayload = "model".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtimeArchive, modelPayload);
        FakeProvisioning provisioning = new FakeProvisioning(cache, manifest, runtimeArchive, modelPayload);
        ManagedLocalAiService service = service(cache, true, "test-model",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB),
                manifest, provisioning);
        assertEquals(ManagedLocalAiSnapshot.State.READY,
                service.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS).state());
        ManagedLocalAiCache.Installation prior = adopt(cache, "prior-reviewed-version",
                new Payload("prior.bin", "prior".getBytes()));

        boolean cleaned = service.cleanReviewed();

        assertTrue(cleaned);
        assertFalse(Files.exists(cache.resolve("activation-history.json")));
        assertEquals(prior, ManagedLocalAiCache.verify(cache, "prior-reviewed-version"));
        assertFalse(ManagedLocalAiCache.ownsInstallation(cache,
                ManagedLocalAiService.runtimeInstallationId(manifest, "windows-x86_64")));
        assertFalse(ManagedLocalAiCache.ownsInstallation(cache,
                ManagedLocalAiService.modelInstallationId(manifest.models().getFirst())));
    }

    @Test
    void successfulUpdatesAtomicallyRetainOneExactPriorActivation(@TempDir Path cache) throws Exception {
        ManagedLocalAiHardware.HostAccess host = host("Windows 11", "amd64", "windows-msvc", "",
                16 * GIB, 8, 64 * GIB);
        byte[] firstRuntime = "runtime-one".getBytes();
        byte[] firstModel = "model-one".getBytes();
        ManagedLocalAiManifest firstManifest = manifest(firstRuntime, firstModel);
        ManagedLocalAiService first = service(cache, true, "test-model", host, firstManifest,
                new FakeProvisioning(cache, firstManifest, firstRuntime, firstModel));
        assertEquals(ManagedLocalAiSnapshot.State.READY,
                first.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS).state());

        byte[] secondRuntime = "runtime-two".getBytes();
        byte[] secondModel = "model-two".getBytes();
        ManagedLocalAiManifest secondManifest = manifest(secondRuntime, secondModel);
        ManagedLocalAiService second = service(cache, true, "test-model", host, secondManifest,
                new FakeProvisioning(cache, secondManifest, secondRuntime, secondModel));
        assertEquals(ManagedLocalAiSnapshot.State.READY,
                second.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS).state());

        Path historyFile = cache.resolve("activation-history.json");
        assertTrue(Files.isRegularFile(historyFile));
        var history = tools.jackson.databind.json.JsonMapper.builder().build().readTree(historyFile.toFile());
        assertEquals(2, history.path("schemaVersion").asInt());
        assertEquals(ManagedLocalAiService.runtimeInstallationId(secondManifest, "windows-x86_64"),
                history.path("active").path("runtimeId").asText());
        assertEquals(ManagedLocalAiService.modelInstallationId(secondManifest.models().getFirst()),
                history.path("active").path("modelId").asText());
        assertEquals(secondManifest.runtime().assets().getFirst().sha256(),
                history.path("active").path("runtimeSha256").asText());
        assertEquals(secondManifest.models().getFirst().sha256(),
                history.path("active").path("modelSha256").asText());
        assertEquals(secondManifest.runtime().assets().getFirst().size(),
                history.path("active").path("runtimeArtifactBytes").asLong());
        assertEquals(secondManifest.models().getFirst().size(),
                history.path("active").path("modelArtifactBytes").asLong());
        assertEquals(secondManifest.runtime().assets().getFirst().url().toString(),
                history.path("active").path("runtimeUrl").asText());
        assertEquals(secondManifest.models().getFirst().url().toString(),
                history.path("active").path("modelUrl").asText());
        assertEquals(ManagedLocalAiService.runtimeInstallationId(firstManifest, "windows-x86_64"),
                history.path("previous").path("runtimeId").asText());
        assertEquals(ManagedLocalAiService.modelInstallationId(firstManifest.models().getFirst()),
                history.path("previous").path("modelId").asText());
        assertFalse(history.toString().contains(cache.toString()));
    }

    @Test
    void conflictedReviewedCleanPreservesActivationHistoryAndRollbackCandidate(@TempDir Path cache)
            throws Exception {
        ManagedLocalAiHardware.HostAccess host = host("Windows 11", "amd64", "windows-msvc", "",
                16 * GIB, 8, 64 * GIB);
        byte[] firstRuntime = "runtime-one".getBytes();
        byte[] firstModel = "model-one".getBytes();
        ManagedLocalAiManifest firstManifest = manifest(firstRuntime, firstModel);
        ManagedLocalAiService first = service(cache, true, "test-model", host, firstManifest,
                new FakeProvisioning(cache, firstManifest, firstRuntime, firstModel));
        assertEquals(ManagedLocalAiSnapshot.State.READY,
                first.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS).state());

        byte[] secondRuntime = "runtime-two".getBytes();
        byte[] secondModel = "model-two".getBytes();
        ManagedLocalAiManifest secondManifest = manifest(secondRuntime, secondModel);
        byte[] extraModelPayload = "extra-model".getBytes();
        ManagedLocalAiManifest.ModelManifest extraModel = new ManagedLocalAiManifest.ModelManifest(
                "extra-model", "Extra model", "lite", false, true, "Apache-2.0", "owner/repo",
                "1123456789012345678901234567890123456789", "extra.gguf",
                URI.create("https://huggingface.co/owner/repo/resolve/1123456789012345678901234567890123456789/extra.gguf"),
                extraModelPayload.length, sha256(extraModelPayload), 1, 1, 1);
        secondManifest = new ManagedLocalAiManifest(secondManifest.schemaVersion(), secondManifest.runtime(),
                List.of(secondManifest.models().getFirst(), extraModel));
        ManagedLocalAiService second = service(cache, true, "test-model", host, secondManifest,
                new FakeProvisioning(cache, secondManifest, secondRuntime, secondModel));
        assertEquals(ManagedLocalAiSnapshot.State.READY,
                second.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS).state());

        Path historyFile = cache.resolve("activation-history.json");
        byte[] historyBefore = Files.readAllBytes(historyFile);
        ManagedLocalAiActivationHistory.Activation rollbackBefore = second.rollbackCandidate();
        ManagedLocalAiCache.Installation changed = adopt(cache, ManagedLocalAiService.modelInstallationId(extraModel),
                new Payload(extraModel.file(), extraModelPayload));
        Path changedFile = changed.root().resolve(extraModel.file());
        Files.writeString(changedFile, "changed-owned-model");

        assertFalse(second.cleanReviewed());

        assertArrayEquals(historyBefore, Files.readAllBytes(historyFile));
        assertEquals("changed-owned-model", Files.readString(changedFile));
        assertEquals(rollbackBefore, second.rollbackCandidate());
    }

    @Test
    void cleanRecoveryNeverLeavesHistoryReferencingAnAlreadyDeletedInstallation(@TempDir Path cache)
            throws Exception {
        ManagedLocalAiHardware.HostAccess host = host("Windows 11", "amd64", "windows-msvc", "",
                16 * GIB, 8, 64 * GIB);
        byte[] runtime = "runtime".getBytes();
        byte[] model = "model".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtime, model);
        ManagedLocalAiService service = service(cache, true, "test-model", host, manifest,
                new FakeProvisioning(cache, manifest, runtime, model));
        assertEquals(ManagedLocalAiSnapshot.State.READY,
                service.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS).state());

        Path historyFile = cache.resolve(ManagedLocalAiActivationHistory.FILE);
        byte[] staleHistory = Files.readAllBytes(historyFile);
        ManagedLocalAiActivationHistory.Activation active = ManagedLocalAiActivationHistory.parse(staleHistory)
                .active();
        ManagedLocalAiCache.Installation installed = ManagedLocalAiCache.verify(cache, active.runtimeId());
        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.clean(cache, java.util.Set.of(active.runtimeId())));
        Files.write(historyFile, staleHistory);

        String source = cache.toAbsolutePath().normalize().relativize(installed.root())
                .toString().replace('\\', '/');
        String target = "trash/" + active.runtimeId() + "-interrupted";
        String files = installed.files().stream().map(file ->
                "{\"path\":\"%s\",\"size\":%d,\"sha256\":\"%s\"}".formatted(
                        file.path(), file.size(), file.sha256()))
                .collect(java.util.stream.Collectors.joining(","));
        Files.writeString(cache.resolve("transaction.json"), """
                {"schemaVersion":1,"operation":"CLEAN","id":"%s","source":"%s","target":"%s","files":[%s]}
                """.formatted(active.runtimeId(), source, target, files));

        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1), () -> null);

        assertFalse(Files.exists(historyFile));
        assertFalse(Files.exists(cache.resolve("transaction.json")));
    }

    @Test
    void failedAndCancelledUpdatesNeverPublishFalseActivation(@TempDir Path cache) throws Exception {
        ManagedLocalAiHardware.HostAccess host = host("Windows 11", "amd64", "windows-msvc", "",
                16 * GIB, 8, 64 * GIB);
        byte[] firstRuntime = "runtime-one".getBytes();
        byte[] firstModel = "model-one".getBytes();
        ManagedLocalAiManifest firstManifest = manifest(firstRuntime, firstModel);
        ManagedLocalAiService first = service(cache, true, "test-model", host, firstManifest,
                new FakeProvisioning(cache, firstManifest, firstRuntime, firstModel));
        first.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS);
        Path historyFile = cache.resolve("activation-history.json");
        byte[] before = Files.readAllBytes(historyFile);

        byte[] cancelledRuntime = "runtime-cancelled".getBytes();
        byte[] cancelledModel = "model-cancelled".getBytes();
        ManagedLocalAiManifest cancelledManifest = manifest(cancelledRuntime, cancelledModel);
        ManagedLocalAiService cancelledService = service(cache, true, "test-model", host, cancelledManifest,
                new FakeProvisioning(cache, cancelledManifest, cancelledRuntime, cancelledModel));
        AtomicReference<ManagedLocalAiOperation> current = new AtomicReference<>();
        ManagedLocalAiOperation cancelled = cancelledService.provision(snapshot -> {
            if (snapshot.state() == ManagedLocalAiSnapshot.State.READY) {
                while (current.get() == null) Thread.onSpinWait();
                current.get().cancel();
            }
        });
        current.set(cancelled);
        assertThrows(java.util.concurrent.CancellationException.class,
                () -> cancelled.completion().get(5, TimeUnit.SECONDS));
        assertArrayEquals(before, Files.readAllBytes(historyFile));

        Path failedCache = cache.resolve("failed-activation");
        ManagedLocalAiService.Provisioning failure = (ignoredCache, ignoredManifest, profile, model, settings,
                                                        ignoredHost, ignoredCancellation, ignoredProgress) -> {
            throw new IOException("failed before ready");
        };
        ManagedLocalAiService failed = service(failedCache, true, "test-model", host, firstManifest, failure);
        assertThrows(java.util.concurrent.ExecutionException.class,
                () -> failed.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS));
        assertFalse(Files.exists(failedCache.resolve("activation-history.json")));
    }

    @Test
    void cancellationWhileActivationWaitsForTheCacheLockRollsBackNewInstallations(@TempDir Path cache)
            throws Exception {
        byte[] runtime = "runtime-lock-race".getBytes();
        byte[] model = "model-lock-race".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtime, model);
        FakeProvisioning delegate = new FakeProvisioning(cache, manifest, runtime, model);
        CountDownLatch provisioned = new CountDownLatch(1);
        CountDownLatch lockHeld = new CountDownLatch(1);
        CountDownLatch releaseLock = new CountDownLatch(1);
        ManagedLocalAiService.Provisioning blockedPublication = (ignoredCache, ignoredManifest, profile, selected,
                                                                 settings, host, cancelled, progress) -> {
            ManagedLocalAiService.ProvisionResult result = delegate.provision(ignoredCache, ignoredManifest,
                    profile, selected, settings, host, cancelled, progress);
            provisioned.countDown();
            assertTrue(lockHeld.await(5, TimeUnit.SECONDS));
            return result;
        };
        ManagedLocalAiService service = service(cache, true, "test-model",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB), manifest,
                blockedPublication);
        ManagedLocalAiOperation operation = service.provision(ignored -> { });
        assertTrue(provisioned.await(5, TimeUnit.SECONDS));
        Thread holder = Thread.ofVirtual().start(() -> {
            try {
                ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(5), () -> {
                    lockHeld.countDown();
                    assertTrue(releaseLock.await(5, TimeUnit.SECONDS));
                    return null;
                });
            } catch (Exception failure) {
                throw new AssertionError(failure);
            }
        });
        assertTrue(lockHeld.await(5, TimeUnit.SECONDS));
        Thread.sleep(50);
        assertTrue(operation.cancel());
        releaseLock.countDown();
        holder.join(Duration.ofSeconds(5));
        assertThrows(java.util.concurrent.CancellationException.class,
                () -> operation.completion().get(5, TimeUnit.SECONDS));
        assertFalse(ManagedLocalAiCache.ownsInstallation(cache,
                ManagedLocalAiService.runtimeInstallationId(manifest, "windows-x86_64")));
        assertFalse(ManagedLocalAiCache.ownsInstallation(cache,
                ManagedLocalAiService.modelInstallationId(manifest.models().getFirst())));
        assertFalse(Files.exists(cache.resolve("activation-history.json")));
    }

    @Test
    void malformedHistoryBlocksPromotionAndCleanBeforeArtifactMutation(@TempDir Path cache) throws Exception {
        byte[] runtime = "runtime-history".getBytes();
        byte[] model = "model-history".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtime, model);
        ManagedLocalAiService service = service(cache, true, "test-model",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB), manifest,
                new FakeProvisioning(cache, manifest, runtime, model));
        service.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS);
        Path history = cache.resolve("activation-history.json");
        String tampered = Files.readString(history).replace("\"runtimeFile\":\"runtime.zip\"",
                "\"runtimeFile\":\"../../outside\"");
        Files.writeString(history, tampered);
        assertThrows(IllegalStateException.class, () -> service.provision(ignored -> { }));

        Files.delete(history);
        Files.createDirectory(history);
        assertThrows(IllegalStateException.class, service::clean);
        assertTrue(ManagedLocalAiCache.ownsInstallation(cache,
                ManagedLocalAiService.runtimeInstallationId(manifest, "windows-x86_64")));
        assertTrue(ManagedLocalAiCache.ownsInstallation(cache,
                ManagedLocalAiService.modelInstallationId(manifest.models().getFirst())));

        Path lateFailure = cache.resolve("late-history-failure");
        Files.createDirectories(lateFailure);
        Files.createDirectory(lateFailure.resolve("activation-history.json"));
        ManagedLocalAiService failed = service(lateFailure, true, "test-model",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB), manifest,
                new FakeProvisioning(lateFailure, manifest, runtime, model));
        assertThrows(IllegalStateException.class, () -> failed.provision(ignored -> { }));
        assertFalse(ManagedLocalAiCache.ownsInstallation(lateFailure,
                ManagedLocalAiService.runtimeInstallationId(manifest, "windows-x86_64")));
        assertFalse(ManagedLocalAiCache.ownsInstallation(lateFailure,
                ManagedLocalAiService.modelInstallationId(manifest.models().getFirst())));
    }

    @Test
    void activationPublicationRevalidatesOwnedArtifactsUnderTheCacheLock(@TempDir Path cache) throws Exception {
        byte[] runtime = "runtime-stale-ready".getBytes();
        byte[] model = "model-stale-ready".getBytes();
        ManagedLocalAiManifest manifest = manifest(runtime, model);
        ManagedLocalAiService service = service(cache, true, "test-model",
                host("Windows 11", "amd64", "windows-msvc", "", 16 * GIB, 8, 64 * GIB), manifest,
                new FakeProvisioning(cache, manifest, runtime, model));
        ManagedLocalAiSnapshot ready = service.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS);
        ManagedLocalAiActivationHistory.Activation activation = ManagedLocalAiActivationHistory.from(ready, manifest);
        service.clean();
        AtomicBoolean claimed = new AtomicBoolean();

        assertThrows(IllegalStateException.class, () -> ManagedLocalAiActivationHistory.publish(cache,
                Duration.ofSeconds(1), activation, () -> {
                    claimed.set(true);
                    return true;
                }));

        assertFalse(claimed.get());
        assertFalse(Files.exists(cache.resolve("activation-history.json")));
    }

    @Test
    void reviewedRollbackAtomicallyActivatesTheExactPreviousPairAndCanUpdateAgain(@TempDir Path cache)
            throws Exception {
        ManagedLocalAiHardware.HostAccess host = host("Windows 11", "amd64", "windows-msvc", "",
                16 * GIB, 8, 64 * GIB);
        byte[] firstRuntime = "runtime-rollback-one".getBytes();
        byte[] firstModel = "model-rollback-one".getBytes();
        ManagedLocalAiManifest firstManifest = manifest(firstRuntime, firstModel);
        ManagedLocalAiService first = service(cache, true, "test-model", host, firstManifest,
                new FakeProvisioning(cache, firstManifest, firstRuntime, firstModel));
        first.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS);

        byte[] secondRuntime = "runtime-rollback-two".getBytes();
        byte[] secondModel = "model-rollback-two".getBytes();
        ManagedLocalAiManifest secondManifest = manifest(secondRuntime, secondModel);
        FakeProvisioning secondProvisioning = new FakeProvisioning(cache, secondManifest, secondRuntime, secondModel);
        ManagedLocalAiService second = service(cache, true, "test-model", host, secondManifest, secondProvisioning);
        second.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS);

        ManagedLocalAiSnapshot rolledBack = second.rollbackReviewed();

        assertEquals(ManagedLocalAiSnapshot.State.READY, rolledBack.state());
        assertEquals(firstManifest.runtime().assets().getFirst().sha256(), rolledBack.runtimeAssetSha256());
        assertEquals(firstManifest.models().getFirst().sha256(),
                rolledBack.models().get(rolledBack.selectedModelId()).sha256());
        assertTrue(second.readyRuntime().executable().toString().contains(
                ManagedLocalAiService.runtimeInstallationId(firstManifest, "windows-x86_64")));

        ManagedLocalAiSnapshot updated = second.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS);
        assertEquals(secondManifest.runtime().assets().getFirst().sha256(), updated.runtimeAssetSha256());
        assertEquals(1, secondProvisioning.runtimeDownloads);
        assertEquals(1, secondProvisioning.modelDownloads);
    }

    @Test
    void rollbackRevalidatesHistoricalEligibilityInsideTheCommitLock(@TempDir Path cache) throws Exception {
        java.util.concurrent.atomic.AtomicLong memory = new java.util.concurrent.atomic.AtomicLong(16 * GIB);
        ManagedLocalAiHardware.HostAccess mutableHost = new ManagedLocalAiHardware.HostAccess() {
            @Override public String osName() { return "Windows 11"; }
            @Override public String architecture() { return "amd64"; }
            @Override public String abi() { return "windows-msvc"; }
            @Override public String abiVersion() { return ""; }
            @Override public long availableMemoryBytes() { return memory.get(); }
            @Override public int availableProcessors() { return 8; }
            @Override public long usableSpace(Path ignored) { return 64 * GIB; }
            @Override public String read(String ignored) { return ""; }
        };
        byte[] firstRuntime = "runtime-eligibility-one".getBytes();
        byte[] firstModel = "model-eligibility-one".getBytes();
        ManagedLocalAiManifest firstManifest = manifest(firstRuntime, firstModel);
        service(cache, true, "test-model", mutableHost, firstManifest,
                new FakeProvisioning(cache, firstManifest, firstRuntime, firstModel))
                .provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS);
        byte[] secondRuntime = "runtime-eligibility-two".getBytes();
        byte[] secondModel = "model-eligibility-two".getBytes();
        ManagedLocalAiManifest secondManifest = manifest(secondRuntime, secondModel);
        ManagedLocalAiService service = service(cache, true, "test-model", mutableHost, secondManifest,
                new FakeProvisioning(cache, secondManifest, secondRuntime, secondModel));
        service.provision(ignored -> { }).completion().get(5, TimeUnit.SECONDS);
        ManagedLocalAiActivationHistory.Activation candidate = service.rollbackCandidate();
        byte[] before = Files.readAllBytes(cache.resolve("activation-history.json"));

        memory.set(1);

        assertThrows(IllegalStateException.class, () -> service.rollbackReviewed(candidate));
        assertArrayEquals(before, Files.readAllBytes(cache.resolve("activation-history.json")));
    }

    private static final class ThrowingHost implements ManagedLocalAiHardware.HostAccess {
        private AssertionError unexpected() { return new AssertionError("disabled inspection touched hardware"); }
        @Override
        public String osName() { throw unexpected(); }
        @Override
        public String architecture() { throw unexpected(); }
        @Override
        public String abi() { throw unexpected(); }
        @Override
        public String abiVersion() { throw unexpected(); }
        @Override
        public long availableMemoryBytes() { throw unexpected(); }
        @Override
        public int availableProcessors() { throw unexpected(); }
        @Override
        public long usableSpace(Path ignored) { throw unexpected(); }
        @Override
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
        private volatile CountDownLatch modelWaitingForCancellation = new CountDownLatch(1);
        private int runtimeDownloads;
        private int modelDownloads;

        private FakeProvisioning(Path cache, ManagedLocalAiManifest manifest, byte[] runtime, byte[] model) {
            this.cache = cache;
            this.manifest = manifest;
            this.runtime = runtime;
            this.model = model;
        }

        @Override
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
                try {
                    while (blockModel.get() && !cancelled.getAsBoolean()) {
                        modelWaitingForCancellation.countDown();
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
