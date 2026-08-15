package com.shaft.ai.local;

import com.shaft.pilot.ai.AiProviderAvailability;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.AiUsage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import tools.jackson.databind.node.JsonNodeFactory;

import java.nio.file.Path;
import java.nio.file.Files;
import java.net.URI;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.security.MessageDigest;
import java.util.HexFormat;
import java.util.List;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedLocalAiProviderTest {
    @TempDir
    Path temp;

    @Test
    void availabilityAllowsReadyAndTransparentProvisionableStatesOnly() {
        assertTrue(provider(snapshot(ManagedLocalAiSnapshot.State.READY, true), null, true).availability().available());
        assertTrue(provider(snapshot(ManagedLocalAiSnapshot.State.NOT_PROVISIONED, true), null, true)
                .availability().available());

        AiProviderAvailability explicit = provider(
                snapshot(ManagedLocalAiSnapshot.State.NOT_PROVISIONED, false), null, true).availability();
        assertFalse(explicit.available());
        assertEquals("Provision the reviewed managed runtime and model.", explicit.reason());
        assertFalse(provider(snapshot(ManagedLocalAiSnapshot.State.EXCLUDED, true), null, true)
                .availability().available());
        assertFalse(provider(snapshot(ManagedLocalAiSnapshot.State.READY, true), null, false)
                .availability().available());
    }

    @Test
    void executionDelegatesOnlyTheAlreadyApprovedRequest() {
        AiRequest request = AiRequest.builder("managed-provider", JsonNodeFactory.instance.objectNode())
                .deterministicFallback(JsonNodeFactory.instance.objectNode().put("fallback", true)).build();
        AiResponse expected = AiResponse.success("managed-local", "selected-model",
                JsonNodeFactory.instance.objectNode().put("answer", "local"), Duration.ofMillis(5),
                AiUsage.empty(), request.deterministicFallback());

        AtomicReference<AiRequest> received = new AtomicReference<>();
        AtomicInteger calls = new AtomicInteger();
        ManagedLocalAiProvider provider = new ManagedLocalAiProvider(new ManagedLocalAiProvider.Lifecycle() {
            @Override public boolean executable() { return true; }
            @Override public ManagedLocalAiSnapshot inspect() {
                return snapshot(ManagedLocalAiSnapshot.State.READY, true);
            }
            @Override public AiResponse execute(AiRequest actual) {
                received.set(actual);
                calls.incrementAndGet();
                return expected;
            }
        });

        assertSame(expected, provider.execute(request));
        assertSame(request, received.get());
        assertEquals(1, calls.get());

        AiResponse failed = new ManagedLocalAiProvider(new ManagedLocalAiProvider.Lifecycle() {
            @Override public boolean executable() { return true; }
            @Override public ManagedLocalAiSnapshot inspect() { throw new AssertionError(); }
            @Override public AiResponse execute(AiRequest ignored) {
                throw new IllegalStateException("secret-lifecycle-detail");
            }
        }).execute(request);
        assertEquals(request.deterministicFallback(), failed.structuredPayload());
        assertFalse(failed.fallbackReason().contains("secret-lifecycle-detail"));
    }

    @Test
    void productionLifecycleIsExecutable() {
        var lifecycle = new ManagedLocalAiProvider.ServiceLifecycle(new ManagedLocalAiService());

        assertTrue(lifecycle.executable());
    }

    @Test
    void serviceLoadedProvidersShareOneProcessLifecycleOwner() {
        ManagedLocalAiProvider first = new ManagedLocalAiProvider();
        ManagedLocalAiProvider second = new ManagedLocalAiProvider();

        assertSame(first.lifecycle, second.lifecycle);
    }

    @Test
    void timedOutTransparentProvisioningIsCancelled() throws Exception {
        byte[] runtimeBytes = "runtime".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        byte[] modelBytes = "model".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        ManagedLocalAiManifest manifest = manifest(runtimeBytes, modelBytes);
        AtomicBoolean cancellationObserved = new AtomicBoolean();
        AtomicBoolean release = new AtomicBoolean();
        var started = new java.util.concurrent.CountDownLatch(1);
        ManagedLocalAiService.Provisioning provisioning = (cache, ignoredManifest, profile, selected, settings,
                                                             host, cancelled, progress) -> {
            started.countDown();
            try {
                while (!cancelled.getAsBoolean() && !release.get()) {
                    Thread.sleep(5);
                }
                cancellationObserved.set(cancelled.getAsBoolean());
                throw new InterruptedException("provisioning stopped");
            } catch (InterruptedException interrupted) {
                cancellationObserved.set(cancelled.getAsBoolean());
                throw interrupted;
            }
        };
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> new ManagedLocalAiService.Settings(true, true, "test-model", temp.toString()),
                host(), () -> manifest, provisioning);
        var lifecycle = serviceLifecycle(service);
        AiRequest request = AiRequest.builder("provision-timeout", JsonNodeFactory.instance.objectNode())
                .timeout(Duration.ofMillis(50)).build();

        try {
            AiResponse response = lifecycle.execute(request);
            assertTrue(started.await(1, java.util.concurrent.TimeUnit.SECONDS));
            assertEquals(com.shaft.pilot.ai.AiResponseStatus.TIMEOUT, response.status());
            assertTrue(waitUntil(cancellationObserved, Duration.ofSeconds(1)),
                    "timed-out provisioning must receive cooperative cancellation");
        } finally {
            release.set(true);
        }
    }

    @Test
    void transparentProvisioningWaitUsesTimeRemainingAfterSynchronousPreflight() throws Exception {
        byte[] runtimeBytes = "runtime".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        byte[] modelBytes = "model".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        ManagedLocalAiManifest manifest = manifest(runtimeBytes, modelBytes);
        AtomicInteger manifestReads = new AtomicInteger();
        AtomicBoolean release = new AtomicBoolean();
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> new ManagedLocalAiService.Settings(true, true, "test-model", temp.toString()), host(), () -> {
                    if (manifestReads.incrementAndGet() == 2) {
                        try {
                            Thread.sleep(300);
                        } catch (InterruptedException interrupted) {
                            Thread.currentThread().interrupt();
                            throw new IllegalStateException(interrupted);
                        }
                    }
                    return manifest;
                }, (cache, ignoredManifest, profile, selected, settings, host, cancelled, progress) -> {
                    while (!cancelled.getAsBoolean() && !release.get()) {
                        Thread.sleep(5);
                    }
                    throw new InterruptedException("provisioning stopped");
                });
        var lifecycle = serviceLifecycle(service);
        AiRequest request = AiRequest.builder("preflight-timeout", JsonNodeFactory.instance.objectNode())
                .timeout(Duration.ofMillis(200)).build();
        long started = System.nanoTime();

        try {
            AiResponse response = lifecycle.execute(request);
            assertEquals(com.shaft.pilot.ai.AiResponseStatus.TIMEOUT, response.status());
            assertTrue(Duration.ofNanos(System.nanoTime() - started).toMillis() < 450,
                    "expired synchronous preflight must not be followed by a stale full wait");
        } finally {
            release.set(true);
        }
    }

    @Test
    void concurrentRequestTimeoutIncludesLifecycleQueueTime() throws Exception {
        ManagedLocalAiManifest manifest = manifest("runtime".getBytes(), "model".getBytes());
        AtomicBoolean release = new AtomicBoolean();
        var provisioningStarted = new java.util.concurrent.CountDownLatch(1);
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> new ManagedLocalAiService.Settings(true, true, "test-model", temp.toString()),
                host(), () -> manifest,
                (cache, ignoredManifest, profile, selected, settings, host, cancelled, progress) -> {
                    provisioningStarted.countDown();
                    while (!release.get() && !cancelled.getAsBoolean()) {
                        Thread.sleep(5);
                    }
                    throw new InterruptedException("provisioning stopped");
                });
        var lifecycle = serviceLifecycle(service);
        AiRequest longRequest = AiRequest.builder("long", JsonNodeFactory.instance.objectNode())
                .timeout(Duration.ofSeconds(1)).build();
        AiRequest shortRequest = AiRequest.builder("short", JsonNodeFactory.instance.objectNode())
                .timeout(Duration.ofMillis(50)).build();
        var first = java.util.concurrent.CompletableFuture.supplyAsync(() -> invoke(lifecycle, longRequest));
        assertTrue(provisioningStarted.await(1, java.util.concurrent.TimeUnit.SECONDS));
        var second = java.util.concurrent.CompletableFuture.supplyAsync(() -> invoke(lifecycle, shortRequest));

        try {
            Thread.sleep(150);
            assertTrue(second.isDone(), "the short request must time out while queued for lifecycle ownership");
            assertEquals(com.shaft.pilot.ai.AiResponseStatus.TIMEOUT, second.join().status());
        } finally {
            release.set(true);
            first.join();
            second.join();
        }
    }

    @Test
    void failureCleanupUsesCurrentRequestBudgetInsteadOfPriorSessionBudget() throws Exception {
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> { throw new IllegalStateException("inspection failed"); }, host());
        var lifecycle = serviceLifecycle(service);
        lifecycle.session = new ManagedLocalAiProcess.Session(
                new SlowProcess(), 18181, "prior", "key", Duration.ofMillis(300));
        AiRequest request = AiRequest.builder("short-cleanup", JsonNodeFactory.instance.objectNode())
                .timeout(Duration.ofMillis(50)).build();
        long started = System.nanoTime();

        AiResponse response = lifecycle.execute(request);

        assertEquals(com.shaft.pilot.ai.AiResponseStatus.PROVIDER_UNAVAILABLE, response.status());
        assertTrue(Duration.ofNanos(System.nanoTime() - started).toMillis() < 180,
                "cleanup must not inherit the prior session's longer timeout");
    }

    @Test
    void availabilityFirstCallerRetiresSessionAfterConfigurationIsDisabled() throws Exception {
        AtomicInteger inspections = new AtomicInteger();
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> {
                    inspections.incrementAndGet();
                    return new ManagedLocalAiService.Settings(false, false, "auto", temp.toString());
                }, host());
        var lifecycle = serviceLifecycle(service);
        CloseAwareProcess process = new CloseAwareProcess();
        lifecycle.session = new ManagedLocalAiProcess.Session(
                process, 18181, "prior", "key", Duration.ofMillis(100));
        ManagedLocalAiProvider provider = new ManagedLocalAiProvider(lifecycle);

        AiProviderAvailability availability = provider.availability();

        assertFalse(availability.available());
        assertEquals(1, inspections.get(), "disabled state must be inspected under lifecycle ownership");
        assertFalse(process.isAlive(), "disabled managed AI must not leave its prior runtime alive");
    }

    @Test
    void availabilityInspectionFailureRetiresAnIdleSession() throws Exception {
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> { throw new IllegalStateException("settings unavailable"); }, host());
        var lifecycle = serviceLifecycle(service);
        CloseAwareProcess process = new CloseAwareProcess();
        lifecycle.session = new ManagedLocalAiProcess.Session(
                process, 18181, "prior", "key", Duration.ofMillis(100));
        ManagedLocalAiProvider provider = new ManagedLocalAiProvider(lifecycle);

        AiProviderAvailability availability = provider.availability();

        assertFalse(availability.available());
        assertFalse(process.isAlive(), "failed availability inspection must retire an idle stale runtime");
    }

    @Test
    void anotherThreadsUnavailableConfigurationDoesNotKillAnActiveRequest() throws Exception {
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> new ManagedLocalAiService.Settings(false, false, "auto", temp.toString()), host());
        var lifecycle = serviceLifecycle(service);
        CloseAwareProcess process = new CloseAwareProcess();
        lifecycle.session = new ManagedLocalAiProcess.Session(
                process, 18181, "prior", "key", Duration.ofMillis(100));
        var lock = lifecycle.executionLock;
        var lockHeld = new java.util.concurrent.CountDownLatch(1);
        var release = new java.util.concurrent.CountDownLatch(1);
        var active = java.util.concurrent.CompletableFuture.runAsync(() -> {
            lock.lock();
            try {
                lockHeld.countDown();
                release.await();
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
            } finally {
                lock.unlock();
            }
        });
        assertTrue(lockHeld.await(1, java.util.concurrent.TimeUnit.SECONDS));
        ManagedLocalAiProvider provider = new ManagedLocalAiProvider(lifecycle);

        try {
            assertFalse(provider.availability().available());
            assertTrue(process.isAlive(),
                    "one thread's unavailable override must not abort another thread's active request");
        } finally {
            release.countDown();
            active.join();
            process.destroyForcibly();
        }
    }

    @Test
    void readyRuntimeWithSameAliasDoesNotReuseAProcessFromAnotherInstallation() throws Exception {
        byte[] runtimeBytes = "runtime-b".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        byte[] modelBytes = "model-b".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        ManagedLocalAiManifest manifest = manifest(runtimeBytes, modelBytes);
        Path cache = temp.resolve("cache-b");
        String runtimeId = ManagedLocalAiService.runtimeInstallationId(manifest, "windows-x86_64");
        String modelId = ManagedLocalAiService.modelInstallationId(manifest.models().getFirst());
        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1), () ->
                ManagedLocalAiCache.adopt(cache, runtimeId,
                        readyStage(cache, "runtime-b", "llama-server.exe", "runtime-b")));
        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1), () ->
                ManagedLocalAiCache.adopt(cache, modelId,
                        readyStage(cache, "model-b", "model.gguf", "model-b")));
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> new ManagedLocalAiService.Settings(true, false, "test-model", cache.toString()),
                host(), () -> manifest, (ignoredCache, ignoredManifest, profile, selected, settings,
                                         ignoredHost, cancelled, progress) -> {
                    throw new AssertionError("ready cache must not provision");
                });
        var lifecycle = serviceLifecycle(service);
        CloseAwareProcess process = new CloseAwareProcess();
        lifecycle.session = new ManagedLocalAiProcess.Session(
                process, 18181, "test-model", "key", Duration.ofMillis(100));
        ManagedLocalAiProvider provider = new ManagedLocalAiProvider(lifecycle);

        provider.execute(AiRequest.builder("changed-runtime", JsonNodeFactory.instance.objectNode())
                .timeout(Duration.ofSeconds(3)).build());

        assertFalse(process.isAlive(), "alias equality must not reuse a process from another runtime installation");
    }

    @Test
    void transparentCleanCacheProvisioningReachesLaunchAndInference() throws Exception {
        byte[] runtimeBytes = "runtime-clean-host".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        byte[] modelBytes = "model-clean-host".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        ManagedLocalAiManifest manifest = manifest(runtimeBytes, modelBytes);
        Path cache = temp.resolve("clean-host-cache");
        AtomicInteger provisions = new AtomicInteger();
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> new ManagedLocalAiService.Settings(true, true, "test-model", cache.toString()), host(),
                () -> manifest, (root, reviewed, profile, selected, settings, ignoredHost, cancelled, progress) -> {
                    provisions.incrementAndGet();
                    String runtimeId = ManagedLocalAiService.runtimeInstallationId(reviewed, profile.platform());
                    String modelId = ManagedLocalAiService.modelInstallationId(selected);
                    Path runtimeStage = root.resolve("staging/clean-runtime.extract-test");
                    Files.createDirectories(runtimeStage);
                    Files.write(runtimeStage.resolve("runtime.zip"), runtimeBytes);
                    Files.writeString(runtimeStage.resolve("llama-server.exe"), "executable");
                    Files.writeString(runtimeStage.resolve(".shaft-ready"), "");
                    ManagedLocalAiCache.withLock(root, Duration.ofSeconds(1), () -> {
                        ManagedLocalAiCache.adopt(root, runtimeId, runtimeStage);
                        ManagedLocalAiCache.adopt(root, modelId,
                                readyStage(root, "clean-model", "model.gguf", "model-clean-host"));
                        return null;
                    });
                    return new ManagedLocalAiService.ProvisionResult(java.util.Set.of(runtimeId, modelId));
                });
        AtomicInteger launches = new AtomicInteger();
        AtomicInteger inferences = new AtomicInteger();
        CloseAwareProcess process = new CloseAwareProcess();
        AiRequest request = AiRequest.builder("clean-host", JsonNodeFactory.instance.objectNode())
                .timeout(Duration.ofSeconds(3)).build();
        AiResponse expected = AiResponse.success("managed-local", "test-model",
                JsonNodeFactory.instance.objectNode().put("answer", "local"), Duration.ofMillis(1),
                AiUsage.empty(), request.deterministicFallback());
        ManagedLocalAiProvider.RuntimeClient runtime = new ManagedLocalAiProvider.RuntimeClient() {
            @Override
            public ManagedLocalAiProcess.Session launch(ManagedLocalAiService.ReadyRuntime ready, Duration timeout,
                                                        java.util.function.BooleanSupplier shuttingDown) {
                launches.incrementAndGet();
                assertTrue(Files.isRegularFile(ready.executable()));
                assertTrue(Files.isRegularFile(ready.model()));
                return new ManagedLocalAiProcess.Session(process, 18181, ready.alias(), "key", timeout);
            }

            @Override
            public AiResponse infer(ManagedLocalAiProcess.Session session, AiRequest actual, long deadline) {
                inferences.incrementAndGet();
                assertSame(request, actual);
                return expected;
            }
        };
        ManagedLocalAiProvider provider = new ManagedLocalAiProvider(serviceLifecycle(service, runtime));

        AiResponse actual = provider.execute(request);
        ManagedLocalAiSnapshot after = service.inspect();
        assertEquals(ManagedLocalAiSnapshot.State.READY, after.state(), after.toString());
        ManagedLocalAiService.ReadyRuntime ready = service.readyRuntime();

        assertEquals(1, provisions.get());
        assertTrue(Files.isRegularFile(ready.executable()));
        assertEquals(1, launches.get());
        assertEquals(1, inferences.get());
        assertSame(expected, actual);
    }

    @Test
    void expiredCleanupDoesNotMaskTimeoutOrDiscardLiveSession() throws Exception {
        ManagedLocalAiManifest manifest = manifest("runtime".getBytes(), "model".getBytes());
        AtomicInteger reads = new AtomicInteger();
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> new ManagedLocalAiService.Settings(true, true, "test-model", temp.toString()), host(), () -> {
                    if (reads.incrementAndGet() == 2) {
                        try {
                            Thread.sleep(100);
                        } catch (InterruptedException interrupted) {
                            Thread.currentThread().interrupt();
                        }
                    }
                    return manifest;
                }, (cache, ignoredManifest, profile, selected, settings, host, cancelled, progress) -> {
                    while (!cancelled.getAsBoolean()) Thread.sleep(5);
                    throw new InterruptedException("cancelled");
                });
        var lifecycle = serviceLifecycle(service);
        AsyncForceProcess process = new AsyncForceProcess();
        lifecycle.session = new ManagedLocalAiProcess.Session(
                process, 18181, "prior", "key", Duration.ofMillis(300));
        ManagedLocalAiProvider provider = new ManagedLocalAiProvider(lifecycle);
        AiRequest request = AiRequest.builder("expired-cleanup", JsonNodeFactory.instance.objectNode())
                .timeout(Duration.ofMillis(30)).build();

        AiResponse response = provider.execute(request);

        assertEquals(com.shaft.pilot.ai.AiResponseStatus.TIMEOUT, response.status());
        assertTrue(process.forceKillRequested);
        assertSame(process, lifecycle.session.process(),
                "a still-live process must retain a lifecycle owner");
    }

    @Test
    void shutdownCleanupCancelsActiveExecutionInsteadOfWaitingForeverForLock() throws Exception {
        ManagedLocalAiManifest manifest = manifest("runtime".getBytes(), "model".getBytes());
        AtomicBoolean release = new AtomicBoolean();
        var provisioningStarted = new java.util.concurrent.CountDownLatch(1);
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> new ManagedLocalAiService.Settings(true, true, "test-model", temp.toString()), host(),
                () -> manifest, (cache, ignoredManifest, profile, selected, settings, host, cancelled, progress) -> {
                    provisioningStarted.countDown();
                    while (!release.get() && !cancelled.getAsBoolean()) Thread.sleep(5);
                    throw new InterruptedException("stopped");
                });
        var lifecycle = serviceLifecycle(service);
        var active = java.util.concurrent.CompletableFuture.supplyAsync(() -> invoke(lifecycle,
                AiRequest.builder("active", JsonNodeFactory.instance.objectNode())
                        .timeout(Duration.ofSeconds(1)).build()));
        assertTrue(provisioningStarted.await(1, java.util.concurrent.TimeUnit.SECONDS));
        var shutdown = java.util.concurrent.CompletableFuture.runAsync(lifecycle::shutdown);

        try {
            Thread.sleep(200);
            assertTrue(shutdown.isDone(), "shutdown cleanup must cancel active work before taking lifecycle ownership");
        } finally {
            release.set(true);
            active.join();
            shutdown.join();
        }
    }

    @Test
    void shutdownLockTimeoutStillForceKillsExistingSession() throws Exception {
        var lifecycle = serviceLifecycle(new ManagedLocalAiService(
                () -> { throw new IllegalStateException("unused"); }, host()));
        DelayedExitProcess process = new DelayedExitProcess();
        lifecycle.session = new ManagedLocalAiProcess.Session(
                process, 18181, "prior", "key", Duration.ofMillis(300));
        var lock = lifecycle.executionLock;
        lock.lock();
        var shutdown = java.util.concurrent.CompletableFuture.runAsync(lifecycle::shutdown);

        try {
            shutdown.get(3, java.util.concurrent.TimeUnit.SECONDS);
            assertTrue(process.forceKillRequested,
                    "bounded shutdown fallback must still issue termination to the live session");
            assertFalse(process.isAlive(),
                    "shutdown fallback must wait until asynchronous force termination is observable");
        } finally {
            lock.unlock();
            shutdown.join();
        }
    }

    @Test
    void shutdownFallbackDoesNotWaitForConcurrentSessionCleanup() throws Exception {
        var lifecycle = serviceLifecycle(new ManagedLocalAiService(
                () -> { throw new IllegalStateException("unused"); }, host()));
        BlockingCloseProcess process = new BlockingCloseProcess();
        ManagedLocalAiProcess.Session owned = new ManagedLocalAiProcess.Session(
                process, 18181, "prior", "key", Duration.ofSeconds(5));
        lifecycle.session = owned;
        var lock = lifecycle.executionLock;
        var requestCleanup = java.util.concurrent.CompletableFuture.runAsync(() ->
                owned.close(Duration.ofSeconds(5), new IllegalStateException("request cleanup")));
        assertTrue(process.destroyEntered.await(1, java.util.concurrent.TimeUnit.SECONDS));
        lock.lock();
        var shutdown = java.util.concurrent.CompletableFuture.runAsync(lifecycle::shutdown);

        try {
            Thread.sleep(2_500);
            assertTrue(shutdown.isDone(),
                    "shutdown fallback must remain bounded while request cleanup owns the session");
            assertTrue(process.forceKillRequested,
                    "shutdown fallback must force-kill without waiting for the session cleanup monitor");
        } finally {
            lock.unlock();
            process.release.countDown();
            requestCleanup.join();
            shutdown.join();
        }
    }

    @Test
    void shutdownIsTerminalForRacingExecutions() throws Exception {
        AtomicInteger inspections = new AtomicInteger();
        ManagedLocalAiService service = new ManagedLocalAiService(() -> {
            inspections.incrementAndGet();
            return new ManagedLocalAiService.Settings(false, false, "auto", temp.toString());
        }, host());
        var lifecycle = serviceLifecycle(service);
        lifecycle.shutdown();
        ManagedLocalAiProvider provider = new ManagedLocalAiProvider(lifecycle);

        provider.execute(AiRequest.builder("after-shutdown", JsonNodeFactory.instance.objectNode()).build());

        assertEquals(0, inspections.get(), "execution must be rejected without reopening lifecycle work");
    }

    @Test
    void shutdownCannotBeFollowedByAProcessPublishedFromAnInFlightInspection() throws Exception {
        byte[] runtimeBytes = "runtime".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        byte[] modelBytes = "model".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        ManagedLocalAiManifest manifest = manifest(runtimeBytes, modelBytes);
        Path cache = temp.resolve("shutdown-race-cache");
        String runtimeId = ManagedLocalAiService.runtimeInstallationId(manifest, "windows-x86_64");
        String modelId = ManagedLocalAiService.modelInstallationId(manifest.models().getFirst());
        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1), () ->
                ManagedLocalAiCache.adopt(cache, runtimeId,
                        readyStage(cache, "runtime-race", "llama-server.exe", "runtime")));
        ManagedLocalAiCache.withLock(cache, Duration.ofSeconds(1), () ->
                ManagedLocalAiCache.adopt(cache, modelId,
                        readyStage(cache, "model-race", "model.gguf", "model")));
        var inspectionStarted = new java.util.concurrent.CountDownLatch(1);
        var releaseInspection = new java.util.concurrent.CountDownLatch(1);
        AtomicBoolean firstInspection = new AtomicBoolean(true);
        ManagedLocalAiService service = new ManagedLocalAiService(() -> {
            if (firstInspection.compareAndSet(true, false)) {
                inspectionStarted.countDown();
                boolean released = false;
                while (!released) {
                    try {
                        released = releaseInspection.await(25, java.util.concurrent.TimeUnit.MILLISECONDS);
                    } catch (InterruptedException ignored) {
                        // Deliberately model a dependency that does not cooperate with interruption.
                    }
                }
            }
            return new ManagedLocalAiService.Settings(true, false, "test-model", cache.toString());
        }, host(), () -> manifest, (ignoredCache, ignoredManifest, profile, selected, settings,
                                     ignoredHost, cancelled, progress) -> {
            throw new AssertionError("ready cache must not provision");
        });
        var lifecycle = serviceLifecycle(service);
        var active = java.util.concurrent.CompletableFuture.supplyAsync(() -> invoke(lifecycle,
                AiRequest.builder("shutdown-race", JsonNodeFactory.instance.objectNode())
                        .timeout(Duration.ofSeconds(10)).build()));
        assertTrue(inspectionStarted.await(1, java.util.concurrent.TimeUnit.SECONDS));

        var shutdown = java.util.concurrent.CompletableFuture.runAsync(lifecycle::shutdown);
        shutdown.get(3, java.util.concurrent.TimeUnit.SECONDS);
        releaseInspection.countDown();
        AiResponse response = active.get(2, java.util.concurrent.TimeUnit.SECONDS);

        assertEquals(com.shaft.pilot.ai.AiResponseStatus.PROVIDER_UNAVAILABLE, response.status());
        assertFalse(java.nio.file.Files.exists(cache.resolve("staging/logs")),
                "an execution released after shutdown must not begin a process launch");
    }

    @Test
    void exitedParentWithLiveDescendantRetainsLifecycleOwnership() throws Exception {
        ManagedLocalAiService service = new ManagedLocalAiService(
                () -> { throw new IllegalStateException("inspection failed"); }, host());
        var lifecycle = serviceLifecycle(service);
        ParentExitProcess process = new ParentExitProcess();
        lifecycle.session = new ManagedLocalAiProcess.Session(process, 18181, "prior", "key", Duration.ZERO);
        process.alive = false;
        ManagedLocalAiProvider provider = new ManagedLocalAiProvider(lifecycle);

        provider.execute(AiRequest.builder("descendant-cleanup", JsonNodeFactory.instance.objectNode()).build());

        assertTrue(process.descendant.forceKillRequested);
        assertTrue(lifecycle.session != null,
                "a surviving descendant must keep the session under lifecycle ownership");
    }

    private AiResponse invoke(ManagedLocalAiProvider.ServiceLifecycle lifecycle, AiRequest request) {
        return lifecycle.execute(request);
    }

    private ManagedLocalAiProvider.ServiceLifecycle serviceLifecycle(ManagedLocalAiService service) {
        return new ManagedLocalAiProvider.ServiceLifecycle(service);
    }

    private ManagedLocalAiProvider.ServiceLifecycle serviceLifecycle(ManagedLocalAiService service,
                                                                      ManagedLocalAiProvider.RuntimeClient runtime) {
        return new ManagedLocalAiProvider.ServiceLifecycle(service, runtime);
    }

    private boolean waitUntil(AtomicBoolean value, Duration timeout) throws InterruptedException {
        long deadline = System.nanoTime() + timeout.toNanos();
        while (!value.get() && System.nanoTime() < deadline) {
            Thread.sleep(5);
        }
        return value.get();
    }

    private ManagedLocalAiManifest manifest(byte[] runtimeBytes, byte[] modelBytes) throws Exception {
        String runtimeHash = HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(runtimeBytes));
        String modelHash = HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(modelBytes));
        var asset = new ManagedLocalAiManifest.RuntimeAsset("windows-x86_64", "runtime.zip",
                URI.create("https://github.com/ggml-org/llama.cpp/releases/download/test/runtime.zip"),
                runtimeBytes.length, runtimeHash,
                "llama-server.exe", "windows-msvc", "");
        var runtime = new ManagedLocalAiManifest.RuntimeManifest("llama.cpp", "test", "MIT",
                URI.create("https://example.invalid/runtime"), List.of(asset));
        var model = new ManagedLocalAiManifest.ModelManifest("test-model", "Test model", "lite", true, true,
                "Apache-2.0", "owner/repo", "0123456789012345678901234567890123456789", "model.gguf",
                URI.create("https://huggingface.co/owner/repo/resolve/0123456789012345678901234567890123456789/"
                        + "model.gguf"), modelBytes.length, modelHash, 1, 1, 1);
        return new ManagedLocalAiManifest(1, runtime, List.of(model));
    }

    private Path readyStage(Path cache, String prefix, String fileName, String content) throws Exception {
        Path stage = cache.resolve("staging/" + prefix + ".extract-test");
        java.nio.file.Files.createDirectories(stage);
        java.nio.file.Files.writeString(stage.resolve(fileName), content);
        java.nio.file.Files.writeString(stage.resolve(".shaft-ready"), "");
        return stage;
    }

    private ManagedLocalAiHardware.HostAccess host() {
        return new ManagedLocalAiHardware.HostAccess() {
            @Override
            public String osName() { return "Windows 11"; }
            @Override
            public String architecture() { return "amd64"; }
            @Override
            public String abi() { return "windows-msvc"; }
            @Override
            public String abiVersion() { return ""; }
            @Override
            public long availableMemoryBytes() { return 16L * 1024 * 1024 * 1024; }
            @Override
            public int availableProcessors() { return 8; }
            @Override
            public long usableSpace(Path ignored) { return 64L * 1024 * 1024 * 1024; }
            @Override
            public String read(String ignored) { return null; }
        };
    }

    private static final class SlowProcess extends Process {
        private boolean alive = true;
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public java.io.InputStream getErrorStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public int waitFor() throws InterruptedException { Thread.currentThread().join(); return -1; }
        @Override public boolean waitFor(long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException {
            if (!alive) {
                return true;
            }
            unit.sleep(timeout);
            return false;
        }
        @Override public int exitValue() { if (alive) throw new IllegalThreadStateException(); return -1; }
        @Override public void destroy() { /* Simulates a process that ignores graceful termination. */ }
        @Override public Process destroyForcibly() { alive = false; return this; }
        @Override public boolean isAlive() { return alive; }
    }

    private static final class CloseAwareProcess extends Process {
        private boolean alive = true;
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public java.io.InputStream getErrorStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public int waitFor() { return -1; }
        @Override public boolean waitFor(long timeout, java.util.concurrent.TimeUnit unit) { return !alive; }
        @Override public int exitValue() { if (alive) throw new IllegalThreadStateException(); return -1; }
        @Override public void destroy() { alive = false; }
        @Override public Process destroyForcibly() { alive = false; return this; }
        @Override public boolean isAlive() { return alive; }
    }

    private static final class AsyncForceProcess extends Process {
        private boolean forceKillRequested;
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public java.io.InputStream getErrorStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public int waitFor() throws InterruptedException { Thread.currentThread().join(); return -1; }
        @Override public boolean waitFor(long timeout, java.util.concurrent.TimeUnit unit) { return false; }
        @Override public int exitValue() { throw new IllegalThreadStateException(); }
        @Override public void destroy() { /* Simulates a process that ignores graceful termination. */ }
        @Override public Process destroyForcibly() { forceKillRequested = true; return this; }
        @Override public boolean isAlive() { return true; }
    }

    private static final class DelayedExitProcess extends Process {
        private volatile boolean forceKillRequested;
        private volatile boolean alive = true;
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public java.io.InputStream getErrorStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public int waitFor() throws InterruptedException {
            while (alive) {
                Thread.sleep(5);
            }
            return -1;
        }
        @Override public boolean waitFor(long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException {
            long deadline = System.nanoTime() + unit.toNanos(timeout);
            while (alive && System.nanoTime() < deadline) {
                Thread.sleep(5);
            }
            return !alive;
        }
        @Override public int exitValue() { if (alive) throw new IllegalThreadStateException(); return -1; }
        @Override public void destroy() { /* Simulates a process that ignores graceful termination. */ }
        @Override public Process destroyForcibly() {
            forceKillRequested = true;
            Thread.ofVirtual().start(() -> {
                try {
                    Thread.sleep(50);
                    alive = false;
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                }
            });
            return this;
        }
        @Override public boolean isAlive() { return alive; }
    }

    private static final class BlockingCloseProcess extends Process {
        private final java.util.concurrent.CountDownLatch destroyEntered = new java.util.concurrent.CountDownLatch(1);
        private final java.util.concurrent.CountDownLatch release = new java.util.concurrent.CountDownLatch(1);
        private volatile boolean forceKillRequested;
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public java.io.InputStream getErrorStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public int waitFor() throws InterruptedException { release.await(); return -1; }
        @Override public boolean waitFor(long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException {
            release.await(timeout, unit);
            return forceKillRequested;
        }
        @Override public int exitValue() { if (!forceKillRequested) throw new IllegalThreadStateException(); return -1; }
        @Override public void destroy() { destroyEntered.countDown(); }
        @Override public Process destroyForcibly() { forceKillRequested = true; return this; }
        @Override public boolean isAlive() { return !forceKillRequested; }
    }

    private static final class ParentExitProcess extends Process {
        private boolean alive = true;
        private final AsyncHandle descendant = new AsyncHandle();
        @Override public java.io.OutputStream getOutputStream() { return new ByteArrayOutputStream(); }
        @Override public java.io.InputStream getInputStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public java.io.InputStream getErrorStream() { return new ByteArrayInputStream(new byte[0]); }
        @Override public int waitFor() throws InterruptedException { Thread.currentThread().join(); return -1; }
        @Override public boolean waitFor(long timeout, java.util.concurrent.TimeUnit unit) { return !alive; }
        @Override public int exitValue() { if (alive) throw new IllegalThreadStateException(); return -1; }
        @Override public void destroy() { /* Simulates a process that ignores graceful termination. */ }
        @Override public Process destroyForcibly() { alive = false; return this; }
        @Override public boolean isAlive() { return alive; }
        @Override public ProcessHandle toHandle() { return new ExitingParentHandle(this); }
    }

    private static final class ExitingParentHandle implements ProcessHandle {
        private final ParentExitProcess process;
        private ExitingParentHandle(ParentExitProcess process) { this.process = process; }
        @Override public long pid() { return 42; }
        @Override public java.util.Optional<ProcessHandle> parent() { return java.util.Optional.empty(); }
        @Override public java.util.stream.Stream<ProcessHandle> children() { return descendants(); }
        @Override public java.util.stream.Stream<ProcessHandle> descendants() {
            return process.alive ? java.util.stream.Stream.of(process.descendant) : java.util.stream.Stream.empty();
        }
        @Override public Info info() { throw new UnsupportedOperationException(); }
        @Override public java.util.concurrent.CompletableFuture<ProcessHandle> onExit() {
            return new java.util.concurrent.CompletableFuture<>();
        }
        @Override public boolean supportsNormalTermination() { return true; }
        @Override public boolean destroy() { return true; }
        @Override public boolean destroyForcibly() { process.alive = false; return true; }
        @Override public boolean isAlive() { return process.alive; }
        @Override public int compareTo(ProcessHandle other) { return Long.compare(pid(), other.pid()); }
    }

    private static final class AsyncHandle implements ProcessHandle {
        private boolean forceKillRequested;
        @Override public long pid() { return 43; }
        @Override public java.util.Optional<ProcessHandle> parent() { return java.util.Optional.empty(); }
        @Override public java.util.stream.Stream<ProcessHandle> children() { return java.util.stream.Stream.empty(); }
        @Override public java.util.stream.Stream<ProcessHandle> descendants() { return java.util.stream.Stream.of(this); }
        @Override public Info info() { throw new UnsupportedOperationException(); }
        @Override public java.util.concurrent.CompletableFuture<ProcessHandle> onExit() {
            return new java.util.concurrent.CompletableFuture<>();
        }
        @Override public boolean supportsNormalTermination() { return true; }
        @Override public boolean destroy() { return true; }
        @Override public boolean destroyForcibly() { forceKillRequested = true; return true; }
        @Override public boolean isAlive() { return true; }
        @Override public int compareTo(ProcessHandle other) { return Long.compare(pid(), other.pid()); }
    }

    private ManagedLocalAiProvider provider(ManagedLocalAiSnapshot snapshot, AiResponse response,
                                             boolean executable) {
        return new ManagedLocalAiProvider(new ManagedLocalAiProvider.Lifecycle() {
            @Override
            public boolean executable() {
                return executable;
            }

            @Override
            public ManagedLocalAiSnapshot inspect() {
                return snapshot;
            }

            @Override
            public AiResponse execute(AiRequest request) {
                return response;
            }
        });
    }

    private ManagedLocalAiSnapshot snapshot(ManagedLocalAiSnapshot.State state, boolean transparent) {
        return new ManagedLocalAiSnapshot(state, "Provision the reviewed managed runtime and model.",
                temp.toAbsolutePath(), true, transparent, "auto", "selected-model", "windows-x86_64",
                "llama.cpp", "test", "MIT", "runtime.zip", "0".repeat(64), "llama-server.exe", 1,
                ManagedLocalAiSnapshot.CacheHealth.MISSING, ManagedLocalAiSnapshot.CacheHealth.MISSING,
                ManagedLocalAiSnapshot.Phase.IDLE, 0, 0, 16, 8, 32, Map.of());
    }
}
