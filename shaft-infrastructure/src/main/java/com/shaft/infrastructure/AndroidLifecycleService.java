package com.shaft.infrastructure;

import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.URI;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

/** Lease-safe lifecycle owner for one verified Android emulator and local Appium server. */
final class AndroidLifecycleService {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final ConcurrentHashMap<Path, ReentrantLock> JVM_LOCKS = new ConcurrentHashMap<>();
    private static final int EMULATOR_CONSOLE_PORT = 5554;
    private static final int EMULATOR_ADB_PORT = EMULATOR_CONSOLE_PORT + 1;

    private final ShaftCachePaths paths;
    private final SetupPlatform platform;
    private final SetupArchitecture architecture;
    private final AndroidSetupRequest request;
    private final AndroidToolchainOperations operations;
    private final AndroidRuntimeController runtime;
    private final AndroidRuntimeHealth health;
    private final AndroidRuntimeLayout layout;

    AndroidLifecycleService(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture,
                            AndroidSetupRequest request, AndroidToolchainOperations operations,
                            AndroidRuntimeController runtime, AndroidRuntimeHealth health) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.platform = java.util.Objects.requireNonNull(platform, "platform");
        this.architecture = java.util.Objects.requireNonNull(architecture, "architecture");
        this.request = java.util.Objects.requireNonNull(request, "request").resolve(architecture);
        this.operations = java.util.Objects.requireNonNull(operations, "operations");
        this.runtime = java.util.Objects.requireNonNull(runtime, "runtime");
        this.health = java.util.Objects.requireNonNull(health, "health");
        this.layout = AndroidRuntimeLayout.resolve(paths, platform, architecture, this.request);
    }

    ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        requireCompatible(plan, options);
        SetupExecutor.validate(plan, approval);
        SetupReceipt receipt = requireInstallReceipt(plan);
        requireInstalled(plan);
        Path lockPath = paths.state().resolve("mobile-android-runtime.lock").toAbsolutePath().normalize();
        VerifiedArtifactStore.requireUnlinkedAncestors(lockPath);
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        try {
            jvmLock.lockInterruptibly();
            Files.createDirectories(paths.state());
            try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                 FileLock ignored = channel.lock()) {
                receipt = requireInstallReceipt(plan);
                requireInstalled(plan);
                Optional<ActiveRuntime> reusable = readReusable(plan, options);
                if (reusable.isPresent()) return environment(plan, receipt, reusable.orElseThrow(), options);
                return startNew(plan, receipt, options);
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for the Android runtime lock.", interrupted);
        } finally {
            if (jvmLock.isHeldByCurrentThread()) jvmLock.unlock();
        }
    }

    Path emulatorLog() { return layout.emulatorLog(); }
    Path appiumLog() { return layout.appiumLog(); }

    boolean stop(Duration timeout) throws IOException {
        Path lockPath = paths.state().resolve("mobile-android-runtime.lock").toAbsolutePath().normalize();
        VerifiedArtifactStore.requireUnlinkedAncestors(lockPath);
        if (Files.notExists(leasePath(), LinkOption.NOFOLLOW_LINKS)) return false;
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        jvmLock.lock();
        try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
             FileLock ignored = channel.lock()) {
            Optional<AndroidRuntimeLease> current = readLease();
            if (current.isEmpty()) return false;
            AndroidRuntimeLease lease = current.orElseThrow();
            requireLeaseRequest(lease);
            Optional<AndroidOwnedProcess> emulator = find(lease.emulator());
            Optional<AndroidOwnedProcess> appium = find(lease.appium());
            if (emulator.isEmpty() && appium.isEmpty()) {
                Files.deleteIfExists(leasePath());
                return false;
            }
            if (emulator.isEmpty() || appium.isEmpty()) throw new IOException(
                    "Android runtime lease is partially alive; refusing to kill an uncertain process identity.");
            IOException cleanup = stopStarted(appium.orElseThrow(), emulator.orElseThrow(), timeout);
            if (cleanup != null) throw cleanup;
            Files.deleteIfExists(leasePath());
            return true;
        } finally {
            jvmLock.unlock();
        }
    }

    private ManagedEnvironment startNew(SetupPlan plan, SetupReceipt receipt, SetupOptions options) throws IOException {
        requireAvailablePort(EMULATOR_CONSOLE_PORT, "Android emulator console");
        requireAvailablePort(EMULATOR_ADB_PORT, "Android emulator adb");
        requireAvailablePort(request.appiumPort(), "Appium");
        Map<String, String> androidEnvironment = androidEnvironment();
        Set<String> removed = Set.of("APPIUM_HOME", "ANDROID_HOME", "ANDROID_SDK_ROOT", "ANDROID_AVD_HOME",
                "ANDROID_SERIAL", "REPO_OS_OVERRIDE");
        AndroidOwnedProcess emulator = null;
        AndroidOwnedProcess appium = null;
        try {
            emulator = runtime.start("emulator", List.of(layout.emulator().toString(), "-avd", request.avdName(),
                    "-port", Integer.toString(EMULATOR_CONSOLE_PORT), "-no-snapshot-save", "-no-boot-anim",
                    "-no-audio", "-no-window", "-memory", Integer.toString(request.ramMb()), "-cores",
                    Integer.toString(request.cores())), layout.sdkRoot(), androidEnvironment, removed,
                    layout.emulatorLog());
            health.awaitEmulator(layout.serial(), layout, androidEnvironment, options.startupTimeout());
            URI endpoint = URI.create("http://127.0.0.1:" + request.appiumPort() + '/');
            Map<String, String> appiumEnvironment = new LinkedHashMap<>(androidEnvironment);
            appiumEnvironment.put("APPIUM_HOME", layout.appiumHome().toString());
            appium = runtime.start("appium", List.of(layout.nodeExecutable().toString(),
                    layout.appiumEntryPoint().toString(), "--address", "127.0.0.1", "--port",
                    Integer.toString(request.appiumPort()), "--base-path", "/"), layout.appiumHome(),
                    Map.copyOf(appiumEnvironment), removed, layout.appiumLog());
            health.awaitAppium(endpoint, options.startupTimeout());
            AndroidRuntimeLease lease = new AndroidRuntimeLease(1, plan.digest(), request.avdName(), layout.serial(),
                    endpoint.toString(), ProcessIdentity.of(emulator), ProcessIdentity.of(appium), 1);
            writeLease(lease);
            return environment(plan, receipt, new ActiveRuntime(lease, emulator, appium), options);
        } catch (IOException | RuntimeException failure) {
            IOException cleanup = stopStarted(appium, emulator, options.shutdownTimeout());
            if (cleanup != null) failure.addSuppressed(cleanup);
            if (failure instanceof IOException io) throw io;
            throw failure;
        }
    }

    private ManagedEnvironment environment(SetupPlan plan, SetupReceipt receipt, ActiveRuntime active,
                                           SetupOptions options) {
        Map<String, String> properties = new LinkedHashMap<>(androidEnvironment());
        properties.put("APPIUM_HOME", layout.appiumHome().toString());
        properties.put("ANDROID_SERIAL", layout.serial());
        properties.put("appium.endpoint", active.lease().endpoint());
        return new ManagedEnvironment(SetupProfile.MOBILE_ANDROID, receipt,
                Optional.of(URI.create(active.lease().endpoint())), Map.copyOf(properties), () -> {
                    try {
                        release(active, options.shutdownTimeout());
                    } catch (IOException failure) {
                        throw new IllegalStateException("Failed to release the owned Android runtime.", failure);
                    }
                });
    }

    private Optional<ActiveRuntime> readReusable(SetupPlan plan, SetupOptions options) throws IOException {
        Optional<AndroidRuntimeLease> existing = readLease();
        if (existing.isEmpty()) return Optional.empty();
        AndroidRuntimeLease lease = existing.orElseThrow();
        if (!lease.planDigest().equals(plan.digest()) || !lease.avdName().equals(request.avdName())
                || !lease.endpoint().equals("http://127.0.0.1:" + request.appiumPort() + '/')) {
            throw new IOException("An existing SHAFT Android lease does not match the reviewed plan.");
        }
        Optional<AndroidOwnedProcess> emulator = find(lease.emulator());
        Optional<AndroidOwnedProcess> appium = find(lease.appium());
        if (emulator.isEmpty() && appium.isEmpty()) {
            Files.deleteIfExists(leasePath());
            return Optional.empty();
        }
        if (emulator.isEmpty() || appium.isEmpty()) {
            throw new IOException("The SHAFT Android runtime lease is stale or only partially alive; manual recovery "
                    + "is required before starting another runtime.");
        }
        if (!options.reuseOwnedProcesses()) {
            throw new IOException("A compatible SHAFT Android runtime is already active and reuse is disabled.");
        }
        health.awaitEmulator(lease.serial(), layout, androidEnvironment(), options.startupTimeout());
        health.awaitAppium(URI.create(lease.endpoint()), options.startupTimeout());
        AndroidRuntimeLease incremented = lease.withRefCount(lease.refCount() + 1);
        writeLease(incremented);
        return Optional.of(new ActiveRuntime(incremented, emulator.orElseThrow(), appium.orElseThrow()));
    }

    private void requireLeaseRequest(AndroidRuntimeLease lease) throws IOException {
        if (!lease.avdName().equals(request.avdName())
                || !lease.endpoint().equals("http://127.0.0.1:" + request.appiumPort() + '/')) {
            throw new IOException("Android runtime lease does not match the requested AVD and Appium endpoint.");
        }
    }

    private void release(ActiveRuntime active, Duration timeout) throws IOException {
        Path lockPath = paths.state().resolve("mobile-android-runtime.lock").toAbsolutePath().normalize();
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        jvmLock.lock();
        try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
             FileLock ignored = channel.lock()) {
            Optional<AndroidRuntimeLease> current = readLease();
            if (current.isEmpty()) return;
            AndroidRuntimeLease lease = current.orElseThrow();
            if (!lease.sameIdentity(active.lease())) {
                throw new IOException("Android runtime lease changed; refusing to stop an unowned process.");
            }
            if (lease.refCount() > 1) {
                writeLease(lease.withRefCount(lease.refCount() - 1));
                return;
            }
            IOException cleanup = stopStarted(active.appium(), active.emulator(), timeout);
            if (cleanup != null) throw cleanup;
            Files.deleteIfExists(leasePath());
        } finally {
            jvmLock.unlock();
        }
    }

    private void requireCompatible(SetupPlan plan, SetupOptions options) {
        java.util.Objects.requireNonNull(plan, "plan");
        java.util.Objects.requireNonNull(options, "options");
        if (plan.profile() != SetupProfile.MOBILE_ANDROID || options.profile() != SetupProfile.MOBILE_ANDROID) {
            throw new IllegalArgumentException("Android lifecycle requires profile MOBILE_ANDROID.");
        }
        if (plan.platform() != platform || plan.architecture() != architecture) {
            throw new IllegalArgumentException("Android lifecycle plan does not match this host.");
        }
        if (plan.mode() == SetupMode.EXTERNAL || options.effectiveMode() == SetupMode.EXTERNAL) {
            throw new IllegalArgumentException("External Android setup cannot start local processes.");
        }
        SetupPlan expected = AndroidSetupPlanner.plan(platform, architecture, plan.mode(), request);
        if (!expected.equals(plan)) throw new IllegalArgumentException(
                "Android lifecycle plan does not match the release manifest.");
    }

    private SetupReceipt requireInstallReceipt(SetupPlan plan) throws IOException {
        Path receiptPath = paths.receipts().resolve("mobile-android.json");
        VerifiedArtifactStore.requireUnlinkedAncestors(receiptPath);
        if (!Files.isRegularFile(receiptPath, LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("A complete compatible Android install receipt is required before start.");
        }
        SetupReceipt receipt;
        try {
            receipt = JSON.readValue(receiptPath.toFile(), SetupReceipt.class);
        } catch (RuntimeException invalid) {
            throw new IOException("Android install receipt is invalid.", invalid);
        }
        if (!receipt.planDigest().equals(plan.digest()) || !receipt.completedActions().equals(plan.actions())) {
            throw new IOException("Android install receipt does not match the reviewed plan.");
        }
        return receipt;
    }

    private void requireInstalled(SetupPlan plan) throws IOException {
        for (SetupAction action : plan.actions()) {
            SetupStatus status = operations.status(action);
            if (status.readiness() != SetupReadiness.READY) {
                throw new IOException(action.target() + " is not ready: " + status.detail());
            }
        }
    }

    private Map<String, String> androidEnvironment() {
        String path = String.join(java.io.File.pathSeparator, layout.sdkRoot().resolve("platform-tools").toString(),
                layout.sdkRoot().resolve("emulator").toString(),
                Optional.ofNullable(System.getenv("PATH")).orElse(""));
        return Map.of("ANDROID_HOME", layout.sdkRoot().toString(), "ANDROID_SDK_ROOT", layout.sdkRoot().toString(),
                "ANDROID_AVD_HOME", layout.avdHome().toString(), "ANDROID_SERIAL", layout.serial(), "PATH", path);
    }

    private void requireAvailablePort(int port, String owner) throws IOException {
        try (ServerSocket socket = new ServerSocket()) {
            socket.setReuseAddress(false);
            socket.bind(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), port));
        } catch (IOException occupied) {
            throw new IOException(owner + " loopback port " + port + " is already occupied.", occupied);
        }
    }

    private Optional<AndroidOwnedProcess> find(ProcessIdentity identity) throws IOException {
        return runtime.find(identity.pid(), Instant.ofEpochMilli(identity.startEpochMilli()), identity.commandIdentity());
    }

    private Optional<AndroidRuntimeLease> readLease() throws IOException {
        Path path = leasePath();
        VerifiedArtifactStore.requireUnlinkedAncestors(path);
        if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) return Optional.empty();
        try {
            AndroidRuntimeLease lease = JSON.readValue(path.toFile(), AndroidRuntimeLease.class);
            if (lease.schemaVersion() != 1 || lease.refCount() < 1) throw new IOException("Invalid Android lease.");
            return Optional.of(lease);
        } catch (RuntimeException invalid) {
            throw new IOException("Android runtime lease is invalid.", invalid);
        }
    }

    private void writeLease(AndroidRuntimeLease lease) throws IOException {
        Files.createDirectories(paths.state());
        Path temporary = Files.createTempFile(paths.state(), "mobile-android-runtime", ".tmp");
        try {
            Files.writeString(temporary, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(lease));
            VerifiedArtifactStore.move(temporary, leasePath());
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private Path leasePath() { return paths.state().resolve("mobile-android-runtime.json"); }

    private static IOException stopStarted(AndroidOwnedProcess appium, AndroidOwnedProcess emulator,
                                           Duration timeout) {
        IOException failure = null;
        Instant deadline = Instant.now().plus(timeout);
        for (AndroidOwnedProcess process : new AndroidOwnedProcess[]{appium, emulator}) {
            if (process == null) continue;
            try {
                Duration remaining = Duration.between(Instant.now(), deadline);
                process.stop(remaining.isNegative() ? Duration.ZERO : remaining);
            } catch (IOException cleanup) {
                if (failure == null) failure = cleanup;
                else failure.addSuppressed(cleanup);
            }
        }
        return failure;
    }

    private record ActiveRuntime(AndroidRuntimeLease lease, AndroidOwnedProcess emulator,
                                 AndroidOwnedProcess appium) { }

    private record ProcessIdentity(long pid, long startEpochMilli, String commandIdentity) {
        static ProcessIdentity of(AndroidOwnedProcess process) {
            return new ProcessIdentity(process.pid(), process.startInstant().toEpochMilli(),
                    process.commandIdentity());
        }
    }

    private record AndroidRuntimeLease(int schemaVersion, String planDigest, String avdName, String serial,
                                       String endpoint, ProcessIdentity emulator, ProcessIdentity appium,
                                       int refCount) {
        AndroidRuntimeLease withRefCount(int value) {
            return new AndroidRuntimeLease(schemaVersion, planDigest, avdName, serial, endpoint, emulator, appium,
                    value);
        }

        boolean sameIdentity(AndroidRuntimeLease other) {
            return planDigest.equals(other.planDigest) && emulator.equals(other.emulator) && appium.equals(other.appium);
        }
    }
}
