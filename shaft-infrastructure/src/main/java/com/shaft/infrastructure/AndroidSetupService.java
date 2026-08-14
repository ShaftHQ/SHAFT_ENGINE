package com.shaft.infrastructure;

import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

/** Transaction coordinator for one exact Appium and Android emulator setup plan. */
final class AndroidSetupService {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final ConcurrentHashMap<Path, ReentrantLock> JVM_LOCKS = new ConcurrentHashMap<>();

    private final ShaftCachePaths paths;
    private final SetupPlatform platform;
    private final SetupArchitecture architecture;
    private final AndroidSetupRequest request;
    private final AndroidToolchainOperations operations;
    private final boolean offline;

    AndroidSetupService(ShaftCachePaths paths, SetupPlatform platform, SetupArchitecture architecture,
                        AndroidSetupRequest request, AndroidToolchainOperations operations, boolean offline) {
        this.paths = java.util.Objects.requireNonNull(paths, "paths");
        this.platform = java.util.Objects.requireNonNull(platform, "platform");
        this.architecture = java.util.Objects.requireNonNull(architecture, "architecture");
        this.request = java.util.Objects.requireNonNull(request, "request");
        this.operations = java.util.Objects.requireNonNull(operations, "operations");
        this.offline = offline;
    }

    SetupProfileStatus status() {
        List<SetupStatus> targets = AndroidSetupPlanner.plan(platform, architecture, SetupMode.MANAGED, request)
                .actions().stream().map(operations::status).toList();
        SetupReadiness readiness = targets.stream().allMatch(target -> target.readiness() == SetupReadiness.READY)
                ? SetupReadiness.READY
                : targets.stream().anyMatch(target -> target.readiness() == SetupReadiness.DEGRADED)
                ? SetupReadiness.DEGRADED : SetupReadiness.MISSING;
        if (readiness == SetupReadiness.READY && !hasCompatibleReceipt()) {
            java.util.ArrayList<SetupStatus> adjusted = new java.util.ArrayList<>(targets);
            SetupStatus last = adjusted.getLast();
            adjusted.set(adjusted.size() - 1, new SetupStatus(last.target(), SetupReadiness.DEGRADED,
                    last.detectedVersion(), "Managed files exist without a compatible SHAFT receipt."));
            targets = List.copyOf(adjusted);
            readiness = SetupReadiness.DEGRADED;
        }
        return new SetupProfileStatus(1, SetupProfile.MOBILE_ANDROID, readiness, targets);
    }

    SetupReceipt install(SetupPlan plan, SetupApproval approval) throws IOException {
        requireCompatible(plan);
        SetupExecutor.validate(plan, approval);
        operations.preflight(plan.actions(), offline);
        Path lockPath = paths.state().resolve("mobile-android.lock").toAbsolutePath().normalize();
        VerifiedArtifactStore.requireUnlinkedAncestors(lockPath);
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath, ignored -> new ReentrantLock());
        boolean acquired = false;
        try {
            jvmLock.lockInterruptibly();
            acquired = true;
            Files.createDirectories(paths.state());
            try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                 FileLock ignored = channel.lock()) {
                operations.preflight(plan.actions(), offline);
                SetupReceipt receipt = SetupExecutor.execute(plan, approval, action -> {
                    try {
                        operations.install(action);
                    } catch (IOException failure) {
                        throw new SetupOperationException(failure);
                    }
                });
                writeReceipt(receipt);
                return receipt;
            }
        } catch (InterruptedException interrupted) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while waiting for the Android setup lock.", interrupted);
        } finally {
            if (acquired) jvmLock.unlock();
        }
    }

    private void requireCompatible(SetupPlan plan) {
        if (plan.profile() != SetupProfile.MOBILE_ANDROID) {
            throw new IllegalArgumentException("Not an Android mobile setup plan.");
        }
        if (plan.platform() != platform || plan.architecture() != architecture) {
            throw new IllegalArgumentException("Plan platform does not match this host.");
        }
        if (plan.mode() == SetupMode.EXTERNAL) {
            throw new IllegalArgumentException("External plans are diagnostic and cannot be installed.");
        }
        SetupPlan expected = AndroidSetupPlanner.plan(platform, architecture, plan.mode(), request);
        if (!expected.equals(plan)) {
            throw new IllegalArgumentException("Plan does not match the Android manifest shipped with this release.");
        }
    }

    private void writeReceipt(SetupReceipt receipt) throws IOException {
        Files.createDirectories(paths.receipts());
        Path destination = paths.receipts().resolve("mobile-android.json");
        Path temporary = Files.createTempFile(paths.receipts(), "mobile-android", ".tmp");
        try {
            Files.writeString(temporary, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(receipt));
            VerifiedArtifactStore.move(temporary, destination);
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private boolean hasCompatibleReceipt() {
        Path receiptPath = paths.receipts().resolve("mobile-android.json");
        try {
            VerifiedArtifactStore.requireUnlinkedAncestors(receiptPath);
            if (!Files.isRegularFile(receiptPath, java.nio.file.LinkOption.NOFOLLOW_LINKS)) return false;
            SetupReceipt receipt = JSON.readValue(receiptPath.toFile(), SetupReceipt.class);
            for (SetupMode mode : List.of(SetupMode.MANAGED, SetupMode.HYBRID)) {
                SetupPlan expected = AndroidSetupPlanner.plan(platform, architecture, mode, request);
                if (receipt.planDigest().equals(expected.digest())
                        && receipt.completedActions().equals(expected.actions())) return true;
            }
            return false;
        } catch (IOException | RuntimeException invalid) {
            return false;
        }
    }

    private static final class SetupOperationException extends RuntimeException {
        private SetupOperationException(IOException cause) {
            super(cause);
        }
    }
}
