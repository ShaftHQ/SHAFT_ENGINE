package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.LinkOption;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;
import tools.jackson.databind.json.JsonMapper;

final class OcrSetupProvider implements SetupProvider {
    private static final JsonMapper JSON = JsonMapper.builder().build();
    private static final ConcurrentHashMap<Path, ReentrantLock> JVM_LOCKS = new ConcurrentHashMap<>();
    private final List<SetupAction> managedActions;
    private final Path legacyDirectory;

    OcrSetupProvider() {
        this(null,
                OcrSetupManifest.legacyModelsDirectory(Path.of(System.getProperty("user.home"))));
    }

    OcrSetupProvider(List<SetupAction> managedActions) {
        this(managedActions, null);
    }

    OcrSetupProvider(List<SetupAction> managedActions, Path legacyDirectory) {
        this.managedActions = managedActions == null ? null : List.copyOf(managedActions);
        this.legacyDirectory = legacyDirectory == null ? null : legacyDirectory.toAbsolutePath().normalize();
    }

    @Override
    public SetupProfile profile() {
        return SetupProfile.OCR;
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return plan(options, SetupSelection.defaults(), platform, architecture);
    }

    @Override
    public SetupPlan plan(SetupOptions options, SetupSelection selection,
                          SetupPlatform platform, SetupArchitecture architecture) {
        List<SetupAction> selected = managedActions == null
                ? OcrSetupManifest.actions(SetupMode.MANAGED, selection.components()) : managedActions;
        List<SetupAction> actions = options.effectiveMode() == SetupMode.EXTERNAL
                ? selected.stream().map(action -> new SetupAction(action.target(), SetupActionKind.DIAGNOSE,
                action.version(), action.source(), action.checksum(), action.dependencyLockChecksum(), false,
                action.requiredLicenses())).toList()
                : selected;
        return SetupPlan.create(profile(), platform, architecture, options.effectiveMode(), actions);
    }

    @Override
    public SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture) {
        return status(options, SetupSelection.defaults(), platform, architecture);
    }

    @Override
    public SetupReport status(SetupOptions options, SetupSelection selection,
                              SetupPlatform platform, SetupArchitecture architecture) {
        Path directory = OcrSetupManifest.modelsDirectory(options.paths());
        List<SetupAction> selected = managedActions == null
                ? OcrSetupManifest.actions(SetupMode.MANAGED, selection.components()) : managedActions;
        List<String> failures = new ArrayList<>();
        boolean corrupt = false;
        try {
            VerifiedArtifactStore.requireUnlinkedAncestors(directory);
        } catch (IOException unsafePath) {
            SetupStatus target = new SetupStatus(SetupTarget.OCR_TESSDATA, SetupReadiness.DEGRADED, "",
                    unsafePath.getMessage());
            return new SetupReport(1, profile(), SetupReadiness.DEGRADED, List.of(target),
                    List.of(unsafePath.getMessage()));
        }
        for (SetupAction action : selected) {
            Path model = directory.resolve(fileName(action));
            try {
                if (!Files.isRegularFile(model, LinkOption.NOFOLLOW_LINKS)) failures.add(fileName(action) + " is missing");
                else if (!action.checksum().substring("sha256:".length())
                        .equalsIgnoreCase(VerifiedArtifactStore.digest(model))) {
                    failures.add(fileName(action) + " failed SHA-256 verification");
                    corrupt = true;
                }
            } catch (IOException failure) {
                failures.add(fileName(action) + " could not be verified: " + failure.getMessage());
                corrupt = true;
            }
        }
        SetupReadiness readiness = failures.isEmpty() ? SetupReadiness.READY
                : corrupt || failures.size() < selected.size() ? SetupReadiness.DEGRADED : SetupReadiness.MISSING;
        String detail = failures.isEmpty() ? "Pinned OCR baseline models are verified at " + directory
                : String.join("; ", failures);
        SetupStatus target = new SetupStatus(SetupTarget.OCR_TESSDATA, readiness,
                failures.isEmpty() ? OcrSetupManifest.TESSDATA_REVISION : "", detail);
        return new SetupReport(1, profile(), readiness, List.of(target), failures);
    }

    @Override
    public SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        SetupExecutor.validate(plan, approval);
        Path directory = OcrSetupManifest.modelsDirectory(options.paths());
        requireSafePaths(options.paths(), directory);
        VerifiedArtifactStore artifacts = new VerifiedArtifactStore(options.paths().downloads());
        List<Path> preflightArtifacts = new ArrayList<>();
        if (options.offline()) {
            for (SetupAction action : plan.actions()) {
                Path installed = directory.resolve(fileName(action));
                Path legacy = legacyFile(action);
                if (verified(installed, action)) preflightArtifacts.add(installed);
                else if (verified(legacy, action)) preflightArtifacts.add(legacy);
                else preflightArtifacts.add(artifacts.fetch(action, true));
            }
        }
        Path lockPath = options.paths().state().resolve("ocr-tessdata.lock");
        ReentrantLock jvmLock = JVM_LOCKS.computeIfAbsent(lockPath.toAbsolutePath().normalize(), ignored -> new ReentrantLock());
        jvmLock.lock();
        try {
            Files.createDirectories(lockPath.getParent());
            VerifiedArtifactStore.requireUnlinkedAncestors(lockPath);
            try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                 FileLock ignored = channel.lock()) {
                Files.createDirectories(directory);
                VerifiedArtifactStore.requireUnlinkedAncestors(directory);
                int[] actionIndex = {0};
                SetupReceipt receipt = SetupExecutor.execute(plan, approval, action -> {
                    try {
                        Path destination = directory.resolve(fileName(action));
                        Path source;
                        if (verified(destination, action)) return;
                        Path legacy = legacyFile(action);
                        if (verified(legacy, action)) source = legacy;
                        else source = options.offline()
                                ? preflightArtifacts.get(actionIndex[0]) : artifacts.fetch(action, false);
                        publish(source, destination);
                    } catch (IOException failure) {
                        throw new SetupOperationException(failure);
                    } finally {
                        actionIndex[0]++;
                    }
                });
                writeReceipt(options.paths(), receipt);
                return receipt;
            }
        } finally {
            jvmLock.unlock();
        }
    }

    private static void writeReceipt(ShaftCachePaths paths, SetupReceipt receipt) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(paths.receipts());
        Files.createDirectories(paths.receipts());
        VerifiedArtifactStore.requireUnlinkedAncestors(paths.receipts());
        Path destination = paths.receipts().resolve("ocr.json");
        Path temporary = Files.createTempFile(paths.receipts(), "ocr", ".tmp");
        try {
            Files.writeString(temporary, JSON.writerWithDefaultPrettyPrinter().writeValueAsString(receipt));
            VerifiedArtifactStore.move(temporary, destination);
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private static void publish(Path cached, Path destination) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(destination);
        String expected = destination.getFileName() + ".part";
        Path temporary = Files.createTempFile(destination.getParent(), expected, "");
        Path quarantine = destination.resolveSibling(destination.getFileName() + ".quarantine");
        try {
            Files.copy(cached, temporary, StandardCopyOption.REPLACE_EXISTING);
            VerifiedArtifactStore.replaceWithRollback(temporary, destination, quarantine);
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private static String fileName(SetupAction action) {
        String path = action.source().getPath();
        return path.substring(path.lastIndexOf('/') + 1);
    }

    private Path legacyFile(SetupAction action) {
        return legacyDirectory == null ? null : legacyDirectory.resolve(fileName(action));
    }

    private static boolean verified(Path candidate, SetupAction action) throws IOException {
        if (candidate == null) return false;
        VerifiedArtifactStore.requireUnlinkedAncestors(candidate);
        return Files.isRegularFile(candidate, LinkOption.NOFOLLOW_LINKS)
                && action.checksum().substring("sha256:".length())
                .equalsIgnoreCase(VerifiedArtifactStore.digest(candidate));
    }

    private static void requireSafePaths(ShaftCachePaths paths, Path directory) throws IOException {
        VerifiedArtifactStore.requireUnlinkedAncestors(paths.cacheRoot());
        VerifiedArtifactStore.requireUnlinkedAncestors(paths.dataRoot());
        VerifiedArtifactStore.requireUnlinkedAncestors(paths.downloads());
        VerifiedArtifactStore.requireUnlinkedAncestors(paths.state());
        VerifiedArtifactStore.requireUnlinkedAncestors(paths.receipts());
        VerifiedArtifactStore.requireUnlinkedAncestors(directory);
    }

    private static final class SetupOperationException extends RuntimeException {
        private SetupOperationException(IOException cause) { super(cause); }
    }
}
