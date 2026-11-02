package com.shaft.ai.local;

import tools.jackson.core.StreamReadFeature;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/** Cross-process ownership and immutable-cache boundary for managed local inference. */
final class ManagedLocalAiCache {
    private static final String OWNER_MANIFEST = "owner-manifest.json";
    private static final String LOCK = ".managed-local-ai.lock";
    private static final String TRANSACTION = "transaction.json";
    private static final String CLEAN_BATCH_TRANSACTION = "clean-transaction.json";
    private static final Pattern ID = Pattern.compile("[A-Za-z0-9][A-Za-z0-9._+-]{0,199}");
    private static final ObjectMapper JSON = JsonMapper.builder()
            .enable(StreamReadFeature.STRICT_DUPLICATE_DETECTION).build();

    private ManagedLocalAiCache() {
        throw new IllegalStateException("Utility class");
    }

    static <T> T withLock(Path cache, Duration timeout, CheckedSupplier<T> action) throws Exception {
        if (timeout == null || timeout.isZero() || timeout.isNegative()) {
            throw new IllegalArgumentException("Cache lock timeout must be positive.");
        }
        Path root = cache.toAbsolutePath().normalize();
        Files.createDirectories(root);
        Path lockPath = root.resolve(LOCK);
        long deadline = System.nanoTime() + timeout.toNanos();
        try (FileChannel channel = FileChannel.open(lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE)) {
            while (true) {
                FileLock lock = null;
                try {
                    lock = channel.tryLock();
                } catch (OverlappingFileLockException busy) {
                    // Another thread in this JVM holds the same process-wide lock.
                }
                if (lock != null) {
                    try (FileLock acquired = lock) {
                        recoverCleanBatch(root);
                        recover(root);
                        return action.get();
                    }
                }
                if (System.nanoTime() >= deadline) {
                    throw new IllegalStateException("Timed out waiting for the managed local AI cache lock.");
                }
                Thread.sleep(10);
            }
        }
    }

    static Installation adopt(Path cache, String id, Path stage) throws IOException {
        return adopt(cache, id, stage, null);
    }

    static Installation adopt(Path cache, String id, Path stage, List<Path> expectedFiles) throws IOException {
        validateId(id);
        Path root = cache.toAbsolutePath().normalize();
        Path source = requireVerifiedStage(root, stage);
        validateTree(source);
        List<String> expectedRelative = expectedRelativeFiles(source, expectedFiles);
        Path installations = root.resolve("installations");
        Files.createDirectories(installations);
        Path destination = installations.resolve(id + "-" + UUID.randomUUID());
        writeTransaction(root, new Transaction("ADOPT", id, relative(root, source), relative(root, destination),
                List.of()));
        moveAdoptedStage(source, destination);
        Installation installation;
        try {
            installation = inventoryAdoption(root, id, destination, expectedRelative);
            publishAdoption(root, id, installation);
            clearTransaction(root);
        } catch (IOException | RuntimeException primary) {
            rollbackAdopt(root, source, destination, primary);
            throw primary;
        }
        return installation;
    }

    private static Path requireVerifiedStage(Path root, Path stage) {
        Path source = stage.toAbsolutePath().normalize();
        if (!source.getParent().equals(root.resolve("staging"))
                || !source.getFileName().toString().contains(".extract-")
                || !Files.isDirectory(source, LinkOption.NOFOLLOW_LINKS)
                || !Files.isRegularFile(source.resolve(".shaft-ready"), LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalArgumentException("Installation stage is not a verified cache-owned stage.");
        }
        return source;
    }

    private static List<String> expectedRelativeFiles(Path source, List<Path> expectedFiles) throws IOException {
        if (expectedFiles == null) {
            return null;
        }
        List<String> expectedRelative = expectedFiles.stream().map(path -> expectedRelativeFile(source, path))
                .distinct().sorted().toList();
        requireExactStage(source, expectedRelative);
        return expectedRelative;
    }

    private static String expectedRelativeFile(Path source, Path path) {
        Path normalized = path.toAbsolutePath().normalize();
        if (!normalized.startsWith(source) || normalized.equals(source)) {
            throw new IllegalArgumentException("Expected installation file escapes its stage.");
        }
        return source.relativize(normalized).toString().replace('\\', '/');
    }

    private static void moveAdoptedStage(Path source, Path destination) throws IOException {
        try {
            Files.move(source, destination, StandardCopyOption.ATOMIC_MOVE);
        } catch (AtomicMoveNotSupportedException unsupported) {
            throw new IOException("Cache filesystem does not support atomic installation adoption.", unsupported);
        }
    }

    private static Installation inventoryAdoption(Path root, String id, Path destination,
                                                  List<String> expectedRelative) throws IOException {
        if (expectedRelative == null) {
            return inventory(root, id, destination);
        }
        return inventoryExact(root, id, destination, expectedRelative);
    }

    private static void publishAdoption(Path root, String id, Installation installation) throws IOException {
        Map<String, Installation> owned = readOwnerManifest(root);
        if (owned.containsKey(id)) {
            throw new IllegalStateException("An installation with this identifier is already owned.");
        }
        owned.put(id, installation);
        writeOwnerManifest(root, owned);
    }

    static Installation verify(Path cache, String id) throws IOException {
        validateId(id);
        Path root = cache.toAbsolutePath().normalize();
        Installation installation = readOwnerManifest(root).get(id);
        if (installation == null || !matches(root, installation, true)) {
            throw new IllegalStateException("Managed local AI installation is unowned or changed.");
        }
        return installation;
    }

    static Path verifyOwnedFile(Path cache, Path candidate) throws IOException {
        Path root = cache.toAbsolutePath().normalize();
        Path path = candidate.toAbsolutePath().normalize();
        String relative = relative(root, path);
        for (Installation installation : readOwnerManifest(root).values()) {
            if (installation.files().stream().anyMatch(file -> file.path().equals(relative))
                    && matches(root, installation, true)) {
                return path;
            }
        }
        throw new IllegalStateException("Managed local AI file is unowned or changed.");
    }

    static boolean ownsInstallation(Path cache, String id) throws IOException {
        return readOwnerManifest(cache.toAbsolutePath().normalize()).containsKey(id);
    }

    static CleanResult clean(Path cache) throws IOException {
        return clean(cache, null);
    }

    static CleanResult clean(Path cache, Set<String> installationIds) throws IOException {
        return clean(cache, installationIds, () -> { }, () -> { });
    }

    static CleanResult clean(Path cache, Set<String> installationIds, IoRunnable afterPreflight,
                             IoRunnable commitMetadata) throws IOException {
        Path root = cache.toAbsolutePath().normalize();
        Map<String, Installation> owned = readOwnerManifest(root);
        if (installationIds != null) {
            installationIds.forEach(ManagedLocalAiCache::validateId);
        }
        List<String> conflicts = new ArrayList<>();
        List<Installation> selected = owned.values().stream()
                .filter(value -> installationIds == null || installationIds.contains(value.id())).toList();
        for (Installation installation : selected) {
            if (!matches(root, installation, true)) {
                conflicts.add(installation.id());
            }
        }
        if (!conflicts.isEmpty()) {
            return new CleanResult(0, List.copyOf(conflicts));
        }
        afterPreflight.run();
        if (selected.isEmpty()) {
            commitMetadata.run();
            return new CleanResult(0, List.of());
        }
        Path trashRoot = root.resolve("trash");
        Files.createDirectories(trashRoot);
        List<CleanBatchEntry> entries = selected.stream().map(installation -> new CleanBatchEntry(installation,
                trashRoot.resolve(installation.id() + "-" + UUID.randomUUID()))).toList();
        writeCleanBatch(root, new CleanBatch("STAGING", entries), false);
        List<CleanBatchEntry> staged = new ArrayList<>();
        boolean committing = false;
        try {
            for (CleanBatchEntry entry : entries) {
                if (!matches(root, entry.installation(), true)) {
                    rollbackStagedBatch(staged);
                    clearCleanBatch(root);
                    return new CleanResult(0, List.of(entry.installation().id()));
                }
                Files.move(entry.installation().root(), entry.trash(), StandardCopyOption.ATOMIC_MOVE);
                staged.add(entry);
            }
            writeCleanBatch(root, new CleanBatch("COMMITTING", entries), true);
            committing = true;
            commitCleanBatch(root, entries, owned, commitMetadata);
        } catch (IOException | RuntimeException primary) {
            if (!committing) {
                try {
                    rollbackStagedBatch(staged);
                    clearCleanBatch(root);
                } catch (IOException | RuntimeException rollback) {
                    primary.addSuppressed(rollback);
                }
            }
            throw primary;
        }
        return new CleanResult(selected.stream().mapToInt(value -> value.files().size()).sum(), List.of());
    }

    private static void commitCleanBatch(Path root, List<CleanBatchEntry> entries,
                                         Map<String, Installation> owned, IoRunnable commitMetadata)
            throws IOException {
        commitMetadata.run();
        Map<String, Installation> remaining = new LinkedHashMap<>(owned);
        entries.forEach(entry -> remaining.remove(entry.installation().id()));
        writeOwnerManifest(root, remaining);
        for (CleanBatchEntry entry : entries) {
            deleteOwnedFromTrash(root, entry.installation().root(), entry.trash(), entry.installation().files());
        }
        clearCleanBatch(root);
    }

    private static void rollbackStagedBatch(List<CleanBatchEntry> staged) throws IOException {
        for (CleanBatchEntry entry : staged.reversed()) {
            Path source = entry.installation().root();
            if (Files.exists(entry.trash(), LinkOption.NOFOLLOW_LINKS)
                    && !Files.exists(source, LinkOption.NOFOLLOW_LINKS)) {
                Files.move(entry.trash(), source, StandardCopyOption.ATOMIC_MOVE);
            } else if (Files.exists(entry.trash(), LinkOption.NOFOLLOW_LINKS)) {
                throw new IllegalStateException("Cache batch rollback is conflicted; journal retained.");
            }
        }
    }

    private static void recoverCleanBatch(Path root) throws IOException {
        CleanBatch batch = readCleanBatch(root);
        if (batch == null) return;
        Map<String, Installation> owned = readOwnerManifest(root);
        for (CleanBatchEntry entry : batch.entries()) {
            Installation recorded = owned.get(entry.installation().id());
            if (recorded != null && !recorded.equals(entry.installation())) {
                throw new IllegalStateException("Cache batch transaction does not match owned installation.");
            }
        }
        if ("STAGING".equals(batch.phase())) {
            rollbackStagedBatch(batch.entries().stream()
                    .filter(entry -> Files.exists(entry.trash(), LinkOption.NOFOLLOW_LINKS)).toList());
            clearCleanBatch(root);
            return;
        }
        for (CleanBatchEntry entry : batch.entries()) {
            boolean sourceExists = Files.exists(entry.installation().root(), LinkOption.NOFOLLOW_LINKS);
            boolean trashExists = Files.exists(entry.trash(), LinkOption.NOFOLLOW_LINKS);
            boolean stillOwned = owned.containsKey(entry.installation().id());
            if (sourceExists && trashExists || stillOwned && (sourceExists || !trashExists)) {
                throw new IllegalStateException("Committed cache batch has an invalid staged installation.");
            }
            ManagedLocalAiActivationHistory.clearIfReferencesLocked(root, entry.installation().id());
        }
        commitCleanBatch(root, batch.entries(), owned, () -> { });
    }

    private static void clearCleanBatch(Path root) throws IOException {
        Files.deleteIfExists(root.resolve(CLEAN_BATCH_TRANSACTION));
    }

    static void recover(Path cache) throws IOException {
        Path root = cache.toAbsolutePath().normalize();
        Transaction transaction = readTransaction(root);
        if (transaction == null) {
            return;
        }
        Path source = contained(root, transaction.source());
        Path target = contained(root, transaction.target());
        Map<String, Installation> owned = readOwnerManifest(root);
        if ("ADOPT".equals(transaction.operation())) {
            requireTransactionPath(root, source, "staging", ".extract-");
            requireTransactionPath(root, target, "installations", transaction.id() + "-");
            Installation committed = owned.get(transaction.id());
            if (committed == null && Files.exists(target, LinkOption.NOFOLLOW_LINKS)
                    && !Files.exists(source, LinkOption.NOFOLLOW_LINKS)) {
                Files.move(target, source, StandardCopyOption.ATOMIC_MOVE);
            }
        } else if ("CLEAN".equals(transaction.operation())) {
            requireTransactionPath(root, source, "installations", transaction.id() + "-");
            requireTransactionPath(root, target, "trash", transaction.id() + "-");
            Installation installation = owned.get(transaction.id());
            if (installation != null && !installation.root().equals(source)) {
                throw new IllegalStateException("Cache transaction does not match owned installation.");
            }
            if (installation != null && Files.exists(source, LinkOption.NOFOLLOW_LINKS)
                    && Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
                throw new IllegalStateException("Cache recovery is conflicted; journal retained.");
            }
            if (installation != null && Files.exists(target, LinkOption.NOFOLLOW_LINKS)
                    && !Files.exists(source, LinkOption.NOFOLLOW_LINKS)) {
                Files.move(target, source, StandardCopyOption.ATOMIC_MOVE);
            } else if (installation == null) {
                if (Files.exists(source, LinkOption.NOFOLLOW_LINKS)) {
                    throw new IllegalStateException("Cache recovery source is unowned; journal retained.");
                }
                ManagedLocalAiActivationHistory.clearIfReferencesLocked(root, transaction.id());
                if (Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
                    deleteOwnedFromTrash(root, source, target, transaction.files());
                }
            }
        } else {
            throw new IllegalStateException("Cache transaction operation is invalid.");
        }
        clearTransaction(root);
    }

    private static void requireTransactionPath(Path cache, Path path, String parentName, String prefix) {
        Path expectedParent = cache.resolve(parentName);
        String name = path.getFileName().toString();
        boolean expectedName = ".extract-".equals(prefix) ? name.contains(prefix) : name.startsWith(prefix);
        if (!path.getParent().equals(expectedParent) || !expectedName) {
            throw new IllegalStateException("Cache transaction path is not valid for its operation.");
        }
    }

    private static Installation inventory(Path cache, String id, Path installationRoot) throws IOException {
        List<OwnedFile> files = new ArrayList<>();
        Files.walkFileTree(installationRoot, new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult preVisitDirectory(Path directory, BasicFileAttributes attributes) {
                requireOrdinaryDirectory(attributes);
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path path, BasicFileAttributes attributes) throws IOException {
                requireOrdinaryFile(attributes);
                files.add(new OwnedFile(relative(cache, path), Files.size(path), sha256(path)));
                return FileVisitResult.CONTINUE;
            }
        });
        files.sort(Comparator.comparing(OwnedFile::path));
        if (files.isEmpty()) {
            throw new IllegalArgumentException("Installation stage contains no owned files.");
        }
        return new Installation(id, installationRoot, List.copyOf(files));
    }

    private static Installation inventoryExact(Path cache, String id, Path installationRoot,
                                               List<String> expectedFiles) throws IOException {
        requireExactStage(installationRoot, expectedFiles);
        List<OwnedFile> files = new ArrayList<>();
        for (String relative : expectedFiles) {
            Path path = installationRoot.resolve(relative).normalize();
            ensureNoLinks(installationRoot, path);
            if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
                throw new IllegalStateException("Expected installation file is missing or changed.");
            }
            files.add(new OwnedFile(relative(cache, path), Files.size(path), sha256(path)));
        }
        files.sort(Comparator.comparing(OwnedFile::path));
        if (files.isEmpty()) {
            throw new IllegalArgumentException("Installation stage contains no owned files.");
        }
        return new Installation(id, installationRoot, List.copyOf(files));
    }

    private static void requireExactStage(Path root, List<String> expectedFiles) throws IOException {
        Set<String> expected = new HashSet<>();
        expected.add("");
        for (String relative : expectedFiles) {
            Path path = contained(root, relative);
            Path cursor = path;
            while (!cursor.equals(root)) {
                expected.add(root.relativize(cursor).toString().replace('\\', '/'));
                cursor = cursor.getParent();
            }
        }
        try (Stream<Path> paths = Files.walk(root)) {
            Set<String> actual = paths.map(path -> root.relativize(path).toString().replace('\\', '/'))
                    .collect(java.util.stream.Collectors.toSet());
            if (!actual.equals(expected)) {
                throw new IllegalStateException("Installation stage contains unknown content.");
            }
        }
    }

    private static boolean matches(Path cache, Installation installation, boolean rejectUnknown) throws IOException {
        Path root = installation.root().toAbsolutePath().normalize();
        if (!root.startsWith(cache.resolve("installations"))) {
            return false;
        }
        ensureNoLinks(cache, root);
        if (!Files.isDirectory(root, LinkOption.NOFOLLOW_LINKS)) {
            return false;
        }
        Set<String> expectedPaths = new HashSet<>();
        expectedPaths.add(relative(cache, root));
        for (OwnedFile file : installation.files()) {
            Path path = contained(cache, file.path());
            ensureNoLinks(cache, path);
            if (!path.startsWith(root) || !Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)
                    || Files.size(path) != file.size() || !sha256(path).equals(file.sha256())) {
                return false;
            }
            Path current = path;
            while (!current.equals(root)) {
                expectedPaths.add(relative(cache, current));
                current = current.getParent();
            }
        }
        if (rejectUnknown) {
            try (Stream<Path> paths = Files.walk(root)) {
                Set<String> actual = paths.map(path -> relative(cache, path))
                        .collect(java.util.stream.Collectors.toSet());
                return actual.equals(expectedPaths);
            }
        }
        return true;
    }

    private static Map<String, Installation> readOwnerManifest(Path cache) throws IOException {
        Path manifest = cache.resolve(OWNER_MANIFEST);
        Map<String, Installation> result = new LinkedHashMap<>();
        Set<String> globalPaths = new HashSet<>();
        Set<String> globalRoots = new HashSet<>();
        if (!Files.exists(manifest, LinkOption.NOFOLLOW_LINKS)) {
            return result;
        }
        if (!Files.isRegularFile(manifest, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalStateException("Owner manifest is not a regular file.");
        }
        JsonNode root = JSON.readTree(Files.newInputStream(manifest));
        if (root == null || !root.isObject() || root.path("schemaVersion").asInt(-1) != 1
                || !root.path("installations").isArray() || root.size() != 2) {
            throw new IllegalStateException("Owner manifest is invalid.");
        }
        for (JsonNode item : root.path("installations")) {
            if (!item.isObject() || item.size() != 3 || !item.path("files").isArray()) {
                throw new IllegalStateException("Owner manifest installation is invalid.");
            }
            String id = requiredText(item, "id");
            validateId(id);
            String rootPath = requiredText(item, "rootPath");
            contained(cache, rootPath);
            String portableRoot = rootPath.toLowerCase(java.util.Locale.ROOT);
            if (!globalRoots.add(portableRoot) || globalRoots.stream().anyMatch(existing ->
                    !existing.equals(portableRoot) && (existing.startsWith(portableRoot + "/")
                            || portableRoot.startsWith(existing + "/")))) {
                throw new IllegalStateException("Owner manifest contains overlapping installation roots.");
            }
            List<OwnedFile> files = new ArrayList<>();
            Set<String> filePaths = new HashSet<>();
            for (JsonNode file : item.path("files")) {
                if (!file.isObject() || file.size() != 3 || !file.path("size").isIntegralNumber()) {
                    throw new IllegalStateException("Owner manifest file entry is invalid.");
                }
                String path = requiredText(file, "path");
                contained(cache, path);
                if (!filePaths.add(path)) {
                    throw new IllegalStateException("Owner manifest contains duplicate file paths.");
                }
                if (!path.startsWith(rootPath + "/")
                        || !globalPaths.add(path.toLowerCase(java.util.Locale.ROOT))) {
                    throw new IllegalStateException("Owner manifest contains overlapping owned file paths.");
                }
                long size = file.path("size").asLong(-1);
                String digest = requiredText(file, "sha256");
                if (size < 0 || !digest.matches("[0-9a-f]{64}")) {
                    throw new IllegalStateException("Owner manifest file metadata is invalid.");
                }
                files.add(new OwnedFile(path, size, digest));
            }
            if (files.isEmpty() || result.putIfAbsent(id,
                    new Installation(id, contained(cache, rootPath), List.copyOf(files))) != null) {
                throw new IllegalStateException("Owner manifest contains an empty or duplicate installation.");
            }
        }
        return result;
    }

    private static void writeOwnerManifest(Path cache, Map<String, Installation> installations) throws IOException {
        Files.createDirectories(cache);
        ObjectNode root = JSON.createObjectNode();
        root.put("schemaVersion", 1);
        ArrayNode values = root.putArray("installations");
        installations.values().stream().sorted(Comparator.comparing(Installation::id)).forEach(installation -> {
            ObjectNode item = values.addObject();
            item.put("id", installation.id());
            item.put("rootPath", relative(cache, installation.root()));
            ArrayNode files = item.putArray("files");
            installation.files().forEach(file -> {
                ObjectNode entry = files.addObject();
                entry.put("path", file.path());
                entry.put("size", file.size());
                entry.put("sha256", file.sha256());
            });
        });
        Path stage = cache.resolve(OWNER_MANIFEST + ".stage-" + UUID.randomUUID());
        try {
            Files.write(stage, JSON.writeValueAsBytes(root), StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
            try (FileChannel channel = FileChannel.open(stage, StandardOpenOption.WRITE)) {
                channel.force(true);
            }
            Files.move(stage, cache.resolve(OWNER_MANIFEST), StandardCopyOption.ATOMIC_MOVE,
                    StandardCopyOption.REPLACE_EXISTING);
        } catch (AtomicMoveNotSupportedException unsupported) {
            throw new IOException("Cache filesystem does not support atomic ownership publication.", unsupported);
        } finally {
            Files.deleteIfExists(stage);
        }
    }

    private static void writeTransaction(Path cache, Transaction transaction) throws IOException {
        ObjectNode value = JSON.createObjectNode();
        value.put("schemaVersion", 1);
        value.put("operation", transaction.operation());
        value.put("id", transaction.id());
        value.put("source", transaction.source());
        value.put("target", transaction.target());
        ArrayNode files = value.putArray("files");
        transaction.files().forEach(file -> {
            ObjectNode entry = files.addObject();
            entry.put("path", file.path());
            entry.put("size", file.size());
            entry.put("sha256", file.sha256());
        });
        Path target = cache.resolve(TRANSACTION);
        if (Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalStateException("An unfinished cache transaction already exists.");
        }
        Path stage = cache.resolve(TRANSACTION + ".stage-" + UUID.randomUUID());
        try {
            Files.write(stage, JSON.writeValueAsBytes(value), StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
            try (FileChannel channel = FileChannel.open(stage, StandardOpenOption.WRITE)) {
                channel.force(true);
            }
            Files.createLink(target, stage);
            Files.delete(stage);
        } finally {
            Files.deleteIfExists(stage);
        }
    }

    private static void writeCleanBatch(Path root, CleanBatch batch, boolean replace) throws IOException {
        ObjectNode value = JSON.createObjectNode();
        value.put("schemaVersion", 1);
        value.put("phase", batch.phase());
        ArrayNode items = value.putArray("entries");
        for (CleanBatchEntry batchEntry : batch.entries()) {
            Installation installation = batchEntry.installation();
            ObjectNode entry = items.addObject();
            entry.put("id", installation.id());
            entry.put("source", relative(root, installation.root()));
            entry.put("target", relative(root, batchEntry.trash()));
            ArrayNode files = entry.putArray("files");
            installation.files().forEach(file -> {
                ObjectNode ownedFile = files.addObject();
                ownedFile.put("path", file.path());
                ownedFile.put("size", file.size());
                ownedFile.put("sha256", file.sha256());
            });
        }
        Path target = root.resolve(CLEAN_BATCH_TRANSACTION);
        if (!replace && Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalStateException("An unfinished cache clean transaction already exists.");
        }
        Path stage = root.resolve(CLEAN_BATCH_TRANSACTION + ".stage-" + UUID.randomUUID());
        try {
            Files.write(stage, JSON.writeValueAsBytes(value), StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
            try (FileChannel channel = FileChannel.open(stage, StandardOpenOption.WRITE)) {
                channel.force(true);
            }
            if (replace) {
                Files.move(stage, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
            } else {
                Files.move(stage, target, StandardCopyOption.ATOMIC_MOVE);
            }
        } finally {
            Files.deleteIfExists(stage);
        }
    }

    private static CleanBatch readCleanBatch(Path root) throws IOException {
        Path path = root.resolve(CLEAN_BATCH_TRANSACTION);
        if (!Files.exists(path, LinkOption.NOFOLLOW_LINKS)) return null;
        if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalStateException("Cache clean transaction journal is not a regular file.");
        }
        JsonNode value = JSON.readTree(Files.newInputStream(path));
        if (value == null || !value.isObject() || value.size() != 3
                || value.path("schemaVersion").asInt(-1) != 1 || !value.path("entries").isArray()) {
            throw new IllegalStateException("Cache clean transaction journal is invalid.");
        }
        String phase = requiredText(value, "phase");
        if (!Set.of("STAGING", "COMMITTING").contains(phase)) {
            throw new IllegalStateException("Cache clean transaction phase is invalid.");
        }
        List<CleanBatchEntry> entries = new ArrayList<>();
        for (JsonNode item : value.path("entries")) {
            if (!item.isObject() || item.size() != 4 || !item.path("files").isArray()) {
                throw new IllegalStateException("Cache clean transaction entry is invalid.");
            }
            String id = requiredText(item, "id");
            validateId(id);
            Path source = contained(root, requiredText(item, "source"));
            Path target = contained(root, requiredText(item, "target"));
            requireTransactionPath(root, source, "installations", id + "-");
            requireTransactionPath(root, target, "trash", id + "-");
            List<OwnedFile> files = new ArrayList<>();
            for (JsonNode file : item.path("files")) {
                if (!file.isObject() || file.size() != 3 || !file.path("size").isIntegralNumber()) {
                    throw new IllegalStateException("Cache clean transaction file is invalid.");
                }
                String filePath = requiredText(file, "path");
                contained(root, filePath);
                long size = file.path("size").asLong(-1);
                String digest = requiredText(file, "sha256");
                if (size < 0 || !digest.matches("[0-9a-f]{64}")) {
                    throw new IllegalStateException("Cache clean transaction file metadata is invalid.");
                }
                files.add(new OwnedFile(filePath, size, digest));
            }
            if (files.isEmpty()) {
                throw new IllegalStateException("Cache clean transaction inventory is empty.");
            }
            entries.add(new CleanBatchEntry(new Installation(id, source, List.copyOf(files)), target));
        }
        if (entries.isEmpty() || entries.stream().map(entry -> entry.installation().id()).distinct().count()
                != entries.size()) {
            throw new IllegalStateException("Cache clean transaction inventory is invalid.");
        }
        return new CleanBatch(phase, List.copyOf(entries));
    }

    private static Transaction readTransaction(Path cache) throws IOException {
        Path path = cache.resolve(TRANSACTION);
        if (!Files.exists(path, LinkOption.NOFOLLOW_LINKS)) {
            return null;
        }
        if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalStateException("Cache transaction journal is not a regular file.");
        }
        JsonNode value = JSON.readTree(Files.newInputStream(path));
        if (value == null || !value.isObject() || value.size() != 6
                || value.path("schemaVersion").asInt(-1) != 1 || !value.path("files").isArray()) {
            throw new IllegalStateException("Cache transaction journal is invalid.");
        }
        String operation = requiredText(value, "operation");
        String id = requiredText(value, "id");
        validateId(id);
        String source = requiredText(value, "source");
        String target = requiredText(value, "target");
        contained(cache, source);
        contained(cache, target);
        List<OwnedFile> files = new ArrayList<>();
        for (JsonNode file : value.path("files")) {
            if (!file.isObject() || file.size() != 3 || !file.path("size").isIntegralNumber()) {
                throw new IllegalStateException("Cache transaction file entry is invalid.");
            }
            String filePath = requiredText(file, "path");
            contained(cache, filePath);
            long size = file.path("size").asLong(-1);
            String digest = requiredText(file, "sha256");
            if (size < 0 || !digest.matches("[0-9a-f]{64}")) {
                throw new IllegalStateException("Cache transaction file metadata is invalid.");
            }
            files.add(new OwnedFile(filePath, size, digest));
        }
        if ("CLEAN".equals(operation) && files.isEmpty() || "ADOPT".equals(operation) && !files.isEmpty()) {
            throw new IllegalStateException("Cache transaction file inventory is invalid.");
        }
        return new Transaction(operation, id, source, target, List.copyOf(files));
    }

    private static void clearTransaction(Path cache) throws IOException {
        Files.deleteIfExists(cache.resolve(TRANSACTION));
    }

    private static void rollbackAdopt(Path cache, Path source, Path target, Throwable primary) {
        try {
            Installation committed = readOwnerManifest(cache).values().stream()
                    .filter(value -> value.root().equals(target)).findFirst().orElse(null);
            if (committed != null) {
                return;
            }
            if (Files.exists(target, LinkOption.NOFOLLOW_LINKS) && !Files.exists(source, LinkOption.NOFOLLOW_LINKS)) {
                Files.move(target, source, StandardCopyOption.ATOMIC_MOVE);
            }
            clearTransaction(cache);
        } catch (IOException rollback) {
            primary.addSuppressed(rollback);
        }
    }

    static void rollbackClean(Path cache, Installation installation, Path trash, Throwable primary) {
        try {
            Path source = installation.root();
            if (Files.exists(trash, LinkOption.NOFOLLOW_LINKS)
                    && !Files.exists(source, LinkOption.NOFOLLOW_LINKS)) {
                Files.move(trash, source, StandardCopyOption.ATOMIC_MOVE);
            }
            if (Files.exists(source, LinkOption.NOFOLLOW_LINKS)
                    && !Files.exists(trash, LinkOption.NOFOLLOW_LINKS)) {
                Map<String, Installation> owned = readOwnerManifest(cache);
                Installation existing = owned.get(installation.id());
                if (existing != null && !existing.equals(installation)) {
                    throw new IllegalStateException("Cache clean rollback conflicts with the owner manifest.");
                }
                if (existing == null) {
                    Map<String, Installation> restored = new LinkedHashMap<>(owned);
                    restored.put(installation.id(), installation);
                    writeOwnerManifest(cache, restored);
                }
                clearTransaction(cache);
            }
        } catch (IOException | RuntimeException rollback) {
            primary.addSuppressed(rollback);
        }
    }

    private static void validateTree(Path root) throws IOException {
        Files.walkFileTree(root, new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult preVisitDirectory(Path directory, BasicFileAttributes attributes) {
                requireOrdinaryDirectory(attributes);
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attributes) {
                requireOrdinaryFile(attributes);
                return FileVisitResult.CONTINUE;
            }
        });
    }

    private static void ensureNoLinks(Path cache, Path target) throws IOException {
        Path current = cache;
        for (Path part : cache.relativize(target)) {
            current = current.resolve(part);
            BasicFileAttributes attributes = Files.readAttributes(current, BasicFileAttributes.class,
                    LinkOption.NOFOLLOW_LINKS);
            if (attributes.isSymbolicLink() || attributes.isOther()) {
                throw new IllegalStateException("Owned path contains a link or reparse point.");
            }
        }
    }

    private static void requireOrdinaryDirectory(BasicFileAttributes attributes) {
        if (!attributes.isDirectory() || attributes.isSymbolicLink() || attributes.isOther()) {
            throw new IllegalArgumentException("Installation stage contains an unsupported directory type.");
        }
    }

    private static void requireOrdinaryFile(BasicFileAttributes attributes) {
        if (!attributes.isRegularFile() || attributes.isSymbolicLink() || attributes.isOther()) {
            throw new IllegalArgumentException("Installation stage contains an unsupported file type.");
        }
    }

    private static void removeOwnedEmptyDirectories(Path installationRoot, Path cache) throws IOException {
        if (!installationRoot.startsWith(cache.resolve("installations"))) {
            throw new IllegalStateException("Owned installation root escapes the cache.");
        }
        try (Stream<Path> paths = Files.walk(installationRoot)) {
            for (Path path : paths.sorted(Comparator.reverseOrder()).toList()) {
                try {
                    Files.delete(path);
                } catch (java.nio.file.DirectoryNotEmptyException unknownContent) {
                    // Unknown content is never removed.
                }
            }
        }
    }

    private static void cleanupExactTree(Path root, Throwable primary) {
        try {
            deleteExactTree(root);
        } catch (IOException cleanup) {
            primary.addSuppressed(cleanup);
        }
    }

    private static void deleteExactTree(Path root) throws IOException {
        if (!Files.exists(root, LinkOption.NOFOLLOW_LINKS)) {
            return;
        }
        try (Stream<Path> paths = Files.walk(root)) {
            for (Path path : paths.sorted(Comparator.reverseOrder()).toList()) {
                Files.deleteIfExists(path);
            }
        }
    }

    private static void deleteOwnedFromTrash(Path cache, Path originalRoot, Path trash,
                                             List<OwnedFile> files) throws IOException {
        List<Path> directories = new ArrayList<>();
        for (OwnedFile file : files) {
            Path original = contained(cache, file.path());
            Path suffix = originalRoot.relativize(original);
            Path target = trash.resolve(suffix).normalize();
            if (!target.startsWith(trash)) {
                throw new IllegalStateException("Owned cleanup path escapes trash.");
            }
            if (!Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
                continue;
            }
            ensureNoLinks(cache, target);
            if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS)
                    || Files.size(target) != file.size() || !sha256(target).equals(file.sha256())) {
                throw new IllegalStateException("Owned file changed during cleanup; refusing deletion.");
            }
            Files.delete(target);
            Path parent = target.getParent();
            while (parent != null && parent.startsWith(trash)) {
                directories.add(parent);
                if (parent.equals(trash)) {
                    break;
                }
                parent = parent.getParent();
            }
        }
        directories.stream().distinct().sorted(Comparator.reverseOrder()).forEach(directory -> {
            try {
                Files.delete(directory);
            } catch (java.nio.file.DirectoryNotEmptyException unknown) {
                // Preserve concurrently introduced unknown content.
            } catch (IOException failure) {
                throw new java.io.UncheckedIOException(failure);
            }
        });
        if (Files.exists(trash, LinkOption.NOFOLLOW_LINKS)) {
            Files.move(trash, originalRoot, StandardCopyOption.ATOMIC_MOVE);
        }
    }

    private static Path contained(Path cache, String relative) {
        if (relative == null || relative.isBlank() || relative.contains("\\") || relative.contains(":")
                || relative.startsWith("/") || relative.endsWith("/")
                || Stream.of(relative.split("/", -1)).anyMatch(part -> part.isBlank() || ".".equals(part)
                || "..".equals(part))) {
            throw new IllegalStateException("Owned path is not a portable relative path.");
        }
        Path value = Path.of(relative);
        if (value.isAbsolute()) {
            throw new IllegalStateException("Owned path must be relative.");
        }
        Path target = cache.resolve(value).normalize();
        if (!target.startsWith(cache) || target.equals(cache)) {
            throw new IllegalStateException("Owned path escapes the cache.");
        }
        return target;
    }

    private static String relative(Path cache, Path path) {
        Path normalized = path.toAbsolutePath().normalize();
        if (!normalized.startsWith(cache)) {
            throw new IllegalArgumentException("Path escapes managed local AI cache.");
        }
        return cache.relativize(normalized).toString().replace('\\', '/');
    }

    private static String requiredText(JsonNode node, String field) {
        JsonNode value = node.get(field);
        if (value == null || !value.isTextual() || value.asText().isBlank()) {
            throw new IllegalStateException("Owner manifest field is invalid: " + field + ".");
        }
        return value.asText();
    }

    private static void validateId(String id) {
        if (id == null || !ID.matcher(id).matches()) {
            throw new IllegalArgumentException("Installation identifier is not portable.");
        }
    }

    private static String sha256(Path path) throws IOException {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            try (var input = Files.newInputStream(path)) {
                byte[] buffer = new byte[64 * 1024];
                int count;
                while ((count = input.read(buffer)) != -1) {
                    digest.update(buffer, 0, count);
                }
            }
            return HexFormat.of().formatHex(digest.digest());
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("JDK SHA-256 support is unavailable.", impossible);
        }
    }

    @FunctionalInterface
    interface CheckedSupplier<T> {
        T get() throws Exception;
    }

    @FunctionalInterface
    interface IoRunnable {
        void run() throws IOException;
    }

    record Installation(String id, Path root, List<OwnedFile> files) {
    }

    record OwnedFile(String path, long size, String sha256) {
    }

    record CleanResult(int deletedFiles, List<String> conflicts) {
    }

    record Transaction(String operation, String id, String source, String target, List<OwnedFile> files) {
    }

    record CleanBatch(String phase, List<CleanBatchEntry> entries) {
    }

    record CleanBatchEntry(Installation installation, Path trash) {
    }
}
