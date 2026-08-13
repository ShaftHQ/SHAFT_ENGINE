package com.shaft.intellij.actions;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.ide.BrowserUtil;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.shaft.intellij.notifications.ShaftNotifier;
import com.shaft.intellij.project.ShaftProjectDetector;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.nio.file.attribute.PosixFilePermissions;
import java.nio.file.attribute.AclEntry;
import java.nio.file.attribute.AclEntryPermission;
import java.nio.file.attribute.AclEntryType;
import java.nio.file.attribute.AclEntryFlag;
import java.nio.file.attribute.AclFileAttributeView;
import java.text.Normalizer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.EnumSet;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Opens the offline SHAFT trace viewer HTML for the most recently generated {@code
 * target/shaft-traces} trace in the user's default browser.
 * <p>
 * Uses an external-browser fallback ({@link BrowserUtil#browse}) rather than an embedded JCEF
 * panel: the plugin has no existing JCEF dependency, and plugin tests run headless, so an
 * embedded panel would be unverifiable by automated tests.
 */
public final class ShowTraceViewerAction extends AnAction implements DumbAware {
    private static final String VIEWER_HTML_NAME = "SHAFT Trace Report.html";
    private static final String TRACE_ZIP_NAME = "shaft-trace.zip";
    private static final String NOTIFICATION_TITLE = "Trace viewer";
    private static final long MAX_ARCHIVE_BYTES = 128L * 1024 * 1024;
    private static final long MAX_UNCOMPRESSED_BYTES = 128L * 1024 * 1024;
    private static final long MAX_VIEWER_BYTES = 64L * 1024 * 1024;
    private static final int MAX_ARCHIVE_ENTRIES = 10_000;
    private static final int MAX_GENERATED_VIEWERS = 4;
    private static final long MAX_GENERATED_VIEWER_BYTES = MAX_GENERATED_VIEWERS * MAX_VIEWER_BYTES;
    private static final int CACHE_SLOT_COUNT = 8;
    private static final GeneratedViewerCache GENERATED_VIEWER_CACHE = createGeneratedViewerCache();

    @Override
    public void actionPerformed(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        if (project == null) {
            return;
        }
        String basePath = project.getBasePath();
        Path viewer;
        try {
            viewer = basePath == null ? null : resolveLatestTraceViewer(Path.of(basePath));
        } catch (IOException e) {
            ShaftNotifier.warn(project, NOTIFICATION_TITLE, "Could not resolve a SHAFT trace viewer: " + e.getMessage());
            return;
        }
        if (viewer == null) {
            ShaftNotifier.warn(project, NOTIFICATION_TITLE, "No SHAFT trace was found under target/shaft-traces.");
            return;
        }
        BrowserUtil.browse(viewer.toUri());
    }

    @Override
    public void update(@NotNull AnActionEvent event) {
        Project project = event.getProject();
        event.getPresentation().setEnabledAndVisible(project != null && ShaftProjectDetector.isShaftProject(project));
    }

    /**
     * Resolves the offline viewer HTML for the most recently generated trace under
     * {@code <projectRoot>/target/shaft-traces}, extracting it from {@code shaft-trace.zip} when
     * no loose copy exists on disk. Returns {@code null} when no trace can be found. Pure
     * filesystem logic with no IDE dependency, so it is unit-testable headless.
     *
     * @param projectRoot project base directory
     * @return resolved viewer HTML path, or {@code null} when no trace exists
     */
    static Path resolveLatestTraceViewer(Path projectRoot) throws IOException {
        return resolveLatestTraceViewer(projectRoot, GENERATED_VIEWER_CACHE);
    }

    static Path resolveLatestTraceViewer(Path projectRoot, GeneratedViewerCache cache) throws IOException {
        Path traceRoot = projectRoot.resolve("target").resolve("shaft-traces");
        if (!Files.isDirectory(traceRoot)) {
            return null;
        }
        Path newestDirectory = null;
        Instant newestGeneratedAt = null;
        try (Stream<Path> paths = Files.walk(traceRoot, 2)) {
            List<Path> indexFiles = paths
                    .filter(path -> "index.json".equals(path.getFileName().toString()))
                    .toList();
            for (Path indexPath : indexFiles) {
                Instant generatedAt = generatedAt(indexPath);
                if (newestGeneratedAt == null || generatedAt.isAfter(newestGeneratedAt)) {
                    newestGeneratedAt = generatedAt;
                    newestDirectory = indexPath.getParent();
                }
            }
        }
        return newestDirectory == null ? null : resolveViewerHtml(newestDirectory, cache);
    }

    private static Instant generatedAt(Path indexPath) {
        try {
            String content = Files.readString(indexPath);
            JsonObject index = JsonParser.parseString(content).getAsJsonObject();
            if (index.has("generatedAt")) {
                return Instant.parse(index.get("generatedAt").getAsString());
            }
        } catch (RuntimeException | IOException ignored) {
            // Fall through to the file's modified time; a malformed index must never break discovery.
        }
        try {
            FileTime modified = Files.getLastModifiedTime(indexPath);
            return modified.toInstant();
        } catch (IOException e) {
            return Instant.EPOCH;
        }
    }

    @SuppressWarnings("PMD.NPathComplexity")
    private static Path resolveViewerHtml(Path traceDirectory, GeneratedViewerCache cache) throws IOException {
        Path archive = traceDirectory.resolve(TRACE_ZIP_NAME);
        Path loose = traceDirectory.resolve(VIEWER_HTML_NAME);
        if (Files.isRegularFile(loose)) {
            return loose;
        }
        if (!Files.isRegularFile(archive)) {
            return null;
        }
        synchronized (cache) {
        Path archiveCopy = cache.createTemporary(".shaft-trace-archive-");
        Path viewerCopy = null;
        try {
            BasicFileAttributes before = Files.readAttributes(archive, BasicFileAttributes.class);
            copyArchive(archive, archiveCopy);
            verifyCentralDirectoryEntryCount(archiveCopy);
            BasicFileAttributes after = Files.readAttributes(archive, BasicFileAttributes.class);
            if (!sameFileGeneration(before, after)) {
                throw new IOException("Trace archive changed while it was being read.");
            }
            Set<String> names = new HashSet<>();
            int entryCount = 0;
            long totalUncompressed = 0;
            try (ZipFile zip = new ZipFile(archiveCopy.toFile())) {
            var entries = zip.entries();
            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                if (++entryCount > MAX_ARCHIVE_ENTRIES) {
                    throw new IOException("Trace archive contains too many entries.");
                }
                String identity = safeEntryIdentity(entry);
                if (!names.add(identity)) {
                    throw new IOException("Trace archive contains a conflicting entry: " + entry.getName());
                }
                long size = entry.getSize();
                if (size < 0 || size > MAX_UNCOMPRESSED_BYTES - totalUncompressed) {
                    throw new IOException("Trace archive exceeds the uncompressed-size limit.");
                }
                totalUncompressed += size;
                if (VIEWER_HTML_NAME.equals(entry.getName())) {
                    if (entry.isDirectory() || viewerCopy != null) {
                        throw new IOException("Trace archive contains an invalid viewer entry.");
                    }
                    viewerCopy = cache.createTemporary(".shaft-trace-viewer-");
                    try (InputStream input = zip.getInputStream(entry)) {
                        copyViewer(input, entry, viewerCopy);
                    }
                }
            }
            if (viewerCopy == null) {
                return null;
            }
            BasicFileAttributes latest = Files.readAttributes(archive, BasicFileAttributes.class);
            if (!sameFileGeneration(before, latest)) {
                throw new IOException("Trace archive changed while it was being read.");
            }
            Path immutableViewer = cache.publish(viewerCopy);
            viewerCopy = null;
            return immutableViewer;
            }
        } finally {
            if (viewerCopy != null) {
                Files.deleteIfExists(viewerCopy);
            }
            Files.deleteIfExists(archiveCopy);
        }
        }
    }

    @SuppressWarnings("PMD.NPathComplexity")
    private static String safeEntryIdentity(ZipEntry entry) throws IOException {
        String name = entry.getName();
        if (name == null || name.isBlank() || name.startsWith("/") || name.contains("\\") || name.contains(":")) {
            throw new IOException("Trace archive contains an unsafe entry name.");
        }
        String candidate = entry.isDirectory() && name.endsWith("/") ? name.substring(0, name.length() - 1) : name;
        if (candidate.isBlank()) {
            throw new IOException("Trace archive contains an unsafe entry name.");
        }
        String[] segments = candidate.split("/", -1);
        for (String segment : segments) {
            if (segment.isEmpty() || ".".equals(segment) || "..".equals(segment)
                    || segment.endsWith(".") || segment.endsWith(" ")) {
                throw new IOException("Trace archive contains a non-canonical entry name: " + name);
            }
            String base = segment.split("\\.", 2)[0].toUpperCase(Locale.ROOT);
            if (base.equals("CON") || base.equals("PRN") || base.equals("AUX") || base.equals("NUL")
                    || base.matches("COM[1-9]") || base.matches("LPT[1-9]")) {
                throw new IOException("Trace archive contains a non-portable entry name: " + name);
            }
        }
        return Normalizer.normalize(candidate, Normalizer.Form.NFC).toLowerCase(Locale.ROOT);
    }

    @SuppressWarnings("PMD.NPathComplexity")
    static void verifyCentralDirectoryEntryCount(Path archive) throws IOException {
        long size = Files.size(archive);
        int tailSize = (int) Math.min(size, 65_557L);
        byte[] tail = new byte[tailSize];
        try (var file = new java.io.RandomAccessFile(archive.toFile(), "r")) {
            file.seek(size - tailSize);
            file.readFully(tail);
        }
        for (int offset = tail.length - 22; offset >= 0; offset--) {
            if ((tail[offset] & 0xff) == 0x50 && (tail[offset + 1] & 0xff) == 0x4b
                    && (tail[offset + 2] & 0xff) == 0x05 && (tail[offset + 3] & 0xff) == 0x06) {
                int commentLength = unsignedShort(tail, offset + 20);
                if (offset + 22 + commentLength != tail.length) {
                    continue;
                }
                int disk = unsignedShort(tail, offset + 4);
                int centralDirectoryDisk = unsignedShort(tail, offset + 6);
                int diskEntries = unsignedShort(tail, offset + 8);
                int entries = unsignedShort(tail, offset + 10);
                long centralDirectorySize = unsignedInt(tail, offset + 12);
                long centralDirectoryOffset = unsignedInt(tail, offset + 16);
                long eocdOffset = size - tail.length + offset;
                if (disk != 0 || centralDirectoryDisk != 0 || diskEntries != entries
                        || entries == 0xffff || centralDirectorySize == 0xffffffffL
                        || centralDirectoryOffset == 0xffffffffL) {
                    throw new IOException("ZIP64 trace archives are not supported.");
                }
                if (centralDirectoryOffset + centralDirectorySize != eocdOffset) {
                    continue;
                }
                int actualEntries = countCentralDirectoryEntries(
                        archive, centralDirectoryOffset, centralDirectorySize, eocdOffset);
                if (actualEntries != entries) {
                    throw new IOException("Trace archive central-directory count is inconsistent.");
                }
                if (entries > MAX_ARCHIVE_ENTRIES) {
                    throw new IOException("Trace archive contains too many entries.");
                }
                return;
            }
        }
        throw new IOException("Trace archive has no valid central-directory terminator.");
    }

    private static int countCentralDirectoryEntries(
            Path archive, long centralDirectoryOffset, long centralDirectorySize, long eocdOffset) throws IOException {
        long position = centralDirectoryOffset;
        long end = centralDirectoryOffset + centralDirectorySize;
        int count = 0;
        byte[] header = new byte[46];
        try (var file = new java.io.RandomAccessFile(archive.toFile(), "r")) {
            while (position < end) {
                if (++count > MAX_ARCHIVE_ENTRIES || end - position < header.length) {
                    throw new IOException("Trace archive contains too many or malformed central-directory entries.");
                }
                file.seek(position);
                file.readFully(header);
                if (unsignedInt(header, 0) != 0x02014b50L) {
                    throw new IOException("Trace archive contains a malformed central-directory entry.");
                }
                long recordSize = 46L
                        + unsignedShort(header, 28)
                        + unsignedShort(header, 30)
                        + unsignedShort(header, 32);
                if (recordSize > end - position) {
                    throw new IOException("Trace archive contains a truncated central-directory entry.");
                }
                position += recordSize;
            }
        }
        if (position != end || end != eocdOffset) {
            throw new IOException("Trace archive has invalid central-directory bounds.");
        }
        return count;
    }

    private static int unsignedShort(byte[] bytes, int offset) {
        return (bytes[offset] & 0xff) | ((bytes[offset + 1] & 0xff) << 8);
    }

    private static long unsignedInt(byte[] bytes, int offset) {
        return (bytes[offset] & 0xffL)
                | ((bytes[offset + 1] & 0xffL) << 8)
                | ((bytes[offset + 2] & 0xffL) << 16)
                | ((bytes[offset + 3] & 0xffL) << 24);
    }

    static void copyArchive(Path archive, Path destination) throws IOException {
        byte[] buffer = new byte[8192];
        long total = 0;
        try (InputStream input = Files.newInputStream(archive); OutputStream output = Files.newOutputStream(destination)) {
            int read;
            while ((read = input.read(buffer)) != -1) {
                total += read;
                if (total > MAX_ARCHIVE_BYTES) {
                    throw new IOException("Trace archive exceeds the compressed-size limit.");
                }
                output.write(buffer, 0, read);
            }
        }
    }

    private static boolean sameFileGeneration(BasicFileAttributes left, BasicFileAttributes right) {
        return left.size() == right.size()
                && left.lastModifiedTime().equals(right.lastModifiedTime())
                && (left.fileKey() == null || left.fileKey().equals(right.fileKey()));
    }

    private static String sha256(Path path) throws IOException {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] buffer = new byte[8192];
            try (InputStream input = Files.newInputStream(path)) {
                int read;
                while ((read = input.read(buffer)) != -1) {
                    digest.update(buffer, 0, read);
                }
            }
            return java.util.HexFormat.of().formatHex(digest.digest());
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java runtime.", impossible);
        }
    }

    private static void verifyExistingViewer(Path expected, Path existing) throws IOException {
        if (!Files.isRegularFile(existing) || Files.isSymbolicLink(existing) || Files.mismatch(expected, existing) != -1) {
            throw new IOException("Generated trace viewer cache contains conflicting content.");
        }
    }

    static GeneratedViewerCache generatedViewerCacheForTests() throws IOException {
        return GeneratedViewerCache.create();
    }

    private static GeneratedViewerCache createGeneratedViewerCache() {
        try {
            return GeneratedViewerCache.create();
        } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private static void copyViewer(InputStream input, ZipEntry entry, Path destination) throws IOException {
        if (entry.getSize() > MAX_VIEWER_BYTES) {
            throw new IOException("Trace viewer exceeds the extraction limit.");
        }
        byte[] buffer = new byte[8192];
        long total = 0;
        try (OutputStream output = Files.newOutputStream(destination)) {
            int read;
            while ((read = input.read(buffer)) != -1) {
                total += read;
                if (total > MAX_VIEWER_BYTES) {
                    throw new IOException("Trace viewer exceeds the extraction limit.");
                }
                output.write(buffer, 0, read);
            }
        }
    }

    static final class GeneratedViewerCache implements AutoCloseable {
        private final Path directory;
        private final Path realDirectory;
        private final Object directoryKey;
        private final FileTime directoryCreated;
        private final Object lockKey;
        private final FileTime lockCreated;
        @SuppressWarnings("unused")
        private final FileChannel lockChannel;
        @SuppressWarnings("unused")
        private final FileLock processLock;

        private GeneratedViewerCache(Path directory, FileChannel lockChannel, FileLock processLock) throws IOException {
            this.directory = directory;
            this.realDirectory = directory.toRealPath(java.nio.file.LinkOption.NOFOLLOW_LINKS);
            BasicFileAttributes directoryAttributes = attributes();
            this.directoryKey = directoryAttributes.fileKey();
            this.directoryCreated = directoryAttributes.creationTime();
            BasicFileAttributes lockAttributes = Files.readAttributes(
                    directory.resolve(".owner.lock"), BasicFileAttributes.class,
                    java.nio.file.LinkOption.NOFOLLOW_LINKS);
            this.lockKey = lockAttributes.fileKey();
            this.lockCreated = lockAttributes.creationTime();
            this.lockChannel = lockChannel;
            this.processLock = processLock;
        }

        static GeneratedViewerCache create() throws IOException {
            return create(Path.of(System.getProperty("java.io.tmpdir")));
        }

        static GeneratedViewerCache create(Path tempRoot) throws IOException {
            Path parent = privateCacheParent(tempRoot);
            int start = java.util.concurrent.ThreadLocalRandom.current().nextInt(CACHE_SLOT_COUNT);
            for (int offset = 0; offset < CACHE_SLOT_COUNT; offset++) {
                Path directory = parent.resolve("slot-" + ((start + offset) % CACHE_SLOT_COUNT));
                createPrivateDirectory(directory);
                Path lockPath = directory.resolve(".owner.lock");
                FileChannel channel = FileChannel.open(
                        lockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                FileLock lock;
                try {
                    lock = channel.tryLock();
                } catch (java.nio.channels.OverlappingFileLockException active) {
                    channel.close();
                    continue;
                }
                if (lock == null) {
                    channel.close();
                    continue;
                }
                try {
                    cleanReusableSlot(directory, lockPath);
                    return new GeneratedViewerCache(directory, channel, lock);
                } catch (IOException | RuntimeException failure) {
                    lock.close();
                    channel.close();
                    throw failure;
                }
            }
            throw new IOException("All generated trace viewer cache slots are active.");
        }

        synchronized Path createTemporary(String prefix) throws IOException {
            validateDirectory();
            return Files.createTempFile(directory, prefix, ".tmp");
        }

        @SuppressWarnings("PMD.NPathComplexity")
        synchronized Path publish(Path candidate) throws IOException {
            validateDirectory();
            Path normalizedCandidate = candidate.toAbsolutePath().normalize();
            if (!directory.toAbsolutePath().normalize().equals(normalizedCandidate.getParent())
                    || !Files.isRegularFile(candidate) || Files.isSymbolicLink(candidate)) {
                throw new IOException("Generated trace viewer candidate is outside the owned cache.");
            }
            Object candidateKey = Files.readAttributes(
                            candidate, BasicFileAttributes.class, java.nio.file.LinkOption.NOFOLLOW_LINKS)
                    .fileKey();
            FileTime candidateCreated = Files.readAttributes(
                            candidate, BasicFileAttributes.class, java.nio.file.LinkOption.NOFOLLOW_LINKS)
                    .creationTime();
            try {
                String generation = sha256(candidate);
                Path immutableViewer = directory.resolve(".shaft-trace-viewer-" + generation + ".html");
                List<Path> generated;
                try (Stream<Path> files = Files.list(directory)) {
                    generated = files.filter(path -> path.getFileName().toString()
                                    .matches("\\.shaft-trace-viewer-[0-9a-f]{64}\\.html"))
                            .toList();
                }
                long bytes = Files.size(candidate);
                for (Path path : generated) {
                    verifyOwned(path);
                    if (!path.equals(immutableViewer)) {
                        bytes = Math.addExact(bytes, Files.size(path));
                    }
                }
                if (generated.stream().noneMatch(immutableViewer::equals)
                        && (generated.size() >= MAX_GENERATED_VIEWERS || bytes > MAX_GENERATED_VIEWER_BYTES)) {
                    throw new IOException("Generated trace viewer cache is at its protected limit.");
                }
                if (Files.exists(immutableViewer)) {
                    verifyExistingViewer(candidate, immutableViewer);
                    Files.delete(candidate);
                } else {
                    BasicFileAttributes currentCandidate = Files.readAttributes(
                            candidate, BasicFileAttributes.class, java.nio.file.LinkOption.NOFOLLOW_LINKS);
                    if (candidateKey != null ? !candidateKey.equals(currentCandidate.fileKey())
                            : !candidateCreated.equals(currentCandidate.creationTime())) {
                        throw new IOException("Generated trace viewer candidate identity changed.");
                    }
                    Files.move(candidate, immutableViewer, StandardCopyOption.ATOMIC_MOVE);
                }
                return immutableViewer;
            } catch (IOException | RuntimeException failure) {
                Files.deleteIfExists(candidate);
                throw failure;
            }
        }

        Path directory() {
            return directory;
        }

        @Override
        public synchronized void close() throws IOException {
            processLock.close();
            lockChannel.close();
        }

        private void verifyOwned(Path path) throws IOException {
            if (!Files.isRegularFile(path) || Files.isSymbolicLink(path) || Files.size(path) > MAX_VIEWER_BYTES
                    || !path.getFileName().toString().equals(".shaft-trace-viewer-" + sha256(path) + ".html")) {
                throw new IOException("Generated trace viewer cache contains unowned content.");
            }
        }

        private BasicFileAttributes attributes() throws IOException {
            return Files.readAttributes(directory, BasicFileAttributes.class, java.nio.file.LinkOption.NOFOLLOW_LINKS);
        }

        private void validateDirectory() throws IOException {
            BasicFileAttributes current = attributes();
            if (!current.isDirectory() || current.isSymbolicLink()
                    || !realDirectory.equals(directory.toRealPath(java.nio.file.LinkOption.NOFOLLOW_LINKS))
                    || (directoryKey != null ? !directoryKey.equals(current.fileKey())
                            : !directoryCreated.equals(current.creationTime()))) {
                throw new IOException("Generated trace viewer cache directory identity changed.");
            }
            BasicFileAttributes currentLock = Files.readAttributes(
                    directory.resolve(".owner.lock"), BasicFileAttributes.class,
                    java.nio.file.LinkOption.NOFOLLOW_LINKS);
            if (lockKey != null ? !lockKey.equals(currentLock.fileKey()) : !lockCreated.equals(currentLock.creationTime())) {
                throw new IOException("Generated trace viewer cache lock identity changed.");
            }
        }

        private static void restrictAcl(Path directory) throws IOException {
            AclFileAttributeView view = Files.getFileAttributeView(
                    directory, AclFileAttributeView.class, java.nio.file.LinkOption.NOFOLLOW_LINKS);
            if (view == null) {
                throw new IOException("Owner-only cache permissions are not supported.");
            }
            AclEntry owner = AclEntry.newBuilder()
                    .setType(AclEntryType.ALLOW)
                    .setPrincipal(Files.getOwner(directory, java.nio.file.LinkOption.NOFOLLOW_LINKS))
                    .setPermissions(EnumSet.allOf(AclEntryPermission.class))
                    .setFlags(AclEntryFlag.FILE_INHERIT, AclEntryFlag.DIRECTORY_INHERIT)
                    .build();
            view.setAcl(List.of(owner));
            if (!view.getAcl().equals(List.of(owner))) {
                throw new IOException("Owner-only cache ACL could not be verified.");
            }
        }

        private static Path privateCacheParent(Path tempRoot) throws IOException {
            Path probe = Files.createTempDirectory(tempRoot, ".shaft-owner-probe-");
            java.nio.file.attribute.UserPrincipal owner;
            try {
                owner = Files.getOwner(probe, java.nio.file.LinkOption.NOFOLLOW_LINKS);
            } finally {
                Files.deleteIfExists(probe);
            }
            String suffix;
            try {
                suffix = java.util.HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256")
                        .digest(owner.getName().getBytes(java.nio.charset.StandardCharsets.UTF_8)))
                        .substring(0, 32);
            } catch (NoSuchAlgorithmException impossible) {
                throw new IllegalStateException(impossible);
            }
            Path parent = tempRoot.resolve("shaft-intellij-trace-viewers-" + suffix);
            createPrivateDirectory(parent, owner);
            return parent;
        }

        private static void createPrivateDirectory(Path directory) throws IOException {
            createPrivateDirectory(directory, Files.getOwner(directory.getParent(), java.nio.file.LinkOption.NOFOLLOW_LINKS));
        }

        private static void createPrivateDirectory(Path directory, java.nio.file.attribute.UserPrincipal owner)
                throws IOException {
            if (Files.notExists(directory)) {
                try {
                    Files.createDirectory(directory, PosixFilePermissions.asFileAttribute(
                            PosixFilePermissions.fromString("rwx------")));
                } catch (UnsupportedOperationException ignored) {
                    Files.createDirectory(directory);
                    restrictAcl(directory);
                } catch (java.nio.file.FileAlreadyExistsException concurrentCreator) {
                    // Validate the winner below.
                }
            }
            if (!Files.isDirectory(directory, java.nio.file.LinkOption.NOFOLLOW_LINKS)
                    || Files.isSymbolicLink(directory)
                    || !Files.getOwner(directory, java.nio.file.LinkOption.NOFOLLOW_LINKS).equals(owner)) {
                throw new IOException("Generated trace viewer cache path is not a private directory.");
            }
            AclFileAttributeView acl = Files.getFileAttributeView(
                    directory, AclFileAttributeView.class, java.nio.file.LinkOption.NOFOLLOW_LINKS);
            if (acl != null) {
                restrictAcl(directory);
            } else {
                Set<java.nio.file.attribute.PosixFilePermission> permissions = Files.getPosixFilePermissions(
                        directory, java.nio.file.LinkOption.NOFOLLOW_LINKS);
                if (!permissions.equals(PosixFilePermissions.fromString("rwx------"))) {
                    throw new IOException("Generated trace viewer cache permissions are not owner-only.");
                }
            }
        }

        private static void cleanReusableSlot(Path directory, Path lockPath) throws IOException {
            try (Stream<Path> files = Files.list(directory)) {
                List<Path> contents = files.toList();
                for (Path path : contents) {
                    if (path.equals(lockPath)) continue;
                    String name = path.getFileName().toString();
                    if (Files.isSymbolicLink(path) || !Files.isRegularFile(path, java.nio.file.LinkOption.NOFOLLOW_LINKS)
                            || !(name.matches("\\.shaft-trace-viewer-[0-9a-f]{64}\\.html")
                                    || name.matches("\\.shaft-trace-(archive|viewer)-.*\\.tmp"))) {
                        throw new IOException("Generated trace viewer cache slot contains unowned content.");
                    }
                    Files.delete(path);
                }
            }
        }
    }
}
