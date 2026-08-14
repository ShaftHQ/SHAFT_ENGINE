package com.shaft.ai.local;

import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream;
import org.apache.commons.compress.archivers.zip.UnixStat;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermission;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HexFormat;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.EnumSet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.ExecutorService;
import java.util.function.BooleanSupplier;
import java.util.stream.Stream;

/** Deterministic artifact transfer and extraction boundaries for managed local inference. */
final class ManagedLocalAiArtifacts {
    private static final int BUFFER_SIZE = 64 * 1024;
    private static final int MAXIMUM_MEMBERS = 10_000;
    private static final long MAXIMUM_EXPANDED_BYTES = 4L * 1024 * 1024 * 1024;
    private static final long MINIMUM_EXPANSION_ALLOWANCE = 1024 * 1024;
    private static final long MAXIMUM_COMPRESSION_RATIO = 200;
    private static final int MAXIMUM_REDIRECTS = 3;
    private static final ExecutorService BODY_READERS = Executors.newVirtualThreadPerTaskExecutor();

    private ManagedLocalAiArtifacts() {
        throw new IllegalStateException("Utility class");
    }

    static void download(URI source, long expectedSize, String expectedSha256, Path target, Duration timeout,
                         BooleanSupplier cancelled) throws IOException, InterruptedException {
        requireReviewedArtifact(source, expectedSize, expectedSha256);
        HttpClient client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NEVER)
                .connectTimeout(timeout).build();
        download(source, expectedSize, expectedSha256, target, timeout, cancelled, (uri, requestTimeout) -> {
            HttpRequest request = HttpRequest.newBuilder(uri).timeout(requestTimeout).GET().build();
            HttpResponse<InputStream> response = client.send(request, HttpResponse.BodyHandlers.ofInputStream());
            return new DownloadResponse(response.statusCode(), response.uri(), response.headers().map(),
                    response.body());
        });
    }

    static void download(ManagedLocalAiManifest.RuntimeAsset asset, Path target, Duration timeout,
                         BooleanSupplier cancelled) throws IOException, InterruptedException {
        download(asset.url(), asset.size(), asset.sha256(), target, timeout, cancelled);
    }

    static void download(ManagedLocalAiManifest.ModelManifest model, Path target, Duration timeout,
                         BooleanSupplier cancelled) throws IOException, InterruptedException {
        download(model.url(), model.size(), model.sha256(), target, timeout, cancelled);
    }

    static void download(URI source, long expectedSize, String expectedSha256, Path target, Duration timeout,
                         BooleanSupplier cancelled, DownloadTransport transport)
            throws IOException, InterruptedException {
        validateSource(source);
        if (timeout == null || timeout.isZero() || timeout.isNegative()) {
            throw new IllegalArgumentException("Artifact timeout must be positive.");
        }
        URI current = source;
        for (int redirects = 0; redirects <= MAXIMUM_REDIRECTS; redirects++) {
            checkCancelled(cancelled);
            DownloadResponse response = transport.get(current, timeout);
            if (!current.equals(response.uri())) {
                response.body().close();
                throw new IllegalArgumentException("Artifact response URI does not match its request.");
            }
            if (response.statusCode() >= 300 && response.statusCode() < 400) {
                String location = firstHeader(response.headers(), "location");
                response.body().close();
                if (redirects == MAXIMUM_REDIRECTS || location == null) {
                    throw new IllegalArgumentException("Artifact redirect chain is invalid.");
                }
                URI next = current.resolve(location);
                validateRedirect(source, next);
                current = next;
                continue;
            }
            if (response.statusCode() != 200) {
                response.body().close();
                throw new IOException("Artifact source returned HTTP " + response.statusCode() + ".");
            }
            String contentLength = firstHeader(response.headers(), "content-length");
            if (contentLength != null) {
                try {
                    if (Long.parseLong(contentLength) != expectedSize) {
                        response.body().close();
                        throw new IllegalArgumentException("Artifact Content-Length does not match reviewed size.");
                    }
                } catch (NumberFormatException malformed) {
                    response.body().close();
                    throw new IllegalArgumentException("Artifact Content-Length is malformed.", malformed);
                }
            }
            try {
                download(new DeadlineInputStream(response.body(), timeout, cancelled), expectedSize, expectedSha256,
                        target, cancelled);
            } catch (BodyReadCancelled cancellation) {
                throw new InterruptedException(cancellation.getMessage());
            }
            return;
        }
        throw new IllegalArgumentException("Artifact redirect chain is invalid.");
    }

    /**
     * Streams one already-authorized response into a unique sibling stage and publishes only an exact match.
     *
     * @param input response body, closed by this method
     * @param expectedSize exact reviewed byte count
     * @param expectedSha256 exact reviewed lowercase digest
     * @param target final contained cache path
     * @param cancelled cooperative cancellation signal
     * @throws IOException on filesystem or stream failure
     * @throws InterruptedException when cancellation is observed
     */
    static void download(InputStream input, long expectedSize, String expectedSha256, Path target,
                         BooleanSupplier cancelled) throws IOException, InterruptedException {
        if (expectedSize <= 0 || expectedSha256 == null || !expectedSha256.matches("[0-9a-f]{64}")) {
            throw new IllegalArgumentException("Expected artifact size and SHA-256 must be valid.");
        }
        Path absoluteTarget = target.toAbsolutePath().normalize();
        Path parent = absoluteTarget.getParent();
        if (parent == null) {
            throw new IllegalArgumentException("Artifact target must have a parent directory.");
        }
        Files.createDirectories(parent);
        if (Files.exists(absoluteTarget, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalArgumentException("Artifact target must not already exist.");
        }
        Path stage = parent.resolve(absoluteTarget.getFileName() + ".part-" + UUID.randomUUID());
        MessageDigest digest = sha256();
        long received = 0;
        try (InputStream source = input;
             FileChannel output = FileChannel.open(stage, StandardOpenOption.CREATE_NEW,
                     StandardOpenOption.WRITE)) {
            byte[] buffer = new byte[BUFFER_SIZE];
            int count;
            while ((count = source.read(buffer)) != -1) {
                checkCancelled(cancelled);
                received = Math.addExact(received, count);
                if (received > expectedSize) {
                    throw new IllegalArgumentException("Artifact exceeds its reviewed byte count.");
                }
                digest.update(buffer, 0, count);
                ByteBuffer bytes = ByteBuffer.wrap(buffer, 0, count);
                while (bytes.hasRemaining()) {
                    output.write(bytes);
                }
            }
            checkCancelled(cancelled);
            output.force(true);
            if (received != expectedSize) {
                throw new IllegalArgumentException("Artifact does not match its reviewed byte count.");
            }
            String actual = HexFormat.of().formatHex(digest.digest());
            if (!MessageDigest.isEqual(actual.getBytes(java.nio.charset.StandardCharsets.US_ASCII),
                    expectedSha256.getBytes(java.nio.charset.StandardCharsets.US_ASCII))) {
                throw new IllegalArgumentException("Artifact SHA-256 does not match the reviewed digest.");
            }
            publishFileWithoutReplace(stage, absoluteTarget);
        } catch (IOException | InterruptedException | RuntimeException primary) {
            cleanupFile(stage, primary);
            throw primary;
        }
        Files.deleteIfExists(stage);
    }

    /**
     * Extracts a reviewed ZIP or tar.gz into a unique verified sibling stage.
     *
     * <p>This method deliberately does not publish an installation. The cache transaction owns the
     * later no-replace publication and must accept only a returned stage carrying its ready marker.</p>
     *
     * @param archive verified archive
     * @param destinationPrefix contained cache prefix used only to name the unique stage
     * @param cancelled cooperative cancellation signal
     * @throws IOException on archive or filesystem failure
     * @throws InterruptedException when cancellation is observed
     */
    static Path extractToStage(Path archive, Path destinationPrefix, BooleanSupplier cancelled)
            throws IOException, InterruptedException {
        return extractStage(archive, destinationPrefix, cancelled).root();
    }

    static Extraction extractStage(Path archive, Path destinationPrefix, BooleanSupplier cancelled)
            throws IOException, InterruptedException {
        Path absoluteArchive = archive.toAbsolutePath().normalize();
        if (!Files.isRegularFile(absoluteArchive, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalArgumentException("Archive must be a verified regular file.");
        }
        Path absoluteDestination = destinationPrefix.toAbsolutePath().normalize();
        Path parent = absoluteDestination.getParent();
        if (parent == null) {
            throw new IllegalArgumentException("Extraction destination prefix must have a parent.");
        }
        Files.createDirectories(parent);
        Path stage = parent.resolve(absoluteDestination.getFileName() + ".extract-" + UUID.randomUUID());
        Files.createDirectory(stage);
        List<Path> files = new ArrayList<>();
        List<Path> directories = new ArrayList<>();
        directories.add(stage);
        try {
            List<Path> executables = extractInto(absoluteArchive, stage, cancelled, files, directories);
            markReviewedExecutables(executables);
            checkCancelled(cancelled);
            publishReadyMarker(stage);
            files.add(stage.resolve(".shaft-ready"));
            return new Extraction(stage, List.copyOf(files), List.copyOf(directories));
        } catch (IOException | InterruptedException | RuntimeException primary) {
            cleanupExact(files, directories, primary);
            throw primary;
        }
    }

    private static List<Path> extractInto(Path archive, Path stage, BooleanSupplier cancelled,
                                          List<Path> files, List<Path> directories)
            throws IOException, InterruptedException {
        long archiveBytes = Files.size(archive);
        long ratioLimit;
        try {
            ratioLimit = Math.addExact(Math.multiplyExact(archiveBytes, MAXIMUM_COMPRESSION_RATIO),
                    MINIMUM_EXPANSION_ALLOWANCE);
        } catch (ArithmeticException overflow) {
            ratioLimit = MAXIMUM_EXPANDED_BYTES;
        }
        long byteLimit = Math.min(MAXIMUM_EXPANDED_BYTES, ratioLimit);
        Set<String> memberPaths = new java.util.HashSet<>();
        List<Path> executables = new ArrayList<>();
        try (ArchiveInputStream<?> input = openArchive(archive)) {
            int members = 0;
            long expanded = 0;
            ArchiveEntry entry;
            while ((entry = input.getNextEntry()) != null) {
                checkCancelled(cancelled);
                if (++members > MAXIMUM_MEMBERS) {
                    throw new IllegalArgumentException("Archive has too many members.");
                }
                validateEntry(entry);
                Path target = contained(stage, entry.getName());
                String portableIdentity = stage.relativize(target).toString().replace('\\', '/')
                        .toLowerCase(Locale.ROOT);
                if (!memberPaths.add(portableIdentity)) {
                    throw new IllegalArgumentException("Archive contains a duplicate portable member path.");
                }
                if (entry.isDirectory()) {
                    createDirectoriesTracked(stage, target, directories);
                    continue;
                }
                long declared = entry.getSize();
                if (declared != ArchiveEntry.SIZE_UNKNOWN && declared > byteLimit - expanded) {
                    throw new IllegalArgumentException("Archive exceeds its expanded byte limit.");
                }
                createDirectoriesTracked(stage, target.getParent(), directories);
                if (Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
                    throw new IllegalArgumentException("Archive contains a duplicate member path.");
                }
                long written = copyBounded(input, target, byteLimit - expanded, cancelled, files);
                if (declared != ArchiveEntry.SIZE_UNKNOWN && written != declared) {
                    throw new IllegalArgumentException("Archive member size does not match its declaration.");
                }
                expanded = Math.addExact(expanded, written);
                String basename = target.getFileName().toString();
                if (basename.equals("llama-server") || basename.equals("llama-server.exe")) {
                    executables.add(target);
                }
            }
            if (members == 0) {
                throw new IllegalArgumentException("Archive must contain at least one member.");
            }
        }
        if (executables.size() != 1) {
            throw new IllegalArgumentException("Archive must contain exactly one reviewed runtime executable.");
        }
        return List.copyOf(executables);
    }

    private static void createDirectoriesTracked(Path root, Path target, List<Path> directories) throws IOException {
        List<Path> missing = new ArrayList<>();
        Path cursor = target;
        while (!cursor.equals(root) && !Files.exists(cursor, LinkOption.NOFOLLOW_LINKS)) {
            missing.add(cursor);
            cursor = cursor.getParent();
            if (cursor == null || !cursor.startsWith(root)) {
                throw new IllegalArgumentException("Archive directory escapes its extraction root.");
            }
        }
        if (!Files.isDirectory(cursor, LinkOption.NOFOLLOW_LINKS)) {
            throw new IllegalArgumentException("Archive directory collides with a non-directory path.");
        }
        for (int index = missing.size() - 1; index >= 0; index--) {
            Path directory = missing.get(index);
            Files.createDirectory(directory);
            directories.add(directory);
        }
    }

    private static ArchiveInputStream<?> openArchive(Path archive) throws IOException {
        String name = archive.getFileName().toString().toLowerCase(Locale.ROOT);
        InputStream raw = new BufferedInputStream(Files.newInputStream(archive));
        try {
            if (name.endsWith(".zip")) {
                return new ZipArchiveInputStream(raw);
            }
            if (name.endsWith(".tar.gz") || name.endsWith(".tgz")) {
                return new TarArchiveInputStream(new GzipCompressorInputStream(raw));
            }
            throw new IllegalArgumentException("Only reviewed ZIP and tar.gz archives are supported.");
        } catch (RuntimeException | IOException failure) {
            raw.close();
            throw failure;
        }
    }

    private static void validateEntry(ArchiveEntry entry) {
        String name = entry.getName();
        if (name == null || name.isBlank() || name.indexOf('\0') >= 0) {
            throw new IllegalArgumentException("Archive member name is invalid.");
        }
        if (entry instanceof ZipArchiveEntry zip) {
            int type = zip.getUnixMode() & UnixStat.FILE_TYPE_FLAG;
            if (zip.isUnixSymlink() || type != 0 && type != UnixStat.FILE_FLAG && type != UnixStat.DIR_FLAG) {
                throw new IllegalArgumentException("Archive links and device members are not supported.");
            }
        }
        if (entry instanceof TarArchiveEntry tar && (tar.isLink() || tar.isSymbolicLink()
                || tar.isBlockDevice() || tar.isCharacterDevice() || tar.isFIFO())) {
            throw new IllegalArgumentException("Archive links and device members are not supported.");
        }
    }

    private static Path contained(Path root, String memberName) {
        String normalizedSeparators = memberName.replace('\\', '/');
        if (normalizedSeparators.startsWith("/") || normalizedSeparators.matches("^[A-Za-z]:.*")) {
            throw new IllegalArgumentException("Archive member must use a relative path.");
        }
        Path target = root.resolve(normalizedSeparators).normalize();
        if (!target.startsWith(root) || target.equals(root)) {
            throw new IllegalArgumentException("Archive member escapes its extraction root.");
        }
        for (Path part : root.relativize(target)) {
            String value = part.toString();
            String stem = value.split("\\.", 2)[0].toUpperCase(Locale.ROOT);
            if (value.isBlank() || value.endsWith(".") || value.endsWith(" ")
                    || value.matches(".*[<>:\"|?*].*")
                    || ManagedLocalAiManifest.WINDOWS_DEVICES.contains(stem)) {
                throw new IllegalArgumentException("Archive member is not portable.");
            }
        }
        return target;
    }

    private static long copyBounded(ArchiveInputStream<?> input, Path target, long remaining,
                                    BooleanSupplier cancelled, List<Path> createdFiles)
            throws IOException, InterruptedException {
        long[] written = {0};
        createExclusive(target, createdFiles, output -> {
            byte[] buffer = new byte[BUFFER_SIZE];
            int count;
            while ((count = input.read(buffer)) != -1) {
                checkCancelled(cancelled);
                written[0] = Math.addExact(written[0], count);
                if (written[0] > remaining) throw new IllegalArgumentException(
                        "Archive exceeds its expanded byte limit.");
                ByteBuffer bytes = ByteBuffer.wrap(buffer, 0, count);
                while (bytes.hasRemaining()) {
                    output.write(bytes);
                }
            }
            output.force(true);
        });
        return written[0];
    }

    private static void publishFileWithoutReplace(Path source, Path target) throws IOException {
        Files.createLink(target, source);
        Files.delete(source);
    }

    private static void markReviewedExecutables(List<Path> executables) throws IOException {
        Path executable = executables.getFirst();
        if (Files.getFileStore(executable).supportsFileAttributeView("posix")) {
            Files.setPosixFilePermissions(executable, EnumSet.of(PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE, PosixFilePermission.OWNER_EXECUTE,
                    PosixFilePermission.GROUP_READ, PosixFilePermission.GROUP_EXECUTE,
                    PosixFilePermission.OTHERS_READ, PosixFilePermission.OTHERS_EXECUTE));
        }
    }

    private static void publishReadyMarker(Path root) throws IOException {
        Path marker = root.resolve(".shaft-ready");
        try (FileChannel output = FileChannel.open(marker, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)) {
            output.force(true);
        }
    }

    private static void validateSource(URI source) {
        if (source == null || !"https".equalsIgnoreCase(source.getScheme()) || source.getHost() == null
                || source.getUserInfo() != null || source.getPort() != -1
                || !Set.of("github.com", "huggingface.co").contains(source.getHost().toLowerCase(Locale.ROOT))) {
            throw new IllegalArgumentException("Artifact source must use a reviewed canonical HTTPS host.");
        }
        String host = source.getHost().toLowerCase(Locale.ROOT);
        String path = source.getPath();
        boolean boundPath = "github.com".equals(host)
                ? path.matches("/ggml-org/llama\\.cpp/releases/download/[A-Za-z0-9._+-]+/[A-Za-z0-9._+-]+")
                : path.matches("/[A-Za-z0-9._-]+/[A-Za-z0-9._-]+/resolve/[0-9a-f]{40}/[A-Za-z0-9._+-]+");
        if (!boundPath || source.getQuery() != null || source.getFragment() != null) {
            throw new IllegalArgumentException("Artifact source path must bind a reviewed release or revision.");
        }
    }

    private static void requireReviewedArtifact(URI source, long size, String sha256) {
        ManagedLocalAiManifest manifest = ManagedLocalAiManifest.loadDefault();
        boolean runtime = manifest.runtime().assets().stream().anyMatch(asset -> asset.url().equals(source)
                && asset.size() == size && asset.sha256().equals(sha256));
        boolean model = manifest.models().stream().anyMatch(candidate -> candidate.url().equals(source)
                && candidate.size() == size && candidate.sha256().equals(sha256));
        if (!runtime && !model) {
            throw new IllegalArgumentException("Artifact is not present in the reviewed managed local AI manifest.");
        }
    }

    private static void validateRedirect(URI original, URI redirect) {
        if (!"https".equalsIgnoreCase(redirect.getScheme()) || redirect.getHost() == null
                || redirect.getUserInfo() != null || redirect.getPort() != -1 || redirect.getFragment() != null) {
            throw new IllegalArgumentException("Artifact redirect must use canonical HTTPS.");
        }
        String originalHost = original.getHost().toLowerCase(Locale.ROOT);
        String redirectHost = redirect.getHost().toLowerCase(Locale.ROOT);
        boolean trusted = "github.com".equals(originalHost)
                ? "release-assets.githubusercontent.com".equals(redirectHost)
                : "huggingface.co".equals(originalHost)
                && (redirectHost.endsWith(".cdn.hf.co") || "cdn-lfs.hf.co".equals(redirectHost)
                || "cas-bridge.xethub.hf.co".equals(redirectHost));
        if (!trusted) {
            throw new IllegalArgumentException("Artifact redirect host is not trusted for its source.");
        }
    }

    private static String firstHeader(Map<String, List<String>> headers, String name) {
        return headers.entrySet().stream().filter(entry -> entry.getKey().equalsIgnoreCase(name))
                .flatMap(entry -> entry.getValue().stream()).findFirst().orElse(null);
    }

    private static void checkCancelled(BooleanSupplier cancelled) throws InterruptedException {
        if (Thread.currentThread().isInterrupted() || cancelled.getAsBoolean()) {
            throw new InterruptedException("Managed local AI artifact operation was cancelled.");
        }
    }

    private static MessageDigest sha256() {
        try {
            return MessageDigest.getInstance("SHA-256");
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("JDK SHA-256 support is unavailable.", impossible);
        }
    }

    static void deleteCreatedTree(Path root) throws IOException {
        if (!Files.exists(root, LinkOption.NOFOLLOW_LINKS)) {
            return;
        }
        CreatedTreeDeletionVisitor visitor = new CreatedTreeDeletionVisitor(root);
        Files.walkFileTree(root, visitor);
        visitor.throwIfFailed();
    }

    private static final class CreatedTreeDeletionVisitor extends java.nio.file.SimpleFileVisitor<Path> {
        private final Path root;
        private final List<IOException> failures = new ArrayList<>();

        private CreatedTreeDeletionVisitor(Path root) {
            this.root = root;
        }

        @Override
        public java.nio.file.FileVisitResult preVisitDirectory(Path directory,
                java.nio.file.attribute.BasicFileAttributes attributes) {
            if (!directory.equals(root) && (attributes.isSymbolicLink() || attributes.isOther())) {
                return java.nio.file.FileVisitResult.SKIP_SUBTREE;
            }
            return java.nio.file.FileVisitResult.CONTINUE;
        }

        @Override
        public java.nio.file.FileVisitResult visitFile(Path file,
                java.nio.file.attribute.BasicFileAttributes attributes) throws IOException {
            if (!attributes.isSymbolicLink() && !attributes.isOther()) {
                Files.deleteIfExists(file);
            }
            return java.nio.file.FileVisitResult.CONTINUE;
        }

        @Override
        public java.nio.file.FileVisitResult postVisitDirectory(Path directory, IOException failure) {
            if (failure != null) {
                failures.add(failure);
            }
            deleteDirectory(directory);
            return java.nio.file.FileVisitResult.CONTINUE;
        }

        private void deleteDirectory(Path directory) {
            try {
                Files.delete(directory);
            } catch (java.nio.file.DirectoryNotEmptyException unknownContent) {
                // Concurrent unknown or reparse content is preserved.
            } catch (IOException deleteFailure) {
                failures.add(deleteFailure);
            }
        }

        private void throwIfFailed() throws IOException {
            if (!failures.isEmpty()) {
                throw failures.getFirst();
            }
        }
    }

    static void cleanupExact(List<Path> files, List<Path> directories, Throwable primary) {
        IOException cleanup = null;
        for (Path file : files.reversed()) {
            try {
                Files.deleteIfExists(file);
            } catch (IOException failure) {
                cleanup = append(cleanup, failure);
            }
        }
        for (Path directory : directories.reversed()) {
            try {
                Files.delete(directory);
            } catch (java.nio.file.DirectoryNotEmptyException unknown) {
                // Preserve concurrently introduced unknown content and its containing stage.
            } catch (IOException failure) {
                cleanup = append(cleanup, failure);
            }
        }
        if (cleanup != null) {
            primary.addSuppressed(cleanup);
        }
    }

    static void createExclusive(Path target, List<Path> createdFiles, IoAction action)
            throws IOException, InterruptedException {
        try (FileChannel channel = FileChannel.open(target, StandardOpenOption.CREATE_NEW,
                StandardOpenOption.WRITE)) {
            createdFiles.add(target);
            action.run(channel);
        }
    }

    private static IOException append(IOException existing, IOException next) {
        if (existing == null) return next;
        existing.addSuppressed(next);
        return existing;
    }

    private static void cleanupFile(Path stage, Throwable primary) {
        try {
            Files.deleteIfExists(stage);
        } catch (IOException cleanup) {
            primary.addSuppressed(cleanup);
        }
    }

    private static void cleanupTree(Path stage, Throwable primary) {
        try {
            deleteCreatedTree(stage);
        } catch (IOException cleanup) {
            primary.addSuppressed(cleanup);
        }
    }

    @FunctionalInterface
    interface DownloadTransport {
        DownloadResponse get(URI uri, Duration timeout) throws IOException, InterruptedException;
    }

    @FunctionalInterface
    interface IoAction {
        void run(FileChannel channel) throws IOException, InterruptedException;
    }

    record DownloadResponse(int statusCode, URI uri, Map<String, List<String>> headers, InputStream body) {
        DownloadResponse {
            headers = Map.copyOf(headers);
        }
    }

    record Extraction(Path root, List<Path> files, List<Path> directories) {
        Extraction {
            files = List.copyOf(files);
            directories = List.copyOf(directories);
        }
    }

    private static final class DeadlineInputStream extends InputStream {
        private final InputStream delegate;
        private final long deadlineNanos;
        private final BooleanSupplier cancelled;

        private DeadlineInputStream(InputStream delegate, Duration timeout, BooleanSupplier cancelled) {
            this.delegate = delegate;
            this.deadlineNanos = System.nanoTime() + timeout.toNanos();
            this.cancelled = cancelled;
        }

        @Override
        public int read() throws IOException {
            byte[] single = new byte[1];
            int count = read(single, 0, 1);
            return count == -1 ? -1 : Byte.toUnsignedInt(single[0]);
        }

        @Override
        public int read(byte[] buffer, int offset, int length) throws IOException {
            Future<Integer> read = BODY_READERS.submit(() -> delegate.read(buffer, offset, length));
            try {
                while (true) {
                    if (Thread.currentThread().isInterrupted() || cancelled.getAsBoolean()) {
                        read.cancel(true);
                        closeAsynchronously();
                        throw new BodyReadCancelled("Artifact response-body read was cancelled.");
                    }
                    long remaining = deadlineNanos - System.nanoTime();
                    if (remaining <= 0) {
                        read.cancel(true);
                        closeAsynchronously();
                        throw new IOException("Artifact response body timed out.");
                    }
                    try {
                        return read.get(Math.min(remaining, TimeUnit.MILLISECONDS.toNanos(100)),
                                TimeUnit.NANOSECONDS);
                    } catch (TimeoutException polling) {
                        // Polling keeps cooperative cancellation observable while a native read is blocked.
                    }
                }
            } catch (InterruptedException cancellation) {
                Thread.currentThread().interrupt();
                read.cancel(true);
                closeAsynchronously();
                throw new BodyReadCancelled("Artifact response-body read was interrupted.");
            } catch (ExecutionException failure) {
                Throwable cause = failure.getCause();
                if (cause instanceof IOException ioFailure) {
                    throw ioFailure;
                }
                throw new IOException("Artifact response-body read failed.", cause);
            }
        }

        @Override
        public void close() throws IOException {
            delegate.close();
        }

        private void closeAsynchronously() {
            BODY_READERS.submit(() -> {
                try {
                    delegate.close();
                } catch (IOException ignored) {
                    // The timeout/cancellation failure remains primary.
                }
            });
        }
    }

    private static final class BodyReadCancelled extends InterruptedIOException {
        private BodyReadCancelled(String message) {
            super(message);
        }
    }
}
