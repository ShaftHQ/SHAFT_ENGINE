package com.shaft.infrastructure;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

/** Download cache that publishes an artifact only after its approved SHA-256 matches. */
public final class VerifiedArtifactStore {
    static final long MAX_ARTIFACT_BYTES = 128L * 1024 * 1024;
    private final Path downloads;

    public VerifiedArtifactStore(Path downloads) {
        this.downloads = downloads.toAbsolutePath().normalize();
    }

    public Path fetch(SetupAction action) throws IOException {
        return fetch(action, false);
    }

    public Path fetch(SetupAction action, boolean offline) throws IOException {
        requireUnlinkedAncestors(downloads);
        String expected = action.checksum().substring("sha256:".length()).toLowerCase();
        String sourcePath = action.source().getPath();
        String sourceName = sourcePath.substring(sourcePath.lastIndexOf('/') + 1);
        if (sourceName.isBlank()) throw new IOException("Artifact source has no file name: " + action.source());
        Path destination = downloads.resolve(expected + '-' + sourceName);
        if (Files.isRegularFile(destination, java.nio.file.LinkOption.NOFOLLOW_LINKS)
                && expected.equals(digest(destination))) return destination;
        if (offline) throw new IOException("Artifact is not available in the verified offline cache: " + sourceName);
        Files.createDirectories(downloads);
        requireUnlinkedAncestors(downloads);
        if (Files.isSymbolicLink(destination)) {
            throw new IOException("Verified artifact cache entry must not be a symbolic link: " + destination);
        }
        Path temporary = Files.createTempFile(downloads, sourceName, ".part");
        Path quarantine = downloads.resolve(destination.getFileName() + ".quarantine");
        try (InputStream input = open(action.source());
             OutputStream output = Files.newOutputStream(temporary)) {
            copyBounded(input, output, action.source());
            String actual = digest(temporary);
            if (!expected.equals(actual)) {
                throw new IOException("SHA-256 mismatch for " + action.source() + ": expected " + expected
                        + " but received " + actual);
            }
            replaceWithRollback(temporary, destination, quarantine);
            return destination;
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    static void replaceWithRollback(Path replacement, Path destination, Path quarantine) throws IOException {
        replaceWithRollback(replacement, destination, quarantine, VerifiedArtifactStore::move);
    }

    static void replaceWithRollback(Path replacement, Path destination, Path quarantine,
                                    MoveOperation mover) throws IOException {
        if (Files.exists(quarantine, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
            if (Files.exists(destination, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
                throw new IOException("Unresolved setup quarantine requires manual recovery: " + quarantine);
            }
            mover.move(quarantine, destination);
        }
        boolean hadDestination = Files.exists(destination, java.nio.file.LinkOption.NOFOLLOW_LINKS);
        if (hadDestination) mover.move(destination, quarantine);
        try {
            mover.move(replacement, destination);
            Files.deleteIfExists(quarantine);
        } catch (IOException failure) {
            if (hadDestination && Files.exists(quarantine, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
                try {
                    mover.move(quarantine, destination);
                } catch (IOException rollbackFailure) {
                    failure.addSuppressed(rollbackFailure);
                }
            }
            throw failure;
        }
    }

    @FunctionalInterface
    interface MoveOperation {
        void move(Path source, Path destination) throws IOException;
    }

    private static void copyBounded(InputStream input, OutputStream output, URI source) throws IOException {
        byte[] buffer = new byte[64 * 1024];
        long total = 0;
        for (int read; (read = input.read(buffer)) >= 0;) {
            total += read;
            if (total > MAX_ARTIFACT_BYTES) {
                throw new IOException("Artifact exceeds the " + MAX_ARTIFACT_BYTES + " byte safety limit: " + source);
            }
            output.write(buffer, 0, read);
        }
    }

    static void requireUnlinkedAncestors(Path path) throws IOException {
        Path absolute = path.toAbsolutePath().normalize();
        for (Path current = absolute; current != null; current = current.getParent()) {
            if (Files.isSymbolicLink(current)) {
                throw new IOException("SHAFT setup paths must not contain symbolic links: " + current);
            }
            if (Files.exists(current, java.nio.file.LinkOption.NOFOLLOW_LINKS)
                    && Files.getAttribute(current, "basic:isOther", java.nio.file.LinkOption.NOFOLLOW_LINKS)
                    .equals(Boolean.TRUE)) {
                throw new IOException("SHAFT setup paths must not contain reparse/special entries: " + current);
            }
        }
    }

    private static InputStream open(URI source) throws IOException {
        if ("file".equalsIgnoreCase(source.getScheme())) return Files.newInputStream(Path.of(source));
        if (!"https".equalsIgnoreCase(source.getScheme())) {
            throw new IOException("Only HTTPS and file setup artifacts are supported: " + source);
        }
        HttpURLConnection connection = (HttpURLConnection) source.toURL().openConnection();
        connection.setConnectTimeout(30_000);
        connection.setReadTimeout(120_000);
        connection.setInstanceFollowRedirects(true);
        int status = connection.getResponseCode();
        if (status < 200 || status >= 300) {
            connection.disconnect();
            throw new IOException("Artifact download failed with HTTP " + status + ": " + source);
        }
        return connection.getInputStream();
    }

    public static String digest(Path file) throws IOException {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            try (InputStream input = Files.newInputStream(file)) {
                byte[] buffer = new byte[64 * 1024];
                for (int read; (read = input.read(buffer)) >= 0;) digest.update(buffer, 0, read);
            }
            return HexFormat.of().formatHex(digest.digest());
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException("SHA-256 is required by the Java platform.", impossible);
        }
    }

    static void move(Path source, Path destination) throws IOException {
        try {
            Files.move(source, destination, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
        } catch (AtomicMoveNotSupportedException ignored) {
            Files.move(source, destination, StandardCopyOption.REPLACE_EXISTING);
        }
    }
}
