package com.shaft.infrastructure;

import java.io.IOException;
import java.io.InputStream;
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
    private final Path downloads;

    public VerifiedArtifactStore(Path downloads) {
        this.downloads = downloads.toAbsolutePath().normalize();
    }

    public Path fetch(SetupAction action) throws IOException {
        String expected = action.checksum().substring("sha256:".length()).toLowerCase();
        String sourcePath = action.source().getPath();
        String sourceName = sourcePath.substring(sourcePath.lastIndexOf('/') + 1);
        if (sourceName.isBlank()) throw new IOException("Artifact source has no file name: " + action.source());
        Path destination = downloads.resolve(expected + '-' + sourceName);
        if (Files.isRegularFile(destination) && expected.equals(digest(destination))) return destination;
        Files.createDirectories(downloads);
        Files.deleteIfExists(destination);
        Path temporary = Files.createTempFile(downloads, sourceName, ".part");
        try (InputStream input = open(action.source())) {
            Files.copy(input, temporary, StandardCopyOption.REPLACE_EXISTING);
            String actual = digest(temporary);
            if (!expected.equals(actual)) {
                throw new IOException("SHA-256 mismatch for " + action.source() + ": expected " + expected
                        + " but received " + actual);
            }
            move(temporary, destination);
            return destination;
        } finally {
            Files.deleteIfExists(temporary);
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

    static String digest(Path file) throws IOException {
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
