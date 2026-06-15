package com.shaft.mcp.install;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.AccessDeniedException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

final class InstallerFileOperations {
    private static final ObjectMapper JSON = new ObjectMapper(
            new JsonFactory().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION))
            .enable(SerializationFeature.INDENT_OUTPUT);

    private InstallerFileOperations() {
        throw new IllegalStateException("Utility class");
    }

    static ObjectNode readJsonObject(Path path) throws IOException {
        if (!Files.exists(path)) {
            return JSON.createObjectNode();
        }
        JsonNode value = JSON.readTree(Files.readAllBytes(path));
        if (!(value instanceof ObjectNode object)) {
            throw new IOException("Configuration root must be a JSON object: " + path);
        }
        return object;
    }

    static JsonNode parseJson(String value) throws IOException {
        return JSON.readTree(value);
    }

    static ObjectNode newJsonObject() {
        return JSON.createObjectNode();
    }

    static String writeJson(ObjectNode value) throws IOException {
        return JSON.writeValueAsString(value);
    }

    static void writeJsonAtomically(Path destination, ObjectNode value) throws IOException {
        byte[] content = (JSON.writerWithDefaultPrettyPrinter().writeValueAsString(value) + System.lineSeparator())
                .getBytes(StandardCharsets.UTF_8);
        Path absolute = destination.toAbsolutePath().normalize();
        Path parent = requireParent(absolute);
        Files.createDirectories(parent);
        Path temporary = Files.createTempFile(parent, "." + absolute.getFileName(), ".tmp");
        try {
            try (FileChannel channel = FileChannel.open(
                    temporary,
                    StandardOpenOption.WRITE,
                    StandardOpenOption.TRUNCATE_EXISTING)) {
                channel.write(ByteBuffer.wrap(content));
                channel.force(true);
            }
            readJsonObject(temporary);
            moveReplacing(temporary, absolute);
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    static void copyVerifiedAtomically(Path source, Path destination) throws IOException {
        Path absoluteSource = source.toAbsolutePath().normalize();
        Path absoluteDestination = destination.toAbsolutePath().normalize();
        String sourceHash = sha256(absoluteSource);
        if (Files.isRegularFile(absoluteDestination) && sourceHash.equals(sha256(absoluteDestination))) {
            return;
        }

        Path parent = requireParent(absoluteDestination);
        Files.createDirectories(parent);
        Path temporary = Files.createTempFile(parent, "." + absoluteDestination.getFileName(), ".tmp");
        try {
            Files.copy(absoluteSource, temporary, StandardCopyOption.REPLACE_EXISTING);
            if (!sourceHash.equals(sha256(temporary))) {
                throw new IOException("Copied shaft-mcp JAR failed SHA-256 verification.");
            }
            moveReplacing(temporary, absoluteDestination);
            if (!sourceHash.equals(sha256(absoluteDestination))) {
                throw new IOException("Installed shaft-mcp JAR failed SHA-256 verification.");
            }
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    static String sha256(Path path) throws IOException {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            try (var input = Files.newInputStream(path)) {
                byte[] buffer = new byte[8192];
                int count;
                while ((count = input.read(buffer)) >= 0) {
                    if (count > 0) {
                        digest.update(buffer, 0, count);
                    }
                }
            }
            return HexFormat.of().formatHex(digest.digest());
        } catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is unavailable.", exception);
        }
    }

    static void writeBytesAtomically(Path destination, byte[] content) throws IOException {
        Path absolute = destination.toAbsolutePath().normalize();
        Path parent = requireParent(absolute);
        Files.createDirectories(parent);
        Path temporary = Files.createTempFile(parent, "." + absolute.getFileName(), ".tmp");
        try {
            try (FileChannel channel = FileChannel.open(
                    temporary,
                    StandardOpenOption.WRITE,
                    StandardOpenOption.TRUNCATE_EXISTING)) {
                channel.write(ByteBuffer.wrap(content));
                channel.force(true);
            }
            moveReplacing(temporary, absolute);
        } finally {
            Files.deleteIfExists(temporary);
        }
    }

    private static Path requireParent(Path path) throws IOException {
        Path parent = path.getParent();
        if (parent == null) {
            throw new IOException("Configuration path requires a parent directory: " + path);
        }
        return parent;
    }

    private static void moveReplacing(Path source, Path destination) throws IOException {
        boolean atomic = true;
        for (int attempt = 1; attempt <= 50; attempt++) {
            try {
                if (atomic) {
                    Files.move(source, destination, StandardCopyOption.ATOMIC_MOVE,
                            StandardCopyOption.REPLACE_EXISTING);
                } else {
                    Files.move(source, destination, StandardCopyOption.REPLACE_EXISTING);
                }
                return;
            } catch (AtomicMoveNotSupportedException ignored) {
                atomic = false;
            } catch (AccessDeniedException exception) {
                if (attempt == 50) {
                    throw exception;
                }
                try {
                    Thread.sleep(10);
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while publishing installer data.", interrupted);
                }
            }
        }
    }

    static final class ConfigurationBackup implements AutoCloseable {
        private final Path configuration;
        private final boolean existed;
        private final byte[] original;
        private final Path backup;
        private boolean committed;
        private boolean restored;

        ConfigurationBackup(Path configuration) throws IOException {
            this.configuration = configuration.toAbsolutePath().normalize();
            existed = Files.exists(this.configuration);
            original = existed ? Files.readAllBytes(this.configuration) : new byte[0];
            Path parent = requireParent(this.configuration);
            Files.createDirectories(parent);
            backup = Files.createTempFile(parent, "." + this.configuration.getFileName(), ".shaft-mcp-backup");
            try (FileChannel channel = FileChannel.open(
                    backup,
                    StandardOpenOption.WRITE,
                    StandardOpenOption.TRUNCATE_EXISTING)) {
                channel.write(ByteBuffer.wrap(original));
                channel.force(true);
            }
        }

        Path path() {
            return configuration;
        }

        Path backupPath() {
            return backup;
        }

        void commit() {
            committed = true;
        }

        void restore() throws IOException {
            if (existed) {
                writeBytesAtomically(configuration, original);
            } else {
                Files.deleteIfExists(configuration);
            }
            restored = true;
        }

        @Override
        public void close() throws IOException {
            if (committed || restored) {
                Files.deleteIfExists(backup);
            }
        }
    }
}
