package com.shaft.gui.mobile;

import com.shaft.tools.io.internal.TraceArchiveWriter;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.gui.mobile.internal.MobileEvidenceState;
import io.appium.java_client.AppiumDriver;
import tools.jackson.databind.ObjectMapper;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

final class MobileEvidenceArchiveWriter {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final Map<Path, TargetLock> TARGET_LOCKS = new HashMap<>();

    private MobileEvidenceArchiveWriter() {
        throw new IllegalStateException("Utility class");
    }

    static void write(Path target, byte[] manifest, List<Entry> artifacts, long maxBytes) {
        Objects.requireNonNull(target, "target");
        Objects.requireNonNull(manifest, "manifest");
        Objects.requireNonNull(artifacts, "artifacts");
        if (maxBytes < 1) {
            throw new IllegalArgumentException("maxBytes must be positive");
        }
        List<Entry> entries = new ArrayList<>();
        entries.add(Entry.bytes("mobile-evidence.json", manifest));
        entries.addAll(List.copyOf(artifacts));
        validate(entries, maxBytes);

        try (StagedArchive staged = stage(manifest, artifacts, maxBytes)) {
            withTargetLock(target, () -> publishLocked(staged, target.toAbsolutePath().normalize()));
        }
    }

    static StagedArchive stage(byte[] manifest, List<Entry> artifacts, long maxBytes) {
        Objects.requireNonNull(manifest, "manifest");
        Objects.requireNonNull(artifacts, "artifacts");
        if (maxBytes < 1) {
            throw new IllegalArgumentException("maxBytes must be positive");
        }
        List<Entry> entries = new ArrayList<>();
        entries.add(Entry.bytes("mobile-evidence.json", manifest));
        entries.addAll(List.copyOf(artifacts));
        validate(entries, maxBytes);

        Path staging = null;
        try {
            staging = Files.createTempFile("shaft-mobile-evidence-", ".zip");
            registerSensitivePath(staging);
            try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(staging,
                    StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE))) {
                byte[] buffer = new byte[16 * 1024];
                for (Entry entry : entries) {
                    zip.putNextEntry(new ZipEntry(entry.path()));
                    try (InputStream input = entry.open()) {
                        long written = 0;
                        int count;
                        while ((count = input.read(buffer)) != -1) {
                            written = Math.addExact(written, count);
                            if (written > entry.size()) {
                                throw new IOException("Mobile evidence entry changed while being archived.");
                            }
                            zip.write(buffer, 0, count);
                        }
                        if (written != entry.size()) {
                            throw new IOException("Mobile evidence entry changed while being archived.");
                        }
                    } finally {
                        zip.closeEntry();
                    }
                }
            }
            return new StagedArchive(staging);
        } catch (IOException | ArithmeticException exception) {
            delete(staging);
            throw new IllegalStateException("Could not publish mobile evidence archive.", exception);
        }
    }

    static void publish(AppiumDriver driver, StagedArchive staged, Path target, Runnable validation) {
        AppiumDriver requiredDriver = Objects.requireNonNull(driver, "driver");
        Objects.requireNonNull(staged, "staged");
        Runnable requiredValidation = Objects.requireNonNull(validation, "validation");
        Path requiredTarget = Objects.requireNonNull(target, "target").toAbsolutePath().normalize();
        registerSensitivePath(requiredTarget);
        withTargetLock(requiredTarget, () -> MobileEvidenceState.publish(requiredDriver, () -> {
            requiredValidation.run();
            publishLocked(staged, requiredTarget);
        }));
    }

    private static void publishLocked(StagedArchive staged, Path requiredTarget) {
        try {
            TraceArchiveWriter.copy(staged.path(), requiredTarget);
        } catch (IOException exception) {
            throw new IllegalStateException("Could not publish mobile evidence archive.", exception);
        }
    }

    static Optional<byte[]> serializeBounded(Object value, long maxBytes) {
        Objects.requireNonNull(value, "value");
        if (maxBytes < 0) {
            return Optional.empty();
        }
        BoundedOutputStream output = new BoundedOutputStream((int) Math.min(maxBytes, Integer.MAX_VALUE));
        try {
            JSON.writeValue(output, value);
            return Optional.of(output.toByteArray());
        } catch (RuntimeException exception) {
            if (causedByLimit(exception)) {
                return Optional.empty();
            }
            throw exception;
        }
    }

    static void withTargetLock(Path target, Runnable action) {
        Path key = Objects.requireNonNull(target, "target").toAbsolutePath().normalize();
        Runnable requiredAction = Objects.requireNonNull(action, "action");
        TargetLock lock;
        synchronized (TARGET_LOCKS) {
            lock = TARGET_LOCKS.computeIfAbsent(key, ignored -> new TargetLock());
            lock.users++;
        }
        try {
            synchronized (lock.monitor) {
                requiredAction.run();
            }
        } finally {
            synchronized (TARGET_LOCKS) {
                lock.users--;
                if (lock.users == 0) {
                    TARGET_LOCKS.remove(key, lock);
                }
            }
        }
    }

    private static boolean causedByLimit(Throwable throwable) {
        Throwable current = throwable;
        while (current != null) {
            if (current instanceof LimitExceededException) {
                return true;
            }
            current = current.getCause();
        }
        return false;
    }

    private static void registerSensitivePath(Path path) {
        if (path == null) {
            return;
        }
        String value = path.toString();
        FailureTraceReporter.registerSensitiveValue(value);
        FailureTraceReporter.registerSensitiveSourceValue(value);
    }

    private static void delete(Path path) {
        if (path == null) {
            return;
        }
        try {
            Files.deleteIfExists(path);
        } catch (IOException ignored) {
            path.toFile().deleteOnExit();
        }
    }

    private static void validate(List<Entry> entries, long maxBytes) {
        Set<String> paths = new HashSet<>();
        long total = 0;
        for (Entry entry : entries) {
            if (!paths.add(entry.path())) {
                throw new IllegalArgumentException("Mobile evidence archive paths must be unique.");
            }
            total = Math.addExact(total, entry.size());
            if (total > maxBytes) {
                throw new IllegalArgumentException("Mobile evidence archive exceeds its aggregate byte limit.");
            }
        }
    }

    record Entry(String path, byte[] content, Path file, long size) {
        Entry {
            validatePath(path);
            int sources = (content == null ? 0 : 1) + (file == null ? 0 : 1);
            if (sources != 1) {
                throw new IllegalArgumentException("Mobile evidence entries require one content source.");
            }
            if (size < 0 || content != null && size != content.length) {
                throw new IllegalArgumentException("Mobile evidence entry size must match its content.");
            }
            content = content == null ? null : Arrays.copyOf(content, content.length);
        }

        static Entry bytes(String path, byte[] content) {
            byte[] required = Objects.requireNonNull(content, "content");
            return new Entry(path, required, null, required.length);
        }

        static Entry file(String path, Path file, long size) {
            return new Entry(path, null, Objects.requireNonNull(file, "file"), size);
        }

        InputStream open() throws IOException {
            return content == null ? Files.newInputStream(file) : new ByteArrayInputStream(content);
        }

        @Override
        public byte[] content() {
            return content == null ? null : Arrays.copyOf(content, content.length);
        }

        private static void validatePath(String path) {
            if (path == null || path.isBlank() || path.startsWith("/") || path.startsWith("\\")
                    || path.contains("\\") || path.contains(":") || path.endsWith("/")
                    || path.contains("//")) {
                throw new IllegalArgumentException("Mobile evidence archive path must be portable and relative.");
            }
            for (String segment : path.split("/")) {
                if (segment.isBlank() || segment.equals(".") || segment.equals("..")) {
                    throw new IllegalArgumentException("Mobile evidence archive path must not traverse directories.");
                }
            }
        }
    }

    static final class StagedArchive implements AutoCloseable {
        private final Path path;

        private StagedArchive(Path path) {
            this.path = Objects.requireNonNull(path, "path");
        }

        Path path() {
            return path;
        }

        @Override
        public void close() {
            delete(path);
        }
    }

    private static final class TargetLock {
        private final Object monitor = new Object();
        private int users;
    }

    private static final class BoundedOutputStream extends OutputStream {
        private final ByteArrayOutputStream delegate;
        private final int maxBytes;

        private BoundedOutputStream(int maxBytes) {
            this.maxBytes = maxBytes;
            delegate = new ByteArrayOutputStream(Math.min(maxBytes, 16 * 1024));
        }

        @Override
        public void write(int value) throws IOException {
            requireCapacity(1);
            delegate.write(value);
        }

        @Override
        public void write(byte[] value, int offset, int length) throws IOException {
            requireCapacity(length);
            delegate.write(value, offset, length);
        }

        private void requireCapacity(int additional) throws LimitExceededException {
            if (additional < 0 || delegate.size() > maxBytes - additional) {
                throw new LimitExceededException();
            }
        }

        private byte[] toByteArray() {
            return delegate.toByteArray();
        }
    }

    private static final class LimitExceededException extends IOException {
    }
}
