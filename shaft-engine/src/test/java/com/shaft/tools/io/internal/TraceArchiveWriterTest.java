package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.File;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.ZipFile;

public class TraceArchiveWriterTest {

    @Test
    public void shouldStreamByteAndFileEntriesIntoACompletedArchive() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-");
        Path target = directory.resolve("shaft-trace.zip");
        Path source = directory.resolve("native-trace.zip");
        Files.writeString(source, "native trace", StandardCharsets.UTF_8);
        try {
            TraceArchiveWriter.write(target, List.of(
                    TraceArchiveWriter.Entry.text("shaft-trace.json", "{\"schemaVersion\":\"2.0\"}"),
                    TraceArchiveWriter.Entry.file("native-trace.zip", source)), 1024, "omitted");

            Assert.assertTrue(Files.isRegularFile(target));
            try (ZipFile zip = new ZipFile(target.toFile())) {
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry("shaft-trace.json")).readAllBytes(),
                        StandardCharsets.UTF_8), "{\"schemaVersion\":\"2.0\"}");
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry("native-trace.zip")).readAllBytes(),
                        StandardCharsets.UTF_8), "native trace");
            }
            Assert.assertEquals(temporaryArchiveCount(directory), 0L,
                    "A completed write must not leave temporary archives behind.");
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void shouldReplaceOversizedEntryWithMarkerAndKeepZipValid() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-cap-");
        Path target = directory.resolve("shaft-trace.zip");
        try {
            TraceArchiveWriter.write(target,
                    List.of(TraceArchiveWriter.Entry.bytes("large.bin", new byte[9])), 8, "bounded omission" );

            try (ZipFile zip = new ZipFile(target.toFile())) {
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry("large.bin")).readAllBytes(),
                        StandardCharsets.UTF_8), "bounded omission");
            }
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void failedWriteShouldPreservePreviousArchiveAndRemoveTemporaryFile() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-failure-");
        Path target = directory.resolve("shaft-trace.zip");
        Path missing = directory.resolve("missing-native-trace.zip");
        Files.writeString(target, "previous archive", StandardCharsets.UTF_8);
        try {
            Assert.expectThrows(IOException.class, () -> TraceArchiveWriter.write(target,
                    List.of(TraceArchiveWriter.Entry.file("native-trace.zip", missing)), 1024, "omitted"));

            Assert.assertEquals(Files.readString(target, StandardCharsets.UTF_8), "previous archive");
            Assert.assertEquals(temporaryArchiveCount(directory), 0L);
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void optionalFileFailureShouldKeepCoreEvidenceAndWriteAnOmissionMarker() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-optional-");
        Path target = directory.resolve("shaft-trace.zip");
        try {
            TraceArchiveWriter.write(target, List.of(
                    TraceArchiveWriter.Entry.text("shaft-trace.json", "core evidence"),
                    TraceArchiveWriter.Entry.optionalFile("playwright-trace.zip", directory.resolve("missing.zip"))),
                    1024, "optional evidence omitted");

            try (ZipFile zip = new ZipFile(target.toFile())) {
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry("shaft-trace.json")).readAllBytes(),
                        StandardCharsets.UTF_8), "core evidence");
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry("playwright-trace.zip")).readAllBytes(),
                        StandardCharsets.UTF_8), "optional evidence omitted");
            }
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void optionalMidStreamFailureShouldContainOnlyTheOmissionMarker() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-optional-stream-");
        Path target = directory.resolve("shaft-trace.zip");
        TraceArchiveWriter.Source failingSource = new TraceArchiveWriter.Source() {
            @Override
            public long size() {
                return 32;
            }

            @Override
            public InputStream open() {
                return new InputStream() {
                    private int emitted;

                    @Override
                    public int read() throws IOException {
                        if (emitted++ < 8) {
                            return 'x';
                        }
                        throw new IOException("forced mid-stream failure");
                    }
                };
            }
        };
        try {
            TraceArchiveWriter.write(target,
                    List.of(TraceArchiveWriter.Entry.optional("playwright-trace.zip", failingSource)),
                    1024, "optional evidence omitted");

            try (ZipFile zip = new ZipFile(target.toFile())) {
                Assert.assertEquals(new String(zip.getInputStream(zip.getEntry("playwright-trace.zip")).readAllBytes(),
                        StandardCharsets.UTF_8), "optional evidence omitted");
            }
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void fallbackPublicationFailureShouldRestoreThePreviousArchive() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-fallback-");
        Path target = directory.resolve("shaft-trace.zip");
        Files.writeString(target, "previous archive", StandardCharsets.UTF_8);
        AtomicInteger nonAtomicTemporaryMoves = new AtomicInteger();
        TraceArchiveWriter.MoveStrategy moves = (source, destination, options) -> {
            boolean atomic = List.of(options).contains(StandardCopyOption.ATOMIC_MOVE);
            if (atomic) {
                throw new java.nio.file.AtomicMoveNotSupportedException(source.toString(), destination.toString(),
                        "forced by test");
            }
            if (source.getFileName().toString().contains(".tmp-") && destination.equals(target.toAbsolutePath())
                    && nonAtomicTemporaryMoves.getAndIncrement() == 0) {
                throw new IOException("forced fallback publication failure");
            }
            Files.move(source, destination, options);
        };
        try {
            Assert.expectThrows(IOException.class, () -> TraceArchiveWriter.write(target,
                    List.of(TraceArchiveWriter.Entry.text("shaft-trace.json", "new archive")),
                    1024, "omitted", moves));

            Assert.assertEquals(Files.readString(target, StandardCharsets.UTF_8), "previous archive");
            Assert.assertEquals(temporaryArchiveCount(directory), 0L);
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void successfulFallbackShouldPublishWithAndWithoutAnExistingTarget() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-fallback-success-");
        TraceArchiveWriter.MoveStrategy nonAtomicMoves = (source, destination, options) -> {
            if (List.of(options).contains(StandardCopyOption.ATOMIC_MOVE)) {
                throw new java.nio.file.AtomicMoveNotSupportedException(source.toString(), destination.toString(),
                        "forced by test");
            }
            Files.move(source, destination, options);
        };
        Path target = directory.resolve("shaft-trace.zip");
        try {
            TraceArchiveWriter.write(target, List.of(TraceArchiveWriter.Entry.text("first.txt", "first")),
                    1024, "omitted", nonAtomicMoves);
            TraceArchiveWriter.write(target, List.of(TraceArchiveWriter.Entry.text("second.txt", "second")),
                    1024, "omitted", nonAtomicMoves);

            try (ZipFile zip = new ZipFile(target.toFile())) {
                Assert.assertNotNull(zip.getEntry("second.txt"));
                Assert.assertNull(zip.getEntry("first.txt"));
            }
            Assert.assertEquals(temporaryArchiveCount(directory), 0L);
            Assert.assertEquals(backupArchiveCount(directory), 0L);
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void noTargetFallbackFailureShouldRemoveAPartiallyPublishedTarget() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-fallback-partial-");
        Path target = directory.resolve("shaft-trace.zip");
        TraceArchiveWriter.MoveStrategy partialMoves = (source, destination, options) -> {
            if (List.of(options).contains(StandardCopyOption.ATOMIC_MOVE)) {
                throw new java.nio.file.AtomicMoveNotSupportedException(source.toString(), destination.toString(),
                        "forced by test");
            }
            Files.writeString(destination, "partial", StandardCharsets.UTF_8);
            throw new IOException("forced partial fallback");
        };
        try {
            Assert.expectThrows(IOException.class, () -> TraceArchiveWriter.write(target,
                    List.of(TraceArchiveWriter.Entry.text("trace.txt", "trace")), 1024, "omitted", partialMoves));

            Assert.assertFalse(Files.exists(target));
            Assert.assertEquals(temporaryArchiveCount(directory), 0L);
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void failedRestorationShouldRetainTheImmutableKnownGoodBackup() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-restore-failure-");
        Path target = directory.resolve("shaft-trace.zip");
        Files.writeString(target, "known good archive", StandardCharsets.UTF_8);
        TraceArchiveWriter.MoveStrategy failingMoves = (source, destination, options) -> {
            if (List.of(options).contains(StandardCopyOption.ATOMIC_MOVE)) {
                throw new java.nio.file.AtomicMoveNotSupportedException(source.toString(), destination.toString(),
                        "forced by test");
            }
            Files.writeString(destination, "partial replacement", StandardCharsets.UTF_8);
            throw new IOException("forced replacement failure");
        };
        TraceArchiveWriter.CopyStrategy failingRestore = (source, destination, options) -> {
            if (source.getFileName().toString().contains(".backup-")) {
                Files.writeString(destination, "partial restoration", StandardCharsets.UTF_8);
                throw new IOException("forced restoration failure");
            }
            Files.copy(source, destination, options);
        };
        try {
            IOException failure = Assert.expectThrows(IOException.class, () -> TraceArchiveWriter.write(target,
                    List.of(TraceArchiveWriter.Entry.text("trace.txt", "new trace")), 1024, "omitted",
                    failingMoves, failingRestore));

            Path backup;
            try (var paths = Files.list(directory)) {
                backup = paths.filter(path -> path.getFileName().toString().contains(".backup-"))
                        .findFirst().orElseThrow();
            }
            Assert.assertEquals(Files.readString(backup, StandardCharsets.UTF_8), "known good archive");
            Assert.assertTrue(List.of(failure.getSuppressed()).stream()
                    .anyMatch(item -> item.getMessage().contains(backup.toString())));
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void copiedArchiveShouldBePublishedThroughTheSameSafeReplacementProtocol() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-copy-");
        Path source = directory.resolve("source.zip");
        Path target = directory.resolve("retained.zip");
        Files.writeString(source, "completed invocation archive", StandardCharsets.UTF_8);
        try {
            TraceArchiveWriter.copy(source, target);

            Assert.assertEquals(Files.readString(target, StandardCharsets.UTF_8), "completed invocation archive");
            Assert.assertEquals(temporaryArchiveCount(directory), 0L);
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void nonAtomicFallbackShouldBackUpASymlinkEntryWithoutReadingItsReferent() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-symlink-");
        Path outside = directory.resolve("outside-secret.txt");
        Path target = directory.resolve("shaft-trace.zip");
        Files.writeString(outside, "outside-secret", StandardCharsets.UTF_8);
        try {
            Files.createSymbolicLink(target, outside);
        } catch (IOException | UnsupportedOperationException unavailable) {
            deleteRecursively(directory);
            throw new org.testng.SkipException("Symbolic links are unavailable on this host", unavailable);
        }
        TraceArchiveWriter.MoveStrategy nonAtomicMoves = (source, destination, options) -> {
            if (List.of(options).contains(StandardCopyOption.ATOMIC_MOVE)) {
                throw new java.nio.file.AtomicMoveNotSupportedException(source.toString(), destination.toString(),
                        "forced by test");
            }
            if (source.equals(target) && destination.getFileName().toString().contains(".backup-")) {
                Files.move(source, destination, options);
                return;
            }
            Path backup;
            try (var paths = Files.list(directory)) {
                backup = paths.filter(path -> path.getFileName().toString().contains(".backup-"))
                        .findFirst().orElseThrow();
            }
            if (!Files.isSymbolicLink(backup)) {
                throw new IOException("Fallback materialized the symlink referent instead of preserving the link entry.");
            }
            Files.move(source, destination, options);
        };
        try {
            TraceArchiveWriter.write(target, List.of(TraceArchiveWriter.Entry.text("trace.txt", "new trace")),
                    1024, "omitted", nonAtomicMoves);

            Assert.assertFalse(Files.isSymbolicLink(target));
            Assert.assertEquals(Files.readString(outside, StandardCharsets.UTF_8), "outside-secret");
            Assert.assertEquals(backupArchiveCount(directory), 0L);
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void failedNonAtomicSymlinkPublicationShouldRestoreTheLinkEntry() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-symlink-restore-");
        Path outside = directory.resolve("outside-secret.txt");
        Path target = directory.resolve("shaft-trace.zip");
        Files.writeString(outside, "outside-secret", StandardCharsets.UTF_8);
        try {
            Files.createSymbolicLink(target, outside);
        } catch (IOException | UnsupportedOperationException unavailable) {
            deleteRecursively(directory);
            throw new org.testng.SkipException("Symbolic links are unavailable on this host", unavailable);
        }
        TraceArchiveWriter.MoveStrategy failingPublication = (source, destination, options) -> {
            if (List.of(options).contains(StandardCopyOption.ATOMIC_MOVE)) {
                throw new java.nio.file.AtomicMoveNotSupportedException(source.toString(), destination.toString(),
                        "forced by test");
            }
            if (source.equals(target) && destination.getFileName().toString().contains(".backup-")) {
                Files.move(source, destination, options);
                return;
            }
            Files.writeString(destination, "partial replacement", StandardCharsets.UTF_8);
            throw new IOException("forced publication failure");
        };
        try {
            Assert.expectThrows(IOException.class, () -> TraceArchiveWriter.write(target,
                    List.of(TraceArchiveWriter.Entry.text("trace.txt", "new trace")), 1024, "omitted",
                    failingPublication));

            Assert.assertTrue(Files.isSymbolicLink(target));
            Assert.assertEquals(Files.readSymbolicLink(target), outside);
            Assert.assertEquals(Files.readString(outside, StandardCharsets.UTF_8), "outside-secret");
            Assert.assertEquals(backupArchiveCount(directory), 0L);
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void largeIncompressibleArchiveShouldCompleteInAConstrainedHeap() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-writer-low-heap-");
        String executable = System.getProperty("os.name", "").toLowerCase().contains("win") ? "java.exe" : "java";
        Path java = Path.of(System.getProperty("java.home"), "bin", executable);
        String classpath = Path.of(TraceArchiveWriterTest.class.getProtectionDomain().getCodeSource()
                        .getLocation().toURI())
                + File.pathSeparator
                + Path.of(TraceArchiveWriter.class.getProtectionDomain().getCodeSource().getLocation().toURI())
                + File.pathSeparator
                + System.getProperty("surefire.test.class.path", System.getProperty("java.class.path"));
        try {
            Process process = new ProcessBuilder(java.toString(), "-Xmx20m", "-cp", classpath,
                    "com.shaft.tools.io.internal.TraceArchiveWriterLowHeapProbe", directory.toString())
                    .redirectErrorStream(true)
                    .start();
            String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
            Assert.assertEquals(process.waitFor(), 0, output);
        } finally {
            deleteRecursively(directory);
        }
    }

    private static long temporaryArchiveCount(Path directory) throws IOException {
        try (var paths = Files.list(directory)) {
            return paths.filter(path -> path.getFileName().toString().contains(".tmp-")).count();
        }
    }

    private static long backupArchiveCount(Path directory) throws IOException {
        try (var paths = Files.list(directory)) {
            return paths.filter(path -> path.getFileName().toString().contains(".backup-")).count();
        }
    }

    private static void deleteRecursively(Path directory) throws IOException {
        if (!Files.exists(directory)) {
            return;
        }
        try (var paths = Files.walk(directory)) {
            paths.sorted((left, right) -> right.compareTo(left)).forEach(path -> {
                try {
                    Files.deleteIfExists(path);
                } catch (IOException ignored) {
                    // Test cleanup must not hide the behavior assertion.
                }
            });
        }
    }
}
