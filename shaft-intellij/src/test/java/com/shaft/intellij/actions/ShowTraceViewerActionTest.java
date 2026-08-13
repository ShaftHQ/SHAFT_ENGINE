package com.shaft.intellij.actions;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.stream.Stream;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Exercises only {@link ShowTraceViewerAction#resolveLatestTraceViewer(Path)} -- the pure
 * filesystem resolution logic, with no IDE/JCEF dependency -- so it runs headless like the rest
 * of this plugin's test suite.
 */
class ShowTraceViewerActionTest {
    @TempDir
    Path project;
    private ShowTraceViewerAction.GeneratedViewerCache cache;

    @org.junit.jupiter.api.BeforeEach
    void createCache() throws IOException {
        cache = ShowTraceViewerAction.generatedViewerCacheForTests();
    }

    @org.junit.jupiter.api.AfterEach
    void closeCache() throws IOException {
        cache.close();
    }

    @Test
    void returnsNullWhenNoTracesExist() throws IOException {
        assertNull(ShowTraceViewerAction.resolveLatestTraceViewer(project));
    }

    @Test
    void resolvesLooseHtmlWhenAlreadyPresent() throws IOException {
        Path traceDirectory = writeTraceIndex("only-trace", Instant.parse("2026-07-08T10:00:00Z"));
        Path html = traceDirectory.resolve("SHAFT Trace Report.html");
        Files.writeString(html, "<html>loose</html>");
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(traceDirectory.resolve("shaft-trace.zip")))) {
            zip.putNextEntry(new ZipEntry("../hostile"));
            zip.write(1);
            zip.closeEntry();
        }

        Path resolved = ShowTraceViewerAction.resolveLatestTraceViewer(project);

        assertEquals(html, resolved);
        assertEquals("<html>loose</html>", Files.readString(resolved));
    }

    @Test
    void rejectsFakeEocdSignatureInsideZipComment() throws IOException {
        Path archive = project.resolve("fake-eocd.zip");
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
            for (int index = 0; index < 10_001; index++) {
                zip.putNextEntry(new ZipEntry("entry-" + index));
                zip.closeEntry();
            }
        }
        patchTerminalEocdCounts(archive, 1);

        IOException error = assertThrows(
                IOException.class, () -> ShowTraceViewerAction.verifyCentralDirectoryEntryCount(archive));

        assertTrue(error.getMessage().contains("too many"), error.getMessage());
    }

    @Test
    void rejectsDigestCollisionAndBoundsGeneratedViewerCache() throws IOException {
        Path traceDirectory = writeTraceIndex("cache-trust", Instant.parse("2026-07-08T10:00:00Z"));
        Path archive = traceDirectory.resolve("shaft-trace.zip");
        byte[] expectedViewer = "<html>viewer-0</html>".getBytes(java.nio.charset.StandardCharsets.UTF_8);
        writeTraceZip(archive, new String(expectedViewer, java.nio.charset.StandardCharsets.UTF_8));
        Path cacheDirectory = cache.directory();
        Path collision = cacheDirectory.resolve(".shaft-trace-viewer-" + sha256(expectedViewer) + ".html");
        Files.writeString(collision, "hostile");
        Path conflictingCandidate = Files.createTempFile(cacheDirectory, ".candidate-", ".html");
        Files.write(conflictingCandidate, expectedViewer);

        assertThrows(
                IOException.class,
                () -> cache.publish(conflictingCandidate));

        Path externalCandidate = Files.createTempFile(project, "external-candidate-", ".html");
        Files.writeString(externalCandidate, "external");
        IOException outside = assertThrows(IOException.class, () -> cache.publish(externalCandidate));
        assertEquals("Generated trace viewer candidate is outside the owned cache.", outside.getMessage());
        assertEquals("external", Files.readString(externalCandidate));

        Files.delete(collision);
        for (int generation = 0; generation < 4; generation++) {
            Path candidate = Files.createTempFile(cacheDirectory, ".candidate-", ".html");
            Files.writeString(candidate, "<html>viewer-" + generation + "</html>");
            Path resolved = cache.publish(candidate);
            assertEquals("<html>viewer-" + generation + "</html>", Files.readString(resolved));
        }
        try (Stream<Path> files = Files.list(cacheDirectory)) {
            assertTrue(files.filter(path -> path.getFileName().toString()
                            .matches("\\.shaft-trace-viewer-[0-9a-f]{64}\\.html"))
                    .count() <= 4);
        }
    }

    @Test
    void extractsHtmlFromTraceZipWhenNoLooseCopyExists() throws IOException {
        Path traceDirectory = writeTraceIndex("zipped-trace", Instant.parse("2026-07-08T10:00:00Z"));
        writeTraceZip(traceDirectory.resolve("shaft-trace.zip"), "<html>from-zip</html>");

        Path resolved = ShowTraceViewerAction.resolveLatestTraceViewer(project);

        assertTrue(resolved.getFileName().toString().matches("\\.shaft-trace-viewer-[0-9a-f]{64}\\.html"));
        assertEquals("<html>from-zip</html>", Files.readString(resolved));
        assertTrue(Files.notExists(traceDirectory.resolve("SHAFT Trace Report.html")));
    }

    @Test
    void rejectsOversizedAndHostileViewerArchivesWithoutPublishingHtml() throws IOException {
        Path oversized = writeTraceIndex("oversized", Instant.parse("2026-07-08T10:00:00Z"));
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(oversized.resolve("shaft-trace.zip")))) {
            zip.putNextEntry(new ZipEntry("SHAFT Trace Report.html"));
            byte[] block = new byte[8192];
            for (int written = 0; written <= 64 * 1024 * 1024; written += block.length) zip.write(block);
            zip.closeEntry();
        }

        IOException oversizedViewer = assertThrows(
                IOException.class, () -> ShowTraceViewerAction.resolveLatestTraceViewer(project, cache));
        assertEquals("Trace viewer exceeds the extraction limit.", oversizedViewer.getMessage());
        assertTrue(Files.notExists(oversized.resolve("SHAFT Trace Report.html")));

        for (String hostileName : new String[] {
            "../SHAFT Trace Report.html",
            "/SHAFT Trace Report.html",
            "C:/SHAFT Trace Report.html",
            "folder\\SHAFT Trace Report.html",
            "./SHAFT Trace Report.html",
            "folder//SHAFT Trace Report.html"
        }) {
            Files.deleteIfExists(oversized.resolve("shaft-trace.zip"));
            try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(oversized.resolve("shaft-trace.zip")))) {
                zip.putNextEntry(new ZipEntry(hostileName));
                zip.write("hostile".getBytes(java.nio.charset.StandardCharsets.UTF_8));
                zip.closeEntry();
                zip.putNextEntry(new ZipEntry("SHAFT Trace Report.html"));
                zip.write("valid-looking".getBytes(java.nio.charset.StandardCharsets.UTF_8));
                zip.closeEntry();
            }

            assertThrows(IOException.class, () -> ShowTraceViewerAction.resolveLatestTraceViewer(project, cache), hostileName);
            assertTrue(Files.notExists(oversized.resolve("SHAFT Trace Report.html")), hostileName);
        }
    }

    @Test
    void rejectsOversizedIgnoredEntriesAndPortableNameConflicts() throws IOException {
        Path traceDirectory = writeTraceIndex("archive-trust", Instant.parse("2026-07-08T10:00:00Z"));
        Path archive = traceDirectory.resolve("shaft-trace.zip");
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
            zip.putNextEntry(new ZipEntry("ignored.bin"));
            byte[] block = new byte[8192];
            for (int written = 0; written <= 128 * 1024 * 1024; written += block.length) zip.write(block);
            zip.closeEntry();
            zip.putNextEntry(new ZipEntry("SHAFT Trace Report.html"));
            zip.write("valid-looking".getBytes(java.nio.charset.StandardCharsets.UTF_8));
            zip.closeEntry();
        }

        IOException uncompressed = assertThrows(
                IOException.class, () -> ShowTraceViewerAction.resolveLatestTraceViewer(project, cache));
        assertEquals("Trace archive exceeds the uncompressed-size limit.", uncompressed.getMessage());
        assertNoPublishedViewerOrTemporaryFile(traceDirectory);

        Files.delete(archive);
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
            zip.putNextEntry(new ZipEntry("SHAFT Trace Report.html"));
            zip.write("valid-looking".getBytes(java.nio.charset.StandardCharsets.UTF_8));
            zip.closeEntry();
            zip.putNextEntry(new ZipEntry("RESOURCE"));
            zip.write(1);
            zip.closeEntry();
            zip.putNextEntry(new ZipEntry("resource/"));
            zip.closeEntry();
        }

        assertThrows(IOException.class, () -> ShowTraceViewerAction.resolveLatestTraceViewer(project, cache));
        assertNoPublishedViewerOrTemporaryFile(traceDirectory);
    }

    @Test
    void concurrentGeneratedViewerPublicationHonorsHardCacheLimit() throws Exception {
        Path traceDirectory = writeTraceIndex("concurrent-cache", Instant.parse("2026-07-08T10:00:00Z"));
        CompletableFuture<?>[] publishers = new CompletableFuture[6];
        AtomicInteger successes = new AtomicInteger();
        AtomicInteger protectedFailures = new AtomicInteger();
        for (int generation = 0; generation < publishers.length; generation++) {
            int id = generation;
            publishers[generation] = CompletableFuture.runAsync(() -> {
                try {
                    Path candidate = Files.createTempFile(
                            cache.directory(), ".candidate-", ".html");
                    Files.writeString(candidate, "<html>concurrent-" + id + "</html>");
                    Path published = cache.publish(candidate);
                    assertEquals("<html>concurrent-" + id + "</html>", Files.readString(published));
                    successes.incrementAndGet();
                } catch (IOException expectedAtProtectedLimit) {
                    assertEquals("Generated trace viewer cache is at its protected limit.", expectedAtProtectedLimit.getMessage());
                    protectedFailures.incrementAndGet();
                }
            });
        }
        CompletableFuture.allOf(publishers).join();

        assertEquals(4, successes.get());
        assertEquals(2, protectedFailures.get());
        try (Stream<Path> files = Files.list(cache.directory())) {
            var contents = files.map(path -> path.getFileName().toString()).toList();
            assertEquals(5, contents.size());
            assertTrue(contents.contains(".owner.lock"));
            assertEquals(4, contents.stream()
                    .filter(name -> name.matches("\\.shaft-trace-viewer-[0-9a-f]{64}\\.html"))
                    .count());
            assertTrue(contents.stream().noneMatch(name -> name.contains("candidate") || name.endsWith(".tmp")));
        }
        try (Stream<Path> files = Files.list(traceDirectory)) {
            assertTrue(files.noneMatch(path -> path.getFileName().toString().startsWith(".shaft-trace-")));
        }
    }

    @Test
    void rejectsCompressedSizeAndEntryCountBeforeExtraction() throws IOException {
        Path traceDirectory = writeTraceIndex("archive-bounds", Instant.parse("2026-07-08T10:00:00Z"));
        Path archive = traceDirectory.resolve("shaft-trace.zip");
        writeTraceZip(archive, "valid-looking");
        try (RandomAccessFile file = new RandomAccessFile(archive.toFile(), "rw")) {
            file.setLength(128L * 1024 * 1024 + 1);
        }
        Path copied = traceDirectory.resolve("bounded-copy.zip");
        IOException compressed = assertThrows(
                IOException.class, () -> ShowTraceViewerAction.copyArchive(archive, copied));
        assertEquals("Trace archive exceeds the compressed-size limit.", compressed.getMessage());
        Files.deleteIfExists(copied);
        assertThrows(IOException.class, () -> ShowTraceViewerAction.resolveLatestTraceViewer(project, cache));
        assertNoPublishedViewerOrTemporaryFile(traceDirectory);

        Files.delete(archive);
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
            zip.putNextEntry(new ZipEntry("SHAFT Trace Report.html"));
            zip.write("valid-looking".getBytes(java.nio.charset.StandardCharsets.UTF_8));
            zip.closeEntry();
            for (int index = 0; index < 10_000; index++) {
                zip.putNextEntry(new ZipEntry("entry-" + index));
                zip.closeEntry();
            }
        }
        assertThrows(IOException.class, () -> ShowTraceViewerAction.resolveLatestTraceViewer(project, cache));
        assertNoPublishedViewerOrTemporaryFile(traceDirectory);
    }

    @Test
    void selectsTheNewestTraceByGeneratedAt() throws IOException {
        Path older = writeTraceIndex("older", Instant.parse("2026-07-08T09:00:00Z"));
        writeTraceZip(older.resolve("shaft-trace.zip"), "<html>older</html>");
        Path newer = writeTraceIndex("newer", Instant.parse("2026-07-08T11:00:00Z"));
        writeTraceZip(newer.resolve("shaft-trace.zip"), "<html>newer</html>");

        Path resolved = ShowTraceViewerAction.resolveLatestTraceViewer(project);

        assertEquals("<html>newer</html>", Files.readString(resolved));
    }

    @Test
    void fallsBackToFileModifiedTimeWhenGeneratedAtIsMissing() throws IOException {
        Path traceDirectory = project.resolve("target").resolve("shaft-traces").resolve("no-timestamp");
        Files.createDirectories(traceDirectory);
        Files.writeString(traceDirectory.resolve("index.json"), "{\"testId\": \"no-timestamp\"}");
        Files.writeString(traceDirectory.resolve("SHAFT Trace Report.html"), "<html>fallback</html>");
        Files.setLastModifiedTime(traceDirectory.resolve("index.json"), FileTime.from(Instant.now()));

        Path resolved = ShowTraceViewerAction.resolveLatestTraceViewer(project);

        assertEquals(traceDirectory.resolve("SHAFT Trace Report.html"), resolved);
    }

    private Path writeTraceIndex(String testId, Instant generatedAt) throws IOException {
        Path traceDirectory = project.resolve("target").resolve("shaft-traces").resolve(testId);
        Files.createDirectories(traceDirectory);
        Files.writeString(traceDirectory.resolve("index.json"), """
                {
                  "testId": "%s",
                  "generatedAt": "%s",
                  "archive": "target/shaft-traces/%s/shaft-trace.zip",
                  "entries": {"html": "SHAFT Trace Report.html", "json": "shaft-trace.json", "network": "shaft-network.har"}
                }
                """.formatted(testId, generatedAt, testId));
        return traceDirectory;
    }

    private void writeTraceZip(Path zipPath, String html) throws IOException {
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(zipPath))) {
            zip.putNextEntry(new ZipEntry("SHAFT Trace Report.html"));
            zip.write(html.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            zip.closeEntry();
        }
    }

    private void assertNoPublishedViewerOrTemporaryFile(Path traceDirectory) throws IOException {
        assertTrue(Files.notExists(traceDirectory.resolve("SHAFT Trace Report.html")));
        try (Stream<Path> files = Files.list(traceDirectory)) {
            assertTrue(files.noneMatch(path -> {
                String name = path.getFileName().toString();
                return name.startsWith(".shaft-trace-viewer-") || name.startsWith(".shaft-trace-archive-");
            }));
        }
        try (Stream<Path> files = Files.list(cache.directory())) {
            assertTrue(files.noneMatch(path -> {
                String name = path.getFileName().toString();
                return name.endsWith(".tmp") || name.contains("candidate");
            }));
        }
    }

    @Test
    void fixedSlotsRejectNinthActiveOwnerAndReuseClosedAbandonedSlot() throws Exception {
        Path isolatedTemp = Files.createDirectory(project.resolve("slot-root"));
        ShowTraceViewerAction.GeneratedViewerCache[] active = new ShowTraceViewerAction.GeneratedViewerCache[8];
        try {
            for (int index = 0; index < active.length; index++) {
                active[index] = ShowTraceViewerAction.GeneratedViewerCache.create(isolatedTemp);
            }
            IOException saturated = assertThrows(
                    IOException.class, () -> ShowTraceViewerAction.GeneratedViewerCache.create(isolatedTemp));
            assertEquals("All generated trace viewer cache slots are active.", saturated.getMessage());

            Path abandonedDirectory = active[0].directory();
            Path abandoned = active[0].createTemporary(".shaft-trace-viewer-");
            Files.writeString(abandoned, "abandoned");
            active[0].close();
            active[0] = null;

            ShowTraceViewerAction.GeneratedViewerCache reused = ShowTraceViewerAction.GeneratedViewerCache.create(isolatedTemp);
            try {
                assertEquals(abandonedDirectory, reused.directory());
                assertTrue(Files.notExists(abandoned));
                assertEquals(Files.getOwner(isolatedTemp), Files.getOwner(reused.directory()));
            } finally {
                reused.close();
            }
        } finally {
            for (ShowTraceViewerAction.GeneratedViewerCache owner : active) {
                if (owner != null) owner.close();
            }
        }
    }

    private void patchTerminalEocdCounts(Path archive, int count) throws IOException {
        try (RandomAccessFile file = new RandomAccessFile(archive.toFile(), "rw")) {
            long start = Math.max(0, file.length() - 65_557L);
            for (long position = file.length() - 22; position >= start; position--) {
                file.seek(position);
                if (Integer.reverseBytes(file.readInt()) == 0x06054b50) {
                    file.seek(position + 8);
                    file.write(count & 0xff);
                    file.write((count >>> 8) & 0xff);
                    file.write(count & 0xff);
                    file.write((count >>> 8) & 0xff);
                    return;
                }
            }
        }
        throw new IOException("EOCD not found");
    }

    private String sha256(byte[] bytes) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (NoSuchAlgorithmException impossible) {
            throw new IllegalStateException(impossible);
        }
    }
}
