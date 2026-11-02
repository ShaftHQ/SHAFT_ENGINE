package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

public class TraceArchiveReaderTest {

    @Test
    public void extractShouldPublishTheNamedViewerAndRefuseTraversal() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-reader-");
        Path archive = directory.resolve("shaft-trace.zip");
        Path viewer = directory.resolve("SHAFT Trace Report.html");
        try {
            writeZip(archive, "SHAFT Trace Report.html", "<html>viewer</html>".getBytes(StandardCharsets.UTF_8));

            Assert.assertEquals(TraceArchiveReader.extractNamed(archive, "SHAFT Trace Report.html", viewer, 1024),
                    "<html>viewer</html>".getBytes(StandardCharsets.UTF_8).length);
            Assert.assertEquals(Files.readString(viewer, StandardCharsets.UTF_8), "<html>viewer</html>");

            Path traversalArchive = directory.resolve("traversal.zip");
            writeZip(traversalArchive, "../evil.html", "<html>no</html>".getBytes(StandardCharsets.UTF_8));
            Path escaped = directory.resolve("should-not-exist.html");
            IOException traversal = Assert.expectThrows(IOException.class,
                    () -> TraceArchiveReader.extractNamed(traversalArchive, "../evil.html", escaped, 1024));
            Assert.assertTrue(traversal.getMessage().contains("unsafe"), traversal.getMessage());
            Assert.assertFalse(Files.exists(escaped));
        } finally {
            deleteRecursively(directory);
        }
    }

    @Test
    public void extractShouldRefuseADecompressedBombBeforePublishingHtml() throws Exception {
        Path directory = Files.createTempDirectory("shaft-trace-reader-bomb-");
        Path archive = directory.resolve("bomb.zip");
        Path viewer = directory.resolve("SHAFT Trace Report.html");
        try {
            writeZip(archive, "SHAFT Trace Report.html", new byte[2048]);
            IOException bomb = Assert.expectThrows(IOException.class,
                    () -> TraceArchiveReader.extractNamed(archive, "SHAFT Trace Report.html", viewer, 128));
            Assert.assertTrue(bomb.getMessage().toLowerCase().contains("limit"), bomb.getMessage());
            Assert.assertFalse(Files.exists(viewer));
        } finally {
            deleteRecursively(directory);
        }
    }

    private static void writeZip(Path archive, String name, byte[] bytes) throws IOException {
        try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(archive))) {
            zip.putNextEntry(new ZipEntry(name));
            zip.write(bytes);
            zip.closeEntry();
        }
    }

    private static void deleteRecursively(Path directory) throws IOException {
        try (var paths = Files.walk(directory)) {
            paths.sorted(java.util.Comparator.reverseOrder()).forEach(path -> {
                try {
                    Files.deleteIfExists(path);
                } catch (IOException exception) {
                    throw new java.io.UncheckedIOException(exception);
                }
            });
        }
    }
}
