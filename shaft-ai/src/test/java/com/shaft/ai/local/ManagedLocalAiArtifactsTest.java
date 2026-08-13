package com.shaft.ai.local;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.time.Duration;
import java.util.concurrent.atomic.AtomicInteger;
import java.io.InputStream;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.zip.GZIPOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedLocalAiArtifactsTest {
    @TempDir
    Path temp;

    @Test
    void publishesOnlyAnExactVerifiedDownload() throws Exception {
        byte[] content = "verified-runtime".getBytes(StandardCharsets.UTF_8);
        Path target = temp.resolve("downloads/runtime.zip");

        ManagedLocalAiArtifacts.download(new ByteArrayInputStream(content), content.length, sha256(content),
                target, () -> false);

        assertArrayEquals(content, Files.readAllBytes(target));
        assertFalse(Files.list(target.getParent()).anyMatch(path -> path.getFileName().toString().contains(".part-")));
    }

    @Test
    void rejectsShortOversizedChangedAndCancelledDownloadsWithoutPublication() throws Exception {
        byte[] content = "verified-runtime".getBytes(StandardCharsets.UTF_8);
        Path target = temp.resolve("downloads/runtime.zip");

        assertThrows(IllegalArgumentException.class, () -> ManagedLocalAiArtifacts.download(
                new ByteArrayInputStream(content), content.length + 1, sha256(content), target, () -> false));
        assertThrows(IllegalArgumentException.class, () -> ManagedLocalAiArtifacts.download(
                new ByteArrayInputStream(content), content.length - 1, sha256(content), target, () -> false));
        assertThrows(IllegalArgumentException.class, () -> ManagedLocalAiArtifacts.download(
                new ByteArrayInputStream(content), content.length, "0".repeat(64), target, () -> false));
        AtomicBoolean cancelled = new AtomicBoolean(true);
        assertThrows(InterruptedException.class, () -> ManagedLocalAiArtifacts.download(
                new ByteArrayInputStream(content), content.length, sha256(content), target, cancelled::get));

        assertFalse(Files.exists(target));

        Files.createDirectories(target.getParent());
        Files.writeString(target, "existing");
        assertThrows(IllegalArgumentException.class,
                () -> ManagedLocalAiArtifacts.download(new ByteArrayInputStream(content), content.length,
                        sha256(content), target, () -> false));
        assertTrue(Files.readString(target).equals("existing"));
    }

    @Test
    void followsOnlyBoundedReviewedHttpsRedirects() throws Exception {
        byte[] content = "verified-runtime".getBytes(StandardCharsets.UTF_8);
        URI source = URI.create("https://github.com/ggml-org/llama.cpp/releases/download/b1/runtime.zip");
        URI storage = URI.create("https://release-assets.githubusercontent.com/signed-runtime");
        AtomicInteger calls = new AtomicInteger();
        ManagedLocalAiArtifacts.DownloadTransport transport = (uri, ignored) -> {
            if (calls.getAndIncrement() == 0) {
                return response(302, source, Map.of("location", List.of(storage.toString())), new byte[0]);
            }
            return response(200, storage, Map.of("content-length", List.of(String.valueOf(content.length))),
                    content);
        };

        Path target = temp.resolve("https/runtime.zip");
        ManagedLocalAiArtifacts.download(source, content.length, sha256(content), target, Duration.ofSeconds(5),
                () -> false, transport);
        assertArrayEquals(content, Files.readAllBytes(target));

        ManagedLocalAiArtifacts.DownloadTransport foreign = (uri, ignored) -> response(302, source,
                Map.of("location", List.of("https://attacker.invalid/runtime.zip")), new byte[0]);
        assertThrows(IllegalArgumentException.class, () -> ManagedLocalAiArtifacts.download(source, content.length,
                sha256(content), temp.resolve("foreign.zip"), Duration.ofSeconds(5), () -> false, foreign));
        assertThrows(IllegalArgumentException.class, () -> ManagedLocalAiArtifacts.download(
                URI.create("http://github.com/runtime.zip"), content.length, sha256(content),
                temp.resolve("http.zip"), Duration.ofSeconds(5), () -> false, foreign));
        assertThrows(IllegalArgumentException.class, () -> ManagedLocalAiArtifacts.download(
                URI.create("https://github.com/other/project/releases/download/v1/runtime.zip"), content.length,
                sha256(content), temp.resolve("same-host.zip"), Duration.ofSeconds(5), () -> false, foreign));

        assertThrows(IllegalArgumentException.class, () -> ManagedLocalAiArtifacts.download(
                URI.create("https://github.com/ggml-org/llama.cpp/releases/download/b999/runtime.zip"),
                content.length, sha256(content), temp.resolve("unreviewed.zip"), Duration.ofSeconds(5),
                () -> false));
    }

    @Test
    void boundsAStalledResponseBody() throws Exception {
        byte[] content = "verified-runtime".getBytes(StandardCharsets.UTF_8);
        URI source = URI.create("https://github.com/ggml-org/llama.cpp/releases/download/b1/runtime.zip");
        InputStream stalled = new InputStream() {
            @Override
            public int read() throws java.io.IOException {
                try {
                    Thread.sleep(60_000);
                    return -1;
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    throw new java.io.InterruptedIOException("cancelled");
                }
            }
        };
        ManagedLocalAiArtifacts.DownloadTransport transport = (uri, ignored) ->
                new ManagedLocalAiArtifacts.DownloadResponse(200, source, Map.of(), stalled);

        long started = System.nanoTime();
        assertThrows(java.io.IOException.class, () -> ManagedLocalAiArtifacts.download(source, content.length,
                sha256(content), temp.resolve("stalled.zip"), Duration.ofMillis(100), () -> false, transport));
        assertTrue(Duration.ofNanos(System.nanoTime() - started).compareTo(Duration.ofSeconds(2)) < 0);

        AtomicBoolean cancelled = new AtomicBoolean();
        Thread canceller = Thread.ofVirtual().start(() -> {
            try {
                Thread.sleep(100);
                cancelled.set(true);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
            }
        });
        started = System.nanoTime();
        assertThrows(InterruptedException.class, () -> ManagedLocalAiArtifacts.download(source, content.length,
                sha256(content), temp.resolve("cancelled-stall.zip"), Duration.ofSeconds(30), cancelled::get,
                transport));
        canceller.join();
        assertTrue(Duration.ofNanos(System.nanoTime() - started).compareTo(Duration.ofSeconds(2)) < 0);
    }

    @Test
    void extractsValidatedZipAndTarGzipIntoVerifiedUniqueStages() throws Exception {
        Path zipTarget = temp.resolve("runtime-zip");
        zipTarget = ManagedLocalAiArtifacts.extractToStage(write("runtime.zip", zip("bin/llama-server", "binary")),
                zipTarget, () -> false);
        assertTrue(Files.isRegularFile(zipTarget.resolve("bin/llama-server")));
        assertTrue(Files.isRegularFile(zipTarget.resolve(".shaft-ready")));

        Path tarTarget = temp.resolve("runtime-tar");
        tarTarget = ManagedLocalAiArtifacts.extractToStage(
                write("runtime.tar.gz", tarGzip("bin/llama-server", "binary")), tarTarget, () -> false);
        assertTrue(Files.isRegularFile(tarTarget.resolve("bin/llama-server")));
        if (Files.getFileStore(tarTarget).supportsFileAttributeView("posix")) {
            assertTrue(Files.isExecutable(tarTarget.resolve("bin/llama-server")));
        }
    }

    @Test
    void rejectsTraversalLinksAndArchiveBombsWithoutPublishing() throws Exception {
        Path traversal = write("traversal.zip", zip("../outside", "bad"));
        assertThrows(IllegalArgumentException.class,
                () -> ManagedLocalAiArtifacts.extractToStage(traversal, temp.resolve("traversal"), () -> false));
        assertFalse(Files.exists(temp.resolve("outside")));

        byte[] linkTar = tarGzipLink("bin/llama-server", "../../outside");
        assertThrows(IllegalArgumentException.class,
                () -> ManagedLocalAiArtifacts.extractToStage(write("link.tar.gz", linkTar), temp.resolve("link"),
                        () -> false));

        byte[] bomb = zip("huge", "x".repeat(2_000_000));
        assertThrows(IllegalArgumentException.class,
                () -> ManagedLocalAiArtifacts.extractToStage(write("bomb.zip", bomb), temp.resolve("bomb"),
                        () -> false));
        assertFalse(Files.exists(temp.resolve("bomb")));

        assertThrows(IllegalArgumentException.class, () -> ManagedLocalAiArtifacts.extractToStage(
                write("collision.zip", zipEntries("bin/Server", "one", "BIN/server", "two")),
                temp.resolve("collision"), () -> false));
    }

    private Path write(String name, byte[] value) throws Exception {
        Path path = temp.resolve(name);
        Files.write(path, value);
        return path;
    }

    private static byte[] zip(String name, String content) throws Exception {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        try (ZipOutputStream zip = new ZipOutputStream(bytes)) {
            zip.putNextEntry(new ZipEntry(name));
            zip.write(content.getBytes(StandardCharsets.UTF_8));
            zip.closeEntry();
        }
        return bytes.toByteArray();
    }

    private static byte[] zipEntries(String firstName, String firstContent, String secondName,
                                     String secondContent) throws Exception {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        try (ZipOutputStream zip = new ZipOutputStream(bytes)) {
            for (String[] entry : List.of(new String[]{firstName, firstContent},
                    new String[]{secondName, secondContent})) {
                zip.putNextEntry(new ZipEntry(entry[0]));
                zip.write(entry[1].getBytes(StandardCharsets.UTF_8));
                zip.closeEntry();
            }
        }
        return bytes.toByteArray();
    }

    private static byte[] tarGzip(String name, String content) throws Exception {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        try (TarArchiveOutputStream tar = new TarArchiveOutputStream(new GZIPOutputStream(bytes))) {
            byte[] value = content.getBytes(StandardCharsets.UTF_8);
            TarArchiveEntry entry = new TarArchiveEntry(name);
            entry.setSize(value.length);
            tar.putArchiveEntry(entry);
            tar.write(value);
            tar.closeArchiveEntry();
        }
        return bytes.toByteArray();
    }

    private static byte[] tarGzipLink(String name, String target) throws Exception {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        try (TarArchiveOutputStream tar = new TarArchiveOutputStream(new GZIPOutputStream(bytes))) {
            TarArchiveEntry entry = new TarArchiveEntry(name, TarArchiveEntry.LF_SYMLINK);
            entry.setLinkName(target);
            tar.putArchiveEntry(entry);
            tar.closeArchiveEntry();
        }
        return bytes.toByteArray();
    }

    private static String sha256(byte[] value) throws Exception {
        return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(value));
    }

    private static ManagedLocalAiArtifacts.DownloadResponse response(int status, URI uri,
                                                                     Map<String, List<String>> headers,
                                                                     byte[] body) {
        return new ManagedLocalAiArtifacts.DownloadResponse(status, uri, headers, new ByteArrayInputStream(body));
    }
}
