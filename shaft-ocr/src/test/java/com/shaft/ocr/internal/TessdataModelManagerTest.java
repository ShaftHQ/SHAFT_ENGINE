package com.shaft.ocr.internal;

import com.sun.net.httpserver.HttpServer;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

public class TessdataModelManagerTest {
    private HttpServer server;
    private Path cache;
    private byte[] model;
    private AtomicInteger requests;

    @BeforeMethod
    public void setUp() throws Exception {
        cache = Files.createTempDirectory("shaft-ocr-model-test");
        model = "verified trained data".getBytes(StandardCharsets.UTF_8);
        requests = new AtomicInteger();
        server = HttpServer.create(new InetSocketAddress(0), 0);
        server.createContext("/eng.traineddata", exchange -> {
            requests.incrementAndGet();
            exchange.sendResponseHeaders(200, model.length);
            exchange.getResponseBody().write(model);
            exchange.close();
        });
        server.start();
    }

    @AfterMethod
    public void tearDown() throws Exception {
        if (server != null) {
            server.stop(0);
        }
        try (var paths = Files.walk(cache)) {
            paths.sorted((left, right) -> right.compareTo(left)).forEach(path -> {
                try {
                    Files.deleteIfExists(path);
                } catch (Exception ignored) {
                    // Best-effort cleanup of a test-owned temporary directory.
                }
            });
        }
    }

    @Test
    public void downloadsVerifiesAndReusesCachedModel() throws Exception {
        TessdataModelManager manager = manager(sha256(model), true);

        Path tessdata = manager.ensureAvailable(List.of("eng"));
        manager.ensureAvailable(List.of("eng"));

        Assert.assertEquals(Files.readAllBytes(tessdata.resolve("eng.traineddata")), model);
        Assert.assertEquals(requests.get(), 1);
    }

    @Test
    public void checksumMismatchNeverReplacesKnownGoodCache() throws Exception {
        Path existing = cache.resolve("eng.traineddata");
        byte[] knownGood = "known good".getBytes(StandardCharsets.UTF_8);
        Files.write(existing, knownGood);
        TessdataModelManager manager = manager(sha256(model), true);

        Assert.expectThrows(IllegalStateException.class, () -> manager.ensureAvailable(List.of("eng")));
        Assert.assertEquals(Files.readAllBytes(existing), knownGood);
    }

    @Test
    public void offlineFailureNamesLanguageAndCachePath() {
        TessdataModelManager manager = manager(sha256(model), false);

        IllegalStateException error = Assert.expectThrows(IllegalStateException.class,
                () -> manager.ensureAvailable(List.of("eng")));

        Assert.assertTrue(error.getMessage().contains("eng"));
        Assert.assertTrue(error.getMessage().contains(cache.toString()));
    }

    @Test
    public void concurrentCallersShareOneAtomicDownload() throws Exception {
        TessdataModelManager manager = manager(sha256(model), true);
        try (var executor = Executors.newFixedThreadPool(2)) {
            var first = executor.submit(() -> manager.ensureAvailable(List.of("eng")));
            var second = executor.submit(() -> manager.ensureAvailable(List.of("eng")));
            Assert.assertEquals(first.get(), cache);
            Assert.assertEquals(second.get(), cache);
        }
        Assert.assertEquals(requests.get(), 1);
    }

    private TessdataModelManager manager(String checksum, boolean downloadsEnabled) {
        URI baseUri = URI.create("http://127.0.0.1:" + server.getAddress().getPort() + "/");
        return new TessdataModelManager(cache, baseUri, downloadsEnabled, Map.of("eng", checksum));
    }

    private static String sha256(byte[] bytes) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (Exception exception) {
            throw new IllegalStateException(exception);
        }
    }
}
