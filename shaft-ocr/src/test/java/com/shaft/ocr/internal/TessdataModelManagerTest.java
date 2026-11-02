package com.shaft.ocr.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;

public class TessdataModelManagerTest {
    @Test
    public void verifiesAndReusesProvisionedModel() throws Exception {
        Path cache = Files.createTempDirectory("shaft-ocr-model-test");
        byte[] model = "verified trained data".getBytes(StandardCharsets.UTF_8);
        Files.write(cache.resolve("eng.traineddata"), model);
        TessdataModelManager manager = manager(cache, sha256(model), false);

        Path tessdata = manager.ensureAvailable(List.of("eng"));
        manager.ensureAvailable(List.of("eng"));

        Assert.assertEquals(Files.readAllBytes(tessdata.resolve("eng.traineddata")), model);
    }

    @Test
    public void checksumMismatchNeverReplacesKnownGoodCache() throws Exception {
        Path cache = Files.createTempDirectory("shaft-ocr-model-test");
        byte[] model = "verified trained data".getBytes(StandardCharsets.UTF_8);
        Path existing = cache.resolve("eng.traineddata");
        byte[] knownGood = "known good".getBytes(StandardCharsets.UTF_8);
        Files.write(existing, knownGood);
        TessdataModelManager manager = manager(cache, sha256(model), true);

        Assert.expectThrows(IllegalStateException.class, () -> manager.ensureAvailable(List.of("eng")));
        Assert.assertEquals(Files.readAllBytes(existing), knownGood);
    }

    @Test
    public void missingModelRequiresApprovedSetupEvenWhenLegacyDownloadFlagIsTrue() throws Exception {
        Path cache = Files.createTempDirectory("shaft-ocr-model-test");
        byte[] model = "verified trained data".getBytes(StandardCharsets.UTF_8);
        TessdataModelManager manager = manager(cache, sha256(model), true);

        IllegalStateException error = Assert.expectThrows(IllegalStateException.class,
                () -> manager.ensureAvailable(List.of("eng")));

        Assert.assertTrue(error.getMessage().contains("eng"));
        Assert.assertTrue(error.getMessage().contains(cache.toString()));
        Assert.assertTrue(error.getMessage().contains("shaft-cli setup plan --profile OCR --language eng"));
        Assert.assertTrue(error.getMessage().contains("no longer bypasses setup approval"));
        Assert.assertFalse(Files.exists(cache.resolve("eng.traineddata")));
    }

    @Test
    public void emptyCustomCacheFallsBackToVerifiedSharedSetupCache() throws Exception {
        Path custom = Files.createTempDirectory("shaft-ocr-custom-test");
        Path shared = Files.createTempDirectory("shaft-ocr-shared-test");
        byte[] model = "verified shared model".getBytes(StandardCharsets.UTF_8);
        Files.write(shared.resolve("eng.traineddata"), model);
        TessdataModelManager manager = new TessdataModelManager(custom, shared,
                URI.create("https://example.invalid/"), false, Map.of("eng", sha256(model)),
                TessdataModelManager.IntegrityAlgorithm.SHA256);

        Assert.assertEquals(manager.ensureAvailable(List.of("eng")), shared);
        Assert.assertFalse(Files.exists(custom.resolve("eng.traineddata")));
    }

    @Test
    public void partialCustomCacheFallsBackToCompleteVerifiedSharedCache() throws Exception {
        Path custom = Files.createTempDirectory("shaft-ocr-custom-test");
        Path shared = Files.createTempDirectory("shaft-ocr-shared-test");
        byte[] english = "english".getBytes(StandardCharsets.UTF_8);
        byte[] french = "french".getBytes(StandardCharsets.UTF_8);
        Files.write(custom.resolve("eng.traineddata"), english);
        Files.write(shared.resolve("eng.traineddata"), english);
        Files.write(shared.resolve("fra.traineddata"), french);
        TessdataModelManager manager = new TessdataModelManager(custom, shared,
                URI.create("https://example.invalid/"), false,
                Map.of("eng", sha256(english), "fra", sha256(french)),
                TessdataModelManager.IntegrityAlgorithm.SHA256);

        Assert.assertEquals(manager.ensureAvailable(List.of("eng", "fra")), shared);
    }

    private TessdataModelManager manager(Path cache, String checksum, boolean downloadsEnabled) {
        URI baseUri = URI.create("https://example.invalid/");
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
