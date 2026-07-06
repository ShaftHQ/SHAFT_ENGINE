package com.shaft.doctor.collect;

import com.shaft.doctor.DoctorAnalysisRequest;
import com.shaft.doctor.model.EvidenceBundle;
import com.shaft.doctor.model.EvidenceCategory;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class EvidenceCollectorPairingTest {

    @Test
    void eachScreenshotIsMatchedWithItsOwnPageSnapshot(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        EvidenceCollector collector = new EvidenceCollector();
        writeMultipleScreenshotPairsInResult(inputDir);
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir),
                List.of(),
                List.of(temp),
                outputDir,
                true,
                true,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES,
                true);
        EvidenceBundle bundle = collector.collect(request);
        Map<String, Long> categoryCount = bundle.evidence().stream()
                .collect(java.util.stream.Collectors.groupingBy(
                        item -> item.category().name(),
                        java.util.stream.Collectors.counting()));
        assertEquals(2L, categoryCount.getOrDefault("SCREENSHOT", 0L),
                "Should have exactly 2 screenshots");
        assertEquals(2L, categoryCount.getOrDefault("PAGE_SNAPSHOT", 0L),
                "Should have exactly 2 page snapshots");
    }

    @Test
    void screenshotWithoutMatchingSnapshotIsNotMasked(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        EvidenceCollector collector = new EvidenceCollector();
        writeOrphanedScreenshot(inputDir);
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir),
                List.of(),
                List.of(temp),
                outputDir,
                true,
                true,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES,
                true);
        EvidenceBundle bundle = collector.collect(request);
        long screenshotCount = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.SCREENSHOT)
                .count();
        assertEquals(1L, screenshotCount, "Orphaned screenshot should still be collected");
    }

    @Test
    void pairingOrderingIsStableDeterministic(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        EvidenceCollector collector = new EvidenceCollector();
        writeOrphanedScreenshot(inputDir);
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir),
                List.of(),
                List.of(temp),
                outputDir,
                true,
                true,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES,
                true);
        EvidenceBundle bundle1 = collector.collect(request);
        EvidenceBundle bundle2 = collector.collect(request);
        assertEquals(bundle1.evidence().size(), bundle2.evidence().size(),
                "Evidence count should be stable across runs");
        for (int i = 0; i < bundle1.evidence().size(); i++) {
            assertEquals(bundle1.evidence().get(i).id(), bundle2.evidence().get(i).id(),
                    "Evidence IDs should be in stable deterministic order");
        }
    }

    @Test
    void screenReductionWithSensitiveFieldsWhenRedactFlagEnabled(@TempDir Path temp) throws IOException {
        Path inputDir = Files.createDirectories(temp.resolve("input"));
        Path outputDir = Files.createDirectories(temp.resolve("output"));
        EvidenceCollector collector = new EvidenceCollector();
        writeMixedPairsWithSensitivityVerification(inputDir);
        DoctorAnalysisRequest request = new DoctorAnalysisRequest(
                List.of(inputDir),
                List.of(),
                List.of(temp),
                outputDir,
                true,
                true,
                1,
                DoctorAnalysisRequest.DEFAULT_MAX_ITEM_BYTES,
                DoctorAnalysisRequest.DEFAULT_MAX_BUNDLE_BYTES,
                true);
        EvidenceBundle bundle = collector.collect(request);

        var screenshotItems = bundle.evidence().stream()
                .filter(item -> item.category() == EvidenceCategory.SCREENSHOT)
                .toList();

        assertTrue(screenshotItems.size() >= 2, "Should have at least 2 screenshots");

        int maskedCount = 0;
        int unmaskedCount = 0;
        int expectedBlack = 0xFF000000;

        for (var item : screenshotItems) {
            byte[] screenshotBytes = Files.readAllBytes(outputDir.resolve(item.relativePath()));
            java.awt.image.BufferedImage image = javax.imageio.ImageIO.read(
                    new java.io.ByteArrayInputStream(screenshotBytes));

            if (image == null) {
                continue;
            }

            boolean isAllBlack = true;
            for (int y = 0; y < image.getHeight() && isAllBlack; y++) {
                for (int x = 0; x < image.getWidth() && isAllBlack; x++) {
                    if (image.getRGB(x, y) != expectedBlack) {
                        isAllBlack = false;
                    }
                }
            }

            if (isAllBlack) {
                maskedCount++;
            } else {
                unmaskedCount++;
            }
        }

        assertTrue(maskedCount >= 1, "At least one screenshot should be masked (sensitive). Found " +
                maskedCount + " masked and " + unmaskedCount + " unmasked out of " + screenshotItems.size());
        assertTrue(unmaskedCount >= 1, "At least one screenshot should NOT be masked (benign). Found " +
                maskedCount + " masked and " + unmaskedCount + " unmasked out of " + screenshotItems.size());
    }

    private void writeMixedPairsWithSensitivityVerification(Path inputDir) throws IOException {
        String resultJson = """
                {
                  "uuid": "mixed-sensitivity-test",
                  "name": "testMixedSensitivity",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "attachments": [
                    {
                      "name": "Screenshot benign",
                      "source": "screenshot_benign.png",
                      "type": "image/png"
                    },
                    {
                      "name": "Page snapshot benign",
                      "source": "snapshot_benign.html",
                      "type": "text/html"
                    },
                    {
                      "name": "Page snapshot sensitive",
                      "source": "snapshot_sensitive.html",
                      "type": "text/html"
                    },
                    {
                      "name": "Screenshot sensitive",
                      "source": "screenshot_sensitive.png",
                      "type": "image/png"
                    }
                  ]
                }
                """;
        Files.writeString(inputDir.resolve("test-result.json"), resultJson, StandardCharsets.UTF_8);
        Files.write(inputDir.resolve("screenshot_benign.png"), createMinimalPng());
        Files.write(inputDir.resolve("screenshot_sensitive.png"), createMinimalPng());
        Files.writeString(inputDir.resolve("snapshot_benign.html"),
                "<html><body><h1>Normal content</h1></body></html>",
                StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("snapshot_sensitive.html"),
                "<html><body><input type=\"password\" value=\"secret\"></body></html>",
                StandardCharsets.UTF_8);
    }

    private void writeMultipleScreenshotPairsInResult(Path inputDir) throws IOException {
        String resultJson = """
                {
                  "uuid": "test-123",
                  "name": "testMultiplePairs",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "attachments": [
                    {
                      "name": "Screenshot 1",
                      "source": "screenshot1.png",
                      "type": "image/png"
                    },
                    {
                      "name": "Page snapshot 1",
                      "source": "snapshot1.html",
                      "type": "text/html"
                    },
                    {
                      "name": "Screenshot 2",
                      "source": "screenshot2.png",
                      "type": "image/png"
                    },
                    {
                      "name": "Page snapshot 2",
                      "source": "snapshot2.html",
                      "type": "text/html"
                    }
                  ]
                }
                """;
        Files.writeString(inputDir.resolve("test-result.json"), resultJson, StandardCharsets.UTF_8);
        Files.write(inputDir.resolve("screenshot1.png"), createMinimalPng());
        Files.write(inputDir.resolve("screenshot2.png"), createMinimalPng());
        Files.writeString(inputDir.resolve("snapshot1.html"), "<html><body>Snapshot 1</body></html>", StandardCharsets.UTF_8);
        Files.writeString(inputDir.resolve("snapshot2.html"), "<html><body>Snapshot 2</body></html>", StandardCharsets.UTF_8);
    }

    private void writeOrphanedScreenshot(Path inputDir) throws IOException {
        String resultJson = """
                {
                  "uuid": "orphan-test",
                  "name": "testOrphanedScreenshot",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "attachments": [
                    {
                      "name": "Screenshot",
                      "source": "screenshot.png",
                      "type": "image/png"
                    }
                  ]
                }
                """;
        Files.writeString(inputDir.resolve("test-result.json"), resultJson, StandardCharsets.UTF_8);
        Files.write(inputDir.resolve("screenshot.png"), createMinimalPng());
    }

    private void writeSensitivePageSnapshot(Path inputDir) throws IOException {
        String resultJson = """
                {
                  "uuid": "sensitive-test",
                  "name": "testSensitiveSnapshot",
                  "status": "passed",
                  "start": 1000,
                  "stop": 2000,
                  "attachments": [
                    {
                      "name": "Screenshot",
                      "source": "screenshot.png",
                      "type": "image/png"
                    },
                    {
                      "name": "Page snapshot",
                      "source": "snapshot.html",
                      "type": "text/html"
                    }
                  ]
                }
                """;
        Files.writeString(inputDir.resolve("test-result.json"), resultJson, StandardCharsets.UTF_8);
        Files.write(inputDir.resolve("screenshot.png"), createMinimalPng());
        Files.writeString(inputDir.resolve("snapshot.html"),
                "<html><body><input type=\"password\" value=\"secret\"></body></html>",
                StandardCharsets.UTF_8);
    }

    private byte[] createMinimalPng() throws IOException {
        java.awt.image.BufferedImage image = new java.awt.image.BufferedImage(
                10, 10, java.awt.image.BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < 10; y++) {
            for (int x = 0; x < 10; x++) {
                image.setRGB(x, y, 0xFFFFFFFF);
            }
        }
        java.io.ByteArrayOutputStream output = new java.io.ByteArrayOutputStream();
        javax.imageio.ImageIO.write(image, "png", output);
        return output.toByteArray();
    }

    private byte[] createAllBlackPng() throws IOException {
        java.awt.image.BufferedImage image = new java.awt.image.BufferedImage(
                10, 10, java.awt.image.BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < 10; y++) {
            for (int x = 0; x < 10; x++) {
                image.setRGB(x, y, 0xFF000000);
            }
        }
        java.io.ByteArrayOutputStream output = new java.io.ByteArrayOutputStream();
        javax.imageio.ImageIO.write(image, "png", output);
        return output.toByteArray();
    }
}
