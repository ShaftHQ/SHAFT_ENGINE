package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.HexFormat;
import java.security.MessageDigest;
import java.util.List;
import java.util.zip.ZipFile;

public class TraceArtifactManifestTest {

    @Test
    public void retainingActionsShouldReleaseDroppedPhysicalDomResources() {
        SeleniumTraceCapture.Result first = new SeleniumTraceCapture.Result(
                "webdriver", "structural", "available", "", "action-dom-snapshot", "first", false);
        SeleniumTraceCapture.Result second = new SeleniumTraceCapture.Result(
                "webdriver", "structural", "available", "", "action-dom-snapshot", "second", false);
        List<TraceArtifactManifest.SnapshotResource> snapshots = List.of(
                new TraceArtifactManifest.SnapshotResource("snapshot-action-1-before", "action-1", "before",
                        first, first.content().getBytes(StandardCharsets.UTF_8)),
                new TraceArtifactManifest.SnapshotResource("snapshot-action-2-before", "action-2", "before",
                        second, second.content().getBytes(StandardCharsets.UTF_8)));
        try (TraceArtifactManifest manifest = TraceArtifactManifest.create(
                "[]", Map.of(), snapshots, null, 1024, "omitted")) {
            String retainedPath = manifest.references().stream()
                    .filter(reference -> reference.id().equals("snapshot-action-1-before"))
                    .findFirst().orElseThrow().path();

            manifest.retainActionArtifacts(java.util.Set.of("snapshot-action-1-before"));

            Assert.assertTrue(manifest.references().stream()
                    .anyMatch(reference -> reference.id().equals("snapshot-action-1-before")));
            Assert.assertFalse(manifest.references().stream()
                    .anyMatch(reference -> reference.id().equals("snapshot-action-2-before")));
            Assert.assertEquals(manifest.resourceBytes().keySet(), java.util.Set.of(retainedPath));
        }
    }

    @Test
    public void identicalDomSnapshotsShouldShareExactContentAddressedResource() throws Exception {
        String html = "<html>same state</html>";
        byte[] bytes = html.getBytes(StandardCharsets.UTF_8);
        String sha256 = HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        SeleniumTraceCapture.Result result = new SeleniumTraceCapture.Result(
                "webdriver", "structural", "available", "", "action-dom-snapshot", html, false);
        List<TraceArtifactManifest.SnapshotResource> snapshots = List.of(
                new TraceArtifactManifest.SnapshotResource(
                        "snapshot-action-1-before", "action-1", "before", result, bytes),
                new TraceArtifactManifest.SnapshotResource(
                        "snapshot-action-1-after", "action-1", "after", result, bytes));

        try (TraceArtifactManifest manifest = TraceArtifactManifest.create(
                "[]", Map.of(), snapshots, null, 1024, "omitted")) {
            var references = manifest.references().stream()
                    .filter(item -> item.kind().equals("dom-snapshot")).toList();
            String expectedPath = "resources/" + sha256 + ".html";
            Assert.assertEquals(references.get(0).path(), expectedPath);
            Assert.assertEquals(references.get(1).path(), expectedPath);
            Assert.assertEquals(references.get(0).metadata().get("sha256"), sha256);
            Assert.assertEquals(references.get(0).metadata().get("sizeBytes"), String.valueOf(bytes.length));
            Assert.assertEquals(manifest.resourceBytes().size(), 1);
            Assert.assertEquals(manifest.resourceBytes().get(expectedPath), bytes);

            Path directory = Files.createTempDirectory("shaft-deduplicated-dom-");
            Path archive = directory.resolve("shaft-trace.zip");
            String json = """
                    {"session":{"artifacts":[
                    {"id":"snapshot-action-1-before","path":"%s","omitted":false,"metadata":{}},
                    {"id":"snapshot-action-1-after","path":"%s","omitted":false,"metadata":{}}]}}
                    """.formatted(expectedPath, expectedPath);
            try {
                FailureTraceReporter.convergeTraceArchive(archive, json, "[]", Map.of(), manifest,
                        1024 * 1024, 3L * 1024 * 1024, "omitted", List.of());
                try (ZipFile zip = new ZipFile(archive.toFile())) {
                    Assert.assertEquals(zip.stream().filter(entry -> entry.getName().equals(expectedPath)).count(),
                            1L);
                    Assert.assertEquals(zip.getInputStream(zip.getEntry(expectedPath)).readAllBytes(), bytes);
                }
            } finally {
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
    }

    @Test
    public void identicalScreenshotBytesShouldShareOneContentAddressedArtifactWithIntegrityMetadata() throws Exception {
        byte[] screenshot = "same screenshot".getBytes(StandardCharsets.UTF_8);
        String sha256 = HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(screenshot));

        try (TraceArtifactManifest manifest = TraceArtifactManifest.create("[]",
                Map.of("action-1", screenshot, "action-2", screenshot), null, 1024, "omitted")) {
            var screenshots = manifest.references().stream()
                    .filter(item -> item.kind().equals("screenshot"))
                    .toList();

            Assert.assertEquals(screenshots.size(), 2);
            Assert.assertEquals(screenshots.get(0).path(), "resources/" + sha256 + ".png");
            Assert.assertEquals(screenshots.get(1).path(), screenshots.get(0).path());
            Assert.assertEquals(screenshots.get(0).metadata().get("sha256"), sha256);
            Assert.assertEquals(screenshots.get(0).metadata().get("sizeBytes"),
                    String.valueOf(screenshot.length));

            Path directory = Files.createTempDirectory("shaft-deduplicated-screenshots-");
            Path archive = directory.resolve("shaft-trace.zip");
            String json = """
                    {"session":{"artifacts":[
                    {"id":"screenshot-action-1","path":"%s","omitted":false,"metadata":{}},
                    {"id":"screenshot-action-2","path":"%s","omitted":false,"metadata":{}}]}}
                    """.formatted(screenshots.get(0).path(), screenshots.get(1).path());
            try {
                FailureTraceReporter.convergeTraceArchive(archive, json, "[]",
                        Map.of("action-1", screenshot, "action-2", screenshot), manifest,
                        1024 * 1024, 3L * 1024 * 1024, "omitted", List.of());
                try (ZipFile zip = new ZipFile(archive.toFile())) {
                    Assert.assertEquals(zip.stream().filter(entry -> entry.getName().equals(screenshots.get(0).path()))
                            .count(), 1L);
                    Assert.assertEquals(zip.getInputStream(zip.getEntry(screenshots.get(0).path())).readAllBytes(),
                            screenshot);
                }
            } finally {
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
    }

    @Test
    public void identicalOmittedScreenshotsShouldShareOneMarkerWithoutReasonCollision() throws Exception {
        byte[] screenshot = new byte[128];
        String reason = "per-entry omission";
        try (TraceArtifactManifest manifest = TraceArtifactManifest.create("[]",
                Map.of("action-1", screenshot, "action-2", screenshot), null, 64, reason)) {
            Path directory = Files.createTempDirectory("shaft-deduplicated-omissions-");
            Path archive = directory.resolve("shaft-trace.zip");
            String path = manifest.references().stream().filter(item -> item.kind().equals("screenshot"))
                    .findFirst().orElseThrow().path();
            String json = """
                    {"session":{"artifacts":[
                    {"id":"screenshot-action-1","path":"%s","omitted":true,"metadata":{"omissionReason":"%s"}},
                    {"id":"screenshot-action-2","path":"%s","omitted":true,"metadata":{"omissionReason":"%s"}}]}}
                    """.formatted(path, reason, path, reason);
            try {
                FailureTraceReporter.convergeTraceArchive(archive, json, "[]",
                        Map.of("action-1", screenshot, "action-2", screenshot), manifest,
                        1024 * 1024, 3L * 1024 * 1024, "aggregate omission", List.of(path));
                try (ZipFile zip = new ZipFile(archive.toFile())) {
                    Assert.assertEquals(zip.stream().filter(entry -> entry.getName().equals(path)).count(), 1L);
                    Assert.assertEquals(new String(zip.getInputStream(zip.getEntry(path)).readAllBytes(),
                            StandardCharsets.UTF_8), reason);
                }
            } finally {
                try (var paths = Files.walk(directory)) {
                    paths.sorted(java.util.Comparator.reverseOrder()).forEach(item -> {
                        try {
                            Files.deleteIfExists(item);
                        } catch (IOException exception) {
                            throw new java.io.UncheckedIOException(exception);
                        }
                    });
                }
            }
        }
    }

    @Test
    public void aggregateOmissionShouldPreserveSharedScreenshotIntegrityMetadata() throws Exception {
        byte[] screenshot = "shared aggregate screenshot".getBytes(StandardCharsets.UTF_8);
        String sha256 = HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(screenshot));
        try (TraceArtifactManifest manifest = TraceArtifactManifest.create("[]",
                Map.of("action-1", screenshot, "action-2", screenshot), null, 1024, "per-entry omission")) {
            String path = "resources/" + sha256 + ".png";

            manifest.markOmitted(List.of(path), "aggregate omission");

            var screenshots = manifest.references().stream()
                    .filter(item -> item.kind().equals("screenshot"))
                    .toList();
            Assert.assertEquals(screenshots.size(), 2);
            for (var reference : screenshots) {
                Assert.assertTrue(reference.omitted());
                Assert.assertEquals(reference.path(), path);
                Assert.assertEquals(reference.metadata().get("omissionReason"), "aggregate omission");
                Assert.assertEquals(reference.metadata().get("sha256"), sha256);
                Assert.assertEquals(reference.metadata().get("sizeBytes"), String.valueOf(screenshot.length));
            }
        }
    }

    @Test
    public void shouldFinalizeOmissionStateAndStageNativeTraceBeforeSerialization() throws Exception {
        Path nativeTrace = Files.createTempFile("playwright-native-", ".zip");
        Files.writeString(nativeTrace, "native trace", StandardCharsets.UTF_8);
        TraceArtifactManifest manifest = TraceArtifactManifest.create("[{\"body\":\"large\"}]",
                Map.of("action-1", new byte[12]), nativeTrace, 16, "omitted");
        try {
            var network = manifest.references().stream().filter(item -> item.id().equals("network")).findFirst()
                    .orElseThrow();
            var screenshot = manifest.references().stream().filter(item -> item.id().equals("screenshot-action-1"))
                    .findFirst().orElseThrow();
            var nativeArtifact = manifest.references().stream().filter(item -> item.id().equals("native-trace"))
                    .findFirst().orElseThrow();

            Assert.assertTrue(network.omitted());
            Assert.assertFalse(screenshot.omitted());
            Assert.assertFalse(nativeArtifact.omitted());
            Files.delete(nativeTrace);
            Assert.assertEquals(new String(manifest.nativeEntry().open().readAllBytes(), StandardCharsets.UTF_8),
                    "native trace", "The archive must use the staged source after the producer path disappears.");
        } finally {
            manifest.close();
            Files.deleteIfExists(nativeTrace);
        }
    }

    @Test
    public void oversizedNativeTraceShouldPublishOnlyAnIntentionalMarker() throws Exception {
        Path nativeTrace = Files.createTempFile("playwright-native-large-", ".zip");
        Files.write(nativeTrace, new byte[32]);
        try (TraceArtifactManifest manifest = TraceArtifactManifest.create("[]", Map.of(), nativeTrace, 16,
                "bounded omission")) {
            var nativeArtifact = manifest.references().stream().filter(item -> item.id().equals("native-trace"))
                    .findFirst().orElseThrow();
            Assert.assertTrue(nativeArtifact.omitted());
            Assert.assertEquals(nativeArtifact.metadata().get("omissionReason"), "bounded omission");
            Assert.assertEquals(new String(manifest.nativeEntry().open().readAllBytes(), StandardCharsets.UTF_8),
                    "bounded omission");
        } finally {
            Files.deleteIfExists(nativeTrace);
        }
    }

    @Test
    public void withinCapNetworkShouldRemainAvailable() {
        try (TraceArtifactManifest manifest = TraceArtifactManifest.create("[]", Map.of(), null, 1024, "omitted")) {
            var network = manifest.references().stream().filter(item -> item.id().equals("network")).findFirst()
                    .orElseThrow();
            Assert.assertFalse(network.omitted());
        }
    }

    @Test
    public void advertisedMissingNativeTraceShouldRemainAnExplicitOmittedArtifact() throws Exception {
        Path missing = Path.of("target", "missing-playwright-trace.zip");
        Files.deleteIfExists(missing);
        try (TraceArtifactManifest manifest = TraceArtifactManifest.create("[]", Map.of(), missing, 1024,
                "bounded omission")) {
            var nativeArtifact = manifest.references().stream().filter(item -> item.id().equals("native-trace"))
                    .findFirst().orElseThrow();
            Assert.assertTrue(nativeArtifact.omitted());
            Assert.assertTrue(nativeArtifact.metadata().get("omissionReason").contains("unavailable"));
            Assert.assertTrue(new String(manifest.nativeEntry().open().readAllBytes(), StandardCharsets.UTF_8)
                    .contains("unavailable"));
        }
    }

    @Test
    public void nativeReadFailureShouldKeepMarkerAndCleanPartialStage() throws Exception {
        Path advertised = Path.of("playwright-trace.zip");
        TraceArtifactManifest.NativeTraceSource failing = new TraceArtifactManifest.NativeTraceSource() {
            @Override
            public boolean isRegularFile(Path ignored) {
                return true;
            }

            @Override
            public long size(Path ignored) {
                return 8;
            }

            @Override
            public InputStream open(Path ignored) {
                return new InputStream() {
                    private int count;

                    @Override
                    public int read() throws IOException {
                        if (count++ < 3) {
                            return 'x';
                        }
                        throw new IOException("simulated midstream failure");
                    }
                };
            }
        };
        long before = stagedNativeFiles();

        try (TraceArtifactManifest manifest = TraceArtifactManifest.create("[]", Map.of(), advertised, 1024,
                "bounded omission", failing)) {
            Assert.assertEquals(stagedNativeFiles(), before);
            Assert.assertEquals(manifest.omittedPaths(), java.util.List.of("playwright-trace.zip"));
            Assert.assertEquals(manifest.references().stream()
                            .filter(item -> item.id().equals("native-trace")).findFirst().orElseThrow()
                            .metadata().get("omissionReason"),
                    "Omitted because SHAFT could not read the native Playwright trace.");
            Assert.assertTrue(new String(manifest.nativeEntry().open().readAllBytes(), StandardCharsets.UTF_8)
                    .contains("could not read"));
        }
    }

    private static long stagedNativeFiles() throws IOException {
        try (var paths = Files.list(Path.of(System.getProperty("java.io.tmpdir")))) {
            return paths.filter(path -> path.getFileName().toString().startsWith("shaft-native-trace-")).count();
        }
    }
}
