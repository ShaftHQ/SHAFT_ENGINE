package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

public class TraceArtifactManifestTest {

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
