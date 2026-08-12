package com.shaft.tools.io.internal;

import org.apache.commons.compress.archivers.zip.ZipArchiveEntry;
import org.apache.commons.compress.archivers.zip.ZipArchiveOutputStream;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

class PlaywrightTraceArchiveLoaderTest {
    @Test
    void loadsTraceStreamsAndSnapshotResourcesEntirelyOffline() throws Exception {
        Path archive = Files.createTempFile("playwright-trace", ".zip");
        try {
            writeArchive(archive, Map.of(
                    "test.trace", "{\"version\":8,\"type\":\"context-options\",\"origin\":\"testRunner\"}\n",
                    "0-trace.trace", "{\"version\":8,\"type\":\"context-options\",\"origin\":\"library\"}\n"
                            + "{\"type\":\"before\",\"callId\":\"call@1\",\"beforeSnapshot\":\"before@call@1\"}\n",
                    "0-trace.network", "{\"type\":\"resource-snapshot\",\"snapshot\":{\"request\":{\"url\":\"https://example.test\"}}}\n",
                    "0-trace.stacks", "{\"files\":[\"ExampleTest.java\"],\"stacks\":[]}",
                    "resources/page@1.jpeg", "offline screenshot",
                    "resources/abc.html", "<main>offline snapshot</main>"));

            Object loaded = load(archive);
            Assert.assertEquals(invoke(loaded, "traceEntryNames"),
                    java.util.List.of("0-trace.network", "0-trace.stacks", "0-trace.trace", "test.trace"));
            Assert.assertEquals(invoke(loaded, "resourceEntryNames"),
                    java.util.List.of("resources/abc.html", "resources/page@1.jpeg"));
            Assert.assertEquals(new String((byte[]) invoke(loaded, "entry", "resources/abc.html"),
                    StandardCharsets.UTF_8), "<main>offline snapshot</main>");
            byte[] mutableCopy = (byte[]) invoke(loaded, "entry", "resources/abc.html");
            mutableCopy[0] = 'X';
            Assert.assertEquals(new String((byte[]) invoke(loaded, "entry", "resources/abc.html"),
                    StandardCharsets.UTF_8), "<main>offline snapshot</main>");
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void rejectsAggregateExpansionEvenWhenEveryEntryIsIndividuallySmall() throws Exception {
        Path archive = Files.createTempFile("playwright-trace-budget", ".zip");
        try {
            writeArchive(archive, Map.of(
                    "test.trace", "12345678",
                    "resources/one", "12345678"));

            IOException failure = Assert.expectThrows(IOException.class,
                    () -> load(archive, 10, 12, 10));
            Assert.assertTrue(failure.getMessage().contains("decompressed byte limit"), failure.getMessage());
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void rejectsHostileAndNonCanonicalEntryNames() throws Exception {
        for (String name : java.util.List.of("../escape.trace", "C:/escape.trace", "resources/./x", "a//b.trace")) {
            Path archive = Files.createTempFile("playwright-trace-path", ".zip");
            try {
                writeArchive(archive, Map.of(name, "{}"));
                IOException failure = Assert.expectThrows(IOException.class, () -> load(archive));
                Assert.assertTrue(failure.getMessage().contains("Unsafe"), name + ": " + failure.getMessage());
            } finally {
                Files.deleteIfExists(archive);
            }
        }
    }

    @Test
    void enforcesEntryAndPerEntryLimits() throws Exception {
        Path archive = Files.createTempFile("playwright-trace-limits", ".zip");
        try {
            writeArchive(archive, Map.of("test.trace", "12345678", "resources/one", "1"));
            Assert.assertTrue(Assert.expectThrows(IOException.class,
                    () -> load(archive, 7, 20, 10)).getMessage().contains("entry exceeds"));
            Assert.assertTrue(Assert.expectThrows(IOException.class,
                    () -> load(archive, 10, 20, 1)).getMessage().contains("entry limit"));
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void countsDirectoryRecordsAndBoundsTheCompressedArchive() throws Exception {
        Path archive = Files.createTempFile("playwright-trace-structure", ".zip");
        try {
            try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(archive))) {
                output.putNextEntry(new ZipEntry("resources/"));
                output.closeEntry();
                output.putNextEntry(new ZipEntry("test.trace"));
                output.write("{\"version\":8,\"type\":\"context-options\"}\n".getBytes(StandardCharsets.UTF_8));
                output.closeEntry();
            }
            Assert.assertTrue(Assert.expectThrows(IOException.class,
                    () -> load(archive, 1_024, 2_048, 1)).getMessage().contains("entry limit"));
            Assert.assertTrue(Assert.expectThrows(IOException.class,
                    () -> load(archive, 1_024, 2_048, 10, 1)).getMessage().contains("compressed byte limit"));
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void rejectsMalformedJsonlAndMissingReferencedResources() throws Exception {
        Path malformed = Files.createTempFile("playwright-trace-malformed", ".zip");
        Path missingResource = Files.createTempFile("playwright-trace-resource", ".zip");
        try {
            writeArchive(malformed, Map.of("test.trace", "not-json\n"));
            Assert.assertTrue(Assert.expectThrows(IOException.class, () -> load(malformed))
                    .getMessage().contains("Malformed Playwright trace JSON"));

            writeArchive(missingResource, Map.of("test.trace",
                    "{\"version\":8,\"type\":\"context-options\"}\n"
                            + "{\"type\":\"resource-snapshot\",\"snapshot\":{\"response\":{\"content\":{\"_sha1\":\"absent.html\"}}}}\n"));
            Assert.assertTrue(Assert.expectThrows(IOException.class, () -> load(missingResource))
                    .getMessage().contains("missing referenced resource"));
        } finally {
            Files.deleteIfExists(malformed);
            Files.deleteIfExists(missingResource);
        }
    }

    @Test
    void rejectsMissingScreencastResourceAndResourceOnlyPseudoTrace() throws Exception {
        Path missingScreenshot = Files.createTempFile("playwright-trace-screencast", ".zip");
        Path pseudoTrace = Files.createTempFile("playwright-trace-pseudo", ".zip");
        try {
            writeArchive(missingScreenshot, Map.of("test.trace",
                    "{\"version\":8,\"type\":\"context-options\"}\n"
                            + "{\"type\":\"screencast-frame\",\"sha1\":\"missing.jpeg\"}\n"));
            Assert.assertTrue(Assert.expectThrows(IOException.class, () -> load(missingScreenshot))
                    .getMessage().contains("missing referenced resource"));

            writeArchive(pseudoTrace, Map.of("resources/fake.trace",
                    "{\"version\":8,\"type\":\"context-options\"}\n"));
            Assert.assertTrue(Assert.expectThrows(IOException.class, () -> load(pseudoTrace))
                    .getMessage().contains("no trace data"));
        } finally {
            Files.deleteIfExists(missingScreenshot);
            Files.deleteIfExists(pseudoTrace);
        }
    }

    @Test
    void rejectsMissingCamelCaseSha1Resources() throws Exception {
        Path archive = Files.createTempFile("playwright-trace-camel-sha1", ".zip");
        try {
            writeArchive(archive, Map.of("test.trace",
                    "{\"version\":8,\"type\":\"context-options\"}\n"
                            + "{\"type\":\"resource-snapshot\",\"requestSha1\":\"missing-request.txt\"}\n"));
            Assert.assertTrue(Assert.expectThrows(IOException.class, () -> load(archive))
                    .getMessage().contains("missing referenced resource"));
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void rejectsDuplicateArchiveEntries() throws Exception {
        Path archive = Files.createTempFile("playwright-trace-duplicate", ".zip");
        try {
            try (ZipArchiveOutputStream output = new ZipArchiveOutputStream(archive)) {
                for (String value : java.util.List.of(
                        "{\"version\":8,\"type\":\"context-options\"}\n", "{}\n")) {
                    output.putArchiveEntry(new ZipArchiveEntry("test.trace"));
                    output.write(value.getBytes(StandardCharsets.UTF_8));
                    output.closeArchiveEntry();
                }
            }
            Assert.assertTrue(Assert.expectThrows(IOException.class, () -> load(archive))
                    .getMessage().contains("duplicate entry"));
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void usesStrictUtf8AndOnlyLfForJsonlFraming() throws Exception {
        Path invalidUtf8 = Files.createTempFile("playwright-trace-utf8", ".zip");
        Path unicodeSeparator = Files.createTempFile("playwright-trace-jsonl", ".zip");
        try {
            writeBinaryArchive(invalidUtf8, "test.trace", new byte[]{'{', '}', (byte) 0xC3, (byte) 0x28, '\n'});
            Assert.assertTrue(Assert.expectThrows(IOException.class, () -> load(invalidUtf8))
                    .getMessage().contains("UTF-8"));

            writeArchive(unicodeSeparator, Map.of("test.trace",
                    "{\"version\":8,\"type\":\"context-options\",\"title\":\"left right\"}\n"));
            Object loaded = load(unicodeSeparator);
            Assert.assertEquals(invoke(loaded, "traceEntryNames"), java.util.List.of("test.trace"));
        } finally {
            Files.deleteIfExists(invalidUtf8);
            Files.deleteIfExists(unicodeSeparator);
        }
    }

    @Test
    void appliesSemanticRecordLimitToStacks() throws Exception {
        Path archive = Files.createTempFile("playwright-trace-stacks-limit", ".zip");
        try {
            byte[] oversizedStacks = new byte[4 * 1024 * 1024 + 1];
            java.util.Arrays.fill(oversizedStacks, (byte) ' ');
            oversizedStacks[0] = '{';
            oversizedStacks[oversizedStacks.length - 1] = '}';
            try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(archive))) {
                output.putNextEntry(new ZipEntry("test.trace"));
                output.write("{\"version\":8,\"type\":\"context-options\"}\n".getBytes(StandardCharsets.UTF_8));
                output.closeEntry();
                output.putNextEntry(new ZipEntry("0-trace.stacks"));
                output.write(oversizedStacks);
                output.closeEntry();
            }
            Assert.assertTrue(Assert.expectThrows(IOException.class, () -> load(archive))
                    .getMessage().contains("JSON record exceeds"));
        } finally {
            Files.deleteIfExists(archive);
        }
    }

    @Test
    void loadsDownloadedOfficialSampleWhenAcceptancePathIsSupplied() throws Exception {
        String sample = System.getProperty("shaft.playwrightTraceSample", "");
        if (sample.isBlank()) {
            throw new SkipException("Set shaft.playwrightTraceSample to run the pinned official-sample acceptance.");
        }

        Path samplePath = Path.of(sample);
        Assert.assertEquals(Files.size(samplePath), 167_630L);
        Assert.assertEquals(sha256(Files.readAllBytes(samplePath)),
                "76a2cbb0451bda1b799c3cc3a8270874d33e246976a5dbd4ec7be8fae8234f24");
        Object loaded = load(samplePath);
        @SuppressWarnings("unchecked")
        java.util.List<String> traceEntries = (java.util.List<String>) invoke(loaded, "traceEntryNames");
        @SuppressWarnings("unchecked")
        java.util.List<String> resources = (java.util.List<String>) invoke(loaded, "resourceEntryNames");
        Assert.assertTrue(traceEntries.contains("test.trace"));
        Assert.assertTrue(traceEntries.contains("0-trace.trace"));
        Assert.assertTrue(traceEntries.contains("0-trace.network"));
        Assert.assertTrue(resources.stream().anyMatch(name -> name.endsWith(".html")));
        Assert.assertTrue(resources.stream().anyMatch(name -> name.endsWith(".jpeg")));
    }

    private static Object load(Path archive) throws Exception {
        try {
            Class<?> loader = Class.forName("com.shaft.tools.io.internal.PlaywrightTraceArchiveLoader");
            Method load = loader.getDeclaredMethod("load", Path.class);
            load.setAccessible(true);
            return load.invoke(null, archive);
        } catch (ClassNotFoundException exception) {
            throw new AssertionError("The bounded offline Playwright trace archive loader is missing.", exception);
        } catch (InvocationTargetException exception) {
            if (exception.getCause() instanceof Exception cause) {
                throw cause;
            }
            throw exception;
        }
    }

    private static Object load(Path archive, int maximumEntryBytes, int maximumArchiveBytes,
                               int maximumEntries) throws Exception {
        Class<?> loader = Class.forName("com.shaft.tools.io.internal.PlaywrightTraceArchiveLoader");
        try {
            Method load = loader.getDeclaredMethod("load", Path.class, int.class, int.class, int.class);
            load.setAccessible(true);
            return load.invoke(null, archive, maximumEntryBytes, maximumArchiveBytes, maximumEntries);
        } catch (NoSuchMethodException exception) {
            throw new AssertionError("The loader has no reproducible aggregate-budget seam.", exception);
        } catch (InvocationTargetException exception) {
            if (exception.getCause() instanceof Exception cause) {
                throw cause;
            }
            throw exception;
        }
    }

    private static Object load(Path archive, int maximumEntryBytes, int maximumArchiveBytes,
                               int maximumEntries, int maximumArchiveFileBytes) throws Exception {
        Class<?> loader = Class.forName("com.shaft.tools.io.internal.PlaywrightTraceArchiveLoader");
        try {
            Method load = loader.getDeclaredMethod("load", Path.class, int.class, int.class, int.class, int.class);
            load.setAccessible(true);
            return load.invoke(null, archive, maximumEntryBytes, maximumArchiveBytes, maximumEntries,
                    maximumArchiveFileBytes);
        } catch (InvocationTargetException exception) {
            if (exception.getCause() instanceof Exception cause) {
                throw cause;
            }
            throw exception;
        }
    }

    private static Object invoke(Object target, String methodName, Object... arguments) throws Exception {
        Class<?>[] parameterTypes = java.util.Arrays.stream(arguments).map(Object::getClass).toArray(Class<?>[]::new);
        Method method = target.getClass().getDeclaredMethod(methodName, parameterTypes);
        method.setAccessible(true);
        return method.invoke(target, arguments);
    }

    private static void writeArchive(Path target, Map<String, String> entries) throws IOException {
        try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(target))) {
            for (Map.Entry<String, String> entry : entries.entrySet()) {
                output.putNextEntry(new ZipEntry(entry.getKey()));
                output.write(entry.getValue().getBytes(StandardCharsets.UTF_8));
                output.closeEntry();
            }
        }
    }

    private static void writeBinaryArchive(Path target, String name, byte[] bytes) throws IOException {
        try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(target))) {
            output.putNextEntry(new ZipEntry(name));
            output.write(bytes);
            output.closeEntry();
        }
    }

    private static String sha256(byte[] bytes) throws Exception {
        return java.util.HexFormat.of().formatHex(java.security.MessageDigest.getInstance("SHA-256").digest(bytes));
    }
}
