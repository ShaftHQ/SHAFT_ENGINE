package com.shaft.intellij.settings;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftCustomPropertiesFileTest {
    private Path tempDir;

    @AfterEach
    void cleanUp() throws IOException {
        if (tempDir != null && Files.exists(tempDir)) {
            try (var walk = Files.walk(tempDir)) {
                walk.sorted((a, b) -> b.compareTo(a)).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (IOException ignored) {
                        // best-effort cleanup
                    }
                });
            }
        }
    }

    private Path newTempDir() throws IOException {
        tempDir = Files.createTempDirectory("shaft-custom-properties-test");
        return tempDir;
    }

    @Test
    void readReturnsPresentKeysFromAnExistingFile() throws IOException {
        Path dir = newTempDir();
        Path file = dir.resolve("custom.properties");
        Files.writeString(file, """
                # SHAFT custom overrides
                executionAddress=local

                targetBrowserName=chrome
                headlessExecution=false
                """);

        Map<String, String> result = ShaftCustomPropertiesFile.read(file);

        assertEquals("chrome", result.get("targetBrowserName"));
        assertEquals("false", result.get("headlessExecution"));
    }

    @Test
    void readReturnsNoEntryForAbsentKeys() throws IOException {
        Path dir = newTempDir();
        Path file = dir.resolve("custom.properties");
        Files.writeString(file, """
                # SHAFT custom overrides
                executionAddress=local
                """);

        Map<String, String> result = ShaftCustomPropertiesFile.read(file);

        assertFalse(result.containsKey("targetBrowserName"));
        assertFalse(result.containsKey("headlessExecution"));
    }

    @Test
    void readOfNonexistentFileReturnsEmptyMap() throws IOException {
        Path dir = newTempDir();
        Path file = dir.resolve("does-not-exist.properties");

        Map<String, String> result = ShaftCustomPropertiesFile.read(file);

        assertTrue(result.isEmpty());
    }

    @Test
    void writeIntoExistingFileReplacesOnlyTheTargetedValueLeavingOtherLinesByteIdentical() throws IOException {
        Path dir = newTempDir();
        Path file = dir.resolve("custom.properties");
        List<String> original = List.of(
                "# SHAFT custom overrides",
                "# Keep only the properties you want to customize.",
                "",
                "executionAddress=local",
                "targetBrowserName=chrome",
                "retryMaximumNumberOfAttempts=0");
        Files.write(file, original);

        ShaftCustomPropertiesFile.write(file, Map.of("targetBrowserName", "firefox"), Set.of());

        List<String> result = Files.readAllLines(file);
        assertEquals(List.of(
                "# SHAFT custom overrides",
                "# Keep only the properties you want to customize.",
                "",
                "executionAddress=local",
                "targetBrowserName=firefox",
                "retryMaximumNumberOfAttempts=0"), result);
    }

    @Test
    void writeAppendsANewKeyWhenNotAlreadyPresent() throws IOException {
        Path dir = newTempDir();
        Path file = dir.resolve("custom.properties");
        Files.write(file, List.of("executionAddress=local"));

        ShaftCustomPropertiesFile.write(file, Map.of("headlessExecution", "true"), Set.of());

        List<String> result = Files.readAllLines(file);
        assertEquals(List.of("executionAddress=local", "headlessExecution=true"), result);
    }

    @Test
    void writeIntoNonexistentFileCreatesItWithMinimalContent() throws IOException {
        Path dir = newTempDir();
        Path file = dir.resolve("properties").resolve("custom.properties");
        assertFalse(Files.exists(file));

        ShaftCustomPropertiesFile.write(file, Map.of("targetBrowserName", "edge"), Set.of());

        assertTrue(Files.exists(file));
        List<String> result = Files.readAllLines(file);
        assertTrue(result.get(0).startsWith("#"), "first line should be a header comment");
        assertTrue(result.stream().anyMatch("targetBrowserName=edge"::equals));
    }

    @Test
    void writeRemovesAnExistingKeyLineLeavingEverythingElseIntact() throws IOException {
        Path dir = newTempDir();
        Path file = dir.resolve("custom.properties");
        List<String> original = List.of(
                "# SHAFT custom overrides",
                "executionAddress=local",
                "targetBrowserName=chrome",
                "headlessExecution=false",
                "retryMaximumNumberOfAttempts=0");
        Files.write(file, original);

        ShaftCustomPropertiesFile.write(file, Map.of(), Set.of("targetBrowserName", "headlessExecution"));

        List<String> result = Files.readAllLines(file);
        assertEquals(List.of(
                "# SHAFT custom overrides",
                "executionAddress=local",
                "retryMaximumNumberOfAttempts=0"), result);
    }

    @Test
    void writeDoesNotCreateAFileWhenSetKeysIsEmptyAndFileDoesNotExist() throws IOException {
        Path dir = newTempDir();
        Path file = dir.resolve("custom.properties");

        ShaftCustomPropertiesFile.write(file, Map.of(), Set.of("targetBrowserName"));

        assertFalse(Files.exists(file), "no file should be created for a no-op write");
    }

    @Test
    void lastDuplicateKeyWinsOnRead() throws IOException {
        Path dir = newTempDir();
        Path file = dir.resolve("custom.properties");
        Files.write(file, List.of("targetBrowserName=chrome", "targetBrowserName=firefox"));

        Map<String, String> result = ShaftCustomPropertiesFile.read(file);

        assertEquals("firefox", result.get("targetBrowserName"));
    }
}
