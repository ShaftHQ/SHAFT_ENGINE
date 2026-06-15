package com.shaft.heal.internal;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.heal.HealingConfiguration;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class HealingHistoryStoreTest {
    private static final ObjectMapper JSON = new ObjectMapper();
    private final Path directory = Path.of("target", "shaft-heal-tests", "history-store").toAbsolutePath();

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws IOException {
        if (Files.exists(directory)) {
            try (var paths = Files.walk(directory)) {
                paths.sorted(Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (IOException ignored) {
                        // Best-effort isolated test cleanup.
                    }
                });
            }
        }
    }

    @Test
    public void tamperedChecksumShouldInvalidateHistory() throws IOException {
        HealingConfiguration configuration = configuration(10);
        HealingHistoryStore store = new HealingHistoryStore(configuration);
        store.save("https://example.test/page", "By.id: old", "frame=", 
                DeterministicScorerTest.fingerprint("old", "Username"), "");
        String content = Files.readString(configuration.historyPath(), StandardCharsets.UTF_8)
                .replace("\"Username\"", "\"Tampered\"");
        Files.writeString(configuration.historyPath(), content, StandardCharsets.UTF_8);

        Assert.assertTrue(store.find("https://example.test/page", "By.id: old", "frame=").isEmpty());
    }

    @Test
    public void historyShouldRemainBounded() throws IOException {
        HealingConfiguration configuration = configuration(2);
        HealingHistoryStore store = new HealingHistoryStore(configuration);
        store.save("page", "one", "context", DeterministicScorerTest.fingerprint("one", "One"), "");
        store.save("page", "two", "context", DeterministicScorerTest.fingerprint("two", "Two"), "");
        store.save("page", "three", "context", DeterministicScorerTest.fingerprint("three", "Three"), "");

        JsonNode document = JSON.readTree(configuration.historyPath().toFile());
        Assert.assertEquals(document.path("records").size(), 2);
    }

    @Test
    public void corruptHistoryShouldBeQuarantinedAndRebuilt() throws IOException {
        HealingConfiguration configuration = configuration(10);
        Files.writeString(configuration.historyPath(), "{not-json", StandardCharsets.UTF_8);
        HealingHistoryStore store = new HealingHistoryStore(configuration);

        Assert.assertTrue(store.find("page", "locator", "context").isEmpty());
        try (var files = Files.list(directory)) {
            Assert.assertEquals(files.filter(path -> path.getFileName().toString()
                    .startsWith("history.json.corrupt-")).count(), 1);
        }

        store.save("page", "locator", "context",
                DeterministicScorerTest.fingerprint("new", "New"), "");

        JsonNode document = JSON.readTree(configuration.historyPath().toFile());
        Assert.assertEquals(document.path("schemaVersion").asText(), "2.0");
        Assert.assertEquals(document.path("records").size(), 1);
    }

    @Test
    public void unsupportedHistoryVersionShouldBeQuarantined() throws IOException {
        HealingConfiguration configuration = configuration(10);
        Files.writeString(
                configuration.historyPath(),
                "{\"schemaVersion\":\"99.0\",\"records\":[]}",
                StandardCharsets.UTF_8);

        Assert.assertTrue(new HealingHistoryStore(configuration)
                .find("page", "locator", "context").isEmpty());
        Assert.assertFalse(Files.exists(configuration.historyPath()));
        try (var files = Files.list(directory)) {
            Assert.assertEquals(files.filter(path -> path.getFileName().toString()
                    .startsWith("history.json.corrupt-")).count(), 1);
        }
    }

    @Test
    public void validVersionOneHistoryShouldMigrateOnRead() throws IOException {
        HealingConfiguration configuration = configuration(10);
        String pageKey = "https://example.test/page";
        String originalLocator = "By.id: old";
        String context = "frame=By.id: frame;shadowHost=;shadowContent=";
        Map<String, Object> fingerprint = legacyFingerprint();
        Map<String, Object> record = new LinkedHashMap<>();
        record.put("schemaVersion", "1.0");
        record.put("key", HealingHistoryStore.key(pageKey, originalLocator, context));
        record.put("pageKey", pageKey);
        record.put("originalLocator", originalLocator);
        record.put("context", context);
        record.put("fingerprint", fingerprint);
        record.put("visualReference", "");
        record.put("updatedAt", "2026-06-15T00:00:00Z");
        record.put("checksum", legacyChecksum(record));
        Files.writeString(
                configuration.historyPath(),
                JSON.writerWithDefaultPrettyPrinter().writeValueAsString(Map.of(
                        "schemaVersion", "1.0",
                        "records", List.of(record))),
                StandardCharsets.UTF_8);

        HistoryRecord migrated = new HealingHistoryStore(configuration)
                .find(pageKey, originalLocator, context)
                .orElseThrow();

        Assert.assertEquals(migrated.schemaVersion(), "2.0");
        Assert.assertEquals(migrated.fingerprint().schemaVersion(), "2.0");
        Assert.assertEquals(migrated.contextMetadata().frameLocator(), "By.id: frame");
        JsonNode document = JSON.readTree(configuration.historyPath().toFile());
        Assert.assertEquals(document.path("schemaVersion").asText(), "2.0");
    }

    @Test
    public void concurrentWritesShouldRemainValidAndBounded() throws Exception {
        HealingConfiguration configuration = configuration(12);
        HealingHistoryStore store = new HealingHistoryStore(configuration);
        try (var executor = Executors.newFixedThreadPool(6)) {
            for (int index = 0; index < 30; index++) {
                int value = index;
                executor.submit(() -> store.save(
                        "page-" + value,
                        "locator-" + value,
                        "context",
                        DeterministicScorerTest.fingerprint("id-" + value, "Name " + value),
                        ""));
            }
            executor.shutdown();
            Assert.assertTrue(executor.awaitTermination(10, TimeUnit.SECONDS));
        }

        JsonNode document = JSON.readTree(configuration.historyPath().toFile());
        Assert.assertEquals(document.path("schemaVersion").asText(), "2.0");
        Assert.assertEquals(document.path("records").size(), 12);
        for (JsonNode record : document.path("records")) {
            Assert.assertFalse(record.path("checksum").asText().isBlank());
        }
    }

    @Test
    public void privacySanitizerShouldRemoveSecretCanaries() {
        String canary = "authorization=Bearer eyJabcdefgh.abcdefgh.abcdefgh token=secret-value-12345678901234567890";

        PrivacySanitizer.SanitizedValue sanitized = PrivacySanitizer.sanitize(canary);

        Assert.assertFalse(sanitized.value().contains("eyJabcdefgh"));
        Assert.assertFalse(sanitized.value().contains("secret-value"));
        Assert.assertTrue(sanitized.redactedCount() > 0);
    }

    private HealingConfiguration configuration(int maxEntries) throws IOException {
        Files.createDirectories(directory);
        return new HealingConfiguration(
                0.75,
                0.10,
                Set.of("accessibility", "label", "test-id", "stable-id-name", "semantic", "dom-fingerprint"),
                List.of("data-testid"),
                true,
                directory.resolve("history.json"),
                maxEntries,
                Duration.ofDays(30),
                false,
                false,
                false);
    }

    private static Map<String, Object> legacyFingerprint() {
        Map<String, Object> fingerprint = new LinkedHashMap<>();
        fingerprint.put("schemaVersion", "1.0");
        fingerprint.put("tagName", "input");
        fingerprint.put("accessibleName", "Username");
        fingerprint.put("associatedLabel", "Username");
        fingerprint.put("visibleText", "");
        fingerprint.put("id", "old");
        fingerprint.put("name", "username");
        fingerprint.put("role", "textbox");
        fingerprint.put("type", "text");
        fingerprint.put("placeholder", "Username");
        fingerprint.put("title", "");
        fingerprint.put("testIds", Map.of("data-testid", "username"));
        fingerprint.put("semanticAttributes", Map.of());
        fingerprint.put("domPathChecksum", "legacy-checksum");
        return fingerprint;
    }

    private static String legacyChecksum(Map<String, Object> record) throws IOException {
        Map<String, Object> content = new LinkedHashMap<>(record);
        content.remove("checksum");
        return HealingSupport.sha256(JSON.writeValueAsString(content));
    }
}
