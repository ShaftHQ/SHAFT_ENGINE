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
import java.util.List;
import java.util.Set;

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
}
