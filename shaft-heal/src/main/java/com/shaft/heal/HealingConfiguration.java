package com.shaft.heal;

import com.shaft.driver.SHAFT;

import java.nio.file.Path;
import java.time.Duration;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Immutable current-thread SHAFT Heal configuration snapshot.
 *
 * @param minimumConfidence minimum accepted score
 * @param ambiguityMargin required top-candidate separation
 * @param evidenceCategories enabled deterministic evidence categories
 * @param testIdAttributes configured test ID attributes
 * @param historyEnabled whether local history is enabled
 * @param historyPath local history path
 * @param historyMaxEntries maximum history records
 * @param historyRetention history retention
 * @param visualEnabled whether optional visual evidence is enabled
 * @param aiEnabled whether optional provider reranking is enabled
 * @param sourcePatchEnabled whether reviewed source-patch proposals are permitted
 */
public record HealingConfiguration(
        double minimumConfidence,
        double ambiguityMargin,
        Set<String> evidenceCategories,
        List<String> testIdAttributes,
        boolean historyEnabled,
        Path historyPath,
        int historyMaxEntries,
        Duration historyRetention,
        boolean visualEnabled,
        boolean aiEnabled,
        boolean sourcePatchEnabled) {
    /**
     * Reads the effective current-thread configuration.
     *
     * @return configuration snapshot
     */
    public static HealingConfiguration current() {
        var properties = SHAFT.Properties.healing;
        return new HealingConfiguration(
                bounded(properties.minimumConfidence(), 0.75),
                bounded(properties.ambiguityMargin(), 0.10),
                split(properties.evidenceCategories()).stream()
                        .map(value -> value.toLowerCase(Locale.ROOT))
                        .collect(Collectors.toUnmodifiableSet()),
                split(properties.testIdAttributes()),
                properties.historyEnabled(),
                Path.of(properties.historyPath()).toAbsolutePath().normalize(),
                Math.max(1, properties.historyMaxEntries()),
                Duration.ofDays(Math.max(1, properties.historyRetentionDays())),
                properties.visualEnabled(),
                properties.aiEnabled(),
                properties.sourcePatchEnabled());
    }

    private static List<String> split(String value) {
        if (value == null || value.isBlank()) {
            return List.of();
        }
        return Arrays.stream(value.split(","))
                .map(String::trim)
                .filter(item -> !item.isEmpty())
                .toList();
    }

    private static double bounded(double value, double fallback) {
        return value >= 0 && value <= 1 ? value : fallback;
    }
}
