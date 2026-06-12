package com.shaft.heal.internal;

import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.model.HealingScore;
import com.shaft.heal.model.LocatorFingerprint;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

final class DeterministicScorer {
    private static final Map<String, Double> WEIGHTS = Map.of(
            "accessibility", 0.25,
            "label", 0.20,
            "test-id", 0.20,
            "stable-id-name", 0.15,
            "semantic", 0.15,
            "dom-fingerprint", 0.05);
    private final HealingConfiguration configuration;

    DeterministicScorer(HealingConfiguration configuration) {
        this.configuration = Objects.requireNonNull(configuration, "configuration");
    }

    HealingScore score(LocatorFingerprint original, LocatorFingerprint candidate) {
        Map<String, Double> evidence = new LinkedHashMap<>();
        add(evidence, "accessibility",
                max(similarity(original.accessibleName(), candidate.accessibleName()),
                        similarity(original.accessibleName(), candidate.visibleText())),
                present(original.accessibleName()));
        add(evidence, "label",
                max(similarity(original.associatedLabel(), candidate.associatedLabel()),
                        similarity(original.associatedLabel(), candidate.accessibleName())),
                present(original.associatedLabel()));
        add(evidence, "test-id", mapSimilarity(original.testIds(), candidate.testIds()),
                !original.testIds().isEmpty());
        add(evidence, "stable-id-name",
                max(similarity(original.id(), candidate.id()), similarity(original.name(), candidate.name())),
                present(original.id()) || present(original.name()));
        add(evidence, "semantic", semanticSimilarity(original, candidate), hasSemanticEvidence(original));
        add(evidence, "dom-fingerprint",
                exact(original.domPathChecksum(), candidate.domPathChecksum()),
                present(original.domPathChecksum()));

        double weightedScore = 0;
        double availableWeight = 0;
        for (Map.Entry<String, Double> item : evidence.entrySet()) {
            double weight = WEIGHTS.getOrDefault(item.getKey(), 0.0);
            weightedScore += weight * item.getValue();
            availableWeight += weight;
        }
        double deterministic = availableWeight == 0 ? 0 : weightedScore / availableWeight;
        return new HealingScore(deterministic, null, null, deterministic, evidence);
    }

    private void add(Map<String, Double> evidence, String category, double score, boolean available) {
        if (available && configuration.evidenceCategories().contains(category)) {
            evidence.put(category, bounded(score));
        }
    }

    private static boolean hasSemanticEvidence(LocatorFingerprint fingerprint) {
        return present(fingerprint.tagName())
                || present(fingerprint.role())
                || present(fingerprint.type())
                || present(fingerprint.placeholder())
                || present(fingerprint.title())
                || present(fingerprint.visibleText())
                || !fingerprint.semanticAttributes().isEmpty();
    }

    private static double semanticSimilarity(LocatorFingerprint original, LocatorFingerprint candidate) {
        double total = 0;
        int count = 0;
        for (double value : new double[]{
                exact(original.tagName(), candidate.tagName()),
                exact(original.role(), candidate.role()),
                exact(original.type(), candidate.type()),
                similarity(original.placeholder(), candidate.placeholder()),
                similarity(original.title(), candidate.title()),
                similarity(original.visibleText(), candidate.visibleText()),
                mapSimilarity(original.semanticAttributes(), candidate.semanticAttributes())}) {
            if (value >= 0) {
                total += value;
                count++;
            }
        }
        return count == 0 ? 0 : total / count;
    }

    private static double mapSimilarity(Map<String, String> original, Map<String, String> candidate) {
        if (original.isEmpty()) {
            return -1;
        }
        double best = 0;
        for (Map.Entry<String, String> item : original.entrySet()) {
            String candidateValue = candidate.get(item.getKey());
            if (candidateValue != null) {
                best = Math.max(best, similarity(item.getValue(), candidateValue));
            }
        }
        return best;
    }

    private static double exact(String original, String candidate) {
        if (!present(original)) {
            return -1;
        }
        return HealingSupport.normalize(original).equals(HealingSupport.normalize(candidate)) ? 1 : 0;
    }

    static double similarity(String original, String candidate) {
        if (!present(original)) {
            return -1;
        }
        String left = HealingSupport.normalize(original);
        String right = HealingSupport.normalize(candidate);
        if (left.equals(right)) {
            return 1;
        }
        if (right.isBlank()) {
            return 0;
        }
        int distance = levenshtein(left, right);
        return bounded(1 - ((double) distance / Math.max(left.length(), right.length())));
    }

    private static int levenshtein(String left, String right) {
        int[] previous = new int[right.length() + 1];
        int[] current = new int[right.length() + 1];
        for (int column = 0; column <= right.length(); column++) {
            previous[column] = column;
        }
        for (int row = 1; row <= left.length(); row++) {
            current[0] = row;
            for (int column = 1; column <= right.length(); column++) {
                int substitution = previous[column - 1]
                        + (left.charAt(row - 1) == right.charAt(column - 1) ? 0 : 1);
                current[column] = Math.min(
                        Math.min(current[column - 1] + 1, previous[column] + 1),
                        substitution);
            }
            int[] swap = previous;
            previous = current;
            current = swap;
        }
        return previous[right.length()];
    }

    private static double max(double left, double right) {
        return Math.max(Math.max(0, left), Math.max(0, right));
    }

    private static boolean present(String value) {
        return value != null && !value.isBlank();
    }

    private static double bounded(double value) {
        return Math.max(0, Math.min(1, value));
    }
}
