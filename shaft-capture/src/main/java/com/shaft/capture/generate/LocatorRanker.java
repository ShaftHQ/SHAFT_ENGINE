package com.shaft.capture.generate;

import com.fasterxml.jackson.databind.JsonNode;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.LocatorCandidate;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;

/**
 * Deterministically ranks captured locator evidence with explainable scoring.
 */
public final class LocatorRanker {
    private static final Map<LocatorCandidate.LocatorStrategy, Integer> STRATEGY_PRIORITY = Map.of(
            LocatorCandidate.LocatorStrategy.ROLE, 800,
            LocatorCandidate.LocatorStrategy.ACCESSIBLE_NAME, 750,
            LocatorCandidate.LocatorStrategy.LABEL, 700,
            LocatorCandidate.LocatorStrategy.TEST_ID, 650,
            LocatorCandidate.LocatorStrategy.ID, 600,
            LocatorCandidate.LocatorStrategy.NAME, 550,
            LocatorCandidate.LocatorStrategy.CSS, 400,
            LocatorCandidate.LocatorStrategy.XPATH, 200);

    /**
     * Selects the strongest candidate for one captured target.
     *
     * @param target target evidence
     * @param context event context
     * @param interaction whether the event interacts with the target
     * @return ranked selection
     */
    public LocatorSelection select(ElementSnapshot target, EventContext context, boolean interaction) {
        Objects.requireNonNull(target, "target");
        Objects.requireNonNull(context, "context");
        if (target.locatorCandidates().isEmpty()) {
            throw new IllegalArgumentException("Element " + target.logicalElementId()
                    + " has no locator candidates.");
        }
        List<ScoredLocator> ranked = target.locatorCandidates().stream()
                .map(candidate -> score(candidate, target, context, interaction))
                .sorted(Comparator.comparingInt(ScoredLocator::score).reversed()
                        .thenComparingInt(item -> -STRATEGY_PRIORITY.get(item.candidate().strategy()))
                        .thenComparing(item -> item.candidate().strategy().name())
                        .thenComparing(item -> item.candidate().expression()))
                .toList();
        return new LocatorSelection(ranked.getFirst(), ranked.subList(1, ranked.size()));
    }

    private static ScoredLocator score(
            LocatorCandidate candidate,
            ElementSnapshot target,
            EventContext context,
            boolean interaction) {
        Map<String, Integer> components = new TreeMap<>();
        components.put("strategy", STRATEGY_PRIORITY.get(candidate.strategy()));
        components.put("uniqueness", uniqueness(candidate.uniquenessCount()));
        components.put("visibility", candidate.visible() && target.visible() ? 80 : -120);
        components.put("interactability", interaction ? (target.enabled() ? 70 : -180) : 0);
        components.put("semanticMatch", semantic(candidate, target));
        components.put("volatility", volatility(candidate));
        components.put("context", context(candidate, context));
        components.put("replay", replay(candidate, context));
        return new ScoredLocator(candidate, components.values().stream().mapToInt(Integer::intValue).sum(),
                components.entrySet().stream()
                        .map(entry -> entry.getKey() + "=" + signed(entry.getValue()))
                        .toList());
    }

    private static int uniqueness(int count) {
        if (count == 1) {
            return 240;
        }
        if (count == 0) {
            return -240;
        }
        return -60 - Math.min(240, (count - 1) * 30);
    }

    private static int semantic(LocatorCandidate candidate, ElementSnapshot target) {
        String expression = candidate.expression().toLowerCase(Locale.ROOT);
        String name = target.accessibleName().toLowerCase(Locale.ROOT);
        String label = target.label().toLowerCase(Locale.ROOT);
        return switch (candidate.strategy()) {
            case ROLE -> expression.equals(target.role() + ":" + name) ? 160 : 80;
            case ACCESSIBLE_NAME -> expression.equals(name) ? 150 : 70;
            case LABEL -> expression.equals(label) ? 140 : 60;
            case TEST_ID -> hasAttributeValue(target, expression, "data-testid", "data-test", "data-qa") ? 120 : 70;
            case ID -> expression.equals(value(target, "id")) ? 100 : 50;
            case NAME -> expression.equals(value(target, "name")) ? 90 : 40;
            case CSS -> 20;
            case XPATH -> 0;
        };
    }

    private static int volatility(LocatorCandidate candidate) {
        int score = candidate.stable() ? 100 : -80;
        for (LocatorCandidate.LocatorSignal signal : candidate.signals()) {
            score += switch (signal) {
                case USER_PROVIDED -> 260;
                case ACCESSIBLE -> 50;
                case LABEL_ASSOCIATED -> 40;
                case TEST_ATTRIBUTE -> 50;
                case STABLE_ATTRIBUTE -> 60;
                case GENERATED -> -50;
                case POSITIONAL -> -130;
                case DYNAMIC_VALUE -> -240;
            };
        }
        return score;
    }

    private static int context(LocatorCandidate candidate, EventContext context) {
        int score = context.page().framePath().isEmpty() ? 20 : 0;
        JsonNode shadowHosts = context.extensions().get("shadowHosts");
        if (shadowHosts != null && shadowHosts.isArray() && !shadowHosts.isEmpty()) {
            score += candidate.strategy() == LocatorCandidate.LocatorStrategy.CSS ? 50 : -80;
        }
        return score;
    }

    private static int replay(LocatorCandidate candidate, EventContext context) {
        JsonNode locatorReplay = context.extensions().get("locatorReplay");
        String value = locatorReplay != null && locatorReplay.isObject()
                ? locatorReplay.path(candidate.expression()).asText("")
                : context.replayStatus().name();
        return switch (value.toUpperCase(Locale.ROOT)) {
            case "PASSED" -> 180;
            case "FAILED" -> -220;
            case "SKIPPED" -> -40;
            case "UNSUPPORTED" -> -300;
            default -> 0;
        };
    }

    private static boolean hasAttributeValue(ElementSnapshot target, String expression, String... names) {
        for (String name : names) {
            String value = value(target, name);
            if (!value.isBlank() && expression.contains(value)) {
                return true;
            }
        }
        return false;
    }

    private static String value(ElementSnapshot target, String name) {
        return target.normalizedAttributes().getOrDefault(name, "").toLowerCase(Locale.ROOT);
    }

    private static String signed(int value) {
        return value > 0 ? "+" + value : String.valueOf(value);
    }

    /**
     * Ranked selection and fallback candidates.
     *
     * @param selected selected candidate
     * @param alternatives ranked fallback candidates
     */
    public record LocatorSelection(ScoredLocator selected, List<ScoredLocator> alternatives) {
        /**
         * Creates an immutable selection.
         */
        public LocatorSelection {
            alternatives = alternatives == null ? List.of() : List.copyOf(alternatives);
        }
    }

    /**
     * One scored candidate.
     *
     * @param candidate captured candidate
     * @param score deterministic score
     * @param breakdown ordered score explanation
     */
    public record ScoredLocator(LocatorCandidate candidate, int score, List<String> breakdown) {
        /**
         * Creates an immutable score.
         */
        public ScoredLocator {
            breakdown = breakdown == null ? List.of() : List.copyOf(new ArrayList<>(breakdown));
        }
    }
}
