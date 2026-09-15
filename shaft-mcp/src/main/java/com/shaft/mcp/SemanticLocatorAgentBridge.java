package com.shaft.mcp;

import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.gui.internal.aria.AriaNode;
import com.shaft.gui.internal.locator.semantic.AmbiguousSemanticLocatorException;
import com.shaft.gui.internal.locator.semantic.SemanticElementEvidence;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorResolution;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorResolver;
import com.shaft.gui.internal.locator.semantic.SemanticLocatorStrategy;
import com.shaft.gui.internal.locator.semantic.SemanticMatchInspector;

import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;

/**
 * Wires engine {@link SemanticLocatorResolver} into shaft-mcp agent locator context
 * (issue #5818 / parent #5457). Builds evidence from capture snapshots (and optional
 * aria forests), resolves with FR-1 precedence, and never silently picks an ambiguous
 * target — payloads retain evidence + confidence (FR-3).
 */
public final class SemanticLocatorAgentBridge {
    private SemanticLocatorAgentBridge() {
    }

    /**
     * Builds resolver evidence from a capture {@link ElementSnapshot} and its ranked
     * {@link LocatorCandidate} uniqueness counts.
     *
     * @param snapshot inspected element
     * @return evidence ready for {@link SemanticLocatorResolver#resolve}
     */
    public static SemanticElementEvidence evidenceFrom(ElementSnapshot snapshot) {
        return evidenceFrom(snapshot, "mcp-dom inspection");
    }

    /**
     * Builds resolver evidence with an inspection provenance note (e.g. aria snapshot seed).
     *
     * @param snapshot inspected element
     * @param notes inspection notes retained on the resolution (FR-3)
     * @return evidence ready for resolve
     */
    public static SemanticElementEvidence evidenceFrom(ElementSnapshot snapshot, String notes) {
        EnumMap<SemanticLocatorStrategy, LocatorCandidate> byStrategy = new EnumMap<>(SemanticLocatorStrategy.class);
        for (LocatorCandidate candidate : snapshot.locatorCandidates()) {
            toEngine(candidate.strategy()).ifPresent(strategy ->
                    byStrategy.putIfAbsent(strategy, candidate));
        }
        SemanticElementEvidence.Builder builder = SemanticElementEvidence.builder()
                .inspectionNotes(notes == null ? "" : notes);

        LocatorCandidate role = byStrategy.get(SemanticLocatorStrategy.ROLE);
        if (role != null && role.expression().contains(":")) {
            int colon = role.expression().indexOf(':');
            String roleName = role.expression().substring(0, colon);
            String accessible = role.expression().substring(colon + 1);
            builder.role(roleName, accessible, role.uniquenessCount());
        } else if (!snapshot.role().isBlank() && !snapshot.accessibleName().isBlank()) {
            builder.role(snapshot.role(), snapshot.accessibleName(),
                    countOrDefault(byStrategy, SemanticLocatorStrategy.ROLE, 1));
        }

        putAccessible(builder, snapshot, byStrategy);
        putLabel(builder, snapshot, byStrategy);
        putTestId(builder, snapshot, byStrategy);
        putId(builder, snapshot, byStrategy);
        putName(builder, snapshot, byStrategy);
        putStructural(builder, byStrategy);
        return builder.build();
    }

    /**
     * Seeds evidence from an aria-accessible-name forest when role+name are known
     * ({@link SemanticMatchInspector#fromAriaForest}).
     *
     * @param forest aria forest nodes
     * @param role target role
     * @param accessibleName target accessible name
     * @return evidence from the forest
     */
    public static SemanticElementEvidence evidenceFromAriaForest(
            List<AriaNode> forest, String role, String accessibleName) {
        return SemanticMatchInspector.fromAriaForest(forest, role, accessibleName);
    }

    /**
     * Resolves FR-1 locator context for an element. Ambiguity is returned as a failed
     * outcome (never a silent arbitrary pick — SC-2).
     *
     * @param snapshot inspected element
     * @return resolution outcome
     */
    public static ResolveOutcome resolve(ElementSnapshot snapshot) {
        return resolve(evidenceFrom(snapshot));
    }

    /**
     * Resolves pre-built evidence (e.g. aria-seeded).
     *
     * @param evidence inspected evidence
     * @return resolution outcome
     */
    public static ResolveOutcome resolve(SemanticElementEvidence evidence) {
        try {
            SemanticLocatorResolution resolution = SemanticLocatorResolver.resolve(evidence);
            return ResolveOutcome.resolved(resolution);
        } catch (AmbiguousSemanticLocatorException ambiguous) {
            return ResolveOutcome.ambiguous(ambiguous, evidence);
        } catch (IllegalArgumentException empty) {
            return ResolveOutcome.empty(empty.getMessage(), evidence);
        }
    }

    /**
     * MCP payload fields for agent context (strategy, confidence, evidence, ambiguity).
     *
     * @param outcome resolve outcome
     * @return JSON-friendly map
     */
    public static Map<String, Object> toPayload(ResolveOutcome outcome) {
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("ambiguous", outcome.ambiguous());
        payload.put("resolved", outcome.resolution().isPresent());
        outcome.resolution().ifPresent(resolution -> {
            payload.put("strategy", resolution.strategy().name());
            payload.put("expression", resolution.expression());
            payload.put("confidence", resolution.confidence());
            payload.put("matchCount", resolution.matchCount());
            payload.put("usedExplicitScope", resolution.usedExplicitScope());
            payload.put("usedSemanticFallback", resolution.usedSemanticFallback());
            payload.put("locator", resolution.locator().toString());
        });
        outcome.ambiguity().ifPresent(ex -> {
            payload.put("strategy", ex.strategy().name());
            payload.put("expression", ex.expression());
            payload.put("matchCount", ex.matchCount());
            payload.put("confidence", 0.0);
            payload.put("reason", ex.getMessage());
        });
        if (outcome.resolution().isEmpty() && outcome.ambiguity().isEmpty()) {
            payload.put("reason", outcome.emptyReason());
            payload.put("confidence", 0.0);
        }
        SemanticElementEvidence evidence = outcome.evidence();
        Map<String, Object> evidenceMap = new LinkedHashMap<>();
        evidenceMap.put("role", evidence.role());
        evidenceMap.put("accessibleName", evidence.accessibleName());
        evidenceMap.put("label", evidence.label());
        evidenceMap.put("visibleText", evidence.visibleText());
        evidenceMap.put("testId", evidence.testId());
        evidenceMap.put("id", evidence.id());
        evidenceMap.put("name", evidence.name());
        evidenceMap.put("inspectionNotes", evidence.inspectionNotes());
        evidenceMap.put("hasSemanticSignal", evidence.hasSemanticSignal());
        payload.put("evidence", evidenceMap);
        return Map.copyOf(payload);
    }

    private static void putAccessible(
            SemanticElementEvidence.Builder builder,
            ElementSnapshot snapshot,
            EnumMap<SemanticLocatorStrategy, LocatorCandidate> byStrategy) {
        LocatorCandidate candidate = byStrategy.get(SemanticLocatorStrategy.ACCESSIBLE_NAME);
        String name = candidate != null ? candidate.expression() : snapshot.accessibleName();
        if (!name.isBlank()) {
            builder.accessibleName(name, countOrDefault(byStrategy, SemanticLocatorStrategy.ACCESSIBLE_NAME,
                    candidate != null ? candidate.uniquenessCount() : 1));
        }
    }

    private static void putLabel(
            SemanticElementEvidence.Builder builder,
            ElementSnapshot snapshot,
            EnumMap<SemanticLocatorStrategy, LocatorCandidate> byStrategy) {
        LocatorCandidate candidate = byStrategy.get(SemanticLocatorStrategy.LABEL);
        String label = candidate != null ? candidate.expression() : snapshot.label();
        if (!label.isBlank()) {
            builder.label(label, countOrDefault(byStrategy, SemanticLocatorStrategy.LABEL,
                    candidate != null ? candidate.uniquenessCount() : 1));
        }
    }

    private static void putTestId(
            SemanticElementEvidence.Builder builder,
            ElementSnapshot snapshot,
            EnumMap<SemanticLocatorStrategy, LocatorCandidate> byStrategy) {
        LocatorCandidate candidate = byStrategy.get(SemanticLocatorStrategy.TEST_ID);
        if (candidate != null) {
            builder.testId(extractTestId(candidate.expression()), candidate.uniquenessCount());
            return;
        }
        for (String key : List.of("data-testid", "data-test", "data-qa")) {
            String value = snapshot.normalizedAttributes().getOrDefault(key, "");
            if (!value.isBlank()) {
                builder.testId(value, 1);
                return;
            }
        }
    }

    private static void putId(
            SemanticElementEvidence.Builder builder,
            ElementSnapshot snapshot,
            EnumMap<SemanticLocatorStrategy, LocatorCandidate> byStrategy) {
        LocatorCandidate candidate = byStrategy.get(SemanticLocatorStrategy.ID);
        String id = candidate != null ? candidate.expression()
                : snapshot.normalizedAttributes().getOrDefault("id", "");
        if (!id.isBlank()) {
            builder.id(id, countOrDefault(byStrategy, SemanticLocatorStrategy.ID,
                    candidate != null ? candidate.uniquenessCount() : 1));
        }
    }

    private static void putName(
            SemanticElementEvidence.Builder builder,
            ElementSnapshot snapshot,
            EnumMap<SemanticLocatorStrategy, LocatorCandidate> byStrategy) {
        LocatorCandidate candidate = byStrategy.get(SemanticLocatorStrategy.NAME);
        String name = candidate != null ? candidate.expression()
                : snapshot.normalizedAttributes().getOrDefault("name", "");
        if (!name.isBlank()) {
            builder.name(name, countOrDefault(byStrategy, SemanticLocatorStrategy.NAME,
                    candidate != null ? candidate.uniquenessCount() : 1));
        }
    }

    private static void putStructural(
            SemanticElementEvidence.Builder builder,
            EnumMap<SemanticLocatorStrategy, LocatorCandidate> byStrategy) {
        LocatorCandidate css = byStrategy.get(SemanticLocatorStrategy.CSS);
        if (css != null && !css.expression().isBlank()) {
            builder.css(css.expression(), css.uniquenessCount());
        }
        LocatorCandidate xpath = byStrategy.get(SemanticLocatorStrategy.XPATH);
        if (xpath != null && !xpath.expression().isBlank()) {
            builder.xpath(xpath.expression(), xpath.uniquenessCount());
        }
    }

    private static int countOrDefault(
            EnumMap<SemanticLocatorStrategy, LocatorCandidate> byStrategy,
            SemanticLocatorStrategy strategy,
            int fallback) {
        LocatorCandidate candidate = byStrategy.get(strategy);
        return candidate != null ? candidate.uniquenessCount() : fallback;
    }

    private static String extractTestId(String expression) {
        String value = expression == null ? "" : expression.trim();
        int eq = value.indexOf("=\"");
        if (eq >= 0 && value.endsWith("\"]")) {
            return value.substring(eq + 2, value.length() - 2);
        }
        return value;
    }

    private static Optional<SemanticLocatorStrategy> toEngine(LocatorCandidate.LocatorStrategy strategy) {
        if (strategy == null) {
            return Optional.empty();
        }
        return switch (strategy) {
            case ROLE -> Optional.of(SemanticLocatorStrategy.ROLE);
            case ACCESSIBLE_NAME -> Optional.of(SemanticLocatorStrategy.ACCESSIBLE_NAME);
            case LABEL -> Optional.of(SemanticLocatorStrategy.LABEL);
            case TEST_ID -> Optional.of(SemanticLocatorStrategy.TEST_ID);
            case ID -> Optional.of(SemanticLocatorStrategy.ID);
            case NAME -> Optional.of(SemanticLocatorStrategy.NAME);
            case CSS -> Optional.of(SemanticLocatorStrategy.CSS);
            case XPATH -> Optional.of(SemanticLocatorStrategy.XPATH);
        };
    }

    /**
     * Outcome of an MCP semantic resolve attempt (resolved, ambiguous, or empty).
     *
     * @param resolution successful resolution when present
     * @param ambiguity ambiguity exception when present
     * @param emptyReason reason when neither resolved nor ambiguous
     * @param evidence inspected evidence retained for the agent
     */
    public record ResolveOutcome(
            Optional<SemanticLocatorResolution> resolution,
            Optional<AmbiguousSemanticLocatorException> ambiguity,
            String emptyReason,
            SemanticElementEvidence evidence) {

        public ResolveOutcome {
            resolution = resolution == null ? Optional.empty() : resolution;
            ambiguity = ambiguity == null ? Optional.empty() : ambiguity;
            emptyReason = emptyReason == null ? "" : emptyReason;
            evidence = java.util.Objects.requireNonNull(evidence, "evidence");
        }

        static ResolveOutcome resolved(SemanticLocatorResolution resolution) {
            return new ResolveOutcome(Optional.of(resolution), Optional.empty(), "", resolution.evidence());
        }

        static ResolveOutcome ambiguous(AmbiguousSemanticLocatorException ex, SemanticElementEvidence evidence) {
            return new ResolveOutcome(Optional.empty(), Optional.of(ex), "", evidence);
        }

        static ResolveOutcome empty(String reason, SemanticElementEvidence evidence) {
            return new ResolveOutcome(Optional.empty(), Optional.empty(), reason, evidence);
        }

        public boolean ambiguous() {
            return ambiguity.isPresent();
        }

        /**
         * Preferred capture strategy name for shaftLocatorCode when resolved.
         *
         * @return capture strategy name, or empty
         */
        public Optional<String> preferredCaptureStrategy() {
            return resolution().map(r -> r.strategy().name().toUpperCase(Locale.ROOT));
        }
    }
}
