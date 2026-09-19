package com.shaft.mcp;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Objects;

/**
 * Draft / Needs questions / Ready gate for Design packs (issue #5955).
 *
 * <p>Ready requires analysis without open questions, clean (or waived) lint, full AC coverage
 * (or waived), and an explicit accept. Editing Gherkin after accept returns the pack to Draft.
 * Never writes production Java or repository files.
 */
final class DesignReadinessComputer {
    private DesignReadinessComputer() {
    }

    static McpDesignReadiness evaluate(
            McpDesignAnalysis analysis,
            McpDesignLint lint,
            McpDesignCoverage coverage,
            String gherkin,
            String acceptRaw,
            String acceptedGherkinSnapshot) {
        String feature = gherkin == null ? "" : gherkin.strip();
        String snapshot = acceptedGherkinSnapshot == null ? "" : acceptedGherkinSnapshot.strip();

        McpDesignReadiness early = earlyGate(analysis, feature, snapshot);
        if (early != null) {
            return early;
        }

        List<String> unmet = new ArrayList<>();
        collectLintCoverage(unmet, lint, coverage);
        if (!unmet.isEmpty()) {
            return draft("Ready conditions are unmet.", unmet);
        }
        if (!isTrue(acceptRaw)) {
            unmet.add("accept: user has not accepted the pack");
            return draft("All automated gates are green; Accept to mark Ready.", unmet);
        }
        return new McpDesignReadiness(
                McpDesignReadiness.CURRENT_SCHEMA_VERSION,
                McpDesignReadiness.STATUS_READY,
                "Pack is Ready for automation handoff. Recording or live execute still required.",
                List.of(),
                true,
                false);
    }

    private static McpDesignReadiness earlyGate(
            McpDesignAnalysis analysis, String feature, String snapshot) {
        if (analysis == null || McpDesignAnalysis.STATUS_ERROR.equals(analysis.status())) {
            return draft("Analysis is required before Ready.",
                    List.of("analysis: pack analysis is missing or errored"));
        }
        if (McpDesignAnalysis.STATUS_NEEDS_QUESTIONS.equals(analysis.status())
                || analysis.blockingCount() > 0) {
            return needsQuestions("Pack still needs questions before automation handoff.",
                    List.of("analysis: blocking gaps or open questions remain"));
        }
        if (!analysis.gherkinGenerationAllowed() && !analysis.residualRiskAccepted()) {
            return needsQuestions("Analysis does not allow progressing the pack.",
                    List.of("analysis: Gherkin generation is not allowed yet"));
        }
        if (feature.isEmpty()) {
            return draft("A Gherkin draft is required for Ready.",
                    List.of("gherkin: no accepted Gherkin draft"));
        }
        if (!snapshot.isEmpty() && !Objects.equals(snapshot, feature)) {
            return draft("Gherkin changed after accept; re-accept when gates are green.",
                    List.of("gherkin: edited after Ready; pack returned to Draft"));
        }
        return null;
    }

    private static void collectLintCoverage(
            List<String> unmet, McpDesignLint lint, McpDesignCoverage coverage) {
        if (lint == null || McpDesignLint.STATUS_ERROR.equals(lint.status()) || lint.acceptBlocked()) {
            unmet.add("lint: error-level findings remain (waive with a reason or fix)");
        }
        if (coverage == null
                || McpDesignCoverage.STATUS_ERROR.equals(coverage.status())
                || coverage.readyBlocked()) {
            unmet.add("coverage: uncovered AC or untagged scenarios remain (waive with a reason or tag)");
        }
    }

    private static McpDesignReadiness draft(String message, List<String> unmet) {
        return new McpDesignReadiness(
                McpDesignReadiness.CURRENT_SCHEMA_VERSION,
                McpDesignReadiness.STATUS_DRAFT,
                message,
                unmet,
                false,
                false);
    }

    private static McpDesignReadiness needsQuestions(String message, List<String> unmet) {
        return new McpDesignReadiness(
                McpDesignReadiness.CURRENT_SCHEMA_VERSION,
                McpDesignReadiness.STATUS_NEEDS_QUESTIONS,
                message,
                unmet,
                false,
                false);
    }

    private static boolean isTrue(String raw) {
        if (raw == null || raw.isBlank()) {
            return false;
        }
        String value = raw.strip().toLowerCase(Locale.ROOT);
        return "true".equals(value) || "yes".equals(value) || "1".equals(value) || "accept".equals(value);
    }
}
