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
        List<String> unmet = new ArrayList<>();
        String feature = gherkin == null ? "" : gherkin.strip();
        String snapshot = acceptedGherkinSnapshot == null ? "" : acceptedGherkinSnapshot.strip();
        boolean accept = isTrue(acceptRaw);

        if (analysis == null || McpDesignAnalysis.STATUS_ERROR.equals(analysis.status())) {
            unmet.add("analysis: pack analysis is missing or errored");
            return draft("Analysis is required before Ready.", unmet);
        }
        if (McpDesignAnalysis.STATUS_NEEDS_QUESTIONS.equals(analysis.status())
                || analysis.blockingCount() > 0) {
            unmet.add("analysis: blocking gaps or open questions remain");
            return needsQuestions("Pack still needs questions before automation handoff.", unmet);
        }
        if (!analysis.gherkinGenerationAllowed() && !analysis.residualRiskAccepted()) {
            unmet.add("analysis: Gherkin generation is not allowed yet");
            return needsQuestions("Analysis does not allow progressing the pack.", unmet);
        }
        if (feature.isEmpty()) {
            unmet.add("gherkin: no accepted Gherkin draft");
            return draft("A Gherkin draft is required for Ready.", unmet);
        }
        if (!snapshot.isEmpty() && !Objects.equals(snapshot, feature)) {
            unmet.add("gherkin: edited after Ready; pack returned to Draft");
            return draft("Gherkin changed after accept; re-accept when gates are green.", unmet);
        }
        if (lint == null || McpDesignLint.STATUS_ERROR.equals(lint.status()) || lint.acceptBlocked()) {
            unmet.add("lint: error-level findings remain (waive with a reason or fix)");
        }
        if (coverage == null
                || McpDesignCoverage.STATUS_ERROR.equals(coverage.status())
                || coverage.readyBlocked()) {
            unmet.add("coverage: uncovered AC or untagged scenarios remain (waive with a reason or tag)");
        }
        if (!unmet.isEmpty()) {
            return draft("Ready conditions are unmet.", unmet);
        }
        if (!accept) {
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
