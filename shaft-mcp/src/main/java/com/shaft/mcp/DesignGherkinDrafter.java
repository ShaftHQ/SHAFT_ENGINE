package com.shaft.mcp;

import java.util.Locale;

/**
 * Deterministic Feature/Scenario text from an allowed analysis pack (issue #5949).
 */
final class DesignGherkinDrafter {
    private DesignGherkinDrafter() {
    }

    static McpDesignGherkinDraft draft(McpDesignAnalysis analysis) {
        if (!DesignGherkinGate.allowed(analysis)) {
            String message = analysis == null ? "No analysis." : analysis.message();
            return new McpDesignGherkinDraft(
                    McpDesignGherkinDraft.CURRENT_SCHEMA_VERSION,
                    McpDesignGherkinDraft.STATUS_ERROR,
                    message.isBlank() ? "Gherkin is blocked until blocking gaps are cleared." : message,
                    "",
                    false);
        }
        McpDesignPack pack = analysis.pack();
        for (McpDesignAcceptanceCriterion criterion : pack.acceptanceCriteria()) {
            if (looksLikeLocator(criterion.text())) {
                return new McpDesignGherkinDraft(
                        McpDesignGherkinDraft.CURRENT_SCHEMA_VERSION,
                        McpDesignGherkinDraft.STATUS_ERROR,
                        "Draft forbids locator or xpath phrasing.",
                        "",
                        false);
            }
        }
        String actor = pack.actor().isBlank() ? "user" : pack.actor();
        String outcome = pack.outcome().isBlank() ? "the action completes" : pack.outcome();
        StringBuilder feature = new StringBuilder();
        feature.append("Feature: ").append(capitalize(outcome)).append('\n');
        for (McpDesignAcceptanceCriterion criterion : pack.acceptanceCriteria()) {
            feature.append('\n')
                    .append("  @").append(criterion.id()).append('\n')
                    .append("  Scenario: ").append(criterion.text()).append('\n')
                    .append("    Given a ").append(actor).append('\n')
                    .append("    When ").append(outcome).append('\n')
                    .append("    Then ").append(criterion.text()).append('\n');
        }
        return new McpDesignGherkinDraft(
                McpDesignGherkinDraft.CURRENT_SCHEMA_VERSION,
                McpDesignGherkinDraft.STATUS_DRAFTED,
                "Gherkin draft ready for review. Not written to the repository.",
                feature.toString(),
                false);
    }

    static boolean looksLikeLocator(String text) {
        String lower = text == null ? "" : text.toLowerCase(Locale.ROOT);
        return lower.contains("xpath") || lower.contains("by.xpath") || lower.contains("css selector")
                || lower.contains("locator.");
    }

    private static String capitalize(String value) {
        if (value == null || value.isBlank()) {
            return "Story";
        }
        return Character.toUpperCase(value.charAt(0)) + value.substring(1);
    }
}
