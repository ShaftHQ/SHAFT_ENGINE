package com.shaft.mcp;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Bounded ISTQB-style Examples for outline-shaped acceptance criteria (issue #5950).
 */
final class DesignExamplesPlanner {
    private static final List<String> HEADERS = List.of("membership", "coupon", "basketMin", "discount");
    private static final int ROW_CAP = 8;

    private DesignExamplesPlanner() {
    }

    static McpDesignExamples plan(McpDesignAnalysis analysis, List<String> droppedIds) {
        if (!DesignGherkinGate.allowed(analysis)) {
            String message = analysis == null ? "No analysis." : analysis.message();
            return error(message.isBlank() ? "Examples are blocked until blocking gaps are cleared." : message);
        }
        if (!outlineShaped(analysis.pack())) {
            return new McpDesignExamples(
                    McpDesignExamples.CURRENT_SCHEMA_VERSION,
                    McpDesignExamples.STATUS_SCENARIO,
                    "Acceptance criteria stay as Scenarios; no Outline is required.",
                    List.of(),
                    List.of(),
                    "",
                    false);
        }
        Set<String> dropped = new LinkedHashSet<>(droppedIds == null ? List.of() : droppedIds);
        List<McpDesignExampleRow> rows = new ArrayList<>();
        for (McpDesignExampleRow row : defaultRows()) {
            if (dropped.contains(row.id())) {
                continue;
            }
            if (rows.size() >= ROW_CAP) {
                break;
            }
            rows.add(row);
        }
        return new McpDesignExamples(
                McpDesignExamples.CURRENT_SCHEMA_VERSION,
                McpDesignExamples.STATUS_DRAFTED,
                "Examples ready for edit. Not written to the repository.",
                HEADERS,
                rows,
                renderOutline(analysis.pack(), rows),
                false);
    }

    static boolean outlineShaped(McpDesignPack pack) {
        String joined = (pack.outcome() + " " + pack.acceptanceCriteria().stream()
                .map(McpDesignAcceptanceCriterion::text)
                .reduce("", (left, right) -> left + " " + right)).toLowerCase(Locale.ROOT);
        return joined.contains("membership") || joined.contains("coupon")
                || joined.contains("basket") || joined.contains("discount percent");
    }

    private static List<McpDesignExampleRow> defaultRows() {
        return List.of(
                new McpDesignExampleRow("EX-01", "valid", List.of("gold", "SAVE10", "100", "15")),
                new McpDesignExampleRow("EX-02", "invalid", List.of("none", "SAVE10", "100", "0")),
                new McpDesignExampleRow("EX-03", "boundary", List.of("gold", "none", "50", "10")),
                new McpDesignExampleRow("EX-04", "alternate", List.of("silver", "SAVE5", "80", "8")));
    }

    private static String renderOutline(McpDesignPack pack, List<McpDesignExampleRow> rows) {
        String outcome = pack.outcome().isBlank() ? "the discount is applied" : pack.outcome();
        StringBuilder text = new StringBuilder();
        text.append("Feature: ").append(outcome).append("\n\n");
        text.append("  Scenario Outline: apply discount partitions\n");
        text.append("    Given a shopper with <membership> membership\n");
        text.append("    When the coupon is <coupon> and the basket is <basketMin>\n");
        text.append("    Then the discount is <discount>\n\n");
        text.append("    Examples:\n");
        text.append("      | ").append(String.join(" | ", HEADERS)).append(" |\n");
        for (McpDesignExampleRow row : rows) {
            text.append("      | ").append(String.join(" | ", row.cells())).append(" |\n");
        }
        return text.toString();
    }

    private static McpDesignExamples error(String message) {
        return new McpDesignExamples(
                McpDesignExamples.CURRENT_SCHEMA_VERSION,
                McpDesignExamples.STATUS_ERROR,
                message,
                List.of(),
                List.of(),
                "",
                false);
    }
}
