package com.shaft.mcp;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Deterministic playbook practices 1–10. Does not parse playbook.md at runtime (issue #5948).
 */
final class DesignPlaybookRules {
    static final Set<Integer> PRACTICES = Set.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

    private static final List<String> VAGUE = List.of(
            "test everything", "test the website", "test the app", "test this",
            "as needed", "as appropriate", "user-friendly", "easy to use",
            "etc.", "tbd", "somehow", "properly", "intuitively");
    private static final List<String> NFR = List.of(
            "performance", "latency", "accessibility", "accessible", "wcag",
            "security", "secure", "robust", "fast", "quickly");
    private static final Pattern MEASURABLE = Pattern.compile(
            "(?i)\\b(then|must|shall|shown|displayed|returned|places|placed|emptied|empties|"
                    + "announced|status|error|within|seconds|ms)\\b|\\d");
    private static final Pattern CHECKOUT = Pattern.compile("(?i)\\b(checkout|check out|cart|payment|order)\\b");
    private static final Pattern DECLINE = Pattern.compile("(?i)declin|fail|reject|denied|unauthor");
    private static final Pattern DUPLICATE = Pattern.compile("(?i)\\b(duplicate|idempoten)\\b");
    private static final Pattern MONEY = Pattern.compile("(?i)\\b(price|total|currency|dollar|money|visa|rounding)\\b");
    private static final Pattern ROUND = Pattern.compile("(?i)round");

    private DesignPlaybookRules() {
    }

    static List<DraftGap> collect(McpDesignPack pack) {
        List<DraftGap> gaps = new ArrayList<>();
        addActorOutcome(pack, gaps);
        addVagueAndUnverifiable(pack, gaps);
        addContradictions(pack, gaps);
        addDomainQuestions(pack, gaps);
        addNfrHints(pack, gaps);
        if (!hasMeasurable(pack)) {
            forceNonWaivableStoryGaps(pack, gaps);
        }
        return gaps;
    }

    static boolean hasMeasurable(McpDesignPack pack) {
        for (McpDesignAcceptanceCriterion criterion : pack.acceptanceCriteria()) {
            if (measurable(criterion.text())) {
                return true;
            }
        }
        return false;
    }

    static boolean measurable(String text) {
        return text != null && MEASURABLE.matcher(text).find();
    }

    private static void addActorOutcome(McpDesignPack pack, List<DraftGap> gaps) {
        if (pack.actor() == null || pack.actor().isBlank()) {
            gaps.add(DraftGap.omission(1, traced(pack),
                    "No stakeholder actor was stated.",
                    "Who is the user or stakeholder this requirement serves?",
                    hasMeasurable(pack)));
        }
        if (pack.outcome() == null || pack.outcome().isBlank()) {
            gaps.add(DraftGap.omission(1, traced(pack),
                    "No observable business outcome was stated.",
                    "What outcome must be true after this behavior succeeds?",
                    hasMeasurable(pack)));
        }
    }

    private static void addVagueAndUnverifiable(McpDesignPack pack, List<DraftGap> gaps) {
        for (McpDesignAcceptanceCriterion criterion : pack.acceptanceCriteria()) {
            String text = lower(criterion.text());
            if (containsAny(text, VAGUE)) {
                gaps.add(new DraftGap("unverifiable", "blocking", 1, List.of(criterion.id()),
                        "Criterion is too vague to verify.",
                        "Rewrite this as an observable, measurable acceptance criterion.",
                        2, hasMeasurable(pack) && measurable(criterion.text())));
                continue;
            }
            if (!measurable(criterion.text()) && !containsAny(text, NFR)) {
                gaps.add(new DraftGap("unverifiable", "blocking", 2, List.of(criterion.id()),
                        "Criterion has no observable oracle.",
                        "What evidence would prove this criterion passed or failed?",
                        9, hasMeasurable(pack)));
            }
        }
    }

    private static void addContradictions(McpDesignPack pack, List<DraftGap> gaps) {
        List<McpDesignAcceptanceCriterion> criteria = pack.acceptanceCriteria();
        for (int left = 0; left < criteria.size(); left++) {
            for (int right = left + 1; right < criteria.size(); right++) {
                if (!opposites(criteria.get(left).text(), criteria.get(right).text())) {
                    continue;
                }
                gaps.add(new DraftGap("contradiction", "blocking", 1,
                        List.of(criteria.get(left).id(), criteria.get(right).id()),
                        "Acceptance criteria contradict each other.",
                        "Which of these two criteria is the intended behavior?",
                        4, true));
            }
        }
    }

    private static void addDomainQuestions(McpDesignPack pack, List<DraftGap> gaps) {
        if (!hasMeasurable(pack)) {
            return;
        }
        String joined = joined(pack);
        if (CHECKOUT.matcher(joined).find() && !DECLINE.matcher(joined).find()) {
            gaps.add(DraftGap.question(5, traced(pack),
                    "Failure or declined-payment behavior is not specified.",
                    "What happens when payment is declined?"));
        }
        if (CHECKOUT.matcher(joined).find() && !DUPLICATE.matcher(joined).find()) {
            gaps.add(DraftGap.question(5, traced(pack),
                    "Duplicate submit or idempotency behavior is not specified.",
                    "What happens when the same order is submitted twice?"));
        }
        if (MONEY.matcher(joined).find() && !ROUND.matcher(joined).find()) {
            gaps.add(DraftGap.question(8, traced(pack),
                    "Currency rounding is not specified.",
                    "How are monetary totals rounded?"));
        }
    }

    private static void addNfrHints(McpDesignPack pack, List<DraftGap> gaps) {
        for (McpDesignAcceptanceCriterion criterion : pack.acceptanceCriteria()) {
            String text = lower(criterion.text());
            if (!containsAny(text, NFR) || text.matches(".*\\d.*")) {
                continue;
            }
            gaps.add(new DraftGap("nfr_hint", "warning", 8, List.of(criterion.id()),
                    "Non-functional hint has no measurable threshold.",
                    "What measurable non-functional limit applies, if any?",
                    6, true));
        }
    }

    private static void forceNonWaivableStoryGaps(McpDesignPack pack, List<DraftGap> gaps) {
        List<DraftGap> locked = new ArrayList<>();
        for (DraftGap gap : gaps) {
            locked.add(gap.withWaivable(false));
        }
        gaps.clear();
        gaps.addAll(locked);
        gaps.add(0, DraftGap.omission(2, traced(pack),
                "Story is not testable; measurable acceptance criteria are missing.",
                "What observable behavior must hold, including failure cases?",
                false));
    }

    static boolean opposites(String left, String right) {
        PolarPhrase first = PolarPhrase.parse(left);
        PolarPhrase second = PolarPhrase.parse(right);
        return !first.core().isBlank() && first.core().equals(second.core()) && first.negated() != second.negated();
    }

    private static List<String> traced(McpDesignPack pack) {
        return pack.acceptanceCriteria().stream().map(McpDesignAcceptanceCriterion::id).toList();
    }

    private static String joined(McpDesignPack pack) {
        StringBuilder builder = new StringBuilder();
        builder.append(pack.actor()).append(' ').append(pack.outcome());
        for (McpDesignAcceptanceCriterion criterion : pack.acceptanceCriteria()) {
            builder.append(' ').append(criterion.text());
        }
        return builder.toString();
    }

    private static boolean containsAny(String haystack, List<String> needles) {
        for (String needle : needles) {
            if (haystack.contains(needle)) {
                return true;
            }
        }
        return false;
    }

    private static String lower(String value) {
        return value == null ? "" : value.toLowerCase(Locale.ROOT);
    }

    record DraftGap(String kind, String severity, int rank, List<String> tracedAcIds, String summary,
                    String question, int practice, boolean waivable) {
        DraftGap withWaivable(boolean value) {
            return new DraftGap(kind, severity, rank, tracedAcIds, summary, question, practice, value);
        }

        static DraftGap omission(int practice, List<String> traced, String summary, String question,
                                 boolean waivable) {
            return new DraftGap("omission", "blocking", 1, traced, summary, question, practice, waivable);
        }

        static DraftGap question(int practice, List<String> traced, String summary, String question) {
            return new DraftGap("question", "blocking", 3, traced, summary, question, practice, true);
        }
    }

    private record PolarPhrase(String core, boolean negated) {
        static PolarPhrase parse(String text) {
            String normalized = lower(text).replaceAll("[^a-z0-9 ]+", " ").replaceAll("\\s+", " ").strip();
            boolean negated = normalized.contains("must not") || normalized.contains("shall not")
                    || normalized.contains("does not") || normalized.contains("cannot");
            String core = normalized
                    .replace("must not", " ")
                    .replace("shall not", " ")
                    .replace("does not", " ")
                    .replace("cannot", " ")
                    .replace("must", " ")
                    .replace("shall", " ")
                    .replace("then", " ")
                    .replaceAll("\\s+", " ")
                    .strip();
            return new PolarPhrase(core, negated);
        }
    }
}
