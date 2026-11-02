package com.shaft.mcp;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Turns an ingested pack plus optional accepted gap IDs into a gap register (issue #5948).
 */
final class DesignAnalyzer {
    private DesignAnalyzer() {
    }

    static McpDesignAnalysis analyze(McpDesignPack pack, List<String> acceptedGapIds) {
        if (pack == null || McpDesignAnalysis.STATUS_ERROR.equals(pack.status())) {
            return error(pack);
        }
        List<DesignPlaybookRules.DraftGap> drafts = DesignPlaybookRules.collect(pack);
        List<McpDesignGap> gaps = assignIds(sort(drafts), acceptedGapIds);
        return summarize(pack, gaps, oraclesFor(pack));
    }

    static List<String> parseAcceptedIds(String acceptedGapIds) {
        if (acceptedGapIds == null || acceptedGapIds.isBlank()) {
            return List.of();
        }
        Set<String> ids = new LinkedHashSet<>();
        for (String part : acceptedGapIds.split(",")) {
            String id = part.strip();
            if (!id.isEmpty()) {
                ids.add(id);
            }
        }
        return List.copyOf(ids);
    }

    private static McpDesignAnalysis error(McpDesignPack pack) {
        McpDesignPack safe = pack == null
                ? new McpDesignPack(McpDesignPack.CURRENT_SCHEMA_VERSION, "error", "No pack.",
                "", "", "paste", List.of(), List.of(), false)
                : pack;
        return new McpDesignAnalysis(
                McpDesignAnalysis.CURRENT_SCHEMA_VERSION,
                McpDesignAnalysis.STATUS_ERROR,
                safe.message(),
                safe,
                List.of(),
                List.of(),
                0,
                false,
                false,
                McpDesignAnalysis.PLAYBOOK,
                false);
    }

    private static List<DesignPlaybookRules.DraftGap> sort(List<DesignPlaybookRules.DraftGap> drafts) {
        List<DesignPlaybookRules.DraftGap> sorted = new ArrayList<>(drafts);
        sorted.sort(Comparator
                .comparing((DesignPlaybookRules.DraftGap gap) -> !"blocking".equals(gap.severity()))
                .thenComparing(DesignPlaybookRules.DraftGap::kind)
                .thenComparing(gap -> gap.tracedAcIds().isEmpty() ? "" : gap.tracedAcIds().get(0))
                .thenComparing(DesignPlaybookRules.DraftGap::summary));
        return sorted;
    }

    private static List<McpDesignGap> assignIds(List<DesignPlaybookRules.DraftGap> drafts,
                                                List<String> acceptedGapIds) {
        Set<String> accepted = Set.copyOf(acceptedGapIds);
        List<McpDesignGap> gaps = new ArrayList<>();
        int index = 1;
        for (DesignPlaybookRules.DraftGap draft : drafts) {
            String id = "GAP-" + String.format(Locale.ROOT, "%02d", index);
            boolean marked = draft.waivable() && accepted.contains(id);
            gaps.add(new McpDesignGap(id, draft.kind(), draft.severity(), index, draft.tracedAcIds(),
                    draft.summary(), draft.question(), draft.practice(), draft.waivable(), marked));
            index++;
        }
        return gaps;
    }

    private static List<McpDesignOracle> oraclesFor(McpDesignPack pack) {
        List<McpDesignOracle> oracles = new ArrayList<>();
        for (McpDesignAcceptanceCriterion criterion : pack.acceptanceCriteria()) {
            if (!DesignPlaybookRules.measurable(criterion.text())) {
                continue;
            }
            oracles.add(new McpDesignOracle(
                    criterion.id(),
                    "Observe: " + criterion.text(),
                    "Assertion or log matching the stated outcome for " + criterion.id()));
        }
        return oracles;
    }

    private static McpDesignAnalysis summarize(McpDesignPack pack, List<McpDesignGap> gaps,
                                               List<McpDesignOracle> oracles) {
        int blocking = 0;
        int acceptedBlocking = 0;
        boolean nonWaivableBlocking = false;
        for (McpDesignGap gap : gaps) {
            if (!"blocking".equals(gap.severity())) {
                continue;
            }
            if (gap.accepted()) {
                acceptedBlocking++;
                continue;
            }
            blocking++;
            if (!gap.waivable()) {
                nonWaivableBlocking = true;
            }
        }
        String status = statusFor(blocking, acceptedBlocking, nonWaivableBlocking);
        boolean allowed = blocking == 0 && !nonWaivableBlocking;
        return new McpDesignAnalysis(
                McpDesignAnalysis.CURRENT_SCHEMA_VERSION,
                status,
                messageFor(status, blocking),
                pack,
                gaps,
                oracles,
                blocking,
                allowed,
                McpDesignAnalysis.STATUS_RESIDUAL.equals(status),
                McpDesignAnalysis.PLAYBOOK,
                false);
    }

    private static String statusFor(int blocking, int acceptedBlocking, boolean nonWaivableBlocking) {
        if (nonWaivableBlocking || blocking > 0) {
            return McpDesignAnalysis.STATUS_NEEDS_QUESTIONS;
        }
        if (acceptedBlocking > 0) {
            return McpDesignAnalysis.STATUS_RESIDUAL;
        }
        return McpDesignAnalysis.STATUS_COMPLETE;
    }

    private static String messageFor(String status, int blocking) {
        if (McpDesignAnalysis.STATUS_NEEDS_QUESTIONS.equals(status)) {
            return "Needs questions: " + blocking + " blocking gap(s) remain. Gherkin is disabled.";
        }
        if (McpDesignAnalysis.STATUS_RESIDUAL.equals(status)) {
            return "Residual risk accepted. Gherkin generation is allowed for S1-03.";
        }
        return "Analysis complete. No blocking gaps.";
    }
}
