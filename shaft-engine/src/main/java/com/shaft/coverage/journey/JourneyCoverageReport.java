package com.shaft.coverage.journey;

import java.util.Collection;
import java.util.stream.Collectors;

/**
 * Deterministic text report for journey / interactive-state coverage.
 */
public final class JourneyCoverageReport {
    private JourneyCoverageReport() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * @param map coverage map
     * @return multi-line summary with unknown/unobservable separated from uncovered (FR-4)
     */
    public static String summarize(CoverageMap map) {
        StringBuilder out = new StringBuilder("Journey interactive-state coverage summary");
        out.append('\n').append("Covered (").append(map.covered().size()).append("): ")
                .append(join(map.covered()));
        out.append('\n').append("Uncovered (").append(map.uncovered().size()).append("): ")
                .append(join(map.uncovered()));
        out.append('\n').append("Unknown (").append(map.unknown().size()).append("): ")
                .append(join(map.unknown()));
        out.append('\n').append("Unobservable (").append(map.unobservable().size()).append("): ")
                .append(join(map.unobservable()));
        return out.toString();
    }

    /**
     * @param comparison baseline comparison
     * @return summary that surfaces baseline reductions before merge (SC-3)
     */
    public static String summarize(BaselineComparison comparison) {
        StringBuilder out = new StringBuilder(summarize(comparison.map()));
        out.append('\n').append("Baseline: ").append(comparison.baselineName());
        out.append('\n').append("Baseline reductions (").append(comparison.reductions().size()).append("): ")
                .append(join(comparison.reductions()));
        out.append('\n').append("Baseline additions (").append(comparison.additions().size()).append("): ")
                .append(join(comparison.additions()));
        out.append('\n').append("Has reductions before merge: ").append(comparison.hasReductions());
        return out.toString();
    }

    private static String join(Collection<CoverageTarget> targets) {
        if (targets == null || targets.isEmpty()) {
            return "(none)";
        }
        return targets.stream().map(CoverageTarget::key).collect(Collectors.joining(", "));
    }
}
