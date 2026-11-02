package com.shaft.mcp;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Maps pack AC IDs onto {@code @AC-*} scenario tags (issue #5952).
 */
final class DesignCoverageComputer {
    private static final Pattern AC_TAG = Pattern.compile("@(AC-\\d+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern SCENARIO = Pattern.compile(
            "^\\s*Scenario(?:\\s+Outline)?\\s*:\\s*(.+?)\\s*$", Pattern.CASE_INSENSITIVE);

    private DesignCoverageComputer() {
    }

    static McpDesignCoverage compute(McpDesignPack pack, String gherkin, String waivedRaw) {
        if (pack == null || "error".equals(pack.status())) {
            String message = pack == null || pack.message().isBlank() ? "No pack." : pack.message();
            return error(message);
        }
        Set<String> known = knownIds(pack);
        Map<String, String> waived = parseWaived(waivedRaw);
        Set<String> tagged = new LinkedHashSet<>();
        List<String> untagged = new ArrayList<>();
        collectScenarioTags(gherkin == null ? "" : gherkin, tagged, untagged);
        tagged.retainAll(known);
        return classify(known, tagged, waived, untagged);
    }

    private static McpDesignCoverage error(String message) {
        return new McpDesignCoverage(
                McpDesignCoverage.CURRENT_SCHEMA_VERSION,
                McpDesignCoverage.STATUS_ERROR,
                message,
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                true,
                false);
    }

    private static Set<String> knownIds(McpDesignPack pack) {
        Set<String> ids = new LinkedHashSet<>();
        for (McpDesignAcceptanceCriterion criterion : pack.acceptanceCriteria()) {
            if (criterion != null && criterion.id() != null && !criterion.id().isBlank()) {
                ids.add(criterion.id());
            }
        }
        return ids;
    }

    static Map<String, String> parseWaived(String waivedRaw) {
        Map<String, String> waived = new LinkedHashMap<>();
        if (waivedRaw == null || waivedRaw.isBlank()) {
            return waived;
        }
        for (String part : waivedRaw.split(",")) {
            int split = part.indexOf(':');
            if (split <= 0) {
                continue;
            }
            String id = part.substring(0, split).strip();
            String reason = part.substring(split + 1).strip();
            if (!id.isEmpty() && !reason.isEmpty()) {
                waived.put(id, reason);
            }
        }
        return waived;
    }

    static void collectScenarioTags(String gherkin, Set<String> tagged, List<String> untagged) {
        Set<String> pending = new LinkedHashSet<>();
        for (String rawLine : gherkin.split("\\R")) {
            String line = rawLine.strip();
            if (line.isEmpty() || line.startsWith("#")) {
                continue;
            }
            Matcher scenario = SCENARIO.matcher(line);
            if (scenario.matches()) {
                if (pending.isEmpty()) {
                    untagged.add(scenario.group(1).strip());
                } else {
                    tagged.addAll(pending);
                }
                pending.clear();
                continue;
            }
            if (line.toLowerCase(Locale.ROOT).startsWith("feature:")) {
                pending.clear();
                continue;
            }
            Matcher tag = AC_TAG.matcher(line);
            while (tag.find()) {
                pending.add(tag.group(1).toUpperCase(Locale.ROOT));
            }
        }
    }

    private static McpDesignCoverage classify(
            Set<String> known, Set<String> tagged, Map<String, String> waived, List<String> untagged) {
        List<String> covered = new ArrayList<>();
        List<String> uncovered = new ArrayList<>();
        List<McpDesignWaivedAc> waivedItems = new ArrayList<>();
        for (String id : known) {
            if (tagged.contains(id)) {
                covered.add(id);
            } else if (waived.containsKey(id)) {
                waivedItems.add(new McpDesignWaivedAc(id, waived.get(id)));
            } else {
                uncovered.add(id);
            }
        }
        boolean readyBlocked = !uncovered.isEmpty() || !untagged.isEmpty();
        String status = readyBlocked ? McpDesignCoverage.STATUS_BLOCKED : McpDesignCoverage.STATUS_OK;
        String message = readyBlocked
                ? "Uncovered AC or untagged scenarios block Ready until resolved or waived with a reason."
                : "All acceptance criteria are covered or waived.";
        return new McpDesignCoverage(
                McpDesignCoverage.CURRENT_SCHEMA_VERSION,
                status,
                message,
                covered,
                uncovered,
                waivedItems,
                untagged,
                readyBlocked,
                false);
    }
}
