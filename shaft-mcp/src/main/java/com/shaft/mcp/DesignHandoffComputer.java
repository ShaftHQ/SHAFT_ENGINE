package com.shaft.mcp;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Serializes a Ready Design pack for Automation prefill (issue #5956).
 *
 * <p>Never invents locators, never writes Java, never invents canonical URLs.
 * Non-Ready packs return {@code blocked} with unmet conditions.
 */
final class DesignHandoffComputer {
    private DesignHandoffComputer() {
    }

    static McpDesignHandoff build(
            McpDesignReadiness readiness,
            McpDesignAnalysis analysis,
            McpDesignCoverage coverage,
            McpDesignGapMap gapMap,
            McpDesignExamples examples,
            String gherkin,
            String optionalUrl) {
        McpDesignHandoff blocked = blockedIfNotReady(readiness, gherkin);
        if (blocked != null) {
            return blocked;
        }
        String feature = gherkin.strip();
        List<String> scenarios = scenarios(feature);
        List<String> acIds = acceptanceCriteria(coverage, feature);
        List<Map<String, String>> exampleRows = exampleRows(examples);
        List<McpDesignGapMapStep> gaps = gapMap == null ? List.of() : gapMap.steps();
        List<McpDesignOracle> oracles = analysis == null ? List.of() : analysis.oracles();
        String url = optionalUrl == null ? "" : optionalUrl.strip();
        return new McpDesignHandoff(
                McpDesignHandoff.CURRENT_SCHEMA_VERSION,
                McpDesignHandoff.STATUS_READY,
                "Automation prefill ready. Capture or live execute still required; no Java written.",
                List.of(),
                scenarios,
                acIds,
                exampleRows,
                gaps,
                oracles,
                url,
                automationPrefill(scenarios, acIds, oracles, url, feature),
                false);
    }

    private static McpDesignHandoff blockedIfNotReady(McpDesignReadiness readiness, String gherkin) {
        if (readiness == null) {
            return blocked("Readiness is required before handoff.", List.of("readiness: missing"));
        }
        if (!readiness.handoffAllowed()
                || !McpDesignReadiness.STATUS_READY.equals(readiness.status())) {
            List<String> unmet = readiness.unmetConditions().isEmpty()
                    ? List.of("readiness: status is " + readiness.status())
                    : readiness.unmetConditions();
            return blocked("Pack is not Ready; Automation handoff is disabled.", unmet);
        }
        if (gherkin == null || gherkin.isBlank()) {
            return blocked("Accepted Gherkin is required for handoff.", List.of("gherkin: empty"));
        }
        return null;
    }

    private static McpDesignHandoff blocked(String message, List<String> unmet) {
        return new McpDesignHandoff(
                McpDesignHandoff.CURRENT_SCHEMA_VERSION,
                McpDesignHandoff.STATUS_BLOCKED,
                message,
                unmet,
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                "",
                Map.of(),
                false);
    }

    private static List<String> scenarios(String feature) {
        List<String> names = new ArrayList<>();
        for (String raw : feature.lines().toList()) {
            String line = stripLeadingTags(raw.strip());
            String lower = line.toLowerCase(Locale.ROOT);
            if (lower.startsWith("scenario outline:")) {
                names.add(line.substring("scenario outline:".length()).strip());
            } else if (lower.startsWith("scenario:")) {
                names.add(line.substring("scenario:".length()).strip());
            }
        }
        return List.copyOf(names);
    }

    private static String stripLeadingTags(String line) {
        String current = line;
        while (current.startsWith("@")) {
            int space = current.indexOf(' ');
            if (space < 0) {
                return "";
            }
            current = current.substring(space + 1).strip();
        }
        return current;
    }

    private static List<String> acceptanceCriteria(McpDesignCoverage coverage, String feature) {
        if (coverage != null && !coverage.covered().isEmpty()) {
            return List.copyOf(coverage.covered());
        }
        List<String> tags = new ArrayList<>();
        for (String raw : feature.lines().toList()) {
            for (String token : raw.strip().split(" ")) {
                if (token.regionMatches(true, 0, "@AC-", 0, 4)) {
                    String id = token.substring(1).toUpperCase(Locale.ROOT);
                    if (!tags.contains(id)) {
                        tags.add(id);
                    }
                }
            }
        }
        return List.copyOf(tags);
    }

    private static List<Map<String, String>> exampleRows(McpDesignExamples examples) {
        if (examples == null || examples.rows().isEmpty()) {
            return List.of();
        }
        List<String> headers = examples.headers();
        List<Map<String, String>> rows = new ArrayList<>();
        for (McpDesignExampleRow row : examples.rows()) {
            Map<String, String> mapped = new LinkedHashMap<>();
            List<String> cells = row.cells();
            for (int i = 0; i < cells.size(); i++) {
                String key = i < headers.size() ? headers.get(i) : "col" + (i + 1);
                mapped.put(key, cells.get(i));
            }
            if (!mapped.isEmpty()) {
                rows.add(Map.copyOf(mapped));
            }
        }
        return List.copyOf(rows);
    }

    private static Map<String, String> automationPrefill(
            List<String> scenarios,
            List<String> acIds,
            List<McpDesignOracle> oracles,
            String url,
            String feature) {
        Map<String, String> prefill = new LinkedHashMap<>();
        prefill.put("intent", scenarios.isEmpty() ? "Ready Design pack" : String.join("; ", scenarios));
        prefill.put("acceptanceCriteria", String.join(",", acIds));
        String oracleText = "";
        for (McpDesignOracle item : oracles) {
            String piece = item.acId() + ":" + item.oracle();
            oracleText = oracleText.isEmpty() ? piece : oracleText + " | " + piece;
        }
        prefill.put("oracles", oracleText);
        if (!url.isEmpty()) {
            prefill.put("url", url);
        }
        prefill.put("gherkinDigest", Integer.toString(feature.hashCode()));
        prefill.put("requiresCaptureOrLiveExecute", "true");
        return Map.copyOf(prefill);
    }
}
