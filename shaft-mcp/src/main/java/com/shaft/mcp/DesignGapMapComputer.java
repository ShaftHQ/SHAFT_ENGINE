package com.shaft.mcp;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Heuristic fluent-API gap map for Design Gherkin (issue #5954).
 *
 * <p>Stage 1 never writes production Java and never invents locator strings. Known
 * assertion-shaped Then steps map to real {@code Validations} entry points; unknown
 * journey When steps are {@code needs-recording}; ambiguous catalog hits stay listed.
 */
final class DesignGapMapComputer {
    private static final Pattern STEP = Pattern.compile(
            "^\\s*(Given|When|Then|And|But)\\b(.*)$", Pattern.CASE_INSENSITIVE);
    private static final List<CatalogEntry> CATALOG = List.of(
            new CatalogEntry(
                    Pattern.compile("\\b(?:should\\s+equal|equals|is\\s+equal\\s+to)\\b", Pattern.CASE_INSENSITIVE),
                    "Validations",
                    "assertThat",
                    "Validations.assertThat().object(...).isEqualTo(...)"),
            new CatalogEntry(
                    Pattern.compile(
                            "\\b(?:is\\s+displayed|is\\s+visible|should\\s+see|should\\s+be\\s+visible)\\b",
                            Pattern.CASE_INSENSITIVE),
                    "Validations",
                    "assertThat",
                    "Validations.assertThat().element(...).exists()"),
            new CatalogEntry(
                    Pattern.compile("\\b(?:url\\s+contains|browser\\s+url)\\b", Pattern.CASE_INSENSITIVE),
                    "Validations",
                    "assertThat",
                    "Validations.assertThat().browser(...).url()"));

    private DesignGapMapComputer() {
    }

    static McpDesignGapMap map(String gherkin) {
        String feature = gherkin == null ? "" : gherkin.strip();
        if (feature.isEmpty()) {
            return new McpDesignGapMap(
                    McpDesignGapMap.CURRENT_SCHEMA_VERSION,
                    McpDesignGapMap.STATUS_ERROR,
                    "No Gherkin to map.",
                    List.of(),
                    false);
        }
        List<McpDesignGapMapStep> steps = new ArrayList<>();
        for (String raw : feature.split("\\R")) {
            Matcher matcher = STEP.matcher(raw.strip());
            if (!matcher.matches()) {
                continue;
            }
            String keyword = matcher.group(1);
            String body = matcher.group(2) == null ? "" : matcher.group(2).strip();
            String line = (keyword + " " + body).strip();
            steps.add(classify(keyword, body, line));
        }
        if (steps.isEmpty()) {
            return new McpDesignGapMap(
                    McpDesignGapMap.CURRENT_SCHEMA_VERSION,
                    McpDesignGapMap.STATUS_ERROR,
                    "No Given/When/Then steps found.",
                    List.of(),
                    false);
        }
        return new McpDesignGapMap(
                McpDesignGapMap.CURRENT_SCHEMA_VERSION,
                McpDesignGapMap.STATUS_OK,
                "Mapped " + steps.size() + " step(s). No production Java was written.",
                steps,
                false);
    }

    private static McpDesignGapMapStep classify(String keyword, String body, String line) {
        List<CatalogEntry> hits = new ArrayList<>();
        for (CatalogEntry entry : CATALOG) {
            if (entry.pattern().matcher(body).find()) {
                hits.add(entry);
            }
        }
        if (hits.size() > 1) {
            List<String> labels = new ArrayList<>();
            for (CatalogEntry hit : hits) {
                labels.add(hit.typeName() + "#" + hit.methodName());
            }
            return new McpDesignGapMapStep(
                    line,
                    McpDesignGapMapStep.AMBIGUOUS,
                    "",
                    "",
                    labels,
                    "Multiple fluent candidates; list only, do not pick silently.");
        }
        if (hits.size() == 1) {
            CatalogEntry hit = hits.get(0);
            return new McpDesignGapMapStep(
                    line,
                    McpDesignGapMapStep.MAPPED,
                    hit.typeName(),
                    hit.methodName(),
                    List.of(),
                    hit.snippet());
        }
        String lower = body.toLowerCase(Locale.ROOT);
        if (isJourney(keyword, lower)) {
            return new McpDesignGapMapStep(
                    line,
                    McpDesignGapMapStep.NEEDS_RECORDING,
                    "",
                    "",
                    List.of(),
                    "Unknown journey step; record live rather than inventing locators.");
        }
        if (looksLikePageHelper(lower)) {
            return new McpDesignGapMapStep(
                    line,
                    McpDesignGapMapStep.NEW_HELPER,
                    "",
                    "",
                    List.of(),
                    "Domain page phrasing without a catalog fluent hit; needs a new helper.");
        }
        return new McpDesignGapMapStep(
                line,
                McpDesignGapMapStep.NEEDS_RECORDING,
                "",
                "",
                List.of(),
                "No fluent catalog hit; default to live recording.");
    }

    private static boolean isJourney(String keyword, String lower) {
        if (keyword == null) {
            return false;
        }
        String key = keyword.toLowerCase(Locale.ROOT);
        if (!key.equals("when") && !key.equals("and") && !key.equals("but")) {
            return false;
        }
        return lower.contains("complete")
                || lower.contains("checkout")
                || lower.contains("navigat")
                || lower.contains("fills")
                || lower.contains("submit")
                || lower.contains("logs in")
                || lower.contains("signs in");
    }

    private static boolean looksLikePageHelper(String lower) {
        return lower.contains(" page ")
                || lower.endsWith(" page")
                || lower.contains("page object")
                || lower.contains("on the ");
    }

    private record CatalogEntry(Pattern pattern, String typeName, String methodName, String snippet) {
    }
}
