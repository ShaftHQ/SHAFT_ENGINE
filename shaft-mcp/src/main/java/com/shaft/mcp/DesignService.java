package com.shaft.mcp;

import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Stage 1 ingest: turn pasted story text or a workspace file into a reviewable pack. Does not
 * write files and does not fetch URLs (issue #5947).
 */
@Service
public class DesignService {
    private static final Pattern SECRET = Pattern.compile(
            "(?i)(?:password|token|secret|api[_-]?key)\\s*[=:]\\s*\\S+|bearer\\s+\\S+");
    private static final Pattern HOME_PATH = Pattern.compile("(?i)(?:/home|/Users)/\\S+");
    private static final Pattern ACTOR = Pattern.compile("(?im)^\\s*as an?\\s+(.+?)\\s*$");
    private static final Pattern WANT = Pattern.compile("(?im)^\\s*i want(?: to)?\\s+(.+?)\\s*$");
    private static final Pattern SO_THAT = Pattern.compile("(?im)^\\s*so that\\s+(.+?)\\s*$");
    private static final Pattern BULLET = Pattern.compile("(?m)^\\s*(?:[-*]|\\d+[.)])\\s+(.+?)\\s*$");

    private final McpWorkspacePolicy workspacePolicy;

    /** Production constructor using the current MCP workspace. */
    public DesignService() {
        this(McpWorkspacePolicy.current());
    }

    DesignService(McpWorkspacePolicy workspacePolicy) {
        this.workspacePolicy = workspacePolicy;
    }

    /**
     * Ingests a user story or acceptance criteria into a structured pack.
     *
     * @param text       pasted story text
     * @param filePath   workspace-relative file to read
     * @param sourceUrl  recorded source URL (not fetched)
     * @return pack; {@code wroteFiles} is always false
     */
    @Tool(name = "design_ingest",
            description = "turns pasted user-story or acceptance-criteria text, or a workspace file, "
                    + "into a structured Design pack with actor, outcome, and numbered AC IDs; never "
                    + "writes files and does not fetch URLs")
    public McpDesignPack ingest(String text, String filePath, String sourceUrl) {
        try {
            Source source = resolveSource(blank(text), blank(filePath), blank(sourceUrl));
            if (source.pack() != null) {
                return source.pack();
            }
            return parsePack(source);
        } catch (IllegalArgumentException | IOException exception) {
            return error("paste", exception.getMessage());
        }
    }

    /**
     * Runs the requirements-analysis playbook against an ingested pack.
     *
     * @param text            pasted story text
     * @param filePath        workspace-relative file to read
     * @param sourceUrl       recorded source URL (not fetched)
     * @param acceptedGapIds  comma-separated waivable GAP IDs to accept, or empty
     * @return analysis; {@code wroteFiles} is always false and Gherkin is never emitted
     */
    @Tool(name = "design_analyze",
            description = "runs the requirements-analysis playbook on a story or workspace file and "
                    + "returns a gap register with stable GAP IDs; never writes files and does not "
                    + "generate Gherkin")
    public McpDesignAnalysis analyze(String text, String filePath, String sourceUrl, String acceptedGapIds) {
        McpDesignPack pack = ingest(text, filePath, sourceUrl);
        return DesignAnalyzer.analyze(pack, DesignAnalyzer.parseAcceptedIds(acceptedGapIds));
    }

    @Tool(name = "design_gherkin_draft",
            description = "drafts declarative Gherkin from an analyzed story pack for human review; never writes files or Java")
    public McpDesignGherkinDraft gherkinDraft(String text, String filePath, String sourceUrl, String acceptedGapIds) {
        return DesignGherkinDrafter.draft(analyze(text, filePath, sourceUrl, acceptedGapIds));
    }

    @Tool(name = "design_examples",
            description = "proposes a Scenario Outline and editable Examples rows from an analyzed pack; never writes files")
    public McpDesignExamples examples(String text, String filePath, String sourceUrl, String acceptedGapIds,
                                      String droppedExampleIds) {
        return DesignExamplesPlanner.plan(analyze(text, filePath, sourceUrl, acceptedGapIds),
                DesignAnalyzer.parseAcceptedIds(droppedExampleIds));
    }

    @Tool(name = "design_lexicon",
            description = "suggests or accepts project-local business phrases; never indexes SHAFT locator steps")
    public McpDesignLexicon lexicon(String action, String query, String phrase) {
        try {
            if ("accept".equalsIgnoreCase(action == null ? "" : action.strip())) {
                return DesignLexiconStore.accept(workspacePolicy.root(), phrase);
            }
            return DesignLexiconStore.suggest(workspacePolicy.root(), query);
        } catch (java.io.IOException exception) {
            return new McpDesignLexicon(McpDesignLexicon.CURRENT_SCHEMA_VERSION, "error",
                    exception.getMessage(), java.util.List.of(), false);
        }
    }

    @Tool(name = "design_coverage",
            description = "maps AC IDs to Gherkin @AC-* scenario tags; uncovered AC blocks Ready unless waived with a reason")
    public McpDesignCoverage coverage(String text, String filePath, String sourceUrl, String acceptedGapIds,
                                      String gherkin, String waived) {
        McpDesignPack pack = ingest(text, filePath, sourceUrl);
        String feature = gherkin == null ? "" : gherkin.strip();
        if (feature.isEmpty()) {
            feature = gherkinDraft(text, filePath, sourceUrl, acceptedGapIds).feature();
        }
        return DesignCoverageComputer.compute(pack, feature, waived);
    }

    @Tool(name = "design_lint",
            description = "lints a Gherkin draft for missing Then, click/xpath smells, duplicates, and invented SHAFT APIs; fail closed on errors")
    public McpDesignLint lint(String text, String filePath, String sourceUrl, String acceptedGapIds,
                              String gherkin, String waived) {
        String feature = gherkin == null ? "" : gherkin.strip();
        if (feature.isEmpty()) {
            feature = gherkinDraft(text, filePath, sourceUrl, acceptedGapIds).feature();
        }
        return DesignLintComputer.lint(feature, waived);
    }

    @Tool(name = "design_gap_map",
            description = "classifies accepted Gherkin steps as mapped to SHAFT fluent API, new-helper, needs-recording, or ambiguous; never writes production Java")
    public McpDesignGapMap gapMap(String text, String filePath, String sourceUrl, String acceptedGapIds,
                                  String gherkin) {
        String feature = gherkin == null ? "" : gherkin.strip();
        if (feature.isEmpty()) {
            feature = gherkinDraft(text, filePath, sourceUrl, acceptedGapIds).feature();
        }
        return DesignGapMapComputer.map(feature);
    }

    private Source resolveSource(String text, String filePath, String sourceUrl) throws IOException {
        if (text.isEmpty() && filePath.isEmpty() && sourceUrl.isEmpty()) {
            return Source.error("paste", "Paste a user story or provide a workspace file path.");
        }
        if (text.isEmpty() && filePath.isEmpty()) {
            return Source.error("url",
                    "URL ingest does not fetch in v1; paste the story text and pass sourceUrl as a reference.");
        }
        if (!filePath.isEmpty()) {
            String raw = Files.readString(workspacePolicy.existing(filePath, "filePath"), StandardCharsets.UTF_8);
            return Source.body("file", raw);
        }
        return Source.body(sourceUrl.isEmpty() ? "paste" : "url", text);
    }

    private static McpDesignPack parsePack(Source source) {
        String raw = source.raw() == null ? "" : source.raw().strip();
        if (raw.isEmpty()) {
            return error(source.kind(), "Story text is empty after reading the source.");
        }
        String redacted = redact(raw);
        String actor = firstGroup(ACTOR, redacted);
        List<String> warnings = new ArrayList<>();
        List<McpDesignAcceptanceCriterion> criteria = criteriaFrom(redacted);
        if (actor.isBlank()) {
            warnings.add("No 'As a …' actor line was found.");
        }
        if (criteria.isEmpty()) {
            criteria = List.of(new McpDesignAcceptanceCriterion("AC-01", firstParagraph(redacted)));
            warnings.add("No bullet or numbered criteria found; the body was stored as AC-01.");
        }
        return new McpDesignPack(
                McpDesignPack.CURRENT_SCHEMA_VERSION,
                "ok",
                "Ingested " + criteria.size() + " acceptance criteria. No files were written.",
                actor,
                joinOutcome(firstGroup(WANT, redacted), firstGroup(SO_THAT, redacted)),
                source.kind(),
                List.copyOf(criteria),
                List.copyOf(warnings),
                false);
    }

    private static String blank(String value) {
        return value == null ? "" : value.strip();
    }

    private record Source(String kind, String raw, McpDesignPack pack) {
        static Source error(String kind, String message) {
            return new Source(kind, "", DesignService.error(kind, message));
        }

        static Source body(String kind, String raw) {
            return new Source(kind, raw, null);
        }
    }

    private static McpDesignPack error(String sourceKind, String message) {
        return new McpDesignPack(
                McpDesignPack.CURRENT_SCHEMA_VERSION,
                "error",
                redact(message),
                "",
                "",
                sourceKind,
                List.of(),
                List.of(),
                false);
    }

    static String redact(String value) {
        if (value == null || value.isBlank()) {
            return "";
        }
        String redacted = SECRET.matcher(value).replaceAll("[redacted]");
        return HOME_PATH.matcher(redacted).replaceAll("[redacted-path]");
    }

    private static String firstGroup(Pattern pattern, String text) {
        Matcher matcher = pattern.matcher(text);
        return matcher.find() ? matcher.group(1).strip() : "";
    }

    private static String joinOutcome(String want, String soThat) {
        if (want.isBlank()) {
            return soThat;
        }
        if (soThat.isBlank()) {
            return want;
        }
        return want + " so that " + soThat;
    }

    private static List<McpDesignAcceptanceCriterion> criteriaFrom(String text) {
        List<McpDesignAcceptanceCriterion> criteria = new ArrayList<>();
        Matcher matcher = BULLET.matcher(text);
        int index = 1;
        while (matcher.find()) {
            String line = matcher.group(1).strip();
            if (looksLikeStoryScaffold(line)) {
                continue;
            }
            criteria.add(new McpDesignAcceptanceCriterion("AC-" + String.format(Locale.ROOT, "%02d", index), line));
            index++;
        }
        return criteria;
    }

    private static boolean looksLikeStoryScaffold(String line) {
        String lower = line.toLowerCase(Locale.ROOT);
        return lower.startsWith("as a ") || lower.startsWith("as an ")
                || lower.startsWith("i want") || lower.startsWith("so that");
    }

    private static String firstParagraph(String text) {
        String[] parts = text.strip().split("\\R\\s*\\R", 2);
        return parts[0].strip();
    }
}
