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
            return ingestInternal(text, filePath, sourceUrl);
        } catch (IllegalArgumentException | IOException exception) {
            return error("paste", exception.getMessage());
        }
    }

    private McpDesignPack ingestInternal(String text, String filePath, String sourceUrl) throws IOException {
        String trimmedText = text == null ? "" : text.strip();
        String trimmedFile = filePath == null ? "" : filePath.strip();
        String trimmedUrl = sourceUrl == null ? "" : sourceUrl.strip();
        if (trimmedText.isEmpty() && trimmedFile.isEmpty() && trimmedUrl.isEmpty()) {
            return error("paste", "Paste a user story or provide a workspace file path.");
        }
        if (!trimmedUrl.isEmpty() && trimmedText.isEmpty() && trimmedFile.isEmpty()) {
            return error("url", "URL ingest does not fetch in v1; paste the story text and pass sourceUrl as a reference.");
        }
        String sourceKind = "paste";
        String raw = trimmedText;
        if (!trimmedFile.isEmpty()) {
            sourceKind = "file";
            raw = Files.readString(workspacePolicy.existing(trimmedFile, "filePath"), StandardCharsets.UTF_8);
        } else if (!trimmedUrl.isEmpty()) {
            sourceKind = "url";
        }
        if (raw == null || raw.strip().isEmpty()) {
            return error(sourceKind, "Story text is empty after reading the source.");
        }
        String redacted = redact(raw);
        String actor = firstGroup(ACTOR, redacted);
        String want = firstGroup(WANT, redacted);
        String soThat = firstGroup(SO_THAT, redacted);
        String outcome = joinOutcome(want, soThat);
        List<McpDesignAcceptanceCriterion> criteria = criteriaFrom(redacted);
        List<String> warnings = new ArrayList<>();
        if (actor == null || actor.isBlank()) {
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
                actor == null ? "" : actor,
                outcome,
                sourceKind,
                List.copyOf(criteria),
                List.copyOf(warnings),
                false);
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
