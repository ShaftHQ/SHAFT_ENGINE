package com.shaft.mcp;

import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * MCP planning tools for a repository-aware SHAFT IntelliJ coding partner.
 */
@Service
public class CodingPartnerService {
    private final McpWorkspacePolicy workspacePolicy;

    /**
     * Creates the default coding partner service.
     */
    public CodingPartnerService() {
        this(McpWorkspacePolicy.current());
    }

    CodingPartnerService(McpWorkspacePolicy workspacePolicy) {
        this.workspacePolicy = workspacePolicy;
    }

    /**
     * Builds a preview-only plan for turning user intent, existing Java code, and evidence into SHAFT changes.
     *
     * @param repositoryPath workspace-contained repository root
     * @param intent user scenario or coding intent
     * @param backend requested backend, usually webdriver or playwright
     * @param currentSourcePath current IntelliJ Java file path, absolute or repository-relative
     * @param selectedText selected source snippet or pasted manual context
     * @param artifactPaths optional evidence paths such as traces, Allure results, or screenshots
     * @param maxResults maximum reuse candidates to return
     * @return preview-only coding partner plan
     */
    @Tool(name = "shaft_coding_partner_plan",
            description = "plans a repository-aware SHAFT IntelliJ coding-partner workflow using current source,"
                    + " selected text, existing page objects/tests, capture evidence, and verification guidance")
    public McpCodingPartnerPlan plan(
            String repositoryPath,
            String intent,
            String backend,
            String currentSourcePath,
            String selectedText,
            List<String> artifactPaths,
            int maxResults) {
        Path repository = workspacePolicy.existing(repositoryPath, "Coding partner repository");
        String normalizedBackend = backend(backend, selectedText);
        List<McpJavaTargetScanner.Candidate> reuseMatches =
                new McpJavaTargetScanner().scan(repository, maxResults);
        String currentSource = normalizeCurrentSource(repository, currentSourcePath);
        List<String> evidencePaths = safeEvidencePaths(repository, artifactPaths);
        List<String> missingCodeItems = missingCodeItems(intent, selectedText, currentSource, reuseMatches);
        List<String> warnings = warnings(selectedText, currentSource);
        McpJavaTargetScanner.Candidate recommended = recommendedCandidate(currentSource, reuseMatches);
        String recommendedSourcePath = recommendedTargetSourcePath(currentSource, recommended);
        String recommendedAnchor = recommendedInsertionAnchor(recommended);
        List<McpCodingPartnerNextAction> nextActions = nextActions(
                repositoryPath,
                intent,
                normalizedBackend,
                selectedText,
                evidencePaths,
                recommendedSourcePath,
                recommendedAnchor,
                recommended);
        List<String> suggestedCalls = nextActions.stream()
                .map(McpCodingPartnerNextAction::toolName)
                .distinct()
                .toList();
        return new McpCodingPartnerPlan(
                "1.2",
                workingSetSummary(text(intent), normalizedBackend, currentSource, reuseMatches.size(), evidencePaths.size()),
                normalizedBackend,
                reuseMatches,
                stepPlan(intent, normalizedBackend, recommended),
                recommendedSourcePath,
                recommendedAnchor,
                missingCodeItems,
                suggestedCalls,
                nextActions,
                verificationCommand(repository),
                evidencePaths,
                warnings);
    }

    /**
     * Builds a preview-only unified diff that inserts reviewed SHAFT code blocks into an existing Java target.
     *
     * <p>The MCP server never writes files; the returned diff is for IntelliJ to preview and apply under
     * explicit user approval.</p>
     *
     * @param repositoryPath workspace-contained repository root
     * @param targetSourcePath Java target file, absolute or repository-relative
     * @param codeBlocks reviewed SHAFT code blocks to insert (Markdown fences are stripped)
     * @param insertionAnchor optional existing method or textual anchor to insert after
     * @return preview-only unified diff
     */
    @Tool(name = "shaft_coding_partner_diff",
            description = "produces a preview-only unified diff that inserts reviewed SHAFT code blocks into an"
                    + " existing Java target at a chosen method or textual anchor; never writes files")
    public McpCodingPartnerDiff diff(
            String repositoryPath,
            String targetSourcePath,
            List<String> codeBlocks,
            String insertionAnchor) {
        Path repository = workspacePolicy.existing(repositoryPath, "Coding partner repository");
        String relativeTarget = normalizeCurrentSource(repository, targetSourcePath);
        if (relativeTarget.isBlank()) {
            throw new IllegalArgumentException("Coding partner diff requires a target source path.");
        }
        Path target = resolveRepositoryPath(repository, relativeTarget, "Coding partner diff target");
        String displayPath = repository.relativize(target).toString().replace('\\', '/');
        String snippet = insertionSnippet(codeBlocks);
        if (snippet.isBlank()) {
            throw new IllegalArgumentException("Coding partner diff requires at least one non-empty code block.");
        }
        boolean exists = Files.isRegularFile(target);
        List<String> originalLines = readSourceLines(target, exists);
        String anchor = text(insertionAnchor);
        int insertAt = insertionIndex(originalLines, anchor);
        List<String> insertedLines = insertedLines(originalLines, snippet);
        String unifiedDiff = unifiedDiff(displayPath, originalLines, insertAt, insertedLines, exists);
        return new McpCodingPartnerDiff(
                "1.0",
                relativeTarget,
                anchor,
                exists,
                insertedLines.size(),
                unifiedDiff,
                diffWarnings(exists, snippet));
    }

    private String normalizeCurrentSource(Path repository, String currentSourcePath) {
        String value = text(currentSourcePath).replace('\\', '/');
        if (value.isBlank()) {
            return "";
        }
        Path normalized = resolveRepositoryPath(repository, value, "Current source path");
        try {
            if (Files.exists(normalized)) {
                Path real = normalized.toRealPath();
                if (!real.startsWith(repository)) {
                    throw new IllegalArgumentException("Current source path is outside the MCP repository.");
                }
                return repository.relativize(real).toString().replace('\\', '/');
            }
        } catch (IOException exception) {
            throw new IllegalArgumentException("Current source path cannot be resolved inside the MCP workspace.",
                    exception);
        }
        return value;
    }

    private List<String> safeEvidencePaths(Path repository, List<String> artifactPaths) {
        if (artifactPaths == null) {
            return List.of();
        }
        List<String> paths = new ArrayList<>();
        for (String artifactPath : artifactPaths) {
            String value = text(artifactPath).replace('\\', '/');
            if (!value.isBlank()) {
                paths.add(repository.relativize(
                        resolveRepositoryPath(repository, value, "Coding partner evidence path"))
                        .toString()
                        .replace('\\', '/'));
            }
        }
        return List.copyOf(paths);
    }

    private Path resolveRepositoryPath(Path repository, String value, String label) {
        Path raw = Path.of(value);
        Path candidate = raw.isAbsolute() ? raw : repository.resolve(raw);
        Path normalized = candidate.toAbsolutePath().normalize();
        if (!normalized.startsWith(workspacePolicy.root()) || !normalized.startsWith(repository)) {
            throw new IllegalArgumentException(label + " is outside the MCP repository.");
        }
        return normalized;
    }

    private static List<String> missingCodeItems(
            String intent,
            String selectedText,
            String currentSource,
            List<McpJavaTargetScanner.Candidate> reuseMatches) {
        List<String> items = new ArrayList<>();
        if (text(intent).isBlank()) {
            items.add("Add a concrete scenario intent before generating code.");
        }
        if (currentSource.isBlank()) {
            items.add("Open or pass the Java target file before applying source changes.");
        }
        if (reuseMatches.isEmpty()) {
            items.add("Create or identify a reusable SHAFT Page Object or test target before inserting generated code.");
        }
        if (containsRawSelenium(selectedText)) {
            items.add("Convert raw Selenium calls to SHAFT page-object methods instead of duplicating driver.findElement usage.");
        }
        if (reuseMatches.stream().noneMatch(candidate -> !candidate.locatorSummaries().isEmpty())) {
            items.add("Add missing stable SHAFT locator fields only after existing locators have been checked.");
        }
        return List.copyOf(items);
    }

    private static List<String> warnings(String selectedText, String currentSource) {
        List<String> warnings = new ArrayList<>();
        warnings.add("Plan is preview-only; IntelliJ/source edits require explicit user approval.");
        if (currentSource.isBlank()) {
            warnings.add("No current source path was supplied, so insertion must be reviewed manually.");
        }
        if (containsRawSelenium(selectedText)) {
            warnings.add("Selected text contains raw Selenium; run guardrails before accepting generated code.");
        }
        return List.copyOf(warnings);
    }

    private static McpJavaTargetScanner.Candidate recommendedCandidate(
            String currentSource,
            List<McpJavaTargetScanner.Candidate> reuseMatches) {
        if (reuseMatches == null || reuseMatches.isEmpty()) {
            return null;
        }
        String source = text(currentSource);
        if (!source.isBlank()) {
            for (McpJavaTargetScanner.Candidate candidate : reuseMatches) {
                if (source.equals(candidate.sourcePath())) {
                    return candidate;
                }
            }
        }
        return reuseMatches.get(0);
    }

    private static String recommendedTargetSourcePath(
            String currentSource,
            McpJavaTargetScanner.Candidate recommended) {
        String source = text(currentSource);
        if (!source.isBlank()) {
            return source;
        }
        return recommended == null ? "" : recommended.sourcePath();
    }

    private static String recommendedInsertionAnchor(McpJavaTargetScanner.Candidate recommended) {
        if (recommended == null || recommended.insertionAnchors().isEmpty()) {
            return "";
        }
        for (String action : recommended.actionSummaries()) {
            if (recommended.insertionAnchors().contains(action)) {
                return action;
            }
        }
        return recommended.insertionAnchors().get(0);
    }

    private static List<McpCodingPartnerNextAction> nextActions(
            String repositoryPath,
            String intent,
            String backend,
            String selectedText,
            List<String> evidencePaths,
            String recommendedSourcePath,
            String recommendedAnchor,
            McpJavaTargetScanner.Candidate recommended) {
        List<McpCodingPartnerNextAction> actions = new ArrayList<>();
        actions.add(action(
                "Read relevant SHAFT guide guidance",
                "shaft_guide_search",
                arguments("query", guideQuery(intent, backend, selectedText)),
                false,
                "Ground generated code in current SHAFT syntax before editing."));
        actions.add(action(
                "Refresh repository reuse candidates",
                "capture_target_candidates",
                arguments("repositoryPath", text(repositoryPath), "maxResults", 10),
                false,
                "Recheck Page Objects, tests, locators, and fluent actions before adding code."));
        if ("Playwright".equals(backend)) {
            actions.add(action(
                    "Inspect current Playwright page DOM",
                    "playwright_browser_get_page_dom",
                    arguments("maxCharacters", 200_000),
                    true,
                    "Use Playwright's current page snapshot before choosing locators."));
            actions.add(action(
                    "Generate Playwright capture code blocks",
                    "playwright_capture_code_blocks",
                    playwrightCodeArguments(recommended),
                    true,
                    "Requires a completed recording path before code generation."));
        } else if ("Mobile".equals(backend)) {
            actions.add(action(
                    "Inspect current mobile accessibility tree",
                    "mobile_get_accessibility_tree",
                    arguments("maxCharacters", 200_000),
                    true,
                    "Prefer Appium accessibility IDs and resource IDs before coordinate fallback."));
            actions.add(action(
                    "Generate mobile target code blocks",
                    "mobile_record_at_target_code_blocks",
                    mobileCodeArguments(recommended, recommendedSourcePath, recommendedAnchor),
                    true,
                    "Requires a completed mobile recording path before code generation."));
        } else {
            actions.add(action(
                    "Open the target URL and rank locators",
                    "browser_open_intent",
                    arguments("targetUrl", "", "userIntent", text(intent), "maxCharacters", 200_000, "maxElements", 10),
                    true,
                    "The user or project must confirm the URL before browser automation runs."));
            actions.add(action(
                    "Generate WebDriver target code blocks",
                    "capture_record_at_target_code_blocks",
                    captureCodeArguments(recommended, recommendedSourcePath, recommendedAnchor),
                    true,
                    "Requires a completed recording path before code generation."));
        }
        actions.add(action(
                "Run generated-code guardrails",
                "test_code_guardrails_check",
                arguments("language", "java"),
                true,
                "Run after code generation with the generated snippet as the code argument."));
        if (!evidencePaths.isEmpty()) {
            actions.add(action(
                    "Summarize the first trace evidence path",
                    "trace_summarize",
                    arguments("tracePath", evidencePaths.get(0)),
                    false,
                    "Use existing failure evidence before changing shared page or test code."));
            actions.add(action(
                    "Assemble local evidence pack",
                    "capture_evidence_pack",
                    arguments(
                            "sourcePath", recommendedSourcePath,
                            "reportPath", "",
                            "reviewPath", "",
                            "screenshotPaths", List.of()),
                    true,
                    "Attach source, review, report, and screenshots to the PR after generation."));
        }
        return List.copyOf(actions);
    }

    private static List<McpCodingPartnerStep> stepPlan(
            String intent,
            String backend,
            McpJavaTargetScanner.Candidate recommended) {
        List<String> instructions = instructions(intent);
        if (instructions.isEmpty()) {
            return List.of();
        }
        List<McpCodingPartnerStep> steps = new ArrayList<>();
        for (int index = 0; index < instructions.size(); index++) {
            String instruction = instructions.get(index);
            steps.add(new McpCodingPartnerStep(
                    index + 1,
                    instruction,
                    reuseHint(recommended),
                    proofTool(backend, instruction)));
        }
        return List.copyOf(steps);
    }

    private static List<String> instructions(String intent) {
        String value = text(intent);
        if (value.isBlank()) {
            return List.of();
        }
        List<String> instructions = new ArrayList<>();
        for (String rawLine : value.split("\\R+")) {
            String line = rawLine
                    .replaceFirst("^\\s*(?:[-*]|\\d+[.)])\\s*", "")
                    .replaceAll("(?i)\\s+and\\s+(?=verify\\b|assert\\b|check\\b|validate\\b)", " then ")
                    .trim();
            for (String piece : line.split("(?i)\\s+(?:then|and then)\\s+|\\s*;\\s*")) {
                String instruction = piece.trim();
                if (!instruction.isBlank()) {
                    instructions.add(instruction);
                }
            }
        }
        return List.copyOf(instructions);
    }

    private static String reuseHint(McpJavaTargetScanner.Candidate recommended) {
        if (recommended == null) {
            return "Create the smallest SHAFT page object or test only after reuse scan finds no target.";
        }
        String anchor = recommendedInsertionAnchor(recommended);
        String suffix = anchor.isBlank() ? "" : "#" + anchor;
        return "Reuse " + recommended.sourcePath() + suffix + " before creating duplicate locators, actions, or tests.";
    }

    private static String proofTool(String backend, String instruction) {
        String lower = text(instruction).toLowerCase(Locale.ROOT);
        if ("Playwright".equals(backend)) {
            return "playwright_browser_get_page_dom";
        }
        if ("Mobile".equals(backend)) {
            return "mobile_get_accessibility_tree";
        }
        if (containsAny(lower, "verify", "assert", "check", "validate", "see ")) {
            return "browser_get_page_dom";
        }
        return "browser_open_intent";
    }

    private static String workingSetSummary(
            String intent,
            String backend,
            String currentSource,
            int reuseCount,
            int evidenceCount) {
        return "Intent: " + (intent.isBlank() ? "<missing>" : intent)
                + "; backend: " + backend
                + "; current source: " + (currentSource.isBlank() ? "<none>" : currentSource)
                + "; reuse candidates: " + reuseCount
                + "; evidence paths: " + evidenceCount + ".";
    }

    private static String verificationCommand(Path repository) {
        if (Files.exists(repository.resolve("build.gradle.kts")) || Files.exists(repository.resolve("build.gradle"))) {
            return "gradle testClasses";
        }
        return "mvn -q -DskipTests test-compile";
    }

    private static String backend(String backend, String selectedText) {
        String value = text(backend).toLowerCase(Locale.ROOT);
        String source = text(selectedText).toLowerCase(Locale.ROOT);
        if (containsAny(value, "mobile", "appium", "android", "ios")
                || containsAny(source, "appium", "mobileelement", "accessibility_id", "iosdriver", "androiddriver")) {
            return "Mobile";
        }
        if (value.contains("playwright")
                || text(selectedText).contains("SHAFT.GUI.Playwright")) {
            return "Playwright";
        }
        return "WebDriver";
    }

    private static boolean containsRawSelenium(String value) {
        String source = text(value);
        return source.contains(".findElement(")
                || source.contains(".findElements(")
                || source.contains("Thread.sleep(")
                || source.contains("PageFactory");
    }

    private static boolean containsAny(String value, String... needles) {
        if (value == null || needles == null) {
            return false;
        }
        for (String needle : needles) {
            if (needle != null && !needle.isBlank() && value.contains(needle)) {
                return true;
            }
        }
        return false;
    }

    private static McpCodingPartnerNextAction action(
            String label,
            String toolName,
            Map<String, Object> arguments,
            boolean requiresConfirmation,
            String rationale) {
        return new McpCodingPartnerNextAction(label, toolName, arguments, requiresConfirmation, List.of(rationale));
    }

    private static Map<String, Object> captureCodeArguments(
            McpJavaTargetScanner.Candidate recommended,
            String recommendedSourcePath,
            String recommendedAnchor) {
        return arguments(
                "sessionPath", "",
                "outputDirectory", ".",
                "packageName", "tests.generated",
                "className", "RecordedFlowTest",
                "overwrite", false,
                "targetSourcePath", text(recommendedSourcePath),
                "insertAfter", text(recommendedAnchor),
                "driverVariableName", recommended == null ? "driver" : recommended.driverVariableName());
    }

    private static Map<String, Object> playwrightCodeArguments(McpJavaTargetScanner.Candidate recommended) {
        return arguments(
                "sessionPath", "",
                "outputDirectory", ".",
                "packageName", "tests.generated",
                "className", "RecordedFlowTest",
                "overwrite", false,
                "driverVariableName", recommended == null ? "driver" : recommended.driverVariableName());
    }

    private static Map<String, Object> mobileCodeArguments(
            McpJavaTargetScanner.Candidate recommended,
            String recommendedSourcePath,
            String recommendedAnchor) {
        return arguments(
                "recordingPath", "",
                "driverVariableName", recommended == null ? "driver" : recommended.driverVariableName(),
                "targetSourcePath", text(recommendedSourcePath),
                "insertAfter", text(recommendedAnchor));
    }

    private static Map<String, Object> arguments(Object... entries) {
        LinkedHashMap<String, Object> values = new LinkedHashMap<>();
        if (entries == null) {
            return values;
        }
        for (int index = 0; index + 1 < entries.length; index += 2) {
            values.put(textValue(entries[index]), argumentValue(entries[index + 1]));
        }
        return values;
    }

    private static Object argumentValue(Object value) {
        return value == null ? "" : value;
    }

    private static String guideQuery(String intent, String backend, String selectedText) {
        if (containsRawSelenium(selectedText)) {
            return "Selenium to SHAFT syntax page object locators guardrails";
        }
        if ("Mobile".equals(backend)) {
            return "SHAFT MCP mobile Appium locator-first recording";
        }
        if ("Playwright".equals(backend)) {
            return "SHAFT Playwright backend capture codegen locators";
        }
        String value = text(intent);
        return value.isBlank() ? "SHAFT coding partner page object locators" : value;
    }

    private static String insertionSnippet(List<String> codeBlocks) {
        if (codeBlocks == null) {
            return "";
        }
        List<String> pieces = new ArrayList<>();
        for (String block : codeBlocks) {
            String cleaned = stripFences(block);
            if (!cleaned.isBlank()) {
                pieces.add(cleaned);
            }
        }
        return String.join("\n\n", pieces).strip();
    }

    private static String stripFences(String block) {
        if (block == null) {
            return "";
        }
        String value = block.strip();
        if (value.startsWith("```")) {
            int firstNewline = value.indexOf('\n');
            value = firstNewline < 0 ? "" : value.substring(firstNewline + 1);
            if (value.endsWith("```")) {
                value = value.substring(0, value.length() - 3);
            }
        }
        return value.stripTrailing();
    }

    private static List<String> readSourceLines(Path target, boolean exists) {
        if (!exists) {
            return List.of();
        }
        String content;
        try {
            content = Files.readString(target);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Coding partner diff target cannot be read inside the MCP workspace.",
                    exception);
        }
        String normalized = content.replace("\r\n", "\n").replace('\r', '\n');
        List<String> lines = new ArrayList<>(Arrays.asList(normalized.split("\n", -1)));
        if (!lines.isEmpty() && lines.get(lines.size() - 1).isEmpty()) {
            lines.remove(lines.size() - 1);
        }
        return lines;
    }

    private static int insertionIndex(List<String> lines, String anchor) {
        if (lines.isEmpty()) {
            return 0;
        }
        if (!anchor.isBlank()) {
            for (int index = 0; index < lines.size(); index++) {
                if (lines.get(index).contains(anchor)) {
                    int methodEnd = methodEnd(lines, index);
                    if (methodEnd >= 0) {
                        return methodEnd + 1;
                    }
                    break;
                }
            }
        }
        return classInsertionIndex(lines);
    }

    private static int methodEnd(List<String> lines, int anchorLine) {
        int depth = 0;
        boolean opened = false;
        for (int index = anchorLine; index < lines.size(); index++) {
            String line = lines.get(index);
            for (int position = 0; position < line.length(); position++) {
                char character = line.charAt(position);
                if (character == '{') {
                    depth++;
                    opened = true;
                } else if (character == '}') {
                    depth--;
                    if (opened && depth == 0) {
                        return index;
                    }
                }
            }
        }
        return -1;
    }

    private static int classInsertionIndex(List<String> lines) {
        for (int index = lines.size() - 1; index >= 0; index--) {
            if ("}".equals(lines.get(index).trim())) {
                return index;
            }
        }
        return lines.size();
    }

    private static List<String> insertedLines(List<String> originalLines, String snippet) {
        List<String> lines = new ArrayList<>();
        if (!originalLines.isEmpty()) {
            lines.add("");
        }
        lines.addAll(Arrays.asList(snippet.replace("\r\n", "\n").replace('\r', '\n').split("\n", -1)));
        return lines;
    }

    private static String unifiedDiff(
            String path,
            List<String> originalLines,
            int insertAt,
            List<String> insertedLines,
            boolean targetExists) {
        StringBuilder diff = new StringBuilder();
        diff.append("--- ").append(targetExists ? "a/" + path : "/dev/null").append('\n');
        diff.append("+++ ").append("b/").append(path).append('\n');
        int size = originalLines.size();
        if (!targetExists || size == 0) {
            diff.append("@@ -0,0 +1,").append(insertedLines.size()).append(" @@\n");
            for (String line : insertedLines) {
                diff.append('+').append(line).append('\n');
            }
            return diff.toString();
        }
        int context = 3;
        int start = Math.max(0, insertAt - context);
        int endAfter = Math.min(size, insertAt + context);
        int oldCount = (insertAt - start) + (endAfter - insertAt);
        int newCount = oldCount + insertedLines.size();
        diff.append("@@ -").append(start + 1).append(',').append(oldCount)
                .append(" +").append(start + 1).append(',').append(newCount).append(" @@\n");
        for (int index = start; index < insertAt; index++) {
            diff.append(' ').append(originalLines.get(index)).append('\n');
        }
        for (String line : insertedLines) {
            diff.append('+').append(line).append('\n');
        }
        for (int index = insertAt; index < endAfter; index++) {
            diff.append(' ').append(originalLines.get(index)).append('\n');
        }
        return diff.toString();
    }

    private static List<String> diffWarnings(boolean exists, String snippet) {
        List<String> warnings = new ArrayList<>();
        warnings.add("Diff is preview-only; apply changes in IntelliJ under explicit user approval.");
        if (!exists) {
            warnings.add("Target file does not exist yet; the diff would create a new file.");
        }
        if (containsRawSelenium(snippet)) {
            warnings.add("Inserted code contains raw Selenium; run test_code_guardrails_check before applying.");
        }
        return List.copyOf(warnings);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }

    private static String textValue(Object value) {
        return value == null ? "" : value.toString().trim();
    }
}
