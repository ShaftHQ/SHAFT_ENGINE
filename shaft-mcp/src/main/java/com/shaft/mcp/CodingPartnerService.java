package com.shaft.mcp;

import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;

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
        List<String> suggestedCalls = suggestedMcpCalls(normalizedBackend, selectedText, evidencePaths);
        List<String> warnings = warnings(selectedText, currentSource);
        return new McpCodingPartnerPlan(
                "1.0",
                workingSetSummary(text(intent), normalizedBackend, currentSource, reuseMatches.size(), evidencePaths.size()),
                normalizedBackend,
                reuseMatches,
                missingCodeItems,
                suggestedCalls,
                verificationCommand(repository),
                evidencePaths,
                warnings);
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

    private static List<String> suggestedMcpCalls(String backend, String selectedText, List<String> evidencePaths) {
        LinkedHashSet<String> calls = new LinkedHashSet<>();
        calls.add("shaft_guide_search");
        calls.add("capture_target_candidates");
        if ("Playwright".equals(backend)) {
            calls.add("playwright_browser_get_page_dom");
            calls.add("playwright_capture_code_blocks");
        } else {
            calls.add("browser_open_intent");
            calls.add("capture_record_at_target_code_blocks");
        }
        calls.add("test_code_guardrails_check");
        if (!evidencePaths.isEmpty()) {
            calls.add("trace_summarize");
            calls.add("capture_evidence_pack");
        }
        return List.copyOf(calls);
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

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
