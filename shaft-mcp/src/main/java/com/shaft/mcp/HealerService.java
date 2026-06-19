package com.shaft.mcp;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.doctor.internal.DoctorRedactor;
import com.shaft.doctor.model.CauseCategory;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

/**
 * MCP healer loop for guarded SHAFT/Selenium reruns and review-only repair suggestions.
 */
@Service
public class HealerService {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final Duration COMMAND_TIMEOUT = Duration.ofMinutes(15);
    private static final int MAX_OUTPUT_CHARS = 24_000;
    private static final int MAX_ATTEMPTS = 5;
    private static final Set<String> ALLOWED_GOALS = Set.of(
            "compile", "test-compile", "test", "verify", "package", "install",
            "surefire:test", "failsafe:integration-test", "failsafe:verify");

    private final McpWorkspacePolicy workspacePolicy;
    private final ProcessExecutor executor;

    /**
     * Creates the default local MCP healer service.
     */
    public HealerService() {
        this(McpWorkspacePolicy.current());
    }

    HealerService(McpWorkspacePolicy workspacePolicy) {
        this(workspacePolicy, HealerService::runProcess);
    }

    HealerService(McpWorkspacePolicy workspacePolicy, ProcessExecutor executor) {
        this.workspacePolicy = workspacePolicy;
        this.executor = executor == null ? HealerService::runProcess : executor;
    }

    /**
     * Reruns a failing SHAFT/Selenium test and returns evidence-backed repair suggestions.
     *
     * @param repositoryRoot Git/Maven project root inside the MCP workspace
     * @param testCommand tokenized Maven command for the failing test
     * @param outputDirectory healer and Doctor output directory inside the MCP workspace
     * @param maxAttempts maximum rerun attempts, clamped to 1..5
     * @param includeScreenshots explicit approval to retain screenshot evidence
     * @param includePageSnapshots explicit approval to retain page-source evidence
     * @param allowedSourcePaths optional repository-relative source paths approved for provider evidence
     * @param networkValidationApproved whether Maven may run without offline mode
     * @param useConfiguredAi whether to request configured SHAFT provider snippets in addition to agent handoff
     * @param allowLocalAi explicit local provider consent when configured AI is requested
     * @param allowRemoteAi explicit remote provider consent when configured AI is requested
     * @param driverVariableName Java driver variable name used in snippets
     * @return guarded rerun attempts plus review-only remediation
     */
    @Tool(name = "healer_run_failed_test",
            description = "reruns a failing SHAFT/Selenium test, analyzes fresh Allure evidence,"
                    + " and returns review-only fixes plus agent replay guidance")
    public McpHealerRunResult runFailedTest(
            String repositoryRoot,
            List<String> testCommand,
            String outputDirectory,
            int maxAttempts,
            boolean includeScreenshots,
            boolean includePageSnapshots,
            List<String> allowedSourcePaths,
            boolean networkValidationApproved,
            boolean useConfiguredAi,
            boolean allowLocalAi,
            boolean allowRemoteAi,
            String driverVariableName) {
        Path repository = workspacePolicy.existing(repositoryRoot, "Repository root");
        if (!Files.isDirectory(repository)) {
            throw new IllegalArgumentException("Repository root must be a directory.");
        }
        List<String> command = validationCommand(testCommand, networkValidationApproved);
        Path output = outputDirectory == null || outputDirectory.isBlank()
                ? workspacePolicy.output("target/shaft-healer", "Healer output directory")
                : workspacePolicy.output(outputDirectory, "Healer output directory");
        int attemptsLimit = Math.max(1, Math.min(maxAttempts <= 0 ? 2 : maxAttempts, MAX_ATTEMPTS));
        boolean expectsAllure = expectsAllure(command);
        List<McpHealerAttemptResult> attempts = new ArrayList<>();
        McpAnalysisReport latestAnalysis = null;
        for (int attempt = 1; attempt <= attemptsLimit; attempt++) {
            AllureSnapshot before = allureSnapshot(repository);
            ProcessResult process = executor.run(command, repository, COMMAND_TIMEOUT);
            AllureSnapshot after = allureSnapshot(repository);
            List<AllureFile> changed = after.changedSince(before);
            int failed = failedCount(changed);
            boolean passed = process.successful() && (!expectsAllure || !changed.isEmpty() && failed == 0);
            attempts.add(new McpHealerAttemptResult(
                    attempt,
                    command,
                    process.exitCode(),
                    process.timedOut(),
                    passed,
                    changed.size(),
                    failed,
                    concise(process.output()),
                    changed.stream().map(file -> file.path().toString()).toList()));
            if (passed) {
                return result(McpHealerRunResult.Status.PASSED, attempts, null, List.of(
                        "The guarded rerun passed; no source change was proposed."));
            }
            if (!changed.isEmpty()) {
                latestAnalysis = analyze(
                        changed,
                        output.resolve("attempt-" + attempt),
                        repository,
                        allowedSourcePaths,
                        includeScreenshots,
                        includePageSnapshots,
                        useConfiguredAi,
                        allowLocalAi,
                        allowRemoteAi,
                        driverVariableName);
            }
        }
        if (latestAnalysis == null) {
            return result(McpHealerRunResult.Status.GUARDRAIL_STOPPED, attempts, null, List.of(
                    "No populated Allure result files changed during the guarded rerun.",
                    "Run the failing test with SHAFT reporting enabled, then call the healer again."));
        }
        McpHealerRunResult.Status status = latestAnalysis.primaryCause() == CauseCategory.PRODUCT
                ? McpHealerRunResult.Status.PRODUCT_BUG_SUSPECTED
                : McpHealerRunResult.Status.FAILED_WITH_SUGGESTIONS;
        return result(status, attempts, latestAnalysis, List.of(
                "Review the suggested snippets before editing source.",
                "Use the existing SHAFT MCP browser, DOM, screenshot, element, and natural action tools to replay"
                        + " the failing Selenium flow when more UI evidence is needed."));
    }

    private McpAnalysisReport analyze(
            List<AllureFile> changed,
            Path output,
            Path repository,
            List<String> allowedSourcePaths,
            boolean includeScreenshots,
            boolean includePageSnapshots,
            boolean useConfiguredAi,
            boolean allowLocalAi,
            boolean allowRemoteAi,
            String driverVariableName) {
        return new DoctorService(workspacePolicy, new McpDoctorRemediationService()).analyzeFailedAllure(
                changed.stream().map(file -> file.path().toString()).toList(),
                List.of(),
                output.toString(),
                includeScreenshots,
                includePageSnapshots,
                1,
                repository.toString(),
                allowedSourcePaths,
                useConfiguredAi,
                allowLocalAi,
                allowRemoteAi,
                driverVariableName);
    }

    private static McpHealerRunResult result(
            McpHealerRunResult.Status status,
            List<McpHealerAttemptResult> attempts,
            McpAnalysisReport analysis,
            List<String> warnings) {
        List<McpActionRecord> actions = new ArrayList<>();
        List<McpCodeBlock> blocks = new ArrayList<>();
        if (analysis != null) {
            actions.addAll(analysis.actions());
            blocks.addAll(analysis.codeBlocks());
            blocks.add(agentHandoffBlock(analysis));
            actions.add(new McpActionRecord(
                    "agent-replay-flow",
                    "Replay and inspect the failing Selenium flow",
                    "AGENT_REPLAY",
                    "Use the calling agent's own LLM and SHAFT MCP tools to replay the failed flow,"
                            + " inspect DOM/screenshot/current UI state, and propose the smallest evidence-backed fix.",
                    List.of(),
                    List.of("agent-healer-handoff"),
                    McpActionRecord.Status.PROVIDER_ADVISORY));
        }
        LinkedHashSet<String> mergedWarnings = new LinkedHashSet<>();
        if (analysis != null) {
            mergedWarnings.addAll(analysis.warnings());
        }
        mergedWarnings.addAll(warnings);
        return new McpHealerRunResult(
                McpHealerRunResult.CURRENT_SCHEMA_VERSION,
                status,
                attempts,
                analysis,
                actions,
                blocks,
                List.copyOf(mergedWarnings));
    }

    private static McpCodeBlock agentHandoffBlock(McpAnalysisReport analysis) {
        return new McpCodeBlock(
                "agent-healer-handoff",
                "Agent Selenium healer handoff",
                McpCodeBlock.Kind.PROVIDER_ADVISORY,
                "text",
                List.of(),
                """
                        This healer call is an implicit agent approval boundary. The calling agent may use its own
                        LLM and the available SHAFT MCP tools to replay the failed Selenium flow, inspect DOM,
                        screenshots, element state, and current browser behavior, then suggest a minimal fix for
                        user confirmation. No SHAFT provider API key is required for this agent-side reasoning.

                        Keep changes review-only until the user approves them. Prefer locator, wait, test data,
                        assertion, or product-bug evidence supported by the Doctor report.

                        Diagnosis: %s
                        """.formatted(analysis.summary()),
                "Agent: replay with SHAFT MCP browser tools when needed, then propose the smallest supported fix.",
                true,
                analysis.actions().stream()
                        .flatMap(action -> action.evidenceIds().stream())
                        .distinct()
                        .toList(),
                List.of());
    }

    private static List<String> validationCommand(List<String> requested, boolean networkApproved) {
        List<String> command = requested == null ? List.of() : new ArrayList<>(requested);
        if (command.isEmpty()) {
            throw new IllegalArgumentException("Validation command arguments are required.");
        }
        String requestedExecutable = command.getFirst().replace('\\', '/').toLowerCase(Locale.ROOT);
        String executable = Path.of(command.getFirst()).getFileName().toString().toLowerCase(Locale.ROOT);
        if (!Set.of("mvn", "mvn.cmd", "mvnw", "mvnw.cmd").contains(executable)) {
            throw new IllegalArgumentException("Only tokenized Maven validation commands are allowed.");
        }
        if (requestedExecutable.contains("/")
                && !Set.of("./mvnw", "./mvnw.cmd").contains(requestedExecutable)) {
            throw new IllegalArgumentException("Maven validation executable paths must use the repository wrapper.");
        }
        boolean hasGoal = false;
        boolean runsTests = false;
        boolean headless = false;
        for (int index = 1; index < command.size(); index++) {
            String argument = command.get(index);
            if (argument == null || argument.isBlank() || argument.length() > 2_000
                    || containsShellSyntax(argument)) {
                throw new IllegalArgumentException("Validation command contains an unsafe argument.");
            }
            String normalized = argument.toLowerCase(Locale.ROOT);
            if (normalized.equals("-f") || normalized.startsWith("--file")
                    || normalized.equals("-s") || normalized.startsWith("--settings")
                    || normalized.equals("-gs") || normalized.startsWith("--global-settings")
                    || normalized.equals("-t") || normalized.startsWith("--toolchains")
                    || normalized.startsWith("-dmaven.ext.class.path")
                    || normalized.startsWith("-dmaven.multimoduleprojectdirectory")) {
                throw new IllegalArgumentException(
                        "Validation commands cannot redirect Maven outside the approved worktree.");
            }
            if (normalized.startsWith("-p") && normalized.contains("release")) {
                throw new IllegalArgumentException("Release Maven profiles are forbidden.");
            }
            if (normalized.startsWith("-dheadlessexecution=")) {
                if (!"-dheadlessexecution=true".equals(normalized)) {
                    throw new IllegalArgumentException("Doctor browser validation must run headlessly.");
                }
                headless = true;
            }
            if (!argument.startsWith("-")) {
                if (normalized.contains("deploy") || normalized.contains("release")
                        || normalized.startsWith("scm:") || normalized.startsWith("versions:")) {
                    throw new IllegalArgumentException("Release and destructive Maven goals are forbidden.");
                }
                if (ALLOWED_GOALS.contains(normalized)) {
                    hasGoal = true;
                    runsTests |= normalized.contains("test") || Set.of("verify", "package", "install")
                            .contains(normalized);
                } else if (!previousOptionTakesValue(command, index)) {
                    throw new IllegalArgumentException("Maven validation goal is not allowlisted: " + argument);
                }
            }
        }
        if (!hasGoal) {
            throw new IllegalArgumentException("Validation command requires an allowlisted Maven goal.");
        }
        if (runsTests && !headless) {
            command.add("-DheadlessExecution=true");
        }
        if (!networkApproved && command.stream().noneMatch(value ->
                value.equals("-o") || value.equals("--offline"))) {
            command.add("--offline");
        }
        return List.copyOf(command);
    }

    private static boolean expectsAllure(List<String> command) {
        boolean skipped = command.stream()
                .map(value -> value.toLowerCase(Locale.ROOT))
                .anyMatch(value -> value.equals("-dskiptests")
                        || value.equals("-dskiptests=true")
                        || value.equals("-dmaven.test.skip=true"));
        return !skipped && command.stream()
                .map(value -> value.toLowerCase(Locale.ROOT))
                .anyMatch(value -> value.equals("test") || value.equals("verify")
                        || value.equals("package") || value.equals("install")
                        || value.equals("surefire:test")
                        || value.equals("failsafe:integration-test")
                        || value.equals("failsafe:verify"));
    }

    private static boolean previousOptionTakesValue(List<String> command, int index) {
        if (index <= 1) {
            return false;
        }
        String option = command.get(index - 1).toLowerCase(Locale.ROOT);
        if (Set.of("-pl", "--projects").contains(option)
                && (command.get(index).contains("..")
                || Path.of(command.get(index)).isAbsolute())) {
            throw new IllegalArgumentException("Maven project selection must remain repository-relative.");
        }
        return Set.of("-pl", "--projects", "-p", "--activate-profiles").contains(option);
    }

    private static boolean containsShellSyntax(String value) {
        return value.contains(";") || value.contains("&&") || value.contains("||")
                || value.contains("|") || value.contains(">") || value.contains("<")
                || value.contains("`") || value.contains("$(")
                || value.contains("\r") || value.contains("\n");
    }

    private static ProcessResult runProcess(List<String> command, Path directory, Duration timeout) {
        Path output = null;
        try {
            output = Files.createTempFile("shaft-mcp-healer-command-", ".log");
            Process process = new ProcessBuilder(command)
                    .directory(directory.toAbsolutePath().normalize().toFile())
                    .redirectErrorStream(true)
                    .redirectOutput(output.toFile())
                    .start();
            boolean finished = process.waitFor(timeout.toMillis(), TimeUnit.MILLISECONDS);
            if (!finished) {
                process.destroyForcibly();
                process.waitFor(5, TimeUnit.SECONDS);
            }
            String diagnostics = new DoctorRedactor().redact(Files.readString(output, StandardCharsets.UTF_8));
            if (diagnostics.length() > MAX_OUTPUT_CHARS) {
                diagnostics = diagnostics.substring(diagnostics.length() - MAX_OUTPUT_CHARS);
            }
            return new ProcessResult(finished ? process.exitValue() : -1, !finished, diagnostics);
        } catch (IOException exception) {
            throw new IllegalStateException("MCP healer command could not be launched.", exception);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("MCP healer command was interrupted.", exception);
        } finally {
            if (output != null) {
                try {
                    Files.deleteIfExists(output);
                } catch (IOException ignored) {
                    // Best-effort cleanup of bounded process output.
                }
            }
        }
    }

    private static AllureSnapshot allureSnapshot(Path root) {
        List<AllureFile> files = new ArrayList<>();
        try (Stream<Path> paths = Files.walk(root)) {
            for (Path path : paths
                    .filter(Files::isRegularFile)
                    .filter(value -> value.getFileName().toString().endsWith("-result.json"))
                    .toList()) {
                try {
                    JsonNode result = JSON.readTree(Files.readString(path, StandardCharsets.UTF_8));
                    if (result.path("status").asText().isBlank()
                            || result.path("name").asText().isBlank()) {
                        continue;
                    }
                    files.add(new AllureFile(
                            path.toRealPath(),
                            Files.size(path),
                            Files.getLastModifiedTime(path).toMillis(),
                            result.path("status").asText()));
                } catch (IOException ignored) {
                    // Malformed files are not populated Allure evidence.
                }
            }
        } catch (IOException exception) {
            throw new IllegalStateException("Allure validation outputs could not be inspected.", exception);
        }
        return new AllureSnapshot(List.copyOf(files));
    }

    private static int failedCount(List<AllureFile> files) {
        return Math.toIntExact(files.stream()
                .filter(file -> !"passed".equalsIgnoreCase(file.status()))
                .count());
    }

    private static String concise(String value) {
        String normalized = value == null ? "" : value.replaceAll("\\s+", " ").trim();
        return normalized.substring(0, Math.min(2_000, normalized.length()));
    }

    @FunctionalInterface
    interface ProcessExecutor {
        ProcessResult run(List<String> command, Path directory, Duration timeout);
    }

    record ProcessResult(int exitCode, boolean timedOut, String output) {
        boolean successful() {
            return !timedOut && exitCode == 0;
        }
    }

    private record AllureFile(Path path, long size, long modified, String status) {
    }

    private record AllureSnapshot(List<AllureFile> files) {
        List<AllureFile> changedSince(AllureSnapshot before) {
            Set<AllureFile> previous = new HashSet<>(before.files());
            return files.stream().filter(file -> !previous.contains(file)).toList();
        }
    }
}
