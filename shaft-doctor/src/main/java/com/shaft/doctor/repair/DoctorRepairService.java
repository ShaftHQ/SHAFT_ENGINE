package com.shaft.doctor.repair;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.doctor.format.DoctorJsonCodec;
import com.shaft.doctor.internal.DoctorHashing;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.EvidenceBundle;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

/**
 * Creates reviewed repairs in isolated Git worktrees and publishes only
 * explicitly approved draft pull requests.
 */
public final class DoctorRepairService {
    private static final Duration GIT_TIMEOUT = Duration.ofMinutes(2);
    private static final Duration GITHUB_TIMEOUT = Duration.ofMinutes(3);
    private static final Duration STALE_WORKTREE_AGE = Duration.ofHours(24);
    private static final ObjectMapper JSON = new ObjectMapper();

    private final RepairProcessRunner processes;
    private final RepairProposalStore store;
    private final RepairGuard guard;
    private final DoctorJsonCodec doctorJson;

    /**
     * Creates the default repair service.
     */
    public DoctorRepairService() {
        this(new RepairProcessRunner(), new RepairProposalStore(), new RepairGuard(),
                new DoctorJsonCodec());
    }

    DoctorRepairService(
            RepairProcessRunner processes,
            RepairProposalStore store,
            RepairGuard guard,
            DoctorJsonCodec doctorJson) {
        this.processes = processes;
        this.store = store;
        this.guard = guard;
        this.doctorJson = doctorJson;
    }

    /**
     * Creates and validates a reviewable proposal without changing the caller's
     * current branch or tracked files and without writing to GitHub.
     *
     * @param request explicit repair and validation policy
     * @return persisted proposal, complete diff, and approval token
     */
    public RepairProposal propose(DoctorRepairRequest request) {
        if (request == null) {
            throw new IllegalArgumentException("Doctor repair request is required.");
        }
        Path repository = realDirectory(request.repositoryRoot(), "Repository root");
        verifyRepositoryRoot(repository);
        verifyFullSha(request.baseSha());
        String resolvedBase = git(repository, "rev-parse", request.baseSha() + "^{commit}").trim();
        if (!resolvedBase.equalsIgnoreCase(request.baseSha())) {
            throw new IllegalArgumentException("Doctor repair requires an exact approved base SHA.");
        }
        Diagnosis diagnosis = readDiagnosis(request.diagnosisPath());
        EvidenceBundle bundle = readBundle(request.evidenceBundlePath());
        Set<String> evidenceIds = new LinkedHashSet<>(evidenceIds(diagnosis, bundle));
        request.allowedPaths().stream()
                .map(value -> value.replace('\\', '/'))
                .forEach(evidenceIds::add);
        Path output = request.outputDirectory().toAbsolutePath().normalize();
        createDirectory(output, "Doctor repair output directory");

        String proposalId = proposalId(request.issueReference());
        String branchName = "codex/doctor-" + slug(request.issueReference()) + "-" + proposalId;
        Path worktree = createWorktree(repository, resolvedBase, branchName, proposalId);
        Path manifest = output.resolve("repair-proposal-" + proposalId + ".json");
        try {
            List<Path> approved = guard.allowedPaths(worktree, request.allowedPaths());
            List<RepairProposal.PatchManifestEntry> patchManifest = applyPatches(
                    worktree, approved, request.patches(), request.maxPatchBytes(), evidenceIds);
            List<String> createdPaths = patchManifest.stream()
                    .filter(patch -> patch.operation() == DoctorRepairRequest.FilePatch.Operation.CREATE)
                    .map(RepairProposal.PatchManifestEntry::path)
                    .toList();
            if (!createdPaths.isEmpty()) {
                List<String> intentToAdd = new ArrayList<>(List.of("git", "add", "-N", "--"));
                intentToAdd.addAll(createdPaths);
                runRequired(intentToAdd, worktree, GIT_TIMEOUT,
                        "New Doctor repair files could not be included in the review diff.");
            }
            String diff = reviewDiff(worktree,
                    "--src-prefix=a/", "--dst-prefix=b/", resolvedBase, "--");
            List<List<String>> validationPlan = request.validationCommands().stream()
                    .map(command -> guard.validationCommand(
                            command, request.networkValidationApproved()))
                    .toList();
            List<RepairProposal.ValidationResult> validationResults =
                    validate(worktree, validationPlan, request.commandTimeout());
            String validatedDiff = reviewDiff(worktree,
                    "--src-prefix=a/", "--dst-prefix=b/", resolvedBase, "--");
            if (!validatedDiff.equals(diff)) {
                throw new IllegalStateException(
                        "Validation changed tracked repair files outside the reviewed patch.");
            }
            boolean passed = !validationResults.isEmpty()
                    && validationResults.stream().allMatch(RepairProposal.ValidationResult::passed);
            String risk = passed
                    ? "Automated validation passed; human review remains required for behavioral and scope risk."
                    : "One or more validations failed or produced no populated passing Allure evidence.";
            String token = approvalToken(proposalId, resolvedBase, diff);
            RepairProposal proposal = new RepairProposal(
                    RepairProposal.CURRENT_SCHEMA_VERSION,
                    proposalId,
                    RepairProposal.Status.PROPOSED,
                    repository.toString(),
                    resolvedBase,
                    branchName,
                    worktree.toString(),
                    request.issueReference(),
                    request.diagnosisPath().toAbsolutePath().normalize().toString(),
                    request.evidenceBundlePath() == null ? ""
                            : request.evidenceBundlePath().toAbsolutePath().normalize().toString(),
                    patchManifest,
                    validationPlan,
                    validationResults,
                    diff,
                    risk,
                    "Close the draft pull request and delete branch " + branchName
                            + "; no commit is created before publication approval.",
                    token,
                    manifest.toString(),
                    null);
            store.write(manifest, proposal);
            return proposal;
        } catch (RuntimeException exception) {
            removeWorktree(repository, worktree);
            deleteLocalBranch(repository, branchName);
            throw exception;
        }
    }

    /**
     * Commits, pushes, and opens or reuses a draft pull request only after a
     * separate explicit approval action.
     *
     * @param request publication approval and optional failed-validation override
     * @return draft publication details
     */
    public RepairPublicationResult publishDraft(DoctorRepairPublicationRequest request) {
        if (request == null || !request.approved()) {
            throw new IllegalArgumentException("Explicit publication approval is required.");
        }
        RepairProposal proposal = store.read(request.manifestPath());
        if (proposal.status() == RepairProposal.Status.CANCELLED) {
            throw new IllegalArgumentException("Cancelled Doctor repair proposals cannot be published.");
        }
        if (!tokensEqual(proposal.approvalToken(), request.approvalToken())) {
            throw new IllegalArgumentException("Doctor repair approval token is invalid.");
        }
        if (proposal.status() == RepairProposal.Status.PUBLISHED && proposal.publication() != null) {
            return new RepairPublicationResult(
                    proposal.proposalId(),
                    proposal.branchName(),
                    proposal.publication().commitSha(),
                    proposal.publication().url(),
                    true);
        }
        if (!proposal.validationPassed()) {
            if (!request.overrideFailedValidation() || request.overrideRationale().isBlank()) {
                throw new IllegalArgumentException(
                        "Failed validation blocks publication without an explicit rationale.");
            }
        }
        if (proposal.unifiedDiff().isBlank()) {
            throw new IllegalArgumentException("An empty Doctor repair proposal cannot be published.");
        }

        Path repository = realDirectory(Path.of(proposal.repositoryRoot()), "Repository root");
        Path worktree = realDirectory(Path.of(proposal.worktreePath()), "Doctor worktree");
        String head = verifyWorktree(repository, worktree, proposal);
        String commitSha;
        if (head.equals(proposal.baseSha())) {
            String currentDiff = reviewDiff(worktree,
                    "--src-prefix=a/", "--dst-prefix=b/", proposal.baseSha(), "--");
            if (!currentDiff.equals(proposal.unifiedDiff())) {
                throw new IllegalArgumentException("Doctor worktree changed after proposal review.");
            }
            List<String> patchPaths = proposal.patches().stream()
                    .map(RepairProposal.PatchManifestEntry::path)
                    .toList();
            List<String> add = new ArrayList<>(List.of("git", "add", "--"));
            add.addAll(patchPaths);
            runRequired(add, worktree, GIT_TIMEOUT, "Doctor repair files could not be staged.");
            Path hooks = emptyHooksDirectory();
            try {
                runRequired(List.of(
                                "git", "-c", "core.hooksPath=" + hooks,
                                "-c", "user.name=SHAFT Doctor Agent",
                                "-c", "user.email=shaft-doctor@users.noreply.github.com",
                                "commit", "--author=SHAFT Doctor <shaft-doctor@users.noreply.github.com>",
                                "-m", "fix: apply reviewed SHAFT Doctor repair"),
                        worktree, GIT_TIMEOUT, "Doctor repair commit could not be created.");
            } finally {
                deleteTree(hooks);
            }
            commitSha = git(worktree, "rev-parse", "HEAD").trim();
        } else {
            String parent = git(worktree, "rev-parse", "HEAD^").trim();
            String committedDiff = reviewDiff(worktree,
                    "--src-prefix=a/", "--dst-prefix=b/", proposal.baseSha(), "HEAD", "--");
            if (!parent.equals(proposal.baseSha()) || !committedDiff.equals(proposal.unifiedDiff())) {
                throw new IllegalArgumentException(
                        "Doctor proposal contains an unreviewed commit after an interrupted publication.");
            }
            commitSha = head;
        }
        Path hooks = emptyHooksDirectory();
        try {
            runRequired(List.of(
                            "git", "-c", "core.hooksPath=" + hooks,
                            "push", "-u", "origin", proposal.branchName()),
                    worktree, GITHUB_TIMEOUT, "Doctor repair branch could not be pushed.");
        } finally {
            deleteTree(hooks);
        }

        ExistingPullRequest existing = existingPullRequest(worktree, proposal.branchName());
        String pullRequestUrl;
        boolean reused;
        if (existing != null) {
            if (!existing.draft() || !"OPEN".equalsIgnoreCase(existing.state())) {
                throw new IllegalStateException(
                        "An existing non-draft or closed pull request prevents Doctor publication.");
            }
            pullRequestUrl = existing.url();
            reused = true;
        } else {
            Path body = writePullRequestBody(proposal, commitSha, request);
            try {
                String title = request.title().isBlank()
                        ? "Draft: SHAFT Doctor repair for " + proposal.issueReference()
                        : request.title();
                RepairProcessRunner.ProcessResult created = processes.run(List.of(
                                "gh", "pr", "create", "--draft",
                                "--head", proposal.branchName(),
                                "--title", title,
                                "--body-file", body.toString()),
                        worktree, GITHUB_TIMEOUT);
                if (!created.successful()) {
                    throw new IllegalStateException("Draft pull request could not be created: "
                            + concise(created.output()));
                }
                pullRequestUrl = firstUrl(created.output());
                reused = false;
            } finally {
                deleteIfExists(body);
            }
        }

        RepairProposal published = new RepairProposal(
                proposal.schemaVersion(),
                proposal.proposalId(),
                RepairProposal.Status.PUBLISHED,
                proposal.repositoryRoot(),
                proposal.baseSha(),
                proposal.branchName(),
                proposal.worktreePath(),
                proposal.issueReference(),
                proposal.diagnosisPath(),
                proposal.evidenceBundlePath(),
                proposal.patches(),
                proposal.validationPlan(),
                proposal.validationResults(),
                proposal.unifiedDiff(),
                proposal.risk(),
                proposal.rollback(),
                proposal.approvalToken(),
                proposal.manifestPath(),
                new RepairProposal.Publication(
                        pullRequestUrl,
                        commitSha,
                        request.overrideFailedValidation(),
                        request.overrideRationale()));
        store.write(request.manifestPath(), published);
        removeWorktree(repository, worktree);
        return new RepairPublicationResult(
                proposal.proposalId(), proposal.branchName(), commitSha, pullRequestUrl, reused);
    }

    /**
     * Cancels an unpublished proposal and removes its isolated worktree.
     *
     * @param manifestPath proposal manifest
     * @param approvalToken proposal approval token
     */
    public void cancel(Path manifestPath, String approvalToken) {
        RepairProposal proposal = store.read(manifestPath);
        if (!tokensEqual(proposal.approvalToken(), approvalToken)) {
            throw new IllegalArgumentException("Doctor repair approval token is invalid.");
        }
        if (proposal.status() == RepairProposal.Status.PUBLISHED) {
            throw new IllegalArgumentException("Published Doctor repair proposals cannot be cancelled.");
        }
        Path repository = realDirectory(Path.of(proposal.repositoryRoot()), "Repository root");
        Path worktree = Path.of(proposal.worktreePath()).toAbsolutePath().normalize();
        removeWorktree(repository, worktree);
        deleteLocalBranch(repository, proposal.branchName());
        RepairProposal cancelled = new RepairProposal(
                proposal.schemaVersion(), proposal.proposalId(), RepairProposal.Status.CANCELLED,
                proposal.repositoryRoot(), proposal.baseSha(), proposal.branchName(),
                proposal.worktreePath(), proposal.issueReference(), proposal.diagnosisPath(),
                proposal.evidenceBundlePath(), proposal.patches(), proposal.validationPlan(),
                proposal.validationResults(), proposal.unifiedDiff(), proposal.risk(),
                proposal.rollback(), proposal.approvalToken(), proposal.manifestPath(), null);
        store.write(manifestPath, cancelled);
    }

    private List<RepairProposal.PatchManifestEntry> applyPatches(
            Path worktree,
            List<Path> allowedPaths,
            List<DoctorRepairRequest.FilePatch> patches,
            long maxPatchBytes,
            Set<String> evidenceIds) {
        long retained = 0;
        Set<Path> seen = new HashSet<>();
        List<RepairProposal.PatchManifestEntry> manifest = new ArrayList<>();
        for (DoctorRepairRequest.FilePatch patch : patches) {
            RepairGuard.GuardedPatch guarded = guard.guard(
                    worktree, allowedPaths, patch, maxPatchBytes - retained, evidenceIds);
            if (!seen.add(guarded.relativePath())) {
                throw new IllegalArgumentException("A Doctor repair path may be patched only once.");
            }
            retained += guarded.sizeBytes();
            try {
                if (patch.operation() == DoctorRepairRequest.FilePatch.Operation.DELETE) {
                    Files.delete(guarded.target());
                } else {
                    Files.createDirectories(guarded.target().getParent());
                    Files.write(guarded.target(), guarded.content());
                }
            } catch (IOException exception) {
                throw new IllegalStateException("Doctor repair patch could not be applied.", exception);
            }
            manifest.add(new RepairProposal.PatchManifestEntry(
                    guarded.relativePath().toString().replace('\\', '/'),
                    patch.operation(),
                    guarded.contentSha256(),
                    guarded.sizeBytes(),
                    patch.rationale(),
                    patch.evidenceIds()));
        }
        return List.copyOf(manifest);
    }

    private List<RepairProposal.ValidationResult> validate(
            Path worktree,
            List<List<String>> commands,
            Duration timeout) {
        List<RepairProposal.ValidationResult> results = new ArrayList<>();
        for (List<String> command : commands) {
            AllureSnapshot before = allureSnapshot(worktree);
            RepairProcessRunner.ProcessResult process = processes.run(command, worktree, timeout);
            AllureSnapshot after = allureSnapshot(worktree);
            AllureVerdict allure = after.changedSince(before);
            boolean expectsAllure = RepairGuard.expectsAllure(command);
            boolean passed = process.successful()
                    && (!expectsAllure || allure.count() > 0 && allure.failed() == 0);
            List<String> diagnostics = new ArrayList<>();
            if (!process.output().isBlank()) {
                diagnostics.add(concise(process.output()));
            }
            if (expectsAllure) {
                diagnostics.add(allure.count() == 0
                        ? "Validation produced no populated Allure result files."
                        : "Validation produced " + allure.count() + " populated Allure result file(s), "
                        + allure.failed() + " non-passing.");
            }
            results.add(new RepairProposal.ValidationResult(
                    command,
                    process.exitCode(),
                    process.timedOut(),
                    passed,
                    allure.count(),
                    List.copyOf(diagnostics)));
        }
        return List.copyOf(results);
    }

    private Path createWorktree(
            Path repository,
            String baseSha,
            String branchName,
            String proposalId) {
        recoverStaleWorktrees(repository);
        runRequired(List.of("git", "worktree", "prune"), repository, GIT_TIMEOUT,
                "Stale Git worktrees could not be pruned.");
        try {
            Path worktree = Files.createTempDirectory("shaft-doctor-" + proposalId + "-")
                    .toAbsolutePath().normalize();
            Files.delete(worktree);
            RepairProcessRunner.ProcessResult added = processes.run(List.of(
                            "git", "worktree", "add", "-b", branchName,
                            worktree.toString(), baseSha),
                    repository, GIT_TIMEOUT);
            if (!added.successful()) {
                deleteTree(worktree);
                deleteLocalBranch(repository, branchName);
                throw new IllegalStateException("Isolated Doctor worktree could not be created: "
                        + concise(added.output()));
            }
            return worktree.toRealPath();
        } catch (IOException exception) {
            throw new IllegalStateException("Doctor worktree path could not be created.", exception);
        }
    }

    private void verifyRepositoryRoot(Path repository) {
        String top = git(repository, "rev-parse", "--show-toplevel").trim();
        Path resolved = realDirectory(Path.of(top), "Git repository root");
        if (!resolved.equals(repository)) {
            throw new IllegalArgumentException("Doctor repair repository root must be the Git top level.");
        }
    }

    private String verifyWorktree(Path repository, Path worktree, RepairProposal proposal) {
        String common = git(worktree, "rev-parse", "--git-common-dir").trim();
        Path commonPath = worktree.resolve(common).normalize();
        try {
            commonPath = commonPath.toRealPath();
            Path repositoryGit = repository.resolve(".git").toRealPath();
            if (!commonPath.equals(repositoryGit)) {
                throw new IllegalArgumentException("Doctor proposal worktree belongs to another repository.");
            }
        } catch (IOException exception) {
            throw new IllegalArgumentException("Doctor proposal worktree metadata cannot be verified.", exception);
        }
        String branch = git(worktree, "branch", "--show-current").trim();
        String head = git(worktree, "rev-parse", "HEAD").trim();
        if (!branch.equals(proposal.branchName())) {
            throw new IllegalArgumentException("Doctor proposal branch changed after review.");
        }
        return head;
    }

    private ExistingPullRequest existingPullRequest(Path worktree, String branchName) {
        RepairProcessRunner.ProcessResult result = processes.run(List.of(
                        "gh", "pr", "list", "--state", "all", "--head", branchName,
                        "--json", "url,isDraft,state", "--limit", "1"),
                worktree, GITHUB_TIMEOUT);
        if (!result.successful()) {
            throw new IllegalStateException("Existing pull requests could not be inspected: "
                    + concise(result.output()));
        }
        try {
            JsonNode values = JSON.readTree(result.output());
            if (!values.isArray() || values.isEmpty()) {
                return null;
            }
            JsonNode value = values.get(0);
            return new ExistingPullRequest(
                    value.path("url").asText(),
                    value.path("isDraft").asBoolean(),
                    value.path("state").asText());
        } catch (IOException exception) {
            throw new IllegalStateException("GitHub pull-request response was malformed.", exception);
        }
    }

    private Path writePullRequestBody(
            RepairProposal proposal,
            String commitSha,
            DoctorRepairPublicationRequest request) {
        StringBuilder body = new StringBuilder();
        body.append("## SHAFT Doctor repair\n\n")
                .append("Generated by SHAFT Doctor and published by an explicitly approved agent action.\n\n")
                .append("- Issue/diagnosis: ").append(issueLink(proposal.issueReference())).append('\n')
                .append("- Proposal: `").append(proposal.proposalId()).append("`\n")
                .append("- Base SHA: `").append(proposal.baseSha()).append("`\n")
                .append("- Repair commit: `").append(commitSha).append("`\n")
                .append("- Validation passed: `").append(proposal.validationPassed()).append("`\n")
                .append("- Residual risk: ").append(proposal.risk()).append("\n\n")
                .append("## Validation\n\n");
        for (RepairProposal.ValidationResult result : proposal.validationResults()) {
            body.append("- `").append(String.join(" ", result.command())).append("`: ")
                    .append(result.passed() ? "passed" : "failed")
                    .append(", exit ").append(result.exitCode())
                    .append(", Allure results ").append(result.allureResultCount()).append('\n');
            for (String diagnostic : result.diagnostics()) {
                body.append("  - ").append(diagnostic).append('\n');
            }
        }
        body.append("\n## Evidence references\n\n");
        for (RepairProposal.PatchManifestEntry patch : proposal.patches()) {
            body.append("- `").append(patch.path()).append("`: ")
                    .append(patch.evidenceIds().isEmpty()
                            ? "deterministic diagnosis"
                            : String.join(", ", patch.evidenceIds()))
                    .append('\n');
        }
        if (request.overrideFailedValidation()) {
            body.append("\nFailed-validation override: ")
                    .append(request.overrideRationale()).append('\n');
        }
        body.append("\n## Rollback\n\n").append(proposal.rollback()).append('\n');
        try {
            Path file = Files.createTempFile("shaft-doctor-pr-", ".md");
            Files.writeString(file, body.toString(), StandardCharsets.UTF_8);
            return file;
        } catch (IOException exception) {
            throw new IllegalStateException("Draft pull-request body could not be written.", exception);
        }
    }

    private Diagnosis readDiagnosis(Path path) {
        try {
            JsonNode root = JSON.readTree(Files.readString(path, StandardCharsets.UTF_8));
            JsonNode diagnosis = root.has("diagnosis") ? root.path("diagnosis") : root;
            return JSON.treeToValue(diagnosis, Diagnosis.class);
        } catch (IOException | RuntimeException exception) {
            throw new IllegalArgumentException("Doctor diagnosis could not be read.", exception);
        }
    }

    private EvidenceBundle readBundle(Path path) {
        return path == null ? null : doctorJson.readBundle(path);
    }

    private static Set<String> evidenceIds(Diagnosis diagnosis, EvidenceBundle bundle) {
        Set<String> ids = new LinkedHashSet<>();
        diagnosis.findings().forEach(finding -> ids.addAll(finding.evidenceIds()));
        if (bundle != null) {
            bundle.evidence().forEach(item -> ids.add(item.id()));
        }
        return Set.copyOf(ids);
    }

    private String git(Path directory, String... arguments) {
        List<String> command = new ArrayList<>(List.of("git"));
        command.addAll(List.of(arguments));
        RepairProcessRunner.ProcessResult result = processes.run(command, directory, GIT_TIMEOUT);
        if (!result.successful()) {
            throw new IllegalArgumentException("Git repository state could not be verified: "
                    + concise(result.output()));
        }
        return result.output();
    }

    private String reviewDiff(Path directory, String... arguments) {
        List<String> command = new ArrayList<>(List.of("diff", "--no-ext-diff", "--no-color"));
        command.addAll(List.of(arguments));
        String output = git(directory, command.toArray(String[]::new)).replace("\r\n", "\n");
        int start = output.indexOf("diff --git ");
        return start < 0 ? "" : output.substring(start);
    }

    private void runRequired(
            List<String> command,
            Path directory,
            Duration timeout,
            String failure) {
        RepairProcessRunner.ProcessResult result = processes.run(command, directory, timeout);
        if (!result.successful()) {
            throw new IllegalStateException(failure + " " + concise(result.output()));
        }
    }

    private void removeWorktree(Path repository, Path worktree) {
        if (Files.exists(worktree, LinkOption.NOFOLLOW_LINKS)) {
            processes.run(List.of("git", "worktree", "remove", "--force", worktree.toString()),
                    repository, GIT_TIMEOUT);
        }
        deleteTree(worktree);
        processes.run(List.of("git", "worktree", "prune"), repository, GIT_TIMEOUT);
    }

    private void deleteLocalBranch(Path repository, String branchName) {
        processes.run(List.of("git", "branch", "-D", branchName), repository, GIT_TIMEOUT);
    }

    private void recoverStaleWorktrees(Path repository) {
        String output = git(repository, "worktree", "list", "--porcelain");
        String path = "";
        String branch = "";
        for (String line : (output + "\n").replace("\r\n", "\n").split("\n", -1)) {
            if (line.startsWith("worktree ")) {
                path = line.substring("worktree ".length()).trim();
            } else if (line.startsWith("branch refs/heads/")) {
                branch = line.substring("branch refs/heads/".length()).trim();
            } else if (line.isBlank() && !path.isBlank()) {
                Path candidate = Path.of(path).toAbsolutePath().normalize();
                String name = candidate.getFileName() == null ? "" : candidate.getFileName().toString();
                if (branch.startsWith("codex/doctor-")
                        && name.startsWith("shaft-doctor-")
                        && isStale(candidate)) {
                    removeWorktree(repository, candidate);
                    deleteLocalBranch(repository, branch);
                }
                path = "";
                branch = "";
            }
        }
    }

    private static boolean isStale(Path path) {
        try {
            return Files.exists(path, LinkOption.NOFOLLOW_LINKS)
                    && Files.getLastModifiedTime(path).toInstant()
                    .isBefore(Instant.now().minus(STALE_WORKTREE_AGE));
        } catch (IOException exception) {
            return false;
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
                            root.relativize(path).toString(),
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

    private static Path realDirectory(Path path, String label) {
        try {
            Path real = path.toRealPath();
            if (!Files.isDirectory(real)) {
                throw new IllegalArgumentException(label + " must be a directory.");
            }
            return real;
        } catch (IOException exception) {
            throw new IllegalArgumentException(label + " cannot be resolved.", exception);
        }
    }

    private static void createDirectory(Path path, String label) {
        try {
            Files.createDirectories(path);
            if (!Files.isDirectory(path)) {
                throw new IllegalArgumentException(label + " must be a directory.");
            }
        } catch (IOException exception) {
            throw new IllegalArgumentException(label + " could not be created.", exception);
        }
    }

    private static void verifyFullSha(String sha) {
        if (sha == null || !sha.matches("(?i)[0-9a-f]{40}")) {
            throw new IllegalArgumentException("Doctor repair base SHA must be a full 40-character commit SHA.");
        }
    }

    private static String proposalId(String issueReference) {
        return slug(issueReference) + "-" + UUID.randomUUID().toString().substring(0, 8);
    }

    private static String slug(String value) {
        String normalized = value == null ? "session" : value.toLowerCase(Locale.ROOT)
                .replaceAll("[^a-z0-9]+", "-")
                .replaceAll("(^-+|-+$)", "");
        if (normalized.isBlank()) {
            return "session";
        }
        return normalized.substring(0, Math.min(32, normalized.length()));
    }

    private static String approvalToken(String proposalId, String baseSha, String diff) {
        String nonce = UUID.randomUUID().toString();
        return DoctorHashing.sha256(
                (proposalId + "\n" + baseSha + "\n" + diff + "\n" + nonce)
                        .getBytes(StandardCharsets.UTF_8));
    }

    private static boolean tokensEqual(String expected, String actual) {
        return MessageDigest.isEqual(
                (expected == null ? "" : expected).getBytes(StandardCharsets.UTF_8),
                (actual == null ? "" : actual).getBytes(StandardCharsets.UTF_8));
    }

    private static String concise(String value) {
        String normalized = value == null ? "" : value.replaceAll("\\s+", " ").trim();
        return normalized.substring(0, Math.min(2_000, normalized.length()));
    }

    private static String firstUrl(String output) {
        for (String token : output.split("\\s+")) {
            if (token.startsWith("https://") || token.startsWith("http://")) {
                return token.trim();
            }
        }
        throw new IllegalStateException("GitHub did not return a draft pull-request URL.");
    }

    private static String issueLink(String value) {
        String normalized = value == null ? "" : value.trim();
        return normalized.matches("\\d+") ? "#" + normalized : normalized;
    }

    private static void deleteIfExists(Path path) {
        try {
            Files.deleteIfExists(path);
        } catch (IOException ignored) {
            // Best-effort cleanup of a temporary pull-request body.
        }
    }

    private static Path emptyHooksDirectory() {
        try {
            return Files.createTempDirectory("shaft-doctor-hooks-").toAbsolutePath().normalize();
        } catch (IOException exception) {
            throw new IllegalStateException("Doctor Git hook isolation could not be created.", exception);
        }
    }

    private static void deleteTree(Path root) {
        if (root == null || !Files.exists(root, LinkOption.NOFOLLOW_LINKS)) {
            return;
        }
        try (Stream<Path> paths = Files.walk(root)) {
            for (Path path : paths.sorted(Comparator.reverseOrder()).toList()) {
                Files.deleteIfExists(path);
            }
        } catch (IOException ignored) {
            // Git worktree removal is authoritative; filesystem cleanup is best effort.
        }
    }

    private record ExistingPullRequest(String url, boolean draft, String state) {
    }

    private record AllureFile(String path, long size, long modified, String status) {
    }

    private record AllureSnapshot(List<AllureFile> files) {
        AllureVerdict changedSince(AllureSnapshot before) {
            Set<AllureFile> previous = new HashSet<>(before.files());
            List<AllureFile> changed = files.stream().filter(file -> !previous.contains(file)).toList();
            int failed = Math.toIntExact(changed.stream()
                    .filter(file -> !"passed".equalsIgnoreCase(file.status()))
                    .count());
            return new AllureVerdict(changed.size(), failed);
        }
    }

    private record AllureVerdict(int count, int failed) {
    }
}
