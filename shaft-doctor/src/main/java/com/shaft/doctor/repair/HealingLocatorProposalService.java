package com.shaft.doctor.repair;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.doctor.internal.DoctorHashing;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.UUID;

/**
 * Creates reviewable locator patch proposals from verified SHAFT Heal reports.
 *
 * <p>This service never edits source. Its structured patch is intended for a
 * separate explicitly invoked {@link DoctorRepairService} proposal.</p>
 */
public final class HealingLocatorProposalService {
    private static final ObjectMapper JSON = new ObjectMapper();

    /**
     * Creates and persists a proposal-only artifact.
     *
     * @param request explicit proposal request
     * @return reviewable proposal and structured Doctor patch
     */
    public HealingLocatorProposal propose(HealingLocatorProposalRequest request) {
        if (!SHAFT.Properties.healing.sourcePatchEnabled()) {
            throw new IllegalArgumentException(
                    "Locator source-patch proposals are disabled by healing.sourcePatch.enabled.");
        }
        if (request == null || !request.sourcePatchConsent()) {
            throw new IllegalArgumentException("Explicit locator source-patch proposal consent is required.");
        }
        Path repository = realDirectory(request.repositoryRoot(), "Repository root");
        Path source = repository.resolve(request.sourcePath()).normalize();
        if (!source.startsWith(repository) || !Files.isRegularFile(source)
                || !source.getFileName().toString().endsWith(".java")) {
            throw new IllegalArgumentException("Locator proposals require one approved Java source file.");
        }
        JsonNode report = read(request.healingReportPath());
        validateReport(report);
        String originalLocator = report.path("originalLocator").asText();
        JsonNode selected = selectedCandidate(report);
        String proposedLocator = selected.path("proposedLocator").asText();
        String originalExpression = javaExpression(originalLocator);
        String proposedExpression = javaExpression(proposedLocator);
        String content = readText(source);
        int first = content.indexOf(originalExpression);
        int last = content.lastIndexOf(originalExpression);
        if (first < 0 || first != last) {
            throw new IllegalArgumentException(
                    "The original locator must map to exactly one supported Java expression.");
        }
        String proposedContent = content.substring(0, first)
                + proposedExpression
                + content.substring(first + originalExpression.length());
        int line = 1 + Math.toIntExact(content.substring(0, first).chars()
                .filter(value -> value == '\n').count());
        String sourceSha = sha256(content);
        String proposedSha = sha256(proposedContent);
        String proposalId = "heal-" + report.path("attemptId").asText()
                + "-" + UUID.randomUUID().toString().substring(0, 8);
        List<String> evidence = new ArrayList<>();
        selected.path("evidence").forEach(item -> evidence.add(item.asText()));
        DoctorRepairRequest.FilePatch patch = new DoctorRepairRequest.FilePatch(
                request.sourcePath(),
                DoctorRepairRequest.FilePatch.Operation.REPLACE,
                proposedContent,
                "Replace a uniquely mapped locator using verified SHAFT Heal attempt "
                        + report.path("attemptId").asText() + ".",
                List.of());
        String token = DoctorHashing.sha256(
                (proposalId + "\n" + sourceSha + "\n" + proposedSha)
                        .getBytes(StandardCharsets.UTF_8));
        Path output = request.outputDirectory().toAbsolutePath().normalize();
        createDirectory(output);
        Path manifest = output.resolve("healing-locator-proposal-" + proposalId + ".json");
        HealingLocatorProposal proposal = new HealingLocatorProposal(
                HealingLocatorProposal.CURRENT_SCHEMA_VERSION,
                proposalId,
                report.path("attemptId").asText(),
                request.sourcePath(),
                line,
                originalExpression,
                proposedExpression,
                report.path("decision").path("confidence").asDouble(),
                List.copyOf(evidence),
                sourceSha,
                proposedSha,
                patch,
                token,
                manifest.toString());
        write(manifest, proposal);
        return proposal;
    }

    /**
     * Creates and persists an advisory-only, unconfirmed review proposal from a low-trust
     * SHAFT Heal ladder outcome (issue #4194).
     *
     * <p>Unlike {@link #propose(HealingLocatorProposalRequest)}, this never requires a confirmed
     * {@code RECOVERED} decision or a passed action outcome -- a {@code BELOW_THRESHOLD} decision
     * is exactly the "low trust" signal this exists for. The resulting patch only adds a review
     * comment near the original locator; it never replaces or otherwise edits the locator itself,
     * matching issue #2885's closed acceptance criteria that runtime healing never silently
     * modifies source.</p>
     *
     * @param request explicit proposal request
     * @return reviewable advisory proposal and structured Doctor patch
     */
    public HealingLocatorProposal proposeAdvisory(HealingLocatorProposalRequest request) {
        if (!SHAFT.Properties.healing.sourcePatchEnabled()) {
            throw new IllegalArgumentException(
                    "Locator source-patch proposals are disabled by healing.sourcePatch.enabled.");
        }
        if (request == null || !request.sourcePatchConsent()) {
            throw new IllegalArgumentException("Explicit locator source-patch proposal consent is required.");
        }
        Path repository = realDirectory(request.repositoryRoot(), "Repository root");
        Path source = repository.resolve(request.sourcePath()).normalize();
        if (!source.startsWith(repository) || !Files.isRegularFile(source)
                || !source.getFileName().toString().endsWith(".java")) {
            throw new IllegalArgumentException("Locator proposals require one approved Java source file.");
        }
        JsonNode report = read(request.healingReportPath());
        JsonNode topCandidate = validateAdvisoryReport(report);
        String originalLocator = report.path("originalLocator").asText();
        String proposedLocator = topCandidate.path("proposedLocator").asText();
        String originalExpression = javaExpression(originalLocator);
        String proposedExpression = javaExpression(proposedLocator);
        String content = readText(source);
        int first = content.indexOf(originalExpression);
        int last = content.lastIndexOf(originalExpression);
        if (first < 0 || first != last) {
            throw new IllegalArgumentException(
                    "The original locator must map to exactly one supported Java expression.");
        }
        int lineStart = content.lastIndexOf('\n', first) + 1;
        String indent = content.substring(lineStart, first).replaceAll("\\S.*", "");
        double confidence = report.path("decision").path("confidence").asDouble();
        String comment = indent + "// SHAFT Doctor advisory (unconfirmed): a low-confidence auto-heal"
                + " candidate suggests " + proposedExpression + " (confidence "
                + String.format(Locale.ROOT, "%.2f", confidence)
                + ") -- below the trust threshold; verify before adopting.\n";
        String proposedContent = content.substring(0, lineStart) + comment + content.substring(lineStart);
        int line = 1 + Math.toIntExact(content.substring(0, first).chars()
                .filter(value -> value == '\n').count());
        String sourceSha = sha256(content);
        String proposedSha = sha256(proposedContent);
        String proposalId = "heal-advisory-" + report.path("attemptId").asText()
                + "-" + UUID.randomUUID().toString().substring(0, 8);
        List<String> evidence = new ArrayList<>();
        topCandidate.path("evidence").forEach(item -> evidence.add(item.asText()));
        DoctorRepairRequest.FilePatch patch = new DoctorRepairRequest.FilePatch(
                request.sourcePath(),
                DoctorRepairRequest.FilePatch.Operation.REPLACE,
                proposedContent,
                "ADVISORY (unconfirmed): add a review comment for a low-confidence auto-heal candidate"
                        + " from SHAFT Heal attempt " + report.path("attemptId").asText()
                        + "; verify before adopting -- this never replaces the existing locator.",
                List.of());
        String token = DoctorHashing.sha256(
                (proposalId + "\n" + sourceSha + "\n" + proposedSha)
                        .getBytes(StandardCharsets.UTF_8));
        Path output = request.outputDirectory().toAbsolutePath().normalize();
        createDirectory(output);
        Path manifest = output.resolve("healing-locator-advisory-" + proposalId + ".json");
        HealingLocatorProposal proposal = new HealingLocatorProposal(
                HealingLocatorProposal.CURRENT_SCHEMA_VERSION,
                proposalId,
                report.path("attemptId").asText(),
                request.sourcePath(),
                line,
                originalExpression,
                proposedExpression,
                confidence,
                List.copyOf(evidence),
                sourceSha,
                proposedSha,
                patch,
                token,
                manifest.toString());
        write(manifest, proposal);
        return proposal;
    }

    /**
     * Verifies that the mapped source has not changed since proposal creation.
     *
     * @param repositoryRoot approved repository root
     * @param proposal persisted proposal
     * @return {@code true} only when the source checksum still matches
     */
    public boolean isSourceCurrent(Path repositoryRoot, HealingLocatorProposal proposal) {
        if (repositoryRoot == null || proposal == null) {
            return false;
        }
        Path repository = repositoryRoot.toAbsolutePath().normalize();
        Path source = repository.resolve(proposal.sourcePath()).normalize();
        return source.startsWith(repository)
                && Files.isRegularFile(source)
                && MessageDigest.isEqual(
                proposal.sourceSha256().getBytes(StandardCharsets.UTF_8),
                sha256(readText(source)).getBytes(StandardCharsets.UTF_8));
    }

    private static void validateReport(JsonNode report) {
        String attemptId = report.path("attemptId").asText();
        if (!"2.0".equals(report.path("schemaVersion").asText())
                || !attemptId.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,127}")) {
            throw new IllegalArgumentException("A SHAFT Heal 2.0 report is required.");
        }
        if (!"RECOVERED".equals(report.path("decision").path("status").asText())
                || report.path("decision").path("selectedCandidateId").asText().isBlank()) {
            throw new IllegalArgumentException("Only an unambiguous recovered decision can propose a locator.");
        }
        if (!"PASSED".equals(report.path("action").path("outcome").asText())) {
            throw new IllegalArgumentException("The recovered action must pass before source mapping.");
        }
        String verification = report.path("action").path("postActionVerification").asText();
        if ("FAILED".equals(verification)
                || "ELEMENT_NOT_INTERACTABLE".equals(verification)
                || "ELEMENT_STALE".equals(verification)) {
            throw new IllegalArgumentException("Failed post-action verification blocks source mapping.");
        }
    }

    /**
     * Validates a low-trust report for an advisory-only proposal and selects the highest-scoring
     * candidate (issue #4194). Unlike {@link #validateReport(JsonNode)}, this deliberately accepts
     * a {@code BELOW_THRESHOLD} decision with no selected candidate and no passed action outcome --
     * that absence of confirmation is exactly the condition this proposal type exists for.
     */
    private static JsonNode validateAdvisoryReport(JsonNode report) {
        String attemptId = report.path("attemptId").asText();
        if (!"2.0".equals(report.path("schemaVersion").asText())
                || !attemptId.matches("[A-Za-z0-9][A-Za-z0-9._-]{0,127}")) {
            throw new IllegalArgumentException("A SHAFT Heal 2.0 report is required.");
        }
        if (!"BELOW_THRESHOLD".equals(report.path("decision").path("status").asText())) {
            throw new IllegalArgumentException(
                    "Only a below-threshold decision can propose an advisory review.");
        }
        JsonNode candidates = report.path("candidates");
        if (!candidates.isArray() || candidates.isEmpty()) {
            throw new IllegalArgumentException("An advisory proposal requires at least one scored candidate.");
        }
        // The report's candidates are already ranked highest score first (ShaftHealingProvider),
        // but that ranking alone doesn't re-derive HealingDecisionEngine's own eligibility gate --
        // the persisted decision.confidence came from that engine's pre-filtered `eligible` list,
        // so the advisory must approximate it before picking a candidate (issue #4209). This
        // requires `interactable` unconditionally, even though the engine only applies it when
        // visibilityRequired=true (the common case for real callers): the report has no field
        // recording whether visibilityRequired applied to this attempt, so the advisory can't
        // perfectly reconstruct the engine's exact gate. When the original attempt had
        // visibilityRequired=false, this makes the advisory slightly more conservative than the
        // engine -- a missed suggestion rather than a mismatched-confidence one, the safe
        // direction to be wrong in.
        for (JsonNode candidate : candidates) {
            if (candidate.path("unique").asBoolean()
                    && candidate.path("contextMatched").asBoolean()
                    && candidate.path("interactable").asBoolean()) {
                return candidate;
            }
        }
        throw new IllegalArgumentException(
                "No candidate passed uniqueness/context-match/interactability preconditions"
                        + " for an advisory proposal.");
    }

    private static JsonNode selectedCandidate(JsonNode report) {
        String selectedId = report.path("decision").path("selectedCandidateId").asText();
        JsonNode selected = null;
        int matches = 0;
        for (JsonNode candidate : report.path("candidates")) {
            if (selectedId.equals(candidate.path("candidateId").asText())) {
                selected = candidate;
                matches++;
            }
        }
        if (matches == 1
                && selected.path("unique").asBoolean()
                && selected.path("contextMatched").asBoolean()) {
            return selected;
        }
        throw new IllegalArgumentException("The selected candidate is missing or failed preconditions.");
    }

    private static String javaExpression(String locator) {
        String normalized = locator == null ? "" : locator.trim();
        int separator = normalized.indexOf(": ");
        if (separator < 0) {
            throw new IllegalArgumentException("Unsupported locator representation.");
        }
        String strategy = normalized.substring(0, separator);
        String value = normalized.substring(separator + 2);
        String method = switch (strategy.toLowerCase(Locale.ROOT)) {
            case "by.id" -> "By.id";
            case "by.name" -> "By.name";
            case "by.xpath" -> "By.xpath";
            case "by.cssselector" -> "By.cssSelector";
            case "by.classname" -> "By.className";
            case "by.tagname" -> "By.tagName";
            case "appiumby.accessibilityid" -> "AppiumBy.accessibilityId";
            case "appiumby.androiduiautomator" -> "AppiumBy.androidUIAutomator";
            case "appiumby.iosclasschain" -> "AppiumBy.iOSClassChain";
            case "appiumby.iosnspredicate" -> "AppiumBy.iOSNsPredicateString";
            default -> throw new IllegalArgumentException("Unsupported locator strategy: " + strategy);
        };
        return method + "(\"" + escapeJava(value) + "\")";
    }

    private static String escapeJava(String value) {
        return value.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\r", "\\r")
                .replace("\n", "\\n");
    }

    private static JsonNode read(Path path) {
        try {
            return JSON.readTree(Files.readString(path, StandardCharsets.UTF_8));
        } catch (IOException exception) {
            throw new IllegalArgumentException("SHAFT Heal report could not be read.", exception);
        }
    }

    private static String readText(Path path) {
        try {
            return Files.readString(path, StandardCharsets.UTF_8);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Locator source could not be read.", exception);
        }
    }

    private static void write(Path path, HealingLocatorProposal proposal) {
        try {
            Files.writeString(path,
                    JSON.writerWithDefaultPrettyPrinter().writeValueAsString(proposal) + "\n",
                    StandardCharsets.UTF_8);
        } catch (IOException exception) {
            throw new IllegalStateException("Locator proposal could not be persisted.", exception);
        }
    }

    private static Path realDirectory(Path path, String label) {
        try {
            Path resolved = path.toRealPath();
            if (!Files.isDirectory(resolved)) {
                throw new IllegalArgumentException(label + " must be a directory.");
            }
            return resolved;
        } catch (IOException exception) {
            throw new IllegalArgumentException(label + " cannot be resolved.", exception);
        }
    }

    private static void createDirectory(Path path) {
        try {
            Files.createDirectories(path);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Locator proposal output could not be created.", exception);
        }
    }

    private static String sha256(String value) {
        return DoctorHashing.sha256(value.getBytes(StandardCharsets.UTF_8));
    }
}
