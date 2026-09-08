package com.shaft.ai.agentic;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;

/**
 * Explicit permissions and scoped tools for one phase (FR-2). Defaults deny mutations.
 *
 * @param allowedTools tools the phase may invoke
 * @param mayMutateApprovedTests whether approved test sources may be written
 * @param mayPerformCleanup whether cleanup of workspace artifacts is allowed
 * @param mayUseDestructiveBrowser whether destructive browser actions are allowed
 * @param mayReadCredentials whether credential material may be read
 * @param mayApplyModelAuthority whether model output may authorize test or cleanup actions (always false in bindings)
 */
public record PhasePermissions(
        Set<String> allowedTools,
        boolean mayMutateApprovedTests,
        boolean mayPerformCleanup,
        boolean mayUseDestructiveBrowser,
        boolean mayReadCredentials,
        boolean mayApplyModelAuthority) {

    /**
     * Creates an immutable permission set. Model authority is always forced off (FR-4).
     */
    public PhasePermissions {
        allowedTools = allowedTools == null || allowedTools.isEmpty()
                ? Set.of()
                : Collections.unmodifiableSet(normalize(allowedTools));
        mayApplyModelAuthority = false;
    }

    private static Set<String> normalize(Set<String> tools) {
        Set<String> normalized = new LinkedHashSet<>();
        for (String tool : tools) {
            if (tool == null || tool.isBlank()) {
                continue;
            }
            normalized.add(tool.trim().toLowerCase(Locale.ROOT));
        }
        return normalized;
    }

    /**
     * Returns whether a tool name is in the phase allowlist.
     *
     * @param toolName requested tool
     * @return {@code true} when allowed
     */
    public boolean allowsTool(String toolName) {
        if (toolName == null || toolName.isBlank()) {
            return false;
        }
        return allowedTools.contains(toolName.trim().toLowerCase(Locale.ROOT));
    }

    /**
     * Planner binding: read-only exploration tools.
     *
     * @return planner permissions
     */
    public static PhasePermissions planner() {
        return new PhasePermissions(
                Set.of("read_journey", "sanitize_page", "list_flows"),
                false, false, false, false, false);
    }

    /**
     * Generator binding: draft-only generation tools.
     *
     * @return generator permissions
     */
    public static PhasePermissions generator() {
        return new PhasePermissions(
                Set.of("draft_test", "render_diff"),
                false, false, false, false, false);
    }

    /**
     * Runner binding: allowlisted verification commands only.
     *
     * @return runner permissions
     */
    public static PhasePermissions runner() {
        return new PhasePermissions(
                Set.of("run_allowlisted_maven", "record_artifact"),
                false, false, false, false, false);
    }

    /**
     * Diagnoser binding: evidence analysis only.
     *
     * @return diagnoser permissions
     */
    public static PhasePermissions diagnoser() {
        return new PhasePermissions(
                Set.of("read_artifact", "classify_failure", "consider_model_advice"),
                false, false, false, false, false);
    }

    /**
     * Proposal binding: emit review-only proposal artifacts.
     *
     * @return proposal permissions
     */
    public static PhasePermissions proposal() {
        return new PhasePermissions(
                Set.of("write_proposal", "write_provenance"),
                false, false, false, false, false);
    }

    /**
     * Returns a copy with an explicit tool set, preserving other flags.
     *
     * @param tools replacement allowlist
     * @return new permissions
     */
    public PhasePermissions withTools(Set<String> tools) {
        return new PhasePermissions(
                tools,
                mayMutateApprovedTests,
                mayPerformCleanup,
                mayUseDestructiveBrowser,
                mayReadCredentials,
                mayApplyModelAuthority);
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof PhasePermissions that)) {
            return false;
        }
        return mayMutateApprovedTests == that.mayMutateApprovedTests
                && mayPerformCleanup == that.mayPerformCleanup
                && mayUseDestructiveBrowser == that.mayUseDestructiveBrowser
                && mayReadCredentials == that.mayReadCredentials
                && mayApplyModelAuthority == that.mayApplyModelAuthority
                && Objects.equals(allowedTools, that.allowedTools);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
                allowedTools,
                mayMutateApprovedTests,
                mayPerformCleanup,
                mayUseDestructiveBrowser,
                mayReadCredentials,
                mayApplyModelAuthority);
    }
}
