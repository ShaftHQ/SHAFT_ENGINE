package com.shaft.intellij.approval;

import java.util.EnumSet;
import java.util.Locale;
import java.util.Set;

/**
 * Describes how a local agent CLI can be granted permission to run tools, and which
 * {@link ApprovalScope} values it can act on <em>interactively</em> — i.e. while its process is
 * still running, by writing a decision back to its stdin in response to a structured-stream
 * approval-request event — versus scopes that can only ever be pre-approved through a launch-time
 * config flag baked into the command line before the process starts.
 *
 * <ul>
 *   <li>{@link #CLAUDE_CODE}: the {@code stream-json} output format surfaces permission-request
 *       events that a still-open stdin pipe can answer, so it supports the full interactive scope
 *       set ({@link ApprovalScope#ONCE}, {@link ApprovalScope#TOOL_ALWAYS}, {@link
 *       ApprovalScope#ALL}, {@link ApprovalScope#DENY}).
 *   <li>{@link #CODEX}: the experimental {@code exec --json} event stream does not (yet) surface
 *       approval-request events, so today only {@code codex}'s launch-time config flags (for
 *       example {@code default_tools_approval_mode}) can grant permission ahead of time. Should a
 *       future Codex CLI release add approval events to its schema, this capability should gain
 *       interactive scopes the same way Claude Code did.
 *   <li>{@link #COPILOT_CLI}: no structured approval protocol is known at all; only its own
 *       pre-approval subcommand/flag selection (e.g. {@code agent} vs {@code ask}) applies.
 * </ul>
 */
public enum AgentApprovalCapability {
    CLAUDE_CODE(true, EnumSet.of(ApprovalScope.ONCE, ApprovalScope.TOOL_ALWAYS, ApprovalScope.ALL, ApprovalScope.DENY)),
    CODEX(false, EnumSet.noneOf(ApprovalScope.class)),
    COPILOT_CLI(false, EnumSet.noneOf(ApprovalScope.class));

    private final boolean interactive;
    private final Set<ApprovalScope> interactiveScopes;

    AgentApprovalCapability(boolean interactive, Set<ApprovalScope> interactiveScopes) {
        this.interactive = interactive;
        this.interactiveScopes = interactiveScopes;
    }

    /**
     * Approval granularity a CLI might support for a single tool-call permission request.
     */
    public enum ApprovalScope {
        /** Approve just this one tool call. */
        ONCE,
        /** Approve this tool for the remainder of the run. */
        TOOL_ALWAYS,
        /** Approve every tool for the remainder of the run. */
        ALL,
        /** Deny this tool call. */
        DENY
    }

    /**
     * Returns whether this CLI can receive an approval decision while its process is still running
     * (as opposed to only supporting pre-approval flags baked into the launch command).
     */
    public boolean supportsInteractiveApproval() {
        return interactive;
    }

    /**
     * Returns whether this CLI can act on {@code scope} interactively while its process is still
     * running.
     */
    public boolean supportsInteractiveScope(ApprovalScope scope) {
        return interactiveScopes.contains(scope);
    }

    /**
     * Returns whether the launch-time command for this CLI should be built to auto-approve tool
     * calls, given whether the caller has granted source-mutation permission for this request. This
     * is the single decision point command builders consult instead of hard-coding an
     * always-on approval flag, so a future richer approval store (persisted per-tool grants, scope
     * selection, etc.) can replace this method's body without touching call sites.
     */
    public boolean isAutoApproveGranted(boolean allowSourceMutation) {
        return allowSourceMutation;
    }

    /**
     * Resolves the capability descriptor for a {@code client} value as stored in local agent
     * invocation arguments (for example {@code "CLAUDE_CODE"}, {@code "Claude Code"}, {@code
     * "codex"}). Unrecognized or blank values resolve to {@link #CODEX}, matching the default client
     * used elsewhere when none is configured.
     */
    public static AgentApprovalCapability forClient(String client) {
        return switch (normalize(client)) {
            case "CLAUDE_CODE" -> CLAUDE_CODE;
            case "COPILOT_CLI" -> COPILOT_CLI;
            default -> CODEX;
        };
    }

    private static String normalize(String value) {
        return (value == null || value.isBlank() ? "" : value.trim())
                .toUpperCase(Locale.ROOT)
                .replace('-', '_')
                .replace(' ', '_');
    }
}
