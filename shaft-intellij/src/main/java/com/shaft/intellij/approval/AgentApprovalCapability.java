package com.shaft.intellij.approval;

import java.util.Locale;

/**
 * Describes how a local agent CLI can be granted permission to run tools ahead of time, via a
 * launch-time config flag baked into the command line before the process starts (for example
 * Codex's {@code default_tools_approval_mode} or Claude Code's {@code --permission-mode}).
 */
public enum AgentApprovalCapability {
    CLAUDE_CODE,
    CODEX,
    COPILOT_CLI;

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
