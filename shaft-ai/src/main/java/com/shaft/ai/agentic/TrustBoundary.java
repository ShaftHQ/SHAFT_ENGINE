package com.shaft.ai.agentic;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

/**
 * Fail-closed trust boundary for model-driven and untrusted-content actions (SC-2, edge cases).
 */
public final class TrustBoundary {
    private static final Pattern INJECTION = Pattern.compile(
            "(?i)((ignore|disregard)\\s+(all\\s+)?(previous|prior)\\s+instructions|system\\s*:\\s*you\\s+are|</?tool>|sudo\\s+rm\\s+-rf)");
    private static final Pattern CREDENTIAL = Pattern.compile(
            "(?i)((?:api[_-]?key|password|secret)\\s*[:=]\\s*\\S+|bearer\\s+[a-z0-9._\\-]+)");
    private static final Pattern APPROVED_TEST = Pattern.compile(
            "(?i)^(?:src/test/java/|tests/).+\\.java$");

    private TrustBoundary() {
    }

    /**
     * Detects prompt-injection markers in untrusted text.
     *
     * @param text candidate text
     * @return {@code true} when injection markers are present
     */
    public static boolean looksLikePromptInjection(String text) {
        return text != null && INJECTION.matcher(text).find();
    }

    /**
     * Detects credential-like material.
     *
     * @param text candidate text
     * @return {@code true} when credentials appear present
     */
    public static boolean looksLikeCredential(String text) {
        return text != null && CREDENTIAL.matcher(text).find();
    }

    /**
     * Returns whether a path is an approved test source that must not be model-mutated.
     *
     * @param path repository-relative path
     * @return {@code true} when the path is protected
     */
    public static boolean isApprovedTestPath(String path) {
        if (path == null || path.isBlank()) {
            return false;
        }
        String normalized = path.trim().replace('\\', '/');
        return APPROVED_TEST.matcher(normalized).matches();
    }

    /**
     * Audits untrusted model advice against phase permissions. Never grants authority (FR-4).
     *
     * @param phase current phase
     * @param permissions phase permissions
     * @param advice untrusted advice
     * @return denial reasons (empty when no mutation was requested)
     */
    public static List<String> auditModelAdvice(
            AgenticPhase phase,
            PhasePermissions permissions,
            UntrustedModelAdvice advice) {
        List<String> denials = new ArrayList<>();
        if (advice == null) {
            return denials;
        }
        if (!advice.suggestedTestMutation().isBlank()) {
            denials.add(deny(phase, "model suggested mutating "
                    + advice.suggestedTestMutation()
                    + "; model output is never test authority"));
        }
        if (advice.suggestedCleanup()) {
            denials.add(deny(phase, "model suggested cleanup; model output is never cleanup authority"));
        }
        if (advice.suggestedDestructiveBrowser()) {
            denials.add(deny(phase, "destructive browser action denied"));
        }
        if (advice.suggestedCredentialRead() || permissions.mayReadCredentials()) {
            // mayReadCredentials is always false in PhasePermissions constructors for bindings,
            // but still deny any credential read attempt from advice.
            if (advice.suggestedCredentialRead()) {
                denials.add(deny(phase, "credential read denied"));
            }
        }
        // FR-4: permissions.mayApplyModelAuthority is forced false; restate if somehow true.
        if (permissions.mayApplyModelAuthority()) {
            denials.add(deny(phase, "mayApplyModelAuthority must remain false"));
        }
        return List.copyOf(denials);
    }

    /**
     * Sanitizes untrusted page content for planner use: redacts credentials and flags injection.
     *
     * @param pageContent raw page content
     * @return sanitization result
     */
    public static SanitizedContent sanitizePage(String pageContent) {
        String raw = pageContent == null ? "" : pageContent;
        boolean injection = looksLikePromptInjection(raw);
        boolean credentials = looksLikeCredential(raw);
        String sanitized = CREDENTIAL.matcher(raw).replaceAll("[REDACTED]");
        if (injection) {
            sanitized = "[UNTRUSTED_PAGE_QUARANTINED]\n" + sanitized;
        }
        return new SanitizedContent(sanitized, injection, credentials);
    }

    /**
     * Returns whether a tool name is known to the workflow catalog.
     *
     * @param toolName tool name
     * @return {@code true} when known
     */
    public static boolean isKnownTool(String toolName) {
        if (toolName == null || toolName.isBlank()) {
            return false;
        }
        String normalized = toolName.trim().toLowerCase(Locale.ROOT);
        return switch (normalized) {
            case "read_journey",
                    "sanitize_page",
                    "list_flows",
                    "draft_test",
                    "render_diff",
                    "run_allowlisted_maven",
                    "record_artifact",
                    "read_artifact",
                    "classify_failure",
                    "consider_model_advice",
                    "write_proposal",
                    "write_provenance" -> true;
            default -> false;
        };
    }

    private static String deny(AgenticPhase phase, String reason) {
        return phase.name() + ": " + reason;
    }

    /**
     * Result of sanitizing untrusted page content.
     *
     * @param text sanitized text
     * @param promptInjectionDetected whether injection markers were found
     * @param credentialsDetected whether credential-like material was found
     */
    public record SanitizedContent(String text, boolean promptInjectionDetected, boolean credentialsDetected) {
    }
}
