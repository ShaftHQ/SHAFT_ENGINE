package com.shaft.capture.guardrail;

import java.util.List;

/**
 * Result of checking generated or agent-authored code against SHAFT guardrails.
 *
 * @param passed whether no blocking (ERROR severity) findings were detected
 * @param violations blocking and advisory findings
 */
public record GuardrailCheckResult(boolean passed, List<GuardrailViolation> violations) {
    /**
     * Creates an immutable guardrail check result.
     */
    public GuardrailCheckResult {
        violations = violations == null ? List.of() : List.copyOf(violations);
    }
}
