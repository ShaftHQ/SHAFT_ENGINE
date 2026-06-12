package com.shaft.doctor.model;

import java.util.List;

/**
 * Cited observation or inference emitted by a rule.
 *
 * @param id stable finding identifier
 * @param kind whether this is an observation or inference
 * @param category associated cause category
 * @param severity finding severity
 * @param title concise title
 * @param detail safe explanatory detail
 * @param ruleId rule that emitted the finding
 * @param evidenceIds cited evidence identifiers
 */
public record Finding(
        String id,
        Kind kind,
        CauseCategory category,
        Severity severity,
        String title,
        String detail,
        String ruleId,
        List<String> evidenceIds) {
    /**
     * Finding epistemic type.
     */
    public enum Kind {
        OBSERVATION,
        INFERENCE
    }

    /**
     * Finding severity.
     */
    public enum Severity {
        INFO,
        WARNING,
        ERROR
    }

    /**
     * Creates a validated finding.
     */
    public Finding {
        id = DoctorModelSupport.requireText(id, "Finding ID");
        kind = kind == null ? Kind.OBSERVATION : kind;
        category = category == null ? CauseCategory.UNKNOWN : category;
        severity = severity == null ? Severity.INFO : severity;
        title = DoctorModelSupport.requireText(title, "Finding title");
        detail = DoctorModelSupport.requireText(detail, "Finding detail");
        ruleId = DoctorModelSupport.requireText(ruleId, "Rule ID");
        evidenceIds = DoctorModelSupport.list(evidenceIds);
    }
}
