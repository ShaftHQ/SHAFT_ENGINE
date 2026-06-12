package com.shaft.doctor.model;

import java.util.List;

/**
 * Deterministic next action linked to findings and evidence.
 *
 * @param id stable remediation identifier
 * @param title concise title
 * @param action actionable guidance
 * @param findingIds findings that justify this action
 * @param evidenceIds evidence supporting this action
 */
public record Remediation(
        String id,
        String title,
        String action,
        List<String> findingIds,
        List<String> evidenceIds) {
    /**
     * Creates a validated remediation.
     */
    public Remediation {
        id = DoctorModelSupport.requireText(id, "Remediation ID");
        title = DoctorModelSupport.requireText(title, "Remediation title");
        action = DoctorModelSupport.requireText(action, "Remediation action");
        findingIds = DoctorModelSupport.list(findingIds);
        evidenceIds = DoctorModelSupport.list(evidenceIds);
    }
}
