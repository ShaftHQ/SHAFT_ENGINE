package com.shaft.pilot.ai;

import java.util.Collections;
import java.util.EnumSet;
import java.util.Set;

/**
 * Explicit consent for local or remote inference and evidence categories.
 *
 * @param localInferenceAllowed whether local endpoints may receive evidence
 * @param onPremInferenceAllowed whether explicitly classified on-prem endpoints may receive evidence
 * @param remoteInferenceAllowed whether remote endpoints may receive evidence
 * @param allowedEvidenceCategories categories approved for this operation
 */
public record ApprovalPolicy(
        boolean localInferenceAllowed,
        boolean onPremInferenceAllowed,
        boolean remoteInferenceAllowed,
        Set<EvidenceCategory> allowedEvidenceCategories) {
    /**
     * Creates a backward-compatible local/remote policy.
     */
    public ApprovalPolicy(
            boolean localInferenceAllowed,
            boolean remoteInferenceAllowed,
            Set<EvidenceCategory> allowedEvidenceCategories) {
        this(localInferenceAllowed, false, remoteInferenceAllowed, allowedEvidenceCategories);
    }

    /**
     * Creates an immutable approval policy.
     */
    public ApprovalPolicy {
        allowedEvidenceCategories = allowedEvidenceCategories == null || allowedEvidenceCategories.isEmpty()
                ? Collections.emptySet()
                : Collections.unmodifiableSet(EnumSet.copyOf(allowedEvidenceCategories));
    }

    /**
     * Returns whether a provider location and all requested evidence categories are approved.
     *
     * @param location provider processing location
     * @param requestedCategories evidence categories in the request
     * @return {@code true} when the operation is explicitly approved
     */
    public boolean allows(ProcessingLocation location, Set<EvidenceCategory> requestedCategories) {
        boolean locationAllowed = switch (location) {
            case LOCAL -> localInferenceAllowed;
            case ON_PREM -> onPremInferenceAllowed;
            case REMOTE -> remoteInferenceAllowed;
            case NONE -> false;
        };
        return locationAllowed && allowedEvidenceCategories.containsAll(requestedCategories);
    }

    /**
     * Returns a policy that denies all inference.
     *
     * @return deny-all policy
     */
    public static ApprovalPolicy denyAll() {
        return new ApprovalPolicy(false, false, false, Collections.emptySet());
    }
}
