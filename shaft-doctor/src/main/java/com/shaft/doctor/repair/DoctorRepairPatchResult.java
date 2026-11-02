package com.shaft.doctor.repair;

import com.shaft.pilot.ai.AiResponseStatus;

import java.util.List;

/**
 * Optional provider patch result that never changes repository state.
 *
 * @param status normalized provider status
 * @param provider provider identifier
 * @param model model identifier
 * @param patches validated structured patches
 * @param fallbackReason safe failure reason
 */
public record DoctorRepairPatchResult(
        AiResponseStatus status,
        String provider,
        String model,
        List<DoctorRepairRequest.FilePatch> patches,
        String fallbackReason) {
    /**
     * Creates an immutable provider patch result.
     */
    public DoctorRepairPatchResult {
        patches = patches == null ? List.of() : List.copyOf(patches);
        provider = provider == null ? "" : provider;
        model = model == null ? "" : model;
        fallbackReason = fallbackReason == null ? "" : fallbackReason;
    }

    /**
     * Returns whether a provider produced at least one accepted patch.
     *
     * @return true for a successful non-empty structured patch set
     */
    public boolean successful() {
        return status == AiResponseStatus.SUCCESS && !patches.isEmpty();
    }
}
