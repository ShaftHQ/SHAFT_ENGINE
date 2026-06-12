package com.shaft.doctor.repair;

/**
 * MCP-friendly result for deterministic or optional provider-assisted repair proposals.
 *
 * @param proposal isolated reviewed proposal, absent when provider generation failed safely
 * @param providerPatch optional provider patch status
 */
public record DoctorRepairProposalResult(
        RepairProposal proposal,
        DoctorRepairPatchResult providerPatch) {
}
