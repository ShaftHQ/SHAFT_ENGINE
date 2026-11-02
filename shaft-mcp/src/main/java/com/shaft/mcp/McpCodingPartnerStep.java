package com.shaft.mcp;

/**
 * Single deterministic step in a preview-only coding partner plan.
 *
 * @param index one-based step index
 * @param instruction normalized user instruction
 * @param reuseHint existing source/anchor to prefer before creating code
 * @param proofTool MCP tool that can prove the step or its locator before code is accepted
 */
public record McpCodingPartnerStep(
        int index,
        String instruction,
        String reuseHint,
        String proofTool) {
    /**
     * Creates an immutable coding partner step.
     */
    public McpCodingPartnerStep {
        index = Math.max(index, 1);
        instruction = instruction == null ? "" : instruction.trim();
        reuseHint = reuseHint == null ? "" : reuseHint.trim();
        proofTool = proofTool == null ? "" : proofTool.trim();
    }
}
