package com.shaft.mcp;

/**
 * Evidence oracle for one measurable acceptance criterion (playbook practice 9, issue #5948).
 *
 * @param acId     traced acceptance-criterion ID
 * @param oracle   observable pass/fail statement
 * @param evidence what would prove the criterion
 */
public record McpDesignOracle(String acId, String oracle, String evidence) {
    public McpDesignOracle {
        acId = acId == null ? "" : acId;
        oracle = oracle == null ? "" : oracle;
        evidence = evidence == null ? "" : evidence;
    }
}
