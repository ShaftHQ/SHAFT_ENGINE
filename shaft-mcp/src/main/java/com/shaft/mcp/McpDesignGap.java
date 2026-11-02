package com.shaft.mcp;

import java.util.List;

/**
 * One gap-register row from {@code design_analyze} (issue #5948).
 *
 * @param id           stable identifier such as {@code GAP-01}
 * @param kind         contradiction, omission, ambiguity, unverifiable, infeasible, question, or nfr_hint
 * @param severity     {@code blocking} or {@code warning}
 * @param rank         1 is highest priority
 * @param tracedAcIds  related acceptance-criterion IDs
 * @param summary      short description
 * @param question     ranked question to resolve the gap
 * @param practice     playbook practice number 1–10
 * @param waivable     whether residual-risk accept may clear this gap
 * @param accepted     whether this waivable gap is in {@code acceptedGapIds}
 */
public record McpDesignGap(
        String id,
        String kind,
        String severity,
        int rank,
        List<String> tracedAcIds,
        String summary,
        String question,
        int practice,
        boolean waivable,
        boolean accepted) {
    public McpDesignGap {
        tracedAcIds = tracedAcIds == null ? List.of() : List.copyOf(tracedAcIds);
        summary = summary == null ? "" : summary;
        question = question == null ? "" : question;
        kind = kind == null ? "" : kind;
        severity = severity == null ? "" : severity;
        id = id == null ? "" : id;
    }
}
