package com.shaft.doctor.shard;

/**
 * One shard's doctor {@code ExecutionIntelligence} digest, reduced to the fields useful for
 * cross-shard doctor-triage aggregation in the merged speedboard -- SHAFT's differentiator over
 * Playwright's plain {@code merge-reports}, which carries no failure intelligence.
 *
 * @param shardId shard identifier (blob directory name)
 * @param primaryCause deterministic primary cause name
 * @param confidence deterministic confidence name
 * @param failingAttempts failed or broken attempt count reported by the shard's doctor run
 * @param hiddenRetryFailures retry-hidden failure count reported by the shard's doctor run
 * @param recurringFailures recurring failure count reported by the shard's doctor run
 * @param summary concise per-shard doctor summary
 */
public record ShardIntelligence(
        String shardId,
        String primaryCause,
        String confidence,
        int failingAttempts,
        int hiddenRetryFailures,
        int recurringFailures,
        String summary) {
    public ShardIntelligence {
        shardId = shardId == null ? "" : shardId;
        primaryCause = primaryCause == null ? "UNKNOWN" : primaryCause;
        confidence = confidence == null ? "UNKNOWN" : confidence;
        failingAttempts = Math.max(0, failingAttempts);
        hiddenRetryFailures = Math.max(0, hiddenRetryFailures);
        recurringFailures = Math.max(0, recurringFailures);
        summary = summary == null ? "" : summary;
    }
}
