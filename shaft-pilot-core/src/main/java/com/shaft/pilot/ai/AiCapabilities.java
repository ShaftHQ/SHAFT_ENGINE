package com.shaft.pilot.ai;

/**
 * Provider capabilities that callers can inspect before submitting evidence.
 *
 * @param structuredOutput whether JSON-schema-constrained output is supported
 * @param vision whether image input is supported
 * @param toolCalling whether tool calling is supported
 * @param contextLimitTokens advertised context limit, or zero when unknown
 * @param processingLocation local, remote, or disabled processing
 */
public record AiCapabilities(
        boolean structuredOutput,
        boolean vision,
        boolean toolCalling,
        long contextLimitTokens,
        ProcessingLocation processingLocation) {
}
