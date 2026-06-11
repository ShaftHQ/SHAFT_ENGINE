package com.shaft.pilot.ai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NullNode;

import java.time.Duration;
import java.util.List;
import java.util.Objects;

/**
 * Provider-neutral AI result with deterministic fallback.
 *
 * @param schemaVersion response contract version
 * @param status normalized outcome
 * @param provider provider identifier
 * @param model model or configuration identifier
 * @param structuredPayload validated provider payload
 * @param warnings safe warnings
 * @param duration request duration
 * @param usage provider-reported usage
 * @param fallbackReason safe fallback reason
 * @param deterministicFallback caller-owned deterministic result
 */
public record AiResponse(
        String schemaVersion,
        AiResponseStatus status,
        String provider,
        String model,
        JsonNode structuredPayload,
        List<String> warnings,
        Duration duration,
        AiUsage usage,
        String fallbackReason,
        JsonNode deterministicFallback) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable response.
     */
    public AiResponse {
        schemaVersion = Objects.requireNonNullElse(schemaVersion, CURRENT_SCHEMA_VERSION);
        status = Objects.requireNonNull(status, "status");
        provider = Objects.requireNonNullElse(provider, "none");
        model = Objects.requireNonNullElse(model, "");
        structuredPayload = structuredPayload == null ? NullNode.getInstance() : structuredPayload.deepCopy();
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
        duration = duration == null ? Duration.ZERO : duration;
        usage = usage == null ? AiUsage.empty() : usage;
        fallbackReason = Objects.requireNonNullElse(fallbackReason, "");
        deterministicFallback = deterministicFallback == null
                ? NullNode.getInstance()
                : deterministicFallback.deepCopy();
    }

    /**
     * Creates a successful response.
     *
     * @param provider provider identifier
     * @param model model identifier
     * @param payload structured payload
     * @param duration request duration
     * @param usage usage metadata
     * @param fallback deterministic fallback retained for caller reference
     * @return successful response
     */
    public static AiResponse success(String provider, String model, JsonNode payload, Duration duration,
                                     AiUsage usage, JsonNode fallback) {
        return new AiResponse(CURRENT_SCHEMA_VERSION, AiResponseStatus.SUCCESS, provider, model, payload,
                List.of(), duration, usage, "", fallback);
    }

    /**
     * Creates a safe failure response that returns the deterministic result.
     *
     * @param status normalized failure
     * @param provider provider identifier
     * @param model model identifier
     * @param reason safe reason
     * @param duration elapsed duration
     * @param fallback deterministic result
     * @return failure response
     */
    public static AiResponse failure(AiResponseStatus status, String provider, String model, String reason,
                                     Duration duration, JsonNode fallback) {
        return new AiResponse(CURRENT_SCHEMA_VERSION, status, provider, model, fallback, List.of(),
                duration, AiUsage.empty(), reason, fallback);
    }

    /**
     * Returns whether provider output was accepted.
     *
     * @return {@code true} only for successful output
     */
    public boolean successful() {
        return status == AiResponseStatus.SUCCESS;
    }
}
