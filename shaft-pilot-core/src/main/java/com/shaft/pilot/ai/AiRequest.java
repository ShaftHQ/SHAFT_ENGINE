package com.shaft.pilot.ai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.NullNode;

import java.math.BigDecimal;
import java.time.Duration;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

/**
 * Immutable provider-neutral request.
 *
 * @param requestId safe request correlation identifier
 * @param purpose purpose-bound operation name
 * @param schemaVersion request contract version
 * @param text primary text input
 * @param evidence additional textual evidence
 * @param images approved image evidence
 * @param desiredResponseSchema desired JSON response schema
 * @param timeout request timeout
 * @param budget request budget
 * @param approvalPolicy explicit processing and evidence approval
 * @param deterministicFallback caller-owned deterministic result
 */
public record AiRequest(
        String requestId,
        String purpose,
        String schemaVersion,
        String text,
        List<EvidenceReference> evidence,
        List<AiImage> images,
        JsonNode desiredResponseSchema,
        Duration timeout,
        AiBudget budget,
        ApprovalPolicy approvalPolicy,
        JsonNode deterministicFallback) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates and defensively copies a request.
     */
    public AiRequest {
        requestId = requestId == null || requestId.isBlank() ? UUID.randomUUID().toString() : requestId;
        purpose = Objects.requireNonNullElse(purpose, "").trim();
        if (purpose.isEmpty()) {
            throw new IllegalArgumentException("AI request purpose is required.");
        }
        schemaVersion = Objects.requireNonNullElse(schemaVersion, CURRENT_SCHEMA_VERSION);
        text = Objects.requireNonNullElse(text, "");
        evidence = evidence == null ? List.of() : List.copyOf(evidence);
        images = images == null ? List.of() : List.copyOf(images);
        desiredResponseSchema = desiredResponseSchema == null
                ? JsonNodeFactory.instance.objectNode()
                : desiredResponseSchema.deepCopy();
        timeout = timeout == null ? Duration.ofSeconds(30) : timeout;
        if (timeout.isZero() || timeout.isNegative()) {
            throw new IllegalArgumentException("AI request timeout must be positive.");
        }
        budget = budget == null ? new AiBudget(0, 0, BigDecimal.ZERO) : budget;
        approvalPolicy = approvalPolicy == null ? ApprovalPolicy.denyAll() : approvalPolicy;
        deterministicFallback = deterministicFallback == null
                ? NullNode.getInstance()
                : deterministicFallback.deepCopy();
    }

    /**
     * Starts a request builder.
     *
     * @param purpose purpose-bound operation name
     * @param responseSchema desired JSON response schema
     * @return request builder
     */
    public static Builder builder(String purpose, JsonNode responseSchema) {
        return new Builder(purpose, responseSchema);
    }

    /**
     * Returns all evidence categories present in this request.
     *
     * @return immutable category set
     */
    public Set<EvidenceCategory> evidenceCategories() {
        EnumSet<EvidenceCategory> categories = EnumSet.noneOf(EvidenceCategory.class);
        if (!text.isBlank()) {
            categories.add(EvidenceCategory.TEXT);
        }
        evidence.forEach(item -> categories.add(item.category()));
        images.forEach(item -> categories.add(item.category()));
        return Set.copyOf(categories);
    }

    /**
     * Creates a copy containing sanitized text and evidence.
     *
     * @param sanitizedText redacted primary text
     * @param sanitizedEvidence redacted evidence
     * @return sanitized request copy
     */
    public AiRequest withSanitizedContent(String sanitizedText, List<EvidenceReference> sanitizedEvidence) {
        return new AiRequest(requestId, purpose, schemaVersion, sanitizedText, sanitizedEvidence, images,
                desiredResponseSchema, timeout, budget, approvalPolicy, deterministicFallback);
    }

    /**
     * Creates a copy with an effective timeout.
     *
     * @param effectiveTimeout timeout already constrained by global policy
     * @return request copy
     */
    public AiRequest withTimeout(Duration effectiveTimeout) {
        return new AiRequest(requestId, purpose, schemaVersion, text, evidence, images,
                desiredResponseSchema, effectiveTimeout, budget, approvalPolicy, deterministicFallback);
    }

    /**
     * Fluent builder for {@link AiRequest}.
     */
    public static final class Builder {
        private String requestId;
        private final String purpose;
        private final JsonNode responseSchema;
        private String text = "";
        private final List<EvidenceReference> evidence = new ArrayList<>();
        private final List<AiImage> images = new ArrayList<>();
        private Duration timeout = Duration.ofSeconds(30);
        private AiBudget budget = new AiBudget(0, 0, BigDecimal.ZERO);
        private ApprovalPolicy approvalPolicy = ApprovalPolicy.denyAll();
        private JsonNode deterministicFallback = NullNode.getInstance();

        private Builder(String purpose, JsonNode responseSchema) {
            this.purpose = purpose;
            this.responseSchema = responseSchema;
        }

        /**
         * Sets a safe request correlation identifier.
         *
         * @param value identifier
         * @return this builder
         */
        public Builder requestId(String value) {
            requestId = value;
            return this;
        }

        /**
         * Sets primary text input.
         *
         * @param value text input
         * @return this builder
         */
        public Builder text(String value) {
            text = value;
            return this;
        }

        /**
         * Adds textual evidence.
         *
         * @param value evidence
         * @return this builder
         */
        public Builder evidence(EvidenceReference value) {
            evidence.add(Objects.requireNonNull(value, "value"));
            return this;
        }

        /**
         * Adds image evidence.
         *
         * @param value image
         * @return this builder
         */
        public Builder image(AiImage value) {
            images.add(Objects.requireNonNull(value, "value"));
            return this;
        }

        /**
         * Sets the request timeout.
         *
         * @param value timeout
         * @return this builder
         */
        public Builder timeout(Duration value) {
            timeout = value;
            return this;
        }

        /**
         * Sets per-request budgets.
         *
         * @param value budget
         * @return this builder
         */
        public Builder budget(AiBudget value) {
            budget = value;
            return this;
        }

        /**
         * Sets explicit request approval.
         *
         * @param value approval policy
         * @return this builder
         */
        public Builder approvalPolicy(ApprovalPolicy value) {
            approvalPolicy = value;
            return this;
        }

        /**
         * Sets the deterministic result returned on failure.
         *
         * @param value fallback payload
         * @return this builder
         */
        public Builder deterministicFallback(JsonNode value) {
            deterministicFallback = value;
            return this;
        }

        /**
         * Builds the immutable request.
         *
         * @return request
         */
        public AiRequest build() {
            return new AiRequest(requestId, purpose, CURRENT_SCHEMA_VERSION, text, evidence, images,
                    responseSchema, timeout, budget, approvalPolicy, deterministicFallback);
        }
    }
}
