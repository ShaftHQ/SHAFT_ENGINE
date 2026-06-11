package com.shaft.capture.generate;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.pilot.ai.AiBudget;
import com.shaft.pilot.ai.AiExecutionService;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.AiResponse;
import com.shaft.pilot.ai.ApprovalPolicy;

import java.math.BigDecimal;
import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Pattern;

/**
 * Produces schema-validated AI naming and assertion previews from sanitized Capture metadata.
 */
public final class CaptureEnrichmentService {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final Pattern JAVA_IDENTIFIER = Pattern.compile("[A-Za-z_$][A-Za-z0-9_$]*");
    private final Function<AiRequest, AiResponse> executor;

    /**
     * Creates a service using the configured SHAFT Pilot provider.
     */
    public CaptureEnrichmentService() {
        this(new AiExecutionService()::execute);
    }

    /**
     * Creates a service with an injectable provider execution boundary.
     *
     * @param executor provider request executor
     */
    public CaptureEnrichmentService(Function<AiRequest, AiResponse> executor) {
        this.executor = Objects.requireNonNull(executor, "executor");
    }

    /**
     * Requests a review-only enrichment proposal.
     *
     * @param session sanitized Capture session
     * @param fingerprint deterministic source fingerprint
     * @param deterministicClassName current class name
     * @param deterministicMethodName current method name
     * @param deterministicElementNames current field names
     * @param approvalPolicy explicit provider approval
     * @return reviewable preview
     */
    public CaptureEnrichmentPreview preview(
            CaptureSession session,
            String fingerprint,
            String deterministicClassName,
            String deterministicMethodName,
            Map<String, String> deterministicElementNames,
            ApprovalPolicy approvalPolicy) {
        Objects.requireNonNull(session, "session");
        JsonNode fallback = JSON.valueToTree(CaptureEnrichmentPreview.Proposal.empty());
        AiRequest request = AiRequest.builder("shaft-capture-generation-enrichment", schema())
                .requestId("capture-enrichment-" + fingerprint.substring(0, Math.min(16, fingerprint.length())))
                .text(prompt(session, deterministicClassName, deterministicMethodName, deterministicElementNames))
                .timeout(Duration.ofSeconds(30))
                .budget(new AiBudget(8_000, 1_500, BigDecimal.ZERO))
                .approvalPolicy(approvalPolicy)
                .deterministicFallback(fallback)
                .build();
        AiResponse response = executor.apply(request);
        if (!response.successful()) {
            throw new IllegalStateException("AI enrichment preview was not accepted: " + response.status()
                    + (response.fallbackReason().isBlank() ? "" : " (" + response.fallbackReason() + ")"));
        }
        CaptureEnrichmentPreview.Proposal proposal = parseProposal(response.structuredPayload());
        validateProposal(proposal, deterministicElementNames);
        List<String> diff = diff(deterministicClassName, deterministicMethodName,
                deterministicElementNames, proposal);
        return new CaptureEnrichmentPreview(
                CaptureEnrichmentPreview.CURRENT_SCHEMA_VERSION,
                fingerprint,
                response.provider(),
                proposal,
                diff);
    }

    private static String prompt(
            CaptureSession session,
            String className,
            String methodName,
            Map<String, String> elementNames) {
        ObjectNode root = JSON.createObjectNode();
        root.put("instruction", "Suggest concise Java names and optional state assertions only. "
                + "Do not suggest locators, values, URLs, credentials, or assertions that are not supported "
                + "by captured visible/enabled/selected state.");
        root.put("sessionId", session.sessionId());
        root.put("className", className);
        root.put("methodName", methodName);
        root.set("elementNames", JSON.valueToTree(elementNames));
        ArrayNode events = root.putArray("events");
        for (CaptureEvent event : session.events()) {
            ObjectNode item = events.addObject();
            item.put("sequence", event.context().sequence());
            item.put("type", event.getClass().getSimpleName());
            target(event).ifPresent(target -> {
                item.put("logicalElementId", target.logicalElementId());
                item.put("tagName", target.tagName());
                item.put("role", target.role());
                item.put("accessibleName", target.accessibleName());
                item.put("label", target.label());
                item.put("visible", target.visible());
                item.put("enabled", target.enabled());
                item.put("selected", target.selected());
            });
        }
        try {
            return JSON.writeValueAsString(root);
        } catch (JsonProcessingException exception) {
            throw new IllegalStateException("Sanitized enrichment context could not be serialized.", exception);
        }
    }

    private static JsonNode schema() {
        ObjectNode assertion = JSON.createObjectNode();
        assertion.put("type", "object");
        assertion.putArray("required").add("eventSequence").add("verification").add("negated");
        ObjectNode assertionProperties = assertion.putObject("properties");
        assertionProperties.putObject("eventSequence").put("type", "integer").put("minimum", 1);
        assertionProperties.putObject("verification").put("type", "string")
                .putArray("enum")
                .add("ELEMENT_PRESENT")
                .add("ELEMENT_VISIBLE")
                .add("ELEMENT_ENABLED")
                .add("ELEMENT_SELECTED");
        assertionProperties.putObject("negated").put("type", "boolean");
        assertion.put("additionalProperties", false);

        ObjectNode schema = JSON.createObjectNode();
        schema.put("type", "object");
        schema.putArray("required").add("className").add("methodName").add("elementNames").add("assertions");
        ObjectNode properties = schema.putObject("properties");
        properties.putObject("className").put("type", "string");
        properties.putObject("methodName").put("type", "string");
        properties.putObject("elementNames").put("type", "object");
        properties.putObject("assertions").put("type", "array").set("items", assertion);
        schema.put("additionalProperties", false);
        return schema;
    }

    private static CaptureEnrichmentPreview.Proposal parseProposal(JsonNode payload) {
        Map<String, String> elementNames = new LinkedHashMap<>();
        payload.path("elementNames").fields().forEachRemaining(entry ->
                elementNames.put(entry.getKey(), entry.getValue().asText("")));
        List<CaptureEnrichmentPreview.AssertionSuggestion> assertions = new ArrayList<>();
        for (JsonNode item : payload.path("assertions")) {
            assertions.add(new CaptureEnrichmentPreview.AssertionSuggestion(
                    item.path("eventSequence").asLong(),
                    item.path("verification").asText(),
                    item.path("negated").asBoolean()));
        }
        return new CaptureEnrichmentPreview.Proposal(
                payload.path("className").asText(""),
                payload.path("methodName").asText(""),
                elementNames,
                assertions);
    }

    private static void validateProposal(
            CaptureEnrichmentPreview.Proposal proposal,
            Map<String, String> deterministicElementNames) {
        if (!proposal.className().isBlank() && !JAVA_IDENTIFIER.matcher(proposal.className()).matches()) {
            throw new IllegalStateException("AI enrichment class name is not a Java identifier.");
        }
        if (!proposal.methodName().isBlank() && !JAVA_IDENTIFIER.matcher(proposal.methodName()).matches()) {
            throw new IllegalStateException("AI enrichment method name is not a Java identifier.");
        }
        Set<String> names = new HashSet<>();
        proposal.elementNames().forEach((id, name) -> {
            if (!deterministicElementNames.containsKey(id)) {
                throw new IllegalStateException("AI enrichment references an unknown element.");
            }
            if (!JAVA_IDENTIFIER.matcher(name).matches() || !names.add(name)) {
                throw new IllegalStateException("AI enrichment element names must be unique Java identifiers.");
            }
        });
    }

    private static List<String> diff(
            String className,
            String methodName,
            Map<String, String> elementNames,
            CaptureEnrichmentPreview.Proposal proposal) {
        List<String> diff = new ArrayList<>();
        addChange(diff, "class", className, proposal.className());
        addChange(diff, "method", methodName, proposal.methodName());
        proposal.elementNames().entrySet().stream()
                .sorted(Map.Entry.comparingByKey())
                .forEach(entry -> addChange(diff, "element " + entry.getKey(),
                        elementNames.getOrDefault(entry.getKey(), ""), entry.getValue()));
        proposal.assertions().stream()
                .sorted(java.util.Comparator.comparingLong(CaptureEnrichmentPreview.AssertionSuggestion::eventSequence)
                        .thenComparing(CaptureEnrichmentPreview.AssertionSuggestion::verification))
                .forEach(assertion -> diff.add("+ assertion event-" + assertion.eventSequence() + " "
                        + (assertion.negated() ? "NOT " : "") + assertion.verification()));
        return List.copyOf(diff);
    }

    private static void addChange(List<String> diff, String label, String before, String after) {
        if (after != null && !after.isBlank() && !after.equals(before)) {
            diff.add(label + ": " + before + " -> " + after);
        }
    }

    private static java.util.Optional<com.shaft.capture.model.ElementSnapshot> target(CaptureEvent event) {
        if (event instanceof CaptureEvent.ClickEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.TypeEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.ClearEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.SelectEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.ToggleEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.UploadEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.KeyboardEvent value) {
            return java.util.Optional.ofNullable(value.target());
        }
        if (event instanceof CaptureEvent.FrameEvent value) {
            return java.util.Optional.ofNullable(value.target());
        }
        if (event instanceof CaptureEvent.WaitEvent value) {
            return java.util.Optional.ofNullable(value.target());
        }
        if (event instanceof CaptureEvent.VerificationEvent value) {
            return java.util.Optional.ofNullable(value.target());
        }
        return java.util.Optional.empty();
    }
}
