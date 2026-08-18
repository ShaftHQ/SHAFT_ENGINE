package com.shaft.pilot.natural;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.SmartLocators;
import com.shaft.gui.internal.natural.NaturalActionKind;
import com.shaft.gui.internal.natural.NaturalActionPlan;
import com.shaft.gui.internal.natural.NaturalActionPlanner;
import com.shaft.gui.internal.natural.NaturalActionRequest;
import com.shaft.gui.internal.natural.NaturalActionStep;
import com.shaft.pilot.ai.AiExecutionService;
import com.shaft.pilot.ai.AiRequest;
import com.shaft.pilot.ai.EvidenceCategory;
import com.shaft.pilot.ai.EvidenceReference;
import com.shaft.pilot.config.PilotConfiguration;
import org.openqa.selenium.By;

import java.time.Duration;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.function.Function;

/**
 * Optional plan-only AI planner for SHAFT natural actions.
 */
public class PilotNaturalActionPlanner implements NaturalActionPlanner {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final Set<String> ALLOWED_STEP_FIELDS = Set.of("kind", "label", "argIndex", "trust");
    private static final int MAX_STEPS = 8;
    private final Function<AiRequest, com.shaft.pilot.ai.AiResponse> executor;

    /**
     * Creates the service-loadable planner.
     */
    public PilotNaturalActionPlanner() {
        this(new AiExecutionService()::execute);
    }

    PilotNaturalActionPlanner(AiExecutionService executionService) {
        this(executionService::execute);
    }

    PilotNaturalActionPlanner(Function<AiRequest, com.shaft.pilot.ai.AiResponse> executor) {
        this.executor = executor;
    }

    @Override
    public String id() {
        return "pilot";
    }

    @Override
    public int priority() {
        return 50;
    }

    @Override
    public boolean supports(NaturalActionRequest request) {
        return SHAFT.Properties.naturalActions.aiFallbackEnabled() && SHAFT.Properties.pilot.enabled();
    }

    @Override
    public NaturalActionPlan plan(NaturalActionRequest request) {
        if (!supports(request)) {
            return NaturalActionPlan.unsupported(id(), request.intent(), "Pilot natural-action planning is disabled.");
        }
        PilotConfiguration configuration;
        try {
            configuration = PilotConfiguration.current();
        } catch (RuntimeException exception) {
            return NaturalActionPlan.unsupported(id(), request.intent(), "Pilot configuration is invalid.");
        }
        AiRequest aiRequest = AiRequest.builder("natural-action-plan", responseSchema())
                .text("""
                        Return a SHAFT natural-action plan. Do not execute actions.
                        Use only these action kinds:
                        ELEMENT_CLICK, ELEMENT_TYPE, ELEMENT_TYPE_SECURELY, ELEMENT_CLEAR,
                        TOUCH_TAP, TOUCH_DOUBLE_TAP, TOUCH_LONG_TAP,
                        BROWSER_NAVIGATE, BROWSER_REFRESH, BROWSER_BACK, BROWSER_FORWARD.
                        For element and touch actions, return a short visible/accessibility label.
                        For typed values, bind by argIndex; never echo argument values.
                        """)
                .evidence(new EvidenceReference(
                        "natural-action-request",
                        EvidenceCategory.TEXT,
                        "text/plain",
                        evidence(request)))
                .timeout(Duration.ofSeconds(Math.max(1, configuration.timeout().toSeconds())))
                .approvalPolicy(configuration.approvalPolicy())
                .deterministicFallback(unsupportedPayload(request.intent(), "Provider fallback was used."))
                .build();
        var response = executor.apply(aiRequest);
        if (!response.successful()) {
            return NaturalActionPlan.unsupported(id(), request.intent(), response.fallbackReason());
        }
        return parse(request, response.structuredPayload());
    }

    private NaturalActionPlan parse(NaturalActionRequest request, JsonNode payload) {
        JsonNode stepsNode = payload.path("steps");
        if (!payload.path("trust").isNumber() || !stepsNode.isArray() || stepsNode.size() > MAX_STEPS) {
            return NaturalActionPlan.unsupported(id(), request.intent(), "Provider returned a malformed plan.");
        }
        List<NaturalActionStep> steps = new ArrayList<>();
        for (JsonNode node : stepsNode) {
            if (!node.isObject()) {
                return NaturalActionPlan.unsupported(id(), request.intent(), "Provider returned a malformed plan.");
            }
            Set<String> fields = new HashSet<>();
            node.propertyNames().forEach(fields::add);
            if (!ALLOWED_STEP_FIELDS.containsAll(fields)) {
                return NaturalActionPlan.unsupported(id(), request.intent(), "Provider returned an unknown parameter.");
            }
            NaturalActionKind kind;
            try {
                kind = NaturalActionKind.valueOf(node.path("kind").asText("").toUpperCase(Locale.ROOT));
            } catch (IllegalArgumentException exception) {
                return NaturalActionPlan.unsupported(id(), request.intent(), "Provider returned an unknown action kind.");
            }
            String label = node.path("label").asText("");
            if (unsafeLabel(label)) {
                return NaturalActionPlan.unsupported(id(), request.intent(), "Provider returned an unsafe parameter.");
            }
            int argIndex = node.path("argIndex").asInt(-1);
            Object data = argIndex >= 0 && argIndex < request.arguments().size()
                    ? request.arguments().get(argIndex)
                    : null;
            By locator = locator(kind, label);
            double trust = node.path("trust").asDouble(0);
            steps.add(new NaturalActionStep(kind, locator, data, trust, "Provider-planned " + kind.name() + "."));
        }
        String explanation = payload.path("explanation").asText("Provider returned a structured plan.");
        return NaturalActionPlan.of(
                id(),
                request.intent(),
                steps,
                payload.path("trust").asDouble(0),
                "Untrusted advisory: " + explanation);
    }

    private static boolean unsafeLabel(String label) {
        if (label == null || label.isBlank()) {
            return false;
        }
        String normalized = label.strip().toLowerCase(Locale.ROOT);
        return normalized.startsWith("javascript:") || normalized.startsWith("data:");
    }

    private static By locator(NaturalActionKind kind, String label) {
        if (label == null || label.isBlank()) {
            return null;
        }
        return switch (kind) {
            case ELEMENT_TYPE, ELEMENT_TYPE_SECURELY, ELEMENT_CLEAR -> SmartLocators.inputField(label);
            case ELEMENT_CLICK, TOUCH_TAP, TOUCH_DOUBLE_TAP, TOUCH_LONG_TAP -> SmartLocators.clickableField(label);
            default -> null;
        };
    }

    private static String evidence(NaturalActionRequest request) {
        String url;
        try {
            url = request.driver().getCurrentUrl();
        } catch (RuntimeException exception) {
            url = "";
        }
        return "intent=" + request.intent()
                + System.lineSeparator() + "argumentCount=" + request.arguments().size()
                + System.lineSeparator() + "currentUrl=" + url
                + System.lineSeparator() + "mobileNative=" + request.mobileNativeExecution()
                + System.lineSeparator() + "mobileWeb=" + request.mobileWebExecution();
    }

    private static ObjectNode responseSchema() {
        ObjectNode root = JSON.createObjectNode();
        root.put("type", "object");
        ObjectNode properties = root.putObject("properties");
        properties.putObject("trust").put("type", "number").put("minimum", 0).put("maximum", 1);
        properties.putObject("explanation").put("type", "string");
        ObjectNode steps = properties.putObject("steps");
        steps.put("type", "array");
        ObjectNode item = steps.putObject("items");
        item.put("type", "object");
        ObjectNode itemProperties = item.putObject("properties");
        ObjectNode kindSchema = itemProperties.putObject("kind");
        kindSchema.put("type", "string");
        ArrayNode kinds = kindSchema.putArray("enum");
        for (NaturalActionKind kind : NaturalActionKind.values()) {
            kinds.add(kind.name());
        }
        itemProperties.putObject("label").put("type", "string");
        itemProperties.putObject("argIndex").put("type", "integer");
        itemProperties.putObject("trust").put("type", "number").put("minimum", 0).put("maximum", 1);
        ArrayNode requiredItem = item.putArray("required");
        requiredItem.add("kind");
        requiredItem.add("trust");
        item.put("additionalProperties", false);
        ArrayNode required = root.putArray("required");
        required.add("trust");
        required.add("steps");
        root.put("additionalProperties", false);
        return root;
    }

    private static ObjectNode unsupportedPayload(String intent, String explanation) {
        ObjectNode root = JSON.createObjectNode();
        root.put("trust", 0);
        root.put("explanation", explanation + " Intent: " + intent);
        root.putArray("steps");
        return root;
    }
}
