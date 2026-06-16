package com.shaft.pilot.natural;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
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
import java.util.List;
import java.util.Locale;

/**
 * Optional plan-only AI planner for SHAFT natural actions.
 */
public class PilotNaturalActionPlanner implements NaturalActionPlanner {
    private static final ObjectMapper JSON = new ObjectMapper();
    private final AiExecutionService executionService;

    /**
     * Creates the service-loadable planner.
     */
    public PilotNaturalActionPlanner() {
        this(new AiExecutionService());
    }

    PilotNaturalActionPlanner(AiExecutionService executionService) {
        this.executionService = executionService;
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
        var response = executionService.execute(aiRequest);
        if (!response.successful()) {
            return NaturalActionPlan.unsupported(id(), request.intent(), response.fallbackReason());
        }
        return parse(request, response.structuredPayload());
    }

    private NaturalActionPlan parse(NaturalActionRequest request, JsonNode payload) {
        List<NaturalActionStep> steps = new ArrayList<>();
        for (JsonNode node : payload.path("steps")) {
            NaturalActionKind kind;
            try {
                kind = NaturalActionKind.valueOf(node.path("kind").asText("").toUpperCase(Locale.ROOT));
            } catch (IllegalArgumentException exception) {
                return NaturalActionPlan.unsupported(id(), request.intent(), "Provider returned an unknown action kind.");
            }
            String label = node.path("label").asText("");
            int argIndex = node.path("argIndex").asInt(-1);
            Object data = argIndex >= 0 && argIndex < request.arguments().size()
                    ? request.arguments().get(argIndex)
                    : node.path("value").asText(null);
            By locator = locator(kind, label);
            double trust = node.path("trust").asDouble(0);
            steps.add(new NaturalActionStep(kind, locator, data, trust, "Provider-planned " + kind.name() + "."));
        }
        return new NaturalActionPlan(
                id(),
                request.intent(),
                steps,
                payload.path("trust").asDouble(0),
                payload.path("explanation").asText("Provider returned a structured plan."));
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
        itemProperties.putObject("kind").put("type", "string");
        itemProperties.putObject("label").put("type", "string");
        itemProperties.putObject("value").put("type", "string");
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
