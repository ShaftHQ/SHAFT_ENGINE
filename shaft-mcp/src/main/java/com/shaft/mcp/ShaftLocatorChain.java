package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.locator.LocatorBuilder;
import org.openqa.selenium.By;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.util.ArrayList;
import java.util.List;

/**
 * Builds a SHAFT {@code Locator} builder chain, sent as a JSON array of steps such as
 * {@code [["hasTagName","button"],["containsText","Save"],["isFirst"]]}, with the engine itself,
 * so callers like the IntelliJ plugin get exactly the locator the test builds (issue #6445).
 * Only the allowlisted builder steps below are accepted.
 */
final class ShaftLocatorChain {
    private static final ObjectMapper MAPPER = new ObjectMapper();

    private ShaftLocatorChain() {
    }

    static By build(String chainJson) {
        List<List<String>> steps = parse(chainJson);
        LocatorBuilder builder = start(steps.getFirst());
        for (List<String> step : steps.subList(1, steps.size())) {
            builder = apply(builder, step);
        }
        return builder.build();
    }

    /** Java source for the chain, for example {@code SHAFT.GUI.Locator.hasTagName("a").isFirst().build()}. */
    static String code(String chainJson) {
        build(chainJson);
        StringBuilder code = new StringBuilder("SHAFT.GUI.Locator");
        for (List<String> step : parse(chainJson)) {
            code.append('.').append(step.getFirst()).append('(');
            for (int i = 1; i < step.size(); i++) {
                code.append(i > 1 ? ", " : "");
                code.append("hasIndex".equals(step.getFirst()) ? String.valueOf(index(step.get(i))) : literal(step.get(i)));
            }
            code.append(')');
        }
        return code.append(".build()").toString();
    }

    private static String literal(String value) {
        return '"' + value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + '"';
    }

    private static LocatorBuilder start(List<String> step) {
        return switch (signature(step)) {
            case "hasTagName/1" -> SHAFT.GUI.Locator.hasTagName(step.get(1));
            case "hasAnyTagName/0" -> SHAFT.GUI.Locator.hasAnyTagName();
            default -> throw unsupported(step);
        };
    }

    private static LocatorBuilder apply(LocatorBuilder builder, List<String> step) {
        return switch (signature(step)) {
            case "hasAttribute/1" -> builder.hasAttribute(step.get(1));
            case "hasAttribute/2" -> builder.hasAttribute(step.get(1), step.get(2));
            case "containsAttribute/2" -> builder.containsAttribute(step.get(1), step.get(2));
            case "hasId/1" -> builder.hasId(step.get(1));
            case "containsId/1" -> builder.containsId(step.get(1));
            case "hasClass/1" -> builder.hasClass(step.get(1));
            case "containsClass/1" -> builder.containsClass(step.get(1));
            case "hasText/1" -> builder.hasText(step.get(1));
            case "hasNormalizedText/1" -> builder.hasNormalizedText(step.get(1));
            case "containsText/1" -> builder.containsText(step.get(1));
            case "hasIndex/1" -> builder.hasIndex(index(step.get(1)));
            case "isFirst/0" -> builder.isFirst();
            case "isLast/0" -> builder.isLast();
            case "and/0" -> builder.and();
            default -> throw unsupported(step);
        };
    }

    private static List<List<String>> parse(String chainJson) {
        JsonNode root;
        try {
            root = MAPPER.readTree(chainJson == null ? "" : chainJson);
        } catch (RuntimeException malformed) {
            throw new IllegalArgumentException("SHAFT_LOCATOR value must be a JSON array of builder steps", malformed);
        }
        if (root == null || !root.isArray() || root.isEmpty()) {
            throw new IllegalArgumentException("SHAFT_LOCATOR value must be a non-empty JSON array of builder steps");
        }
        List<List<String>> steps = new ArrayList<>();
        for (JsonNode step : root) {
            if (!step.isArray() || step.isEmpty()) {
                throw new IllegalArgumentException("each SHAFT_LOCATOR step must be [method, args...]");
            }
            List<String> parts = new ArrayList<>();
            step.forEach(part -> parts.add(part.asString()));
            steps.add(parts);
        }
        return steps;
    }

    private static String signature(List<String> step) {
        return step.getFirst() + "/" + (step.size() - 1);
    }

    private static int index(String value) {
        try {
            return Integer.parseInt(value.strip());
        } catch (NumberFormatException notANumber) {
            throw new IllegalArgumentException("hasIndex needs an integer, got: " + value, notANumber);
        }
    }

    private static IllegalArgumentException unsupported(List<String> step) {
        return new IllegalArgumentException("unsupported SHAFT_LOCATOR step: " + signature(step));
    }
}
