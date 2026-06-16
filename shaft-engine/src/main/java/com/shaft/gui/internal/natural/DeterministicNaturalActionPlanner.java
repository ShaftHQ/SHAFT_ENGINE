package com.shaft.gui.internal.natural;

import com.shaft.gui.internal.locator.SmartLocators;
import org.openqa.selenium.By;
import org.openqa.selenium.support.pagefactory.ByAll;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

/**
 * Deterministic natural-language planner for common browser, element, and touch actions.
 */
public class DeterministicNaturalActionPlanner implements NaturalActionPlanner {
    private static final double HIGH_TRUST = 0.92;
    private static final double MEDIUM_TRUST = 0.88;

    @Override
    public String id() {
        return "deterministic";
    }

    @Override
    public int priority() {
        return 100;
    }

    @Override
    public NaturalActionPlan plan(NaturalActionRequest request) {
        String intent = request.intent();
        String normalized = normalize(intent);
        if (normalized.isBlank()) {
            return NaturalActionPlan.unsupported(id(), intent, "Natural action intent is blank.");
        }
        NaturalActionPlan browserPlan = browserPlan(request, normalized);
        if (!browserPlan.steps().isEmpty()) {
            return browserPlan;
        }
        NaturalActionPlan loginPlan = loginPlan(request, normalized);
        if (!loginPlan.steps().isEmpty()) {
            return loginPlan;
        }
        NaturalActionPlan elementPlan = elementPlan(request, normalized);
        if (!elementPlan.steps().isEmpty()) {
            return elementPlan;
        }
        NaturalActionPlan touchPlan = touchPlan(request, normalized);
        if (!touchPlan.steps().isEmpty()) {
            return touchPlan;
        }
        return NaturalActionPlan.unsupported(id(), intent, "No deterministic natural-action pattern matched.");
    }

    private NaturalActionPlan browserPlan(NaturalActionRequest request, String normalized) {
        if (normalized.equals("refresh") || normalized.equals("refresh page")
                || normalized.equals("reload") || normalized.equals("reload page")) {
            return plan(request, List.of(new NaturalActionStep(
                    NaturalActionKind.BROWSER_REFRESH, null, null, HIGH_TRUST, "Refresh the current page.")),
                    HIGH_TRUST, "Matched a deterministic browser refresh intent.");
        }
        if (normalized.equals("go back") || normalized.equals("navigate back") || normalized.equals("back")) {
            return plan(request, List.of(new NaturalActionStep(
                    NaturalActionKind.BROWSER_BACK, null, null, HIGH_TRUST, "Navigate back.")),
                    HIGH_TRUST, "Matched a deterministic browser back intent.");
        }
        if (normalized.equals("go forward") || normalized.equals("navigate forward") || normalized.equals("forward")) {
            return plan(request, List.of(new NaturalActionStep(
                    NaturalActionKind.BROWSER_FORWARD, null, null, HIGH_TRUST, "Navigate forward.")),
                    HIGH_TRUST, "Matched a deterministic browser forward intent.");
        }
        if ((normalized.startsWith("navigate to ") || normalized.startsWith("open "))
                && !request.arguments().isEmpty()) {
            return plan(request, List.of(new NaturalActionStep(
                    NaturalActionKind.BROWSER_NAVIGATE,
                    null,
                    String.valueOf(request.arguments().getFirst()),
                    MEDIUM_TRUST,
                    "Navigate to the supplied URL argument.")),
                    MEDIUM_TRUST, "Matched a deterministic browser navigation intent.");
        }
        return NaturalActionPlan.unsupported(id(), request.intent(), "");
    }

    private NaturalActionPlan loginPlan(NaturalActionRequest request, String normalized) {
        if (!normalized.contains("login") && !normalized.contains("log in") && !normalized.contains("sign in")) {
            return NaturalActionPlan.unsupported(id(), request.intent(), "");
        }
        if (request.arguments().size() < 2) {
            return NaturalActionPlan.unsupported(
                    id(), request.intent(), "Login intents require username and password arguments.");
        }
        List<NaturalActionStep> steps = new ArrayList<>();
        steps.add(new NaturalActionStep(
                NaturalActionKind.ELEMENT_TYPE,
                new ByAll(
                        SmartLocators.inputField("username"),
                        SmartLocators.inputField("email"),
                        SmartLocators.inputField("login")),
                request.arguments().get(0),
                HIGH_TRUST,
                "Type the first argument into the username, email, or login field."));
        steps.add(new NaturalActionStep(
                NaturalActionKind.ELEMENT_TYPE_SECURELY,
                new ByAll(By.cssSelector("input[type='password']"), SmartLocators.inputField("password")),
                request.arguments().get(1),
                HIGH_TRUST,
                "Type the second argument into the password field securely."));
        steps.add(new NaturalActionStep(
                NaturalActionKind.ELEMENT_CLICK,
                new ByAll(
                        SmartLocators.clickableField("login"),
                        SmartLocators.clickableField("log in"),
                        SmartLocators.clickableField("sign in"),
                        SmartLocators.clickableField("submit")),
                null,
                MEDIUM_TRUST,
                "Click the login, sign in, or submit action."));
        return plan(request, steps, MEDIUM_TRUST, "Matched a deterministic login workflow.");
    }

    private NaturalActionPlan elementPlan(NaturalActionRequest request, String normalized) {
        if (normalized.startsWith("click ")) {
            String label = request.intent().substring(request.intent().toLowerCase(Locale.ROOT).indexOf("click ") + 6).trim();
            if (!label.isBlank()) {
                return plan(request, List.of(new NaturalActionStep(
                        NaturalActionKind.ELEMENT_CLICK,
                        SmartLocators.clickableField(label),
                        null,
                        MEDIUM_TRUST,
                        "Click an element resolved by semantic label.")),
                        MEDIUM_TRUST, "Matched a deterministic element click intent.");
            }
        }
        if ((normalized.startsWith("type ") || normalized.startsWith("enter ")) && !request.arguments().isEmpty()) {
            String label = extractFieldLabel(request.intent());
            if (!label.isBlank()) {
                return plan(request, List.of(new NaturalActionStep(
                        NaturalActionKind.ELEMENT_TYPE,
                        SmartLocators.inputField(label),
                        request.arguments().getFirst(),
                        MEDIUM_TRUST,
                        "Type the first argument into an input field resolved by semantic label.")),
                        MEDIUM_TRUST, "Matched a deterministic element type intent.");
            }
        }
        if (normalized.startsWith("clear ")) {
            String label = request.intent().substring(request.intent().toLowerCase(Locale.ROOT).indexOf("clear ") + 6).trim();
            if (!label.isBlank()) {
                return plan(request, List.of(new NaturalActionStep(
                        NaturalActionKind.ELEMENT_CLEAR,
                        SmartLocators.inputField(label),
                        null,
                        MEDIUM_TRUST,
                        "Clear an input field resolved by semantic label.")),
                        MEDIUM_TRUST, "Matched a deterministic element clear intent.");
            }
        }
        return NaturalActionPlan.unsupported(id(), request.intent(), "");
    }

    private NaturalActionPlan touchPlan(NaturalActionRequest request, String normalized) {
        if (normalized.startsWith("tap ")) {
            String label = request.intent().substring(request.intent().toLowerCase(Locale.ROOT).indexOf("tap ") + 4).trim();
            if (!label.isBlank()) {
                return plan(request, List.of(new NaturalActionStep(
                        NaturalActionKind.TOUCH_TAP,
                        SmartLocators.clickableField(label),
                        null,
                        MEDIUM_TRUST,
                        "Tap an element resolved by semantic label.")),
                        MEDIUM_TRUST, "Matched a deterministic touch tap intent.");
            }
        }
        return NaturalActionPlan.unsupported(id(), request.intent(), "");
    }

    private NaturalActionPlan plan(
            NaturalActionRequest request,
            List<NaturalActionStep> steps,
            double trust,
            String explanation) {
        return new NaturalActionPlan(id(), request.intent(), steps, trust, explanation);
    }

    private static String extractFieldLabel(String intent) {
        String lower = intent.toLowerCase(Locale.ROOT);
        int intoIndex = lower.indexOf(" into ");
        if (intoIndex >= 0) {
            return intent.substring(intoIndex + 6).trim();
        }
        int inIndex = lower.indexOf(" in ");
        if (inIndex >= 0) {
            return intent.substring(inIndex + 4).trim();
        }
        int typeIndex = lower.indexOf("type ");
        if (typeIndex >= 0) {
            return intent.substring(typeIndex + 5).trim();
        }
        int enterIndex = lower.indexOf("enter ");
        if (enterIndex >= 0) {
            return intent.substring(enterIndex + 6).trim();
        }
        return "";
    }

    private static String normalize(String value) {
        return value == null ? "" : value.trim().replaceAll("\\s+", " ").toLowerCase(Locale.ROOT);
    }
}
