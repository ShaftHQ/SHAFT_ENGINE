package com.shaft.gui.internal.natural;

import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import com.shaft.gui.driver.ShaftLocator;
import com.shaft.gui.internal.locator.CompositeLocator;
import com.shaft.gui.playwright.browser.BrowserActions;
import com.shaft.gui.playwright.element.ElementActions;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.By;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Validates and executes natural-language action plans through Playwright SHAFT actions.
 */
public final class PlaywrightNaturalActionExecutor {
    private static final Pattern SECRET_ASSIGNMENT = Pattern.compile(
            "(?i)(password|passwd|secret|token|api[-_]?key|authorization|cookie)\\s*[:=]\\s*[^\\s,;]+");
    private static final Pattern LONG_TOKEN = Pattern.compile("\\b[a-zA-Z0-9_-]{32,}\\b");

    private PlaywrightNaturalActionExecutor() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Plans, validates, and executes a natural-language Playwright action.
     *
     * @param session active Playwright session
     * @param intent  user intent
     * @param args    user-provided action arguments
     */
    public static void perform(PlaywrightSession session, String intent, Object... args) {
        if (!SHAFT.Properties.naturalActions.enabled()) {
            FailureReporter.fail("Natural actions are disabled. Set `naturalActions.enabled=true` to use driver.act(...).");
        }
        NaturalActionRequest request = new NaturalActionRequest(
                null,
                intent,
                args == null ? List.of() : Arrays.asList(args),
                false,
                false);
        NaturalActionPlan planned = NaturalActionPlannerRegistry.plan(request);
        NaturalActionPlan validated = validate(session.page(), planned, allowedTargets());
        double threshold = threshold();
        if (validated.steps().isEmpty() || validated.trust() < threshold) {
            String message = "Natural action was not executed because trust "
                    + percent(validated.trust()) + " is below the configured threshold "
                    + percent(threshold) + ". Intent: \"" + safe(intent) + "\". Reason: "
                    + safe(validated.explanation());
            ReportManager.log(message, Level.WARN);
            FailureReporter.fail(message);
        }

        ReportManager.log("Natural action accepted with trust " + percent(validated.trust())
                + " using planner \"" + safe(validated.plannerId()) + "\" for intent \""
                + safe(intent) + "\".", Level.INFO);
        execute(session, validated);
    }

    private static NaturalActionPlan validate(
            Page page,
            NaturalActionPlan plan,
            Set<NaturalActionKind.Target> allowedTargets) {
        double aggregateTrust = plan.trust();
        for (NaturalActionStep step : plan.steps()) {
            double stepTrust = step.trust();
            if (!allowedTargets.contains(step.kind().target())) {
                stepTrust = 0;
            } else if (step.kind() == NaturalActionKind.BROWSER_NAVIGATE && isBlank(step.data())) {
                stepTrust = 0;
            } else if (step.kind().target() == NaturalActionKind.Target.ELEMENT
                    || step.kind().target() == NaturalActionKind.Target.TOUCH) {
                stepTrust = Math.min(stepTrust, locatorTrust(page, step.locator()));
            }
            aggregateTrust = Math.min(aggregateTrust, stepTrust);
        }
        return new NaturalActionPlan(
                plan.plannerId(),
                plan.intent(),
                plan.steps(),
                aggregateTrust,
                plan.explanation());
    }

    private static double locatorTrust(Page page, By locator) {
        for (By candidate : expand(locator)) {
            try {
                Locator playwrightLocator = ShaftLocator.from(candidate).toPlaywrightLocator(page);
                if (playwrightLocator.count() == 1 && playwrightLocator.isVisible() && playwrightLocator.isEnabled()) {
                    return 1.0;
                }
            } catch (RuntimeException ignored) {
                // Unsupported candidates in composite locators do not make later portable candidates invalid.
            }
        }
        return 0;
    }

    private static List<By> expand(By locator) {
        if (locator == null) {
            return List.of();
        }
        if (locator instanceof CompositeLocator compositeLocator) {
            return compositeLocator.alternatives();
        }
        return List.of(locator);
    }

    private static void execute(PlaywrightSession session, NaturalActionPlan plan) {
        ElementActions elements = null;
        BrowserActions browser = null;
        for (NaturalActionStep step : plan.steps()) {
            switch (step.kind()) {
                case ELEMENT_CLICK -> (elements = elements == null ? new ElementActions(session) : elements).click(resolve(session.page(), step.locator()));
                case ELEMENT_TYPE ->
                        (elements = elements == null ? new ElementActions(session) : elements).type(resolve(session.page(), step.locator()), asCharSequence(step.data()));
                case ELEMENT_TYPE_SECURELY ->
                        (elements = elements == null ? new ElementActions(session) : elements).typeSecure(resolve(session.page(), step.locator()), asCharSequence(step.data()));
                case ELEMENT_CLEAR -> (elements = elements == null ? new ElementActions(session) : elements).clear(resolve(session.page(), step.locator()));
                case TOUCH_TAP, TOUCH_DOUBLE_TAP, TOUCH_LONG_TAP ->
                        FailureReporter.fail("Touch natural actions are not available through the Playwright backend. Use WebDriver/Appium touch actions for native gestures.");
                case BROWSER_NAVIGATE ->
                        (browser = browser == null ? new BrowserActions(session) : browser).navigateToURL(String.valueOf(step.data()));
                case BROWSER_REFRESH -> (browser = browser == null ? new BrowserActions(session) : browser).refreshCurrentPage();
                case BROWSER_BACK -> (browser = browser == null ? new BrowserActions(session) : browser).navigateBack();
                case BROWSER_FORWARD -> (browser = browser == null ? new BrowserActions(session) : browser).navigateForward();
                default -> FailureReporter.fail("Unsupported Playwright natural action kind: " + step.kind());
            }
        }
    }

    private static Locator resolve(Page page, By locator) {
        for (By candidate : expand(locator)) {
            try {
                Locator playwrightLocator = ShaftLocator.from(candidate).toPlaywrightLocator(page);
                if (playwrightLocator.count() == 1) {
                    return playwrightLocator;
                }
            } catch (RuntimeException ignored) {
                // Unsupported candidates in composite locators do not make later portable candidates invalid.
            }
        }
        throw new IllegalArgumentException("No unique Playwright element matched natural-action locator: " + locator);
    }

    private static CharSequence[] asCharSequence(Object value) {
        if (value instanceof CharSequence[] sequences) {
            return sequences;
        }
        if (value instanceof CharSequence sequence) {
            return new CharSequence[]{sequence};
        }
        return new CharSequence[]{String.valueOf(value)};
    }

    private static Set<NaturalActionKind.Target> allowedTargets() {
        return Arrays.stream(SHAFT.Properties.naturalActions.allowedActions().split(","))
                .map(String::trim)
                .filter(value -> !value.isEmpty())
                .map(value -> value.toUpperCase(Locale.ROOT))
                .map(value -> {
                    try {
                        return NaturalActionKind.Target.valueOf(value);
                    } catch (IllegalArgumentException ignored) {
                        return null;
                    }
                })
                .filter(java.util.Objects::nonNull)
                .collect(Collectors.toUnmodifiableSet());
    }

    private static double threshold() {
        int configured = SHAFT.Properties.naturalActions.minimumTrustPercentage();
        return Math.max(0, Math.min(100, configured)) / 100.0;
    }

    private static boolean isBlank(Object value) {
        return value == null || String.valueOf(value).isBlank();
    }

    private static String percent(double value) {
        return String.format(Locale.ROOT, "%.0f%%", value * 100);
    }

    private static String safe(String value) {
        String sanitized = value == null ? "" : value;
        sanitized = SECRET_ASSIGNMENT.matcher(sanitized).replaceAll("$1=[REDACTED]");
        sanitized = LONG_TOKEN.matcher(sanitized).replaceAll("[REDACTED]");
        return sanitized.replaceAll("[\\p{Cntrl}&&[^\\r\\n\\t]]", "").trim();
    }
}
