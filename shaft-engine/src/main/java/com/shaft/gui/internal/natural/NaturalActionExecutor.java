package com.shaft.gui.internal.natural;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Validates and executes natural-language action plans through normal SHAFT actions.
 */
public final class NaturalActionExecutor {
    private static final Pattern SECRET_ASSIGNMENT = Pattern.compile(
            "(?i)(password|passwd|secret|token|api[-_]?key|authorization|cookie)\\s*[:=]\\s*[^\\s,;]+");
    private static final Pattern LONG_TOKEN = Pattern.compile("\\b[a-zA-Z0-9_-]{32,}\\b");

    private NaturalActionExecutor() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Plans, validates, and executes a natural-language action.
     *
     * @param helper active driver helper
     * @param intent user intent
     * @param args user-provided action arguments
     */
    public static void perform(DriverFactoryHelper helper, String intent, Object... args) {
        if (!SHAFT.Properties.naturalActions.enabled()) {
            FailureReporter.fail("Natural actions are disabled. Set `naturalActions.enabled=true` to use driver.act(...).");
        }
        WebDriver driver = helper.getDriver();
        NaturalActionRequest request = new NaturalActionRequest(
                driver,
                intent,
                args == null ? List.of() : Arrays.asList(args),
                DriverFactoryHelper.isMobileNativeExecution(),
                DriverFactoryHelper.isMobileWebExecution());
        NaturalActionPlan planned = NaturalActionPlannerRegistry.plan(request);
        NaturalActionPlan validated = validate(driver, planned, allowedTargets());
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
        execute(helper, validated);
    }

    private static NaturalActionPlan validate(
            WebDriver driver,
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
                stepTrust = Math.min(stepTrust, locatorTrust(driver, step.locator()));
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

    private static double locatorTrust(WebDriver driver, By locator) {
        if (locator == null) {
            return 0;
        }
        List<WebElement> elements = ElementActionsHelper.safeFindElements(driver, locator);
        if (elements.size() != 1) {
            return 0;
        }
        WebElement element = elements.getFirst();
        try {
            return element.isDisplayed() && element.isEnabled() ? 1.0 : 0;
        } catch (WebDriverException exception) {
            return 0;
        }
    }

    private static void execute(DriverFactoryHelper helper, NaturalActionPlan plan) {
        Actions elements = null;
        BrowserActions browser = null;
        TouchActions touch = null;
        for (NaturalActionStep step : plan.steps()) {
            switch (step.kind()) {
                case ELEMENT_CLICK -> (elements = elements == null ? new Actions(helper) : elements).click(step.locator());
                case ELEMENT_TYPE ->
                        (elements = elements == null ? new Actions(helper) : elements).type(step.locator(), asCharSequence(step.data()));
                case ELEMENT_TYPE_SECURELY ->
                        (elements = elements == null ? new Actions(helper) : elements).typeSecure(step.locator(), asCharSequence(step.data()));
                case ELEMENT_CLEAR -> (elements = elements == null ? new Actions(helper) : elements).clear(step.locator());
                case TOUCH_TAP -> (touch = touch == null ? new TouchActions(helper) : touch).tap(step.locator());
                case TOUCH_DOUBLE_TAP -> (touch = touch == null ? new TouchActions(helper) : touch).doubleTap(step.locator());
                case TOUCH_LONG_TAP -> (touch = touch == null ? new TouchActions(helper) : touch).longTap(step.locator());
                case BROWSER_NAVIGATE ->
                        (browser = browser == null ? new BrowserActions(helper) : browser).navigateToURL(String.valueOf(step.data()));
                case BROWSER_REFRESH -> (browser = browser == null ? new BrowserActions(helper) : browser).refreshCurrentPage();
                case BROWSER_BACK -> (browser = browser == null ? new BrowserActions(helper) : browser).navigateBack();
                case BROWSER_FORWARD -> (browser = browser == null ? new BrowserActions(helper) : browser).navigateForward();
            }
        }
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
