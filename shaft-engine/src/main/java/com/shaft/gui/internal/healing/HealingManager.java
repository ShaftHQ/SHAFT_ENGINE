package com.shaft.gui.internal.healing;

import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.By;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;

import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.regex.Pattern;

/**
 * Guards optional SHAFT Heal provider calls and preserves deterministic fallback.
 */
public final class HealingManager {
    private static final Pattern SECRET_ASSIGNMENT = Pattern.compile(
            "(?i)(password|passwd|secret|token|api[-_]?key|authorization|cookie)\\s*[:=]\\s*[^\\s,;]+");
    private static final Pattern LONG_TOKEN = Pattern.compile("\\b[a-zA-Z0-9_-]{32,}\\b");

    private HealingManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Attempts recovery only for locator-not-found failures at SHAFT-owned
     * element-resolution boundaries.
     *
     * @param driver active driver
     * @param locator failed locator
     * @param action intended action
     * @param visibilityRequired whether visibility and enabled state are required
     * @param frameLocator active frame locator
     * @param shadowHostLocator active shadow host locator
     * @param shadowContentLocator active shadow content locator
     * @return validated optional resolution
     */
    public static Optional<HealingResolution> resolve(
            WebDriver driver,
            By locator,
            String action,
            boolean visibilityRequired,
            By frameLocator,
            By shadowHostLocator,
            By shadowContentLocator) {
        if (!HealingStrategy.current().usesShaftHeal() || !isApplicable(action)) {
            return Optional.empty();
        }
        try {
            Optional<HealingProvider> provider = HealingProviderRegistry.findProvider();
            if (provider.isEmpty()) {
                ReportManager.logDiscrete(
                        "SHAFT Heal was requested but io.github.shafthq:shaft-heal is not on the runtime classpath.");
                return Optional.empty();
            }
            HealingProvider healingProvider = provider.get();
            Optional<HealingResolution> resolution = healingProvider.resolve(new HealingRequest(
                    driver, locator, action, visibilityRequired, frameLocator, shadowHostLocator, shadowContentLocator));
            Optional<HealingResolution> accepted = resolution.filter(value -> value.elements().size() == 1);
            accepted.ifPresent(value -> reportAcceptedRecovery(healingProvider, value, locator));
            return accepted;
        } catch (RuntimeException exception) {
            ReportManagerHelper.logDiscrete(exception);
            ReportManager.logDiscrete("SHAFT Heal recovery failed; preserving the original locator failure.");
            return Optional.empty();
        }
    }

    /**
     * Records a unique successful original resolution when SHAFT Heal is active.
     *
     * @param driver active driver
     * @param locator successful locator
     * @param elements matching elements
     * @param action action name
     * @param frameLocator active frame locator
     * @param shadowHostLocator active shadow host locator
     * @param shadowContentLocator active shadow content locator
     */
    public static void observe(
            WebDriver driver,
            By locator,
            List<WebElement> elements,
            String action,
            By frameLocator,
            By shadowHostLocator,
            By shadowContentLocator) {
        if (!HealingStrategy.current().usesShaftHeal()
                || elements == null
                || elements.size() != 1) {
            return;
        }
        try {
            HealingProviderRegistry.findProvider().ifPresent(provider -> provider.observe(new HealingObservation(
                    driver, locator, elements.getFirst(), action,
                    frameLocator, shadowHostLocator, shadowContentLocator)));
        } catch (RuntimeException exception) {
            ReportManagerHelper.logDiscrete(exception);
        }
    }

    /**
     * Records the result of an action performed by SHAFT on a recovered element.
     *
     * @param driver active driver
     * @param resolution recovery resolution
     * @param originalLocator original locator
     * @param action action name
     * @param successful action result
     * @param failure safe failure category
     */
    public static void recordOutcome(
            WebDriver driver,
            HealingResolution resolution,
            By originalLocator,
            String action,
            boolean successful,
            String failure) {
        if (resolution == null) {
            return;
        }
        try {
            String verification = verification(resolution, action, successful);
            HealingProviderRegistry.findProvider().ifPresent(provider -> provider.recordOutcome(
                    new HealingActionOutcome(
                            driver,
                            resolution.attemptId(),
                            originalLocator,
                            resolution.selectedLocator(),
                            action,
                            successful,
                            verification,
                            failure)));
        } catch (RuntimeException exception) {
            ReportManagerHelper.logDiscrete(exception);
        }
    }

    /**
     * Clears optional provider state for a driver.
     *
     * @param driver closing driver
     */
    public static void clear(WebDriver driver) {
        if (driver == null) {
            return;
        }
        try {
            HealingProviderRegistry.findProvider().ifPresent(provider -> provider.clear(driver));
        } catch (RuntimeException exception) {
            ReportManagerHelper.logDiscrete(exception);
        }
    }

    private static boolean isApplicable(String action) {
        String normalized = action == null ? "" : action.toUpperCase(java.util.Locale.ROOT);
        if (normalized.contains("ASSERT") || normalized.contains("VERIFY")
                || normalized.contains("VALIDATION")) {
            return false;
        }
        for (StackTraceElement frame : Thread.currentThread().getStackTrace()) {
            if (frame.getClassName().startsWith("com.shaft.validation.")) {
                return false;
            }
        }
        return true;
    }

    private static String verification(
            HealingResolution resolution,
            String action,
            boolean successful) {
        if (!successful || resolution.elements().size() != 1) {
            return "FAILED";
        }
        String normalized = action == null ? "" : action.toUpperCase(java.util.Locale.ROOT);
        if (!(normalized.contains("TYPE")
                || normalized.contains("SELECT")
                || normalized.contains("CLEAR")
                || normalized.contains("ELEMENT_RESOLUTION")
                || normalized.contains("WAIT"))) {
            return "UNVERIFIABLE";
        }
        try {
            WebElement element = resolution.elements().getFirst();
            return element.isDisplayed() && element.isEnabled()
                    ? "ELEMENT_INTERACTABLE"
                    : "ELEMENT_NOT_INTERACTABLE";
        } catch (StaleElementReferenceException exception) {
            return "ELEMENT_STALE";
        } catch (WebDriverException exception) {
            return "UNVERIFIABLE";
        }
    }

    private static void reportAcceptedRecovery(
            HealingProvider provider,
            HealingResolution resolution,
            By originalLocator) {
        Optional<HealingExplanation> explanation = provider.explain(resolution.attemptId());
        String message = explanation
                .map(HealingManager::formatExplanation)
                .orElseGet(() -> "SHAFT Heal recovered locator with provider-validated trust. Original locator: \""
                        + safe(originalLocator) + "\", healed locator: \""
                        + safe(resolution.selectedLocator()) + "\".");
        ReportManager.log(message, Level.WARN);
    }

    private static String formatExplanation(HealingExplanation explanation) {
        String evidence = explanation.evidence().isEmpty()
                ? ""
                : ", evidence: " + safe(String.join(", ", explanation.evidence()));
        String providerStatus = explanation.providerStatus().isBlank()
                ? ""
                : ", provider: " + safe(explanation.providerStatus());
        String reason = explanation.reason().isBlank()
                ? ""
                : ", reason: " + safe(explanation.reason());
        return "SHAFT Heal recovered locator. Original locator: \"" + safe(explanation.originalLocator())
                + "\", healed locator: \"" + safe(explanation.healedLocator())
                + "\", trust: " + percent(explanation.confidence())
                + ", threshold: " + percent(explanation.threshold())
                + providerStatus
                + evidence
                + reason
                + ".";
    }

    private static String percent(double value) {
        return String.format(Locale.ROOT, "%.0f%%", value * 100);
    }

    private static String safe(By locator) {
        return safe(locator == null ? "" : locator.toString());
    }

    private static String safe(String value) {
        String sanitized = value == null ? "" : value;
        sanitized = SECRET_ASSIGNMENT.matcher(sanitized).replaceAll("$1=[REDACTED]");
        sanitized = LONG_TOKEN.matcher(sanitized).replaceAll("[REDACTED]");
        return sanitized.replaceAll("[\\p{Cntrl}&&[^\\r\\n\\t]]", "").trim();
    }
}
