package com.shaft.gui.internal.healing;

import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.util.List;
import java.util.Optional;

/**
 * Guards optional SHAFT Heal provider calls and preserves deterministic fallback.
 */
public final class HealingManager {
    private HealingManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Attempts recovery only for web locator-not-found failures.
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
        if (!HealingStrategy.current().usesShaftHeal() || DriverFactoryHelper.isMobileNativeExecution()) {
            return Optional.empty();
        }
        try {
            Optional<HealingProvider> provider = HealingProviderRegistry.findProvider();
            if (provider.isEmpty()) {
                ReportManager.logDiscrete(
                        "SHAFT Heal was requested but io.github.shafthq:shaft-heal is not on the runtime classpath.");
                return Optional.empty();
            }
            Optional<HealingResolution> resolution = provider.get().resolve(new HealingRequest(
                    driver, locator, action, visibilityRequired, frameLocator, shadowHostLocator, shadowContentLocator));
            return resolution.filter(value -> value.elements().size() == 1);
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
                || DriverFactoryHelper.isMobileNativeExecution()
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
            HealingProviderRegistry.findProvider().ifPresent(provider -> provider.recordOutcome(
                    new HealingActionOutcome(
                            driver,
                            resolution.attemptId(),
                            originalLocator,
                            resolution.selectedLocator(),
                            action,
                            successful,
                            "UNVERIFIABLE",
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
}
