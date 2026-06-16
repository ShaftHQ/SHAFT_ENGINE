package com.shaft.gui.internal.healing;

import org.openqa.selenium.WebDriver;

import java.util.Optional;

/**
 * SHAFT-owned contract for optional explainable element recovery providers.
 *
 * <p>Providers are discovered through {@link java.util.ServiceLoader}. They may
 * inspect the current page and return already-derived candidates, but they must
 * never execute the intended user action.</p>
 */
public interface HealingProvider {
    /**
     * Attempts recovery for a locator-not-found failure.
     *
     * @param request recovery request
     * @return a unique validated resolution, or empty to preserve the original failure
     */
    Optional<HealingResolution> resolve(HealingRequest request);

    /**
     * Returns a safe explanation for a recovery attempt.
     *
     * @param attemptId provider-owned safe correlation identifier
     * @return explanation when available
     */
    default Optional<HealingExplanation> explain(String attemptId) {
        return Optional.empty();
    }

    /**
     * Records evidence from a unique successful original resolution.
     *
     * @param observation successful observation
     */
    void observe(HealingObservation observation);

    /**
     * Records the action outcome after SHAFT, not the provider, executes it.
     *
     * @param outcome action outcome
     */
    void recordOutcome(HealingActionOutcome outcome);

    /**
     * Clears provider session state for a closed driver.
     *
     * @param driver closed driver
     */
    default void clear(WebDriver driver) {
        // Optional provider hook.
    }
}
