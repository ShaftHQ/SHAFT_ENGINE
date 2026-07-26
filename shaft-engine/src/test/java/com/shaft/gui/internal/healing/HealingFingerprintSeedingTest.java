package com.shaft.gui.internal.healing;

import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * Covers the generation-time fingerprint seeding seam (issue #4161): a plain-data seed that lets
 * an optional provider persist recorded element evidence without a live driver or element.
 */
public class HealingFingerprintSeedingTest {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        HealingProviderRegistry.resetProviderForTesting();
    }

    @Test
    public void managerShouldDispatchFingerprintSeedToConfiguredProvider() {
        HealingProvider provider = mock(HealingProvider.class);
        HealingProviderRegistry.setProviderForTesting(provider);
        HealingFingerprintObservation observation = observation();

        HealingManager.observeFingerprint(observation);

        verify(provider).observeFingerprint(observation);
    }

    @Test
    public void managerShouldSwallowProviderFailuresWithoutThrowing() {
        HealingProvider provider = mock(HealingProvider.class);
        doThrow(new IllegalStateException("boom")).when(provider).observeFingerprint(any());
        HealingProviderRegistry.setProviderForTesting(provider);

        HealingManager.observeFingerprint(observation());
        // Reaching here without a thrown exception is the assertion: optional-provider failures
        // must never surface to the caller (matches every other HealingManager entry point).
    }

    @Test
    public void managerShouldNoOpWhenNoProviderIsConfigured() {
        HealingProviderRegistry.resetProviderForTesting();

        HealingManager.observeFingerprint(observation());
        // No provider on the classpath -- must be a silent no-op, not a failure.
    }

    @Test
    public void seedShouldDefaultNullFieldsToEmptyRatherThanThrowing() {
        HealingFingerprintSeed seed = new HealingFingerprintSeed(
                null, null, null, null, null, null, null, null, null, null,
                null, null, true, true, false);

        Assert.assertEquals(seed.tagName(), "");
        Assert.assertEquals(seed.testIds(), Map.of());
        Assert.assertEquals(seed.semanticAttributes(), Map.of());
    }

    private static HealingFingerprintObservation observation() {
        HealingFingerprintSeed seed = new HealingFingerprintSeed(
                "input", "Username", "Username label", "", "user-id", "username",
                "textbox", "text", "Username", "",
                Map.of("data-testid", "user-id-test"), Map.of(),
                true, true, false);
        return new HealingFingerprintObservation(
                "https://example.test/form", By.id("user-id"), "TYPE", seed);
    }
}
