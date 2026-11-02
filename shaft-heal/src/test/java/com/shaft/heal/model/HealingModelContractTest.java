package com.shaft.heal.model;

import org.testng.annotations.Test;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotEquals;
import static org.testng.Assert.assertTrue;

/**
 * Issue #6375: direct contract tests for healing history keys and platform classification.
 */
public class HealingModelContractTest {
    @Test
    public void stableKeyIsDeterministicAndSeparatesContexts() {
        HealingContext checkout = new HealingContext(null, HealingPlatform.WEB, "shop", "checkout",
                null, "main", "#frame", null, null);
        HealingContext sameCheckout = new HealingContext(null, HealingPlatform.WEB, "shop", "checkout",
                null, "main", "#frame", null, null);
        HealingContext cart = new HealingContext(null, HealingPlatform.WEB, "shop", "cart",
                null, "main", "#frame", null, null);

        assertEquals(checkout.stableKey(), sameCheckout.stableKey());
        assertNotEquals(checkout.stableKey(), cart.stableKey());
        assertTrue(checkout.stableKey().startsWith("platform=WEB;app=shop;screen=checkout;"));
    }

    @Test
    public void onlyMobileAndNativePlatformsUseTheAccessibilityTree() {
        assertTrue(HealingPlatform.ANDROID.nativePlatform());
        assertTrue(HealingPlatform.IOS.nativePlatform());
        assertTrue(HealingPlatform.NATIVE.nativePlatform());
        assertFalse(HealingPlatform.WEB.nativePlatform());
        assertFalse(HealingPlatform.UNKNOWN.nativePlatform());
    }
}
