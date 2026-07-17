package com.shaft.intellij.testindex;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Real PSI discovery ({@link ShaftTestDiscovery#discover}'s scan path) needs a live IntelliJ
 * platform {@link com.intellij.openapi.project.Project} this plain JUnit environment does not have
 * (see {@code ShaftRunConfigurationResolverTest}'s javadoc for the same constraint) -- so only the
 * {@code null}-project contract is exercised here. {@code ShaftTestsPanelTest} covers the
 * discovery-consuming tree logic against a fixed, injected {@link ShaftTestDiscovery.DiscoveredTestClass}
 * list instead.
 */
class ShaftTestDiscoveryTest {
    @Test
    void discoverReturnsEmptyImmediatelyForNullProject() {
        List<ShaftTestDiscovery.DiscoveredTestClass> discovered = ShaftTestDiscovery.discover(null);

        assertTrue(discovered.isEmpty());
    }

    @Test
    void discoveredTestClassCarriesPlainData() {
        ShaftTestDiscovery.DiscoveredTestClass discoveredClass = new ShaftTestDiscovery.DiscoveredTestClass(
                "com.example.CheckoutTest", "com.example", "CheckoutTest", List.of("testCheckout"));

        assertEquals("com.example.CheckoutTest", discoveredClass.qualifiedName());
        assertEquals("com.example", discoveredClass.packageName());
        assertEquals("CheckoutTest", discoveredClass.simpleName());
        assertEquals(List.of("testCheckout"), discoveredClass.methodNames());
    }
}
