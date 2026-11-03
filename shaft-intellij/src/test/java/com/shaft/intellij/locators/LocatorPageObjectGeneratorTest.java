package com.shaft.intellij.locators;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

class LocatorPageObjectGeneratorTest {
    @Test
    void generatesPageObjectAndFlagsNonHittable() {
        String code = LocatorPageObjectGenerator.generate("LoginPage", List.of(
                new LocatorPageObjectGenerator.LocatorRow(
                        "userName", "SHAFT.GUI.Locator.hasTagName(\"input\").build()", true, true, true, 1),
                new LocatorPageObjectGenerator.LocatorRow(
                        "hidden", "SHAFT.GUI.Locator.hasId(\"x\").build()", false, false, true, 2)
        ));
        assertTrue(code.contains("class LoginPage"));
        assertTrue(code.contains("userName()"));
        assertTrue(code.contains("WARNING"));
        assertTrue(LocatorPageObjectGenerator.crossPlatformPairFields("a()", "i()").contains("android=a()"));
    }
}
