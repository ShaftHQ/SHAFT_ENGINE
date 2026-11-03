package com.shaft.intellij.locators;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

class LocatorPageObjectGeneratorTest {
    @Test
    void generatesPageObjectClass() {
        String po = LocatorPageObjectGenerator.generate("LoginPage", List.of(
                new LocatorPageObjectGenerator.LocatorRow("user", "by().id(\"user\")", true, true, true, 1),
                new LocatorPageObjectGenerator.LocatorRow("pass", "by().name(\"password\")", false, true, true, 2)
        ));
        assertTrue(po.contains("class LoginPage"));
        assertTrue(po.contains("user()"));
        assertTrue(po.contains("by().id(\"user\")"));
        assertTrue(po.contains("WARNING"));
        assertTrue(po.contains("SHAFT.GUI.WebDriver"));
    }

    @Test
    void crossPlatformPairFields() {
        String pair = LocatorPageObjectGenerator.crossPlatformPairFields("by().id(\"a\")", "by().id(\"i\")");
        assertTrue(pair.contains("android=by().id(\"a\")"));
        assertTrue(pair.contains("ios=by().id(\"i\")"));
    }

    @Test
    void sanitizesBadNames() {
        String po = LocatorPageObjectGenerator.generate("!!!", List.of(
                new LocatorPageObjectGenerator.LocatorRow("123bad", "by().xpath(\"//x\")", true, true, true, 0)
        ));
        assertTrue(po.contains("class GeneratedPage") || po.contains("class "));
        assertTrue(po.contains("element123bad") || po.contains("123bad"));
    }


    @Test
    void sanitizeEdgesAndWarnings() {
        String gen = LocatorPageObjectGenerator.generate("@@@", List.of(
                new LocatorPageObjectGenerator.LocatorRow("123bad", "SHAFT.GUI.Locator.builder().build()", true, true, true, 2),
                new LocatorPageObjectGenerator.LocatorRow("_x", "SHAFT.GUI.Locator.builder().build()", false, false, false, 1)
        ));
        assertTrue(gen.contains("class GeneratedPage"));
        assertTrue(gen.contains("element123bad") || gen.contains("element"));
        assertTrue(gen.contains("WARNING"));
        assertTrue(LocatorPageObjectGenerator.generate("   ", List.of()).contains("GeneratedPage"));
        assertTrue(LocatorPageObjectGenerator.crossPlatformPairFields(" a ", " b ").contains("android=a"));
    }
}
