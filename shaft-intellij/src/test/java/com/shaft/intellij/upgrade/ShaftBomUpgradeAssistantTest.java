package com.shaft.intellij.upgrade;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftBomUpgradeAssistantTest {
    @Test
    void detectsPropertyAndSuggestsBump() {
        String pom = """
                <project>
                  <properties><shaft.engine.version>8.4.20240101</shaft.engine.version></properties>
                  <dependency>
                    <groupId>io.github.shafthq</groupId>
                    <artifactId>shaft</artifactId>
                    <version>${shaft.engine.version}</version>
                  </dependency>
                </project>
                """;
        assertEquals("8.4.20240101", ShaftBomUpgradeAssistant.detectShaftVersion(pom).orElseThrow());
        var suggestion = ShaftBomUpgradeAssistant.suggest(pom, "9.0.20261103", "Retry defaults changed.").orElseThrow();
        assertTrue(suggestion.bumpRecommended());
        assertTrue(suggestion.summary().contains("8.4.20240101 → 9.0.20261103"));
        String bumped = ShaftBomUpgradeAssistant.applyBump(pom, "9.0.20261103");
        assertTrue(bumped.contains("<shaft.engine.version>9.0.20261103</shaft.engine.version>"));
    }

    @Test
    void comparesVersions() {
        assertTrue(ShaftBomUpgradeAssistant.compareVersions("9.0.1", "9.0.0") > 0);
        assertEquals(0, ShaftBomUpgradeAssistant.compareVersions("1.2.3", "1.2.3"));
    }
}
