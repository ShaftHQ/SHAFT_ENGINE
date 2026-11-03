package com.shaft.intellij.upgrade;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftBomUpgradeAssistantTest {
    @Test
    void detectsPropertyAndSuggestsBump() {
        String pom = """
                <project>
                  <properties><shaft.engine.version>9.4.0</shaft.engine.version></properties>
                  <dependency><groupId>io.github.shafthq</groupId><artifactId>shaft-engine</artifactId></dependency>
                </project>
                """;
        assertEquals("9.4.0", ShaftBomUpgradeAssistant.detectShaftVersion(pom).orElseThrow());
        var sug = ShaftBomUpgradeAssistant.suggest(pom, "9.5.0", "healer fixes");
        assertTrue(sug.orElseThrow().bumpRecommended());
        assertTrue(sug.orElseThrow().summary().contains("9.4.0 → 9.5.0"));
        assertTrue(ShaftBomUpgradeAssistant.applyBump(pom, "9.5.0").contains("9.5.0"));
    }

    @Test
    void detectsInlineArtifactVersion() {
        String pom = """
                <project>
                  <dependency>
                    <groupId>io.github.shafthq</groupId>
                    <artifactId>shaft-engine</artifactId>
                    <version>8.1.2</version>
                  </dependency>
                </project>
                """;
        assertEquals("8.1.2", ShaftBomUpgradeAssistant.detectShaftVersion(pom).orElseThrow());
        assertTrue(ShaftBomUpgradeAssistant.applyBump(pom, "8.2.0").contains("<version>8.2.0</version>"));
    }

    @Test
    void ignoresPropertyPlaceholderAndBlank() {
        String pom = """
                <project>
                  <dependency>
                    <groupId>io.github.shafthq</groupId>
                    <artifactId>shaft-engine</artifactId>
                    <version>${shaft.engine.version}</version>
                  </dependency>
                </project>
                """;
        assertTrue(ShaftBomUpgradeAssistant.detectShaftVersion(pom).isEmpty());
        assertTrue(ShaftBomUpgradeAssistant.detectShaftVersion(null).isEmpty());
        assertTrue(ShaftBomUpgradeAssistant.detectShaftVersion("no shaft here").isEmpty());
        assertTrue(ShaftBomUpgradeAssistant.suggest("nope", "9.0.0", null).isEmpty());
        assertEquals("plain", ShaftBomUpgradeAssistant.applyBump("plain", "9.0.0"));
    }

    @Test
    void compareVersionsAndAlreadyLatest() {
        assertTrue(ShaftBomUpgradeAssistant.compareVersions("9.5.0", "9.4.0") > 0);
        assertTrue(ShaftBomUpgradeAssistant.compareVersions("9.4.0", "9.5.0") < 0);
        assertEquals(0, ShaftBomUpgradeAssistant.compareVersions("9.4.0", "9.4.0"));
        assertTrue(ShaftBomUpgradeAssistant.compareVersions("9.4.0-SNAPSHOT", "9.3.9") > 0);
        String pom = "<properties><shaftVersion>9.5.0</shaftVersion></properties><g>io.github.shafthq</g>";
        // need io.github.shafthq mention
        pom = "<project><properties><shaftVersion>9.5.0</shaftVersion></properties>"
                + "<dependency><groupId>io.github.shafthq</groupId><artifactId>shaft</artifactId></dependency></project>";
        var sug = ShaftBomUpgradeAssistant.suggest(pom, "9.5.0", "   ");
        assertFalse(sug.orElseThrow().bumpRecommended());
        assertTrue(sug.orElseThrow().summary().contains("Already on SHAFT"));
        var withDefaultDiff = ShaftBomUpgradeAssistant.suggest(pom.replace("9.5.0", "9.0.0"), "9.5.0", null);
        assertTrue(withDefaultDiff.orElseThrow().summary().contains("changelog"));
    }


    @Test
    void unevenVersionPartsAndNonNumeric() {
        assertTrue(ShaftBomUpgradeAssistant.compareVersions("10", "9.9.9") > 0);
        assertTrue(ShaftBomUpgradeAssistant.compareVersions("1.2.3", "1.2") > 0);
        assertEquals(0, ShaftBomUpgradeAssistant.compareVersions("1.x", "1.0"));
        assertEquals(0, ShaftBomUpgradeAssistant.compareVersions("abc", "0"));
        assertTrue(ShaftBomUpgradeAssistant.compareVersions("2rc1", "1") > 0);
    }

}
