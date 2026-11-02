package com.shaft.intellij.properties;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShaftPropertyCatalogTest {
    @Test
    void buildGeneratesTheCatalogFromTheEngineKeyInterfaces() {
        ShaftProperty timeout = ShaftPropertyCatalog.find("browserNavigationTimeout");
        assertNotNull(timeout);
        assertAll(
                () -> assertTrue(ShaftPropertyCatalog.all().size() > 100),
                () -> assertEquals("int", timeout.type()),
                () -> assertEquals("30", timeout.defaultValue()),
                () -> assertEquals("Timeouts", timeout.group()),
                () -> assertTrue(timeout.description().startsWith("Timeout in seconds for browser navigation"),
                        timeout.description()),
                () -> assertEquals("Boolean", ShaftPropertyCatalog.find("waitForLazyLoading").type()));
    }

    @Test
    void suggestsTheNearestKeyOnlyForLikelyTypos() {
        assertAll(
                () -> assertEquals("browserNavigationTimeout", ShaftPropertyCatalog.suggest("browserNavigatonTimeout")),
                () -> assertEquals("waitForLazyLoading", ShaftPropertyCatalog.suggest("WaitForLazyLoading")),
                () -> assertNull(ShaftPropertyCatalog.suggest("myCompanyCheckoutBaseUrl")));
    }

    @Test
    void validatesValuesAgainstTheDeclaredType() {
        ShaftProperty flag = new ShaftProperty("k", "boolean", "true", "", "G");
        ShaftProperty count = new ShaftProperty("k", "int", "1", "", "G");
        ShaftProperty ratio = new ShaftProperty("k", "double", "0.1", "", "G");
        assertAll(
                () -> assertNull(ShaftPropertyCatalog.valueProblem(flag, "TRUE")),
                () -> assertEquals("Expected true or false", ShaftPropertyCatalog.valueProblem(flag, "yes")),
                () -> assertNull(ShaftPropertyCatalog.valueProblem(count, " 15 ")),
                () -> assertEquals("Expected a whole number", ShaftPropertyCatalog.valueProblem(count, "15s")),
                () -> assertNull(ShaftPropertyCatalog.valueProblem(ratio, "0.25")),
                () -> assertEquals("Expected a number", ShaftPropertyCatalog.valueProblem(ratio, "a")),
                () -> assertNull(ShaftPropertyCatalog.valueProblem(count, "${env.COUNT}")),
                () -> assertNull(ShaftPropertyCatalog.valueProblem(new ShaftProperty("k", "String", "", "", "G"), "x")));
    }

    @Test
    void parseSkipsMalformedRowsAndKeepsTheFirstDeclaration() {
        var parsed = ShaftPropertyCatalog.parse(List.of("a\tint\t1\tFirst\tG", "broken", "a\tint\t2\tSecond\tH"));
        assertEquals(1, parsed.size());
        assertEquals("First", parsed.get("a").description());
    }
}
