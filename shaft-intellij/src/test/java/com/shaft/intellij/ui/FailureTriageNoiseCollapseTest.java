package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FailureTriageNoiseCollapseTest {
    @Test
    void highlightsFirstRealError() {
        String log = """
                [INFO] Downloading from central
                SLF4J: Class path contains multiple SLF4J providers.
                java.lang.AssertionError: expected:<1> but was:<2>
                    at FooTest.bar(FooTest.java:10)
                """;
        assertEquals("java.lang.AssertionError: expected:<1> but was:<2>",
                FailureTriageNoiseCollapse.firstRealError(log));
        assertTrue(FailureTriageNoiseCollapse.collapse(log, 5).get(0).startsWith("FIRST_ERROR:"));
    }
}
