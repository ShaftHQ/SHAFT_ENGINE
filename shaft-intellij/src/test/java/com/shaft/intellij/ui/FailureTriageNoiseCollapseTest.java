package com.shaft.intellij.ui;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FailureTriageNoiseCollapseTest {
    @Test
    void findsExceptionLine() {
        String log = "SLF4J: noise\njava.lang.AssertionError: boom\n\tat demo.Test.x(Test.java:1)";
        assertEquals("java.lang.AssertionError: boom", FailureTriageNoiseCollapse.firstRealError(log));
    }

    @Test
    void findsAssertionPhrasesAndFallsBack() {
        assertEquals("expected: <1> but was: <2>",
                FailureTriageNoiseCollapse.firstRealError("[INFO] skip\nexpected: <1> but was: <2>"));
        assertEquals("useful", FailureTriageNoiseCollapse.firstRealError("[DEBUG] x\nuseful\n"));
        assertEquals("", FailureTriageNoiseCollapse.firstRealError(null));
        assertEquals("", FailureTriageNoiseCollapse.firstRealError("   "));
        assertEquals("", FailureTriageNoiseCollapse.firstRealError("[INFO] only noise"));
    }

    @Test
    void collapseKeepsFirstErrorAndCapsLines() {
        String log = String.join("\n",
                "Downloading from central",
                "java.lang.RuntimeException: fail",
                "\tat a.A.m(A.java:1)",
                "\tat b.B.m(B.java:2)",
                "\tat c.C.m(C.java:3)",
                "Progress (50%)");
        List<String> kept = FailureTriageNoiseCollapse.collapse(log, 3);
        assertTrue(kept.get(0).startsWith("FIRST_ERROR:"));
        assertTrue(kept.size() <= 3);
        assertTrue(FailureTriageNoiseCollapse.collapse(null, 5).isEmpty());
        assertTrue(FailureTriageNoiseCollapse.collapse("  ", 5).isEmpty());
    }


    @Test
    void coversNoiseAssertionAndCollapseEdges() throws Exception {
        assertEquals("", FailureTriageNoiseCollapse.firstRealError(null));
        assertEquals("", FailureTriageNoiseCollapse.firstRealError("   "));
        assertTrue(FailureTriageNoiseCollapse.collapse(null, 5).isEmpty());
        assertTrue(FailureTriageNoiseCollapse.collapse("  ", 5).isEmpty());
        String noiseOnly = "[INFO] hi\nDownload https://x\nSLF4J: warn";
        assertEquals("", FailureTriageNoiseCollapse.firstRealError(noiseOnly));
        String assertion = "[DEBUG] skip\nExpected: 1 but was: 2\nmore";
        assertTrue(FailureTriageNoiseCollapse.firstRealError(assertion).toLowerCase().contains("expected"));
        String plain = "[INFO] n\n\nactual failure detail\n";
        assertEquals("actual failure detail", FailureTriageNoiseCollapse.firstRealError(plain));
        var collapsed = FailureTriageNoiseCollapse.collapse(
                "java.lang.RuntimeException: boom\n[INFO] noise\nboom\nsecond", 2);
        assertEquals(2, collapsed.size());
        assertTrue(collapsed.get(0).startsWith("FIRST_ERROR:"));
    }
}
