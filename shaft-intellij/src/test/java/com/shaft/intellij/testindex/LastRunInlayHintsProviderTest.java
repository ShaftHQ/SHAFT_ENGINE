package com.shaft.intellij.testindex;

import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

class LastRunInlayHintsProviderTest {
    private static LastRunResults.Result failure(String fullName, String className, int line, String message) {
        return new LastRunResults.Result(fullName, "failed", 1, 1,
                new LastRunResults.Frame(className, "m", line), message);
    }

    @Test
    void failureHintsSitOnTheZeroBasedFailingLineOfTheMatchingClassOnly() {
        Map<String, LastRunResults.Result> results = Map.of(
                "demo.CartTest.add", failure("demo.CartTest.add", "demo.CartTest", 12, "expected 42 but found 41"),
                "demo.CartTest.Inner.go", failure("demo.CartTest.Inner.go", "demo.CartTest$Inner", 30, "inner"),
                "demo.OtherTest.go", failure("demo.OtherTest.go", "demo.OtherTest", 3, "other"),
                "demo.CartTest.gone", failure("demo.CartTest.gone", "demo.CartTest", 99, "stale line"));

        assertEquals(Map.of(11, "✗ expected 42 but found 41"),
                LastRunInlayHintsProvider.failureHints(results, "demo.CartTest", 50));
        assertEquals(Map.of(29, "✗ inner"),
                LastRunInlayHintsProvider.failureHints(results, "demo.CartTest.Inner", 50));
    }
}
