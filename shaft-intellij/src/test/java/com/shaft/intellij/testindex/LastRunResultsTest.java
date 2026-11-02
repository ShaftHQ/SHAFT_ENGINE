package com.shaft.intellij.testindex;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LastRunResultsTest {
    @TempDir
    Path project;

    @Test
    void keepsNewestResultPerMethodWithDurationAndFailureFrame() throws Exception {
        Path results = Files.createDirectories(project.resolve("target/allure-results"));
        Files.writeString(results.resolve("a-result.json"), """
                {"fullName":"demo.LoginTest.signIn","status":"passed","start":1,"stop":5}
                """);
        Files.writeString(results.resolve("b-result.json"), """
                {"fullName":"demo.LoginTest.signIn","status":"failed","start":10,"stop":1260,
                 "statusDetails":{"message":"boom","trace":"java.lang.AssertionError: boom\\n\\tat org.testng.Assert.fail(Assert.java:1)\\n\\tat demo.LoginTest.signIn(LoginTest.java:42)\\n"}}
                """);
        Files.writeString(results.resolve("broken-result.json"), "{not json");

        LastRunResults.Result result = LastRunResults.read(project).get("demo.LoginTest#signIn");

        assertEquals("failed", result.status());
        assertEquals(1250L, result.durationMillis());
        assertEquals(new LastRunResults.Frame("demo.LoginTest", "signIn", 42), result.frame());
        assertEquals(List.of(result), LastRunResults.failures(project));
        assertEquals("✗ failed · 1.3 s", result.label());
        assertEquals("boom", result.message());
        assertEquals("✗ boom", result.failureHint());
    }

    @Test
    void failuresAtKeysTheNewestFailurePerLineOfOneClass() throws Exception {
        Path results = Files.createDirectories(project.resolve("allure-results"));
        Files.writeString(results.resolve("a-result.json"), """
                {"fullName":"demo.CartTest.add","status":"failed","start":1,"stop":2,
                 "statusDetails":{"message":"old","trace":"\\tat demo.CartTest.add(CartTest.java:12)"}}
                """);
        Files.writeString(results.resolve("b-result.json"), """
                {"fullName":"demo.CartTest.remove","status":"broken","start":5,"stop":6,
                 "statusDetails":{"message":"expected 42\\nbut found 41","trace":"\\tat demo.CartTest.remove(CartTest.java:12)"}}
                """);
        Files.writeString(results.resolve("c-result.json"), """
                {"fullName":"demo.OtherTest.go","status":"failed","start":7,"stop":8,
                 "statusDetails":{"trace":"\\tat demo.OtherTest.go(OtherTest.java:3)"}}
                """);

        var byLine = LastRunResults.failuresAt(LastRunResults.read(project), "demo.CartTest");

        assertEquals(java.util.Set.of(12), byLine.keySet());
        assertEquals("demo.CartTest.remove", byLine.get(12).fullName());
        assertEquals("✗ expected 42", byLine.get(12).failureHint(), "hint keeps the first message line");
        assertEquals("✗ failed", LastRunResults.read(project).get("demo.OtherTest#go").failureHint());
    }

    @Test
    void longFailureHintsAreCapped() {
        var result = new LastRunResults.Result("demo.A.b", "failed", 0, 0, null, "x".repeat(500));
        assertEquals(LastRunResults.HINT_LIMIT + 2, result.failureHint().length());
        assertTrue(result.failureHint().endsWith("…"));
    }

    @Test
    void missingResultsAreEmpty() {
        assertTrue(LastRunResults.read(project).isEmpty());
        assertTrue(LastRunResults.failures(project).isEmpty());
    }

    @Test
    void reusesParsedResultsUntilTheResultFolderChanges() throws Exception {
        Path results = Files.createDirectories(project.resolve("allure-results"));
        Files.writeString(results.resolve("a-result.json"), """
                {"fullName":"demo.CartTest.add","status":"passed","start":1,"stop":5}
                """);

        var first = LastRunResults.read(project);
        assertSame(first, LastRunResults.read(project), "unchanged folder must not be re-parsed (#6635)");

        Files.writeString(results.resolve("b-result.json"), """
                {"fullName":"demo.CartTest.remove","status":"failed","start":2,"stop":9}
                """);
        var second = LastRunResults.read(project);
        assertEquals("failed", second.get("demo.CartTest#remove").status());
        assertEquals("passed", second.get("demo.CartTest#add").status());
    }
}
