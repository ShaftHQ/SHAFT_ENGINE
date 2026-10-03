package com.shaft.intellij.testindex;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
    }

    @Test
    void missingResultsAreEmpty() {
        assertTrue(LastRunResults.read(project).isEmpty());
        assertTrue(LastRunResults.failures(project).isEmpty());
    }
}
