package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;

public class AllureSuiteWidgetReaderTest {
    @Test
    public void base64SuiteIsPresentOnceAndAnEmptyResultsDirectoryStaysUnverified() throws Exception {
        String suite = "CheckoutSuite";
        String encoded = Base64.getEncoder().encodeToString(("{\"name\":\"" + suite + "\"}").getBytes(StandardCharsets.UTF_8));
        Path root = Files.createTempDirectory("shaft-widgets");
        Path results = Files.createDirectories(root.resolve("allure-results"));
        Path empty = Files.createDirectories(root.resolve("empty-results"));
        Path widget = root.resolve("suites.json");
        Files.writeString(widget, "{\"data\":\"" + encoded + "\"}");
        Files.writeString(results.resolve("abc-result.json"), "{}");

        Assert.assertEquals(AllureSuiteWidgetReader.suite(results, widget, suite),
                AllureSuiteWidgetReader.Presence.PRESENT);
        Assert.assertEquals(AllureSuiteWidgetReader.count(widget, suite), 1);
        Assert.assertFalse(Files.readString(widget).contains(suite));

        Path both = root.resolve("both.json");
        Files.writeString(both, suite + "\n" + encoded);
        Assert.assertEquals(AllureSuiteWidgetReader.count(both, suite), 1);

        Assert.assertEquals(AllureSuiteWidgetReader.suite(empty, widget, suite),
                AllureSuiteWidgetReader.Presence.UNVERIFIED);
    }
}
