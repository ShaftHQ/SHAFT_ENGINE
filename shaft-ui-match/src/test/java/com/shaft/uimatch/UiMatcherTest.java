package com.shaft.uimatch;

import com.shaft.uimatch.UiMatcher.MatchMode;
import com.shaft.uimatch.UiMatcher.MatchResult;
import com.shaft.uimatch.UiMatcher.UiDocument;
import com.shaft.uimatch.UiMatcher.UiElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Properties;

public class UiMatcherTest {
    @Test
    public void fourModesAndAssertionOverrideUseTheShippedDefault() throws Exception {
        UiDocument expected = document("https://example.test/home", "Login", "digest-a", "#login");
        UiDocument textChanged = document("https://example.test/home", "Sign in", "digest-a", "#login");
        UiDocument imageChanged = document("https://example.test/home", "Login", "digest-b", "#login");
        UiDocument moved = new UiDocument(
                "https://example.test/other",
                List.of(element("Login", "digest-a", 80, 90, "#login")));
        Path json = Files.createTempDirectory("ui-match").resolve("run.json");

        MatchResult strict = UiMatcher.compare(expected, textChanged, MatchMode.STRICT, null, json);
        Assert.assertEquals(strict.confidence() < UiMatcher.DEFAULT_CONFIDENCE, true);
        Assert.assertFalse(strict.matched());
        Assert.assertNull(strict.x());
        Assert.assertTrue(strict.difference().contains("text"));
        Assert.assertTrue(strict.summary().isEmpty());
        String written = Files.readString(json);
        Assert.assertTrue(written.contains("\"mode\":\"STRICT\""));

        MatchResult ignoreText = UiMatcher.compare(
                expected, textChanged, MatchMode.IGNORE_TEXT, null, json);
        Assert.assertTrue(ignoreText.matched());
        Assert.assertEquals(ignoreText.x(), Integer.valueOf(25));
        Assert.assertEquals(ignoreText.y(), Integer.valueOf(40));
        Assert.assertEquals(ignoreText.locatorHint(), "#login");
        Assert.assertFalse(ignoreText.summary().isEmpty());

        MatchResult ignoreImage = UiMatcher.compare(
                expected, imageChanged, MatchMode.IGNORE_IMAGE, null, json);
        Assert.assertTrue(ignoreImage.matched());

        MatchResult layout = UiMatcher.compare(expected, moved, MatchMode.LAYOUT, null, json);
        Assert.assertFalse(layout.matched());

        MatchResult overridden = UiMatcher.compare(
                expected, textChanged, MatchMode.STRICT, UiMatcher.DEFAULT_CONFIDENCE / 2, json);
        Assert.assertTrue(overridden.matched());
    }

    @Test
    public void healConfidenceComesOnlyFromProperties() throws Exception {
        UiDocument expected = document("https://example.test/home", "Login", "digest-a", "#login");
        UiDocument textChanged = document("https://example.test/home", "Sign in", "digest-a", "#login");
        Path json = Files.createTempDirectory("ui-match-heal").resolve("heal.json");
        MatchResult defaults = UiMatcher.heal(expected, textChanged, new Properties(), json);
        Assert.assertFalse(defaults.matched());

        Properties properties = new Properties();
        properties.setProperty(
                UiMatcher.HEAL_CONFIDENCE_PROPERTY,
                Double.toString(UiMatcher.DEFAULT_CONFIDENCE / 2));
        MatchResult lowered = UiMatcher.heal(expected, textChanged, properties, json);
        Assert.assertTrue(lowered.matched());
        Assert.assertEquals(lowered.locatorHint(), "#login");
        Assert.assertTrue(Files.readString(json).contains("\"locatorHint\":\"#login\""));

        Properties invalid = new Properties();
        invalid.setProperty(UiMatcher.HEAL_CONFIDENCE_PROPERTY, "not-a-number");
        Assert.expectThrows(
                IllegalArgumentException.class,
                () -> UiMatcher.heal(expected, textChanged, invalid, json));
    }

    @Test
    public void webDesktopAndMobileDocumentsStayOutsideAnyDriver() throws Exception {
        Path directory = Files.createTempDirectory("ui-match-targets");
        assertTarget("web", "https://example.test/web", directory.resolve("web.json"));
        assertTarget("desktop", "desktop://window", directory.resolve("desktop.json"));
        assertTarget("mobile", "mobile://screen", directory.resolve("mobile.json"));
        String sources = Files.readString(Path.of("src/main/java/com/shaft/uimatch/UiMatcher.java"));
        Assert.assertFalse(sources.contains("selenium"));
        Assert.assertFalse(sources.contains("appium"));
        Assert.assertFalse(sources.contains("opencv"));
    }

    private static void assertTarget(String role, String url, Path json) {
        UiElement element = new UiElement(role, "Submit", "Submit", "digest", 4, 6, 20, 10, role + "-hint");
        UiDocument document = new UiDocument(url, List.of(element));
        MatchResult result = UiMatcher.compare(document, document, MatchMode.STRICT, null, json);
        Assert.assertTrue(result.matched());
        Assert.assertEquals(result.locatorHint(), role + "-hint");
        Assert.assertEquals(result.x(), Integer.valueOf(14));
        Assert.assertEquals(result.y(), Integer.valueOf(11));
    }

    private static UiDocument document(String url, String text, String digest, String hint) {
        return new UiDocument(url, List.of(element(text, digest, 10, 20, hint)));
    }

    private static UiElement element(String text, String digest, int x, int y, String hint) {
        return new UiElement("button", text, "Login", digest, x, y, 30, 40, hint);
    }
}
