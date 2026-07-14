package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

public class AssertionEvidenceReporterTest {

    @Test(description = "Passed scalar validations should render a PASSED assertion card with both raw values")
    public void passedScalarShouldRenderAssertionCard() {
        String html = AssertionEvidenceReporter.renderCard("Assert", true, "expectedValue", "expectedValue");

        Assert.assertTrue(html.contains("PASSED"), html);
        Assert.assertTrue(html.contains("Assertion"), html);
        Assert.assertTrue(html.contains("expectedValue"), html);
        Assert.assertTrue(html.contains("<!doctype html>"), html);
    }

    @Test(description = "Failed scalar soft-validations should render a FAILED verification card")
    public void failedScalarShouldRenderVerificationCard() {
        String html = AssertionEvidenceReporter.renderCard("Verify", false, "expected", "actual");

        Assert.assertTrue(html.contains("FAILED"), html);
        Assert.assertTrue(html.contains("Verification"), html);
        Assert.assertTrue(html.contains("expected"), html);
        Assert.assertTrue(html.contains("actual"), html);
    }

    @Test(description = "Both sides parsing as JSON should surface a JSON data-shape badge, pretty-printed content, and a diff region")
    public void jsonBothSidesShouldShowJsonShapeAndDiff() {
        String expectedJson = "{\"id\":1,\"name\":\"Ann\"}";
        String actualJson = "{\"id\":1,\"name\":\"Bob\"}";

        String html = AssertionEvidenceReporter.renderCard("Assert", false, expectedJson, actualJson);

        Assert.assertTrue(html.contains(">JSON<"), html);
        // Jackson pretty-printing indents nested fields onto their own lines; quotes are HTML-escaped.
        Assert.assertTrue(html.contains("&quot;id&quot;"), html);
        Assert.assertTrue(html.contains("aer-diff"), html);
        Assert.assertTrue(html.contains("Ann"), html);
        Assert.assertTrue(html.contains("Bob"), html);
    }

    @Test(description = "Multi-line text values should surface a Text data-shape badge and a diff region")
    public void multiLineTextShouldShowTextShapeAndDiff() {
        String expectedText = "line one\nline two\nline three";
        String actualText = "line one\nline TWO\nline three";

        String html = AssertionEvidenceReporter.renderCard("Assert", false, expectedText, actualText);

        Assert.assertTrue(html.contains(">Text<"), html);
        Assert.assertTrue(html.contains("aer-diff"), html);
        Assert.assertTrue(html.contains("line one"), html);
    }

    @Test(description = "Secrets in expected/actual must be redacted before the card renders (password assignment)")
    public void passwordSecretShouldBeRedacted() {
        String html = AssertionEvidenceReporter.renderCard("Assert", false, "password=hunter2", "password=different");

        Assert.assertFalse(html.contains("hunter2"), html);
        Assert.assertTrue(html.contains("********"), html);
    }

    @Test(description = "Secrets embedded in JSON token fields must be redacted before the card renders")
    public void jsonTokenSecretShouldBeRedacted() {
        String html = AssertionEvidenceReporter.renderCard("Assert", false,
                "{\"token\":\"abcdef123456\"}", "{\"token\":\"abcdef123456\"}");

        Assert.assertFalse(html.contains("abcdef123456"), html);
        Assert.assertTrue(html.contains("********"), html);
    }

    @Test(description = "Authorization header secrets must be redacted before the card renders")
    public void authorizationSecretShouldBeRedacted() {
        String html = AssertionEvidenceReporter.renderCard("Assert", false,
                "Authorization: Bearer xyz", "Authorization: Bearer xyz");

        Assert.assertFalse(html.contains("xyz"), html);
        Assert.assertTrue(html.contains("********"), html);
    }

    @Test(description = "Actual values containing HTML/script markup must be escaped, never left live")
    public void htmlInActualShouldBeEscaped() {
        String html = AssertionEvidenceReporter.renderCard("Assert", false, "safeExpected", "<script>alert(1)</script>");

        Assert.assertTrue(html.contains("&lt;script&gt;"), html);
        Assert.assertFalse(html.contains("<script>alert(1)</script>"), html);
    }

    @Test(description = "Both null or blank expected/actual should skip rendering entirely")
    public void bothBlankOrNullShouldReturnEmptyString() {
        Assert.assertEquals(AssertionEvidenceReporter.renderCard("Assert", true, null, null), "");
        Assert.assertEquals(AssertionEvidenceReporter.renderCard("Assert", true, null, ""), "");
        Assert.assertEquals(AssertionEvidenceReporter.renderCard("Assert", true, "   ", ""), "");
    }

    @Test(description = "A pure pass/fail boolean pair (element-state / visual validations) carries no comparable content and must skip the card")
    public void booleanFlagPairShouldSkipRendering() {
        Assert.assertEquals(AssertionEvidenceReporter.renderCard("Assert", false, true, false), "");
        Assert.assertEquals(AssertionEvidenceReporter.renderCard("Assert", true,
                new java.util.concurrent.atomic.AtomicBoolean(true),
                new java.util.concurrent.atomic.AtomicBoolean(false)), "");
        // A boolean compared against real content still renders (only a boolean *pair* is skipped).
        Assert.assertFalse(AssertionEvidenceReporter.renderCard("Assert", false, true, "unexpected text").isEmpty());
    }

    @Test(description = "Oversized values must be capped with a visible truncation marker instead of rendering unbounded HTML")
    public void oversizedActualShouldBeTruncated() {
        String hugeActual = "x".repeat(50_000);

        String html = AssertionEvidenceReporter.renderCard("Assert", false, "short", hugeActual);

        Assert.assertTrue(html.contains("truncated"), html);
        Assert.assertTrue(html.length() < 200_000, "Card HTML should stay bounded even for a 50k-char value: " + html.length());
    }

    @Test(description = "Malformed JSON-looking input must never throw and should degrade to a scalar/text layout")
    public void malformedJsonLikeInputShouldNeverThrow() {
        String html = AssertionEvidenceReporter.renderCard("Assert", false,
                "{not valid json", "{\"also\": not valid}");

        Assert.assertNotNull(html);
        Assert.assertFalse(html.contains(">JSON<"), html);
    }

    @Test(description = "renderCard should never throw regardless of exotic input, including empty category labels")
    public void rendererShouldNeverThrowOnExoticInput() {
        Assert.assertNotNull(AssertionEvidenceReporter.renderCard(null, true, "a", "b"));
        Assert.assertNotNull(AssertionEvidenceReporter.renderCard("", false, 42, 3.14));
        Assert.assertNotNull(AssertionEvidenceReporter.renderCard("Assert", true, new Object(), new Object()));
    }

    @Test(description = "Accessibility card renders a domain-labelled aria-YAML diff even though the outcome is a boolean (#3532 E)")
    public void accessibilityCardRendersAriaYamlDiff() {
        String baselineYaml = "- textbox \"Email\"\n- button \"Submit\"";
        String actualYaml = "- textbox \"Email\"\n- button \"Send\"";

        String html = AssertionEvidenceReporter.renderAccessibilityCard(false, baselineYaml, actualYaml);

        // Domain-consistent header, real FAILED status, and a rendered line diff — not a raw dump.
        Assert.assertTrue(html.contains("Accessibility"), html);
        Assert.assertTrue(html.contains("FAILED"), html);
        Assert.assertTrue(html.contains(">Text<"), html);
        Assert.assertTrue(html.contains("aer-diff"), html);
        Assert.assertTrue(html.contains("Submit"), html);
        Assert.assertTrue(html.contains("Send"), html);
        Assert.assertTrue(html.contains("<!doctype html>"), html);
    }

    @Test(description = "Accessibility card still redacts secrets and skips rendering when there is no aria YAML (#3532 E)")
    public void accessibilityCardRedactsAndSkipsBlank() {
        // Secrets embedded in an aria label must be redacted like any other card content.
        String withSecret = AssertionEvidenceReporter.renderAccessibilityCard(false,
                "- textbox \"password=hunter2\"", "- textbox \"password=different\"");
        Assert.assertFalse(withSecret.contains("hunter2"), withSecret);
        Assert.assertTrue(withSecret.contains("********"), withSecret);

        // Nothing to diff -> no card (mirrors renderCard's blank contract).
        Assert.assertEquals(AssertionEvidenceReporter.renderAccessibilityCard(true, null, ""), "");
        Assert.assertEquals(AssertionEvidenceReporter.renderAccessibilityCard(true, "  ", null), "");
    }

    @Test(description = "Writes a sample accessibility aria-diff card to the scratch directory for manual visual review (#3532 E)")
    public void writeSampleAccessibilityCardForManualReview() throws Exception {
        String baselineYaml = "- heading \"Checkout\" [level=1]\n- textbox \"Email\"\n- textbox \"Card number\"\n- button \"Pay now\"";
        String actualYaml = "- heading \"Checkout\" [level=2]\n- textbox \"Email\"\n- button \"Pay now\"";

        String html = AssertionEvidenceReporter.renderAccessibilityCard(false, baselineYaml, actualYaml);

        Path outputFile = Path.of(System.getProperty("shaft.accessibilityEvidence.sampleOutput",
                "C:\\Users\\Mohab\\AppData\\Local\\Temp\\claude\\C--Users-Mohab-IdeaProjects-SHAFT-ENGINE\\a0966654-661a-46d0-8c81-75ecf6570c15\\scratchpad\\accessibility-card-sample.html"));
        Files.createDirectories(outputFile.getParent());
        Files.writeString(outputFile, html, StandardCharsets.UTF_8);

        Assert.assertTrue(Files.exists(outputFile));
        Assert.assertTrue(html.contains("Accessibility"), html);
        Assert.assertTrue(html.contains("FAILED"), html);
        Assert.assertTrue(html.contains("aer-diff"), html);
    }

    @Test(description = "Writes a sample failed-JSON-diff card to the scratch directory for manual visual review")
    public void writeSampleFailedJsonDiffCardForManualReview() throws Exception {
        String expectedJson = "{\n  \"orderId\": 1042,\n  \"status\": \"CONFIRMED\",\n  \"items\": [\"sku-1\", \"sku-2\"],\n  \"customer\": {\n    \"name\": \"Ann Example\",\n    \"token\": \"abcdef123456\"\n  }\n}";
        String actualJson = "{\n  \"orderId\": 1042,\n  \"status\": \"PENDING\",\n  \"items\": [\"sku-1\"],\n  \"customer\": {\n    \"name\": \"Ann Example\",\n    \"token\": \"abcdef123456\"\n  }\n}";

        String html = AssertionEvidenceReporter.renderCard("Assert", false, expectedJson, actualJson);

        Path outputFile = Path.of(System.getProperty("shaft.assertionEvidence.sampleOutput",
                "C:\\Users\\Mohab\\AppData\\Local\\Temp\\claude\\C--Users-Mohab-IdeaProjects-SHAFT-ENGINE\\f31b7827-7767-4eff-9dfa-7c3f79d2df84\\scratchpad\\assertion-card-sample.html"));
        Files.createDirectories(outputFile.getParent());
        Files.writeString(outputFile, html, StandardCharsets.UTF_8);

        Assert.assertTrue(Files.exists(outputFile));
        Assert.assertTrue(html.contains("FAILED"), html);
        Assert.assertTrue(html.contains(">JSON<"), html);
        Assert.assertFalse(html.contains("abcdef123456"), html);
    }
}
