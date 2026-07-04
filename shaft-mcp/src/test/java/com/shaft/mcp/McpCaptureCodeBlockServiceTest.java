package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerationReport;
import com.shaft.capture.model.CaptureReadiness;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpCaptureCodeBlockServiceTest {
    @TempDir
    Path temp;

    @Test
    void fromGeneratedSourceAddsPomLocatorAndActionBlocksWithoutChatGuidance() throws Exception {
        Path source = writeSource("""
                package generated.capture;

                import com.shaft.driver.SHAFT;
                import org.testng.annotations.Test;

                public class LoginReplayTest {
                    private SHAFT.GUI.WebDriver driver;

                    @Test
                    public void replayLogin() {
                        driver.element().click(SHAFT.GUI.Locator.inputField("Username"));
                        driver.element().type(SHAFT.GUI.Locator.inputField("Username"), requiredData("username"));
                        // Recorded checkpoint checkpoint-1 (ASSERTION). Login verified
                        driver.element().assertThat(SHAFT.GUI.Locator.clickableField("Sign In")).isVisible().perform();
                    }
                }
                """);

        List<McpCodeBlock> blocks = new McpCaptureCodeBlockService()
                .fromGeneratedSource(source, "browser");

        assertEquals(List.of(
                "capture-full-class",
                "capture-test-method",
                "capture-pom-locator-inventory",
                "capture-pom-action-sequence",
                "capture-page-object-draft"
                ), blocks.stream().map(McpCodeBlock::id).toList());

        McpCodeBlock locators = block(blocks, "capture-pom-locator-inventory");
        assertEquals(McpCodeBlock.Kind.LOCATOR, locators.kind());
        assertTrue(locators.code().contains(
                "// usernameLocator -> SHAFT.GUI.Locator.inputField(\"Username\")"));
        assertTrue(locators.code().contains(
                "// signInLocator -> SHAFT.GUI.Locator.clickableField(\"Sign In\")"));
        assertTrue(locators.placement().contains("SHAFT locator expressions"));

        McpCodeBlock actions = block(blocks, "capture-pom-action-sequence");
        assertEquals(McpCodeBlock.Kind.ACTION, actions.kind());
        assertTrue(actions.code().contains("// Flow: replayLogin"));
        assertTrue(actions.code().contains("browser.element().click(SHAFT.GUI.Locator.inputField(\"Username\"));"));
        assertTrue(actions.code().contains("// Checkpoint: checkpoint-1 (ASSERTION). Login verified"));
        assertTrue(actions.placement().contains("page methods"));

        McpCodeBlock pageObject = block(blocks, "capture-page-object-draft");
        assertEquals(McpCodeBlock.Kind.ACTION, pageObject.kind());
        assertTrue(pageObject.code().contains("public final class LoginPage"));
        assertTrue(pageObject.code().contains("private final SHAFT.GUI.WebDriver browser;"));
        assertTrue(pageObject.code().contains("private final By usernameLocator"));
        assertTrue(pageObject.code().contains("public LoginPage replayLogin()"));

        assertFalse(blocks.stream().anyMatch(item -> item.kind() == McpCodeBlock.Kind.PROVIDER_ADVISORY));
    }

    @Test
    void fromGeneratedSourceAddsRecordAtTargetBlocksWhenTargetIsProvided() throws Exception {
        Path source = writeSource("""
                package generated.capture;

                import com.shaft.driver.SHAFT;
                import org.testng.annotations.Test;

                public class LoginReplayTest {
                    private SHAFT.GUI.WebDriver driver;

                    @Test
                    public void replayLogin() {
                        driver.element().click(SHAFT.GUI.Locator.inputField("Username"));
                        driver.element().type(SHAFT.GUI.Locator.inputField("Username"), requiredData("username"));
                    }
                }
                """);
        Path target = temp.resolve("CheckoutTest.java");
        Files.writeString(target, """
                package tests;

                import com.shaft.driver.SHAFT;

                public class CheckoutTest {
                    private SHAFT.GUI.WebDriver browser;

                    public void replayCheckout() {
                        browser.browser().navigateToURL("https://example.test");
                    }
                }
                """);

        List<McpCodeBlock> blocks = new McpCaptureCodeBlockService()
                .fromGeneratedSource(source, "browser", null, target, "replayCheckout");

        McpCodeBlock locators = block(blocks, "capture-target-locator-fields");
        assertEquals(McpCodeBlock.Kind.LOCATOR, locators.kind());
        assertTrue(locators.code().contains("private final By usernameLocator"));

        McpCodeBlock actions = block(blocks, "capture-target-action-snippet");
        assertEquals(McpCodeBlock.Kind.ACTION, actions.kind());
        assertTrue(actions.code().contains("browser.element().click(usernameLocator);"));
        assertTrue(actions.code().contains("browser.element().type(usernameLocator, requiredData(\"username\"));"));
        assertTrue(actions.placement().contains("after replayCheckout"));
        assertTrue(actions.warnings().isEmpty(), actions.warnings().toString());

        assertTrue(blocks.stream().anyMatch(item -> item.id().equals("capture-target-patch-preview")));
    }

    @Test
    void fromGeneratedSourceUsesPlaywrightDriverTypeInPomGuidance() throws Exception {
        Path source = writeSource("""
                package generated.capture;

                import com.shaft.driver.SHAFT;
                import org.testng.annotations.Test;

                public class PlaywrightReplayTest {
                    private SHAFT.GUI.Playwright driver;

                    @Test
                    public void replaySearch() {
                        driver.element().click(SHAFT.GUI.Locator.inputField("Search"));
                    }
                }
                """);

        List<McpCodeBlock> blocks = new McpCaptureCodeBlockService()
                .fromGeneratedSource(source, "page");

        assertTrue(block(blocks, "capture-test-method").placement().contains("SHAFT.GUI.Playwright"));
    }

    @Test
    void pageObjectDraftKeepsClassNameValidWhenReplayFlowStartsWithDigit() throws Exception {
        Path source = writeSource("""
                package generated.capture;

                import com.shaft.driver.SHAFT;
                import org.testng.annotations.Test;

                public class TwoFactorReplayTest {
                    private SHAFT.GUI.WebDriver driver;

                    @Test
                    public void replay2FactorLogin() {
                        driver.element().click(SHAFT.GUI.Locator.inputField("Code"));
                    }
                }
                """);

        List<McpCodeBlock> blocks = new McpCaptureCodeBlockService()
                .fromGeneratedSource(source, "browser");

        McpCodeBlock pageObject = block(blocks, "capture-page-object-draft");
        assertTrue(pageObject.code().contains("public final class CapturedPage"));
        assertTrue(pageObject.code().contains("public CapturedPage replay2FactorLogin()"));
    }

    @Test
    void fromGeneratedSourceUsesReportLocatorMetadataWhenSourceHasNoConstants() throws Exception {
        Path source = writeSource("""
                package generated.capture;

                import org.testng.annotations.Test;

                public class InlineLocatorReplayTest {
                    @Test
                    public void replayInlineLocator() {
                        driver.element().click(SHAFT.GUI.Locator.inputField("Username"));
                    }
                }
                """);
        CaptureGenerationReport report = new CaptureGenerationReport(
                CaptureGenerationReport.CURRENT_SCHEMA_VERSION,
                "session-1",
                CaptureGenerationReport.Status.SUCCESS,
                "src/test/java/generated/capture/InlineLocatorReplayTest.java",
                "",
                List.of(new CaptureGenerationReport.LocatorDecision(
                        List.of("event-2"),
                        "username-input",
                        "ROLE",
                        "Username",
                        240,
                        List.of("role=120"),
                        List.of("css=#username"))),
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                CaptureGenerationReport.Validation.skipped("Compilation was not requested."),
                CaptureGenerationReport.Validation.skipped("Replay was not requested."),
                CaptureGenerationReport.Enrichment.notRequested());

        List<McpCodeBlock> blocks = new McpCaptureCodeBlockService()
                .fromGeneratedSource(source, "driver", report);

        McpCodeBlock locators = block(blocks, "capture-pom-locator-inventory");
        assertTrue(locators.code().contains("usernameInputLocator"));
        assertTrue(locators.code().contains("ROLE"));
        assertTrue(locators.code().contains("Username"));
        assertTrue(locators.evidenceIds().contains("event-2"));
    }

    @Test
    void fromGeneratedSourceAddsSetupAssertionAndControlFlowReviewBlocksFromReport() throws Exception {
        Path source = writeSource("""
                package generated.capture;

                import org.testng.annotations.Test;

                public class InlineLocatorReplayTest {
                    @Test
                    public void replayInlineLocator() {
                        driver.element().click(SHAFT.GUI.Locator.inputField("Username"));
                    }
                }
                """);
        CaptureGenerationReport report = new CaptureGenerationReport(
                CaptureGenerationReport.CURRENT_SCHEMA_VERSION,
                "session-1",
                CaptureGenerationReport.Status.SUCCESS,
                "src/test/java/generated/capture/InlineLocatorReplayTest.java",
                "src/test/resources/InlineLocatorReplayTest.json",
                CaptureReadiness.State.RISKY,
                List.of("event-4: No assertion follows a navigation or form-submission action."),
                List.of(new CaptureGenerationReport.LocatorDecision(
                        List.of("event-2"),
                        "username-input",
                        "ROLE",
                        "Username",
                        240,
                        List.of("role=120"),
                        List.of("CSS #username (score 210)"))),
                List.of(),
                List.of(),
                List.of(),
                List.of(new CaptureGenerationReport.ControlFlowSuggestion(
                        "control-flow-optional-guard-event-2",
                        CaptureGenerationReport.ControlFlowKind.OPTIONAL_GUARD,
                        List.of("event-2"),
                        "Cookie banner may be optional.",
                        "Preview before applying the optional guard.",
                        false)),
                List.of("data.password: set environment variable SHAFT_CAPTURE_DATA_PASSWORD before replay."),
                List.of("review/ASSERTION/WARNING event-4: Missing final assertion. Recommendation: Assert destination."),
                CaptureGenerationReport.Validation.skipped("Compilation was not requested."),
                CaptureGenerationReport.Validation.skipped("Replay was not requested."),
                CaptureGenerationReport.Enrichment.notRequested());

        List<McpCodeBlock> blocks = new McpCaptureCodeBlockService()
                .fromGeneratedSource(source, "browser", report);

        assertTrue(blocks.stream().map(McpCodeBlock::id).toList().containsAll(List.of(
                "capture-data-setup",
                "capture-assertion-suggestions",
                "capture-control-flow-review")));
        assertTrue(block(blocks, "capture-pom-locator-inventory").code()
                .contains("// alternative -> SHAFT.GUI.Locator.cssSelector(\"#username\")"));
        assertTrue(block(blocks, "capture-data-setup").code()
                .contains("SHAFT_CAPTURE_DATA_PASSWORD"));
        assertTrue(block(blocks, "capture-assertion-suggestions").code()
                .contains("browser.browser().assertThat().url()"));
        assertTrue(block(blocks, "capture-control-flow-review").code()
                .contains("--control-flow-preview"));
    }

    @Test
    void fromGeneratedSourceAddsLocatorRiskAndValidationReviewBlocksFromReport() throws Exception {
        Path source = writeSource("""
                package generated.capture;

                import org.testng.annotations.Test;

                public class RiskyReplayTest {
                    @Test
                    public void replayRiskyLocator() {
                        driver.element().click(SHAFT.GUI.Locator.xpath("/html/body/div[2]/button"));
                    }
                }
                """);
        CaptureGenerationReport report = new CaptureGenerationReport(
                CaptureGenerationReport.CURRENT_SCHEMA_VERSION,
                "session-1",
                CaptureGenerationReport.Status.FAILED,
                "src/test/java/generated/capture/RiskyReplayTest.java",
                "",
                CaptureReadiness.State.RISKY,
                List.of("event-2: absolute XPath requires review before replay."),
                List.of(new CaptureGenerationReport.LocatorDecision(
                        List.of("event-2"),
                        "submit-button",
                        "XPATH",
                        "/html/body/div[2]/button",
                        60,
                        List.of("position-only=10"),
                        List.of("css=button[type=submit]"))),
                List.of(),
                List.of("event-2: recorded replay status is FAILED."),
                List.of("event-2: fallback locator css=button[type=submit] is available."),
                List.of(),
                List.of(),
                List.of("review/LOCATOR/WARNING event-2: Absolute XPath. Recommendation: Prefer semantic locator."),
                new CaptureGenerationReport.Validation(
                        CaptureGenerationReport.Validation.ValidationStatus.FAILED,
                        List.of("line 42: cannot find symbol submitButton"),
                        0),
                new CaptureGenerationReport.Validation(
                        CaptureGenerationReport.Validation.ValidationStatus.FAILED,
                        List.of("trace action action-2 failed for event-2"),
                        1),
                CaptureGenerationReport.Enrichment.notRequested());

        List<McpCodeBlock> blocks = new McpCaptureCodeBlockService()
                .fromGeneratedSource(source, "driver", report);

        McpCodeBlock locatorQueue = block(blocks, "capture-locator-confidence-queue");
        assertEquals(McpCodeBlock.Kind.INVESTIGATION, locatorQueue.kind());
        assertTrue(locatorQueue.code().contains("event-2"));
        assertTrue(locatorQueue.code().contains("absolute XPath"));
        assertTrue(locatorQueue.code().contains("css=button[type=submit]"));

        McpCodeBlock validation = block(blocks, "capture-validation-back-links");
        assertEquals(McpCodeBlock.Kind.INVESTIGATION, validation.kind());
        assertTrue(validation.code().contains("Compilation FAILED"));
        assertTrue(validation.code().contains("Replay FAILED"));
        assertTrue(validation.code().contains("event-2"));
        assertTrue(validation.code().contains("action-2"));
    }

    @Test
    void fromGeneratedSourceAddsManualMappingWarningWhenPomCandidatesAreMissing() throws Exception {
        Path source = writeSource("""
                package generated.capture;

                public class EmptyReplayTest {
                }
                """);

        List<McpCodeBlock> blocks = new McpCaptureCodeBlockService()
                .fromGeneratedSource(source, "driver");

        assertEquals(List.of("capture-full-class"), blocks.stream().map(McpCodeBlock::id).toList());
    }

    private Path writeSource(String source) throws Exception {
        Path path = temp.resolve("Generated.java");
        Files.writeString(path, source);
        return path;
    }

    private static McpCodeBlock block(List<McpCodeBlock> blocks, String id) {
        return blocks.stream()
                .filter(block -> block.id().equals(id))
                .findFirst()
                .orElseThrow(() -> new AssertionError("Missing block " + id));
    }
}
