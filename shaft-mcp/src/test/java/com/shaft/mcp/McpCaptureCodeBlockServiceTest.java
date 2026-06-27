package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerationReport;
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
    void fromGeneratedSourceAddsPomLocatorActionAndInsertionGuidance() throws Exception {
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
                "capture-pom-insertion-guide",
                "capture-agent-integration"), blocks.stream().map(McpCodeBlock::id).toList());

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

        McpCodeBlock guide = block(blocks, "capture-pom-insertion-guide");
        assertEquals(McpCodeBlock.Kind.PROVIDER_ADVISORY, guide.kind());
        assertTrue(guide.code().contains("Locator fields -> page class"));
        assertTrue(guide.code().contains("Action lines -> page methods"));
        assertTrue(guide.code().contains("Orchestration -> tests"));
        assertTrue(guide.code().contains("SHAFT.GUI.WebDriver"));
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

        assertFalse(blocks.stream().anyMatch(block -> block.id().equals("capture-target-locator-fields")));

        McpCodeBlock actions = block(blocks, "capture-target-action-snippet");
        assertEquals(McpCodeBlock.Kind.ACTION, actions.kind());
        assertTrue(actions.code().contains("browser.element().click(SHAFT.GUI.Locator.inputField(\"Username\"));"));
        assertTrue(actions.code().contains("browser.element().type(SHAFT.GUI.Locator.inputField(\"Username\"), requiredData(\"username\"));"));
        assertTrue(actions.placement().contains("after replayCheckout"));
        assertTrue(actions.warnings().isEmpty(), actions.warnings().toString());

        McpCodeBlock guide = block(blocks, "capture-target-insertion-guide");
        assertTrue(guide.code().contains("record-at-target"));
        assertTrue(guide.code().contains("CheckoutTest.java"));
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
        assertTrue(block(blocks, "capture-pom-insertion-guide").code().contains("SHAFT.GUI.Playwright"));
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
    void fromGeneratedSourceAddsManualMappingWarningWhenPomCandidatesAreMissing() throws Exception {
        Path source = writeSource("""
                package generated.capture;

                public class EmptyReplayTest {
                }
                """);

        List<McpCodeBlock> blocks = new McpCaptureCodeBlockService()
                .fromGeneratedSource(source, "driver");

        assertTrue(blocks.stream().anyMatch(block -> block.id().equals("capture-full-class")));
        assertTrue(blocks.stream().anyMatch(block -> block.id().equals("capture-agent-integration")));
        McpCodeBlock warning = block(blocks, "capture-pom-manual-mapping-warning");
        assertTrue(warning.warnings().stream().anyMatch(message -> message.contains("manual mapping")));
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
