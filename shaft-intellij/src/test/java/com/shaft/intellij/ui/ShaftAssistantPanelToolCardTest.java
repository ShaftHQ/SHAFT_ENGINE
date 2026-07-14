package com.shaft.intellij.ui;

import com.shaft.intellij.mcp.ShaftMcpToolResult;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers {@link ShaftAssistantPanel#toolCardMarkdown}, the pure rendering step behind {@link
 * ShaftAssistantPanel#runToolAndRenderCard} -- the auto-doctor-on-failure and "Diagnose"/"Heal"
 * button entry points added for issue #3547. The EDT-marshaling wrapper around it needs a live MCP
 * connection and {@code ApplicationManager}, neither available in this lightweight Gradle unit
 * test JVM, so this class exercises the actual card-formatting logic directly instead.
 */
class ShaftAssistantPanelToolCardTest {
    @Test
    void rendersARepresentativeDoctorAnalysisReportAsAHumanReadableCard() {
        String output = """
                {
                  "schemaVersion": "1.0",
                  "status": "DETERMINISTIC",
                  "bundleId": "bundle-123",
                  "primaryCause": "LOCATOR",
                  "confidence": "HIGH",
                  "summary": "Button locator no longer matches the page.",
                  "actions": [
                    {"title":"Update locator","action":"Replace the stale CSS selector.","status":"SUGGESTED"}
                  ],
                  "codeBlocks": [
                    {"title":"Locator fix","language":"java","code":"driver.element().click(SHAFT.GUI.Locator.id(\\"login\\"));","copyPasteReady":true}
                  ],
                  "bundlePath": "target/shaft-doctor/evidence-bundle.json",
                  "jsonReportPath": "target/shaft-doctor/doctor-report.json",
                  "markdownReportPath": "target/shaft-doctor/doctor-report.md",
                  "warnings": []
                }
                """;

        String markdown = ShaftAssistantPanel.toolCardMarkdown(
                "doctor_analyze_failed_allure", ShaftMcpToolResult.success(output), null);

        assertAll(
                () -> assertTrue(markdown.contains("**Primary cause:** LOCATOR"), markdown),
                () -> assertTrue(markdown.contains("Button locator no longer matches the page."), markdown),
                () -> assertTrue(markdown.contains("Replace the stale CSS selector."), markdown),
                () -> assertFalse(markdown.contains("\"schemaVersion\""), markdown));
    }

    @Test
    void degradesGracefullyWhenTheMcpCallThrowsInsteadOfLeakingTheRawException() {
        Throwable thrown = new IllegalArgumentException(
                "No Allure results were found in this workspace (/repo). Run the failing test with "
                        + "SHAFT reporting enabled first, or pass allureResultPaths explicitly.");

        String markdown = ShaftAssistantPanel.toolCardMarkdown("doctor_analyze_failed_allure", null, thrown);

        assertAll(
                () -> assertTrue(markdown.contains("couldn't finish"), markdown),
                () -> assertTrue(markdown.contains("No Allure results were found"), markdown),
                () -> assertFalse(markdown.contains("IllegalArgumentException"), markdown),
                () -> assertFalse(markdown.contains("java.lang."), markdown),
                () -> assertFalse(markdown.contains("{"), markdown));
    }

    @Test
    void degradesGracefullyWhenNoResultAndNoErrorAreAvailable() {
        String markdown = ShaftAssistantPanel.toolCardMarkdown("doctor_analyze_failed_allure", null, null);

        assertAll(
                () -> assertTrue(markdown.contains("couldn't finish"), markdown),
                () -> assertTrue(markdown.contains("No result returned."), markdown));
    }

    @Test
    void degradesGracefullyOnAToolLevelFailureResultWithoutAThrownException() {
        String markdown = ShaftAssistantPanel.toolCardMarkdown(
                "doctor_analyze_failed_allure", ShaftMcpToolResult.failure("Trace path could not be read."), null);

        assertAll(
                () -> assertTrue(markdown.contains("couldn't finish"), markdown),
                () -> assertTrue(markdown.contains("Trace path could not be read."), markdown));
    }
}
