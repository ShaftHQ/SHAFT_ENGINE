package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.shaft.intellij.approval.ToolApprovalDecision;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import javax.swing.JToggleButton;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolApprovalPromptPanelTest {
    @Test
    void standardCapabilityShowsAllScopesPlusDeny() {
        AtomicReference<ToolApprovalDecision> decided = new AtomicReference<>();
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments(), ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decided::set);

        List<JButton> buttons = panel.decisionButtonsForTest();
        List<String> labels = buttons.stream().map(JButton::getText).toList();

        assertAll(
                () -> assertEquals(4, buttons.size()),
                () -> assertEquals("Deny", buttons.get(0).getText(), "Deny must be the safe first choice"),
                () -> assertTrue(labels.contains("Approve once")),
                () -> assertTrue(labels.contains("Approve tool always")),
                () -> assertTrue(labels.contains("Approve all tools")),
                () -> assertTrue(labels.contains("Deny")),
                () -> assertNull(decided.get(), "no button clicked yet"));
    }

    @Test
    void noneCapabilityShowsOnlyDeny() {
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments(), ToolApprovalPromptPanel.AgentApprovalCapability.NONE, decision -> { });

        List<JButton> buttons = panel.decisionButtonsForTest();

        assertAll(
                () -> assertEquals(1, buttons.size()),
                () -> assertEquals("Deny", buttons.get(0).getText()),
                () -> assertFalse(ToolApprovalPromptPanel.AgentApprovalCapability.NONE.supportsApprovals()),
                () -> assertTrue(ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD.supportsApprovals()));
    }

    @Test
    void everyButtonHasAnAccessibleNameAndDescription() {
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments(), ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decision -> { });

        assertAll(panel.decisionButtonsForTest().stream()
                .flatMap(button -> java.util.stream.Stream.of(
                        (org.junit.jupiter.api.function.Executable) () -> assertFalse(
                                button.getAccessibleContext().getAccessibleName() == null
                                        || button.getAccessibleContext().getAccessibleName().isBlank(),
                                "button should have an accessible name"),
                        () -> assertFalse(
                                button.getAccessibleContext().getAccessibleDescription() == null
                                        || button.getAccessibleContext().getAccessibleDescription().isBlank(),
                                "button should have an accessible description"))));
    }

    @Test
    void clickingAButtonDisablesAllButtonsAndReportsTheDecisionExactlyOnce() {
        AtomicReference<ToolApprovalDecision> decided = new AtomicReference<>();
        java.util.concurrent.atomic.AtomicInteger callCount = new java.util.concurrent.atomic.AtomicInteger();
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments(), ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decision -> {
                    decided.set(decision);
                    callCount.incrementAndGet();
                });
        List<JButton> buttons = panel.decisionButtonsForTest();
        JButton approveOnce = buttons.stream().filter(button -> "Approve once".equals(button.getText())).findFirst()
                .orElseThrow();

        approveOnce.doClick();
        // A second click (e.g. a stray double-click) must not re-report a decision.
        approveOnce.doClick();

        assertAll(
                () -> assertEquals(ToolApprovalDecision.APPROVE_ONCE, decided.get()),
                () -> assertEquals(1, callCount.get(), "the decision callback must fire exactly once"),
                () -> assertTrue(buttons.stream().noneMatch(JButton::isEnabled),
                        "every button should be disabled once a decision is made"));
    }

    @Test
    void denyDecisionIsReportedLikeAnyOtherScope() {
        AtomicReference<ToolApprovalDecision> decided = new AtomicReference<>();
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments(), ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decided::set);
        JButton deny = panel.decisionButtonsForTest().stream().filter(button -> "Deny".equals(button.getText()))
                .findFirst().orElseThrow();

        deny.doClick();

        assertEquals(ToolApprovalDecision.DENY, decided.get());
    }

    @Test
    void plainLanguageSummaryIsHumanReadableNotRawJson() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("url", "https://example.com");
        arguments.addProperty("headless", false);
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments, ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decision -> { });

        String plainLanguageText = plainLanguageAreaText(panel);

        assertAll(
                () -> assertFalse(plainLanguageText.contains("{"), "should not contain raw JSON punctuation"),
                () -> assertFalse(plainLanguageText.contains("}"), "should not contain raw JSON punctuation"),
                () -> assertFalse(plainLanguageText.contains("\":\""), "should not contain raw JSON punctuation"),
                () -> assertTrue(plainLanguageText.contains("url: https://example.com"),
                        "should contain the argument value in readable form"),
                () -> assertTrue(plainLanguageText.contains("headless: false"),
                        "should contain the argument value in readable form"));
    }

    @Test
    void argumentsSummaryRendersFullJsonWithoutTruncation() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("longValue", "x".repeat(300));
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments, ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decision -> { });

        String argumentsText = argumentsAreaText(panel);
        String expectedFullJson = arguments.toString();

        assertAll(
                () -> assertEquals(expectedFullJson, argumentsText,
                        "arguments JSON must render in full, not truncated"),
                () -> assertFalse(argumentsText.endsWith("..."),
                        "arguments JSON must not be truncated with a trailing ellipsis"));
    }

    /** Permanent scopes remain explicit but visually secondary to the safe first decision. */
    @Test
    void broadAndPermanentApprovalScopesAreNotVisuallyPromoted() {
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments(), ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decision -> { });
        List<JButton> buttons = panel.decisionButtonsForTest();

        JButton approveOnce = findByLabel(buttons, "Approve once");
        JButton approveToolAlways = findByLabel(buttons, "Approve tool always");
        JButton approveAllTools = findByLabel(buttons, "Approve all tools");
        assertAll(
                () -> assertEquals(approveOnce.getBackground(), approveToolAlways.getBackground(),
                        "permanent approval must keep the native neutral treatment"),
                () -> assertEquals(approveOnce.getBackground(), approveAllTools.getBackground(),
                        "broad approval must not look like the preferred action"),
                () -> assertFalse(approveToolAlways.isContentAreaFilled(),
                        "permanent approval must not use an opaque primary-button fill"),
                () -> assertFalse(approveAllTools.isContentAreaFilled(),
                        "broad approval must not use an opaque primary-button fill"),
                () -> assertEquals(approveOnce.getFont().getSize2D(), approveToolAlways.getFont().getSize2D(),
                        "Approve tool always must not be font-shrunk relative to Approve once"),
                () -> assertEquals(approveOnce.getFont().getSize2D(), approveAllTools.getFont().getSize2D(),
                        "Approve all tools must not gain a larger visual weight than Approve once"));
    }

    private static JButton findByLabel(List<JButton> buttons, String label) {
        return buttons.stream().filter(button -> label.equals(button.getText())).findFirst().orElseThrow();
    }

    private static String plainLanguageAreaText(ToolApprovalPromptPanel panel) {
        List<javax.swing.JTextArea> textAreas = new java.util.ArrayList<>();
        collectTextAreas(panel, textAreas);
        return textAreas.stream()
                .filter(area -> "Tool approval plain-language summary".equals(
                        area.getAccessibleContext().getAccessibleName()))
                .findFirst()
                .orElseThrow(() -> new AssertionError("plain-language summary text area not found"))
                .getText();
    }

    private static String argumentsAreaText(ToolApprovalPromptPanel panel) {
        List<javax.swing.JTextArea> textAreas = new java.util.ArrayList<>();
        collectTextAreas(panel, textAreas);
        return textAreas.stream()
                .filter(area -> "Tool approval arguments".equals(
                        area.getAccessibleContext().getAccessibleName()))
                .findFirst()
                .orElseThrow(() -> new AssertionError("arguments text area not found"))
                .getText();
    }

    @Test
    void rawArgumentsStayHiddenUntilTechnicalDetailsAreRequested() {
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments(), ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decision -> { });
        javax.swing.JTextArea arguments = findArgumentsArea(panel);
        JToggleButton details = findToggle(panel, "Show technical details");

        assertAll(
                () -> assertFalse(arguments.isVisible(), "raw JSON must not be permanent approval chrome"),
                () -> assertNotNull(details));

        details.doClick();

        assertAll(
                () -> assertTrue(arguments.isVisible()),
                () -> assertEquals("Hide technical details", details.getText()));
    }

    @Test
    void permanentApprovalScopesStayBehindReviewOptions() {
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments(), ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decision -> { });
        List<JButton> buttons = panel.decisionButtonsForTest();
        JToggleButton review = findToggle(panel, "Review options");
        JButton always = findByLabel(buttons, "Approve tool always");
        JButton all = findByLabel(buttons, "Approve all tools");

        assertAll(
                () -> assertNotNull(review),
                () -> assertFalse(always.isVisible()),
                () -> assertFalse(all.isVisible()));

        review.doClick();

        assertAll(
                () -> assertTrue(always.isVisible()),
                () -> assertTrue(all.isVisible()),
                () -> assertEquals("Hide options", review.getText()));
    }

    private static javax.swing.JTextArea findArgumentsArea(ToolApprovalPromptPanel panel) {
        List<javax.swing.JTextArea> textAreas = new java.util.ArrayList<>();
        collectTextAreas(panel, textAreas);
        return textAreas.stream()
                .filter(area -> "Tool approval arguments".equals(area.getAccessibleContext().getAccessibleName()))
                .findFirst().orElseThrow();
    }

    private static JToggleButton findToggle(java.awt.Container container, String text) {
        for (java.awt.Component component : container.getComponents()) {
            if (component instanceof JToggleButton toggle && text.equals(toggle.getText())) {
                return toggle;
            }
            if (component instanceof java.awt.Container child) {
                JToggleButton nested = findToggle(child, text);
                if (nested != null) {
                    return nested;
                }
            }
        }
        return null;
    }

    private static void collectTextAreas(java.awt.Container container, List<javax.swing.JTextArea> found) {
        for (java.awt.Component component : container.getComponents()) {
            if (component instanceof javax.swing.JTextArea textArea) {
                found.add(textArea);
            }
            if (component instanceof java.awt.Container child) {
                collectTextAreas(child, found);
            }
        }
    }

    private static JsonObject arguments() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", "https://example.com");
        return arguments;
    }
}
