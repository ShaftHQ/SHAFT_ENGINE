package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.shaft.intellij.approval.ToolApprovalDecision;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import java.awt.Color;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToolApprovalPromptPanelTest {
    // Issue #3782: a leaked dark-theme UIManager override (e.g. Button.background 0x45494A, a
    // near-gray with red/green/blue only 1-5 units apart) from another test's L&F install can read
    // as "blueish" under a plain greater-than comparison. Requiring a real minimum gap between the
    // dominant channel and the others keeps these heuristics meaningful for genuinely color-coded
    // buttons while no longer misclassifying an ambient near-gray leak as a hue.
    private static final int HUE_DOMINANCE_THRESHOLD = 15;

    @Test
    void standardCapabilityShowsAllScopesPlusDeny() {
        AtomicReference<ToolApprovalDecision> decided = new AtomicReference<>();
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments(), ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decided::set);

        List<JButton> buttons = panel.decisionButtonsForTest();
        List<String> labels = buttons.stream().map(JButton::getText).toList();

        assertAll(
                () -> assertEquals(4, buttons.size()),
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

    /**
     * Issue #3696: previously {@code APPROVE_TOOL_ALWAYS} and {@code APPROVE_ALL_TOOLS} rendered
     * with a smaller, gray, unbordered "de-emphasized" look while {@code APPROVE_ONCE} kept full
     * default emphasis -- the opposite of the intended nudge toward broader approval scopes. Now
     * all three scope buttons share equal font weight and are color-coded by scope breadth: green
     * (broadest) for "Approve all tools", blue (medium) for "Approve tool always", yellow
     * (narrowest) for "Approve once". {@code Deny} is not a scope to weight and keeps the platform
     * default look.
     */
    @Test
    void approvalScopeButtonsShareEqualVisualWeightAndAreColorCodedByScopeBreadth() {
        ToolApprovalPromptPanel panel = new ToolApprovalPromptPanel(
                "capture_start", arguments(), ToolApprovalPromptPanel.AgentApprovalCapability.STANDARD, decision -> { });
        List<JButton> buttons = panel.decisionButtonsForTest();

        JButton approveOnce = findByLabel(buttons, "Approve once");
        JButton approveToolAlways = findByLabel(buttons, "Approve tool always");
        JButton approveAllTools = findByLabel(buttons, "Approve all tools");
        JButton deny = findByLabel(buttons, "Deny");

        assertAll(
                () -> assertEquals(approveOnce.getFont().getSize2D(), approveToolAlways.getFont().getSize2D(),
                        "Approve tool always must not be font-shrunk relative to Approve once"),
                () -> assertEquals(approveOnce.getFont().getSize2D(), approveAllTools.getFont().getSize2D(),
                        "Approve all tools must not be font-shrunk relative to Approve once"),
                () -> assertTrue(isGreenish(approveAllTools.getBackground()),
                        "Approve all tools (broadest scope) should be highlighted green: "
                                + approveAllTools.getBackground()),
                () -> assertTrue(isBlueish(approveToolAlways.getBackground()),
                        "Approve tool always (medium scope) should be highlighted blue: "
                                + approveToolAlways.getBackground()),
                () -> assertTrue(isYellowish(approveOnce.getBackground()),
                        "Approve once (narrowest scope) should be highlighted yellow: "
                                + approveOnce.getBackground()),
                () -> assertFalse(isGreenish(deny.getBackground()) || isBlueish(deny.getBackground())
                                || isYellowish(deny.getBackground()),
                        "Deny should keep the platform default button color, not a scope color"));
    }

    private static boolean isGreenish(Color color) {
        return color.getGreen() - color.getRed() > HUE_DOMINANCE_THRESHOLD
                && color.getGreen() - color.getBlue() > HUE_DOMINANCE_THRESHOLD;
    }

    private static boolean isBlueish(Color color) {
        return color.getBlue() - color.getRed() > HUE_DOMINANCE_THRESHOLD
                && color.getBlue() - color.getGreen() > HUE_DOMINANCE_THRESHOLD;
    }

    private static boolean isYellowish(Color color) {
        return color.getRed() - color.getBlue() > HUE_DOMINANCE_THRESHOLD
                && color.getGreen() - color.getBlue() > HUE_DOMINANCE_THRESHOLD;
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
