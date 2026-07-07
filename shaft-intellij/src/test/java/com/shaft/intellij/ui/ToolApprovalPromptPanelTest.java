package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.shaft.intellij.approval.ToolApprovalDecision;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
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

    private static JsonObject arguments() {
        JsonObject arguments = new JsonObject();
        arguments.addProperty("targetUrl", "https://example.com");
        return arguments;
    }
}
