package com.shaft.intellij.ui;

import com.google.gson.JsonObject;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.approval.ToolApprovalDecision;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Interactive approval bubble rendered inline in the Assistant transcript before a SHAFT MCP
 * tool call is dispatched. Shows one button per {@link ToolApprovalDecision} scope the currently
 * selected agent declares support for (via {@link AgentApprovalCapability}), plus a Deny button
 * that is always present regardless of capability. A click disables every button (so the decision
 * can only be made once) and hands the chosen decision to the supplied callback; callers are
 * responsible for completing any pending future and marshaling follow-up work back onto the EDT.
 */
final class ToolApprovalPromptPanel extends JPanel {

    /**
     * Declares which {@link ToolApprovalDecision} scopes the currently selected agent supports.
     * {@link #NONE} hides every scope button (and the "Approve all SHAFT tools" checkbox) while
     * still allowing Deny; {@link #STANDARD} is every agent SHAFT currently ships that dispatches
     * SHAFT MCP tool calls through the Assistant panel.
     */
    enum AgentApprovalCapability {
        NONE(List.of()),
        STANDARD(List.of(
                ToolApprovalDecision.APPROVE_ONCE,
                ToolApprovalDecision.APPROVE_TOOL_ALWAYS,
                ToolApprovalDecision.APPROVE_ALL_TOOLS));

        private final List<ToolApprovalDecision> scopes;

        AgentApprovalCapability(List<ToolApprovalDecision> scopes) {
            this.scopes = scopes;
        }

        List<ToolApprovalDecision> scopes() {
            return scopes;
        }

        boolean supportsApprovals() {
            return this != NONE;
        }
    }

    private final List<JButton> decisionButtons = new ArrayList<>();
    private Consumer<ToolApprovalDecision> onDecision;

    ToolApprovalPromptPanel(
            String toolName,
            JsonObject arguments,
            AgentApprovalCapability capability,
            Consumer<ToolApprovalDecision> onDecision) {
        super(new BorderLayout(4, 6));
        this.onDecision = onDecision;
        setOpaque(false);
        setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createEtchedBorder(),
                JBUI.Borders.empty(8)));
        getAccessibleContext().setAccessibleName("Tool approval request for " + toolName);
        getAccessibleContext().setAccessibleDescription(
                "SHAFT is asking for approval to run the " + toolName + " tool.");

        JLabel summary = new JLabel("<html>SHAFT wants to run <b>" + escapeHtml(toolName) + "</b></html>");
        summary.getAccessibleContext().setAccessibleName("Tool approval summary");
        JLabel argumentsLabel = new JLabel(escapeHtml(argumentsSummary(arguments)));
        argumentsLabel.getAccessibleContext().setAccessibleName("Tool approval arguments");
        argumentsLabel.getAccessibleContext().setAccessibleDescription(
                "Arguments for the " + toolName + " tool call awaiting approval: " + argumentsSummary(arguments));

        JPanel header = new JPanel(new BorderLayout(2, 2));
        header.setOpaque(false);
        header.add(summary, BorderLayout.NORTH);
        header.add(argumentsLabel, BorderLayout.CENTER);

        JPanel actions = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 0));
        actions.setOpaque(false);
        for (ToolApprovalDecision decision : capability == null ? List.<ToolApprovalDecision>of() : capability.scopes()) {
            actions.add(decisionButton(toolName, decision));
        }
        actions.add(decisionButton(toolName, ToolApprovalDecision.DENY));

        add(header, BorderLayout.NORTH);
        add(actions, BorderLayout.SOUTH);
    }

    private JButton decisionButton(String toolName, ToolApprovalDecision decision) {
        JButton button = new JButton(decision.getDisplayLabel());
        button.getAccessibleContext().setAccessibleName(decision.getDisplayLabel() + " for " + toolName);
        button.getAccessibleContext().setAccessibleDescription(buttonDescription(toolName, decision));
        button.setToolTipText(buttonDescription(toolName, decision));
        button.addActionListener(event -> decide(decision));
        decisionButtons.add(button);
        return button;
    }

    private static String buttonDescription(String toolName, ToolApprovalDecision decision) {
        return switch (decision) {
            case APPROVE_ONCE -> "Approve the " + toolName + " tool call once.";
            case APPROVE_TOOL_ALWAYS -> "Approve every future call to " + toolName + " in this project.";
            case APPROVE_ALL_TOOLS -> "Approve every SHAFT MCP tool call in this project.";
            case DENY -> "Deny the " + toolName + " tool call.";
        };
    }

    private void decide(ToolApprovalDecision decision) {
        for (JButton button : decisionButtons) {
            button.setEnabled(false);
        }
        Consumer<ToolApprovalDecision> callback = onDecision;
        onDecision = null;
        if (callback != null) {
            callback.accept(decision);
        }
    }

    /**
     * Returns the rendered decision buttons (including Deny), in display order. Used by tests to
     * assert which scopes were rendered and to drive clicks without relying on accessible-name
     * lookups.
     *
     * @return immutable snapshot of the decision buttons
     */
    List<JButton> decisionButtonsForTest() {
        return List.copyOf(decisionButtons);
    }

    private static String argumentsSummary(JsonObject arguments) {
        if (arguments == null || arguments.isEmpty()) {
            return "No arguments.";
        }
        String json = arguments.toString();
        int maxLength = 200;
        return json.length() > maxLength ? json.substring(0, maxLength) + "..." : json;
    }

    private static String escapeHtml(String value) {
        return value == null ? "" : value.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
    }
}
