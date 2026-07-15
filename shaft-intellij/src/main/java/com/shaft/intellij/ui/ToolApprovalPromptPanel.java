package com.shaft.intellij.ui;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.intellij.ui.JBColor;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.approval.ToolApprovalDecision;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
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
     * {@link #NONE} hides every scope button while still allowing Deny; {@link #STANDARD} is every
     * agent SHAFT currently ships that dispatches SHAFT MCP tool calls through the Assistant panel.
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

        WrappingArgumentsArea plainLanguageArea = new WrappingArgumentsArea(plainLanguageSummary(arguments));
        plainLanguageArea.getAccessibleContext().setAccessibleName("Tool approval plain-language summary");
        plainLanguageArea.getAccessibleContext().setAccessibleDescription(
                "Plain-language description of the arguments for the " + toolName + " tool call awaiting approval: "
                        + plainLanguageSummary(arguments));

        JTextArea argumentsLabel = new WrappingArgumentsArea(argumentsSummary(arguments));
        argumentsLabel.setFont(argumentsLabel.getFont().deriveFont(Math.max(10f, argumentsLabel.getFont().getSize2D() - 1f)));
        argumentsLabel.setForeground(JBColor.namedColor("Label.disabledForeground", JBColor.GRAY));
        argumentsLabel.getAccessibleContext().setAccessibleName("Tool approval arguments");
        argumentsLabel.getAccessibleContext().setAccessibleDescription(
                "Arguments for the " + toolName + " tool call awaiting approval: " + argumentsSummary(arguments));

        JPanel argumentsPanel = new JPanel();
        argumentsPanel.setOpaque(false);
        argumentsPanel.setLayout(new BoxLayout(argumentsPanel, BoxLayout.Y_AXIS));
        argumentsPanel.add(plainLanguageArea);
        argumentsPanel.add(argumentsLabel);

        JPanel header = new JPanel(new BorderLayout(2, 2));
        header.setOpaque(false);
        header.add(summary, BorderLayout.NORTH);
        header.add(argumentsPanel, BorderLayout.CENTER);

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
        if (isBroadScope(decision)) {
            button.setFont(button.getFont().deriveFont(Math.max(10f, button.getFont().getSize2D() - 1f)));
            button.setForeground(JBColor.namedColor("Label.disabledForeground", JBColor.GRAY));
        }
        decisionButtons.add(button);
        return button;
    }

    /**
     * {@code APPROVE_TOOL_ALWAYS} and {@code APPROVE_ALL_TOOLS} grant broader, longer-lived
     * trust than {@code APPROVE_ONCE} or {@code DENY}; they're rendered with a lighter visual
     * weight so the narrowest/safest choices remain the default-looking path.
     */
    private static boolean isBroadScope(ToolApprovalDecision decision) {
        return decision == ToolApprovalDecision.APPROVE_TOOL_ALWAYS || decision == ToolApprovalDecision.APPROVE_ALL_TOOLS;
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

    /**
     * Renders the top-level entries of {@code arguments} as a plain-language sentence instead of
     * raw JSON, e.g. {@code {"url":"https://example.com","headless":false}} becomes
     * {@code "This will run with url: https://example.com, headless: false."}. SHAFT MCP tool
     * arguments are effectively flat, so nested object/array values are rendered with their own
     * {@code toString()} rather than recursively flattened; this is a one-level flattening, not a
     * general JSON-to-English engine.
     */
    private static String plainLanguageSummary(JsonObject arguments) {
        if (arguments == null || arguments.isEmpty()) {
            return "No arguments.";
        }
        StringBuilder pairs = new StringBuilder();
        for (Map.Entry<String, JsonElement> entry : arguments.entrySet()) {
            if (pairs.length() > 0) {
                pairs.append(", ");
            }
            JsonElement value = entry.getValue();
            String plainValue = value == null || value.isJsonNull() ? "none"
                    : value.isJsonPrimitive() ? value.getAsString() : value.toString();
            pairs.append(entry.getKey()).append(": ").append(plainValue);
        }
        return "This will run with " + pairs + ".";
    }

    private static String escapeHtml(String value) {
        return value == null ? "" : value.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
    }

    /**
     * Non-editable, word-wrapping text area that caps its own preferred width instead of growing to
     * fit a single unbroken line, so a long raw-JSON arguments summary wraps onto multiple lines
     * within the bubble instead of pushing the whole approval prompt past the transcript's right
     * edge (unlike {@link JLabel}, which never wraps and always reports its full single-line width).
     */
    private static final class WrappingArgumentsArea extends JTextArea {
        private static final int MAX_WIDTH = 360;

        private WrappingArgumentsArea(String text) {
            super(text);
            setEditable(false);
            setFocusable(false);
            setOpaque(false);
            setLineWrap(true);
            setWrapStyleWord(true);
            setBorder(null);
            setFont(new JLabel().getFont());
        }

        @Override
        public Dimension getPreferredSize() {
            // Always wrap at a fixed width instead of deriving a cap from super.getPreferredSize()'s
            // natural width: that query happens while this component's own size is still (0, 0) on
            // its first layout pass, and a line-wrapped JTextArea measured at zero width collapses to
            // near-zero preferred width too - wrapping every subsequent line one character at a time.
            // That stayed invisible while this was the sole BorderLayout.CENTER child (which discards
            // the child's preferred width and stretches it to the real available width regardless),
            // but a second wrapping area stacked via BoxLayout honors the (broken) preferred width as
            // the real allocated width, so the fixed-width component must actually be correct.
            int width = JBUI.scale(MAX_WIDTH);
            setSize(new Dimension(width, Short.MAX_VALUE));
            Dimension preferred = super.getPreferredSize();
            preferred.width = width;
            return preferred;
        }

        @Override
        public Dimension getMaximumSize() {
            return getPreferredSize();
        }
    }
}
