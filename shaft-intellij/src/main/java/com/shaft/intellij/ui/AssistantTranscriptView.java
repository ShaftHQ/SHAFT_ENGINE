package com.shaft.intellij.ui;

import com.intellij.ui.components.JBScrollPane;
import com.intellij.util.ui.JBUI;
import org.commonmark.parser.Parser;
import org.commonmark.renderer.html.HtmlRenderer;

import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.UIManager;
import javax.swing.text.html.HTMLEditorKit;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.util.ArrayList;
import java.util.List;

/**
 * Markdown-rendered Assistant transcript with copyable source text.
 */
final class AssistantTranscriptView extends JPanel {
    private static final String INITIAL_MESSAGE = "Type a question or use `/help` for SHAFT commands.";
    private static final Parser MARKDOWN = Parser.builder().build();
    private static final HtmlRenderer HTML = HtmlRenderer.builder().escapeHtml(true).build();
    private static final String USER_ROLE = "user";
    private static final String UNKNOWN_ROLE = "assistant";

    private final JEditorPane pane;
    private final List<ShaftAssistantChatState.Message> messages = new ArrayList<>();
    private String markdown = INITIAL_MESSAGE;

    AssistantTranscriptView() {
        super(new BorderLayout());
        pane = new JEditorPane();
        pane.setContentType("text/html");
        pane.setEditorKit(new HTMLEditorKit());
        pane.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
        pane.getAccessibleContext().setAccessibleName("Assistant transcript");
        pane.setEditable(false);
        pane.setOpaque(true);
        pane.setBorder(JBUI.Borders.empty(8));
        showInitialMessage();
        add(new JBScrollPane(pane), BorderLayout.CENTER);
    }

    String markdown() {
        return markdown;
    }

    String html() {
        return pane.getText();
    }

    void clear() {
        markdown = "";
        messages.clear();
        refresh();
    }

    void append(String role, String message) {
        addMessage(role, message);
        markdown = joinMessages(messages);
        refresh();
    }

    void setMarkdown(String value) {
        markdown = value == null ? "" : value;
        messages.clear();
        addMessage(UNKNOWN_ROLE, markdown);
        markdown = joinMessages(messages);
        refresh();
    }

    void setMessages(List<ShaftAssistantChatState.Message> restoredMessages) {
        messages.clear();
        if (restoredMessages != null) {
            for (ShaftAssistantChatState.Message message : restoredMessages) {
                if (message == null || message.markdown == null || message.markdown.isBlank()) {
                    continue;
                }
                addMessage(message.role, message.markdown);
            }
        }
        markdown = joinMessages(messages);
        refresh();
    }

    private void addMessage(String role, String value) {
        if (value == null || value.isBlank()) {
            return;
        }
        ShaftAssistantChatState.Message message = new ShaftAssistantChatState.Message();
        message.role = normalizedRole(role);
        message.markdown = value;
        messages.add(message);
    }

    private String toHtml(String value) {
        return """
                <html>
                <head>
                  <style>
                    body { font-family: %s; font-size: %dpt; color: %s; background: %s; margin: 0; }
                    table.message-row { margin: 0 0 8px 0; }
                    .message-bubble {
                        width: 86%%;
                        border: 1px solid %s;
                        border-radius: 12px;
                        padding: 8px 10px;
                        text-align: right;
                    }
                    .message-bubble.assistant {
                        background: %s;
                        color: %s;
                    }
                    .message-bubble.user {
                        background: %s;
                        color: %s;
                        border-color: %s;
                    }
                    p, ul, ol, h1, h2, h3 { margin: 0 0 8px 0; }
                    p { text-align: right; }
                    pre {
                        margin: 8px 0;
                        padding: 8px;
                        background: %s;
                        border: 1px solid %s;
                        border-radius: 8px;
                        white-space: pre-wrap;
                    }
                    code { font-family: Monospaced; }
                  </style>
                </head>
                <body><div id="chat">%s</div></body>
                </html>
                """.formatted(
                fontFamily(),
                Math.max(11, pane.getFont() == null ? 12 : pane.getFont().getSize()),
                color("TextArea.foreground", "#000000"),
                color("TextArea.background", "#ffffff"),
                color("Component.borderColor", "#d8d8d8"),
                color("Panel.background", "#f5f7fc"),
                color("TextArea.foreground", "#202020"),
                color("Panel.selectionBackground", "#4B86FF"),
                color("Panel.selectionForeground", "#ffffff"),
                color("Panel.selectionBackground", "#4B86FF"),
                color("Panel.background", "#f2f2f2"),
                color("Component.borderColor", "#cccccc"),
                value);
    }

    private static String convertMarkdown(String value) {
        try {
            return HTML.render(MARKDOWN.parse(value));
        } catch (RuntimeException exception) {
            return "<pre>" + escapeHtml(value) + "</pre>";
        }
    }

    private void refresh() {
        pane.setText(toHtml(renderMessages()));
        pane.setCaretPosition(pane.getDocument().getLength());
    }

    private String renderMessages() {
        StringBuilder rendered = new StringBuilder();
        for (ShaftAssistantChatState.Message message : messages) {
            rendered.append(renderMessage(message.role, message.markdown));
        }
        return rendered.toString();
    }

    private void showInitialMessage() {
        pane.setText(toHtml(renderMessage(UNKNOWN_ROLE, INITIAL_MESSAGE)));
        pane.setCaretPosition(pane.getDocument().getLength());
    }

    private static String renderMessage(String role, String markdown) {
        boolean user = USER_ROLE.equals(normalizedRole(role));
        String roleClass = roleClass(user);
        return "<table class=\"message-row " + roleClass + "\" width=\"100%\" cellspacing=\"0\" cellpadding=\"0\">"
                + "<tr><td align=\"" + (user ? "right" : "left") + "\">"
                + "<div class=\"message-bubble " + roleClass + "\">"
                + convertMarkdown(markdown)
                + "</div></td></tr></table>";
    }

    private static String joinMessages(List<ShaftAssistantChatState.Message> messageList) {
        StringBuilder content = new StringBuilder();
        for (ShaftAssistantChatState.Message message : messageList) {
            if (message == null || message.markdown == null || message.markdown.isBlank()) {
                continue;
            }
            if (content.length() > 0) {
                content.append("\n\n");
            }
            content.append(message.markdown);
        }
        return content.toString();
    }

    private static String roleClass(boolean user) {
        return user ? "user" : "assistant";
    }

    private static String normalizedRole(String role) {
        if (role == null || role.isBlank()) {
            return UNKNOWN_ROLE;
        }
        return role.trim().toLowerCase();
    }

    private String fontFamily() {
        Font font = pane.getFont();
        return font == null ? "Dialog" : font.getFamily();
    }

    private static String color(String uiKey, String fallback) {
        Color resolved = UIManager.getColor(uiKey);
        if (resolved == null) {
            return fallback;
        }
        return "#%02x%02x%02x".formatted(resolved.getRed(), resolved.getGreen(), resolved.getBlue());
    }

    private static String escapeHtml(String value) {
        return value.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;");
    }
}
