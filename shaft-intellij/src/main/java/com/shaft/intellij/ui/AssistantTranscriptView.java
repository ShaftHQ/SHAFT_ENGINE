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

/**
 * Markdown-rendered Assistant transcript with copyable source text.
 */
final class AssistantTranscriptView extends JPanel {
    private static final String INITIAL_MESSAGE = "Type a question or use `/help` for SHAFT commands.";
    private static final Parser MARKDOWN = Parser.builder().build();
    private static final HtmlRenderer HTML = HtmlRenderer.builder().escapeHtml(true).build();

    private final JEditorPane pane;
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
        setMarkdown(INITIAL_MESSAGE);
        add(new JBScrollPane(pane), BorderLayout.CENTER);
    }

    String markdown() {
        return markdown;
    }

    void clear() {
        markdown = "";
        setMarkdown("");
    }

    void append(String message) {
        if (markdown.isBlank() || INITIAL_MESSAGE.equals(markdown)) {
            setMarkdown(message);
        } else {
            setMarkdown(markdown + "\n\n" + message);
        }
    }

    void setMarkdown(String value) {
        markdown = value == null ? "" : value;
        pane.setText(toHtml(markdown));
        pane.setCaretPosition(pane.getDocument().getLength());
    }

    private String toHtml(String value) {
        String body = value == null || value.isBlank() ? "" : convertMarkdown(value);
        return """
                <html>
                <head>
                  <style>
                    body { font-family: %s; font-size: %dpt; color: %s; background: %s; margin: 0; }
                    p { margin: 0 0 8px 0; }
                    pre { margin: 8px 0; padding: 8px; background: %s; border: 1px solid %s; }
                    code { font-family: Monospaced; }
                    ul, ol { margin-top: 4px; }
                    h1, h2, h3 { margin: 12px 0 8px 0; }
                  </style>
                </head>
                <body>%s</body>
                </html>
                """.formatted(
                fontFamily(),
                Math.max(11, pane.getFont() == null ? 12 : pane.getFont().getSize()),
                color("TextArea.foreground", "#000000"),
                color("TextArea.background", "#ffffff"),
                color("Panel.background", "#f2f2f2"),
                color("Component.borderColor", "#cccccc"),
                body);
    }

    private static String convertMarkdown(String value) {
        try {
            return HTML.render(MARKDOWN.parse(value));
        } catch (RuntimeException exception) {
            return "<pre>" + escapeHtml(value) + "</pre>";
        }
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
