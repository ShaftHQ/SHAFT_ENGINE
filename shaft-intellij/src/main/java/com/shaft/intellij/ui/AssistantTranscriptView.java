package com.shaft.intellij.ui;

import com.intellij.lang.Language;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.EditorColorsScheme;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.editor.richcopy.HtmlSyntaxInfoUtil;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightVirtualFile;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.jcef.JBCefApp;
import com.intellij.util.ui.JBUI;
import org.commonmark.parser.Parser;
import org.commonmark.renderer.html.HtmlRenderer;
import org.intellij.plugins.markdown.lang.MarkdownFileType;
import org.intellij.plugins.markdown.ui.preview.html.MarkdownUtil;
import org.intellij.plugins.markdown.ui.preview.jcef.MarkdownJCEFHtmlPanel;

import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.UIManager;
import javax.swing.BoxLayout;
import javax.swing.event.HyperlinkEvent;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.JLabel;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Dimension;
import java.awt.datatransfer.StringSelection;
import java.lang.reflect.Proxy;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Markdown-rendered Assistant transcript with copyable source text.
 */
final class AssistantTranscriptView extends JPanel {
    private static final String INITIAL_MESSAGE = "Type a question or use `/commands` for SHAFT commands.";
    private static final Parser COMMONMARK = Parser.builder().build();
    private static final HtmlRenderer COMMONMARK_HTML = HtmlRenderer.builder().escapeHtml(true).build();
    private static final Pattern LANGUAGE_CLASS = Pattern.compile("(?i)\\blanguage-([a-z0-9_+.#-]+)");
    private static final Set<String> JAVA_KEYWORDS = Set.of(
            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const",
            "continue", "default", "do", "double", "else", "enum", "exports", "extends", "final", "finally",
            "float", "for", "if", "implements", "import", "instanceof", "int", "interface", "long", "module",
            "native", "new", "non-sealed", "open", "opens", "package", "permits", "private", "protected",
            "provides", "public", "record", "requires", "return", "sealed", "short", "static", "strictfp",
            "super", "switch", "synchronized", "this", "throw", "throws", "to", "transient", "transitive",
            "try", "uses", "var", "void", "volatile", "while", "with", "yield", "true", "false", "null");
    private static final String USER_ROLE = "user";
    private static final String UNKNOWN_ROLE = "assistant";

    private final Project project;
    private final JEditorPane pane;
    private final JPanel fallbackPanel;
    private final MarkdownJCEFHtmlPanel markdownPanel;
    private final LightVirtualFile markdownFile;
    private final List<ShaftAssistantChatState.Message> messages = new ArrayList<>();
    private String markdown = "";
    private String renderedHtml = "";

    AssistantTranscriptView() {
        this(null);
    }

    AssistantTranscriptView(Project project) {
        super(new BorderLayout());
        this.project = project;
        markdownFile = new LightVirtualFile("shaft-assistant-chat.md", MarkdownFileType.INSTANCE, "");
        pane = new JEditorPane();
        pane.setContentType("text/html");
        pane.setEditorKit(new HTMLEditorKit());
        pane.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
        pane.getAccessibleContext().setAccessibleName("Assistant transcript");
        pane.setEditable(false);
        pane.setOpaque(true);
        pane.setBorder(JBUI.Borders.empty(8));
        pane.addHyperlinkListener(AssistantTranscriptView::copyCodeFromFallbackLink);
        fallbackPanel = new JPanel();
        fallbackPanel.setLayout(new BoxLayout(fallbackPanel, BoxLayout.Y_AXIS));
        fallbackPanel.setBorder(JBUI.Borders.empty(8));
        fallbackPanel.getAccessibleContext().setAccessibleName("Assistant transcript");

        Color transcriptBackground = resolvedColor("TextArea.background", Color.WHITE);
        pane.setBackground(transcriptBackground);
        fallbackPanel.setBackground(transcriptBackground);
        fallbackPanel.setOpaque(true);
        markdownPanel = createMarkdownPanel(project, markdownFile);
        JComponent component = markdownPanel == null ? fallbackScrollPane(transcriptBackground) : markdownPanel.getComponent();
        component.getAccessibleContext().setAccessibleName("Assistant transcript");
        add(component, BorderLayout.CENTER);
        refresh();
    }

    String markdown() {
        return markdown;
    }

    String html() {
        return renderedHtml;
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

    void replaceLast(String role, String message) {
        if (message == null || message.isBlank()) {
            return;
        }
        if (messages.isEmpty()) {
            append(role, message);
            return;
        }
        ShaftAssistantChatState.Message last = messages.get(messages.size() - 1);
        last.role = normalizedRole(role);
        last.markdown = message;
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

    private JComponent fallbackScrollPane(Color transcriptBackground) {
        JBScrollPane scrollPane = new JBScrollPane(fallbackPanel);
        scrollPane.setBorder(JBUI.Borders.empty());
        scrollPane.setBackground(transcriptBackground);
        scrollPane.getViewport().setOpaque(true);
        scrollPane.getViewport().setBackground(transcriptBackground);
        return scrollPane;
    }

    private static MarkdownJCEFHtmlPanel createMarkdownPanel(Project project, VirtualFile markdownFile) {
        if (project == null || Proxy.isProxyClass(project.getClass())) {
            return null;
        }
        try {
            if (!JBCefApp.isSupported()) {
                return null;
            }
            MarkdownJCEFHtmlPanel panel = new MarkdownJCEFHtmlPanel(project, markdownFile);
            Disposer.register(project, panel);
            return panel;
        } catch (LinkageError | RuntimeException exception) {
            return null;
        }
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
        String transcriptBackground = color("TextArea.background", "#ffffff");
        String assistantBackground = color("TextArea.background", "#ffffff");
        String foreground = color("TextArea.foreground", "#202020");
        String border = color("Component.borderColor", "#d0d7de");
        String userBackground = color("Panel.selectionBackground", "#2563eb");
        String userForeground = color("Panel.selectionForeground", "#ffffff");
        String codeBackground = color("EditorPane.background", transcriptBackground);
        return """
                <html>
                <head>
                  <meta charset="UTF-8">
                  <style>
                    body { font-family: '%s'; font-size: %dpt; color: %s; background: %s; margin: 0; }
                    .shaft-chat { background: %s; padding: 8px; }
                    .shaft-chat-row { clear: both; margin: 0 0 10px 0; width: 100%%; }
                    .shaft-chat-row.user, .shaft-chat-row-user { text-align: right; }
                    .shaft-chat-row.assistant, .shaft-chat-row-assistant { text-align: left; }
                    .shaft-chat-bubble {
                        display: inline-block;
                        box-sizing: border-box;
                        max-width: 88%%;
                        padding: 9px 11px;
                        border-radius: 16px;
                        text-align: left;
                        vertical-align: top;
                    }
                    .shaft-chat-bubble.user, .shaft-chat-bubble-user {
                        color: %s;
                        background-color: %s;
                    }
                    .shaft-chat-bubble.assistant, .shaft-chat-bubble-assistant {
                        color: %s;
                        background-color: %s;
                        border: 1px solid %s;
                    }
                    .shaft-chat-hint {
                        clear: both;
                        margin: 2px 0 0 0;
                        color: %s;
                        background: %s;
                        text-align: left;
                    }
                    p, ul, ol, h1, h2, h3, h4, h5, h6 { margin-top: 0; }
                    p:last-child { margin-bottom: 0; }
                    hr { border: 0; border-top: 1px solid %s; margin: 10px 0; }
                    code { font-family: 'JetBrains Mono', 'Consolas', 'Monospaced', monospace; }
                    .shaft-code-block {
                        margin: 8px 0;
                        border: 1px solid %s;
                        border-radius: 8px;
                        background: %s;
                        overflow: hidden;
                    }
                    .shaft-code-toolbar {
                        padding: 4px 6px;
                        text-align: right;
                        border-bottom: 1px solid %s;
                    }
                    .shaft-code-copy {
                        display: inline-block;
                        box-sizing: border-box;
                        width: 24px;
                        height: 24px;
                        padding: 3px;
                        border: 1px solid %s;
                        border-radius: 6px;
                        color: %s;
                        background: %s;
                        cursor: pointer;
                        text-decoration: none;
                        text-align: center;
                        line-height: 16px;
                        vertical-align: middle;
                    }
                    .shaft-code-copy svg {
                        width: 16px;
                        height: 16px;
                        stroke: currentColor;
                        vertical-align: middle;
                    }
                    .shaft-code-copy-icon {
                        font-family: 'JetBrains Mono', 'Consolas', 'Monospaced', monospace;
                        font-size: 15px;
                        line-height: 16px;
                    }
                    .shaft-code-copy[data-copied="true"] {
                        border-color: %s;
                    }
                    pre {
                        margin: 0;
                        padding: 9px 10px;
                        overflow: auto;
                        white-space: pre-wrap;
                        color: %s;
                        background: %s;
                    }
                  </style>
                  <script>
                    document.addEventListener('click', function(event) {
                      var button = event.target.closest('.shaft-code-copy');
                      if (!button) return;
                      event.preventDefault();
                      var code = button.getAttribute('data-copy-code') || '';
                      var done = function() {
                        button.setAttribute('data-copied', 'true');
                        window.setTimeout(function() { button.removeAttribute('data-copied'); }, 1200);
                      };
                      if (navigator.clipboard && navigator.clipboard.writeText) {
                        navigator.clipboard.writeText(code).then(done);
                        return;
                      }
                      var textarea = document.createElement('textarea');
                      textarea.value = code;
                      document.body.appendChild(textarea);
                      textarea.select();
                      document.execCommand('copy');
                      textarea.remove();
                      done();
                    });
                  </script>
                </head>
                <body><div class="shaft-chat">%s</div></body>
                </html>
                """.formatted(
                cssString(fontFamily()),
                Math.max(11, pane.getFont() == null ? 12 : pane.getFont().getSize()),
                foreground,
                transcriptBackground,
                transcriptBackground,
                userForeground,
                userBackground,
                foreground,
                assistantBackground,
                border,
                foreground,
                transcriptBackground,
                border,
                border,
                codeBackground,
                border,
                border,
                foreground,
                assistantBackground,
                foreground,
                foreground,
                codeBackground,
                value);
    }

    private String convertMarkdown(String value) {
        String safeValue = value == null ? "" : value;
        try {
            markdownFile.setContent(this, safeValue, false);
            String html = MarkdownUtil.INSTANCE.generateMarkdownHtml(markdownFile, safeValue, project);
            return addCodeCopyButtons(bodyContent(html));
        } catch (LinkageError | RuntimeException exception) {
            return addCodeCopyButtons(COMMONMARK_HTML.render(COMMONMARK.parse(safeValue)));
        }
    }

    private void refresh() {
        renderedHtml = toHtml(renderMessages() + renderInitialMessage());
        if (markdownPanel != null) {
            markdownPanel.setHtml(renderedHtml, 0, markdownFile);
        } else {
            renderFallbackTranscript();
        }
    }

    private void renderFallbackTranscript() {
        Color transcriptBackground = resolvedColor("TextArea.background", Color.WHITE);
        fallbackPanel.removeAll();
        fallbackPanel.setBackground(transcriptBackground);
        for (ShaftAssistantChatState.Message message : messages) {
            fallbackPanel.add(fallbackMessage(message.role, message.markdown));
        }
        fallbackPanel.add(fallbackHint());
        fallbackPanel.revalidate();
        fallbackPanel.repaint();
    }

    private JComponent fallbackMessage(String role, String markdown) {
        boolean user = USER_ROLE.equals(normalizedRole(role));
        Color background = user
                ? resolvedColor("Panel.selectionBackground", new Color(0x2563EB))
                : resolvedColor("TextArea.background", Color.WHITE);
        Color foreground = user
                ? resolvedColor("Panel.selectionForeground", Color.WHITE)
                : resolvedColor("TextArea.foreground", new Color(0x202020));
        Color border = user ? null : resolvedColor("Component.borderColor", new Color(0xD0D7DE));
        JPanel row = new JPanel(new BorderLayout());
        row.setOpaque(false);
        row.setBorder(JBUI.Borders.emptyBottom(10));
        RoundedBubblePanel bubble = new RoundedBubblePanel(background, border, 18);
        bubble.setLayout(new BorderLayout());
        bubble.setBorder(JBUI.Borders.empty(9, 11));
        bubble.add(user && isPlainSingleLine(markdown)
                ? fallbackPlainLabel(markdown, foreground)
                : fallbackHtmlPane(convertMarkdown(markdown), foreground, background), BorderLayout.CENTER);
        row.add(bubble, user ? BorderLayout.EAST : BorderLayout.WEST);
        row.setMaximumSize(new Dimension(Integer.MAX_VALUE, row.getPreferredSize().height));
        return row;
    }

    private static boolean isPlainSingleLine(String value) {
        return value != null
                && !value.contains("\n")
                && !value.contains("\r")
                && !value.matches(".*[`*_#\\[\\]<>|!].*");
    }

    private static JLabel fallbackPlainLabel(String text, Color foreground) {
        JLabel label = new JLabel(text);
        label.setForeground(foreground);
        return label;
    }

    private JComponent fallbackHint() {
        JPanel row = new JPanel(new BorderLayout());
        row.setOpaque(false);
        JLabel label = new JLabel(INITIAL_MESSAGE);
        label.setForeground(resolvedColor("TextArea.foreground", new Color(0x202020)));
        row.add(label, BorderLayout.WEST);
        row.setMaximumSize(new Dimension(Integer.MAX_VALUE, row.getPreferredSize().height));
        return row;
    }

    private JEditorPane fallbackHtmlPane(String html, Color foreground, Color background) {
        JEditorPane htmlPane = new JEditorPane();
        htmlPane.setContentType("text/html");
        htmlPane.setEditorKit(new HTMLEditorKit());
        htmlPane.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
        htmlPane.setEditable(false);
        htmlPane.setOpaque(false);
        htmlPane.setBorder(JBUI.Borders.empty());
        htmlPane.setForeground(foreground);
        htmlPane.addHyperlinkListener(AssistantTranscriptView::copyCodeFromFallbackLink);
        htmlPane.setText(toFallbackHtml(html, foreground, background));
        htmlPane.setCaretPosition(0);
        return htmlPane;
    }

    private String toFallbackHtml(String value, Color foreground, Color background) {
        String codeBackground = color("EditorPane.background", color("TextArea.background", "#ffffff"));
        String border = color("Component.borderColor", "#d0d7de");
        return """
                <html>
                <head>
                  <meta charset="UTF-8">
                  <style>
                    body { font-family: '%s'; font-size: %dpt; color: %s; background: %s; margin: 0; }
                    p, ul, ol, h1, h2, h3, h4, h5, h6 { margin-top: 0; }
                    p:last-child { margin-bottom: 0; }
                    hr { border: 0; border-top: 1px solid %s; margin: 10px 0; }
                    code { font-family: 'JetBrains Mono', 'Consolas', 'Monospaced', monospace; }
                    .shaft-code-block { margin: 8px 0; border: 1px solid %s; background: %s; }
                    .shaft-code-toolbar { padding: 4px 6px; text-align: right; border-bottom: 1px solid %s; }
                    .shaft-code-copy {
                        display: inline-block;
                        width: 24px;
                        height: 24px;
                        padding: 3px;
                        color: %s;
                        background: %s;
                        border: 1px solid %s;
                        text-decoration: none;
                        text-align: center;
                        line-height: 16px;
                    }
                    .shaft-code-copy-icon { font-family: Monospaced; font-size: 15px; line-height: 16px; }
                    pre { margin: 0; padding: 9px 10px; white-space: pre-wrap; color: %s; background: %s; }
                  </style>
                </head>
                <body>%s</body>
                </html>
                """.formatted(
                cssString(fontFamily()),
                Math.max(11, pane.getFont() == null ? 12 : pane.getFont().getSize()),
                hex(foreground),
                hex(background),
                border,
                border,
                codeBackground,
                border,
                color("Button.foreground", "#202020"),
                color("Button.background", "#f6f8fa"),
                border,
                color("TextArea.foreground", "#202020"),
                codeBackground,
                value);
    }

    private String renderMessages() {
        StringBuilder rendered = new StringBuilder();
        for (ShaftAssistantChatState.Message message : messages) {
            rendered.append(renderMessage(message.role, message.markdown));
        }
        return rendered.toString();
    }

    private String renderMessage(String role, String markdown) {
        boolean user = USER_ROLE.equals(normalizedRole(role));
        String roleClass = roleClass(user);
        String alignment = user ? "right" : "left";
        String background = user
                ? color("Panel.selectionBackground", "#2563eb")
                : color("TextArea.background", "#ffffff");
        String foreground = user
                ? color("Panel.selectionForeground", "#ffffff")
                : color("TextArea.foreground", "#202020");
        String border = user ? "" : "border:1px solid " + color("Component.borderColor", "#d0d7de") + ";";
        String rowStyle = "clear:both;margin:0 0 10px 0;width:100%;text-align:" + alignment + ";";
        String bubbleStyle = "color:" + foreground
                + ";background-color:" + background
                + ";" + border
                + "border-radius:16px;padding:9px 11px;text-align:left;";
        String bubble = user
                ? renderUserBubble(markdown, background, bubbleStyle, roleClass)
                : "<table cellspacing=\"0\" cellpadding=\"0\"><tr><td bgcolor=\"" + background
                + "\" class=\"shaft-chat-bubble " + roleClass + " shaft-chat-bubble-" + roleClass
                + "\" style=\"" + bubbleStyle + "\">"
                + convertMarkdown(markdown)
                + "</td></tr></table>";
        return "<div class=\"shaft-chat-row " + roleClass + " shaft-chat-row-" + roleClass
                + "\" style=\"" + rowStyle + "\">"
                + "<table width=\"100%\" cellspacing=\"0\" cellpadding=\"0\"><tr><td align=\"" + alignment + "\">"
                + bubble
                + "</td></tr></table></div>";
    }

    private String renderUserBubble(String markdown, String background, String bubbleStyle, String roleClass) {
        return "<div class=\"shaft-chat-bubble " + roleClass + " shaft-chat-bubble-" + roleClass
                + "\" style=\"display:inline-block;box-sizing:border-box;max-width:88%;" + bubbleStyle + "\">"
                + convertMarkdown(markdown)
                + "</div>";
    }

    private String renderInitialMessage() {
        return "<div class=\"shaft-chat-hint\">" + convertMarkdown(INITIAL_MESSAGE) + "</div>";
    }

    private String addCodeCopyButtons(String html) {
        StringBuilder result = new StringBuilder();
        String buttonStyle = "color:" + color("Button.foreground", "#202020")
                + ";background-color:" + color("Button.background", "#f6f8fa")
                + ";border:1px solid " + color("Component.borderColor", "#d0d7de")
                + ";display:inline-block;width:24px;height:24px;padding:3px;text-decoration:none;"
                + "text-align:center;line-height:16px;vertical-align:middle;";
        int offset = 0;
        while (offset < html.length()) {
            int preStart = indexOfIgnoreCase(html, "<pre", offset);
            if (preStart < 0) {
                result.append(html, offset, html.length());
                break;
            }
            int preEnd = indexOfIgnoreCase(html, "</pre>", preStart);
            if (preEnd < 0) {
                result.append(html, offset, html.length());
                break;
            }
            preEnd += "</pre>".length();
            String codeBlock = html.substring(preStart, preEnd);
            String code = stripTags(codeBlock).stripTrailing();
            String language = languageFromCodeBlock(codeBlock);
            result.append(html, offset, preStart)
                    .append("<div class=\"shaft-code-block\"><div class=\"shaft-code-toolbar\">")
                    .append("<a class=\"shaft-code-copy\" href=\"shaft-copy-code:")
                    .append(escapeAttribute(urlEncode(code)))
                    .append("\" role=\"button\" style=\"")
                    .append(buttonStyle)
                    .append("\" ")
                    .append("aria-label=\"Copy code\" title=\"Copy code\" data-copy-code=\"")
                    .append(escapeAttribute(code))
                    .append("\">")
                    .append(copyIconMarkup())
                    .append("</a>")
                    .append("</div>")
                    .append(highlightedCodeBlock(codeBlock, code, language))
                    .append("</div>");
            offset = preEnd;
        }
        return result.toString();
    }

    private String highlightedCodeBlock(String originalCodeBlock, String code, String languageLabel) {
        Language language = languageForFence(languageLabel);
        if (language == null) {
            String highlighted = highlightedBySyntaxHighlighter(code, languageLabel);
            if (!highlighted.isBlank()) {
                return "<pre class=\"shaft-code-highlighted language-" + escapeAttribute(languageLabel) + "\"><code>"
                        + highlighted
                        + "</code></pre>";
            }
            return originalCodeBlock;
        }
        try {
            StringBuilder highlighted = new StringBuilder();
            Project highlightProject = project == null || Proxy.isProxyClass(project.getClass())
                    ? ProjectManager.getInstance().getDefaultProject()
                    : project;
            HtmlSyntaxInfoUtil.appendHighlightedByLexerAndEncodedAsHtmlCodeSnippet(
                    highlighted,
                    highlightProject,
                    language,
                    code,
                    false,
                    1.0F);
            return "<pre class=\"shaft-code-highlighted language-" + escapeAttribute(languageLabel) + "\"><code>"
                    + highlighted
                    + "</code></pre>";
        } catch (LinkageError | RuntimeException exception) {
            String highlighted = highlightedBySyntaxHighlighter(code, languageLabel);
            if (!highlighted.isBlank()) {
                return "<pre class=\"shaft-code-highlighted language-" + escapeAttribute(languageLabel) + "\"><code>"
                        + highlighted
                        + "</code></pre>";
            }
            return originalCodeBlock;
        }
    }

    private static String languageFromCodeBlock(String codeBlock) {
        Matcher matcher = LANGUAGE_CLASS.matcher(codeBlock);
        return matcher.find() ? matcher.group(1).toLowerCase(Locale.ROOT) : "";
    }

    private static Language languageForFence(String languageLabel) {
        return switch (normalizedLanguage(languageLabel)) {
            case "java", "jav" -> findLanguage("JAVA", "Java");
            case "json" -> findLanguage("JSON");
            case "xml" -> findLanguage("XML");
            case "html", "xhtml" -> findLanguage("HTML", "XHTML");
            case "js", "javascript" -> findLanguage("JavaScript", "JS");
            case "ts", "typescript" -> findLanguage("TypeScript");
            case "css" -> findLanguage("CSS");
            case "yaml", "yml" -> findLanguage("yaml", "YAML");
            case "sql" -> findLanguage("SQL");
            case "sh", "shell", "bash" -> findLanguage("Shell Script", "Shell");
            default -> null;
        };
    }

    private static String normalizedLanguage(String languageLabel) {
        return languageLabel == null ? "" : languageLabel.trim().toLowerCase(Locale.ROOT);
    }

    private static Language findLanguage(String... ids) {
        for (String id : ids) {
            Language language = Language.findLanguageByID(id);
            if (language != null) {
                return language;
            }
        }
        for (Language language : Language.getRegisteredLanguages()) {
            for (String id : ids) {
                if (language.getID().equalsIgnoreCase(id) || language.getDisplayName().equalsIgnoreCase(id)) {
                    return language;
                }
            }
        }
        return null;
    }

    private String highlightedBySyntaxHighlighter(String code, String languageLabel) {
        FileType fileType = fileTypeForFence(languageLabel);
        if (fileType == null) {
            return highlightedByIntelliJPaletteFallback(code, languageLabel);
        }
        try {
            String extension = extensionForFence(languageLabel);
            SyntaxHighlighter highlighter = SyntaxHighlighterFactory.getSyntaxHighlighter(
                    fileType,
                    null,
                    new LightVirtualFile("snippet." + extension, fileType, code));
            if (highlighter == null) {
                return highlightedByIntelliJPaletteFallback(code, languageLabel);
            }
            String highlighted = highlightedByLexer(code, highlighter);
            return highlighted.isBlank() ? highlightedByIntelliJPaletteFallback(code, languageLabel) : highlighted;
        } catch (LinkageError | RuntimeException exception) {
            return highlightedByIntelliJPaletteFallback(code, languageLabel);
        }
    }

    private static FileType fileTypeForFence(String languageLabel) {
        String extension = extensionForFence(languageLabel);
        if (extension.isBlank()) {
            return null;
        }
        FileType fileType = FileTypeManager.getInstance().getFileTypeByExtension(extension);
        return "UNKNOWN".equalsIgnoreCase(fileType.getName()) ? null : fileType;
    }

    private static String extensionForFence(String languageLabel) {
        return switch (normalizedLanguage(languageLabel)) {
            case "java", "jav" -> "java";
            case "json" -> "json";
            case "xml" -> "xml";
            case "html", "xhtml" -> "html";
            case "js", "javascript" -> "js";
            case "ts", "typescript" -> "ts";
            case "css" -> "css";
            case "yaml", "yml" -> "yaml";
            case "sql" -> "sql";
            case "sh", "shell", "bash" -> "sh";
            default -> "";
        };
    }

    private static String highlightedByLexer(String code, SyntaxHighlighter highlighter) {
        Lexer lexer = highlighter.getHighlightingLexer();
        lexer.start(code);
        StringBuilder highlighted = new StringBuilder();
        EditorColorsScheme scheme = EditorColorsManager.getInstance() == null
                ? null
                : EditorColorsManager.getInstance().getGlobalScheme();
        boolean styled = false;
        while (lexer.getTokenType() != null) {
            String token = code.substring(lexer.getTokenStart(), lexer.getTokenEnd());
            TextAttributes attributes = attributesFor(highlighter.getTokenHighlights(lexer.getTokenType()), scheme);
            if (attributes == null || attributes.getForegroundColor() == null) {
                highlighted.append(escapeHtml(token));
            } else {
                styled = true;
                highlighted.append("<span style=\"")
                        .append(styleFor(attributes))
                        .append("\">")
                        .append(escapeHtml(token))
                        .append("</span>");
            }
            lexer.advance();
        }
        return styled ? highlighted.toString() : "";
    }

    private static TextAttributes attributesFor(TextAttributesKey[] keys, EditorColorsScheme scheme) {
        if (keys == null || scheme == null) {
            return null;
        }
        for (TextAttributesKey key : keys) {
            if (key == null) {
                continue;
            }
            TextAttributes attributes = scheme.getAttributes(key);
            if (attributes != null && attributes.getForegroundColor() != null) {
                return attributes;
            }
        }
        return null;
    }

    private static String highlightedByIntelliJPaletteFallback(String code, String languageLabel) {
        return switch (normalizedLanguage(languageLabel)) {
            case "java", "jav" -> highlightJavaFallback(code);
            case "json" -> highlightJsonFallback(code);
            case "xml", "html", "xhtml" -> highlightXmlFallback(code);
            default -> "";
        };
    }

    private static String highlightJavaFallback(String code) {
        CodePalette palette = codePalette();
        StringBuilder html = new StringBuilder();
        int offset = 0;
        while (offset < code.length()) {
            char current = code.charAt(offset);
            if (startsWith(code, offset, "//")) {
                int end = indexOfOrLength(code, "\n", offset);
                appendStyled(html, code.substring(offset, end), palette.comment());
                offset = end;
            } else if (startsWith(code, offset, "/*")) {
                int end = code.indexOf("*/", offset + 2);
                end = end < 0 ? code.length() : end + 2;
                appendStyled(html, code.substring(offset, end), palette.comment());
                offset = end;
            } else if (current == '"' || current == '\'') {
                int end = quotedTokenEnd(code, offset, current);
                appendStyled(html, code.substring(offset, end), palette.string());
                offset = end;
            } else if (current == '@') {
                int end = identifierEnd(code, offset + 1);
                appendStyled(html, code.substring(offset, end), palette.annotation());
                offset = end;
            } else if (Character.isDigit(current)) {
                int end = numberEnd(code, offset);
                appendStyled(html, code.substring(offset, end), palette.number());
                offset = end;
            } else if (Character.isJavaIdentifierStart(current)) {
                int end = identifierEnd(code, offset);
                String token = code.substring(offset, end);
                if (JAVA_KEYWORDS.contains(token)) {
                    appendStyled(html, token, palette.keyword());
                } else if (Character.isUpperCase(token.charAt(0))) {
                    appendStyled(html, token, palette.type());
                } else {
                    html.append(escapeHtml(token));
                }
                offset = end;
            } else {
                html.append(escapeHtml(String.valueOf(current)));
                offset++;
            }
        }
        return html.toString();
    }

    private static String highlightJsonFallback(String code) {
        CodePalette palette = codePalette();
        StringBuilder html = new StringBuilder();
        int offset = 0;
        while (offset < code.length()) {
            char current = code.charAt(offset);
            if (current == '"') {
                int end = quotedTokenEnd(code, offset, '"');
                String token = code.substring(offset, end);
                appendStyled(html, token, followedByColon(code, end) ? palette.property() : palette.string());
                offset = end;
            } else if (current == '-' || Character.isDigit(current)) {
                int end = jsonNumberEnd(code, offset);
                appendStyled(html, code.substring(offset, end), palette.number());
                offset = end;
            } else if (startsWithWord(code, offset, "true")
                    || startsWithWord(code, offset, "false")
                    || startsWithWord(code, offset, "null")) {
                int end = identifierEnd(code, offset);
                appendStyled(html, code.substring(offset, end), palette.keyword());
                offset = end;
            } else {
                html.append(escapeHtml(String.valueOf(current)));
                offset++;
            }
        }
        return html.toString();
    }

    private static String highlightXmlFallback(String code) {
        CodePalette palette = codePalette();
        StringBuilder html = new StringBuilder();
        int offset = 0;
        while (offset < code.length()) {
            if (startsWith(code, offset, "<!--")) {
                int end = code.indexOf("-->", offset + 4);
                end = end < 0 ? code.length() : end + 3;
                appendStyled(html, code.substring(offset, end), palette.comment());
                offset = end;
            } else if (code.charAt(offset) == '<') {
                int end = code.indexOf('>', offset + 1);
                end = end < 0 ? code.length() : end + 1;
                html.append(highlightXmlTagFallback(code.substring(offset, end), palette));
                offset = end;
            } else {
                html.append(escapeHtml(String.valueOf(code.charAt(offset))));
                offset++;
            }
        }
        return html.toString();
    }

    private static String highlightXmlTagFallback(String tag, CodePalette palette) {
        StringBuilder html = new StringBuilder();
        int offset = 0;
        boolean tagNamePending = true;
        while (offset < tag.length()) {
            char current = tag.charAt(offset);
            if (current == '"' || current == '\'') {
                int end = quotedTokenEnd(tag, offset, current);
                appendStyled(html, tag.substring(offset, end), palette.string());
                offset = end;
            } else if (isXmlNameStart(current)) {
                int end = xmlNameEnd(tag, offset);
                appendStyled(html, tag.substring(offset, end), tagNamePending ? palette.keyword() : palette.property());
                tagNamePending = false;
                offset = end;
            } else {
                html.append(escapeHtml(String.valueOf(current)));
                offset++;
            }
        }
        return html.toString();
    }

    private static void appendStyled(StringBuilder html, String text, String color) {
        html.append("<span style=\"color:")
                .append(color)
                .append(";\">")
                .append(escapeHtml(text))
                .append("</span>");
    }

    private static CodePalette codePalette() {
        return isDarkBackground(resolvedColor("EditorPane.background", resolvedColor("TextArea.background", Color.WHITE)))
                ? new CodePalette("#cf8e6d", "#6aab73", "#2aacb8", "#7a7e85", "#b3ae60", "#c77dbb", "#56a8f5")
                : new CodePalette("#000080", "#067d17", "#1750eb", "#8c8c8c", "#808000", "#000000", "#871094");
    }

    private static boolean isDarkBackground(Color color) {
        double luminance = (0.2126 * color.getRed()) + (0.7152 * color.getGreen()) + (0.0722 * color.getBlue());
        return luminance < 128;
    }

    private static boolean startsWith(String value, int offset, String prefix) {
        return offset >= 0
                && offset + prefix.length() <= value.length()
                && value.regionMatches(offset, prefix, 0, prefix.length());
    }

    private static boolean startsWithWord(String value, int offset, String word) {
        return startsWith(value, offset, word)
                && (offset + word.length() >= value.length()
                || !Character.isJavaIdentifierPart(value.charAt(offset + word.length())));
    }

    private static int indexOfOrLength(String value, String needle, int offset) {
        int index = value.indexOf(needle, offset);
        return index < 0 ? value.length() : index;
    }

    private static int quotedTokenEnd(String value, int offset, char quote) {
        int index = offset + 1;
        boolean escaped = false;
        while (index < value.length()) {
            char current = value.charAt(index++);
            if (escaped) {
                escaped = false;
            } else if (current == '\\') {
                escaped = true;
            } else if (current == quote) {
                break;
            }
        }
        return index;
    }

    private static int identifierEnd(String value, int offset) {
        int index = offset;
        while (index < value.length() && Character.isJavaIdentifierPart(value.charAt(index))) {
            index++;
        }
        return index;
    }

    private static int numberEnd(String value, int offset) {
        int index = offset;
        while (index < value.length()) {
            char current = value.charAt(index);
            if (Character.isLetterOrDigit(current) || current == '_' || current == '.' || current == '+' || current == '-') {
                index++;
            } else {
                break;
            }
        }
        return index;
    }

    private static int jsonNumberEnd(String value, int offset) {
        int index = offset;
        while (index < value.length()) {
            char current = value.charAt(index);
            if (Character.isDigit(current) || current == '-' || current == '+' || current == '.' || current == 'e' || current == 'E') {
                index++;
            } else {
                break;
            }
        }
        return index == offset ? offset + 1 : index;
    }

    private static boolean followedByColon(String value, int offset) {
        int index = offset;
        while (index < value.length() && Character.isWhitespace(value.charAt(index))) {
            index++;
        }
        return index < value.length() && value.charAt(index) == ':';
    }

    private static boolean isXmlNameStart(char value) {
        return Character.isLetter(value) || value == '_' || value == ':';
    }

    private static int xmlNameEnd(String value, int offset) {
        int index = offset;
        while (index < value.length()) {
            char current = value.charAt(index);
            if (Character.isLetterOrDigit(current) || current == '_' || current == '-' || current == '.' || current == ':') {
                index++;
            } else {
                break;
            }
        }
        return index;
    }

    private record CodePalette(
            String keyword,
            String string,
            String number,
            String comment,
            String annotation,
            String type,
            String property) {
    }

    private static final class RoundedBubblePanel extends JPanel {
        private final Color fill;
        private final Color stroke;
        private final int arc;

        private RoundedBubblePanel(Color fill, Color stroke, int arc) {
            this.fill = fill;
            this.stroke = stroke;
            this.arc = arc;
            setOpaque(false);
        }

        @Override
        protected void paintComponent(Graphics graphics) {
            Graphics2D graphics2D = (Graphics2D) graphics.create();
            try {
                graphics2D.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                graphics2D.setColor(fill);
                graphics2D.fillRoundRect(0, 0, getWidth() - 1, getHeight() - 1, arc, arc);
                if (stroke != null) {
                    graphics2D.setColor(stroke);
                    graphics2D.drawRoundRect(0, 0, getWidth() - 1, getHeight() - 1, arc, arc);
                }
            } finally {
                graphics2D.dispose();
            }
            super.paintComponent(graphics);
        }
    }

    private static String styleFor(TextAttributes attributes) {
        StringBuilder style = new StringBuilder("color:")
                .append(hex(attributes.getForegroundColor()))
                .append(";");
        int fontType = attributes.getFontType();
        if ((fontType & Font.BOLD) != 0) {
            style.append("font-weight:bold;");
        }
        if ((fontType & Font.ITALIC) != 0) {
            style.append("font-style:italic;");
        }
        return style.toString();
    }

    private static String copyIconMarkup() {
        return """
                <svg width="16" height="16" viewBox="0 0 16 16" aria-hidden="true" focusable="false" xmlns="http://www.w3.org/2000/svg">
                  <path d="M5.5 5.5H11.5V12.5H5.5V5.5ZM3.5 3.5H9.5V5.5M3.5 3.5V10.5H5.5" fill="none" stroke="currentColor" stroke-width="1.3" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
                <span aria-hidden="true" class="shaft-code-copy-icon" style="font-family:Monospaced;font-size:15px;line-height:16px;">&#x2398;</span>
                """.strip();
    }

    private static String bodyContent(String html) {
        int bodyStart = indexOfIgnoreCase(html, "<body", 0);
        if (bodyStart < 0) {
            return html;
        }
        int bodyOpenEnd = html.indexOf('>', bodyStart);
        int bodyEnd = indexOfIgnoreCase(html, "</body>", Math.max(bodyOpenEnd, bodyStart));
        if (bodyOpenEnd < 0 || bodyEnd < 0) {
            return html;
        }
        return html.substring(bodyOpenEnd + 1, bodyEnd);
    }

    private static String stripTags(String html) {
        String text = html.replaceAll("(?is)<[^>]+>", "");
        return text.replace("&lt;", "<")
                .replace("&gt;", ">")
                .replace("&quot;", "\"")
                .replace("&#39;", "'")
                .replace("&#x27;", "'")
                .replace("&amp;", "&");
    }

    private static int indexOfIgnoreCase(String value, String needle, int offset) {
        for (int index = Math.max(0, offset); index <= value.length() - needle.length(); index++) {
            if (value.regionMatches(true, index, needle, 0, needle.length())) {
                return index;
            }
        }
        return -1;
    }

    private static void copyCodeFromFallbackLink(HyperlinkEvent event) {
        if (event.getEventType() != HyperlinkEvent.EventType.ACTIVATED) {
            return;
        }
        String description = event.getDescription();
        if (description == null || !description.startsWith("shaft-copy-code:")) {
            return;
        }
        String encoded = description.substring("shaft-copy-code:".length());
        CopyPasteManager.getInstance().setContents(new StringSelection(urlDecode(encoded)));
    }

    private static String urlEncode(String value) {
        return URLEncoder.encode(value, StandardCharsets.UTF_8);
    }

    private static String urlDecode(String value) {
        return URLDecoder.decode(value, StandardCharsets.UTF_8);
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
        return hex(resolved);
    }

    private static Color resolvedColor(String uiKey, Color fallback) {
        Color resolved = UIManager.getColor(uiKey);
        return resolved == null ? fallback : resolved;
    }

    private static String hex(Color color) {
        return "#%02x%02x%02x".formatted(color.getRed(), color.getGreen(), color.getBlue());
    }

    private static String cssString(String value) {
        return value.replace("\\", "\\\\").replace("'", "\\'");
    }

    private static String escapeAttribute(String value) {
        return value.replace("&", "&amp;")
                .replace("\"", "&quot;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\r\n", "&#10;")
                .replace("\n", "&#10;")
                .replace("\r", "&#10;");
    }

    private static String escapeHtml(String value) {
        return value.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;");
    }
}
