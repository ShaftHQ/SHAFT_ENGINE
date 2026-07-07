package com.shaft.intellij.ui;

import com.intellij.ide.ui.LafManagerListener;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.EditorColorsScheme;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.openapi.ide.CopyPasteManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.Disposer;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.util.ui.JBUI;

import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.UIManager;
import javax.swing.BoxLayout;
import javax.swing.SwingUtilities;
import javax.swing.BorderFactory;
import javax.swing.event.HyperlinkEvent;
import javax.swing.text.html.HTMLEditorKit;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.datatransfer.StringSelection;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Markdown-rendered Assistant transcript with copyable source text.
 */
final class AssistantTranscriptView extends JPanel {
    private static final int MAX_AGENT_CONTEXT_CHARACTERS = 16_000;
    private static final Pattern LANGUAGE_CLASS = Pattern.compile("(?i)\\blanguage-([a-z0-9_+.#-]+)");
    private static final Pattern UNORDERED_LIST_ITEM = Pattern.compile("^[-*+]\\s+(.+)$");
    private static final Pattern ORDERED_LIST_ITEM = Pattern.compile("^\\d+[.)]\\s+(.+)$");
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
    static final String TRANSCRIPT_ROLE_PROPERTY = "shaft.transcript.role";
    static final String TRANSCRIPT_BUBBLE_PROPERTY = "shaft.transcript.bubble";
    static final String TRANSCRIPT_RENDERED_HTML_PROPERTY = "shaft.transcript.renderedHtml";

    private final Project project;
    private final JPanel fallbackPanel;
    private final JBScrollPane fallbackScrollPane;
    private final List<ShaftAssistantChatState.Message> messages = new ArrayList<>();
    private String markdown = "";
    private final LafManagerListener lafListener;
    private Disposable lafConnectionDisposable;
    private int truncationBoundaryIndex = -1;

    AssistantTranscriptView() {
        this(null);
    }

    AssistantTranscriptView(Project project) {
        super(new BorderLayout());
        this.project = project;
        Color borderColor = JBColor.namedColor("Component.borderColor", new Color(0xD0D7DE));
        setBorder(BorderFactory.createLineBorder(borderColor));
        fallbackPanel = new JPanel();
        fallbackPanel.setLayout(new BoxLayout(fallbackPanel, BoxLayout.Y_AXIS));
        fallbackPanel.setBorder(JBUI.Borders.empty(8));
        fallbackPanel.getAccessibleContext().setAccessibleName("Assistant transcript");

        Color transcriptBackground = resolvedColor("TextArea.background", Color.WHITE);
        fallbackPanel.setBackground(transcriptBackground);
        fallbackPanel.setOpaque(true);
        fallbackScrollPane = createFallbackScrollPane(transcriptBackground);
        fallbackScrollPane.getAccessibleContext().setAccessibleName("Assistant transcript");
        add(fallbackScrollPane, BorderLayout.CENTER);

        lafListener = lookAndFeel -> SwingUtilities.invokeLater(this::refresh);

        refresh();
    }

    @Override
    public void addNotify() {
        super.addNotify();
        if (lafConnectionDisposable != null) {
            return;
        }
        lafConnectionDisposable = Disposer.newDisposable("ShaftAssistantTranscriptView.laf");
        ApplicationManager.getApplication().getMessageBus()
                .connect(lafConnectionDisposable)
                .subscribe(LafManagerListener.TOPIC, lafListener);
    }

    @Override
    public void removeNotify() {
        if (lafConnectionDisposable != null) {
            Disposer.dispose(lafConnectionDisposable);
            lafConnectionDisposable = null;
        }
        super.removeNotify();
    }

    String markdown() {
        return markdown;
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

    void setTruncationBoundaryIndex(int index) {
        this.truncationBoundaryIndex = index;
    }

    private JBScrollPane createFallbackScrollPane(Color transcriptBackground) {
        JBScrollPane scrollPane = new JBScrollPane(fallbackPanel);
        scrollPane.setBorder(JBUI.Borders.empty());
        scrollPane.setBackground(transcriptBackground);
        scrollPane.getViewport().setOpaque(true);
        scrollPane.getViewport().setBackground(transcriptBackground);
        return scrollPane;
    }

    private void scrollLatestIntoView() {
        SwingUtilities.invokeLater(() -> {
            JScrollBar vertical = fallbackScrollPane.getVerticalScrollBar();
            vertical.setValue(vertical.getMaximum());
        });
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

    private String convertMarkdown(String value) {
        String safeValue = value == null ? "" : value;
        return addCodeCopyButtons(renderMarkdownBlocks(safeValue));
    }

    private void refresh() {
        renderFallbackTranscript();
        scrollLatestIntoView();
    }

    private void renderFallbackTranscript() {
        Color transcriptBackground = resolvedColor("TextArea.background", Color.WHITE);
        fallbackPanel.removeAll();
        fallbackPanel.setBackground(transcriptBackground);
        for (int i = 0; i < messages.size(); i++) {
            if (i == truncationBoundaryIndex) {
                fallbackPanel.add(createTruncationDivider());
            }
            ShaftAssistantChatState.Message message = messages.get(i);
            fallbackPanel.add(fallbackMessage(message.role, message.markdown));
        }
        fallbackPanel.revalidate();
        fallbackPanel.repaint();
    }

    private JComponent createTruncationDivider() {
        JPanel row = new PreferredHeightRow(new BorderLayout());
        row.setOpaque(false);
        row.setBorder(JBUI.Borders.empty(8, 0));
        row.getAccessibleContext().setAccessibleName("Context truncation indicator");

        Color chipBackground = resolvedColor("Component.background", new Color(0xFFF3CD));
        Color chipForeground = resolvedColor("Component.foreground", new Color(0x664D03));
        Color borderColor = resolvedColor("Component.borderColor", new Color(0xFFEBA0));

        JPanel chipPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
        chipPanel.setOpaque(false);
        chipPanel.setBorder(JBUI.Borders.emptyLeft(8));

        JLabel chipLabel = new JLabel("Earlier messages not included in context");
        chipLabel.setFont(chipLabel.getFont().deriveFont(Math.max(10.0F, chipLabel.getFont().getSize2D() - 1.0F)));
        chipLabel.setForeground(chipForeground);
        chipLabel.setOpaque(true);
        chipLabel.setBackground(chipBackground);
        chipLabel.setBorder(JBUI.Borders.compound(
                JBUI.Borders.customLine(borderColor, 1),
                JBUI.Borders.empty(4, 8)));
        chipLabel.setToolTipText("Context window is limited to " + MAX_AGENT_CONTEXT_CHARACTERS + " characters. " +
                "Earlier messages are not sent to the agent. Start a new chat if needed.");
        chipLabel.getAccessibleContext().setAccessibleName("Context truncation chip");
        chipLabel.getAccessibleContext().setAccessibleDescription(chipLabel.getToolTipText());

        chipPanel.add(chipLabel);
        row.add(chipPanel, BorderLayout.CENTER);
        return row;
    }

    private JComponent fallbackMessage(String role, String markdown) {
        String normalizedRole = normalizedRole(role);
        boolean user = USER_ROLE.equals(normalizedRole);
        Color background = user
                ? resolvedColor("Panel.selectionBackground", new Color(0x2563EB))
                : resolvedColor("Panel.background", new Color(0xF6F8FA));
        Color foreground = user
                ? resolvedColor("Panel.selectionForeground", Color.WHITE)
                : resolvedColor("TextArea.foreground", new Color(0x202020));
        JPanel row = new PreferredHeightRow(new BorderLayout());
        row.setOpaque(false);
        row.setBorder(JBUI.Borders.emptyBottom(10));
        row.putClientProperty(TRANSCRIPT_ROLE_PROPERTY, normalizedRole);
        row.getAccessibleContext().setAccessibleName((user ? "User" : "Assistant") + " assistant message row");
        Color stroke = user ? null : resolvedColor("Component.borderColor", new Color(0xD0D7DE));
        RoundedBubblePanel bubble = new RoundedBubblePanel(background, stroke, 18);
        bubble.setLayout(new BorderLayout());
        bubble.setBorder(JBUI.Borders.empty(9, 11));
        bubble.setBackground(background);
        bubble.setForeground(foreground);
        bubble.putClientProperty(TRANSCRIPT_BUBBLE_PROPERTY, normalizedRole);
        bubble.getAccessibleContext().setAccessibleName((user ? "User" : "Assistant") + " assistant message bubble");
        JEditorPane htmlPane = fallbackHtmlPane(convertMarkdown(markdown), foreground, background);
        htmlPane.putClientProperty(TRANSCRIPT_ROLE_PROPERTY, normalizedRole);
        bubble.add(htmlPane, BorderLayout.CENTER);
        row.add(bubble, user ? BorderLayout.EAST : BorderLayout.WEST);
        return row;
    }

    private JEditorPane fallbackHtmlPane(String html, Color foreground, Color background) {
        JEditorPane htmlPane = new WidthAwareHtmlPane(this::fallbackBubbleContentWidth);
        htmlPane.setContentType("text/html");
        htmlPane.setEditorKit(new HTMLEditorKit());
        htmlPane.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
        htmlPane.setEditable(false);
        htmlPane.setOpaque(false);
        htmlPane.setBorder(JBUI.Borders.empty());
        htmlPane.setForeground(foreground);
        htmlPane.addHyperlinkListener(AssistantTranscriptView::copyCodeFromFallbackLink);
        String rendered = toFallbackHtml(html, foreground, background);
        htmlPane.putClientProperty(TRANSCRIPT_RENDERED_HTML_PROPERTY, rendered);
        htmlPane.setText(rendered);
        htmlPane.setCaretPosition(0);
        return htmlPane;
    }

    private int fallbackBubbleContentWidth() {
        int viewportWidth = fallbackScrollPane == null ? 0 : fallbackScrollPane.getViewport().getWidth();
        if (viewportWidth <= 0) {
            viewportWidth = getWidth();
        }
        if (viewportWidth <= 0) {
            viewportWidth = JBUI.scale(520);
        }
        int rowPadding = JBUI.scale(16);
        int bubblePadding = JBUI.scale(22);
        int bubbleWidth = (int) Math.floor(Math.max(JBUI.scale(180), viewportWidth - rowPadding) * 0.88D);
        return Math.max(JBUI.scale(140), bubbleWidth - bubblePadding);
    }

    private String toFallbackHtml(String value, Color foreground, Color background) {
        boolean dark = isDarkBackground(resolvedColor("TextArea.background", Color.WHITE));
        String codeBackground = hex(dark ? new Color(0x24272b) : new Color(0xf6f8fa));
        String toolbarBackground = hex(dark ? new Color(0x2f3338) : new Color(0xeef2f7));
        String border = hex(dark ? new Color(0x6b7078) : new Color(0xd0d7de));
        String codeForeground = hex(dark ? new Color(0xe5e7eb) : new Color(0x24292f));
        String copyForeground = hex(dark ? new Color(0xced0d6) : new Color(0x57606a));
        return """
                <html>
                <head>
                  <meta charset="UTF-8">
                  <style>
                    body { font-family: '%s'; font-size: %dpt; color: %s; background: %s; margin: 0; width: 100%%; overflow-wrap: anywhere; word-wrap: break-word; }
                    p, ul, ol, h1, h2, h3, h4, h5, h6 { margin-top: 0; }
                    p, li, code, a { overflow-wrap: anywhere; word-wrap: break-word; }
                    p:last-child { margin-bottom: 0; }
                    hr { border: 0; border-top: 1px solid %s; margin: 10px 0; }
                    code { font-family: 'JetBrains Mono', 'Consolas', 'Monospaced', monospace; }
                    .shaft-code-block { margin: 8px 0; border: 1px solid %s; background: %s; max-width: 100%%; }
                    .shaft-code-toolbar {
                        padding: 3px 6px;
                        text-align: right;
                        background: %s;
                        border-bottom: 1px solid %s;
                        line-height: 24px;
                    }
                    .shaft-code-copy {
                        display: inline-block;
                        width: 24px;
                        height: 24px;
                        padding: 0;
                        color: %s;
                        text-decoration: none;
                        text-align: center;
                        line-height: 24px;
                        vertical-align: middle;
                    }
                    .shaft-code-copy-icon { font-family: Monospaced; font-size: 15px; line-height: 24px; }
                    pre {
                        margin: 0;
                        padding: 9px 10px;
                        white-space: pre-wrap;
                        word-wrap: break-word;
                        color: %s;
                        background: %s;
                    }
                    .shaft-code-highlighted { white-space: pre-wrap; word-wrap: break-word; }
                  </style>
                </head>
                <body>%s</body>
                </html>
                """.formatted(
                cssString(fontFamily()),
                baseFontSize(),
                hex(foreground),
                hex(background),
                border,
                border,
                codeBackground,
                toolbarBackground,
                border,
                copyForeground,
                codeForeground,
                codeBackground,
                value);
    }

    private static String renderMarkdownBlocks(String value) {
        String[] lines = value.replace("\r\n", "\n").replace('\r', '\n').split("\n", -1);
        StringBuilder html = new StringBuilder();
        StringBuilder paragraph = new StringBuilder();
        StringBuilder fence = new StringBuilder();
        String fenceLanguage = "";
        boolean inFence = false;
        boolean inUnorderedList = false;
        boolean inOrderedList = false;
        for (int index = 0; index < lines.length; index++) {
            String line = lines[index];
            String trimmed = line.trim();
            if (inFence) {
                if (trimmed.startsWith("```")) {
                    appendCodeBlock(html, fenceLanguage, fence.toString().stripTrailing());
                    fence.setLength(0);
                    fenceLanguage = "";
                    inFence = false;
                } else {
                    fence.append(line).append('\n');
                }
                continue;
            }
            if (trimmed.startsWith("```")) {
                closeOpenLists(html, paragraph, inUnorderedList, inOrderedList);
                fenceLanguage = firstToken(trimmed.substring(3));
                inFence = true;
                inUnorderedList = false;
                inOrderedList = false;
                continue;
            }
            if (trimmed.isBlank()) {
                closeOpenLists(html, paragraph, inUnorderedList, inOrderedList);
                inUnorderedList = false;
                inOrderedList = false;
                continue;
            }
            if (isTableHeader(lines, index)) {
                closeOpenLists(html, paragraph, inUnorderedList, inOrderedList);
                index = appendTable(html, lines, index);
                inUnorderedList = false;
                inOrderedList = false;
                continue;
            }
            int headingLevel = headingLevel(trimmed);
            if (headingLevel > 0) {
                closeOpenLists(html, paragraph, inUnorderedList, inOrderedList);
                String heading = trimmed.substring(headingLevel).trim();
                html.append("<h").append(headingLevel).append(">")
                        .append(renderInline(heading))
                        .append("</h").append(headingLevel).append(">");
                inUnorderedList = false;
                inOrderedList = false;
                continue;
            }
            if (isHorizontalRule(trimmed)) {
                closeOpenLists(html, paragraph, inUnorderedList, inOrderedList);
                html.append("<hr>");
                inUnorderedList = false;
                inOrderedList = false;
                continue;
            }
            Matcher unordered = UNORDERED_LIST_ITEM.matcher(trimmed);
            if (unordered.matches()) {
                appendParagraph(html, paragraph);
                if (inOrderedList) {
                    html.append("</ol>");
                    inOrderedList = false;
                }
                if (!inUnorderedList) {
                    html.append("<ul>");
                    inUnorderedList = true;
                }
                html.append("<li>").append(renderInline(unordered.group(1))).append("</li>");
                continue;
            }
            Matcher ordered = ORDERED_LIST_ITEM.matcher(trimmed);
            if (ordered.matches()) {
                appendParagraph(html, paragraph);
                if (inUnorderedList) {
                    html.append("</ul>");
                    inUnorderedList = false;
                }
                if (!inOrderedList) {
                    html.append("<ol>");
                    inOrderedList = true;
                }
                html.append("<li>").append(renderInline(ordered.group(1))).append("</li>");
                continue;
            }
            closeOpenListsOnly(html, inUnorderedList, inOrderedList);
            if (!paragraph.isEmpty()) {
                paragraph.append(' ');
            }
            paragraph.append(trimmed);
            inUnorderedList = false;
            inOrderedList = false;
        }
        if (inFence) {
            appendCodeBlock(html, fenceLanguage, fence.toString().stripTrailing());
        }
        appendParagraph(html, paragraph);
        closeOpenListsOnly(html, inUnorderedList, inOrderedList);
        return html.toString();
    }

    private static void closeOpenLists(StringBuilder html, StringBuilder paragraph, boolean inUnorderedList, boolean inOrderedList) {
        appendParagraph(html, paragraph);
        closeOpenListsOnly(html, inUnorderedList, inOrderedList);
    }

    private static void closeOpenListsOnly(StringBuilder html, boolean inUnorderedList, boolean inOrderedList) {
        if (inUnorderedList) {
            html.append("</ul>");
        }
        if (inOrderedList) {
            html.append("</ol>");
        }
    }

    private static void appendParagraph(StringBuilder html, StringBuilder paragraph) {
        if (paragraph.isEmpty()) {
            return;
        }
        html.append("<p>").append(renderInline(paragraph.toString())).append("</p>");
        paragraph.setLength(0);
    }

    private static void appendCodeBlock(StringBuilder html, String language, String code) {
        String normalizedLanguage = normalizedLanguage(language);
        html.append("<pre><code");
        if (!normalizedLanguage.isBlank()) {
            html.append(" class=\"language-").append(escapeAttribute(normalizedLanguage)).append("\"");
        }
        html.append(">").append(escapeHtml(code)).append("</code></pre>");
    }

    private static int appendTable(StringBuilder html, String[] lines, int headerIndex) {
        List<String> headers = tableCells(lines[headerIndex]);
        html.append("<table><thead><tr>");
        for (String header : headers) {
            html.append("<th>").append(renderInline(header)).append("</th>");
        }
        html.append("</tr></thead><tbody>");
        int index = headerIndex + 2;
        while (index < lines.length && isTableLine(lines[index].trim())) {
            List<String> cells = tableCells(lines[index]);
            html.append("<tr>");
            for (int cell = 0; cell < headers.size(); cell++) {
                String value = cell < cells.size() ? cells.get(cell) : "";
                html.append("<td>").append(renderInline(value)).append("</td>");
            }
            html.append("</tr>");
            index++;
        }
        html.append("</tbody></table>");
        return index - 1;
    }

    private static boolean isTableHeader(String[] lines, int index) {
        return index + 1 < lines.length
                && isTableLine(lines[index].trim())
                && isTableSeparator(lines[index + 1].trim());
    }

    private static boolean isTableLine(String line) {
        return line.startsWith("|") && line.endsWith("|") && line.indexOf('|', 1) > 0;
    }

    private static boolean isTableSeparator(String line) {
        return isTableLine(line) && line.matches("[|:\\-\\s]+") && line.contains("---");
    }

    private static List<String> tableCells(String line) {
        String value = line.trim();
        if (value.startsWith("|")) {
            value = value.substring(1);
        }
        if (value.endsWith("|")) {
            value = value.substring(0, value.length() - 1);
        }
        String[] cells = value.split("\\|", -1);
        List<String> result = new ArrayList<>();
        for (String cell : cells) {
            result.add(cell.trim());
        }
        return result;
    }

    private static int headingLevel(String trimmed) {
        int level = 0;
        while (level < trimmed.length() && level < 6 && trimmed.charAt(level) == '#') {
            level++;
        }
        return level > 0 && level < trimmed.length() && Character.isWhitespace(trimmed.charAt(level)) ? level : 0;
    }

    private static boolean isHorizontalRule(String trimmed) {
        return trimmed.matches("[-*_]{3,}");
    }

    private static String firstToken(String value) {
        String trimmed = value == null ? "" : value.trim();
        if (trimmed.isBlank()) {
            return "";
        }
        return trimmed.split("\\s+", 2)[0];
    }

    private static String renderInline(String value) {
        StringBuilder html = new StringBuilder();
        String[] parts = value.split("`", -1);
        for (int index = 0; index < parts.length; index++) {
            if (index % 2 == 1) {
                html.append("<code>").append(escapeInlineHtml(parts[index])).append("</code>");
            } else {
                html.append(renderInlineEmphasis(escapeInlineHtml(parts[index])));
            }
        }
        return html.toString();
    }

    private static String renderInlineEmphasis(String escapedText) {
        return escapedText
                .replaceAll("\\*\\*([^*]+)\\*\\*", "<strong>$1</strong>")
                .replaceAll("(?<!\\w)_([^_]+)_(?!\\w)", "<em>$1</em>");
    }

    private String addCodeCopyButtons(String html) {
        StringBuilder result = new StringBuilder();
        Color buttonColor = JBColor.namedColor("Button.foreground", new Color(0x202020));
        String buttonStyle = "color:" + hex(buttonColor)
                + ";display:inline-block;width:24px;height:24px;padding:0;text-decoration:none;"
                + "text-align:center;line-height:24px;vertical-align:middle;";
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
        return renderReadAction(() -> highlightedCodeBlockInReadAction(originalCodeBlock, code, languageLabel));
    }

    private String highlightedCodeBlockInReadAction(String originalCodeBlock, String code, String languageLabel) {
        String highlighted = highlightedByIntelliJPaletteFallback(code, languageLabel);
        if (highlighted.isBlank()) {
            highlighted = highlightedBySyntaxHighlighter(code, languageLabel);
        }
        if (highlighted.isBlank()) {
            return originalCodeBlock;
        }
        return "<pre class=\"shaft-code-highlighted language-" + escapeAttribute(languageLabel) + "\"><code>"
                + highlighted
                + "</code></pre>";
    }

    static <T> T renderReadActionForTest(Supplier<T> supplier) {
        return renderReadAction(supplier);
    }

    private static <T> T renderReadAction(Supplier<T> supplier) {
        if (ApplicationManager.getApplication() == null
                || ApplicationManager.getApplication().isReadAccessAllowed()) {
            return supplier.get();
        }
        return ApplicationManager.getApplication().runReadAction((Computable<T>) supplier::get);
    }

    private static String languageFromCodeBlock(String codeBlock) {
        Matcher matcher = LANGUAGE_CLASS.matcher(codeBlock);
        return matcher.find() ? matcher.group(1).toLowerCase(Locale.ROOT) : "";
    }

    private static String normalizedLanguage(String languageLabel) {
        return languageLabel == null ? "" : languageLabel.trim().toLowerCase(Locale.ROOT);
    }

    private String highlightedBySyntaxHighlighter(String code, String languageLabel) {
        FileType fileType = fileTypeForFence(languageLabel);
        if (fileType == null) {
            return highlightedByIntelliJPaletteFallback(code, languageLabel);
        }
        try {
            SyntaxHighlighter highlighter = SyntaxHighlighterFactory.getSyntaxHighlighter(
                    fileType,
                    null,
                    null);
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
        boolean dark = isDarkBackground(resolvedColor("TextArea.background", Color.WHITE));
        if (dark) {
            return new CodePalette(
                    "#cf8e6d",
                    "#6aab73",
                    "#2aacb8",
                    "#7a7e85",
                    "#b3ae60",
                    "#c77dbb",
                    "#56a8f5");
        } else {
            return new CodePalette(
                    "#000080",
                    "#067d17",
                    "#1750eb",
                    "#8c8c8c",
                    "#808000",
                    "#000000",
                    "#871094");
        }
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

    private static final class WidthAwareHtmlPane extends JEditorPane {
        private final Supplier<Integer> widthSupplier;

        private WidthAwareHtmlPane(Supplier<Integer> widthSupplier) {
            this.widthSupplier = widthSupplier;
        }

        @Override
        public Dimension getPreferredSize() {
            int preferredWidth = widthSupplier.get();
            if (preferredWidth > 0) {
                setSize(new Dimension(preferredWidth, Short.MAX_VALUE));
                Dimension preferred = super.getPreferredSize();
                preferred.width = Math.min(preferred.width, preferredWidth);
                return preferred;
            }
            return super.getPreferredSize();
        }

        @Override
        public Dimension getMaximumSize() {
            Dimension preferred = getPreferredSize();
            return new Dimension(preferred.width, preferred.height);
        }
    }

    private static final class PreferredHeightRow extends JPanel {
        private PreferredHeightRow(LayoutManager layout) {
            super(layout);
        }

        @Override
        public Dimension getMaximumSize() {
            return new Dimension(Integer.MAX_VALUE, getPreferredSize().height);
        }
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
                <span aria-hidden="true" class="shaft-code-copy-icon" style="font-family:Monospaced;font-size:15px;line-height:24px;">&#x2398;</span>
                """.strip();
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

    private static String normalizedRole(String role) {
        if (role == null || role.isBlank()) {
            return UNKNOWN_ROLE;
        }
        return role.trim().toLowerCase();
    }

    private String fontFamily() {
        Font font = getFont();
        if (font == null && fallbackPanel != null) {
            font = fallbackPanel.getFont();
        }
        return font == null ? "Dialog" : font.getFamily();
    }

    private int baseFontSize() {
        Font font = getFont();
        if (font == null && fallbackPanel != null) {
            font = fallbackPanel.getFont();
        }
        return Math.max(11, font == null ? 12 : font.getSize());
    }

    private static Color resolvedColor(String uiKey, Color fallback) {
        Color resolved = JBColor.namedColor(uiKey, fallback);
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

    private static String escapeInlineHtml(String value) {
        StringBuilder html = new StringBuilder(value.length() + Math.max(8, value.length() / 12));
        int runLength = 0;
        for (int index = 0; index < value.length(); index++) {
            char current = value.charAt(index);
            appendEscapedHtml(html, current);
            if (Character.isWhitespace(current)) {
                runLength = 0;
                continue;
            }
            runLength++;
            if (isInlineBreakOpportunity(current) || runLength >= 32) {
                html.append("&#8203;");
                runLength = 0;
            }
        }
        return html.toString();
    }

    private static void appendEscapedHtml(StringBuilder html, char value) {
        switch (value) {
            case '&' -> html.append("&amp;");
            case '<' -> html.append("&lt;");
            case '>' -> html.append("&gt;");
            default -> html.append(value);
        }
    }

    private static boolean isInlineBreakOpportunity(char value) {
        return value == '/' || value == '\\' || value == ':' || value == '.' || value == '-';
    }
}
