package com.shaft.intellij.ui;

import com.intellij.ide.BrowserUtil;
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
import com.intellij.ui.components.JBTextArea;
import com.intellij.util.ui.JBUI;
import com.shaft.intellij.mcp.ShaftPluginExecutor;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollBar;
import javax.swing.UIManager;
import javax.swing.BoxLayout;
import javax.swing.SwingUtilities;
import javax.swing.BorderFactory;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Markdown-rendered Assistant transcript with copyable source text.
 */
final class AssistantTranscriptView extends JPanel {
    private static final int MAX_AGENT_CONTEXT_CHARACTERS = 16_000;
    // Rendered-line-count threshold (measured via FontMetrics against the actual rendered HTML
    // height, not raw markdown characters, so wrapped paragraphs and code blocks count fairly)
    // above which a bubble collapses by default -- issue #3629.
    private static final int COLLAPSE_LINE_THRESHOLD = 40;
    // Inline evidence-preview thumbnail cap (issue #3642) -- deliberately smaller than
    // VisualBaselinesPanel's 220px side-by-side comparison, since this renders inline in a
    // message bubble alongside markdown text rather than in its own dedicated preview pane.
    private static final int EVIDENCE_PREVIEW_MAX_DIMENSION = 160;
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
    // Issue #3921: mirrors TRANSCRIPT_ROLE_PROPERTY, but carries the message's resolved
    // ShaftAssistantChatState kind (KIND_USER/KIND_ASSISTANT_TEXT/...) rather than its role, so
    // callers/tests can observe which MessageStyleRegistry entry a rendered row actually used.
    static final String TRANSCRIPT_KIND_PROPERTY = "shaft.transcript.kind";

    private final Project project;
    private final JPanel fallbackPanel;
    private final JBScrollPane fallbackScrollPane;
    private final List<ShaftAssistantChatState.Message> messages = new ArrayList<>();
    // Transient side-channel kept in lockstep with `messages` (same index <-> same message), never
    // persisted through ShaftAssistantChatState -- issue #3601 A5. An empty string means "no raw
    // evidence for this message". Populated only by the 3-arg append(role, message, rawEvidence);
    // every other messages-mutating path (clear/setMarkdown/setMessages/replaceLast) keeps this list
    // sized to match messages, backfilling "" since old evidence was never persisted to restore.
    private final List<String> messageRawEvidence = new ArrayList<>();
    // 1:1 with `messages` -- one rendered bubble handle per persisted message, kept in sync so
    // append()/replaceLast() can mutate the transcript incrementally (issue #3628) instead of
    // rebuilding every bubble on every streamed line. Reset (cleared and repopulated) by every full
    // renderFallbackTranscript() pass; never touched outside rendering code.
    private final List<RenderedBubble> renderedBubbles = new ArrayList<>();
    private int bubbleCreationCount;
    private String markdown = "";
    // issue #3751 part 2, HIGH finding 3: joinMessages() concatenates every persisted message, so
    // recomputing it eagerly on every streamed append()/replaceLast() call is O(total transcript size)
    // per line. Every hot-path mutator instead just sets this flag; markdown() computes lazily, once,
    // the next time anything actually reads it (persist, "Copy full transcript", etc.) -- which happens
    // far less often than once per streamed line.
    private boolean markdownDirty;
    // issue #3751 part 2, HIGH finding 1: ShaftAssistantPanel's Verbose-mode streaming path calls
    // replaceLast() with the ENTIRE accumulated buffer on every output line; re-converting Markdown and
    // re-parsing the whole HTML document (JEditorPane#setText) on every single call is O(current buffer
    // size) per call, O(N^2) over a run. updateLastBubbleIncrementally() throttles the expensive
    // convert+setText work to at most once per this interval -- the underlying message model is always
    // updated immediately regardless, so markdown()/persistence are never stale, only the visible bubble
    // can lag by up to this long mid-stream. Matches the ~100ms throttle LocalAgentOutputCoalescer's
    // production Timer already uses, so a run never renders faster than it can already be batched.
    private static final long STREAMED_RENDER_MIN_INTERVAL_NANOS = TimeUnit.MILLISECONDS.toNanos(100);
    // Seeded a full interval in the past (relative to a real System.nanoTime() reading taken right
    // here) rather than a sentinel like Long.MIN_VALUE: nanoTime()'s absolute value is arbitrary
    // per-JVM, so `now - Long.MIN_VALUE` silently overflows for any realistic `now`, wrapping to a
    // value that satisfies the "too soon" check and throttles away the very first streamed render.
    private long lastStreamedRenderNanos = System.nanoTime() - STREAMED_RENDER_MIN_INTERVAL_NANOS;
    // The closing fence line of a fenced code block: a newline followed by nothing but backticks
    // (three or more -- ShaftAssistantPanel#fencedCodeBlock grows the fence when the content itself
    // contains one).
    private static final Pattern CLOSING_FENCE_LINE = Pattern.compile("\n`{3,}");
    private final LafManagerListener lafListener;
    private Disposable lafConnectionDisposable;
    private int truncationBoundaryIndex = -1;
    private Runnable copyFullTranscriptAction = () -> { };
    private JPopupMenu lastMessageContextMenu;
    private JComponent pendingWidget;
    private String pendingWidgetRole = UNKNOWN_ROLE;

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
        if (markdownDirty) {
            markdown = joinMessages(messages);
            markdownDirty = false;
        }
        return markdown;
    }

    void clear() {
        markdown = "";
        markdownDirty = false;
        messages.clear();
        messageRawEvidence.clear();
        pendingWidget = null;
        refresh();
    }

    void append(String role, String message) {
        append(role, message, "");
    }

    /**
     * Same as {@link #append(String, String)}, plus a raw evidence string (for example a tool's raw
     * JSON output) shown behind a per-message "Show raw output" disclosure toggle -- issue #3601 A5.
     * The evidence is transient view state only: it is never added to {@link #markdown} and never
     * round-trips through {@link ShaftAssistantChatState}, matching that class's documented
     * "persisted without raw MCP payloads" contract.
     *
     * @param role message role
     * @param message rendered Markdown for the message
     * @param rawEvidence raw evidence to show behind the disclosure toggle, or blank/{@code null} for none
     */
    void append(String role, String message, String rawEvidence) {
        int sizeBeforeAdd = messages.size();
        addMessage(role, message, rawEvidence);
        if (messages.size() == sizeBeforeAdd) {
            // addMessage() silently dropped a blank message -- nothing changed, nothing to render.
            return;
        }
        boolean incremental = canAppendIncrementally();
        trimOldestIfOverCapacity(incremental);
        markdownDirty = true;
        if (incremental) {
            appendBubbleIncrementally(messages.size() - 1);
        } else {
            refresh();
        }
    }

    /**
     * Drops the oldest persisted message(s) once the transcript exceeds the same {@link
     * ShaftAssistantChatState#MAX_MESSAGES_PER_SESSION} cap {@code ShaftAssistantChatState} already
     * enforces on save (issue #3751 part 2, MEDIUM finding 4): non-verbose runs append one milestone
     * bubble per output line with no prior cap on {@link #messages}/{@link #messageRawEvidence}/{@link
     * #renderedBubbles}. Called right after a new message is added, before it is rendered, so the view
     * never retains more than the cap regardless of how many messages a run appends. {@code
     * bubbleCreationCount} is a cumulative construction counter and is deliberately never decremented
     * here -- trimming removes old bubbles, it does not "un-construct" them.
     *
     * @param incrementalBubbleTrackingInSync whether {@link #renderedBubbles} is 1:1 with {@link
     *     #messages} minus the just-added message (mirrors {@link #canAppendIncrementally()}, computed
     *     by the caller before this trim can shift indices) -- when {@code false} (a full {@link
     *     #refresh()} is about to run anyway), only the data model is trimmed and {@link
     *     #renderedBubbles} is left for {@link #renderFallbackTranscript()} to rebuild from scratch.
     */
    private void trimOldestIfOverCapacity(boolean incrementalBubbleTrackingInSync) {
        int overflow = messages.size() - ShaftAssistantChatState.MAX_MESSAGES_PER_SESSION;
        if (overflow <= 0) {
            return;
        }
        for (int trimmed = 0; trimmed < overflow; trimmed++) {
            messages.remove(0);
            if (!messageRawEvidence.isEmpty()) {
                messageRawEvidence.remove(0);
            }
            if (incrementalBubbleTrackingInSync && !renderedBubbles.isEmpty()) {
                RenderedBubble oldest = renderedBubbles.remove(0);
                fallbackPanel.remove(oldest.row);
            }
            if (truncationBoundaryIndex >= 0) {
                truncationBoundaryIndex--;
            }
        }
        if (incrementalBubbleTrackingInSync) {
            fallbackPanel.revalidate();
            fallbackPanel.repaint();
        }
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
        // The replaced content no longer corresponds to whatever evidence (if any) was captured for
        // the message being overwritten, and replaceLast() has no evidence of its own to carry over.
        setLastRawEvidence("");
        markdownDirty = true;
        if (updateLastBubbleIncrementally(last.role, message)) {
            scrollLatestIntoView();
        } else {
            refresh();
        }
    }

    /**
     * Forces the last streamed bubble to reflect its current model content immediately, bypassing the
     * {@link #STREAMED_RENDER_MIN_INTERVAL_NANOS} throttle (issue #3751 part 2, HIGH finding 1).
     * {@code ShaftAssistantPanel} must call this once a run's streamed output ends (completed,
     * cancelled, or failed) -- otherwise the visible bubble can remain stale from a throttled-away
     * render for up to that interval past the run's actual last {@link #replaceLast(String, String)}.
     * A no-op if there is no in-sync last bubble to flush (nothing streamed, or the last mutation fell
     * back to a full {@link #refresh()}, which already rendered current content).
     */
    void flushStreamedRender() {
        if (messages.isEmpty() || renderedBubbles.size() != messages.size()) {
            return;
        }
        ShaftAssistantChatState.Message last = messages.get(messages.size() - 1);
        RenderedBubble handle = renderedBubbles.get(renderedBubbles.size() - 1);
        if (!handle.role.equals(last.role)) {
            return;
        }
        renderBubbleContent(handle, last.markdown);
    }

    /**
     * Whether the just-appended last message can be added as a single new bubble instead of
     * rebuilding the whole transcript (issue #3628). Declines -- falling back to a full
     * {@link #refresh()} -- whenever a structural artifact would need to move as a result of this
     * append, rather than trying to reposition it incrementally:
     * <ul>
     *   <li>a transient {@link #pendingWidget} is showing (always trailing; a real persisted append
     *       from {@code ShaftAssistantPanel} always calls {@link #clearWidget()} first, so this is
     *       purely a defensive guard against a caller that doesn't);</li>
     *   <li>{@link #renderedBubbles} has drifted out of sync with {@link #messages} (should never
     *       happen; defensive guard);</li>
     *   <li>the truncation divider needs to render immediately before the new last message.</li>
     * </ul>
     */
    private boolean canAppendIncrementally() {
        return pendingWidget == null
                && renderedBubbles.size() == messages.size() - 1
                && truncationBoundaryIndex != messages.size() - 1;
    }

    private void appendBubbleIncrementally(int index) {
        ShaftAssistantChatState.Message message = messages.get(index);
        String rawEvidence = index < messageRawEvidence.size() ? messageRawEvidence.get(index) : "";
        RenderedBubble handle = fallbackMessage(message.role, message.kind, message.markdown, rawEvidence);
        fallbackPanel.add(handle.row);
        renderedBubbles.add(handle);
        fallbackPanel.revalidate();
        fallbackPanel.repaint();
        scrollLatestIntoView();
    }

    /**
     * Mutates the already-rendered last bubble's HTML content in place -- no new Swing components --
     * instead of rebuilding the whole transcript on every streamed line (issue #3628). Returns
     * {@code false} (leaving the caller to fall back to a full {@link #refresh()}) when the role
     * changed, since that also changes the bubble's side/colors/accessible names, which this
     * in-place path does not attempt to reproduce.
     */
    private boolean updateLastBubbleIncrementally(String role, String updatedMarkdown) {
        if (renderedBubbles.size() != messages.size()) {
            return false;
        }
        RenderedBubble handle = renderedBubbles.get(renderedBubbles.size() - 1);
        if (!role.equals(handle.role)) {
            return false;
        }
        // issue #3751 part 2, HIGH finding 1, fast path: the Verbose streaming shape only ever GROWS
        // inside its trailing fenced code block (constant header + whole accumulated buffer in one
        // fence -- see ShaftAssistantPanel#formatLocalAgentStreamingResponse), so append just the new
        // characters to the pane's existing document model instead of re-converting Markdown and
        // re-parsing the entire HTML document. O(delta) per streamed update instead of O(buffer).
        if (tryAppendStreamedFenceDelta(handle, updatedMarkdown)) {
            long now = System.nanoTime();
            if (now - lastStreamedRenderNanos >= STREAMED_RENDER_MIN_INTERVAL_NANOS) {
                lastStreamedRenderNanos = now;
                // Collapse re-measure forces a preferred-size pass -- throttle it; the delta text
                // itself is already visible regardless.
                handle.outputPanel.updateCollapseState();
            }
            return true;
        }
        // Slow path (content changed in a non-append way): skip the expensive Markdown->HTML convert
        // + full JEditorPane#setText re-parse when the last one ran too recently -- the model
        // (messages/markdown) is already fully up to date regardless via the caller, so this only
        // throttles the VISIBLE bubble's refresh rate to ~10Hz. flushStreamedRender() forces a final
        // render past this throttle once the caller's run actually finishes streaming.
        long now = System.nanoTime();
        if (now - lastStreamedRenderNanos < STREAMED_RENDER_MIN_INTERVAL_NANOS) {
            return true;
        }
        lastStreamedRenderNanos = now;
        renderBubbleContent(handle, updatedMarkdown);
        return true;
    }

    /**
     * Fast path for the Verbose streaming shape (issue #3751 part 2, HIGH finding 1): when {@code
     * next} is exactly the bubble's previously rendered Markdown with new text inserted immediately
     * before its trailing closing code fence, inserts just that delta into the rendered {@link
     * HTMLDocument} -- the fence's content renders as plain text inside {@code <pre><code>}, so a
     * model-level {@code insertString} (no HTML parsing at all) at the document's tail is equivalent
     * to the full re-render, minus the O(buffer) cost. Returns {@code false} for any other change
     * shape, leaving the caller to do a full (throttled) re-render. The rendered-HTML client property
     * intentionally goes stale during a delta streak; the terminal {@link #flushStreamedRender()} (or
     * any slow-path render) restores full consistency.
     */
    private boolean tryAppendStreamedFenceDelta(RenderedBubble handle, String next) {
        if (USER_ROLE.equals(handle.role)) {
            return false;
        }
        String delta = streamedFenceDelta(handle.streamedMarkdown, next);
        if (delta == null) {
            return false;
        }
        HTMLDocument document = (HTMLDocument) handle.htmlPane.getDocument();
        // The document's plain text ends with the code block's last line followed by the implied
        // trailing newline every Swing text document carries; inserting just before that newline,
        // with the code run's own character attributes, extends the <pre><code> content in place.
        int insertOffset = document.getLength() - 1;
        if (insertOffset < 1) {
            return false;
        }
        try {
            document.insertString(insertOffset, delta,
                    document.getCharacterElement(insertOffset - 1).getAttributes());
        } catch (BadLocationException e) {
            return false;
        }
        handle.streamedMarkdown = next;
        return true;
    }

    /**
     * Returns the text that was inserted immediately before {@code prev}'s trailing closing code
     * fence to produce {@code next}, or {@code null} when the change is not that exact append shape.
     */
    private static String streamedFenceDelta(String prev, String next) {
        if (prev == null || next.length() <= prev.length()) {
            return null;
        }
        int fenceStart = prev.lastIndexOf('\n');
        if (fenceStart < 0) {
            return null;
        }
        String closingFence = prev.substring(fenceStart);
        if (!CLOSING_FENCE_LINE.matcher(closingFence).matches() || !next.endsWith(closingFence)) {
            return null;
        }
        int prevCoreLength = prev.length() - closingFence.length();
        int nextCoreLength = next.length() - closingFence.length();
        if (nextCoreLength <= prevCoreLength || !next.regionMatches(0, prev, 0, prevCoreLength)) {
            return null;
        }
        return next.substring(prevCoreLength, nextCoreLength);
    }

    private void renderBubbleContent(RenderedBubble handle, String updatedMarkdown) {
        Color foreground = handle.htmlPane.getForeground();
        Color background = handle.bubble.getBackground();
        String bodyHtml = USER_ROLE.equals(handle.role)
                ? convertPlainUserText(updatedMarkdown)
                : convertMarkdown(updatedMarkdown);
        String rendered = toFallbackHtml(bodyHtml, foreground, background);
        handle.htmlPane.putClientProperty(TRANSCRIPT_RENDERED_HTML_PROPERTY, rendered);
        handle.htmlPane.setText(rendered);
        handle.htmlPane.setCaretPosition(0);
        handle.outputPanel.updateCollapseState();
        removeRawEvidenceDisclosure(handle.bubble);
        handle.bubble.revalidate();
        handle.bubble.repaint();
        handle.streamedMarkdown = updatedMarkdown;
    }

    private void removeRawEvidenceDisclosure(JPanel bubble) {
        Component south = ((BorderLayout) bubble.getLayout()).getLayoutComponent(BorderLayout.SOUTH);
        if (south != null) {
            bubble.remove(south);
        }
    }

    void setMarkdown(String value) {
        markdown = value == null ? "" : value;
        messages.clear();
        messageRawEvidence.clear();
        addMessage(UNKNOWN_ROLE, markdown, "");
        markdown = joinMessages(messages);
        refresh();
    }

    void setMessages(List<ShaftAssistantChatState.Message> restoredMessages) {
        messages.clear();
        messageRawEvidence.clear();
        if (restoredMessages != null) {
            for (ShaftAssistantChatState.Message message : restoredMessages) {
                if (message == null || message.markdown == null || message.markdown.isBlank()) {
                    continue;
                }
                // Restored messages never carry evidence -- it was never persisted in the first place.
                addMessage(message.role, message.markdown, "");
            }
        }
        markdown = joinMessages(messages);
        refresh();
    }

    void setTruncationBoundaryIndex(int index) {
        this.truncationBoundaryIndex = index;
    }

    /**
     * Injects the action driven by the "Copy full transcript" context menu item.
     * Callers (namely {@code ShaftAssistantPanel}) should reuse the exact same
     * export-and-copy path used by their own "Copy assistant transcript" action.
     *
     * @param action callback invoked when a message pane's "Copy full transcript" item is clicked
     */
    void setCopyFullTranscriptAction(Runnable action) {
        this.copyFullTranscriptAction = action == null ? () -> { } : action;
    }

    JPopupMenu lastMessageContextMenuForTest() {
        return lastMessageContextMenu;
    }

    /**
     * Renders {@code component} as an interactive bubble at the end of the transcript, outside the
     * persisted {@code messages}/markdown model. Used for ephemeral interactive UI, such as a
     * pending {@link ToolApprovalPromptPanel}, that should never be serialized into chat history.
     * Replaces any previously shown widget.
     *
     * @param role message role, used only to pick which side of the transcript the bubble renders on
     * @param component the live Swing component to embed
     */
    void showWidget(String role, JComponent component) {
        pendingWidgetRole = normalizedRole(role);
        pendingWidget = component;
        refresh();
    }

    /**
     * Removes the widget shown by {@link #showWidget(String, JComponent)}, if any.
     */
    void clearWidget() {
        pendingWidget = null;
        refresh();
    }

    JComponent pendingWidgetForTest() {
        return pendingWidget;
    }

    /**
     * Total bubble Swing components constructed since this view was created, across both full
     * rebuilds and incremental appends -- lets tests assert streamed {@link #replaceLast(String,
     * String)} calls mutate the existing last bubble instead of constructing a new one each time
     * (issue #3628).
     */
    int bubbleCreationCountForTest() {
        return bubbleCreationCount;
    }

    /**
     * Current number of persisted messages -- distinct from {@link #bubbleCreationCountForTest()}'s
     * cumulative construction count, this reflects the cap-and-trim policy (issue #3751 part 2,
     * MEDIUM finding 4): the view never retains more than {@link
     * ShaftAssistantChatState#MAX_MESSAGES_PER_SESSION} messages/bubbles at once, regardless of how
     * many were appended over a run.
     */
    int currentMessageCountForTest() {
        return messages.size();
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

    private void addMessage(String role, String value, String rawEvidence) {
        if (value == null || value.isBlank()) {
            return;
        }
        ShaftAssistantChatState.Message message = new ShaftAssistantChatState.Message();
        message.role = normalizedRole(role);
        message.kind = ShaftAssistantChatState.resolveKind(message.kind, message.role);
        message.markdown = value;
        messages.add(message);
        messageRawEvidence.add(rawEvidence == null ? "" : rawEvidence);
    }

    private void setLastRawEvidence(String rawEvidence) {
        if (messageRawEvidence.isEmpty()) {
            return;
        }
        messageRawEvidence.set(messageRawEvidence.size() - 1, rawEvidence == null ? "" : rawEvidence);
    }

    private String convertMarkdown(String value) {
        String safeValue = value == null ? "" : value;
        return addCodeCopyButtons(renderMarkdownBlocks(safeValue));
    }

    /**
     * Renders {@code value} as a single HTML paragraph with no Markdown interpretation at all:
     * every character the user typed -- a stray backtick, {@code **}, a leading
     * {@code #} -- stays exactly as typed instead of becoming {@code <code>}/{@code <strong>}/a
     * heading. Only HTML's own {@code &}/{@code <}/{@code >} are escaped (via the same {@link
     * #escapeInlineHtml} helper the Markdown path already uses for plain-text runs), and {@code '\n'}
     * becomes {@code <br>} exactly like {@link #appendParagraph}'s Markdown path, so user bubbles wrap
     * multi-line messages the same visible way assistant bubbles do. Used only for the {@code user}
     * role; assistant/agent bubbles keep the full {@link #convertMarkdown} pipeline.
     *
     * @param value raw user text, or {@code null}
     * @return a single {@code <p>...</p>} HTML body
     */
    private static String convertPlainUserText(String value) {
        String safeValue = value == null ? "" : value;
        return "<p>" + escapeInlineHtml(safeValue).replace("\n", "<br>") + "</p>";
    }

    private void refresh() {
        renderFallbackTranscript();
        scrollLatestIntoView();
    }

    private void renderFallbackTranscript() {
        Color transcriptBackground = resolvedColor("TextArea.background", Color.WHITE);
        fallbackPanel.removeAll();
        fallbackPanel.setBackground(transcriptBackground);
        renderedBubbles.clear();
        for (int i = 0; i < messages.size(); i++) {
            if (i == truncationBoundaryIndex) {
                fallbackPanel.add(createTruncationDivider());
            }
            ShaftAssistantChatState.Message message = messages.get(i);
            String rawEvidence = i < messageRawEvidence.size() ? messageRawEvidence.get(i) : "";
            RenderedBubble handle = fallbackMessage(message.role, message.kind, message.markdown, rawEvidence);
            fallbackPanel.add(handle.row);
            renderedBubbles.add(handle);
        }
        if (pendingWidget != null) {
            fallbackPanel.add(widgetRow(pendingWidgetRole, pendingWidget));
        }
        fallbackPanel.revalidate();
        fallbackPanel.repaint();
    }

    /**
     * Handle to one persisted message's rendered bubble, tracked so {@link #append(String, String,
     * String)} and {@link #replaceLast(String, String)} can mutate the transcript incrementally
     * (issue #3628) instead of rebuilding every bubble on every streamed line.
     */
    private static final class RenderedBubble {
        private final JComponent row;
        private final JPanel bubble;
        private final JEditorPane htmlPane;
        private final String role;
        private final CollapsibleOutputPanel outputPanel;
        // The exact Markdown this bubble last rendered, kept so streamed replaceLast() calls can
        // detect "same content, grown only inside the trailing fenced code block" and append just the
        // delta to the pane's document instead of re-parsing everything (issue #3751 part 2, HIGH
        // finding 1). Updated by every full render and by each successful delta append.
        private String streamedMarkdown;

        private RenderedBubble(JComponent row, JPanel bubble, JEditorPane htmlPane, String role,
                CollapsibleOutputPanel outputPanel) {
            this.row = row;
            this.bubble = bubble;
            this.htmlPane = htmlPane;
            this.role = role;
            this.outputPanel = outputPanel;
        }
    }

    /**
     * Wraps a message's {@link JEditorPane} so it renders collapsed (clipped to {@link
     * #COLLAPSE_LINE_THRESHOLD} rendered lines, tall content clipped rather than scrolled) with a
     * keyboard-focusable "Show full output" toggle once its rendered height exceeds the threshold --
     * issue #3629. Content under the threshold never shows a toggle. {@link #updateCollapseState()}
     * re-measures after every text mutation (streamed {@code replaceLast} calls) without disturbing
     * an already-expanded bubble the user explicitly opened.
     */
    private static final class CollapsibleOutputPanel extends JPanel {
        private final JEditorPane htmlPane;
        private final Supplier<Integer> lineHeightSupplier;
        private final JPanel clipPanel;
        private final JButton toggle;
        private boolean collapsed = true;
        private boolean userExpanded;
        private int collapsedHeightPx;

        private CollapsibleOutputPanel(JEditorPane htmlPane, Supplier<Integer> lineHeightSupplier) {
            super();
            this.htmlPane = htmlPane;
            this.lineHeightSupplier = lineHeightSupplier;
            setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
            setOpaque(false);

            clipPanel = new JPanel(new BorderLayout()) {
                @Override
                public Dimension getPreferredSize() {
                    Dimension preferred = super.getPreferredSize();
                    if (collapsed) {
                        return new Dimension(preferred.width, Math.min(preferred.height, collapsedHeightPx));
                    }
                    return preferred;
                }

                @Override
                public Dimension getMaximumSize() {
                    return getPreferredSize();
                }
            };
            clipPanel.setOpaque(false);
            clipPanel.add(htmlPane, BorderLayout.CENTER);

            toggle = new JButton();
            toggle.setVisible(false);
            toggle.addActionListener(event -> {
                collapsed = !collapsed;
                userExpanded = !collapsed;
                updateToggleLabel();
                revalidate();
                repaint();
            });
            JPanel toggleRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
            toggleRow.setOpaque(false);
            toggleRow.setBorder(JBUI.Borders.emptyTop(4));
            toggleRow.add(toggle);

            add(clipPanel);
            add(toggleRow);
        }

        private void updateCollapseState() {
            int lineHeight = Math.max(1, lineHeightSupplier.get());
            int renderedLineCount = htmlPane.getPreferredSize().height / lineHeight;
            boolean overThreshold = renderedLineCount > COLLAPSE_LINE_THRESHOLD;
            collapsedHeightPx = lineHeight * COLLAPSE_LINE_THRESHOLD;
            // Independent of overThreshold: content under the threshold never clips (the cap exceeds
            // its natural height) and its hidden toggle keeps a correct label if it later grows past
            // the threshold via a streamed replaceLast().
            collapsed = !userExpanded;
            toggle.setVisible(overThreshold);
            updateToggleLabel();
        }

        private void updateToggleLabel() {
            String label = collapsed ? "Show full output" : "Hide full output";
            toggle.setText(label);
            toggle.getAccessibleContext().setAccessibleName(label);
            toggle.getAccessibleContext().setAccessibleDescription(collapsed
                    ? "Message content is collapsed; activate to show the full output"
                    : "Message content is fully shown; activate to collapse it again");
        }
    }

    private JComponent widgetRow(String role, JComponent component) {
        boolean user = USER_ROLE.equals(role);
        JPanel row = new PreferredHeightRow(new BorderLayout());
        row.setOpaque(false);
        row.setBorder(JBUI.Borders.emptyBottom(10));
        row.getAccessibleContext().setAccessibleName("Assistant interactive message row");
        row.add(widthCappedWidget(component), user ? BorderLayout.EAST : BorderLayout.WEST);
        return row;
    }

    /**
     * Wraps an arbitrary widget (for example {@link ToolApprovalPromptPanel}) so it never reports a
     * preferred width past {@link #fallbackBubbleContentWidth()} — the same budget message bubbles
     * use — regardless of how wide its own content wants to be. Without this, a {@code BorderLayout}
     * row sizes itself to the widget's unconstrained preferred width, letting it extend past the
     * transcript's right edge instead of wrapping or shrinking with the viewport.
     */
    private JComponent widthCappedWidget(JComponent component) {
        JPanel wrapper = new JPanel(new BorderLayout()) {
            @Override
            public Dimension getPreferredSize() {
                int cappedWidth = fallbackBubbleContentWidth();
                Dimension preferred = super.getPreferredSize();
                if (cappedWidth > 0 && preferred.width > cappedWidth) {
                    setSize(new Dimension(cappedWidth, Short.MAX_VALUE));
                    preferred = super.getPreferredSize();
                    preferred.width = cappedWidth;
                }
                return preferred;
            }

            @Override
            public Dimension getMaximumSize() {
                return getPreferredSize();
            }
        };
        wrapper.setOpaque(false);
        wrapper.add(component, BorderLayout.CENTER);
        return wrapper;
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

    private RenderedBubble fallbackMessage(String role, String kind, String markdown, String rawEvidence) {
        bubbleCreationCount++;
        String normalizedRole = normalizedRole(role);
        boolean user = USER_ROLE.equals(normalizedRole);
        String normalizedKind = ShaftAssistantChatState.resolveKind(kind, normalizedRole);
        MessageStyleRegistry.MessageStyle style = MessageStyleRegistry.styleFor(normalizedKind);
        Color background = style.background();
        Color foreground = style.foreground();
        JPanel row = new PreferredHeightRow(new BorderLayout());
        row.setOpaque(false);
        row.setBorder(JBUI.Borders.emptyBottom(10));
        row.putClientProperty(TRANSCRIPT_ROLE_PROPERTY, normalizedRole);
        row.putClientProperty(TRANSCRIPT_KIND_PROPERTY, normalizedKind);
        row.getAccessibleContext().setAccessibleName((user ? "User" : "Assistant") + " assistant message row");
        Color stroke = style.stroke();
        RoundedBubblePanel bubble = new RoundedBubblePanel(background, stroke, 18);
        bubble.setLayout(new BorderLayout());
        bubble.setBorder(JBUI.Borders.empty(9, 11));
        bubble.setBackground(background);
        bubble.setForeground(foreground);
        bubble.putClientProperty(TRANSCRIPT_BUBBLE_PROPERTY, normalizedRole);
        bubble.getAccessibleContext().setAccessibleName((user ? "User" : "Assistant") + " assistant message bubble");
        String bodyHtml = user ? convertPlainUserText(markdown) : convertMarkdown(markdown);
        JEditorPane htmlPane = fallbackHtmlPane(bodyHtml, foreground, background);
        htmlPane.putClientProperty(TRANSCRIPT_ROLE_PROPERTY, normalizedRole);
        CollapsibleOutputPanel outputPanel = new CollapsibleOutputPanel(htmlPane, this::bodyLineHeight);
        outputPanel.updateCollapseState();
        bubble.add(outputPanel, BorderLayout.CENTER);
        if (rawEvidence != null && !rawEvidence.isBlank()) {
            bubble.add(evidenceFooter(rawEvidence), BorderLayout.SOUTH);
        }
        row.add(bubble, user ? BorderLayout.EAST : BorderLayout.WEST);
        RenderedBubble handle = new RenderedBubble(row, bubble, htmlPane, normalizedRole, outputPanel);
        handle.streamedMarkdown = markdown;
        return handle;
    }

    /**
     * Builds the raw-evidence footer for a message bubble: an initially empty slot for
     * Doctor/Healer screenshot/page-snapshot previews (issue #3642), populated asynchronously by
     * {@link #schedulePreviewLookup}, stacked above the existing {@link
     * #rawEvidenceDisclosure(String)} "Show raw output" toggle. A message whose evidence resolves
     * to no readable image leaves the preview slot empty forever -- the disclosure below is
     * exactly today's unchanged text-only display, with no error UI and no exception ever
     * reaching the user.
     *
     * @param rawEvidence non-blank raw evidence text
     * @return footer component to add at {@link BorderLayout#SOUTH}
     */
    private JComponent evidenceFooter(String rawEvidence) {
        JPanel footer = new JPanel();
        footer.setLayout(new BoxLayout(footer, BoxLayout.Y_AXIS));
        footer.setOpaque(false);
        JPanel previewSlot = new JPanel();
        previewSlot.setLayout(new BoxLayout(previewSlot, BoxLayout.Y_AXIS));
        previewSlot.setOpaque(false);
        footer.add(previewSlot);
        footer.add(rawEvidenceDisclosure(rawEvidence));
        schedulePreviewLookup(rawEvidence, previewSlot);
        return footer;
    }

    /**
     * Resolves {@code rawEvidence}'s Doctor/Healer screenshot/page-snapshot evidence (via {@link
     * DoctorEvidenceImageLocator}) and decodes each candidate image off the EDT, on the bounded
     * {@link ShaftPluginExecutor} pool (issue #3622) -- never blocking the EDT on file IO -- then
     * adds one click-to-open preview row per readable image to {@code previewSlot} back on the
     * EDT via {@link ApplicationManager#invokeLater}. Fire-and-forget: no resolvable evidence, an
     * unreachable evidence file, or a decode failure all just mean no rows are ever added.
     *
     * @param rawEvidence non-blank raw evidence text to resolve image paths from
     * @param previewSlot empty container, already showing in the tree, to populate on the EDT
     */
    private static void schedulePreviewLookup(String rawEvidence, JPanel previewSlot) {
        CompletableFuture.supplyAsync(() -> loadEvidencePreviews(rawEvidence),
                        ShaftPluginExecutor.getInstance().executor())
                .whenComplete((previews, error) -> {
                    if (error != null || previews == null || previews.isEmpty()) {
                        return;
                    }
                    runOnEdt(() -> {
                        previews.forEach(preview -> previewSlot.add(previewRow(preview)));
                        previewSlot.revalidate();
                        previewSlot.repaint();
                    });
                });
    }

    /**
     * Runs {@code action} on the EDT, matching {@code ShaftAssistantPanel}'s established fallback
     * for headless test JVMs where no live IntelliJ {@code Application} exists (confirmed by
     * {@code ShaftPluginExecutorTest}'s own doc comment): prefer {@link
     * ApplicationManager#invokeLater}, but degrade to direct execution when already on the EDT, or
     * {@link SwingUtilities#invokeLater} otherwise, rather than risk a {@code
     * NullPointerException} from an unconditional {@code ApplicationManager.getApplication()} call.
     *
     * @param action action to run on the EDT
     */
    private static void runOnEdt(Runnable action) {
        if (ApplicationManager.getApplication() != null) {
            ApplicationManager.getApplication().invokeLater(action);
        } else if (SwingUtilities.isEventDispatchThread()) {
            action.run();
        } else {
            SwingUtilities.invokeLater(action);
        }
    }

    private static List<EvidencePreview> loadEvidencePreviews(String rawEvidence) {
        List<EvidencePreview> previews = new ArrayList<>();
        for (Path imagePath : DoctorEvidenceImageLocator.resolveImagePaths(rawEvidence)) {
            Icon icon = ImagePreviewSupport.readScaledIcon(imagePath, EVIDENCE_PREVIEW_MAX_DIMENSION);
            if (icon != null) {
                previews.add(new EvidencePreview(imagePath, icon));
            }
        }
        return previews;
    }

    /**
     * Builds one clickable preview row. Matches {@link
     * com.shaft.intellij.actions.ShowTraceViewerAction}'s established convention for opening a
     * local file in the platform's own viewer ({@link BrowserUtil#browse}) rather than introducing
     * a new {@code OpenFileDescriptor}/{@code Desktop.getDesktop()} path -- neither of which this
     * plugin uses anywhere else.
     *
     * @param preview resolved, already-decoded image preview
     * @return preview row component
     */
    private static JComponent previewRow(EvidencePreview preview) {
        JLabel imageLabel = new JLabel(preview.icon());
        imageLabel.setToolTipText(preview.path().toString());
        imageLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        imageLabel.getAccessibleContext().setAccessibleName("Evidence screenshot preview");
        imageLabel.getAccessibleContext().setAccessibleDescription(
                "Opens " + preview.path() + " in the platform viewer");
        imageLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent event) {
                BrowserUtil.browse(preview.path().toUri());
            }
        });
        JPanel row = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        row.setOpaque(false);
        row.setBorder(JBUI.Borders.emptyTop(6));
        row.add(imageLabel);
        return row;
    }

    /**
     * One resolved, already-decoded evidence image ready to render as a preview row.
     *
     * @param path resolved on-disk image path, shown as the row's tooltip and opened on click
     * @param icon pre-scaled icon, decoded off the EDT by {@link #loadEvidencePreviews}
     */
    private record EvidencePreview(Path path, Icon icon) {
    }

    private int bodyLineHeight() {
        return getFontMetrics(new Font(fontFamily(), Font.PLAIN, baseFontSize())).getHeight();
    }

    /**
     * Builds the collapsed-by-default "Show raw output" disclosure for a message's raw evidence
     * (issue #3601 A5) -- for example a tool's raw JSON result, available here because {@link
     * ShaftAssistantPanel#runToolAndRenderCard} passes it straight through {@link #append(String,
     * String, String)}. Kept as visible button text rather than an icon: #3601/B1.1 already fixed
     * the opposite mistake elsewhere in this plugin (an icon-only raw-output toggle in {@code
     * ShaftFeaturePanel} that only flips state in its tooltip), and the raw-data escape hatch is
     * exactly the control a user most needs to be able to find by reading, not guessing.
     *
     * @param rawEvidence non-blank raw evidence text to show, verbatim, once expanded
     */
    private JComponent rawEvidenceDisclosure(String rawEvidence) {
        JPanel container = new JPanel();
        container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));
        container.setOpaque(false);
        container.setBorder(JBUI.Borders.emptyTop(6));
        container.getAccessibleContext().setAccessibleName("Raw tool output disclosure");

        JBTextArea rawArea = new JBTextArea(rawEvidence);
        rawArea.setEditable(false);
        rawArea.setLineWrap(true);
        rawArea.setWrapStyleWord(true);
        Font monospaced = rawArea.getFont();
        rawArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, monospaced == null ? 12 : monospaced.getSize()));
        rawArea.setCaretPosition(0);
        rawArea.getAccessibleContext().setAccessibleName("Raw tool output");

        JBScrollPane rawScrollPane = new JBScrollPane(rawArea);
        rawScrollPane.setBorder(BorderFactory.createLineBorder(
                resolvedColor("Component.borderColor", new Color(0xD0D7DE))));
        rawScrollPane.setPreferredSize(new Dimension(fallbackBubbleContentWidth(), JBUI.scale(160)));
        rawScrollPane.setVisible(false);
        rawScrollPane.getAccessibleContext().setAccessibleName("Raw tool output");

        JButton toggle = new JButton("Show raw output");
        toggle.getAccessibleContext().setAccessibleName("Show raw output");
        toggle.addActionListener(event -> {
            boolean expanding = !rawScrollPane.isVisible();
            rawScrollPane.setVisible(expanding);
            String label = expanding ? "Hide raw output" : "Show raw output";
            toggle.setText(label);
            toggle.getAccessibleContext().setAccessibleName(label);
            container.revalidate();
            container.repaint();
        });

        JPanel toggleRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        toggleRow.setOpaque(false);
        toggleRow.add(toggle);

        container.add(toggleRow);
        container.add(rawScrollPane);
        return container;
    }

    /**
     * Renders an assistant-styled bubble containing {@code markdown} plus a trailing
     * {@code actions} row (for example a "Got it" dismiss button, or a gate's one-click fix
     * button), sharing the exact bubble chrome and word-wrap-safe {@link WidthAwareHtmlPane}
     * rendering that persisted transcript messages use via {@link #fallbackMessage}. Intended for
     * {@link #showWidget(String, JComponent)} callers (the first-run welcome, and SHAFT's own
     * deterministic pre-flight gates -- issue #3681) that need an assistant bubble look without
     * adding anything to the persisted {@link #messages}/{@link #markdown} model. The returned
     * component is plain content, not a pre-aligned row: {@link #showWidget(String, JComponent)}
     * applies the same West/East alignment {@link #fallbackMessage} applies directly.
     *
     * @param accessibleName distinguishes each caller's bubble for accessibility tools and tests
     *                       (e.g. "Assistant welcome message bubble" vs. a gate-specific name)
     */
    JComponent assistantBubbleWithActions(String markdown, JComponent actions, String accessibleName) {
        Color background = resolvedColor("Panel.background", new Color(0xF6F8FA));
        Color foreground = resolvedColor("TextArea.foreground", new Color(0x202020));
        Color stroke = resolvedColor("Component.borderColor", new Color(0xD0D7DE));
        RoundedBubblePanel bubble = new RoundedBubblePanel(background, stroke, 18);
        bubble.setLayout(new BorderLayout(0, 8));
        bubble.setBorder(JBUI.Borders.empty(9, 11));
        bubble.setBackground(background);
        bubble.setForeground(foreground);
        bubble.putClientProperty(TRANSCRIPT_BUBBLE_PROPERTY, UNKNOWN_ROLE);
        bubble.getAccessibleContext().setAccessibleName(accessibleName);
        JEditorPane htmlPane = fallbackHtmlPane(convertMarkdown(markdown), foreground, background);
        // JEditorPane under-reports the preferred height of a trailing HTML list by roughly its last
        // item's bottom margin, which cropped the final "Review code into a test" step against the
        // actions row below. Reserve extra bottom room so BorderLayout gives the pane enough height.
        htmlPane.setBorder(JBUI.Borders.emptyBottom(JBUI.scale(10)));
        htmlPane.putClientProperty(TRANSCRIPT_ROLE_PROPERTY, UNKNOWN_ROLE);
        // Unlike a persisted fallbackMessage() pane, this one can exist in the tree of a freshly
        // constructed, otherwise message-less panel (the welcome shows before any real message
        // exists), so it needs its own accessible name rather than relying on siblings for context.
        htmlPane.getAccessibleContext().setAccessibleName("Assistant welcome message content");
        bubble.add(htmlPane, BorderLayout.CENTER);
        if (actions != null) {
            JPanel actionsRow = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
            actionsRow.setOpaque(false);
            actionsRow.add(actions);
            bubble.add(actionsRow, BorderLayout.SOUTH);
        }
        return bubble;
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
        htmlPane.setComponentPopupMenu(buildMessageContextMenu(htmlPane));
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

    /**
     * Built once per pane and wired via {@link JComponent#setComponentPopupMenu} so the platform's
     * native popup triggers -- right-click AND the keyboard context-menu key / Shift+F10 on a
     * focused pane -- open it for free, with identical items either way (issue #3630). Selection
     * can change between build time and show time, so {@code Copy}'s enabled state is refreshed on
     * every {@code popupMenuWillBecomeVisible} rather than fixed once at construction.
     */
    private JPopupMenu buildMessageContextMenu(JEditorPane pane) {
        JPopupMenu menu = new JPopupMenu("Assistant transcript message actions");

        JMenuItem copyItem = new JMenuItem("Copy");
        copyItem.getAccessibleContext().setAccessibleName("Copy");
        copyItem.addActionListener(event -> copySelection(pane));

        JMenuItem selectAllItem = new JMenuItem("Select All");
        selectAllItem.getAccessibleContext().setAccessibleName("Select All");
        selectAllItem.addActionListener(event -> pane.selectAll());

        JMenuItem copyFullTranscriptItem = new JMenuItem("Copy full transcript");
        copyFullTranscriptItem.getAccessibleContext().setAccessibleName("Copy full transcript");
        copyFullTranscriptItem.addActionListener(event -> copyFullTranscriptAction.run());

        menu.add(copyItem);
        menu.add(selectAllItem);
        menu.add(copyFullTranscriptItem);
        menu.addPopupMenuListener(new PopupMenuListener() {
            @Override
            public void popupMenuWillBecomeVisible(PopupMenuEvent event) {
                String selectedText = pane.getSelectedText();
                copyItem.setEnabled(selectedText != null && !selectedText.isEmpty());
                lastMessageContextMenu = menu;
            }

            @Override
            public void popupMenuWillBecomeInvisible(PopupMenuEvent event) {
            }

            @Override
            public void popupMenuCanceled(PopupMenuEvent event) {
            }
        });
        return menu;
    }

    private static void copySelection(JEditorPane pane) {
        Action copyAction = pane.getActionMap().get(DefaultEditorKit.copyAction);
        if (copyAction != null) {
            copyAction.actionPerformed(new ActionEvent(pane, ActionEvent.ACTION_PERFORMED, DefaultEditorKit.copyAction));
        }
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
                // A single newline the user/agent actually typed is a deliberate line break, not
                // insignificant Markdown whitespace -- joining with '\n' (turned into <br> below in
                // appendParagraph) preserves it instead of collapsing consecutive lines into one
                // run-on sentence.
                paragraph.append('\n');
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
        // renderInline() (via escapeInlineHtml()) never touches '\n', so the joiner characters
        // appended above survive inline rendering untouched and are safe to turn into <br> here.
        html.append("<p>").append(renderInline(paragraph.toString()).replace("\n", "<br>")).append("</p>");
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
                // Swing's int-truncated HTML measurement leaves zero right-edge slack, so a bubble
                // laid out at exactly this width can clip the last letter or two. A few px of
                // headroom -- still capped by preferredWidth -- fixes the crop without loosening the
                // wrap budget fallbackBubbleContentWidth() computes.
                preferred.width = Math.min(preferred.width + JBUI.scale(4), preferredWidth);
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
