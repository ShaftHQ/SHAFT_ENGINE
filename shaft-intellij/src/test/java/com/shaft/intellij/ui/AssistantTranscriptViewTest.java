package com.shaft.intellij.ui;

import com.intellij.ui.components.JBTextArea;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.imageio.ImageIO;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import java.awt.Component;
import java.awt.Container;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Covers the per-message "Show raw output" disclosure added for issue #3601 A5: narrative-first
 * tool results with raw JSON behind a real Swing disclosure widget. The raw evidence passed to the
 * 3-arg {@link AssistantTranscriptView#append(String, String, String)} is transient view state --
 * it is never added to {@link AssistantTranscriptView#markdown()} and never round-trips through
 * {@link ShaftAssistantChatState}, which is documented as "persisted without raw MCP payloads".
 */
class AssistantTranscriptViewTest {
    @Test
    void onlyMessagesWithNonBlankRawEvidenceRenderTheDisclosureToggle() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", "Tool ran successfully.", "{\"raw\":true}");
        view.append("assistant", "Plain response with no evidence at all.");
        view.append("assistant", "Response with explicitly blank evidence.", "");

        List<JButton> toggles = findButtonsByText(view, "Show raw output");

        assertEquals(1, toggles.size(),
                "Only the message appended with non-blank raw evidence should render a disclosure toggle");
    }

    @Test
    void clickingTheToggleRevealsExactRawEvidenceAndClickingAgainHidesIt() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        String evidence = "{\n  \"tool\": \"doctor_analyze_failed_allure\",\n  \"status\": \"DETERMINISTIC\"\n}";
        view.append("assistant", "Ran the analysis.", evidence);

        JButton toggle = findButtonByText(view, "Show raw output");
        JBTextArea rawArea = findByType(view, JBTextArea.class);
        assertNotNull(toggle, "Expected a Show raw output toggle for a message with evidence");
        assertNotNull(rawArea, "Expected a raw output text area for a message with evidence");
        assertAll(
                () -> assertEquals(evidence, rawArea.getText(), "Raw text area must hold the evidence verbatim"),
                () -> assertFalse(rawArea.isEditable(), "Raw output must be read-only"),
                () -> assertFalse(isVisibleInHierarchy(rawArea), "Raw output must be collapsed by default"));

        toggle.doClick();
        assertAll(
                () -> assertEquals("Hide raw output", toggle.getText()),
                () -> assertEquals("Hide raw output", toggle.getAccessibleContext().getAccessibleName()),
                () -> assertTrue(isVisibleInHierarchy(rawArea), "Raw output must show after clicking the toggle"));

        toggle.doClick();
        assertAll(
                () -> assertEquals("Show raw output", toggle.getText()),
                () -> assertEquals("Show raw output", toggle.getAccessibleContext().getAccessibleName()),
                () -> assertFalse(isVisibleInHierarchy(rawArea), "Raw output must hide again after a second click"));
    }

    @Test
    void rawEvidenceNeverLeaksIntoTranscriptMarkdown() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        String secretMarker = "zzz-raw-evidence-should-not-leak-zzz";
        view.append("assistant", "Tool finished.", "{\"marker\":\"" + secretMarker + "\"}");

        assertFalse(view.markdown().contains(secretMarker),
                "markdown() backs Copy full transcript and must never include raw evidence");
    }

    @Test
    void rawEvidenceNeverRoundTripsThroughPersistedChatState() {
        ShaftAssistantChatState state = new ShaftAssistantChatState();
        String secretMarker = "zzz-raw-evidence-should-not-persist-zzz";
        state.append("assistant", "Tool finished successfully.", "{\"marker\":\"" + secretMarker + "\"}");

        List<ShaftAssistantChatState.Message> active = state.activeMessages();
        assertFalse(active.isEmpty());
        for (ShaftAssistantChatState.Message message : active) {
            assertFalse(message.markdown.contains(secretMarker),
                    "ShaftAssistantChatState.Message has no raw-evidence slot by design; the raw payload "
                            + "must never leak into its persisted markdown field");
        }

        // Confirm the same holds for the actual PersistentStateComponent round trip.
        ShaftAssistantChatState.StateData stateData = state.getState();
        for (ShaftAssistantChatState.Session session : stateData.sessions) {
            for (ShaftAssistantChatState.Message message : session.messages) {
                assertFalse(message.markdown.contains(secretMarker));
            }
        }
    }

    @Test
    void twoArgAppendNeverRendersADisclosureToggle() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", "A response appended without any raw evidence argument.");

        assertNull(findButtonByText(view, "Show raw output"),
                "The plain 2-arg append() must never render a raw-output toggle");
    }

    /**
     * Issue #3628: streamed local-agent output calls {@code replaceLast} once per line against an
     * already-appended placeholder bubble. Before this ticket every call -- append or replaceLast --
     * rebuilt the entire transcript from scratch, which is O(n) per streamed line. Asserts the actual
     * component-construction cost instead of wall-clock time: each append constructs exactly one new
     * bubble, and replaceLast constructs zero.
     */
    @Test
    void appendConstructsOneBubblePerMessageAndReplaceLastConstructsNone() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        for (int i = 0; i < 200; i++) {
            view.append("assistant", "message " + i);
        }
        assertEquals(200, view.bubbleCreationCountForTest(),
                "200 sequential appends must each construct exactly one bubble, never rebuild the whole transcript");

        for (int i = 0; i < 500; i++) {
            view.replaceLast("assistant", "streamed update " + i);
        }
        assertEquals(200, view.bubbleCreationCountForTest(),
                "replaceLast must mutate the existing last bubble in place and construct zero new bubbles");
    }

    @Test
    void replaceLastUpdatesTheExistingBubblesRenderedHtmlInPlace() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", "first draft");
        view.replaceLast("assistant", "final answer");

        JEditorPane pane = findByType(view, JEditorPane.class);
        assertNotNull(pane);
        assertAll(
                () -> assertTrue(pane.getText().contains("final answer"),
                        "The existing bubble's HTML must be updated to the replaced content"),
                () -> assertFalse(pane.getText().contains("first draft"),
                        "The stale content must not remain after replaceLast"));
    }

    @Test
    void replaceLastRemovesAPreviouslyShownRawEvidenceDisclosureFromTheMutatedBubble() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", "Tool ran.", "{\"raw\":true}");
        assertNotNull(findButtonByText(view, "Show raw output"),
                "Sanity check: the initial append with evidence must render the disclosure toggle");

        view.replaceLast("assistant", "Tool finished.");

        assertNull(findButtonByText(view, "Show raw output"),
                "replaceLast() always clears raw evidence -- any previously shown disclosure toggle "
                        + "must be removed from the in-place-mutated bubble, not just orphaned behind stale text");
    }

    @Test
    void appendFallsBackToAFullRebuildWhenTheNewMessageLandsExactlyAtTheTruncationBoundary() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", "first");
        view.setTruncationBoundaryIndex(1);
        view.append("assistant", "second");

        assertNotNull(findByAccessibleName(view, "Context truncation indicator"),
                "The truncation divider must still render when a new message lands exactly at the "
                        + "boundary index, even though append() otherwise skips a full rebuild");
    }

    /**
     * Issue #3629: a Doctor/Healer report or large JSON tool result can push the whole conversation
     * off-screen with no way to fold it back. Long messages must render collapsed by default with a
     * keyboard-focusable toggle; short messages must never show one.
     */
    @Test
    void longMessagesRenderCollapsedWithAShowFullOutputToggle() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", manyLineParagraphs(100));

        assertNotNull(findVisibleButtonByText(view, "Show full output"),
                "A message whose rendered height exceeds the collapse threshold must show a toggle");
    }

    @Test
    void shortMessagesNeverRenderACollapseToggle() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", "A short reply that clearly stays under the collapse threshold.");

        assertAll(
                () -> assertNull(findVisibleButtonByText(view, "Show full output"),
                        "A short message must never render a collapse toggle"),
                () -> assertNull(findVisibleButtonByText(view, "Hide full output"),
                        "A short message must never render a collapse toggle"));
    }

    @Test
    void clickingTheCollapseToggleExpandsTheBubbleAndClickingAgainCollapsesIt() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", manyLineParagraphs(100));

        JButton toggle = findVisibleButtonByText(view, "Show full output");
        assertNotNull(toggle);

        toggle.doClick();
        assertAll(
                () -> assertEquals("Hide full output", toggle.getText()),
                () -> assertEquals("Hide full output", toggle.getAccessibleContext().getAccessibleName()));

        toggle.doClick();
        assertAll(
                () -> assertEquals("Show full output", toggle.getText()),
                () -> assertEquals("Show full output", toggle.getAccessibleContext().getAccessibleName()));
    }

    @Test
    void aCollapsedMessageStillExposesItsFullTextSoCopyIsUnaffected() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", manyLineParagraphs(100));

        JEditorPane pane = findByType(view, JEditorPane.class);
        assertNotNull(pane);
        assertTrue(pane.getText().contains("Line 99"),
                "Collapsing only clips the visible height -- the underlying document text (what "
                        + "Select All / Copy operate on) must still contain the full content");
    }

    @Test
    void replaceLastReevaluatesTheCollapseStateAsStreamedContentGrowsPastTheThreshold() {
        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", "short start");
        assertNull(findVisibleButtonByText(view, "Show full output"),
                "Sanity check: the initial short placeholder must not show a collapse toggle");

        view.replaceLast("assistant", manyLineParagraphs(100));

        assertNotNull(findVisibleButtonByText(view, "Show full output"),
                "A streamed message that grows past the collapse threshold must gain a toggle, "
                        + "proving replaceLast() re-measures collapse state on the mutated bubble");
    }

    /**
     * Issue #3642: a Doctor/Healer tool result whose {@code bundlePath}/{@code jsonReportPath}
     * pointer cannot be read (missing file here) must degrade silently to exactly today's
     * text-only "Show raw output" disclosure -- no error UI, no exception, and no preview row
     * ever appears. Nothing here races: an unresolvable pointer never schedules a UI update in
     * the first place, so the absence is immediately stable, not just momentarily true.
     */
    @Test
    void unresolvableEvidencePathsDegradeSilentlyToTheExistingTextOnlyDisclosure(@TempDir Path directory) {
        String rawEvidence = "{\"schemaVersion\": \"1.0\", \"bundlePath\": \""
                + directory.resolve("missing-bundle.json").toString().replace("\\", "\\\\") + "\"}";

        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", "Ran the analysis.", rawEvidence);
        pumpEdt();

        assertAll(
                () -> assertNotNull(findButtonByText(view, "Show raw output"),
                        "The existing text-only raw-evidence disclosure must still render"),
                () -> assertNull(findByAccessibleName(view, "Evidence screenshot preview"),
                        "An unresolvable evidence path must never render a preview row"));
    }

    /**
     * Issue #3642: a message whose raw evidence resolves (via {@link DoctorEvidenceImageLocator})
     * to a real, readable screenshot must render a click-to-open preview row (built with {@link
     * ImagePreviewSupport}) alongside the existing raw-evidence disclosure. The lookup runs off
     * the EDT on {@code ShaftPluginExecutor}'s pool and lands back via {@code runOnEdt}, so this
     * polls (bounded, pumping the EDT each attempt) instead of asserting immediately after append.
     */
    @Test
    void resolvableScreenshotEvidenceRendersAClickToOpenPreviewRow(@TempDir Path directory) throws IOException {
        Files.createDirectories(directory.resolve("artifacts"));
        BufferedImage image = new BufferedImage(40, 40, BufferedImage.TYPE_INT_RGB);
        if (!ImageIO.write(image, "png", directory.resolve("artifacts/e-1.png").toFile())) {
            throw new IOException("No PNG writer available for the test fixture image");
        }
        Files.writeString(directory.resolve("doctor-evidence.json"), """
                {
                  "schemaVersion": "1.0",
                  "bundleId": "bundle-abc",
                  "evidence": [
                    {
                      "id": "e-1",
                      "category": "SCREENSHOT",
                      "mediaType": "image/png",
                      "relativePath": "artifacts/e-1.png",
                      "sha256": "deadbeef",
                      "sizeBytes": 128,
                      "redacted": false,
                      "truncated": false,
                      "attributes": {},
                      "provenance": {"adapter": "allure-screenshot", "sourceReference": "attempt.png", "originalSha256": "deadbeef"}
                    }
                  ],
                  "redaction": {"appliedRules": [], "removedFieldNames": [], "omittedItems": 0},
                  "metadata": {}
                }
                """, StandardCharsets.UTF_8);
        String rawEvidence = "{\"schemaVersion\": \"1.0\", \"status\": \"DETERMINISTIC\", \"bundlePath\": \""
                + directory.resolve("doctor-evidence.json").toString().replace("\\", "\\\\") + "\"}";

        AssistantTranscriptView view = new AssistantTranscriptView();
        view.append("assistant", "Ran the analysis.", rawEvidence);

        JLabel preview = awaitByAccessibleName(view, "Evidence screenshot preview", Duration.ofSeconds(5));

        assertAll(
                () -> assertNotNull(preview, "A resolvable, readable screenshot must render a preview row"),
                () -> assertNotNull(preview.getIcon(), "The preview row must carry a decoded, scaled icon"),
                () -> assertNotNull(findButtonByText(view, "Show raw output"),
                        "The preview row must render alongside, not instead of, the existing disclosure"));
    }

    private static JLabel awaitByAccessibleName(Component root, String accessibleName, Duration timeout) {
        Instant deadline = Instant.now().plus(timeout);
        while (Instant.now().isBefore(deadline)) {
            pumpEdt();
            Component found = findByAccessibleName(root, accessibleName);
            if (found instanceof JLabel label) {
                return label;
            }
            try {
                Thread.sleep(20);
            } catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt();
                return null;
            }
        }
        return null;
    }

    /** Forces the EDT to process every runnable queued so far (e.g. by {@code runOnEdt}'s
     * {@link SwingUtilities#invokeLater} fallback) before returning. */
    private static void pumpEdt() {
        try {
            SwingUtilities.invokeAndWait(() -> { });
        } catch (Exception pumpFailure) {
            Thread.currentThread().interrupt();
        }
    }

    private static String manyLineParagraphs(int count) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < count; i++) {
            builder.append("Line ").append(i).append("\n\n");
        }
        return builder.toString();
    }

    private static JButton findButtonByText(Component component, String text) {
        if (component instanceof JButton button && text.equals(button.getText())) {
            return button;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JButton found = findButtonByText(child, text);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    /**
     * The collapse toggle (unlike the raw-evidence toggle) always exists in the tree so {@code
     * updateCollapseState()} can flip its visibility as streamed content grows past the threshold --
     * a plain {@link #findButtonByText} match on it is not user-observable. Requires the button
     * itself be visible, matching what a real user (and a screen reader) would actually see.
     */
    private static JButton findVisibleButtonByText(Component component, String text) {
        if (component instanceof JButton button && button.isVisible() && text.equals(button.getText())) {
            return button;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                JButton found = findVisibleButtonByText(child, text);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static List<JButton> findButtonsByText(Component component, String text) {
        List<JButton> result = new ArrayList<>();
        collectButtonsByText(component, text, result);
        return result;
    }

    private static void collectButtonsByText(Component component, String text, List<JButton> result) {
        if (component instanceof JButton button && text.equals(button.getText())) {
            result.add(button);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                collectButtonsByText(child, text, result);
            }
        }
    }

    private static <T extends Component> T findByType(Component component, Class<T> type) {
        if (type.isInstance(component)) {
            return type.cast(component);
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                T found = findByType(child, type);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static Component findByAccessibleName(Component component, String accessibleName) {
        if (component.getAccessibleContext() != null
                && accessibleName.equals(component.getAccessibleContext().getAccessibleName())) {
            return component;
        }
        if (component instanceof Container container) {
            for (Component child : container.getComponents()) {
                Component found = findByAccessibleName(child, accessibleName);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static boolean isVisibleInHierarchy(Component component) {
        for (Component current = component; current != null; current = current.getParent()) {
            if (!current.isVisible()) {
                return false;
            }
        }
        return true;
    }
}
