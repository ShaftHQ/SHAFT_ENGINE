package com.shaft.intellij.ui;

import com.intellij.ui.components.JBTextArea;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
import javax.swing.JEditorPane;
import java.awt.Component;
import java.awt.Container;
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
