package com.shaft.intellij.ui;

import com.intellij.ui.components.JBTextArea;
import org.junit.jupiter.api.Test;

import javax.swing.JButton;
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

    private static boolean isVisibleInHierarchy(Component component) {
        for (Component current = component; current != null; current = current.getParent()) {
            if (!current.isVisible()) {
                return false;
            }
        }
        return true;
    }
}
