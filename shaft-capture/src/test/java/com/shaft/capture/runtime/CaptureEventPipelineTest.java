package com.shaft.capture.runtime;

import com.shaft.capture.collector.BrowserSignal;
import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.privacy.CapturePrivacyPolicy;
import com.shaft.capture.storage.CaptureSessionStore;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureEventPipelineTest {
    private static final String SECRET_CANARY = "capture-secret-canary-value";
    private static final Instant START = Instant.parse("2026-06-11T10:00:00Z");

    @Test
    void debouncesSemanticEventsAndClassifiesValuesBeforePersistence(@TempDir Path temp) throws Exception {
        Path output = temp.resolve("path with spaces").resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("navigation", START, Map.of(), Map.of("action", "OPEN"), Map.of()));
        pipeline.accept(signal("input", START.plusMillis(10), passwordTarget(),
                Map.of("value", SECRET_CANARY, "committed", true), Map.of()));
        pipeline.accept(signal("input", START.plusMillis(20), usernameTarget(),
                Map.of("value", "a"), Map.of()));
        pipeline.accept(signal("input", START.plusMillis(30), usernameTarget(),
                Map.of("value", "alice"), Map.of()));
        pipeline.accept(signal("click", START.plusMillis(40), buttonTarget(),
                Map.of("button", 0, "clickCount", 1), Map.of()));
        pipeline.accept(signal("click", START.plusMillis(50), buttonTarget(),
                Map.of("button", 0, "clickCount", 2), Map.of()));
        pipeline.close();

        CaptureSession session = store.read();
        assertEquals(4, session.events().size());
        assertInstanceOf(CaptureEvent.NavigationEvent.class, session.events().get(0));
        CaptureEvent.TypeEvent secret = assertInstanceOf(
                CaptureEvent.TypeEvent.class, session.events().get(1));
        assertEquals(ExternalTestDataReference.DataClassification.SECRET,
                secret.value().classification());
        CaptureEvent.TypeEvent ordinary = assertInstanceOf(
                CaptureEvent.TypeEvent.class, session.events().get(2));
        assertEquals(ExternalTestDataReference.DataClassification.ORDINARY,
                ordinary.value().classification());
        CaptureEvent.ClickEvent click = assertInstanceOf(
                CaptureEvent.ClickEvent.class, session.events().get(3));
        assertEquals(2, click.clickCount());

        String sessionJson = Files.readString(output, StandardCharsets.UTF_8);
        String dataJson = Files.readString(output.getParent().resolve("capture-data.json"),
                StandardCharsets.UTF_8);
        assertFalse(sessionJson.contains(SECRET_CANARY));
        assertFalse(dataJson.contains(SECRET_CANARY));
        assertTrue(dataJson.contains("alice"));
    }

    @Test
    void pendingSignalCountReportsUncommittedInputAndDebouncedClicks(@TempDir Path temp) {
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        assertEquals(0, pipeline.pendingSignalCount());
        // Fresh timestamps keep the click inside its debounce window and the uncommitted input
        // parked, so both stay pending until close() flushes them.
        Instant now = Instant.now();
        pipeline.accept(signal("input", now, usernameTarget(),
                Map.of("value", "ali"), Map.of()));
        pipeline.accept(signal("click", now.plusMillis(1), buttonTarget(),
                Map.of("button", 0, "clickCount", 1), Map.of()));
        assertEquals(2, pipeline.pendingSignalCount());
        assertEquals(0, pipeline.eventCount());

        pipeline.close();
        assertEquals(0, pipeline.pendingSignalCount());
        assertEquals(2, store.read().events().size());
    }

    @Test
    void suppressesBrowserSynthesizedClickOnInvisibleTarget(@TempDir Path temp) {
        // Issue #3426 B2: pressing Enter in a form makes the browser "click" the form's default
        // submit button even when it is invisible. A real user can never click an element with no
        // rendered box, so such clicks are phantom steps and must not be recorded.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("click", START, invisibleSubmitTarget(),
                Map.of("button", 0, "clickCount", 1), Map.of()));
        pipeline.accept(signal("click", START.plusMillis(500), buttonTarget(),
                Map.of("button", 0, "clickCount", 1), Map.of()));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertEquals(1, events.size());
        assertEquals("submit",
                assertInstanceOf(CaptureEvent.ClickEvent.class, events.get(0)).target().logicalElementId());
    }

    @Test
    void suppressesDuplicateCommittedInputReEmittedByFormSubmission(@TempDir Path temp) {
        // Issue #3426 B2: Enter flushes the pending typed value (keyboard event), then the form
        // submission fires "change" re-announcing the exact same value. Only one type event may
        // survive.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("input", START, usernameTarget(),
                Map.of("value", "shaft_engine"), Map.of()));
        pipeline.accept(signal("keyboard", START.plusMillis(800), usernameTarget(),
                Map.of("keys", List.of("ENTER")), Map.of()));
        pipeline.accept(signal("input", START.plusMillis(1000), usernameTarget(),
                Map.of("value", "shaft_engine", "committed", true), Map.of()));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertEquals(2, events.size());
        assertInstanceOf(CaptureEvent.TypeEvent.class, events.get(0));
        assertInstanceOf(CaptureEvent.KeyboardEvent.class, events.get(1));
    }

    @Test
    void normalizesSelectToggleUploadKeyboardAndAlertActions(@TempDir Path temp) {
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("select", START, selectTarget(),
                Map.of("visibleText", "Egypt", "value", "eg", "index", 1), Map.of()));
        pipeline.accept(signal("toggle", START.plusMillis(1), checkboxTarget(),
                Map.of("checked", true), Map.of()));
        pipeline.accept(signal("upload", START.plusMillis(2), uploadTarget(),
                Map.of("fileName", "avatar.png", "mediaType", "image/png", "sizeBytes", 42), Map.of()));
        pipeline.accept(signal("keyboard", START.plusMillis(3), usernameTarget(),
                Map.of("keys", List.of("CONTROL", "A")), Map.of()));
        pipeline.accept(signal("alert", START.plusMillis(4), Map.of(),
                Map.of("accepted", true, "text", "ordinary prompt text"), Map.of()));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertEquals(6, events.size());
        assertInstanceOf(CaptureEvent.SelectEvent.class, events.get(0));
        assertInstanceOf(CaptureEvent.ToggleEvent.class, events.get(1));
        assertInstanceOf(CaptureEvent.UploadEvent.class, events.get(2));
        assertInstanceOf(CaptureEvent.KeyboardEvent.class, events.get(3));
        assertEquals(CaptureEvent.AlertAction.TYPE,
                assertInstanceOf(CaptureEvent.AlertEvent.class, events.get(4)).action());
        assertEquals(CaptureEvent.AlertAction.ACCEPT,
                assertInstanceOf(CaptureEvent.AlertEvent.class, events.get(5)).action());
    }

    @Test
    void recordsVerificationEventsAndClassifiesExpectedValues(@TempDir Path temp) throws Exception {
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("verification", START, usernameTarget(),
                Map.of("verification", "TEXT_EQUALS", "expected", "Welcome alice"), Map.of()));
        pipeline.accept(signal("verification", START.plusMillis(1), passwordTarget(),
                Map.of("verification", "ATTRIBUTE_EQUALS", "attributeName", "data-secret",
                        "expected", SECRET_CANARY), Map.of()));
        pipeline.accept(signal("verification", START.plusMillis(2), Map.of(),
                Map.of("verification", "URL_CONTAINS", "expected", "/form"), Map.of()));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertEquals(3, events.size());
        CaptureEvent.VerificationEvent text = assertInstanceOf(
                CaptureEvent.VerificationEvent.class, events.get(0));
        assertEquals(CaptureEvent.VerificationKind.TEXT_EQUALS, text.verification());
        assertEquals(ExternalTestDataReference.DataClassification.ORDINARY,
                text.expected().classification());
        CaptureEvent.VerificationEvent attribute = assertInstanceOf(
                CaptureEvent.VerificationEvent.class, events.get(1));
        assertEquals("data-secret", attribute.context().extensions().get("attributeName").asText());
        assertEquals(ExternalTestDataReference.DataClassification.SECRET,
                attribute.expected().classification());
        CaptureEvent.VerificationEvent url = assertInstanceOf(
                CaptureEvent.VerificationEvent.class, events.get(2));
        assertEquals(CaptureEvent.VerificationKind.URL_CONTAINS, url.verification());
        assertNull(url.target());

        String sessionJson = Files.readString(output, StandardCharsets.UTF_8);
        String dataJson = Files.readString(output.getParent().resolve("capture-data.json"),
                StandardCharsets.UTF_8);
        assertTrue(dataJson.contains("Welcome alice"));
        assertFalse(sessionJson.contains(SECRET_CANARY));
        assertFalse(dataJson.contains(SECRET_CANARY));
    }

    @Test
    void locatorPreferenceMarksPreferredCandidateInNextCapturedTarget(@TempDir Path temp) {
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });
        Map<String, Object> target = target("submit", "button", "button", Map.of("id", "submit"), List.of(
                Map.of(
                        "strategy", "ID",
                        "expression", "submit",
                        "uniquenessCount", 1,
                        "visible", true,
                        "stable", true,
                        "signals", List.of("STABLE_ATTRIBUTE")),
                Map.of(
                        "strategy", "CSS",
                        "expression", "form > button",
                        "uniquenessCount", 1,
                        "visible", true,
                        "stable", true,
                        "signals", List.of("GENERATED"))));

        pipeline.accept(signal("locator_preference", START, target,
                Map.of("logicalElementId", "submit", "strategy", "CSS", "expression", "form > button"), Map.of()));
        pipeline.accept(signal("click", START.plusMillis(1), target,
                Map.of("button", 0, "clickCount", 1), Map.of()));
        pipeline.close();

        CaptureEvent.ClickEvent click = assertInstanceOf(
                CaptureEvent.ClickEvent.class, store.read().events().getFirst());
        LocatorCandidate preferred = click.target().locatorCandidates().getFirst();
        assertEquals(LocatorCandidate.LocatorStrategy.CSS, preferred.strategy());
        assertTrue(preferred.signals().contains(LocatorCandidate.LocatorSignal.USER_PROVIDED));
    }

    @Test
    void keepsSlowTypingAsOneTypeEventUntilSpecialKey(@TempDir Path temp) throws Exception {
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("input", START, usernameTarget(),
                Map.of("value", "a", "clientActionId", "ui-1"), Map.of()));
        Thread.sleep(500);
        pipeline.accept(signal("input", START.plusSeconds(1), usernameTarget(),
                Map.of("value", "alice", "clientActionId", "ui-1"), Map.of()));
        pipeline.accept(signal("keyboard", START.plusSeconds(2), usernameTarget(),
                Map.of("keys", List.of("ENTER")), Map.of()));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertEquals(2, events.size());
        assertInstanceOf(CaptureEvent.TypeEvent.class, events.get(0));
        assertEquals("ui-1", events.get(0).context().extensions().get("clientActionId").asText());
        assertInstanceOf(CaptureEvent.KeyboardEvent.class, events.get(1));
        String dataJson = Files.readString(output.getParent().resolve("capture-data.json"),
                StandardCharsets.UTF_8);
        assertFalse(dataJson.contains("\"a\""));
        assertTrue(dataJson.contains("alice"));
    }

    @Test
    void typingCapitalsAndSymbolsStaysAsOneTypeEvent(@TempDir Path temp) throws Exception {
        // The browser recorder must not emit a "keyboard" signal for Shift-only combinations
        // (capital letters, shifted symbols such as "!"): those are normal characters already
        // carried by the "input" signal's value. Only "input" signals are sent here, matching
        // the corrected browser behavior, and typing "Hello World!" must collapse into a single
        // TypeEvent with the final value instead of fragmenting at every shifted character.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        String[] progressiveValues = {"H", "He", "Hel", "Hell", "Hello", "Hello ",
                "Hello W", "Hello Wo", "Hello Wor", "Hello Worl", "Hello World", "Hello World!"};
        for (int index = 0; index < progressiveValues.length; index++) {
            pipeline.accept(signal("input", START.plusMillis(index), usernameTarget(),
                    Map.of("value", progressiveValues[index], "clientActionId", "ui-1"), Map.of()));
        }
        pipeline.accept(signal("input", START.plusMillis(progressiveValues.length), usernameTarget(),
                Map.of("value", "Hello World!", "committed", true, "clientActionId", "ui-1"), Map.of()));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertEquals(1, events.size());
        assertInstanceOf(CaptureEvent.TypeEvent.class, events.getFirst());
        String dataJson = Files.readString(output.getParent().resolve("capture-data.json"),
                StandardCharsets.UTF_8);
        assertTrue(dataJson.contains("Hello World!"));
        assertFalse(dataJson.contains("\"Hello\""));
    }

    @Test
    void suppressesStandaloneEditingKeysInTextInputsAndCoalescesIntoFinalValue(@TempDir Path temp) throws Exception {
        // Type 'abcd', press Backspace twice (should not emit standalone keyboard events),
        // type 'xy', commit. Should result in a single TypeEvent with final value 'abxy',
        // zero standalone Backspace/keyboard events, and zero Delete actions.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("input", START, usernameTarget(),
                Map.of("value", "a", "clientActionId", "ui-7"), Map.of()));
        pipeline.accept(signal("input", START.plusMillis(10), usernameTarget(),
                Map.of("value", "ab", "clientActionId", "ui-7"), Map.of()));
        pipeline.accept(signal("input", START.plusMillis(20), usernameTarget(),
                Map.of("value", "abc", "clientActionId", "ui-7"), Map.of()));
        pipeline.accept(signal("input", START.plusMillis(30), usernameTarget(),
                Map.of("value", "abcd", "clientActionId", "ui-7"), Map.of()));
        // Simulate Backspace twice (editing key, no modifiers) — should be suppressed
        pipeline.accept(signal("keyboard", START.plusMillis(40), usernameTarget(),
                Map.of("keys", List.of("BACKSPACE")), Map.of()));
        pipeline.accept(signal("keyboard", START.plusMillis(50), usernameTarget(),
                Map.of("keys", List.of("BACKSPACE")), Map.of()));
        // Type 'xy' after backspacing
        pipeline.accept(signal("input", START.plusMillis(60), usernameTarget(),
                Map.of("value", "abxy", "clientActionId", "ui-7"), Map.of()));
        pipeline.accept(signal("input", START.plusMillis(70), usernameTarget(),
                Map.of("value", "abxy", "committed", true, "clientActionId", "ui-7"), Map.of()));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        // Should have exactly one TypeEvent (no keyboard events for Backspace)
        assertEquals(1, events.size());
        assertInstanceOf(CaptureEvent.TypeEvent.class, events.getFirst());
        String dataJson = Files.readString(output.getParent().resolve("capture-data.json"),
                StandardCharsets.UTF_8);
        // The final value should be "abxy" (after backspacing "cd")
        assertTrue(dataJson.contains("abxy"));
        // Intermediate values should not be stored
        assertFalse(dataJson.contains("abcd"));
        // No keyboard events should be recorded
        boolean hasKeyboardEvent = events.stream().anyMatch(e -> e instanceof CaptureEvent.KeyboardEvent);
        assertFalse(hasKeyboardEvent);
    }

    @Test
    void preservesBackspaceOutsideTextInputElements(@TempDir Path temp) throws Exception {
        // Backspace on a button should still record as a keyboard event (non-input context)
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("keyboard", START, buttonTarget(),
                Map.of("keys", List.of("BACKSPACE")), Map.of()));
        pipeline.accept(signal("click", START.plusMillis(1), buttonTarget(),
                Map.of("button", 0, "clickCount", 1), Map.of()));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertEquals(2, events.size());
        assertInstanceOf(CaptureEvent.KeyboardEvent.class, events.get(0));
        assertInstanceOf(CaptureEvent.ClickEvent.class, events.get(1));
    }

    @Test
    void writesActionsBeforeStopAndStoresSanitizedDomEvidence(@TempDir Path temp) {
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("click", START, buttonTarget(),
                Map.of("button", 0, "clickCount", 1, "clientActionId", "ui-2"),
                Map.of("domSnapshot", "<html><body><button id=\"submit\">Save</button>"
                        + "<span>bearer abcdefghijklmnop</span></body></html>")));
        pipeline.accept(signal("navigation", START.plusMillis(1), Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/next")));

        CaptureSession session = store.read();
        assertEquals(2, session.events().size());
        String dom = session.events().getFirst().context().extensions().get("domSnapshot").asText();
        assertTrue(dom.contains("button id=\"submit\""));
        assertFalse(dom.contains("abcdefghijklmnop"));
    }

    @Test
    void editsAndDeletesPersistedStepsByClientActionId(@TempDir Path temp) {
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("click", START, buttonTarget(),
                Map.of("button", 0, "clickCount", 1, "clientActionId", "ui-3"), Map.of()));
        pipeline.accept(signal("navigation", START.plusMillis(1), Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/next")));
        pipeline.accept(signal("step_update", START.plusMillis(2), Map.of(),
                Map.of("clientActionId", "ui-3", "description", "Click primary submit"), Map.of()));

        CaptureSession updated = store.read();
        assertEquals("Click primary submit",
                updated.events().getFirst().context().extensions().get("userDescription").asText());

        pipeline.accept(signal("step_delete", START.plusMillis(3), Map.of(),
                Map.of("clientActionId", "ui-3"), Map.of()));

        CaptureSession deleted = store.read();
        assertEquals(1, deleted.events().size());
        assertInstanceOf(CaptureEvent.NavigationEvent.class, deleted.events().getFirst());
    }

    @Test
    void reordersPersistedStepsByClientActionId(@TempDir Path temp) {
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("click", START, usernameTarget(),
                Map.of("button", 0, "clickCount", 1, "clientActionId", "ui-5"), Map.of()));
        pipeline.accept(signal("click", START.plusMillis(1), buttonTarget(),
                Map.of("button", 0, "clickCount", 1, "clientActionId", "ui-6"), Map.of()));
        pipeline.accept(signal("step_reorder", START.plusMillis(2), Map.of(),
                Map.of("clientActionId", "ui-6", "direction", "up"), Map.of()));

        List<CaptureEvent> reordered = store.read().events();
        assertEquals("ui-6", reordered.get(0).context().extensions().get("clientActionId").asText());
        assertEquals("ui-5", reordered.get(1).context().extensions().get("clientActionId").asText());
        assertEquals(1, reordered.get(0).context().sequence());
        assertEquals(2, reordered.get(1).context().sequence());
    }

    @Test
    void deletingTypedStepPrunesExternalDataReference(@TempDir Path temp) throws Exception {
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("input", START, usernameTarget(),
                Map.of("value", "alice", "clientActionId", "ui-4"), Map.of()));
        pipeline.accept(signal("step_delete", START.plusMillis(1), Map.of(),
                Map.of("clientActionId", "ui-4"), Map.of()));

        CaptureSession deleted = store.read();
        assertTrue(deleted.events().isEmpty());
        assertTrue(deleted.dataReferences().isEmpty());
        assertEquals(0, pipeline.eventCount());
        String dataJson = Files.readString(output.getParent().resolve("capture-data.json"),
                StandardCharsets.UTF_8);
        assertFalse(dataJson.contains("alice"));
    }

    private static CaptureSessionStore startedStore(Path output) {
        CaptureSessionStore store = new CaptureSessionStore(output);
        store.start(CaptureSession.start(
                "pipeline",
                START,
                new BrowserMetadata("chrome", "1", "test", "browser", Map.of())));
        return store;
    }

    private static BrowserSignal signal(
            String kind,
            Instant timestamp,
            Map<String, Object> target,
            Map<String, Object> data,
            Map<String, Object> pageOverrides) {
        Map<String, Object> page = new java.util.LinkedHashMap<>(Map.of(
                "url", "https://example.test/form",
                "title", "Form",
                "width", 1280,
                "height", 720,
                "framePath", List.of()));
        page.putAll(pageOverrides);
        return new BrowserSignal(kind, timestamp, "context-1", page, target, data);
    }

    private static Map<String, Object> usernameTarget() {
        return target("username", "input", "textbox",
                Map.of("name", "username", "autocomplete", "username"));
    }

    private static Map<String, Object> passwordTarget() {
        return target("password", "input", "textbox",
                Map.of("name", "password", "type", "password"));
    }

    private static Map<String, Object> buttonTarget() {
        return target("submit", "button", "button", Map.of("id", "submit"));
    }

    private static Map<String, Object> selectTarget() {
        return target("country", "select", "combobox", Map.of("name", "country"));
    }

    private static Map<String, Object> checkboxTarget() {
        return target("terms", "input", "checkbox", Map.of("name", "terms", "type", "checkbox"));
    }

    private static Map<String, Object> uploadTarget() {
        return target("avatar", "input", "textbox", Map.of("name", "avatar", "type", "file"));
    }

    private static Map<String, Object> target(
            String id,
            String tag,
            String role,
            Map<String, String> attributes) {
        return target(id, tag, role, attributes, List.of(Map.of(
                "strategy", "ID",
                "expression", id,
                "uniquenessCount", 1,
                "visible", true,
                "stable", true,
                "signals", List.of("STABLE_ATTRIBUTE"))));
    }

    private static Map<String, Object> target(
            String id,
            String tag,
            String role,
            Map<String, String> attributes,
            List<Map<String, Object>> locators) {
        return Map.of(
                "logicalElementId", id,
                "tagName", tag,
                "role", role,
                "accessibleName", id,
                "label", id,
                "attributes", attributes,
                "locators", locators,
                "visible", true,
                "enabled", true,
                "selected", false);
    }

    private static Map<String, Object> invisibleSubmitTarget() {
        Map<String, Object> visibleShape = target("hidden-submit", "button", "button", Map.of("type", "submit"));
        Map<String, Object> shape = new java.util.LinkedHashMap<>(visibleShape);
        shape.put("visible", false);
        return Map.copyOf(shape);
    }
}
