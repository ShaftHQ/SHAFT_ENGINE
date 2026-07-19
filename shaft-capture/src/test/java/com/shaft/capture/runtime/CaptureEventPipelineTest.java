package com.shaft.capture.runtime;

import com.shaft.capture.collector.BrowserSignal;
import com.shaft.capture.generate.CaptureGenerationRequest;
import com.shaft.capture.generate.CaptureGenerationResult;
import com.shaft.capture.generate.CaptureGenerator;
import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.privacy.CapturePrivacyPolicy;
import com.shaft.capture.storage.CaptureSessionStore;
import com.shaft.pilot.ai.ApprovalPolicy;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
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
    void asyncTypedInteractionDeliveredThroughLoopbackSinkStaysInTheSameLogicalWindow(
            @TempDir Path temp) throws Exception {
        // Issue #3803: BrowserEventSink -- the loopback HTTP channel wired as a second delivery
        // path for BiDi preload signals by #3495 -- tags every signal it carries with the literal
        // browsing-context id "loopback" (BrowserEventSink.java:230), never the tab's real BiDi
        // context id. An async-typed interaction (e.g. a self-driving demo login that types via
        // setTimeout, arriving ~2.5s after the page opened) can race across that channel and win:
        // when it does, logicalWindow() saw a contextId it had never seen before and minted a
        // phantom second logical window for a session that only ever had one tab, emitting a
        // WindowEvent.SWITCH that CaptureGenerator then rejects as a window never opened.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        String realContextId = "context-tab-1";
        // BiDi's own BrowsingContextInspector reports the tab's real context id for window-open
        // and navigation-committed signals: those never go through the loopback sink.
        pipeline.accept(signalFromContext(
                "window_open", START, realContextId, Map.of(), Map.of(), Map.of()));
        pipeline.accept(signalFromContext(
                "navigation", START.plusMillis(50), realContextId, Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/login")));
        pipeline.accept(signalFromContext(
                "input", START.plusSeconds(3), "loopback", usernameTarget(),
                Map.of("value", "shaft.user", "committed", true), Map.of()));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertFalse(events.stream().anyMatch(event -> event instanceof CaptureEvent.WindowEvent windowEvent
                        && windowEvent.action() == CaptureEvent.WindowAction.SWITCH),
                "A single tab that never opened another window must never emit a WindowEvent.SWITCH: "
                        + events);
        CaptureEvent.TypeEvent typed = assertInstanceOf(CaptureEvent.TypeEvent.class, events.getLast());
        assertEquals("window-1", typed.context().page().logicalWindowId(),
                "The loopback-delivered interaction must resolve to the same logical window as the "
                        + "tab's real BiDi context, not mint a second one.");

        CaptureGenerationResult generated = new CaptureGenerator().generate(new CaptureGenerationRequest(
                output, temp.resolve("generated"), "generated.capture", "", false,
                false, false, Duration.ofMinutes(1),
                CaptureGenerationRequest.EnrichmentMode.NONE, null, false,
                ApprovalPolicy.denyAll()));
        assertTrue(generated.successful(),
                "Codegen must succeed for a single-tab async-typed session: "
                        + generated.report().unsupportedEvents());
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
        // The navigation is buffered (blank navigationSource); it is itself a consequence of that
        // click (within the interaction window) so once flushed at stop it never becomes a
        // recorded step.
        pipeline.accept(signal("navigation", START.plusMillis(1), Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/next")));
        pipeline.close();

        CaptureSession session = store.read();
        assertEquals(1, session.events().size());
        String dom = session.events().getFirst().context().extensions().get("domSnapshot").asText();
        assertTrue(dom.contains("button id=\"submit\""));
        assertFalse(dom.contains("abcdefghijklmnop"));
    }

    @Test
    void suppressesNavigationsCausedByRecentInteractions(@TempDir Path temp) {
        // A navigation shortly after a user interaction (link click, form submit, server redirect
        // chain) is a consequence of that interaction, not a navigation the user performed;
        // recording it generated a phantom navigateToURL step. Spontaneous navigations (address
        // bar, initial open) must still be recorded.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("navigation", START, Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/home")));
        pipeline.accept(signal("click", START.plusSeconds(2), buttonTarget(),
                Map.of("button", 0, "clickCount", 1), Map.of()));
        // Submit response page plus its redirect hop: both within the interaction window.
        pipeline.accept(signal("navigation", START.plusSeconds(3), Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/search?q=x")));
        pipeline.accept(signal("navigation", START.plusSeconds(4), Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/results?q=x")));
        // Long after the last interaction: a genuine user navigation, recorded.
        pipeline.accept(signal("navigation", START.plusSeconds(60), Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/pricing")));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        List<CaptureEvent.NavigationEvent> navigations = events.stream()
                .filter(CaptureEvent.NavigationEvent.class::isInstance)
                .map(CaptureEvent.NavigationEvent.class::cast)
                .toList();
        assertEquals(2, navigations.size(),
                "Only the initial open and the spontaneous navigation are user steps; "
                        + "interaction-consequence navigations must be suppressed.");
        assertEquals("https://example.test/home", navigations.get(0).targetUrl());
        assertEquals("https://example.test/pricing", navigations.get(1).targetUrl());
        assertEquals(1, events.stream().filter(CaptureEvent.ClickEvent.class::isInstance).count());
    }

    @Test
    void suppressesNavigationWhenCollectorSignalWinsTheRaceAgainstItsCausingClick(@TempDir Path temp) {
        // Root cause of the phantom-navigation bug: the BiDi collector's navigationCommitted /
        // URL-polling signal (blank navigationSource) can reach accept() before the click's own
        // slower JS-channel signal, even though the click happened first and caused the
        // navigation. Simulate the race directly: accept() the navigation before the click, with
        // the navigation's own timestamp after the click's (it really is the click's consequence).
        // Buffering the navigation lets the click's accept() call -- which stamps
        // lastInteractionAt immediately regardless of buffering -- win before the navigation is
        // evaluated at the debounce flush.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("navigation", START.plusMillis(50), Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/redirected")));
        pipeline.accept(signal("click", START, buttonTarget(),
                Map.of("button", 0, "clickCount", 1), Map.of()));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertEquals(1, events.stream().filter(CaptureEvent.ClickEvent.class::isInstance).count(),
                "The click that caused the navigation must still be recorded.");
        assertFalse(events.stream().anyMatch(CaptureEvent.NavigationEvent.class::isInstance),
                "The racy collector-generated navigation must be suppressed as a click consequence, "
                        + "not persisted as a phantom step.");
    }

    @Test
    void navigationRemainsAConsequenceOfItsCausingInteractionAfterALaterUnrelatedInteraction(@TempDir Path temp) {
        // Regression (the ENTER-triggered "second-replace search journey"): a navigation genuinely
        // caused by an earlier interaction (ENTER) must stay suppressed even when a later,
        // unrelated interaction (a click on the newly loaded page) is recorded before this
        // buffered navigation is evaluated. A single scalar "lastInteractionAt" would be
        // overwritten by the later click, wrongly making the navigation appear to predate its own
        // real, earlier cause once compared only against whatever interaction is currently most
        // recent instead of the one that actually preceded it.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("navigation", START, Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/search")));
        pipeline.accept(signal("keyboard", START.plusMillis(50), buttonTarget(),
                Map.of("keys", List.of("ENTER")), Map.of()));
        // The ENTER-caused navigation's own timestamp: genuinely after ENTER, but it still sits in
        // pendingNavigations when the unrelated click below is recorded.
        pipeline.accept(signal("navigation", START.plusMillis(120), Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/results")));
        pipeline.accept(signal("click", START.plusMillis(400), buttonTarget(),
                Map.of("button", 0, "clickCount", 1), Map.of()));
        pipeline.close();

        List<CaptureEvent.NavigationEvent> navigations = store.read().events().stream()
                .filter(CaptureEvent.NavigationEvent.class::isInstance)
                .map(CaptureEvent.NavigationEvent.class::cast)
                .toList();
        assertEquals(1, navigations.size(),
                "The ENTER-caused navigation must stay suppressed even after a later unrelated click; got: "
                        + navigations.stream().map(CaptureEvent.NavigationEvent::targetUrl).toList());
        assertEquals("https://example.test/search", navigations.getFirst().targetUrl());
        assertEquals(1, store.read().events().stream().filter(CaptureEvent.ClickEvent.class::isInstance).count());
    }

    @Test
    void recordsStandaloneNavigationWithNoPrecedingInteraction(@TempDir Path temp) {
        // Address-bar navigation (or the initial page open) with no click anywhere near it must
        // still be recorded even though it now goes through the pendingNavigations buffer like
        // every other blank-navigationSource signal.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("navigation", START, Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/standalone")));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertEquals(1, events.size());
        assertEquals("https://example.test/standalone",
                assertInstanceOf(CaptureEvent.NavigationEvent.class, events.getFirst()).targetUrl());
    }

    @Test
    void neverRecordsHistoryRewriteNavigationsAsSteps(@TempDir Path temp) {
        // pushState/replaceState URL rewrites (SPA routes, results-page canonicalization such as
        // DuckDuckGo appending query flags after a search) are tagged navigationSource=history by
        // the BiDi collector and must never become navigation steps, even with no recent
        // interaction.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        java.util.concurrent.atomic.AtomicReference<String> currentUrl = new java.util.concurrent.atomic.AtomicReference<>("");
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), currentUrl::set, ignored -> {
                });

        pipeline.accept(signal("navigation", START, Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/results")));
        pipeline.accept(signal("navigation", START.plusSeconds(30), Map.of(),
                Map.of("action", "OPEN", "navigationSource", "history"),
                Map.of("url", "https://example.test/results?ia=web")));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertEquals(1, events.size());
        assertEquals("https://example.test/results",
                assertInstanceOf(CaptureEvent.NavigationEvent.class, events.getFirst()).targetUrl());
        assertEquals("https://example.test/results?ia=web", currentUrl.get(),
                "History rewrites must still refresh the reported current URL.");
    }

    @Test
    void recordsBackForwardTraversalDespiteRecentInteraction(@TempDir Path temp) {
        // Back/forward is a navigation the user performed even right after an interaction, so
        // the overlay reports it as user_traversal and the interaction window must not swallow
        // it. The overlay row's client action id rides along, making the row a server-backed
        // step that survives step syncs.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("navigation", START, Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/a")));
        pipeline.accept(signal("click", START.plusSeconds(3), buttonTarget(),
                Map.of("button", 0, "clickCount", 1), Map.of()));
        pipeline.accept(signal("navigation", START.plusSeconds(4), Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/b")));
        pipeline.accept(signal("navigation", START.plusSeconds(6), Map.of(),
                Map.of("action", "OPEN", "navigationSource", "user_traversal",
                        "clientActionId", "traversal-9", "stepDescription", "Navigate to https://example.test/a"),
                Map.of("url", "https://example.test/a")));
        pipeline.close();

        List<CaptureEvent.NavigationEvent> navigations = store.read().events().stream()
                .filter(CaptureEvent.NavigationEvent.class::isInstance)
                .map(CaptureEvent.NavigationEvent.class::cast)
                .toList();
        assertEquals(2, navigations.size(),
                "The click-consequence navigation is suppressed but the user's back traversal "
                        + "must be recorded.");
        assertEquals("https://example.test/a", navigations.get(1).targetUrl());
        assertEquals("traversal-9",
                navigations.get(1).context().extensions().get("clientActionId").asText());
        assertTrue(store.steps().stream()
                        .anyMatch(step -> "traversal-9".equals(step.clientActionId())),
                "The traversal must surface in the server-backed step list.");
    }

    @Test
    void overlayReportAttachesIdentityToFreshlyRecordedNavigation(@TempDir Path temp) {
        // The overlay's "Open" breadcrumb (and any overlay-reported navigation a collector beat
        // it to) only contributes its row identity to the already-recorded event, so the row
        // survives step syncs without ever appending a duplicate navigation.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("navigation", START, Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/home")));
        pipeline.accept(signal("navigation", START.plusMillis(600), Map.of(),
                Map.of("action", "OPEN", "navigationSource", "user_annotation",
                        "clientActionId", "open-1", "stepDescription", "Open https://example.test/home"),
                Map.of("url", "https://example.test/home")));
        pipeline.close();

        List<CaptureEvent> events = store.read().events();
        assertEquals(1, events.size(), "The annotation must never append a second open event.");
        CaptureEvent.NavigationEvent navigation =
                assertInstanceOf(CaptureEvent.NavigationEvent.class, events.getFirst());
        assertEquals("open-1", navigation.context().extensions().get("clientActionId").asText());
        assertEquals("Open https://example.test/home",
                navigation.context().extensions().get("stepDescription").asText());
        assertEquals(1, store.steps().size());
    }

    @Test
    void deletedOverlayNavigationStaysDeletedWhenRevocationOvertakesTheSignal(@TempDir Path temp) {
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("step_delete", START, Map.of(),
                Map.of("clientActionId", "traversal-4"), Map.of()));
        pipeline.accept(signal("navigation", START.plusSeconds(1), Map.of(),
                Map.of("action", "OPEN", "navigationSource", "user_traversal",
                        "clientActionId", "traversal-4"),
                Map.of("url", "https://example.test/back")));
        pipeline.close();

        assertEquals(0, store.read().events().size(),
                "A deleted overlay navigation must stay deleted even when the revocation "
                        + "overtakes the navigation signal across channels.");
    }

    @Test
    void debouncesSameUrlNavigationRedeliveredAcrossChannels(@TempDir Path temp) {
        // BiDi's navigationCommitted and the recorder's own explicit OPEN signal describe the
        // same navigation; across a slow page load they can arrive several seconds apart.
        Path output = temp.resolve("session.json");
        CaptureSessionStore store = startedStore(output);
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store, output, CapturePrivacyPolicy.defaults(), ignored -> {
                }, ignored -> {
                });

        pipeline.accept(signal("navigation", START, Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/home")));
        pipeline.accept(signal("navigation", START.plusSeconds(4), Map.of(),
                Map.of("action", "OPEN"), Map.of("url", "https://example.test/home")));
        pipeline.close();

        assertEquals(1, store.read().events().size());
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
        pipeline.accept(signal("keyboard", START.plusMillis(1), buttonTarget(),
                Map.of("keys", List.of("CONTROL", "S")), Map.of()));
        pipeline.accept(signal("step_update", START.plusMillis(2), Map.of(),
                Map.of("clientActionId", "ui-3", "description", "Click primary submit"), Map.of()));

        CaptureSession updated = store.read();
        assertEquals("Click primary submit",
                updated.events().getFirst().context().extensions().get("userDescription").asText());

        pipeline.accept(signal("step_delete", START.plusMillis(3), Map.of(),
                Map.of("clientActionId", "ui-3"), Map.of()));

        CaptureSession deleted = store.read();
        assertEquals(1, deleted.events().size());
        assertInstanceOf(CaptureEvent.KeyboardEvent.class, deleted.events().getFirst());
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

    private static BrowserSignal signalFromContext(
            String kind,
            Instant timestamp,
            String browsingContextId,
            Map<String, Object> target,
            Map<String, Object> data,
            Map<String, Object> pageOverrides) {
        Map<String, Object> page = new java.util.LinkedHashMap<>(Map.of(
                "url", "https://example.test/login",
                "title", "Login",
                "width", 1280,
                "height", 720,
                "framePath", List.of()));
        page.putAll(pageOverrides);
        return new BrowserSignal(kind, timestamp, browsingContextId, page, target, data);
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
