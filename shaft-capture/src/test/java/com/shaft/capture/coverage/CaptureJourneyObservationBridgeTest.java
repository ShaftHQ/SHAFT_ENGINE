package com.shaft.capture.coverage;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.PageContext;
import com.shaft.coverage.journey.ObservationRecorder;
import com.shaft.coverage.journey.StateKind;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureJourneyObservationBridgeTest {

    @Test
    void recordSessionRejectsNullSessionOrRecorder() {
        assertThrows(IllegalArgumentException.class,
                () -> CaptureJourneyObservationBridge.recordSession(null, "j", new ObservationRecorder()));
        CaptureSession session = CaptureFixtures.representativeSession();
        assertThrows(IllegalArgumentException.class,
                () -> CaptureJourneyObservationBridge.recordSession(session, "j", null));
    }

    @Test
    void observationsFromBlankJourneyNameFallsBackToSessionIdAndMapsEventKinds() {
        CaptureSession session = CaptureFixtures.representativeSession();
        ObservationRecorder recorder = CaptureJourneyObservationBridge.observationsFrom(session, "  ");
        assertTrue(recorder.distinctCount() >= 10);
        assertTrue(recorder.observations().stream()
                .allMatch(observation -> observation.target().journey().value().equals(session.sessionId())));
        assertTrue(recorder.observations().stream()
                .anyMatch(observation -> observation.target().interaction().value().equals("navigation")));
        assertTrue(recorder.observations().stream()
                .anyMatch(observation -> observation.target().interaction().value().equals("wait")));
        assertTrue(recorder.observations().stream()
                .anyMatch(observation -> observation.target().state().kind() == StateKind.LOADING));
    }

    @Test
    void viewTokenUsesPathTitleOrRawUrlAndReplayStatusesMapToStates() {
        EventContext failed = new EventContext(1, CaptureFixtures.STARTED, CaptureFixtures.page(),
                EventContext.ReplayStatus.FAILED, List.of(), Map.of());
        EventContext unsupported = new EventContext(2, CaptureFixtures.STARTED, CaptureFixtures.page(),
                EventContext.ReplayStatus.UNSUPPORTED, List.of(), Map.of());
        EventContext blankUrl = new EventContext(3, CaptureFixtures.STARTED,
                new PageContext("  ", "Titled View", "w", List.of(), 800, 600),
                EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of());
        EventContext blankEverything = new EventContext(4, CaptureFixtures.STARTED,
                new PageContext(null, "  ", "w", List.of(), 800, 600),
                EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of());
        EventContext bareHost = new EventContext(5, CaptureFixtures.STARTED,
                new PageContext("https://example.test", "Host", "w", List.of(), 800, 600),
                EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of());
        EventContext badUri = new EventContext(6, CaptureFixtures.STARTED,
                new PageContext("not a uri://", "Bad", "w", List.of(), 800, 600),
                EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of());

        CaptureSession session = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "bridge-states",
                CaptureSession.SessionStatus.COMPLETED,
                CaptureFixtures.STARTED,
                CaptureFixtures.STARTED.plusSeconds(10),
                CaptureFixtures.browser(),
                List.of(
                        new CaptureEvent.ClickEvent(failed, CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        new CaptureEvent.ClickEvent(unsupported, CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        new CaptureEvent.WaitEvent(blankUrl, CaptureEvent.WaitCondition.ELEMENT_VISIBLE,
                                Duration.ofSeconds(1), CaptureFixtures.target(), null),
                        new CaptureEvent.ClickEvent(blankEverything, CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        new CaptureEvent.ClickEvent(bareHost, CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        new CaptureEvent.ClickEvent(badUri, CaptureFixtures.target(),
                                CaptureEvent.MouseButton.PRIMARY, 1),
                        verification(7, CaptureEvent.VerificationKind.TEXT_EQUALS, "ordinary value"),
                        verification(8, CaptureEvent.VerificationKind.TEXT_EQUALS, "empty cart"),
                        verification(9, CaptureEvent.VerificationKind.TEXT_EQUALS, "error banner"),
                        verification(10, CaptureEvent.VerificationKind.TEXT_EQUALS, "fail soft"),
                        verification(11, CaptureEvent.VerificationKind.TEXT_EQUALS, "loading spinner"),
                        verification(12, CaptureEvent.VerificationKind.TEXT_EQUALS, null)),
                List.of(),
                List.of(),
                null,
                Map.of());

        ObservationRecorder recorder = CaptureJourneyObservationBridge.observationsFrom(session, "checkout");
        assertTrue(recorder.distinctCount() >= 8);
        assertTrue(recorder.observations().stream()
                .anyMatch(o -> o.target().state().kind() == StateKind.ERROR));
        assertTrue(recorder.observations().stream()
                .anyMatch(o -> o.target().state().kind() == StateKind.UNKNOWN));
        assertTrue(recorder.observations().stream()
                .anyMatch(o -> o.target().view().value().equals("titled view")));
        assertTrue(recorder.observations().stream()
                .anyMatch(o -> o.target().view().value().equals("view")));
        assertTrue(recorder.observations().stream()
                .anyMatch(o -> o.target().view().value().equals("/")));
        assertTrue(recorder.observations().stream()
                .anyMatch(o -> o.target().state().kind() == StateKind.EMPTY));
        assertTrue(recorder.observations().stream()
                .anyMatch(o -> o.target().state().kind() == StateKind.LOADING));
        assertTrue(recorder.observations().stream()
                .anyMatch(o -> o.target().state().kind() == StateKind.SUCCESS));
        assertTrue(recorder.observations().stream()
                .anyMatch(o -> o.evidenceId().startsWith("bridge-states#")));
    }

    private static CaptureEvent.VerificationEvent verification(long sequence, CaptureEvent.VerificationKind kind,
                                                               String expectedName) {
        ExternalTestDataReference expected = expectedName == null ? null : new ExternalTestDataReference(
                "data." + sequence,
                expectedName,
                ExternalTestDataReference.DataSource.JSON,
                "data.json",
                "/x",
                ExternalTestDataReference.DataClassification.ORDINARY);
        return new CaptureEvent.VerificationEvent(
                new EventContext(sequence, CaptureFixtures.STARTED, CaptureFixtures.page(),
                        EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of()),
                kind,
                CaptureFixtures.target(),
                expected,
                false);
    }
}
