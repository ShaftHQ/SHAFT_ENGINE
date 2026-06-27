package com.shaft.capture.model;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.capture.CaptureFixtures;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureReadinessTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test
    void reportsReadyRiskyAndBlockedFromCapturedSignals() {
        CaptureReadiness ready = CaptureReadiness.from(session(List.of(new CaptureEvent.VerificationEvent(
                CaptureFixtures.context(1),
                CaptureEvent.VerificationKind.ELEMENT_VISIBLE,
                stableTarget(),
                null,
                false)), List.of()));

        CaptureReadiness risky = CaptureReadiness.from(session(List.of(new CaptureEvent.ClickEvent(
                CaptureFixtures.context(1),
                positionalTarget(),
                CaptureEvent.MouseButton.PRIMARY,
                1)), List.of("WebDriver BiDi was unavailable; using the compatibility listener.")));

        CaptureReadiness blocked = CaptureReadiness.from(session(List.of(new CaptureEvent.TypeEvent(
                CaptureFixtures.context(1),
                noLocatorTarget(),
                CaptureFixtures.secret())), List.of()));

        assertEquals(CaptureReadiness.State.READY, ready.state());
        assertTrue(ready.warnings().isEmpty());
        assertEquals(CaptureReadiness.State.RISKY, risky.state());
        assertTrue(risky.warnings().stream().anyMatch(warning -> warning.contains("positional CSS")));
        assertTrue(risky.warnings().stream().anyMatch(warning -> warning.contains("compatibility listener")));
        assertEquals(CaptureReadiness.State.BLOCKED, blocked.state());
        assertTrue(blocked.warnings().stream().anyMatch(warning -> warning.contains("no locator evidence")));
        assertTrue(blocked.warnings().stream().anyMatch(warning -> warning.contains("required input")));
    }

    private static CaptureSession session(List<CaptureEvent> events, List<String> warnings) {
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "readiness-session",
                CaptureSession.SessionStatus.INCOMPLETE,
                CaptureFixtures.STARTED,
                null,
                CaptureFixtures.browser(),
                events,
                List.of(),
                List.of(CaptureFixtures.secret()),
                RedactionSummary.empty(),
                Map.of("collectorWarnings", JSON.valueToTree(warnings)));
    }

    private static ElementSnapshot stableTarget() {
        return new ElementSnapshot(
                "submit",
                "button",
                "button",
                "Submit",
                "Submit",
                Map.of("id", "submit"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.ID,
                        "submit", 1, true, true, Set.of(LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE))),
                true,
                true,
                false);
    }

    private static ElementSnapshot positionalTarget() {
        return new ElementSnapshot(
                "submit",
                "button",
                "button",
                "Submit",
                "Submit",
                Map.of("type", "submit"),
                List.of(new LocatorCandidate(LocatorCandidate.LocatorStrategy.CSS,
                        "form > button:nth-of-type(1)", 1, true, false,
                        Set.of(LocatorCandidate.LocatorSignal.GENERATED,
                                LocatorCandidate.LocatorSignal.POSITIONAL))),
                true,
                true,
                false);
    }

    private static ElementSnapshot noLocatorTarget() {
        return new ElementSnapshot(
                "password",
                "input",
                "textbox",
                "Password",
                "Password",
                Map.of("type", "password"),
                List.of(),
                true,
                true,
                false);
    }
}
