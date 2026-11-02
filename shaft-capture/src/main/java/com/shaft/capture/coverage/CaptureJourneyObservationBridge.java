package com.shaft.capture.coverage;

import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.PageContext;
import com.shaft.coverage.journey.CoverageTarget;
import com.shaft.coverage.journey.ExecutionObservation;
import com.shaft.coverage.journey.InteractionId;
import com.shaft.coverage.journey.JourneyId;
import com.shaft.coverage.journey.ObservationRecorder;
import com.shaft.coverage.journey.StateId;
import com.shaft.coverage.journey.StateKind;
import com.shaft.coverage.journey.ViewId;

import java.net.URI;
import java.util.Locale;

/**
 * Maps SHAFT Capture session evidence into journey coverage observations (issue #5455).
 *
 * <p>Coverage is derived only from recorded capture events; this bridge never invents targets
 * that were not present in the session.
 */
public final class CaptureJourneyObservationBridge {
    private CaptureJourneyObservationBridge() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Records one observation per capture event against the supplied recorder.
     *
     * @param session capture session evidence
     * @param journeyLogicalName human journey name (falls back to session id)
     * @param recorder observation sink
     */
    public static void recordSession(CaptureSession session, String journeyLogicalName, ObservationRecorder recorder) {
        if (session == null) {
            throw new IllegalArgumentException("session must not be null");
        }
        if (recorder == null) {
            throw new IllegalArgumentException("recorder must not be null");
        }
        String journeyRaw = (journeyLogicalName == null || journeyLogicalName.isBlank())
                ? session.sessionId()
                : journeyLogicalName;
        JourneyId journey = JourneyId.of(journeyRaw);

        for (CaptureEvent event : session.events()) {
            EventContext context = event.context();
            ViewId view = ViewId.of(viewToken(context.page()));
            InteractionId interaction = InteractionId.of(interactionToken(event));
            StateId state = stateFor(event);
            CoverageTarget target = new CoverageTarget(journey, view, interaction, state);
            String evidenceId = session.sessionId() + "#" + context.sequence();
            recorder.record(new ExecutionObservation(target, evidenceId, context.timestamp()));
        }
    }

    /**
     * @param session capture session
     * @param journeyLogicalName journey name
     * @return recorder seeded from the session
     */
    public static ObservationRecorder observationsFrom(CaptureSession session, String journeyLogicalName) {
        ObservationRecorder recorder = new ObservationRecorder();
        recordSession(session, journeyLogicalName, recorder);
        return recorder;
    }

    private static String viewToken(PageContext page) {
        String url = page.url();
        if (url == null || url.isBlank()) {
            return page.title() == null || page.title().isBlank() ? "view" : page.title();
        }
        try {
            URI uri = URI.create(url);
            String path = uri.getPath();
            if (path == null || path.isBlank()) {
                return "/";
            }
            return path;
        } catch (IllegalArgumentException ignored) {
            return url;
        }
    }

    private static String interactionToken(CaptureEvent event) {
        String type = event.getClass().getSimpleName();
        if (type.endsWith("Event")) {
            type = type.substring(0, type.length() - "Event".length());
        }
        return type.toLowerCase(Locale.ROOT);
    }

    private static StateId stateFor(CaptureEvent event) {
        EventContext.ReplayStatus status = event.context().replayStatus();
        if (status == EventContext.ReplayStatus.FAILED) {
            return StateId.of(StateKind.ERROR);
        }
        if (status == EventContext.ReplayStatus.UNSUPPORTED) {
            return StateId.of(StateKind.UNKNOWN);
        }
        if (event instanceof CaptureEvent.WaitEvent) {
            return StateId.of(StateKind.LOADING);
        }
        if (event instanceof CaptureEvent.VerificationEvent verification) {
            return stateFromVerification(verification);
        }
        return StateId.of(StateKind.SUCCESS);
    }

    private static StateId stateFromVerification(CaptureEvent.VerificationEvent verification) {
        String hint = verificationHint(verification);
        if (hint.contains("empty")) {
            return StateId.of(StateKind.EMPTY, hint);
        }
        if (hint.contains("error") || hint.contains("fail")) {
            return StateId.of(StateKind.ERROR, hint);
        }
        if (hint.contains("load")) {
            return StateId.of(StateKind.LOADING, hint);
        }
        return StateId.of(StateKind.SUCCESS, hint.isBlank() ? "success" : hint);
    }

    private static String verificationHint(CaptureEvent.VerificationEvent verification) {
        StringBuilder hint = new StringBuilder();
        if (verification.verification() != null) {
            hint.append(verification.verification().name().toLowerCase(Locale.ROOT).replace('_', ' '));
        }
        ExternalTestDataReference expected = verification.expected();
        if (expected != null) {
            if (!hint.isEmpty()) {
                hint.append(' ');
            }
            hint.append(expected.logicalName());
        }
        return hint.toString().toLowerCase(Locale.ROOT);
    }
}
