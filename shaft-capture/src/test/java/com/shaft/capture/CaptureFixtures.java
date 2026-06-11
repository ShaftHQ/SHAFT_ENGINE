package com.shaft.capture;

import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.model.PageContext;
import com.shaft.capture.model.RedactionSummary;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Set;

public final class CaptureFixtures {
    public static final Instant STARTED = Instant.parse("2026-01-02T03:04:05Z");

    private CaptureFixtures() {
        throw new IllegalStateException("Utility class");
    }

    public static BrowserMetadata browser() {
        return new BrowserMetadata("chrome", "137", "Windows 11", "browser-1",
                Map.of("browserName", "chrome"));
    }

    public static PageContext page() {
        return new PageContext("https://example.test/form", "Example form", "window-1",
                List.of(), 1440, 900);
    }

    public static ElementSnapshot target() {
        return new ElementSnapshot(
                "username-input",
                "input",
                "textbox",
                "Username",
                "Username",
                Map.of("autocomplete", "username", "name", "username"),
                List.of(
                        new LocatorCandidate(LocatorCandidate.LocatorStrategy.CSS,
                                "form input:nth-child(1)", 1, true, false,
                                Set.of(LocatorCandidate.LocatorSignal.GENERATED,
                                        LocatorCandidate.LocatorSignal.POSITIONAL)),
                        new LocatorCandidate(LocatorCandidate.LocatorStrategy.LABEL,
                                "Username", 1, true, true,
                                Set.of(LocatorCandidate.LocatorSignal.ACCESSIBLE,
                                        LocatorCandidate.LocatorSignal.LABEL_ASSOCIATED))),
                true,
                true,
                false);
    }

    public static ExternalTestDataReference ordinary() {
        return new ExternalTestDataReference(
                "data.username",
                "username",
                ExternalTestDataReference.DataSource.JSON,
                "capture-data.json",
                "/values/data.username",
                ExternalTestDataReference.DataClassification.ORDINARY);
    }

    public static ExternalTestDataReference secret() {
        return new ExternalTestDataReference(
                "data.password",
                "password",
                ExternalTestDataReference.DataSource.ENVIRONMENT,
                "",
                "",
                ExternalTestDataReference.DataClassification.SECRET);
    }

    public static EventContext context(long sequence) {
        return new EventContext(sequence, STARTED.plusSeconds(sequence), page(),
                EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of());
    }

    public static List<CaptureEvent> allEvents() {
        ElementSnapshot target = target();
        return List.of(
                new CaptureEvent.NavigationEvent(context(1), CaptureEvent.NavigationAction.OPEN,
                        "https://example.test/form"),
                new CaptureEvent.ClickEvent(context(2), target, CaptureEvent.MouseButton.PRIMARY, 1),
                new CaptureEvent.TypeEvent(context(3), target, ordinary()),
                new CaptureEvent.ClearEvent(context(4), target),
                new CaptureEvent.SelectEvent(context(5), target, CaptureEvent.SelectMode.VISIBLE_TEXT, ordinary()),
                new CaptureEvent.ToggleEvent(context(6), target, true),
                new CaptureEvent.ToggleEvent(context(7), target, false),
                new CaptureEvent.UploadEvent(context(8), target,
                        new ExternalTestDataReference(
                                "upload.avatar",
                                "avatar",
                                ExternalTestDataReference.DataSource.FILE_FIXTURE,
                                "test-data/uploads/avatar.png",
                                "",
                                ExternalTestDataReference.DataClassification.UPLOAD),
                        "avatar.png", "image/png", 128),
                new CaptureEvent.KeyboardEvent(context(9), target, List.of("CONTROL", "A")),
                new CaptureEvent.WindowEvent(context(10), CaptureEvent.WindowAction.OPEN_TAB, "window-2"),
                new CaptureEvent.FrameEvent(context(11), CaptureEvent.FrameAction.ENTER, "payment-frame", target),
                new CaptureEvent.AlertEvent(context(12), CaptureEvent.AlertAction.TYPE, ordinary()),
                new CaptureEvent.WaitEvent(context(13), CaptureEvent.WaitCondition.ELEMENT_VISIBLE,
                        Duration.ofSeconds(10), target, null),
                new CaptureEvent.VerificationEvent(context(14), CaptureEvent.VerificationKind.TEXT_EQUALS,
                        target, ordinary(), false));
    }

    public static CaptureSession representativeSession() {
        return new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "session-1",
                CaptureSession.SessionStatus.INCOMPLETE,
                STARTED,
                null,
                browser(),
                allEvents(),
                List.of(new Checkpoint("checkpoint-1", 14, STARTED.plusSeconds(15),
                        Checkpoint.CheckpointKind.ASSERTION, "Form completed")),
                List.of(
                        ordinary(),
                        secret(),
                        new ExternalTestDataReference(
                                "upload.avatar",
                                "avatar",
                                ExternalTestDataReference.DataSource.FILE_FIXTURE,
                                "test-data/uploads/avatar.png",
                                "",
                                ExternalTestDataReference.DataClassification.UPLOAD)),
                new RedactionSummary(2, 1, 1,
                        Set.of("externalized-test-data", "sensitive-field")),
                Map.of());
    }
}
