package com.shaft.capture.model;

import com.fasterxml.jackson.databind.JsonNode;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * Deterministic live readiness summary for a Capture session.
 *
 * @param state highest observed readiness state
 * @param warnings actionable readiness warnings
 */
public record CaptureReadiness(State state, List<String> warnings) {
    private static final Pattern INDEXED_LOCATOR = Pattern.compile("\\[\\d+]|:nth-(?:child|of-type)\\(");

    /**
     * Recorder readiness states.
     */
    public enum State {
        READY,
        RISKY,
        BLOCKED
    }

    /**
     * Creates an immutable readiness summary.
     */
    public CaptureReadiness {
        state = state == null ? State.READY : state;
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    /**
     * Returns a clean readiness summary.
     *
     * @return ready summary
     */
    public static CaptureReadiness ready() {
        return new CaptureReadiness(State.READY, List.of());
    }

    /**
     * Calculates readiness from persisted session evidence.
     *
     * @param session source Capture session
     * @return readiness summary
     */
    public static CaptureReadiness from(CaptureSession session) {
        return from(session, List.of());
    }

    /**
     * Calculates readiness from persisted session evidence and live collector warnings.
     *
     * @param session source Capture session
     * @param collectorWarnings live recorder warnings
     * @return readiness summary
     */
    public static CaptureReadiness from(CaptureSession session, List<String> collectorWarnings) {
        if (session == null) {
            return ready();
        }
        List<String> blocked = new ArrayList<>();
        List<String> risky = new ArrayList<>();
        Set<Long> assertionSequences = assertionSequences(session);
        for (CaptureEvent event : session.events()) {
            String step = "Step " + event.context().sequence();
            if (event.context().replayStatus() == EventContext.ReplayStatus.UNSUPPORTED) {
                blocked.add(step + " is marked unsupported; re-record a supported action.");
            } else if (event.context().replayStatus() == EventContext.ReplayStatus.FAILED
                    || event.context().replayStatus() == EventContext.ReplayStatus.SKIPPED) {
                risky.add(step + " recorded replay status " + event.context().replayStatus() + ".");
            }
            inspectEventSupport(step, event, blocked);
            target(event).ifPresent(target -> inspectTarget(step, target, blocked, risky));
            eventReferences(event).stream()
                    .filter(CaptureReadiness::requiresUserInput)
                    .map(ExternalTestDataReference::id)
                    .forEach(id -> blocked.add(step + " uses redacted required input " + id
                            + "; set required input before replay."));
            if (needsPostActionAssertion(event)
                    && assertionSequences.stream().noneMatch(sequence -> sequence > event.context().sequence())) {
                risky.add(step + " has no later assertion after navigation or form submission.");
            }
        }
        liveWarnings(session, collectorWarnings).forEach(warning ->
                risky.add("Recorder warning: " + warning));
        List<String> warnings = new ArrayList<>(distinct(blocked));
        warnings.addAll(distinct(risky));
        State readinessState = State.READY;
        if (!blocked.isEmpty()) {
            readinessState = State.BLOCKED;
        } else if (!warnings.isEmpty()) {
            readinessState = State.RISKY;
        }
        return new CaptureReadiness(readinessState, distinct(warnings));
    }

    private static void inspectEventSupport(String step, CaptureEvent event, List<String> blocked) {
        if (event instanceof CaptureEvent.ClickEvent value) {
            if (value.button() != CaptureEvent.MouseButton.PRIMARY) {
                blocked.add(step + " uses unsupported non-primary click.");
            }
            if (value.clickCount() > 2) {
                blocked.add(step + " uses unsupported click count " + value.clickCount() + ".");
            }
        } else if (event instanceof CaptureEvent.SelectEvent value
                && value.mode() == CaptureEvent.SelectMode.INDEX) {
            blocked.add(step + " uses unsupported index-based selection.");
        }
    }

    private static void inspectTarget(
            String step,
            ElementSnapshot target,
            List<String> blocked,
            List<String> risky) {
        if (target.locatorCandidates().isEmpty()) {
            blocked.add(step + " has no locator evidence for " + target.logicalElementId() + ".");
            return;
        }
        LocatorCandidate selected = target.locatorCandidates().getFirst();
        if (selected.uniquenessCount() == 0) {
            blocked.add(step + " locator has no current match for " + target.logicalElementId() + ".");
        } else if (selected.uniquenessCount() > 1) {
            risky.add(step + " locator matches " + selected.uniquenessCount()
                    + " elements for " + target.logicalElementId() + ".");
        }
        if (positional(selected)) {
            risky.add(step + " uses generated positional " + selected.strategy().name()
                    + " locator for " + target.logicalElementId() + ".");
        }
    }

    private static boolean positional(LocatorCandidate candidate) {
        return (candidate.strategy() == LocatorCandidate.LocatorStrategy.CSS
                || candidate.strategy() == LocatorCandidate.LocatorStrategy.XPATH)
                && (candidate.signals().contains(LocatorCandidate.LocatorSignal.POSITIONAL)
                || INDEXED_LOCATOR.matcher(candidate.expression()).find());
    }

    private static Set<Long> assertionSequences(CaptureSession session) {
        Set<Long> sequences = new HashSet<>();
        for (CaptureEvent event : session.events()) {
            if (event instanceof CaptureEvent.VerificationEvent) {
                sequences.add(event.context().sequence());
            }
        }
        session.checkpoints().stream()
                .filter(checkpoint -> checkpoint.kind() == Checkpoint.CheckpointKind.ASSERTION)
                .map(Checkpoint::sequence)
                .forEach(sequences::add);
        return sequences;
    }

    private static List<String> liveWarnings(CaptureSession session, List<String> collectorWarnings) {
        List<String> warnings = new ArrayList<>();
        if (collectorWarnings != null) {
            warnings.addAll(collectorWarnings);
        }
        JsonNode persisted = session.extensions().get("collectorWarnings");
        if (persisted != null && persisted.isArray()) {
            persisted.forEach(item -> warnings.add(item.asText("")));
        }
        return warnings.stream().filter(warning -> !warning.isBlank()).toList();
    }

    private static boolean requiresUserInput(ExternalTestDataReference reference) {
        return reference.source() == ExternalTestDataReference.DataSource.ENVIRONMENT
                || reference.source() == ExternalTestDataReference.DataSource.SECRET_PROVIDER;
    }

    private static boolean needsPostActionAssertion(CaptureEvent event) {
        if (event instanceof CaptureEvent.NavigationEvent) {
            return true;
        }
        return event instanceof CaptureEvent.ClickEvent
                && target(event).map(CaptureReadiness::looksLikeFormSubmission).orElse(false);
    }

    private static boolean looksLikeFormSubmission(ElementSnapshot target) {
        String type = target.normalizedAttributes().getOrDefault("type", "");
        String text = (target.accessibleName() + " " + target.label() + " "
                + target.normalizedAttributes().getOrDefault("value", "")).toLowerCase(Locale.ROOT);
        return "submit".equalsIgnoreCase(type)
                || text.contains("submit")
                || text.contains("pay")
                || text.contains("checkout")
                || text.contains("place order")
                || text.contains("confirm")
                || text.contains("sign in")
                || text.contains("log in")
                || text.contains("continue");
    }

    private static java.util.Optional<ElementSnapshot> target(CaptureEvent event) {
        if (event instanceof CaptureEvent.ClickEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.TypeEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.ClearEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.SelectEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.ToggleEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.UploadEvent value) {
            return java.util.Optional.of(value.target());
        }
        if (event instanceof CaptureEvent.KeyboardEvent value) {
            return java.util.Optional.ofNullable(value.target());
        }
        if (event instanceof CaptureEvent.FrameEvent value) {
            return java.util.Optional.ofNullable(value.target());
        }
        if (event instanceof CaptureEvent.WaitEvent value) {
            return java.util.Optional.ofNullable(value.target());
        }
        if (event instanceof CaptureEvent.VerificationEvent value) {
            return java.util.Optional.ofNullable(value.target());
        }
        return java.util.Optional.empty();
    }

    private static List<ExternalTestDataReference> eventReferences(CaptureEvent event) {
        List<ExternalTestDataReference> references = new ArrayList<>();
        if (event instanceof CaptureEvent.TypeEvent value) {
            references.add(value.value());
        } else if (event instanceof CaptureEvent.SelectEvent value) {
            references.add(value.value());
        } else if (event instanceof CaptureEvent.UploadEvent value) {
            references.add(value.file());
        } else if (event instanceof CaptureEvent.AlertEvent value && value.text() != null) {
            references.add(value.text());
        } else if (event instanceof CaptureEvent.WaitEvent value && value.expected() != null) {
            references.add(value.expected());
        } else if (event instanceof CaptureEvent.VerificationEvent value && value.expected() != null) {
            references.add(value.expected());
        }
        return references;
    }

    private static List<String> distinct(List<String> values) {
        return values.stream().filter(value -> !value.isBlank()).distinct().toList();
    }
}
