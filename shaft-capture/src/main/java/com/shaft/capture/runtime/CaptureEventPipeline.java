package com.shaft.capture.runtime;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.StringNode;
import com.shaft.capture.collector.BrowserSignal;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.model.PageContext;
import com.shaft.capture.model.RedactionSummary;
import com.shaft.capture.privacy.CapturePrivacyClassifier;
import com.shaft.capture.privacy.CapturePrivacyPolicy;
import com.shaft.capture.privacy.ClassifiedValue;
import com.shaft.capture.storage.CaptureSessionStore;
import com.shaft.capture.storage.ExternalTestDataWriter;

import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

/**
 * Converts low-level browser signals into ordered privacy-safe semantic events.
 */
final class CaptureEventPipeline implements AutoCloseable {
    private static final Duration CLICK_DEBOUNCE = Duration.ofMillis(300);
    // Same-URL navigations within this window are one navigation delivered twice (racing
    // channels, or BiDi's commit racing the recorder's own explicit OPEN across a slow load).
    private static final Duration NAVIGATION_DEBOUNCE = Duration.ofSeconds(10);
    // A navigation observed this soon after a recorded user interaction is a consequence of that
    // interaction (link click, form submit, server redirect chain), not a navigation the user
    // performed: recording it adds a phantom navigateToURL step to the generated test. Only
    // spontaneous navigations (address bar, the initial open) become steps.
    private static final Duration INTERACTION_NAVIGATION_WINDOW = Duration.ofSeconds(10);
    // An overlay-reported user navigation whose URL was appended this recently describes the
    // navigation a collector already recorded (channels race within milliseconds), so it only
    // contributes its row identity. Anything older is a distinct user navigation — e.g. pressing
    // Back to a page whose original open was recorded seconds earlier.
    private static final Duration OVERLAY_NAVIGATION_ATTACH_WINDOW = Duration.ofSeconds(2);

    private final CaptureSessionStore store;
    private final CapturePrivacyClassifier privacy;
    private final CapturePrivacyPolicy policy;
    private final ExternalTestDataWriter dataWriter = new ExternalTestDataWriter();
    private final Path dataPath;
    private final Consumer<String> currentUrlConsumer;
    private final Consumer<String> warningConsumer;
    private final List<ClassifiedValue> classifiedValues = new ArrayList<>();
    private final Map<String, BrowserSignal> pendingInputs = new LinkedHashMap<>();
    /** Last flushed input value per target, to drop the duplicate change-on-submit re-emission. */
    private final Map<String, String> lastEmittedInputValues = new LinkedHashMap<>();
    private final Map<String, BrowserSignal> pendingClicks = new LinkedHashMap<>();
    // Collector-synthesized navigations (BiDi navigationCommitted / URL polling, blank
    // navigationSource) race the click that caused them: the native signal often reaches accept()
    // before the click's own slower JS-channel signal, so no interaction would be recorded yet
    // when interactionConsequence is evaluated, misclassifying a click-caused navigation as
    // spontaneous (phantom step). Buffering them one debounce window lets the click's signal —
    // recorded into recentInteractionTimestamps as soon as accept() sees it, regardless of
    // buffering — win the race. Keyed by an incrementing counter, not targetKey(signal): several
    // redirect hops within the window must each get their own consequence/duplicate evaluation,
    // not coalesce.
    private final Map<String, BrowserSignal> pendingNavigations = new LinkedHashMap<>();
    private long pendingNavigationSequence;
    private final Map<String, String> logicalWindows = new LinkedHashMap<>();
    private final Map<String, LocatorPreference> locatorPreferences = new LinkedHashMap<>();
    private final Map<String, Instant> recentSignals = new LinkedHashMap<>();
    // Every recorder payload is delivered through up to three racing channels (in-page queue
    // drain, loopback sink POST, BiDi script channel). The fingerprint dedup below catches
    // byte-identical duplicates, but clientActionId is the authoritative one-user-action identity:
    // it survives timestamp drift and parse-representation differences between channels.
    private final Set<String> emittedClientActionIds = new LinkedHashSet<>();
    // A step the user revoked (dblclick coalescing, the overlay delete button) must stay deleted
    // even when the revocation overtakes the original event across channels.
    private final Set<String> deletedClientActionIds = new LinkedHashSet<>();
    // Bounded history (not a single scalar) of recent user-interaction timestamps: a scalar
    // "lastInteractionAt" gets overwritten by a *later, unrelated* interaction that occurs while an
    // earlier interaction's caused navigation still sits buffered in pendingNavigations, wrongly
    // making that navigation appear to predate its own (real, earlier) cause once evaluated
    // against the newer interaction's timestamp instead. Regression: an ENTER-triggered search
    // whose results page performs a second location.replace() navigation, followed shortly after
    // by an unrelated click, misclassified both buffered navigations as spontaneous once the click
    // advanced the old scalar past them. Pruned to the interaction window's worth of history.
    private final List<Instant> recentInteractionTimestamps = new ArrayList<>();
    private final ScheduledExecutorService debounceExecutor;
    private long sequence;
    private String lastNavigationUrl = "";
    private Instant lastNavigationAt = Instant.EPOCH;
    private String lastAppendedNavigationUrl = "";
    private Instant lastAppendedNavigationAt = Instant.EPOCH;
    private String lastWindow = "";
    private List<String> lastFramePath = List.of();
    private boolean closed;

    CaptureEventPipeline(
            CaptureSessionStore store,
            Path outputPath,
            CapturePrivacyPolicy policy,
            Consumer<String> currentUrlConsumer,
            Consumer<String> warningConsumer) {
        if (store == null || outputPath == null || currentUrlConsumer == null || warningConsumer == null) {
            throw new IllegalArgumentException("Capture event pipeline dependencies are required.");
        }
        this.store = store;
        this.policy = policy == null ? CapturePrivacyPolicy.defaults() : policy;
        privacy = new CapturePrivacyClassifier(this.policy);
        dataPath = outputPath.toAbsolutePath().normalize().getParent().resolve(this.policy.externalDataPath());
        this.currentUrlConsumer = currentUrlConsumer;
        this.warningConsumer = warningConsumer;
        debounceExecutor = Executors.newSingleThreadScheduledExecutor(runnable -> {
            Thread thread = new Thread(runnable, "shaft-capture-debounce");
            thread.setDaemon(true);
            return thread;
        });
        debounceExecutor.scheduleWithFixedDelay(this::flushReady, 100, 100, TimeUnit.MILLISECONDS);
    }

    synchronized void accept(BrowserSignal signal) {
        if (closed || signal == null) {
            return;
        }
        if (isDuplicate(signal)) {
            return;
        }
        if (isUserInteraction(signal.kind())) {
            recordInteraction(signal.timestamp());
        }
        try {
            switch (signal.kind()) {
                case "input" -> {
                    pendingInputs.put(targetKey(signal), signal);
                    if (signal.dataBoolean("committed")) {
                        // This commit flushes immediately, bypassing flushPendingBefore; an
                        // older buffered navigation must still be appended first so persisted
                        // step order matches browser occurrence order.
                        List<BrowserSignal> readyNavigations = new ArrayList<>();
                        collectReady(pendingNavigations, signal.timestamp(), readyNavigations);
                        flushOrdered(readyNavigations);
                        flushSignal(pendingInputs.remove(targetKey(signal)));
                    }
                }
                case "click" -> pendingClicks.put(targetKey(signal), signal);
                case "navigation" -> {
                    if (signal.dataString("navigationSource").isBlank()) {
                        pendingNavigations.put(
                                "navigation-" + (++pendingNavigationSequence), signal);
                    } else {
                        // history/user_annotation/user_traversal/user_reported are authoritative
                        // (user_traversal in particular must never be delayed or swallowed): keep
                        // their existing immediate handling.
                        flushPendingBefore(signal.timestamp());
                        emit(signal);
                    }
                }
                case "keyboard" -> {
                    // Suppress standalone editing key actions (Backspace, Delete, arrow keys)
                    // when recorded on a text-entry element without modifiers.
                    // These are coalesced into the input value, not emitted as separate keyboard events.
                    if (isStandaloneEditingKey(signal)) {
                        return;
                    }
                    flushPendingBefore(signal.timestamp());
                    emit(signal);
                }
                default -> {
                    flushPendingBefore(signal.timestamp());
                    emit(signal);
                }
            }
        } catch (RuntimeException exception) {
            warningConsumer.accept("A browser interaction could not be normalized and was skipped.");
        }
    }

    synchronized void checkpoint(String description, Checkpoint.CheckpointKind kind) {
        ensureOpen();
        flushAll();
        CapturePrivacyClassifier.SanitizedText safe = privacy.sanitizeText(description);
        store.checkpoint(new Checkpoint(
                "checkpoint-" + (store.read().checkpoints().size() + 1),
                sequence,
                Instant.now(),
                kind,
                safe.value()), safe.summary());
    }

    synchronized int eventCount() {
        return store.read().events().size();
    }

    /**
     * Returns the number of debounced signals (uncommitted typed input, pending clicks, buffered
     * blank-source navigations) that have not yet been persisted as semantic events. During that
     * window {@link #eventCount()} under-reports; status consumers surface this so live monitors
     * do not misread a quiet count.
     */
    synchronized int pendingSignalCount() {
        return pendingInputs.size() + pendingClicks.size() + pendingNavigations.size();
    }

    @Override
    public synchronized void close() {
        if (closed) {
            return;
        }
        flushAll();
        closed = true;
        debounceExecutor.shutdownNow();
    }

    private synchronized void flushReady() {
        if (closed) {
            return;
        }
        Instant now = Instant.now();
        List<BrowserSignal> ready = new ArrayList<>();
        collectReady(pendingClicks, now.minus(CLICK_DEBOUNCE), ready);
        collectReady(pendingNavigations, now.minus(CLICK_DEBOUNCE), ready);
        flushOrdered(ready);
    }

    private void flushPendingBefore(Instant timestamp) {
        List<BrowserSignal> ready = new ArrayList<>();
        collectReady(pendingInputs, timestamp, ready);
        collectReady(pendingClicks, timestamp, ready);
        collectReady(pendingNavigations, timestamp, ready);
        flushOrdered(ready);
    }

    private void collectReady(
            Map<String, BrowserSignal> pending,
            Instant timestamp,
            List<BrowserSignal> destination) {
        List<String> ready = pending.entrySet().stream()
                .filter(entry -> !entry.getValue().timestamp().isAfter(timestamp))
                .map(Map.Entry::getKey)
                .toList();
        ready.forEach(key -> destination.add(pending.remove(key)));
    }

    private void flushAll() {
        List<BrowserSignal> pending = new ArrayList<>(pendingInputs.values());
        pending.addAll(pendingClicks.values());
        pending.addAll(pendingNavigations.values());
        pendingInputs.clear();
        pendingClicks.clear();
        pendingNavigations.clear();
        flushOrdered(pending);
    }

    private void flushOrdered(List<BrowserSignal> signals) {
        signals.stream()
                .sorted(java.util.Comparator.comparing(BrowserSignal::timestamp))
                .forEach(this::flushSignal);
    }

    private void flushSignal(BrowserSignal signal) {
        if (signal == null) {
            return;
        }
        try {
            emit(signal);
        } catch (RuntimeException exception) {
            // Debounced signals flush on the scheduled debounce thread, where an uncaught
            // exception would cancel the flush loop forever; one bad signal must only skip itself.
            warningConsumer.accept("A browser interaction could not be normalized and was skipped.");
        }
    }

    private void emit(BrowserSignal signal) {
        if (suppressDuplicateOrDeletedStep(signal)) {
            return;
        }
        switch (signal.kind()) {
            case "navigation" -> emitNavigation(signal);
            case "click" -> emitClick(signal);
            case "input" -> emitInput(signal);
            case "select" -> emitSelect(signal);
            case "toggle" -> append(new CaptureEvent.ToggleEvent(
                    context(signal), target(signal).snapshot(), signal.dataBoolean("checked")),
                    target(signal).summary(), List.of());
            case "upload" -> emitUpload(signal);
            case "keyboard" -> append(new CaptureEvent.KeyboardEvent(
                    context(signal), target(signal).snapshot(), signal.dataStrings("keys")),
                    target(signal).summary(), List.of());
            case "verification" -> emitVerification(signal);
            case "locator_preference" -> rememberLocatorPreference(signal);
            case "step_update" -> updateStep(signal);
            case "step_delete" -> deleteStep(signal);
            case "step_reorder" -> reorderStep(signal);
            case "window_open" -> append(new CaptureEvent.WindowEvent(
                    context(signal), CaptureEvent.WindowAction.OPEN_TAB, logicalWindow(signal.browsingContextId())),
                    RedactionSummary.empty(), List.of());
            case "window_close" -> append(new CaptureEvent.WindowEvent(
                    context(signal), CaptureEvent.WindowAction.CLOSE, logicalWindow(signal.browsingContextId())),
                    RedactionSummary.empty(), List.of());
            case "alert" -> emitAlert(signal);
            default -> {
                // Unknown browser noise is intentionally ignored.
            }
        }
    }

    private void emitNavigation(BrowserSignal signal) {
        SafePage page = page(signal);
        String url = page.context().url();
        String source = signal.dataString("navigationSource");
        boolean duplicateDelivery = url.equals(lastNavigationUrl)
                && Duration.between(lastNavigationAt, signal.timestamp()).abs()
                .compareTo(NAVIGATION_DEBOUNCE) < 0;
        // Forward-only, searched against history rather than a single scalar: a buffered
        // navigation is a consequence only of an interaction that happened at or before it, within
        // the window. Using the closest preceding interaction (not whatever is currently most
        // recent) keeps this correct even when a later, unrelated interaction has since occurred
        // while this navigation sat in pendingNavigations.
        Instant causingInteraction = mostRecentInteractionAtOrBefore(signal.timestamp());
        boolean interactionConsequence = causingInteraction != null
                && Duration.between(causingInteraction, signal.timestamp()).compareTo(INTERACTION_NAVIGATION_WINDOW) < 0;
        lastNavigationUrl = url;
        lastNavigationAt = signal.timestamp();
        currentUrlConsumer.accept(url);
        switch (source) {
            // pushState/replaceState URL rewrites (SPA route changes, results-page
            // canonicalization like DuckDuckGo appending query flags after a search) are never
            // a user navigation.
            case "history" -> {
            }
            // The overlay's "Open" breadcrumb only contributes its row identity to the
            // already-recorded initial OPEN so the breadcrumb survives server step syncs; it
            // must never append a second open event.
            case "user_annotation" -> attachOverlayIdentity(signal, url);
            // Overlay-observed user navigations (URL poll, back/forward traversal) are
            // authoritative: back/forward in particular happens right after interactions and
            // must not be swallowed by the interaction window. If a collector already appended
            // this navigation, the signal only contributes its row identity.
            case "user_traversal", "user_reported" -> {
                boolean recentlyAppended = url.equals(lastAppendedNavigationUrl)
                        && Duration.between(lastAppendedNavigationAt, signal.timestamp()).abs()
                        .compareTo(OVERLAY_NAVIGATION_ATTACH_WINDOW) < 0;
                if (recentlyAppended) {
                    attachOverlayIdentity(signal, url);
                } else {
                    appendNavigation(signal, page, url);
                }
            }
            default -> {
                if (!duplicateDelivery && !interactionConsequence) {
                    appendNavigation(signal, page, url);
                }
            }
        }
    }

    private void appendNavigation(BrowserSignal signal, SafePage page, String url) {
        CaptureEvent.NavigationAction action = enumValue(
                CaptureEvent.NavigationAction.class,
                signal.dataString("action"),
                CaptureEvent.NavigationAction.OPEN);
        append(new CaptureEvent.NavigationEvent(context(signal, page), action, url),
                page.summary(), List.of());
        lastAppendedNavigationUrl = url;
        lastAppendedNavigationAt = signal.timestamp();
    }

    /**
     * Gives the most recent identity-less recorded navigation to {@code url} the overlay row's
     * client action id and description, so the row becomes a server-backed step: it survives
     * step syncs across navigations and can be edited or deleted like any other step.
     */
    private void attachOverlayIdentity(BrowserSignal signal, String url) {
        String clientActionId = privacy.sanitizeText(signal.dataString("clientActionId")).value();
        if (clientActionId.isBlank()) {
            return;
        }
        String stepDescription = privacy.sanitizeText(signal.dataString("stepDescription")).value();
        store.updateEvents(events -> {
            for (int index = events.size() - 1; index >= 0; index--) {
                if (!(events.get(index) instanceof CaptureEvent.NavigationEvent navigation)
                        || !navigation.targetUrl().equals(url)
                        || !extensionText(navigation.context(), "clientActionId").isBlank()) {
                    continue;
                }
                EventContext context = withExtension(navigation.context(), "clientActionId", clientActionId);
                if (!stepDescription.isBlank()) {
                    context = withExtension(context, "stepDescription", stepDescription);
                }
                List<CaptureEvent> updated = new ArrayList<>(events);
                updated.set(index, withContext(navigation, context));
                return updated;
            }
            return events;
        });
    }

    private static boolean isUserInteraction(String kind) {
        return switch (kind) {
            case "click", "input", "select", "toggle", "upload", "keyboard" -> true;
            default -> false;
        };
    }

    /**
     * Records an interaction timestamp and prunes entries older than the interaction window
     * relative to it, keeping {@link #recentInteractionTimestamps} bounded.
     */
    private void recordInteraction(Instant timestamp) {
        recentInteractionTimestamps.add(timestamp);
        Instant cutoff = timestamp.minus(INTERACTION_NAVIGATION_WINDOW);
        recentInteractionTimestamps.removeIf(candidate -> candidate.isBefore(cutoff));
    }

    /**
     * Finds the most recent recorded interaction at or before {@code navigationTimestamp} -- the
     * interaction that could plausibly have caused a navigation observed at that time. Searching
     * history instead of comparing against whatever interaction is currently most recent is what
     * keeps a navigation's classification stable even after a later, unrelated interaction occurs
     * while that navigation still sits buffered in {@link #pendingNavigations}.
     *
     * @param navigationTimestamp the navigation's own timestamp
     * @return the closest preceding interaction timestamp, or {@code null} if none qualifies
     */
    private Instant mostRecentInteractionAtOrBefore(Instant navigationTimestamp) {
        Instant candidate = null;
        for (Instant interaction : recentInteractionTimestamps) {
            if (!interaction.isAfter(navigationTimestamp) && (candidate == null || interaction.isAfter(candidate))) {
                candidate = interaction;
            }
        }
        return candidate;
    }

    /**
     * Suppresses a one-shot interaction step whose {@code clientActionId} was already emitted (a
     * duplicate delivery from another channel) or already deleted (a revocation that overtook the
     * event). Input signals are excluded: keystroke merging deliberately reuses one
     * clientActionId across many signals, and only the last flushed value becomes an event —
     * deletion tombstones for inputs are enforced in {@link #emitInput} instead.
     */
    private boolean suppressDuplicateOrDeletedStep(BrowserSignal signal) {
        boolean oneShotStep = switch (signal.kind()) {
            // Navigations qualify only when overlay-identified (a blank id falls through
            // below): collector navigations have no client action id and keep their own
            // URL-based debounce.
            case "click", "select", "toggle", "upload", "keyboard", "verification", "navigation" -> true;
            default -> false;
        };
        if (!oneShotStep) {
            return false;
        }
        String clientActionId = privacy.sanitizeText(signal.dataString("clientActionId")).value();
        if (clientActionId.isBlank()) {
            return false;
        }
        if (deletedClientActionIds.contains(clientActionId)) {
            return true;
        }
        return !emittedClientActionIds.add(clientActionId);
    }

    /**
     * Emits a user click. Clicks whose target has no rendered box are browser-synthesized
     * (pressing Enter in a form "clicks" its invisible default submit button) — a real user can
     * never click an invisible element, so recording one adds a phantom step (issue #3426 B2).
     * This mirrors the in-page recorder's own suppression and also covers every other delivery
     * channel that feeds this pipeline.
     */
    private void emitClick(BrowserSignal signal) {
        SafeTarget target = target(signal);
        if (!target.snapshot().visible()) {
            return;
        }
        append(new CaptureEvent.ClickEvent(
                        context(signal),
                        target.snapshot(),
                        mouseButton(signal.dataInt("button", 0)),
                        Math.max(1, signal.dataInt("clickCount", 1))),
                target.summary(), List.of());
    }

    private void emitInput(BrowserSignal signal) {
        String clientActionId = privacy.sanitizeText(signal.dataString("clientActionId")).value();
        if (!clientActionId.isBlank() && deletedClientActionIds.contains(clientActionId)) {
            return;
        }
        SafeTarget target = target(signal);
        String value = signal.dataString("value");
        // Form submission re-fires "change" with the value that was already flushed for this
        // element: appending it again duplicated the type step (issue #3426 B2).
        String lastEmitted = lastEmittedInputValues.get(targetKey(signal));
        if (!value.isEmpty() && value.equals(lastEmitted)) {
            return;
        }
        lastEmittedInputValues.put(targetKey(signal), value);
        if (value.isEmpty()) {
            append(new CaptureEvent.ClearEvent(context(signal), target.snapshot()),
                    target.summary(), List.of());
            return;
        }
        ClassifiedValue classified = privacy.classifyValue(
                logicalValueName(target.snapshot(), sequence + 1),
                value,
                bestSelector(target.snapshot()),
                target.snapshot().normalizedAttributes());
        classifiedValues.add(classified);
        dataWriter.write(dataPath, classifiedValues);
        append(new CaptureEvent.TypeEvent(context(signal), target.snapshot(), classified.reference()),
                target.summary().merge(classified.summary()), List.of(classified.reference()));
    }

    private void emitSelect(BrowserSignal signal) {
        SafeTarget target = target(signal);
        String selected = signal.dataString("visibleText");
        if (selected.isBlank()) {
            selected = signal.dataString("value");
        }
        ClassifiedValue classified = privacy.classifyValue(
                logicalValueName(target.snapshot(), sequence + 1),
                selected,
                bestSelector(target.snapshot()),
                target.snapshot().normalizedAttributes());
        classifiedValues.add(classified);
        dataWriter.write(dataPath, classifiedValues);
        append(new CaptureEvent.SelectEvent(
                context(signal),
                target.snapshot(),
                CaptureEvent.SelectMode.VISIBLE_TEXT,
                classified.reference()),
                target.summary().merge(classified.summary()), List.of(classified.reference()));
    }

    private void emitUpload(BrowserSignal signal) {
        SafeTarget target = target(signal);
        var upload = privacy.classifyUpload(
                logicalValueName(target.snapshot(), sequence + 1),
                signal.dataString("fileName"),
                signal.dataString("mediaType"),
                Math.max(0, signal.dataInt("sizeBytes", 0)));
        append(new CaptureEvent.UploadEvent(
                context(signal),
                target.snapshot(),
                upload.reference(),
                upload.safeFileName(),
                upload.mediaType(),
                upload.sizeBytes()),
                target.summary().merge(upload.summary()), List.of(upload.reference()));
    }

    private void emitAlert(BrowserSignal signal) {
        ExternalTestDataReference textReference = null;
        RedactionSummary summary = RedactionSummary.empty();
        String text = signal.dataString("text");
        if (!text.isBlank()) {
            ClassifiedValue classified = privacy.classifyValue(
                    "alert-" + (sequence + 1),
                    text,
                    "",
                    Map.of());
            classifiedValues.add(classified);
            dataWriter.write(dataPath, classifiedValues);
            textReference = classified.reference();
            summary = classified.summary();
            append(new CaptureEvent.AlertEvent(
                    context(signal), CaptureEvent.AlertAction.TYPE, textReference),
                    summary, List.of(textReference));
        }
        append(new CaptureEvent.AlertEvent(
                context(signal),
                signal.dataBoolean("accepted")
                        ? CaptureEvent.AlertAction.ACCEPT
                        : CaptureEvent.AlertAction.DISMISS,
                null),
                RedactionSummary.empty(), List.of());
    }

    private void emitVerification(BrowserSignal signal) {
        CaptureEvent.VerificationKind kind = enumValue(
                CaptureEvent.VerificationKind.class,
                signal.dataString("verification"),
                null);
        if (kind == null) {
            warningConsumer.accept("An unsupported browser assertion was ignored.");
            return;
        }
        boolean targetRequired = switch (kind) {
            case URL_EQUALS, URL_CONTAINS, TITLE_EQUALS, TITLE_CONTAINS, PAGE_TEXT_CONTAINS -> false;
            default -> true;
        };
        boolean expectedRequired = switch (kind) {
            case TEXT_EQUALS, TEXT_CONTAINS, ATTRIBUTE_EQUALS, URL_EQUALS, URL_CONTAINS,
                 TITLE_EQUALS, TITLE_CONTAINS, PAGE_TEXT_CONTAINS, ARIA_SNAPSHOT_MATCHES -> true;
            default -> false;
        };
        SafeTarget safeTarget = targetRequired ? target(signal) : null;
        RedactionSummary summary = safeTarget == null ? RedactionSummary.empty() : safeTarget.summary();
        List<ExternalTestDataReference> references = List.of();
        ExternalTestDataReference expected = null;
        if (expectedRequired) {
            ClassifiedValue classified = privacy.classifyValue(
                    expectedValueName(kind, safeTarget, sequence + 1),
                    signal.dataString("expected"),
                    safeTarget == null ? "" : bestSelector(safeTarget.snapshot()),
                    safeTarget == null ? Map.of() : safeTarget.snapshot().normalizedAttributes());
            classifiedValues.add(classified);
            dataWriter.write(dataPath, classifiedValues);
            expected = classified.reference();
            summary = summary.merge(classified.summary());
            references = List.of(expected);
        }
        append(new CaptureEvent.VerificationEvent(
                context(signal, page(signal), verificationExtensions(signal)),
                kind,
                safeTarget == null ? null : safeTarget.snapshot(),
                expected,
                signal.dataBoolean("negated")),
                summary,
                references);
    }

    private EventContext context(BrowserSignal signal) {
        return context(signal, page(signal));
    }

    private EventContext context(BrowserSignal signal, SafePage page) {
        return context(signal, page, Map.of());
    }

    private EventContext context(BrowserSignal signal, SafePage page, Map<String, JsonNode> extensions) {
        emitContextTransitions(signal, page);
        return new EventContext(
                ++sequence,
                signal.timestamp(),
                page.context(),
                EventContext.ReplayStatus.NOT_REPLAYED,
                List.of(),
                contextExtensions(signal, page, extensions));
    }

    private void emitContextTransitions(BrowserSignal signal, SafePage page) {
        String window = page.context().logicalWindowId();
        if (lastWindow.isBlank()) {
            lastWindow = window;
        } else if (!lastWindow.equals(window)
                && !signal.kind().startsWith("window_")) {
            append(new CaptureEvent.WindowEvent(
                    bareContext(signal, page),
                    CaptureEvent.WindowAction.SWITCH,
                    window),
                    page.summary(), List.of());
            lastWindow = window;
        }

        List<String> frames = page.context().framePath();
        if (!frames.equals(lastFramePath)) {
            if (frames.isEmpty()) {
                append(new CaptureEvent.FrameEvent(
                        bareContext(signal, page),
                        CaptureEvent.FrameAction.TOP,
                        "",
                        null),
                        page.summary(), List.of());
            } else {
                append(new CaptureEvent.FrameEvent(
                        bareContext(signal, page),
                        CaptureEvent.FrameAction.ENTER,
                        frames.getLast(),
                        null),
                        page.summary(), List.of());
            }
            lastFramePath = frames;
        }
    }

    private EventContext bareContext(BrowserSignal signal, SafePage page) {
        return new EventContext(
                ++sequence,
                signal.timestamp(),
                page.context(),
                EventContext.ReplayStatus.NOT_REPLAYED,
                List.of(),
                Map.of());
    }

    private SafePage page(BrowserSignal signal) {
        var safeUrl = privacy.sanitizeUrl(string(signal.page().get("url")));
        var safeTitle = privacy.sanitizeText(string(signal.page().get("title")));
        var safeDom = privacy.sanitizeText(string(signal.page().get("domSnapshot")));
        RedactionSummary summary = safeUrl.summary().merge(safeTitle.summary()).merge(safeDom.summary());
        List<String> frames = list(signal.page().get("framePath")).stream()
                .map(privacy::sanitizeText)
                .map(safe -> safe.value().isBlank() ? "frame" : safe.value())
                .toList();
        PageContext page = new PageContext(
                safeUrl.value(),
                safeTitle.value(),
                logicalWindow(signal.browsingContextId()),
                frames,
                integer(signal.page().get("width"), 0),
                integer(signal.page().get("height"), 0));
        return new SafePage(page, summary, safeDom.value());
    }

    private SafeTarget target(BrowserSignal signal) {
        Map<String, String> rawAttributes = strings(signal.target().get("attributes"));
        var attributes = privacy.sanitizeAttributes(rawAttributes);
        var accessibleName = privacy.sanitizeText(string(signal.target().get("accessibleName")));
        var label = privacy.sanitizeText(string(signal.target().get("label")));
        var logicalId = privacy.sanitizeText(string(signal.target().get("logicalElementId")));
        RedactionSummary summary = attributes.summary()
                .merge(accessibleName.summary())
                .merge(label.summary())
                .merge(logicalId.summary());
        List<LocatorCandidate> locators = new ArrayList<>();
        Object rawLocators = signal.target().get("locators");
        if (rawLocators instanceof List<?> items) {
            for (Object item : items) {
                if (!(item instanceof Map<?, ?> raw)) {
                    continue;
                }
                var expression = privacy.sanitizeText(string(raw.get("expression")));
                summary = summary.merge(expression.summary());
                if (expression.value().isBlank()) {
                    continue;
                }
                locators.add(new LocatorCandidate(
                        enumValue(
                                LocatorCandidate.LocatorStrategy.class,
                                string(raw.get("strategy")),
                                LocatorCandidate.LocatorStrategy.CSS),
                        expression.value(),
                        integer(raw.get("uniquenessCount"), 0),
                        bool(raw.get("visible")),
                        bool(raw.get("stable")),
                        locatorSignals(raw.get("signals"))));
            }
        }
        String targetId = logicalId.value().isBlank()
                ? "element-" + (sequence + 1)
                : logicalId.value();
        locators = applyLocatorPreference(targetId, locators);
        ElementSnapshot snapshot = new ElementSnapshot(
                targetId,
                string(signal.target().get("tagName")),
                string(signal.target().get("role")),
                accessibleName.value(),
                label.value(),
                attributes.attributes(),
                locators,
                bool(signal.target().get("visible")),
                bool(signal.target().get("enabled")),
                bool(signal.target().get("selected")));
        return new SafeTarget(snapshot, summary);
    }

    private void rememberLocatorPreference(BrowserSignal signal) {
        String logicalElementId = privacy.sanitizeText(signal.dataString("logicalElementId")).value();
        String expression = privacy.sanitizeText(signal.dataString("expression")).value();
        LocatorCandidate.LocatorStrategy strategy = enumValue(
                LocatorCandidate.LocatorStrategy.class,
                signal.dataString("strategy"),
                null);
        if (logicalElementId.isBlank() || expression.isBlank() || strategy == null) {
            warningConsumer.accept("A browser locator preference was ignored because it was incomplete.");
            return;
        }
        locatorPreferences.put(logicalElementId, new LocatorPreference(strategy, expression));
    }

    private void updateStep(BrowserSignal signal) {
        String clientActionId = privacy.sanitizeText(signal.dataString("clientActionId")).value();
        String description = privacy.sanitizeText(signal.dataString("description")).value();
        if (clientActionId.isBlank() || description.isBlank()) {
            warningConsumer.accept("A browser step edit was ignored because it was incomplete.");
            return;
        }
        store.updateEvents(events -> events.stream()
                .map(event -> clientActionId.equals(extensionText(event.context(), "clientActionId"))
                        ? withContext(event, withExtension(event.context(), "userDescription", description))
                        : event)
                .toList());
    }

    private void deleteStep(BrowserSignal signal) {
        String clientActionId = privacy.sanitizeText(signal.dataString("clientActionId")).value();
        if (clientActionId.isBlank()) {
            warningConsumer.accept("A browser step deletion was ignored because it was incomplete.");
            return;
        }
        deletedClientActionIds.add(clientActionId);
        Set<String> removedReferenceIds = new LinkedHashSet<>();
        store.updateEvents(events -> events.stream()
                .filter(event -> {
                    boolean remove = clientActionId.equals(extensionText(event.context(), "clientActionId"));
                    if (remove) {
                        eventReferences(event).forEach(reference -> removedReferenceIds.add(reference.id()));
                    }
                    return !remove;
                })
                .toList());
        if (!removedReferenceIds.isEmpty()) {
            classifiedValues.removeIf(value -> removedReferenceIds.contains(value.reference().id()));
            dataWriter.write(dataPath, classifiedValues);
        }
    }

    private void reorderStep(BrowserSignal signal) {
        String clientActionId = privacy.sanitizeText(signal.dataString("clientActionId")).value();
        String direction = privacy.sanitizeText(signal.dataString("direction")).value().toLowerCase(Locale.ROOT);
        if (clientActionId.isBlank() || (!direction.equals("up") && !direction.equals("down"))) {
            warningConsumer.accept("A browser step reorder was ignored because it was incomplete.");
            return;
        }
        store.updateEvents(events -> {
            List<CaptureEvent> updated = new ArrayList<>(events);
            int index = -1;
            for (int position = 0; position < updated.size(); position++) {
                if (clientActionId.equals(extensionText(updated.get(position).context(), "clientActionId"))) {
                    index = position;
                    break;
                }
            }
            int targetIndex = direction.equals("up") ? index - 1 : index + 1;
            if (index < 0 || targetIndex < 0 || targetIndex >= updated.size()) {
                return events;
            }
            CaptureEvent current = updated.get(index);
            CaptureEvent adjacent = updated.get(targetIndex);
            updated.set(index, withContext(current, withSequence(current.context(), adjacent.context().sequence())));
            updated.set(targetIndex, withContext(adjacent, withSequence(adjacent.context(), current.context().sequence())));
            return updated;
        });
    }

    private List<LocatorCandidate> applyLocatorPreference(
            String logicalElementId,
            List<LocatorCandidate> locators) {
        LocatorPreference preference = locatorPreferences.get(logicalElementId);
        if (preference == null || locators.isEmpty()) {
            return locators;
        }
        return locators.stream()
                .map(candidate -> preference.matches(candidate) ? userProvided(candidate) : candidate)
                .toList();
    }

    private static LocatorCandidate userProvided(LocatorCandidate candidate) {
        EnumSet<LocatorCandidate.LocatorSignal> signals = candidate.signals().isEmpty()
                ? EnumSet.noneOf(LocatorCandidate.LocatorSignal.class)
                : EnumSet.copyOf(candidate.signals());
        signals.add(LocatorCandidate.LocatorSignal.USER_PROVIDED);
        return new LocatorCandidate(
                candidate.strategy(),
                candidate.expression(),
                candidate.uniquenessCount(),
                candidate.visible(),
                candidate.stable(),
                signals);
    }

    private void append(
            CaptureEvent event,
            RedactionSummary summary,
            List<ExternalTestDataReference> references) {
        store.append(event, references, summary);
    }

    private Map<String, JsonNode> contextExtensions(
            BrowserSignal signal,
            SafePage page,
            Map<String, JsonNode> extensions) {
        Map<String, JsonNode> result = new LinkedHashMap<>();
        String clientActionId = privacy.sanitizeText(signal.dataString("clientActionId")).value();
        if (!clientActionId.isBlank()) {
            result.put("clientActionId", StringNode.valueOf(clientActionId));
        }
        String stepDescription = privacy.sanitizeText(signal.dataString("stepDescription")).value();
        if (!stepDescription.isBlank()) {
            result.put("stepDescription", StringNode.valueOf(stepDescription));
        }
        if (!page.domSnapshot().isBlank()) {
            result.put("domSnapshot", StringNode.valueOf(page.domSnapshot()));
        }
        result.putAll(extensions);
        return result;
    }

    private static EventContext withExtension(EventContext context, String name, String value) {
        Map<String, JsonNode> extensions = new LinkedHashMap<>(context.extensions());
        extensions.put(name, StringNode.valueOf(value));
        return new EventContext(
                context.sequence(),
                context.timestamp(),
                context.page(),
                context.replayStatus(),
                context.evidence(),
                extensions);
    }

    private static EventContext withSequence(EventContext context, long sequence) {
        return new EventContext(
                sequence,
                context.timestamp(),
                context.page(),
                context.replayStatus(),
                context.evidence(),
                context.extensions());
    }

    private static String extensionText(EventContext context, String name) {
        JsonNode value = context.extensions().get(name);
        return value == null ? "" : value.asText("");
    }

    private static CaptureEvent withContext(CaptureEvent event, EventContext context) {
        return switch (event) {
            case CaptureEvent.NavigationEvent value -> new CaptureEvent.NavigationEvent(
                    context, value.action(), value.targetUrl());
            case CaptureEvent.ClickEvent value -> new CaptureEvent.ClickEvent(
                    context, value.target(), value.button(), value.clickCount());
            case CaptureEvent.TypeEvent value -> new CaptureEvent.TypeEvent(context, value.target(), value.value());
            case CaptureEvent.ClearEvent value -> new CaptureEvent.ClearEvent(context, value.target());
            case CaptureEvent.SelectEvent value -> new CaptureEvent.SelectEvent(
                    context, value.target(), value.mode(), value.value());
            case CaptureEvent.ToggleEvent value -> new CaptureEvent.ToggleEvent(
                    context, value.target(), value.checked());
            case CaptureEvent.UploadEvent value -> new CaptureEvent.UploadEvent(
                    context,
                    value.target(),
                    value.file(),
                    value.safeFileName(),
                    value.mediaType(),
                    value.sizeBytes());
            case CaptureEvent.KeyboardEvent value -> new CaptureEvent.KeyboardEvent(
                    context, value.target(), value.keys());
            case CaptureEvent.WindowEvent value -> new CaptureEvent.WindowEvent(
                    context, value.action(), value.logicalWindowId());
            case CaptureEvent.FrameEvent value -> new CaptureEvent.FrameEvent(
                    context, value.action(), value.logicalFrameId(), value.target());
            case CaptureEvent.AlertEvent value -> new CaptureEvent.AlertEvent(context, value.action(), value.text());
            case CaptureEvent.WaitEvent value -> new CaptureEvent.WaitEvent(
                    context, value.condition(), value.timeout(), value.target(), value.expected());
            case CaptureEvent.VerificationEvent value -> new CaptureEvent.VerificationEvent(
                    context, value.verification(), value.target(), value.expected(), value.negated());
            case CaptureEvent.NetworkEvent value -> new CaptureEvent.NetworkEvent(
                    context,
                    value.transactionId(),
                    value.resourceKind(),
                    value.request(),
                    value.response(),
                    value.timing(),
                    value.failureReason(),
                    value.initiatorPageUrl(),
                    value.correlatedUiSequence());
        };
    }

    private static List<ExternalTestDataReference> eventReferences(CaptureEvent event) {
        if (event instanceof CaptureEvent.TypeEvent value) {
            return List.of(value.value());
        }
        if (event instanceof CaptureEvent.SelectEvent value) {
            return List.of(value.value());
        }
        if (event instanceof CaptureEvent.UploadEvent value) {
            return List.of(value.file());
        }
        if (event instanceof CaptureEvent.AlertEvent value && value.text() != null) {
            return List.of(value.text());
        }
        if (event instanceof CaptureEvent.WaitEvent value && value.expected() != null) {
            return List.of(value.expected());
        }
        if (event instanceof CaptureEvent.VerificationEvent value && value.expected() != null) {
            return List.of(value.expected());
        }
        // NetworkEvent carries no ExternalTestDataReference (bodies are safe BodyRef
        // references, not externalized test data); intentionally falls through to
        // the empty default below. Wiring network evidence cleanup is P2 territory.
        return List.of();
    }

    private String logicalWindow(String contextId) {
        String key = contextId == null || contextId.isBlank() ? "default" : contextId;
        return logicalWindows.computeIfAbsent(key, ignored -> "window-" + (logicalWindows.size() + 1));
    }

    private static String targetKey(BrowserSignal signal) {
        String target = string(signal.target().get("logicalElementId"));
        return target.isBlank() ? signal.kind() + "-" + signal.browsingContextId() : target;
    }

    private static boolean isStandaloneEditingKey(BrowserSignal signal) {
        if (!"keyboard".equals(signal.kind())) {
            return false;
        }
        List<String> keys = signal.dataStrings("keys");
        if (keys == null || keys.isEmpty()) {
            return false;
        }
        // Single key without modifiers is an editing key: suppress it on text inputs
        if (keys.size() != 1) {
            return false;
        }
        String key = keys.getFirst().toUpperCase(Locale.ROOT);
        if (!("BACKSPACE".equals(key) || "DELETE".equals(key)
                || "ARROWUP".equals(key) || "ARROWDOWN".equals(key)
                || "ARROWLEFT".equals(key) || "ARROWRIGHT".equals(key))) {
            return false;
        }
        // Check if target is a text-entry element
        String tagName = string(signal.target().get("tagName")).toLowerCase(Locale.ROOT);
        String type = string(signal.target().get("attributes"));
        if ("textarea".equals(tagName)) {
            return true;
        }
        if ("input".equals(tagName)) {
            Map<String, String> attributes = strings(signal.target().get("attributes"));
            String inputType = attributes.getOrDefault("type", "text").toLowerCase(Locale.ROOT);
            return !("button".equals(inputType) || "submit".equals(inputType)
                    || "reset".equals(inputType) || "checkbox".equals(inputType)
                    || "radio".equals(inputType) || "file".equals(inputType)
                    || "hidden".equals(inputType));
        }
        return false;
    }

    private boolean isDuplicate(BrowserSignal signal) {
        String fingerprint = signal.kind()
                + "|" + signal.timestamp().toEpochMilli()
                + "|" + targetKey(signal)
                + "|" + signal.data().hashCode();
        if (recentSignals.containsKey(fingerprint)) {
            return true;
        }
        recentSignals.put(fingerprint, signal.timestamp());
        while (recentSignals.size() > 512) {
            String oldest = recentSignals.keySet().iterator().next();
            recentSignals.remove(oldest);
        }
        return false;
    }

    private static String logicalValueName(ElementSnapshot target, long eventSequence) {
        return target.logicalElementId() + "-" + eventSequence;
    }

    private static String expectedValueName(
            CaptureEvent.VerificationKind kind,
            SafeTarget target,
            long eventSequence) {
        return (target == null ? "page" : target.snapshot().logicalElementId())
                + "-" + kind.name().toLowerCase(Locale.ROOT).replace('_', '-')
                + "-" + eventSequence;
    }

    private static String bestSelector(ElementSnapshot target) {
        return target.locatorCandidates().isEmpty()
                ? ""
                : target.locatorCandidates().getFirst().expression();
    }

    private static CaptureEvent.MouseButton mouseButton(int button) {
        return switch (button) {
            case 1 -> CaptureEvent.MouseButton.MIDDLE;
            case 2 -> CaptureEvent.MouseButton.SECONDARY;
            default -> CaptureEvent.MouseButton.PRIMARY;
        };
    }

    private static Set<LocatorCandidate.LocatorSignal> locatorSignals(Object value) {
        EnumSet<LocatorCandidate.LocatorSignal> signals =
                EnumSet.noneOf(LocatorCandidate.LocatorSignal.class);
        for (String item : list(value)) {
            try {
                signals.add(LocatorCandidate.LocatorSignal.valueOf(item.toUpperCase(Locale.ROOT)));
            } catch (IllegalArgumentException ignored) {
                // Forward-compatible unknown scoring signals are ignored.
            }
        }
        return signals;
    }

    private static Map<String, String> strings(Object value) {
        if (!(value instanceof Map<?, ?> map)) {
            return Map.of();
        }
        Map<String, String> result = new LinkedHashMap<>();
        map.forEach((key, item) -> result.put(string(key), string(item)));
        return result;
    }

    private static List<String> list(Object value) {
        if (!(value instanceof List<?> list)) {
            return List.of();
        }
        return list.stream().map(CaptureEventPipeline::string).filter(item -> !item.isBlank()).toList();
    }

    private static String string(Object value) {
        return value == null ? "" : String.valueOf(value);
    }

    private static int integer(Object value, int fallback) {
        if (value instanceof Number number) {
            return number.intValue();
        }
        try {
            return Integer.parseInt(string(value));
        } catch (NumberFormatException ignored) {
            return fallback;
        }
    }

    private static boolean bool(Object value) {
        return value instanceof Boolean bool ? bool : Boolean.parseBoolean(string(value));
    }

    private static <E extends Enum<E>> E enumValue(Class<E> type, String value, E fallback) {
        try {
            return Enum.valueOf(type, value == null ? "" : value.toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException exception) {
            return fallback;
        }
    }

    private Map<String, JsonNode> verificationExtensions(BrowserSignal signal) {
        String attribute = privacy.sanitizeText(signal.dataString("attributeName")).value();
        return attribute.isBlank() ? Map.of() : Map.of("attributeName", StringNode.valueOf(attribute));
    }

    private void ensureOpen() {
        if (closed) {
            throw new IllegalStateException("Capture event pipeline is closed.");
        }
    }

    private record SafePage(PageContext context, RedactionSummary summary, String domSnapshot) {
    }

    private record SafeTarget(ElementSnapshot snapshot, RedactionSummary summary) {
    }

    private record LocatorPreference(LocatorCandidate.LocatorStrategy strategy, String expression) {
        private boolean matches(LocatorCandidate candidate) {
            return candidate.strategy() == strategy && candidate.expression().equals(expression);
        }
    }
}
