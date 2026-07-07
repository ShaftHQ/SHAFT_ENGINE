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
    private static final Duration NAVIGATION_DEBOUNCE = Duration.ofMillis(750);

    private final CaptureSessionStore store;
    private final CapturePrivacyClassifier privacy;
    private final CapturePrivacyPolicy policy;
    private final ExternalTestDataWriter dataWriter = new ExternalTestDataWriter();
    private final Path dataPath;
    private final Consumer<String> currentUrlConsumer;
    private final Consumer<String> warningConsumer;
    private final List<ClassifiedValue> classifiedValues = new ArrayList<>();
    private final Map<String, BrowserSignal> pendingInputs = new LinkedHashMap<>();
    private final Map<String, BrowserSignal> pendingClicks = new LinkedHashMap<>();
    private final Map<String, String> logicalWindows = new LinkedHashMap<>();
    private final Map<String, LocatorPreference> locatorPreferences = new LinkedHashMap<>();
    private final Map<String, Instant> recentSignals = new LinkedHashMap<>();
    private final ScheduledExecutorService debounceExecutor;
    private long sequence;
    private String lastNavigationUrl = "";
    private Instant lastNavigationAt = Instant.EPOCH;
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
        try {
            switch (signal.kind()) {
                case "input" -> {
                    pendingInputs.put(targetKey(signal), signal);
                    if (signal.dataBoolean("committed")) {
                        flushSignal(pendingInputs.remove(targetKey(signal)));
                    }
                }
                case "click" -> pendingClicks.put(targetKey(signal), signal);
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
        flushOrdered(ready);
    }

    private void flushPendingBefore(Instant timestamp) {
        List<BrowserSignal> ready = new ArrayList<>();
        collectReady(pendingInputs, timestamp, ready);
        collectReady(pendingClicks, timestamp, ready);
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
        pendingInputs.clear();
        pendingClicks.clear();
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
        switch (signal.kind()) {
            case "navigation" -> emitNavigation(signal);
            case "click" -> append(new CaptureEvent.ClickEvent(
                    context(signal),
                    target(signal).snapshot(),
                    mouseButton(signal.dataInt("button", 0)),
                    Math.max(1, signal.dataInt("clickCount", 1))),
                    target(signal).summary(), List.of());
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
        if (page.context().url().equals(lastNavigationUrl)
                && Duration.between(lastNavigationAt, signal.timestamp()).abs()
                .compareTo(NAVIGATION_DEBOUNCE) < 0) {
            return;
        }
        lastNavigationUrl = page.context().url();
        lastNavigationAt = signal.timestamp();
        currentUrlConsumer.accept(page.context().url());
        CaptureEvent.NavigationAction action = enumValue(
                CaptureEvent.NavigationAction.class,
                signal.dataString("action"),
                CaptureEvent.NavigationAction.OPEN);
        append(new CaptureEvent.NavigationEvent(context(signal, page), action, page.context().url()),
                page.summary(), List.of());
    }

    private void emitInput(BrowserSignal signal) {
        SafeTarget target = target(signal);
        String value = signal.dataString("value");
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
                 TITLE_EQUALS, TITLE_CONTAINS, PAGE_TEXT_CONTAINS -> true;
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
