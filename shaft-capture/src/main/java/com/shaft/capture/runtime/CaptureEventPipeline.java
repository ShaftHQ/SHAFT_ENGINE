package com.shaft.capture.runtime;

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
    private static final Duration INPUT_DEBOUNCE = Duration.ofMillis(350);
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
        return Math.toIntExact(sequence);
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
        collectReady(pendingInputs, now.minus(INPUT_DEBOUNCE), ready);
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
        if (signal != null) {
            emit(signal);
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

    private EventContext context(BrowserSignal signal) {
        return context(signal, page(signal));
    }

    private EventContext context(BrowserSignal signal, SafePage page) {
        emitContextTransitions(signal, page);
        return new EventContext(
                ++sequence,
                signal.timestamp(),
                page.context(),
                EventContext.ReplayStatus.NOT_REPLAYED,
                List.of(),
                Map.of());
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
        RedactionSummary summary = safeUrl.summary().merge(safeTitle.summary());
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
        return new SafePage(page, summary);
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

    private void append(
            CaptureEvent event,
            RedactionSummary summary,
            List<ExternalTestDataReference> references) {
        store.append(event, references, summary);
    }

    private String logicalWindow(String contextId) {
        String key = contextId == null || contextId.isBlank() ? "default" : contextId;
        return logicalWindows.computeIfAbsent(key, ignored -> "window-" + (logicalWindows.size() + 1));
    }

    private static String targetKey(BrowserSignal signal) {
        String target = string(signal.target().get("logicalElementId"));
        return target.isBlank() ? signal.kind() + "-" + signal.browsingContextId() : target;
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

    private void ensureOpen() {
        if (closed) {
            throw new IllegalStateException("Capture event pipeline is closed.");
        }
    }

    private record SafePage(PageContext context, RedactionSummary summary) {
    }

    private record SafeTarget(ElementSnapshot snapshot, RedactionSummary summary) {
    }
}
