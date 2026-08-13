package com.shaft.tools.io.internal;

import com.shaft.gui.capabilities.AutomationBackend;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

final class PlaywrightTraceImporter {
    private static final int SUPPORTED_TRACE_VERSION = 8;
    private static final int MAX_ACTIONS = 10_000;
    private static final int MAX_LOGS_PER_ACTION = 100;
    private static final int MAX_TEXT_CHARACTERS = 4_096;
    private static final long CORRELATION_WINDOW_MILLIS = 2_000L;
    private static final ObjectMapper JSON = new ObjectMapper();

    private PlaywrightTraceImporter() {
    }

    static final class UnsupportedTraceVersionException extends IOException {
        private UnsupportedTraceVersionException(String message) {
            super(message);
        }
    }

    static ImportedTrace importTrace(Path archive, List<TraceEventRecorder.ActionEvent> shaftActions)
            throws IOException {
        PlaywrightTraceArchiveLoader.LoadedArchive loaded = PlaywrightTraceArchiveLoader.load(archive);
        List<TraceContext> contexts = new ArrayList<>();
        ActionBudget actionBudget = new ActionBudget(MAX_ACTIONS);
        for (String name : loaded.traceEntryNames()) {
            if (!name.endsWith(".trace")) {
                continue;
            }
            contexts.add(parseContext(loaded, name, actionBudget));
        }
        applyStackSidecars(loaded, contexts);
        List<NativeAction> nativeActions = mergeContexts(contexts);
        return correlate(nativeActions, shaftActions == null ? List.of() : List.copyOf(shaftActions));
    }

    private static void applyStackSidecars(PlaywrightTraceArchiveLoader.LoadedArchive loaded,
                                           List<TraceContext> contexts) throws IOException {
        Map<String, TraceContext> byPrefix = new HashMap<>();
        contexts.forEach(context -> byPrefix.put(context.name.substring(0, context.name.length() - ".trace".length()),
                context));
        for (String name : loaded.traceEntryNames()) {
            if (!name.endsWith(".stacks")) {
                continue;
            }
            TraceContext context = byPrefix.get(name.substring(0, name.length() - ".stacks".length()));
            if (context == null) {
                continue;
            }
            JsonNode root = JSON.readTree(loaded.entry(name));
            JsonNode files = root.path("files");
            JsonNode stacks = root.path("stacks");
            if (!files.isArray() || !stacks.isArray()) {
                throw new IOException("Malformed Playwright stack metadata in " + name + ".");
            }
            Map<String, MutableAction> byCall = new HashMap<>();
            context.actions.forEach(action -> byCall.put(action.callId, action));
            for (JsonNode stack : stacks.values()) {
                if (!stack.isArray() || stack.size() < 2 || !stack.get(0).canConvertToInt()
                        || !stack.get(1).isArray() || stack.get(1).isEmpty()) {
                    throw new IOException("Malformed Playwright stack entry in " + name + ".");
                }
                MutableAction action = byCall.get("call@" + stack.get(0).asInt());
                if (action == null || !action.source.isBlank()) {
                    continue;
                }
                JsonNode frame = stack.get(1).get(0);
                if (!frame.isArray() || frame.size() < 3 || !frame.get(0).canConvertToInt()) {
                    throw new IOException("Malformed Playwright stack frame in " + name + ".");
                }
                int fileIndex = frame.get(0).asInt();
                if (fileIndex < 0 || fileIndex >= files.size()) {
                    throw new IOException("Playwright stack frame references an unknown source file in " + name + ".");
                }
                action.source = safeText(files.get(fileIndex).asText()) + ':' + frame.get(1).asInt()
                        + ':' + frame.get(2).asInt();
            }
        }
    }

    private static TraceContext parseContext(PlaywrightTraceArchiveLoader.LoadedArchive loaded, String name,
                                             ActionBudget actionBudget) throws IOException {
        TraceContext context = new TraceContext(name);
        Map<String, MutableAction> actions = new LinkedHashMap<>();
        loaded.visitTraceRecords(name, (node, record) -> {
            String type = node.path("type").asText();
            switch (type) {
                case "context-options" -> {
                    if (context.initialized || !actions.isEmpty()) {
                        throw new IOException("Playwright trace context-options must appear exactly once before actions in "
                                + name + ".");
                    }
                    readContext(context, node);
                }
                case "before", "action" -> {
                    requireInitialized(context);
                    actionBudget.charge();
                    MutableAction action = MutableAction.before(node, context);
                    if (action.callId.isBlank() || actions.putIfAbsent(action.callId, action) != null) {
                        throw new IOException("Playwright trace contains an invalid or duplicate action callId in "
                                + name + ".");
                    }
                    if ("action".equals(type)) {
                        action.readAfter(node);
                    }
                }
                case "input" -> existing(actions, node, name).inputSnapshot = text(node, "inputSnapshot");
                case "log" -> {
                    MutableAction action = actions.get(text(node, "callId"));
                    if (action != null) {
                        action.addLog(text(node, "message"));
                    }
                }
                case "after" -> existing(actions, node, name).readAfter(node);
                default -> {
                    // Snapshots, resources, console, and future optional records are owned by their dedicated adapters.
                }
            }
        });
        if (!context.initialized) {
            throw new IOException("Playwright trace stream has no context-options record: " + name);
        }
        context.actions.addAll(actions.values());
        return context;
    }

    private static void readContext(TraceContext context, JsonNode node) throws IOException {
        JsonNode versionNode = node.path("version");
        if (!versionNode.isIntegralNumber() || !versionNode.canConvertToInt()) {
            throw new IOException("Playwright trace has no valid integral version in " + context.name + ".");
        }
        int version = versionNode.asInt();
        if (version != SUPPORTED_TRACE_VERSION) {
            throw new UnsupportedTraceVersionException("Unsupported Playwright trace version " + version + " in " + context.name
                    + "; only version 8 is importable.");
        }
        context.origin = text(node, "origin");
        context.wallTime = finite(node.path("wallTime").asDouble(Double.NaN), "wallTime", context.name);
        context.monotonicTime = finite(node.path("monotonicTime").asDouble(Double.NaN), "monotonicTime", context.name);
        context.initialized = true;
    }

    private static void requireInitialized(TraceContext context) throws IOException {
        if (!context.initialized) {
            throw new IOException("Playwright trace action appears before context-options in " + context.name + ".");
        }
    }

    private static double finite(double value, String field, String name) throws IOException {
        if (!Double.isFinite(value)) {
            throw new IOException("Playwright trace has no finite " + field + " in " + name + ".");
        }
        return value;
    }

    private static MutableAction existing(Map<String, MutableAction> actions, JsonNode node, String name)
            throws IOException {
        MutableAction action = actions.get(text(node, "callId"));
        if (action == null) {
            throw new IOException("Playwright trace record references an unknown action in " + name + ".");
        }
        return action;
    }

    private static List<NativeAction> mergeContexts(List<TraceContext> contexts) {
        Map<String, MutableAction> libraryByStep = new HashMap<>();
        List<MutableAction> merged = new ArrayList<>();
        for (TraceContext context : contexts) {
            if (!"library".equals(context.origin)) {
                continue;
            }
            for (MutableAction action : context.actions) {
                merged.add(action);
                if (!action.stepId.isBlank()) {
                    libraryByStep.putIfAbsent(action.stepId, action);
                }
            }
        }
        boolean hasLibrary = !merged.isEmpty();
        for (TraceContext context : contexts) {
            if ("library".equals(context.origin)) {
                continue;
            }
            for (MutableAction runner : context.actions) {
                MutableAction library = runner.stepId.isBlank() ? null : libraryByStep.get(runner.stepId);
                if (library == null) {
                    if (!hasLibrary || isUserFacingRunnerAction(runner)) {
                        merged.add(runner);
                    }
                    continue;
                }
                library.mergeRunner(runner);
            }
        }
        return merged.stream().map(MutableAction::freeze)
                .sorted(Comparator.comparingLong(NativeAction::startEpochMillis)
                        .thenComparing(NativeAction::callId))
                .toList();
    }

    private static boolean isUserFacingRunnerAction(MutableAction action) {
        return "Test".equals(action.className) && !"hook".equals(action.method) && !"fixture".equals(action.method);
    }

    private static ImportedTrace correlate(List<NativeAction> nativeActions,
                                           List<TraceEventRecorder.ActionEvent> shaftActions) {
        Map<String, NavigableMap<Long, List<NativeAction>>> byOperation = new HashMap<>();
        for (NativeAction action : nativeActions) {
            byOperation.computeIfAbsent(canonicalOperation(action.method()), ignored -> new TreeMap<>())
                    .computeIfAbsent(action.startEpochMillis(), ignored -> new ArrayList<>()).add(action);
        }
        List<Correlation> correlations = new ArrayList<>();
        List<TraceEventRecorder.ActionEvent> correlated = new ArrayList<>(shaftActions.size());
        for (TraceEventRecorder.ActionEvent shaftAction : shaftActions) {
            NativeAction match = uniqueNearest(shaftAction, byOperation);
            if (match == null) {
                correlated.add(shaftAction);
                continue;
            }
            removeCandidate(byOperation, match);
            correlations.add(new Correlation(shaftAction.id(), match.callId(), "exact-operation-time"));
            Map<String, String> metadata = new LinkedHashMap<>(shaftAction.metadata());
            metadata.put("playwrightCallId", match.callId());
            metadata.put("playwrightStepId", match.stepId());
            metadata.put("playwrightCorrelation", "exact-operation-time");
            correlated.add(copyWithMetadata(shaftAction, metadata));
        }
        return new ImportedTrace(nativeActions, List.copyOf(correlations), List.copyOf(correlated));
    }

    private static NativeAction uniqueNearest(TraceEventRecorder.ActionEvent shaftAction,
                                              Map<String, NavigableMap<Long, List<NativeAction>>> byOperation) {
        if (shaftAction.backend() != AutomationBackend.MICROSOFT_PLAYWRIGHT) {
            return null;
        }
        long start;
        try {
            start = Instant.parse(shaftAction.startTime()).toEpochMilli();
        } catch (DateTimeParseException | ArithmeticException exception) {
            return null;
        }
        NavigableMap<Long, List<NativeAction>> candidates = byOperation.get(canonicalOperation(shaftAction.name()));
        if (candidates == null || candidates.isEmpty()) {
            return null;
        }
        Map.Entry<Long, List<NativeAction>> floor = candidates.floorEntry(start);
        Map.Entry<Long, List<NativeAction>> ceiling = candidates.ceilingEntry(start);
        long floorDistance = floor == null ? Long.MAX_VALUE : nonNegativeDistance(start, floor.getKey());
        long ceilingDistance = ceiling == null ? Long.MAX_VALUE : nonNegativeDistance(ceiling.getKey(), start);
        long distance = Math.min(floorDistance, ceilingDistance);
        boolean tiedDifferentTimes = floor != null && ceiling != null && floorDistance == ceilingDistance
                && !floor.getKey().equals(ceiling.getKey());
        if (distance > CORRELATION_WINDOW_MILLIS || tiedDifferentTimes) {
            return null;
        }
        List<NativeAction> nearest = floorDistance <= ceilingDistance ? floor.getValue() : ceiling.getValue();
        return nearest.size() == 1 ? nearest.getFirst() : null;
    }

    private static long nonNegativeDistance(long greater, long lesser) {
        try {
            return Math.subtractExact(greater, lesser);
        } catch (ArithmeticException exception) {
            return Long.MAX_VALUE;
        }
    }

    private static void removeCandidate(Map<String, NavigableMap<Long, List<NativeAction>>> byOperation,
                                        NativeAction match) {
        NavigableMap<Long, List<NativeAction>> candidates = byOperation.get(canonicalOperation(match.method()));
        List<NativeAction> sameTime = candidates.get(match.startEpochMillis());
        sameTime.remove(match);
        if (sameTime.isEmpty()) {
            candidates.remove(match.startEpochMillis());
        }
    }

    private static TraceEventRecorder.ActionEvent copyWithMetadata(TraceEventRecorder.ActionEvent action,
                                                                    Map<String, String> metadata) {
        return new TraceEventRecorder.ActionEvent(action.id(), action.backend(), action.category(), action.name(),
                action.status(), action.startTime(), action.durationMs(), action.locator(), action.url(), action.caller(),
                action.message(), action.exceptionType(), action.exceptionMessage(), action.attachments(), metadata,
                action.actionability(), action.domSnapshotBefore(), action.domSnapshotAfter(), action.screenshot());
    }

    private static String canonicalOperation(String value) {
        String operation = safeText(value).toLowerCase(Locale.ROOT);
        int separator = Math.max(operation.lastIndexOf('.'), operation.lastIndexOf('/'));
        if (separator >= 0) {
            operation = operation.substring(separator + 1);
        }
        return switch (operation) {
            case "type", "clear", "typeappend", "typesecure" -> "fill";
            case "doubleclick" -> "dblclick";
            case "select" -> "selectoption";
            case "scrolltoelement" -> "scrollintoviewifneeded";
            default -> operation;
        };
    }

    private static String text(JsonNode node, String field) {
        return safeText(node.path(field).asText(""));
    }

    private static String safeText(String value) {
        String bounded = value == null ? "" : value;
        if (bounded.length() > MAX_TEXT_CHARACTERS) {
            bounded = bounded.substring(0, MAX_TEXT_CHARACTERS);
        }
        return FailureTraceReporter.redactSourceText(bounded);
    }

    record ImportedTrace(List<NativeAction> actions, List<Correlation> correlations,
                         List<TraceEventRecorder.ActionEvent> correlatedActions) {
        ImportedTrace {
            actions = List.copyOf(actions);
            correlations = List.copyOf(correlations);
            correlatedActions = List.copyOf(correlatedActions);
        }
    }

    record NativeAction(String callId, String stepId, String className, String method, String title,
                        long startEpochMillis, long endEpochMillis, String beforeSnapshot, String inputSnapshot,
                        String afterSnapshot, String pageId, String source, List<String> logs, String error) {
        NativeAction {
            logs = List.copyOf(logs);
        }
    }

    record Correlation(String shaftActionId, String playwrightCallId, String basis) {
    }

    private static final class ActionBudget {
        private final int limit;
        private int count;

        private ActionBudget(int limit) {
            this.limit = limit;
        }

        private void charge() throws IOException {
            count++;
            if (count > limit) {
                throw new IOException("Playwright trace exceeds the " + limit + " imported action limit.");
            }
        }
    }

    private static final class TraceContext {
        private final String name;
        private final List<MutableAction> actions = new ArrayList<>();
        private String origin = "";
        private double wallTime;
        private double monotonicTime;
        private boolean initialized;

        private TraceContext(String name) {
            this.name = name;
        }

        private long epochMillis(double monotonic) {
            double epoch = wallTime + monotonic - monotonicTime;
            if (!Double.isFinite(epoch) || epoch < Long.MIN_VALUE || epoch > Long.MAX_VALUE) {
                throw new IllegalArgumentException("Playwright trace action timestamp is outside the supported range in "
                        + name + ".");
            }
            return Math.round(epoch);
        }
    }

    private static final class MutableAction {
        private final String callId;
        private final String stepId;
        private final String className;
        private final String method;
        private final TraceContext context;
        private final List<String> logs = new ArrayList<>();
        private String title;
        private String beforeSnapshot;
        private String inputSnapshot = "";
        private String afterSnapshot = "";
        private String pageId;
        private String source;
        private String error = "";
        private double startTime;
        private double endTime;
        private TraceContext timingContext;

        private MutableAction(JsonNode node, TraceContext context) throws IOException {
            this.callId = text(node, "callId");
            this.stepId = text(node, "stepId");
            this.className = text(node, "class");
            this.method = text(node, "method");
            this.title = text(node, "title");
            this.beforeSnapshot = text(node, "beforeSnapshot");
            this.pageId = text(node, "pageId");
            this.source = source(node.path("stack"));
            this.startTime = actionTime(node, "startTime", context.name, true);
            this.endTime = actionTime(node, "endTime", context.name, false);
            this.context = context;
            this.timingContext = context;
        }

        private static MutableAction before(JsonNode node, TraceContext context) throws IOException {
            return new MutableAction(node, context);
        }

        private void readAfter(JsonNode node) throws IOException {
            endTime = node.has("endTime") ? actionTime(node, "endTime", context.name, true) : endTime;
            afterSnapshot = text(node, "afterSnapshot");
            if (node.has("inputSnapshot")) {
                inputSnapshot = text(node, "inputSnapshot");
            }
            JsonNode errorNode = node.path("error");
            error = safeText(errorNode.isTextual() ? errorNode.asText() : errorNode.path("message").asText(""));
        }

        private static double actionTime(JsonNode node, String field, String name, boolean required)
                throws IOException {
            JsonNode value = node.path(field);
            if (value.isMissingNode() && !required) {
                return 0;
            }
            if (!value.isNumber() || !Double.isFinite(value.asDouble())) {
                throw new IOException("Playwright trace action has no finite " + field + " in " + name + ".");
            }
            double time = value.asDouble();
            if (time < -9.0E15 || time > 9.0E15) {
                throw new IOException("Playwright trace action " + field + " is outside the supported range in "
                        + name + ".");
            }
            return time;
        }

        private void addLog(String log) {
            if (logs.size() < MAX_LOGS_PER_ACTION) {
                logs.add(log);
            }
        }

        private void mergeRunner(MutableAction runner) {
            if (!runner.title.isBlank()) {
                title = runner.title;
            }
            if (!runner.source.isBlank()) {
                source = runner.source;
            }
            if (!runner.error.isBlank()) {
                error = runner.error;
            }
            startTime = runner.startTime;
            endTime = runner.endTime;
            timingContext = runner.context;
        }

        private NativeAction freeze() {
            long start = timingContext.epochMillis(startTime);
            long end = endTime <= 0 ? start : timingContext.epochMillis(endTime);
            return new NativeAction(callId, stepId, className, method, title, start, Math.max(start, end),
                    beforeSnapshot, inputSnapshot, afterSnapshot, pageId, source, logs, error);
        }

        private static String source(JsonNode stack) {
            if (!stack.isArray() || stack.isEmpty()) {
                return "";
            }
            JsonNode frame = stack.get(0);
            String file = text(frame, "file");
            if (file.isBlank()) {
                return "";
            }
            return file + ':' + frame.path("line").asInt(0) + ':' + frame.path("column").asInt(0);
        }
    }
}
