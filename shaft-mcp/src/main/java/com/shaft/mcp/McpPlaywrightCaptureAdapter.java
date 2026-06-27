package com.shaft.mcp;

import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.generate.CaptureGenerationRequest;
import com.shaft.capture.generate.CaptureGenerationResult;
import com.shaft.capture.generate.CaptureGenerator;
import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.ExternalTestDataReference;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.model.PageContext;
import com.shaft.capture.model.RedactionSummary;
import com.shaft.capture.privacy.CapturePrivacyClassifier;
import com.shaft.capture.privacy.CapturePrivacyPolicy;
import com.shaft.capture.privacy.ClassifiedValue;
import com.shaft.capture.storage.ExternalTestDataWriter;
import com.shaft.pilot.ai.ApprovalPolicy;

import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * Converts MCP Playwright action recordings into the SHAFT Capture pipeline.
 */
final class McpPlaywrightCaptureAdapter {
    private static final String GENERATED_PACKAGE = "generated.capture";
    private final CaptureJsonCodec codec = new CaptureJsonCodec();
    private final CapturePrivacyPolicy privacyPolicy = CapturePrivacyPolicy.defaults();
    private final CapturePrivacyClassifier privacy = new CapturePrivacyClassifier(privacyPolicy);
    private final ExternalTestDataWriter dataWriter = new ExternalTestDataWriter();

    CaptureAdapterResult generate(Path recordingPath, McpMobileRecording recording) {
        Path capturePath = capturePath(recordingPath);
        AdaptedRecording adapted = adapt(recording);
        codec.write(capturePath, adapted.session());
        if (!adapted.values().isEmpty()) {
            dataWriter.write(capturePath.getParent().resolve(privacyPolicy.externalDataPath()), adapted.values());
        }
        CaptureGenerationResult result = new CaptureGenerator().generate(
                request(capturePath, generatedRoot(recordingPath)), CaptureGenerator.CodegenBackend.PLAYWRIGHT);
        return new CaptureAdapterResult(capturePath, result, adapted.warnings());
    }

    private AdaptedRecording adapt(McpMobileRecording recording) {
        Instant startedAt = instant(recording.startedAt(), Instant.now());
        Instant stoppedAt = recording.stoppedAt().isBlank() ? null : instant(recording.stoppedAt(), startedAt);
        List<CaptureEvent> events = new ArrayList<>();
        List<ClassifiedValue> values = new ArrayList<>();
        List<ExternalTestDataReference> references = new ArrayList<>();
        List<String> warnings = new ArrayList<>();
        RedactionSummary summary = RedactionSummary.empty();
        String currentUrl = "";
        String currentWindow = "window-1";
        int windowCount = 1;
        int viewportWidth = 0;
        int viewportHeight = 0;

        for (McpMobileRecordedAction action : recording.actions()) {
            Instant timestamp = instant(action.timestamp(), startedAt.plusMillis(action.sequence()));
            switch (action.action()) {
                case "navigate" -> {
                    CapturePrivacyClassifier.SanitizedText safeUrl = privacy.sanitizeUrl(action.parameters().get("url"));
                    currentUrl = safeUrl.value();
                    summary = summary.merge(safeUrl.summary());
                    events.add(new CaptureEvent.NavigationEvent(
                            context(events, timestamp, currentUrl, currentWindow, viewportWidth, viewportHeight),
                            CaptureEvent.NavigationAction.OPEN,
                            currentUrl));
                }
                case "refresh" -> events.add(new CaptureEvent.NavigationEvent(
                        context(events, timestamp, currentUrl, currentWindow, viewportWidth, viewportHeight),
                        CaptureEvent.NavigationAction.REFRESH,
                        ""));
                case "navigate_back" -> events.add(new CaptureEvent.NavigationEvent(
                        context(events, timestamp, currentUrl, currentWindow, viewportWidth, viewportHeight),
                        CaptureEvent.NavigationAction.BACK,
                        ""));
                case "navigate_forward" -> events.add(new CaptureEvent.NavigationEvent(
                        context(events, timestamp, currentUrl, currentWindow, viewportWidth, viewportHeight),
                        CaptureEvent.NavigationAction.FORWARD,
                        ""));
                case "set_window_size" -> {
                    viewportWidth = integer(action.parameters().get("width"));
                    viewportHeight = integer(action.parameters().get("height"));
                }
                case "new_window" -> {
                    currentWindow = "window-" + (++windowCount);
                    CaptureEvent.WindowAction windowAction = "WINDOW".equals(action.parameters().get("windowType"))
                            ? CaptureEvent.WindowAction.OPEN_WINDOW
                            : CaptureEvent.WindowAction.OPEN_TAB;
                    events.add(new CaptureEvent.WindowEvent(
                            context(events, timestamp, currentUrl, currentWindow, viewportWidth, viewportHeight),
                            windowAction,
                            currentWindow));
                    String targetUrl = action.parameters().getOrDefault("url", "");
                    if (!targetUrl.isBlank() && !"about:blank".equalsIgnoreCase(targetUrl)) {
                        CapturePrivacyClassifier.SanitizedText safeUrl = privacy.sanitizeUrl(targetUrl);
                        currentUrl = safeUrl.value();
                        summary = summary.merge(safeUrl.summary());
                        events.add(new CaptureEvent.NavigationEvent(
                                context(events, timestamp, currentUrl, currentWindow, viewportWidth, viewportHeight),
                                CaptureEvent.NavigationAction.OPEN,
                                currentUrl));
                    }
                }
                case "click", "click_semantic", "click_js" -> {
                    warnIfApproximated(action, warnings);
                    events.add(new CaptureEvent.ClickEvent(
                            context(events, timestamp, currentUrl, currentWindow, viewportWidth, viewportHeight),
                            target(action, events.size() + 1),
                            CaptureEvent.MouseButton.PRIMARY,
                            1));
                }
                case "double_click" -> events.add(new CaptureEvent.ClickEvent(
                        context(events, timestamp, currentUrl, currentWindow, viewportWidth, viewportHeight),
                        target(action, events.size() + 1),
                        CaptureEvent.MouseButton.PRIMARY,
                        2));
                case "type", "type_semantic", "append_text", "set_value_js" -> {
                    warnIfApproximated(action, warnings);
                    ElementSnapshot target = target(action, events.size() + 1);
                    ClassifiedValue classified = value(action, target, events.size() + 1);
                    values.add(classified);
                    references.add(classified.reference());
                    summary = summary.merge(classified.summary());
                    events.add(new CaptureEvent.TypeEvent(
                            context(events, timestamp, currentUrl, currentWindow, viewportWidth, viewportHeight),
                            target,
                            classified.reference()));
                }
                case "clear" -> events.add(new CaptureEvent.ClearEvent(
                        context(events, timestamp, currentUrl, currentWindow, viewportWidth, viewportHeight),
                        target(action, events.size() + 1)));
                case "upload_file" -> {
                    ElementSnapshot target = target(action, events.size() + 1);
                    CapturePrivacyClassifier.ClassifiedUpload upload = privacy.classifyUpload(
                            logicalValueName(target, events.size() + 1),
                            action.parameters().get("value"),
                            "",
                            0);
                    references.add(upload.reference());
                    summary = summary.merge(upload.summary());
                    events.add(new CaptureEvent.UploadEvent(
                            context(events, timestamp, currentUrl, currentWindow, viewportWidth, viewportHeight),
                            target,
                            upload.reference(),
                            upload.safeFileName(),
                            upload.mediaType(),
                            upload.sizeBytes()));
                }
                default -> warnings.add("Playwright action " + action.action()
                        + " is retained in the legacy replay block; Capture schema mapping is not available yet.");
            }
        }

        CaptureSession.SessionStatus status = stoppedAt == null
                ? CaptureSession.SessionStatus.INCOMPLETE
                : CaptureSession.SessionStatus.COMPLETED;
        CaptureSession session = new CaptureSession(
                CaptureSession.CURRENT_SCHEMA_VERSION,
                "playwright-mcp-recording",
                status,
                startedAt,
                stoppedAt,
                new BrowserMetadata("playwright", "", "mcp", "mcp-playwright", Map.of("source", "shaft-mcp")),
                events,
                List.of(),
                references,
                summary,
                Map.of());
        return new AdaptedRecording(session, values, warnings);
    }

    private CaptureGenerationRequest request(Path capturePath, Path outputRoot) {
        return new CaptureGenerationRequest(
                capturePath,
                outputRoot,
                GENERATED_PACKAGE,
                "PlaywrightMcpRecordingTest",
                true,
                false,
                false,
                Duration.ofMinutes(5),
                CaptureGenerationRequest.EnrichmentMode.NONE,
                null,
                false,
                ApprovalPolicy.denyAll(),
                false,
                CaptureGenerationRequest.ControlFlowMode.NONE,
                null);
    }

    private EventContext context(
            List<CaptureEvent> events,
            Instant timestamp,
            String url,
            String window,
            int viewportWidth,
            int viewportHeight) {
        return new EventContext(
                events.size() + 1L,
                timestamp,
                new PageContext(url, "", window, List.of(), viewportWidth, viewportHeight),
                EventContext.ReplayStatus.NOT_REPLAYED,
                List.of(),
                Map.of());
    }

    private ElementSnapshot target(McpMobileRecordedAction action, long sequence) {
        String name = privacy.sanitizeText(
                action.parameters().getOrDefault("elementName", action.locatorValue())).value();
        boolean semantic = action.action().endsWith("_semantic");
        String logicalId = safeIdentifier(name.isBlank() ? "element-" + sequence : name);
        return new ElementSnapshot(
                logicalId,
                tagName(action, semantic),
                role(action, semantic),
                semantic ? name : "",
                semantic ? name : "",
                attributes(action),
                List.of(locator(action, name, semantic)),
                true,
                true,
                false);
    }

    private LocatorCandidate locator(McpMobileRecordedAction action, String name, boolean semantic) {
        LocatorCandidate.LocatorStrategy strategy = locatorStrategy(action.locatorStrategy(), semantic);
        String expression = privacy.sanitizeText(
                semantic ? name : locatorExpression(action.locatorStrategy(), action.locatorValue())).value();
        return new LocatorCandidate(
                strategy,
                expression.isBlank() ? "body" : expression,
                1,
                true,
                true,
                EnumSet.of(LocatorCandidate.LocatorSignal.USER_PROVIDED));
    }

    private ClassifiedValue value(McpMobileRecordedAction action, ElementSnapshot target, long sequence) {
        String logicalName = logicalValueName(target, sequence);
        if (!action.sensitiveValueStored()) {
            ExternalTestDataReference reference = new ExternalTestDataReference(
                    "data." + safeIdentifier(logicalName),
                    safeIdentifier(logicalName),
                    ExternalTestDataReference.DataSource.ENVIRONMENT,
                    "",
                    "",
                    ExternalTestDataReference.DataClassification.SECRET);
            return new ClassifiedValue(reference, null,
                    new RedactionSummary(1, 0, 0, Set.of("mcp-sensitive-value-redacted")));
        }
        return privacy.classifyValue(
                logicalName,
                action.parameters().get("value"),
                target.locatorCandidates().getFirst().expression(),
                target.normalizedAttributes());
    }

    private static void warnIfApproximated(McpMobileRecordedAction action, List<String> warnings) {
        if (Set.of("click_js", "append_text", "set_value_js").contains(action.action())) {
            warnings.add("Playwright action " + action.action()
                    + " is represented as the closest Capture event; the legacy replay block preserves the exact call.");
        }
    }

    private Map<String, String> attributes(McpMobileRecordedAction action) {
        String locator = privacy.sanitizeText(action.locatorValue()).value();
        return switch (action.locatorStrategy()) {
            case "ID" -> Map.of("id", locator);
            case "NAME" -> Map.of("name", locator);
            default -> Map.of();
        };
    }

    private static LocatorCandidate.LocatorStrategy locatorStrategy(String strategy, boolean semantic) {
        if (semantic) {
            return LocatorCandidate.LocatorStrategy.ACCESSIBLE_NAME;
        }
        return switch (strategy) {
            case "ID" -> LocatorCandidate.LocatorStrategy.ID;
            case "NAME" -> LocatorCandidate.LocatorStrategy.NAME;
            case "XPATH" -> LocatorCandidate.LocatorStrategy.XPATH;
            default -> LocatorCandidate.LocatorStrategy.CSS;
        };
    }

    private static String locatorExpression(String strategy, String value) {
        String locator = value == null ? "" : value.trim();
        return switch (strategy) {
            case "CLASSNAME" -> locator.startsWith(".") ? locator : "." + locator;
            default -> locator;
        };
    }

    private static String tagName(McpMobileRecordedAction action, boolean semantic) {
        if (action.action().contains("type") || action.action().equals("clear") || action.action().equals("upload_file")) {
            return "input";
        }
        return semantic ? "button" : "";
    }

    private static String role(McpMobileRecordedAction action, boolean semantic) {
        if (!semantic) {
            return "";
        }
        return action.action().contains("type") ? "textbox" : "button";
    }

    private static String logicalValueName(ElementSnapshot target, long sequence) {
        return target.logicalElementId() + "-" + sequence;
    }

    private static int integer(String value) {
        try {
            return Math.max(0, Integer.parseInt(value == null ? "" : value));
        } catch (NumberFormatException exception) {
            return 0;
        }
    }

    private static Instant instant(String value, Instant fallback) {
        try {
            return Instant.parse(value == null ? "" : value);
        } catch (RuntimeException exception) {
            return fallback;
        }
    }

    private static String safeIdentifier(String value) {
        String normalized = value == null
                ? ""
                : value.trim().toLowerCase(Locale.ROOT)
                .replaceAll("[^a-z0-9]+", "-")
                .replaceAll("^-|-$", "");
        return normalized.isBlank() ? "value" : normalized;
    }

    private static Path capturePath(Path recordingPath) {
        return recordingPath.resolveSibling(baseName(recordingPath) + "-capture.json");
    }

    private static Path generatedRoot(Path recordingPath) {
        return recordingPath.resolveSibling(baseName(recordingPath) + "-capture-generated");
    }

    private static String baseName(Path path) {
        String name = path.getFileName().toString();
        return name.endsWith(".json") ? name.substring(0, name.length() - 5) : name;
    }

    record CaptureAdapterResult(
            Path captureSessionPath,
            CaptureGenerationResult generation,
            List<String> warnings) {
        CaptureAdapterResult {
            warnings = warnings == null ? List.of() : List.copyOf(warnings);
        }
    }

    private record AdaptedRecording(
            CaptureSession session,
            List<ClassifiedValue> values,
            List<String> warnings) {
    }
}
