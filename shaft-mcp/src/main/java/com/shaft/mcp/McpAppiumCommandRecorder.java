package com.shaft.mcp;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BooleanSupplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Converts Appium WebDriver commands from Inspector traffic into MCP mobile actions.
 */
final class McpAppiumCommandRecorder {
    private static final Pattern FIND_ELEMENT = Pattern.compile(".*/session/[^/]+/element$");
    private static final Pattern FIND_ELEMENTS = Pattern.compile(".*/session/[^/]+/elements$");
    private static final Pattern ELEMENT_COMMAND = Pattern.compile(".*/session/[^/]+/element/([^/]+)/([^/]+)$");
    private static final Pattern ACTIONS = Pattern.compile(".*/session/[^/]+/actions$");
    private static final Pattern ORIENTATION = Pattern.compile(".*/session/[^/]+/orientation$");
    private static final Pattern HIDE_KEYBOARD = Pattern.compile(".*/session/[^/]+/appium/device/hide_keyboard$");
    private static final Pattern BACKGROUND_APP = Pattern.compile(".*/session/[^/]+/appium/app/background$");
    private static final Pattern ACTIVATE_APP = Pattern.compile(".*/session/[^/]+/appium/device/activate_app$");

    private final ObjectMapper mapper = new ObjectMapper();
    private final McpMobileRecordingService recorder;
    private final BooleanSupplier paused;
    private final Map<String, LocatorRef> elementLocators = new ConcurrentHashMap<>();

    McpAppiumCommandRecorder(McpMobileRecordingService recorder, BooleanSupplier paused) {
        this.recorder = recorder;
        this.paused = paused == null ? () -> false : paused;
    }

    void capture(String method, String path, String requestBody, int responseStatus, String responseBody) {
        if (!"POST".equalsIgnoreCase(method) || paused.getAsBoolean() || responseStatus / 100 != 2) {
            return;
        }
        try {
            if (FIND_ELEMENT.matcher(path).matches()) {
                captureFoundElement(requestBody, responseBody, false);
                return;
            }
            if (FIND_ELEMENTS.matcher(path).matches()) {
                captureFoundElement(requestBody, responseBody, true);
                return;
            }
            Matcher element = ELEMENT_COMMAND.matcher(path);
            if (element.matches()) {
                captureElementCommand(element.group(1), element.group(2), requestBody);
                return;
            }
            if (ACTIONS.matcher(path).matches()) {
                captureActions(requestBody);
                return;
            }
            if (ORIENTATION.matcher(path).matches()) {
                captureOrientation(requestBody);
                return;
            }
            if (HIDE_KEYBOARD.matcher(path).matches()) {
                record("hideKeyboard", null, "", Map.of(),
                        "driver.touch().hideNativeKeyboard();",
                        "driver.touch().hideNativeKeyboard();",
                        false);
                return;
            }
            if (BACKGROUND_APP.matcher(path).matches()) {
                captureBackgroundApp(requestBody);
                return;
            }
            if (ACTIVATE_APP.matcher(path).matches()) {
                captureActivateApp(requestBody);
            }
        } catch (Exception exception) {
            recorder.recordWarning("Inspector command was not recorded: " + exception.getMessage());
        }
    }

    private void captureFoundElement(String requestBody, String responseBody, boolean multiple) throws Exception {
        JsonNode request = mapper.readTree(blankJson(requestBody));
        Optional<LocatorRef> locator = locator(request.path("using").asText(), request.path("value").asText());
        if (locator.isEmpty()) {
            recorder.recordWarning("Inspector locator strategy is not replayable by SHAFT: "
                    + request.path("using").asText());
            return;
        }
        JsonNode value = mapper.readTree(blankJson(responseBody)).path("value");
        if (multiple && value.isArray()) {
            for (JsonNode element : value) {
                elementId(element).ifPresent(id -> elementLocators.put(id, locator.get()));
            }
        } else {
            elementId(value).ifPresent(id -> elementLocators.put(id, locator.get()));
        }
    }

    private void captureElementCommand(String elementId, String command, String requestBody) throws Exception {
        LocatorRef locator = elementLocators.get(elementId);
        if (locator == null) {
            recorder.recordWarning("Inspector " + command
                    + " command used an element that was not found through this recording proxy.");
            return;
        }
        String locatorCode = McpMobileCode.locatorCode(locator.strategy(), locator.value());
        switch (command) {
            case "click" -> record("tap", locator.strategy(), locator.value(), Map.of(),
                    "driver.touch().tap(" + locatorCode + ");",
                    "driver.touch().tap(" + locatorCode + ");",
                    false);
            case "clear" -> record("clear", locator.strategy(), locator.value(), Map.of(),
                    "driver.element().clear(" + locatorCode + ");",
                    "driver.element().clear(" + locatorCode + ");",
                    false);
            case "value" -> {
                String value = textValue(mapper.readTree(blankJson(requestBody)));
                record("type", locator.strategy(), locator.value(), Map.of("value", value),
                        "driver.element().type(" + locatorCode + ", " + McpMobileCode.java(value) + ");",
                        "driver.element().type(" + locatorCode + ", \"<redacted>\");",
                        true);
            }
            default -> recorder.recordWarning("Inspector element command is not yet replayable: " + command);
        }
    }

    private void captureActions(String requestBody) throws Exception {
        JsonNode actions = mapper.readTree(blankJson(requestBody)).path("actions");
        if (!actions.isArray()) {
            recorder.recordWarning("Inspector W3C actions payload did not include an actions array.");
            return;
        }
        for (JsonNode source : actions) {
            if (!"pointer".equals(source.path("type").asText())) {
                continue;
            }
            String pointerType = source.path("parameters").path("pointerType").asText();
            if (!pointerType.isBlank() && !"touch".equals(pointerType) && !"pen".equals(pointerType)) {
                continue;
            }
            Optional<PointerGesture> gesture = pointerGesture(source.path("actions"));
            if (gesture.isPresent()) {
                recordGesture(gesture.get());
                return;
            }
        }
        recorder.recordWarning("Inspector W3C actions payload did not contain a replayable touch gesture.");
    }

    private void recordGesture(PointerGesture gesture) {
        if (gesture.swipe()) {
            int duration = Math.max(gesture.durationMillis(), 100);
            Map<String, String> params = Map.of(
                    "startX", String.valueOf(gesture.startX()),
                    "startY", String.valueOf(gesture.startY()),
                    "endX", String.valueOf(gesture.endX()),
                    "endY", String.valueOf(gesture.endY()),
                    "durationMillis", String.valueOf(duration));
            String code = McpMobileCode.swipeCoordinatesCode(
                    gesture.startX(), gesture.startY(), gesture.endX(), gesture.endY(), duration);
            record("swipeCoordinates", null, "", params, code, code, false);
        } else {
            Map<String, String> params = Map.of(
                    "x", String.valueOf(gesture.startX()),
                    "y", String.valueOf(gesture.startY()));
            String code = McpMobileCode.tapCoordinatesCode(gesture.startX(), gesture.startY());
            record("tapCoordinates", null, "", params, code, code, false);
        }
    }

    private void captureOrientation(String requestBody) throws Exception {
        String orientation = mapper.readTree(blankJson(requestBody)).path("orientation").asText("PORTRAIT")
                .toUpperCase(java.util.Locale.ROOT);
        String code = "driver.touch().rotate(ScreenOrientation." + orientation + ");";
        record("rotate", null, "", Map.of("orientation", orientation), code, code, false);
    }

    private void captureBackgroundApp(String requestBody) throws Exception {
        JsonNode request = mapper.readTree(blankJson(requestBody));
        int seconds = request.has("seconds") ? request.path("seconds").asInt() : request.path("duration").asInt(1);
        String code = "driver.touch().sendAppToBackground(" + seconds + ");";
        record("backgroundApp", null, "", Map.of("seconds", String.valueOf(seconds)), code, code, false);
    }

    private void captureActivateApp(String requestBody) throws Exception {
        JsonNode request = mapper.readTree(blankJson(requestBody));
        String appId = firstText(request, "appId", "bundleId");
        if (appId.isBlank()) {
            recorder.recordWarning("Inspector activate app command did not include appId or bundleId.");
            return;
        }
        String code = "driver.touch().activateAppFromBackground(" + McpMobileCode.java(appId) + ");";
        record("activateApp", null, "", Map.of("appId", appId), code, code, false);
    }

    private Optional<PointerGesture> pointerGesture(JsonNode events) {
        if (!events.isArray()) {
            return Optional.empty();
        }
        int lastX = 0;
        int lastY = 0;
        int startX = 0;
        int startY = 0;
        int endX = 0;
        int endY = 0;
        int duration = 0;
        boolean hasPosition = false;
        boolean pointerDown = false;
        boolean sawDown = false;
        boolean movedWhileDown = false;
        for (JsonNode event : events) {
            String type = event.path("type").asText();
            if ("pointerMove".equals(type)) {
                lastX = event.path("x").asInt(lastX);
                lastY = event.path("y").asInt(lastY);
                hasPosition = true;
                if (pointerDown) {
                    endX = lastX;
                    endY = lastY;
                    duration += event.path("duration").asInt(0);
                    movedWhileDown = true;
                }
            } else if ("pointerDown".equals(type) && hasPosition) {
                pointerDown = true;
                sawDown = true;
                startX = lastX;
                startY = lastY;
                endX = lastX;
                endY = lastY;
            } else if ("pointerUp".equals(type) && pointerDown) {
                pointerDown = false;
            } else if ("pause".equals(type) && pointerDown) {
                duration += event.path("duration").asInt(0);
            }
        }
        if (!sawDown) {
            return Optional.empty();
        }
        return Optional.of(new PointerGesture(startX, startY, endX, endY, duration, movedWhileDown
                && (startX != endX || startY != endY)));
    }

    private Optional<LocatorRef> locator(String using, String value) {
        String normalized = using == null ? "" : using.trim().toLowerCase(java.util.Locale.ROOT);
        if (value == null || value.isBlank()) {
            return Optional.empty();
        }
        return switch (normalized) {
            case "accessibility id" -> Optional.of(new LocatorRef(locatorStrategy.ACCESSIBILITY_ID, value));
            case "id" -> Optional.of(new LocatorRef(locatorStrategy.ID, value));
            case "xpath" -> Optional.of(new LocatorRef(locatorStrategy.XPATH, value));
            case "name" -> Optional.of(new LocatorRef(locatorStrategy.NAME, value));
            case "class name" -> Optional.of(new LocatorRef(locatorStrategy.CLASSNAME, value));
            case "css selector" -> Optional.of(new LocatorRef(locatorStrategy.CSSSELECTOR, value));
            case "-android uiautomator" -> Optional.of(new LocatorRef(locatorStrategy.ANDROID_UIAUTOMATOR, value));
            case "-ios predicate string" -> Optional.of(new LocatorRef(locatorStrategy.IOS_PREDICATE, value));
            case "-ios class chain" -> Optional.of(new LocatorRef(locatorStrategy.IOS_CLASS_CHAIN, value));
            default -> Optional.empty();
        };
    }

    private Optional<String> elementId(JsonNode element) {
        if (element == null || element.isMissingNode() || element.isNull()) {
            return Optional.empty();
        }
        String w3c = element.path("element-6066-11e4-a52e-4f735466cecf").asText();
        if (!w3c.isBlank()) {
            return Optional.of(w3c);
        }
        String legacy = element.path("ELEMENT").asText();
        return legacy.isBlank() ? Optional.empty() : Optional.of(legacy);
    }

    private String textValue(JsonNode request) {
        String text = request.path("text").asText();
        if (!text.isBlank()) {
            return text;
        }
        JsonNode value = request.path("value");
        if (value.isArray()) {
            List<String> characters = new ArrayList<>();
            value.forEach(node -> characters.add(node.asText()));
            return String.join("", characters);
        }
        return value.asText("");
    }

    private String firstText(JsonNode request, String first, String second) {
        String value = request.path(first).asText();
        return value.isBlank() ? request.path(second).asText("") : value;
    }

    private McpMobileRecordedAction record(
            String action,
            locatorStrategy strategy,
            String locatorValue,
            Map<String, String> parameters,
            String javaCode,
            String redactedJavaCode,
            boolean sensitive) {
        return recorder.record(action, strategy, locatorValue, parameters, javaCode, redactedJavaCode, sensitive);
    }

    private static String blankJson(String body) {
        return body == null || body.isBlank() ? "{}" : body;
    }

    private record LocatorRef(locatorStrategy strategy, String value) {
    }

    private record PointerGesture(int startX, int startY, int endX, int endY, int durationMillis, boolean swipe) {
    }
}
