package com.shaft.capture.collector;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * Loads the browser-side semantic event listener.
 */
public final class BrowserEventScript {
    private static final String RESOURCE = "/browser/shaft-capture-recorder.js";
    private static final String DEFAULT_TEST_ID_ATTRIBUTES =
            "const testIdAttributes = [\"data-testid\", \"data-test\", \"data-qa\"];";
    private static final String DEFAULT_STEPS_ENDPOINT =
            "const stepsEndpoint = {url: \"\", token: \"\"};";
    private static final String SCRIPT = load();

    private BrowserEventScript() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns the BiDi preload function declaration.
     *
     * @return JavaScript function
     */
    public static String preloadFunction() {
        return SCRIPT;
    }

    /**
     * Returns the BiDi preload function declaration with custom test-id attributes.
     *
     * @param testIdAttributes locator test-id attributes
     * @return JavaScript function
     */
    public static String preloadFunction(List<String> testIdAttributes) {
        return script(testIdAttributes, "", "");
    }

    /**
     * Returns the BiDi preload function declaration with custom test-id attributes and the
     * control-server steps endpoint used to rehydrate the recorder UI step list across
     * navigations, including cross-origin ones, from the server-side session store rather than
     * page-scoped storage.
     *
     * @param testIdAttributes locator test-id attributes
     * @param stepsEndpoint loopback steps query endpoint
     * @param stepsToken loopback steps query token
     * @return JavaScript function
     */
    public static String preloadFunction(List<String> testIdAttributes, String stepsEndpoint, String stepsToken) {
        return script(testIdAttributes, stepsEndpoint, stepsToken);
    }

    /**
     * Returns JavaScript that installs the fallback queue listener.
     *
     * @return installation JavaScript
     */
    public static String fallbackInstallation() {
        return "return (" + SCRIPT + ")(null);";
    }

    /**
     * Returns JavaScript that installs the fallback queue listener with custom test-id attributes.
     *
     * @param testIdAttributes locator test-id attributes
     * @return installation JavaScript
     */
    public static String fallbackInstallation(List<String> testIdAttributes) {
        return "return (" + script(testIdAttributes, "", "") + ")(null);";
    }

    /**
     * Returns JavaScript that installs the fallback queue listener with a loopback event sink.
     *
     * @param testIdAttributes locator test-id attributes
     * @param eventEndpoint loopback event endpoint
     * @param eventToken loopback event token
     * @return installation JavaScript
     */
    public static String fallbackInstallation(
            List<String> testIdAttributes,
            String eventEndpoint,
            String eventToken) {
        return fallbackInstallation(testIdAttributes, eventEndpoint, eventToken, "", "");
    }

    /**
     * Returns JavaScript that installs the fallback queue listener with a loopback event sink and
     * the control-server steps endpoint used to rehydrate the recorder UI step list across
     * navigations, including cross-origin ones, from the server-side session store rather than
     * page-scoped storage.
     *
     * @param testIdAttributes locator test-id attributes
     * @param eventEndpoint loopback event endpoint
     * @param eventToken loopback event token
     * @param stepsEndpoint loopback steps query endpoint
     * @param stepsToken loopback steps query token
     * @return installation JavaScript
     */
    public static String fallbackInstallation(
            List<String> testIdAttributes,
            String eventEndpoint,
            String eventToken,
            String stepsEndpoint,
            String stepsToken) {
        if (eventEndpoint == null || eventEndpoint.isBlank()
                || eventToken == null || eventToken.isBlank()) {
            return fallbackInstallation(testIdAttributes);
        }
        return "return (" + script(testIdAttributes, stepsEndpoint, stepsToken) + ")(null, {url: "
                + jsString(eventEndpoint) + ", token: " + jsString(eventToken) + "});";
    }

    /**
     * Returns JavaScript that drains fallback signals.
     *
     * @return queue drain JavaScript
     */
    public static String fallbackDrain() {
        return "return typeof globalThis.__shaftCaptureDrain === \"function\" "
                + "? globalThis.__shaftCaptureDrain() "
                + ": (globalThis.__shaftCaptureQueue ? globalThis.__shaftCaptureQueue.splice(0) : []);";
    }

    private static String script(List<String> testIdAttributes, String stepsEndpoint, String stepsToken) {
        String result = SCRIPT;
        if (testIdAttributes != null && !testIdAttributes.isEmpty()) {
            result = result.replace(DEFAULT_TEST_ID_ATTRIBUTES,
                    "const testIdAttributes = " + jsArray(testIdAttributes) + ";");
        }
        if (stepsEndpoint != null && !stepsEndpoint.isBlank()
                && stepsToken != null && !stepsToken.isBlank()) {
            result = result.replace(DEFAULT_STEPS_ENDPOINT,
                    "const stepsEndpoint = {url: " + jsString(stepsEndpoint)
                            + ", token: " + jsString(stepsToken) + "};");
        }
        return result;
    }

    private static String jsArray(List<String> values) {
        return values.stream()
                .filter(value -> value != null && !value.isBlank())
                .map(BrowserEventScript::jsString)
                .collect(java.util.stream.Collectors.joining(", ", "[", "]"));
    }

    private static String jsString(String value) {
        StringBuilder result = new StringBuilder("\"");
        for (int index = 0; index < value.length(); index++) {
            char character = value.charAt(index);
            if (character == '"' || character == '\\') {
                result.append('\\');
            }
            result.append(character);
        }
        return result.append('"').toString();
    }

    private static String load() {
        try (InputStream input = BrowserEventScript.class.getResourceAsStream(RESOURCE)) {
            if (input == null) {
                throw new IllegalStateException("Bundled SHAFT Capture browser listener is missing.");
            }
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException exception) {
            throw new IllegalStateException("Bundled SHAFT Capture browser listener could not be read.", exception);
        }
    }
}
