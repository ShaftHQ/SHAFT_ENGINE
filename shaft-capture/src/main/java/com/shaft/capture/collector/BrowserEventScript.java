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
        return script(testIdAttributes);
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
        return "return (" + script(testIdAttributes) + ")(null);";
    }

    /**
     * Returns JavaScript that drains fallback signals.
     *
     * @return queue drain JavaScript
     */
    public static String fallbackDrain() {
        return "return globalThis.__shaftCaptureQueue ? globalThis.__shaftCaptureQueue.splice(0) : [];";
    }

    private static String script(List<String> testIdAttributes) {
        if (testIdAttributes == null || testIdAttributes.isEmpty()) {
            return SCRIPT;
        }
        return SCRIPT.replace(DEFAULT_TEST_ID_ATTRIBUTES,
                "const testIdAttributes = " + jsArray(testIdAttributes) + ";");
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
