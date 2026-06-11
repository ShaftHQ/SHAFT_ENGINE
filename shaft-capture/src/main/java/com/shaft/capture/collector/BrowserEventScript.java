package com.shaft.capture.collector;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

/**
 * Loads the browser-side semantic event listener.
 */
public final class BrowserEventScript {
    private static final String RESOURCE = "/browser/shaft-capture-recorder.js";
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
     * Returns JavaScript that installs the fallback queue listener.
     *
     * @return installation JavaScript
     */
    public static String fallbackInstallation() {
        return "return (" + SCRIPT + ")(null);";
    }

    /**
     * Returns JavaScript that drains fallback signals.
     *
     * @return queue drain JavaScript
     */
    public static String fallbackDrain() {
        return "return globalThis.__shaftCaptureQueue ? globalThis.__shaftCaptureQueue.splice(0) : [];";
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
