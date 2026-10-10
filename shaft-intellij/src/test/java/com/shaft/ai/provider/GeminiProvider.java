package com.shaft.ai.provider;

/**
 * Test-classpath stand-in for shaft-ai {@code GeminiProvider}.
 * The plugin test runtime cannot load that Java 25 module, so the live assertion
 * compiles this copy. {@code GeminiProviderTest} rejects any drift from the real method.
 */
public final class GeminiProvider {
    private GeminiProvider() {
    }

    public static boolean acceptsServedModel(String requested, String served) {
        if (requested == null || requested.isBlank() || served == null || served.isBlank()) {
            return false;
        }
        String normalizedServed = served.startsWith("models/") ? served.substring("models/".length()) : served;
        if (normalizedServed.startsWith(requested)) {
            return true;
        }
        return geminiFlashId(requested) && geminiFlashId(normalizedServed);
    }

    private static boolean geminiFlashId(String modelId) {
        return modelId != null && modelId.startsWith("gemini-") && modelId.contains("-flash");
    }
}
