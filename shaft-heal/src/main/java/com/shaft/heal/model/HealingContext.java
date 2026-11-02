package com.shaft.heal.model;

import java.util.Objects;

/**
 * Privacy-minimized execution context used to prevent cross-page, cross-app,
 * frame, shadow-root, and native-context recovery.
 *
 * @param schemaVersion context schema version
 * @param platform evidence platform
 * @param applicationId package or bundle identifier
 * @param screenId activity, view-controller, or bounded root fingerprint
 * @param automationContext Appium native or webview context
 * @param windowHandle active window identifier
 * @param frameLocator configured frame locator
 * @param shadowHostLocator configured shadow host locator
 * @param shadowContentLocator configured shadow content locator
 */
public record HealingContext(
        String schemaVersion,
        HealingPlatform platform,
        String applicationId,
        String screenId,
        String automationContext,
        String windowHandle,
        String frameLocator,
        String shadowHostLocator,
        String shadowContentLocator) {
    public static final String CURRENT_SCHEMA_VERSION = "2.0";

    /**
     * Creates an immutable context.
     */
    public HealingContext {
        schemaVersion = Objects.requireNonNullElse(schemaVersion, CURRENT_SCHEMA_VERSION);
        platform = platform == null ? HealingPlatform.UNKNOWN : platform;
        applicationId = safe(applicationId);
        screenId = safe(screenId);
        automationContext = safe(automationContext);
        windowHandle = safe(windowHandle);
        frameLocator = safe(frameLocator);
        shadowHostLocator = safe(shadowHostLocator);
        shadowContentLocator = safe(shadowContentLocator);
    }

    /**
     * Returns a deterministic history key component.
     *
     * @return stable context key
     */
    public String stableKey() {
        return "platform=" + platform
                + ";app=" + applicationId
                + ";screen=" + screenId
                + ";automationContext=" + automationContext
                + ";window=" + windowHandle
                + ";frame=" + frameLocator
                + ";shadowHost=" + shadowHostLocator
                + ";shadowContent=" + shadowContentLocator;
    }

    private static String safe(String value) {
        return Objects.requireNonNullElse(value, "");
    }
}
