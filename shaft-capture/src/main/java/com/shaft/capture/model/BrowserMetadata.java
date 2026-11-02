package com.shaft.capture.model;

import java.util.Map;

/**
 * Browser and logical session metadata retained by a capture.
 *
 * @param browserName browser family
 * @param browserVersion browser version
 * @param platform operating-system or device platform
 * @param logicalSessionId safe logical identifier, not a credential
 * @param capabilities sanitized W3C capability values
 */
public record BrowserMetadata(
        String browserName,
        String browserVersion,
        String platform,
        String logicalSessionId,
        Map<String, String> capabilities) {
    /**
     * Creates immutable sanitized browser metadata.
     */
    public BrowserMetadata {
        browserName = ModelSupport.requireText(browserName, "Browser name");
        browserVersion = ModelSupport.text(browserVersion);
        platform = ModelSupport.text(platform);
        logicalSessionId = ModelSupport.requireText(logicalSessionId, "Logical session ID");
        capabilities = ModelSupport.strings(capabilities);
    }
}
