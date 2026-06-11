package com.shaft.capture.model;

import java.util.List;

/**
 * Sanitized page state at the time of an event.
 *
 * @param url sanitized URL
 * @param title page title
 * @param logicalWindowId stable logical window or tab identifier
 * @param framePath logical frame path from the top-level document
 * @param viewportWidth viewport width in CSS pixels
 * @param viewportHeight viewport height in CSS pixels
 */
public record PageContext(
        String url,
        String title,
        String logicalWindowId,
        List<String> framePath,
        int viewportWidth,
        int viewportHeight) {
    /**
     * Creates immutable page context.
     */
    public PageContext {
        url = ModelSupport.text(url);
        title = ModelSupport.text(title);
        logicalWindowId = ModelSupport.requireText(logicalWindowId, "Logical window ID");
        framePath = ModelSupport.list(framePath);
        if (viewportWidth < 0 || viewportHeight < 0) {
            throw new IllegalArgumentException("Viewport dimensions cannot be negative.");
        }
    }
}
