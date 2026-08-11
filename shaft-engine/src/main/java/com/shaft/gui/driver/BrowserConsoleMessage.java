package com.shaft.gui.driver;

import java.util.Locale;

/** One bounded, redacted browser console observation. */
public record BrowserConsoleMessage(String source, String level, String message, long timestamp) {
    /** @return whether this message represents a browser or page error */
    public boolean isError() {
        String normalized = level == null ? "" : level.toUpperCase(Locale.ROOT);
        return normalized.equals("ERROR") || normalized.equals("SEVERE") || normalized.equals("PAGEERROR");
    }
}
