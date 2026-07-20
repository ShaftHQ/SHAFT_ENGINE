package com.shaft.mcp;

/**
 * Window sizing mode requested from the unified {@code browser_set_window_size} tool.
 */
public enum WindowSizeMode {
    /** Uses the given width/height (the default). */
    CUSTOM,
    /** Maximizes the window, absorbing {@code browser_maximize_window}. */
    MAXIMIZE,
    /** Sets the window to fullscreen, absorbing {@code browser_fullscreen_window}. */
    FULLSCREEN
}
