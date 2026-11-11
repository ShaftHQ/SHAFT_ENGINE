package com.shaft.tools.io.internal;

/**
 * Test entry to {@link ReportContext#replaceLastInfoLine(String)}.
 */
public final class ReportContextTestAccess {
    private ReportContextTestAccess() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Stores {@code line} as the current info line, including blank text. {@code null} clears it.
     *
     * @param line replacement text, or {@code null} to clear
     */
    public static void replaceLastInfoLine(String line) {
        ReportContext.replaceLastInfoLine(line);
    }
}
