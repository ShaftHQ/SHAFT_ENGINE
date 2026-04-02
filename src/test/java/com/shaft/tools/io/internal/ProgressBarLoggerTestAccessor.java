package com.shaft.tools.io.internal;

public final class ProgressBarLoggerTestAccessor {

    private ProgressBarLoggerTestAccessor() {
        throw new IllegalStateException("Utility class");
    }

    public static boolean isAnsiColorEnabledForCurrentEnvironment() {
        return ProgressBarLogger.shouldUseAnsiColors();
    }

    public static boolean interruptedFlagShouldBeSet() {
        return ProgressBarLogger.handleInterruptedProgressUpdate(new InterruptedException("test interruption"));
    }
}
