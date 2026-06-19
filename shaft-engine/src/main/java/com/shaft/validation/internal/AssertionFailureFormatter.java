package com.shaft.validation.internal;

/**
 * Formats assertion failures with a clickable Java stack-trace line.
 */
public final class AssertionFailureFormatter {
    private AssertionFailureFormatter() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Gets the formatted failure message with clickable stack trace for a given AssertionError.
     *
     * @param ex The AssertionError to format
     * @param rootPackage The root package name to filter stack trace elements
     * @return Formatted failure message with clickable stack trace, or null if no matching stack element found
     */
    public static String formatFailureWithStackTrace(AssertionError ex, String rootPackage) {
        if (ex == null || rootPackage == null) {
            return null;
        }

        StackTraceElement[] stackTrace = ex.getStackTrace();
        for (StackTraceElement element : stackTrace) {
            if (element.getClassName().contains(rootPackage) && element.getLineNumber() > 0) {
                String className = element.getClassName();
                String methodName = element.getMethodName();
                String fileName = element.getFileName();
                int lineNumber = element.getLineNumber();

                String stackTraceLine = String.format("at %s.%s(%s:%d)",
                        className, methodName, fileName != null ? fileName : "Unknown", lineNumber);

                return "❌ Assertion Failed at Line: " + lineNumber +
                        "\n🔍 Details: " + ex.getMessage() +
                        "\n📍 Navigate:\n" + stackTraceLine;
            }
        }
        return null;
    }
}
