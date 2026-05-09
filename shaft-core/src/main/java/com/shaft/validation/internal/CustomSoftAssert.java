package com.shaft.validation.internal;

import com.shaft.tools.io.ReportManager;
import org.testng.asserts.IAssert;
import org.testng.asserts.SoftAssert;

import java.util.ArrayList;
import java.util.List;

/**
 * Enhanced SoftAssert implementation that provides clickable stack traces for failed assertions.
 * This improves the developer experience when debugging test failures in IDEs and CI logs.
 */
public class CustomSoftAssert extends SoftAssert {
    private String rootPackage;
    private final List<AssertionError> localFailures = new ArrayList<>();

    /**
     * Creates a new custom soft assertion collector.
     */
    public CustomSoftAssert() {
        super();
    }

    /**
     * Sets the root package used to filter stack trace elements when formatting failures.
     */
    public void setRootPackage(String rootPackage) {
        this.rootPackage = rootPackage;
    }

    /**
     * Clears the local failure list tracked by this instance (does not clear the parent SoftAssert errors).
     */
    public void clearFailures() {
        localFailures.clear();
    }

    /**
     * Override to add clickable stack trace to the failure message before reporting.
     */
    @Override
    public void onAssertFailure(IAssert<?> assertCommand, AssertionError ex) {
        localFailures.add(ex);
        List<StackTraceElement> filteredTrace = filterStackTrace(ex.getStackTrace());
        List<String> clickableTrace = new ArrayList<>();
        for (StackTraceElement element : filteredTrace) {
            String link = "(" + element.getFileName() + ":" + element.getLineNumber() + ")";
            clickableTrace.add("  at " + element.getClassName() + "." + element.getMethodName() + link);
        }
        if (!clickableTrace.isEmpty()) {
            ReportManager.logDiscrete("Assertion failure trace:\n" + String.join("\n", clickableTrace));
        }
    }

    private List<StackTraceElement> filterStackTrace(StackTraceElement[] trace) {
        List<StackTraceElement> filtered = new ArrayList<>();
        for (StackTraceElement element : trace) {
            String className = element.getClassName();
            if (rootPackage != null && !rootPackage.isBlank()) {
                if (className.startsWith(rootPackage)) {
                    filtered.add(element);
                }
            } else if (!className.startsWith("org.testng") && !className.startsWith("sun.reflect")
                    && !className.startsWith("java.lang.reflect") && !className.startsWith("com.shaft.validation.internal")) {
                filtered.add(element);
            }
        }
        return filtered;
    }

    /**
     * Formats the assertion error message with a clickable stack trace pointing into the given package.
     *
     * @param error       the assertion error to format
     * @param packageName the package prefix to search for in the stack trace
     * @return a formatted string with clickable trace entries, or null if no matching element is found
     */
    public static String formatFailureWithStackTrace(AssertionError error, String packageName) {
        if (error == null || packageName == null) return null;
        StackTraceElement[] trace = error.getStackTrace();
        List<String> clickableLines = new ArrayList<>();
        for (StackTraceElement element : trace) {
            if (element.getClassName().startsWith(packageName) && element.getLineNumber() > 0) {
                String fileName = element.getFileName() != null ? element.getFileName() : "Unknown";
                String link = "(" + fileName + ":" + element.getLineNumber() + ")";
                clickableLines.add("  at " + element.getClassName() + "." + element.getMethodName() + link);
            }
        }
        if (clickableLines.isEmpty()) return null;
        String message = error.getMessage() != null ? error.getMessage() : "";
        return message + (message.isEmpty() ? "" : "\n") + String.join("\n", clickableLines);
    }
}
