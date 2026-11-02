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
    /**
     * Creates a new custom soft assertion collector.
     */
    public CustomSoftAssert() {
        super();
    }
    // List to store failure details
    private final List<String> failureMessages = new ArrayList<>();
    
    /**
     * Root package name used to filter stack trace elements when reporting assertion failures.
     * Defaults to "test" to match common test package patterns.
     */
    private String rootPackage = "test";

    /**
     * Allows configuring the root package used when filtering stack trace elements.
     *
     * @param rootPackage the root package prefix (e.g., "testPackage", "tests", "com.shaft")
     */
    public void setRootPackage(String rootPackage) {
        this.rootPackage = rootPackage;
    }

    @Override
    public void onAssertFailure(IAssert<?> assertCommand, AssertionError ex) {
        String failureMessage = formatFailureWithStackTrace(ex, rootPackage);
        if (failureMessage != null) {
            failureMessages.add(failureMessage);
        }
        super.onAssertFailure(assertCommand, ex);
    }

    @Override
    public void assertAll() {
        if (!failureMessages.isEmpty()) {
            ReportManager.logDiscrete("\n=== Assertion Failures Summary ===");
            for (String message : failureMessages) {
                ReportManager.logDiscrete(message);
            }
            ReportManager.logDiscrete("==============================");
        }
        super.assertAll();
    }

    /**
     * Clears all currently collected assertion failure messages.
     */
    public void clearFailures() {
        failureMessages.clear();
    }

    /**
     * Gets the formatted failure message with clickable stack trace for a given AssertionError.
     * This method can be used independently to format stack traces.
     * Uses standard Java stack trace format: at package.Class.method(File.java:lineNumber)
     * The stack trace line starts with "at " on its own line for maximum IDE/CI clickability.
     *
     * @param ex The AssertionError to format
     * @param rootPackage The root package name to filter stack trace elements (e.g., "testPackage", "tests", "com.shaft")
     * @return Formatted failure message with clickable stack trace, or null if no matching stack element found
     */
    public static String formatFailureWithStackTrace(AssertionError ex, String rootPackage) {
        return AssertionFailureFormatter.formatFailureWithStackTrace(ex, rootPackage);
    }
}

