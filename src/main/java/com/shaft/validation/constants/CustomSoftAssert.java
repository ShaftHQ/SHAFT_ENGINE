package com.shaft.validation.constants;

import com.shaft.tools.io.ReportManager;
import org.testng.asserts.IAssert;
import org.testng.asserts.SoftAssert;

import java.util.ArrayList;
import java.util.List;

public class CustomSoftAssert extends SoftAssert {
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

    public void clearFailures() {
        failureMessages.clear();
    }

    /**
     * Gets the formatted failure message with clickable stack trace for a given AssertionError.
     * This method can be used independently to format stack traces.
     * Uses standard Java stack trace format: at package.Class.method(File.java:lineNumber)
     *
     * @param ex The AssertionError to format
     * @param rootPackage The root package name to filter stack trace elements (e.g., "testPackage", "tests", "com.shaft")
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

                // Use standard Java stack trace format: at package.Class.method(File.java:lineNumber)
                // This format is recognized by IDEs and CI tools for clickable links
                String stackTraceLine = String.format("at %s.%s(%s:%d)",
                        className, methodName, fileName != null ? fileName : "Unknown", lineNumber);

                return "❌ Assertion Failed at Line: " + lineNumber +
                        "\n🔍 Details: " + ex.getMessage() +
                        "\n📍 Navigate: " + stackTraceLine;
            }
        }
        return null;
    }
}

