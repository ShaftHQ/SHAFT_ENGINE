package com.shaft.validation.constants;

import org.testng.asserts.IAssert;
import org.testng.asserts.SoftAssert;

import java.util.ArrayList;
import java.util.List;

public class CustomSoftAssert extends SoftAssert {
    // List to store failure details
    private final List<String> failureMessages = new ArrayList<>();

    @Override
    public void onAssertFailure(IAssert<?> assertCommand, AssertionError ex) {
        StackTraceElement[] stackTrace = ex.getStackTrace();
        for (StackTraceElement element : stackTrace) {
            if (element.getClassName().contains("tests")) { // Adjust for your package
                String className = element.getClassName();
                String simpleClassName = className.substring(className.lastIndexOf(".") + 1);
                String methodName = element.getMethodName(); // e.g., testAssertions
                int lineNumber = element.getLineNumber();

                // Format as a stack trace-like clickable link
                String clickableLink = String.format("%s.%s(%s.java:%d)",
                        className, methodName, simpleClassName, lineNumber);

                String failureMessage = "❌ Assertion Failed at Line: " + lineNumber +
                        "\n🔍 Details: " + ex.getMessage() +
                        "\n📍 Navigate: " + clickableLink; // Stack trace format
                failureMessages.add(failureMessage);
                break;
            }
        }
        super.onAssertFailure(assertCommand, ex);
    }

    @Override
    public void assertAll() {
        if (!failureMessages.isEmpty()) {
            System.out.println("\n=== Assertion Failures Summary ===");
            for (String message : failureMessages) {
                System.out.println(message);
            }
            System.out.println("==============================");
        }
        super.assertAll();
    }

    public void clearFailures() {
        failureMessages.clear();
    }

    /**
     * Gets the formatted failure message with clickable stack trace for a given AssertionError.
     * This method can be used independently to format stack traces.
     *
     * @param ex The AssertionError to format
     * @param rootPackage The root package name to filter stack trace elements (e.g., "tests", "com.shaft")
     * @return Formatted failure message with clickable stack trace, or null if no matching stack element found
     */
    public static String formatFailureWithStackTrace(AssertionError ex, String rootPackage) {
        StackTraceElement[] stackTrace = ex.getStackTrace();
        for (StackTraceElement element : stackTrace) {
            if (element.getClassName().contains(rootPackage)) {
                String className = element.getClassName();
                String simpleClassName = className.substring(className.lastIndexOf(".") + 1);
                String methodName = element.getMethodName();
                int lineNumber = element.getLineNumber();

                // Format as a stack trace-like clickable link
                String clickableLink = String.format("%s.%s(%s.java:%d)",
                        className, methodName, simpleClassName, lineNumber);

                return "❌ Assertion Failed at Line: " + lineNumber +
                        "\n🔍 Details: " + ex.getMessage() +
                        "\n📍 Navigate: " + clickableLink;
            }
        }
        return null;
    }
}

