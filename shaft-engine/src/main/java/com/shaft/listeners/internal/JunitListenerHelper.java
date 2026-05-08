package com.shaft.listeners.internal;

import com.shaft.tools.io.internal.ReportManagerHelper;
import lombok.Getter;
import org.junit.platform.launcher.TestIdentifier;

/**
 * Holds JUnit listener helper state and report-logging utilities per test thread.
 */
public class JunitListenerHelper {
    @Getter
    private static final ThreadLocal<String> testName = new ThreadLocal<>();

    /**
     * Stores the current JUnit test name for the active thread.
     *
     * @param testIdentifier current JUnit test identifier
     */
    public static void setTestName(TestIdentifier testIdentifier) {
        testName.set(testIdentifier.getDisplayName());
    }

    /**
     * Logs test metadata into SHAFT reporting when the identifier is a test node.
     *
     * @param testIdentifier current JUnit test identifier
     */
    public static void logTestInformation(TestIdentifier testIdentifier) {
        String className = ReportManagerHelper.getTestClassName();
        String methodName = ReportManagerHelper.getTestMethodName();
        String methodDescription = "";

        if (testIdentifier.isTest()) {
            ReportManagerHelper.logTestInformation(className, methodName, methodDescription);
        }
    }
}
