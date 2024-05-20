package com.shaft.listeners.internal;

import com.shaft.tools.io.internal.ReportManagerHelper;
import org.junit.platform.launcher.TestIdentifier;

public class JunitListenerHelper {
    private static final ThreadLocal<String> testName = new ThreadLocal<>();

    public static void setTestName(TestIdentifier testIdentifier) {
        testName.set(testIdentifier.getDisplayName());
    }

    public static void logTestInformation(TestIdentifier testIdentifier) {
        String className = ReportManagerHelper.getTestClassName();
        String methodName = ReportManagerHelper.getTestMethodName();
        String methodDescription = "";

        if (testIdentifier.isTest()) {
            ReportManagerHelper.logTestInformation(className, methodName, methodDescription);
        }
    }
}
