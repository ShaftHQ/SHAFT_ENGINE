package com.shaft.tools.io.internal;

import com.google.common.base.Throwables;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.io.ReportManager;
import org.testng.Assert;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FailureReporter {
    public static void fail(Class<?> failedFileManager, String message, Throwable throwable) {
        String actionName = "fail";
        String rootCause;
        try {
            rootCause = " Root cause: \"" + Throwables.getRootCause(throwable).getClass().getName() + ": " + Throwables.getRootCause(throwable).getLocalizedMessage().split("\n")[0] + "\"";
        } catch (NullPointerException e) {
            rootCause = " Root cause: \"" + Throwables.getRootCause(throwable).getClass().getName() + ": ".split("\n")[0] + "\"";
        }
        for (StackTraceElement stackTraceElement : Arrays.stream(Thread.currentThread().getStackTrace()).toList()) {
            var methodName = stackTraceElement.getMethodName();
            if (!methodName.toLowerCase().contains("fail")) {
                actionName = methodName;
                break;
            }
        }
        actionName = JavaHelper.convertToSentenceCase(actionName);

        List<List<Object>> attachments = new ArrayList<>();
        List<Object> actualValueAttachment = Arrays.asList(JavaHelper.convertToSentenceCase(failedFileManager.getSimpleName()) + " - " +
                        JavaHelper.convertToSentenceCase(actionName),
                "Exception Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(throwable));
        attachments.add(actualValueAttachment);
        if (failedFileManager != ElementActionsHelper.class)
            ReportManagerHelper.log(message + rootCause, attachments);
        Assert.fail(message + rootCause, throwable);
    }

    public static void fail(String message) {
        ReportManager.log(message);
        Assert.fail(message);
    }
}
