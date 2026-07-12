package com.shaft.tools.io.internal;

import com.google.common.base.Throwables;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.tools.internal.support.JavaHelper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FailureReporter {
    public static void fail(Class<?> failedFileManager, String message, Throwable throwable) {
        String rootCause = getRootCause(throwable);

        if (failedFileManager != ElementActionsHelper.class) {
            String actionName = "fail";
            for (StackTraceElement stackTraceElement : Arrays.stream(Thread.currentThread().getStackTrace()).toList()) {
                var methodName = stackTraceElement.getMethodName();
                if (!methodName.toLowerCase().contains("fail")) {
                    actionName = methodName;
                    break;
                }
            }
            List<List<Object>> attachments = new ArrayList<>();
            List<Object> actualValueAttachment = Arrays.asList(JavaHelper.convertToSentenceCase(failedFileManager.getSimpleName()) + " - " +
                            JavaHelper.convertToSentenceCase(actionName),
                    "Exception Stack Trace", ReportManagerHelper.formatStackTraceToLogEntry(throwable));
            attachments.add(actualValueAttachment);
            ReportManagerHelper.log(message + rootCause, attachments, CheckpointStatus.FAIL);
        }
        throw new RuntimeException(message + rootCause, throwable);
    }

    public static void fail(String message) {
        ReportManagerHelper.log(message, null, CheckpointStatus.FAIL);
        throw new RuntimeException(message);
    }

    /**
     * Reports a failed assertion checkpoint and throws an {@link AssertionError} so test
     * runners classify it as a test failure rather than an execution error. Use this for
     * assertion outcomes; use {@link #fail(String)} for execution failures — the exception
     * type is never inferred from the message text.
     *
     * @param message the assertion failure message
     */
    public static void failAssertion(String message) {
        ReportManagerHelper.log(message, null, CheckpointStatus.FAIL);
        throw new AssertionError(message);
    }

    public static String getRootCause(Throwable throwable) {
        var rootCause = Throwables.getRootCause(throwable);
        var rootCauseMessage = rootCause.getLocalizedMessage();
        if (rootCauseMessage != null)
            return " Root cause: \"" + rootCause.getClass().getName() + ": " + rootCauseMessage.split("\n")[0] + "\"";
        return " Root cause: \"" + rootCause.getClass().getName() + "\"";
    }
}
