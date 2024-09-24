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
                    "Exception Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(throwable));
            attachments.add(actualValueAttachment);
            ReportManagerHelper.log(message + rootCause, attachments);
        }
        Assert.fail(message + rootCause, throwable);
    }

    public static void fail(String message) {
        ReportManager.log(message);
        Assert.fail(message);
    }

    public static String getRootCause(Throwable throwable){
        var rootCauseMessage = Throwables.getRootCause(throwable).getLocalizedMessage();
        if (rootCauseMessage!=null)
            return " Root cause: \"" + Throwables.getRootCause(throwable).getClass().getName() + ": " + rootCauseMessage.split("\n")[0] + "\"";
        return" Root cause: \"" + Throwables.getRootCause(throwable).getClass().getName() + ": " + "\"";
    }
}
