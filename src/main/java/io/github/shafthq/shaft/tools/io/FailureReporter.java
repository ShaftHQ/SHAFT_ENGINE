package io.github.shafthq.shaft.tools.io;

import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.tools.support.JavaHelper;
import org.testng.Assert;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FailureReporter {
    public static void fail(Class<?> failedFileManager, String message, Throwable throwable) {
        String actionName = "fail";

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
        ReportManagerHelper.log(message, attachments);
        Assert.fail(message, throwable);
    }

    public static void fail(String message) {
        ReportManager.log(message);
        Assert.fail(message);
    }
}
