package io.github.shafthq.shaft.tools.io.helpers;

import com.shaft.tools.io.ReportManager;
import org.testng.Assert;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FailureReporter {
    public static void fail(Class failedFileManager, String message, Throwable throwable) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);

        List<List<Object>> attachments = new ArrayList<>();
        List<Object> actualValueAttachment = Arrays.asList(failedFileManager.getSimpleName() + " Exception - " + actionName,
                "Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(throwable));
        attachments.add(actualValueAttachment);
        ReportManagerHelper.log(message, attachments);
        Assert.fail(message, throwable);
    }

    public static void fail(String message) {
        ReportManager.log(message);
        Assert.fail(message);
    }
}
