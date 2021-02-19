package com.shaft.tools.io;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static com.shaft.tools.io.ReportManagerHelper.*;

@SuppressWarnings("unused")
public class ReportManager {

    private ReportManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Manages action counter and calls writeLog to format and print the log entry.
     *
     * @param logText the text that needs to be logged in this action
     */
    public static void log(String logText) {
        if (isDiscreteLogging() && !logText.toLowerCase().contains("failed") && isInternalStep()) {
            createLogEntry(logText);
        } else {
            writeStepToReport(actionCounter, logText);
            actionCounter++;
        }
    }

    public static void log(String logText, List<List<Object>> attachments) {
        if (isDiscreteLogging() && !logText.toLowerCase().contains("failed") && isInternalStep()) {
            createLogEntry(logText);
            if (attachments != null) {
                attachments.forEach(attachment -> {
                    if (attachment != null) {
                        if (attachment.get(2) instanceof String) {
                            attachAsStep(attachment.get(0).toString(), attachment.get(1).toString(),
                                    new ByteArrayInputStream(attachment.get(2).toString().getBytes()));
                        } else {
                            attachAsStep(attachment.get(0).toString(), attachment.get(1).toString(),
                                    (InputStream) attachment.get(2));
                        }
                    }
                });
            }
        } else {
            writeStepToReport(actionCounter, logText, attachments);
            actionCounter++;
        }
    }

    /**
     * Format an exception message and stack trace, and calls attach to add it as a
     * log entry.
     *
     * @param t the throwable (exception or error) that will be logged in this
     *          action
     */
    public static void log(Throwable t) {
        String logText;
        logText = formatStackTraceToLogEntry(t);
        if (t.getMessage() != null) {
            ReportManager.log("An Exception Occured with this Message: " + t.getMessage().split("\n")[0].trim() + ".",
                    Collections.singletonList(Arrays.asList("Exception Stack Trace", t.getClass().getName(), logText)));
        } else {
            ReportManager.log("An Exception Occured",
                    Collections.singletonList(Arrays.asList("Exception Stack Trace", t.getClass().getName(), logText)));
        }
        actionCounter++;
    }

    public static void logDiscrete(String logText) {
        createLogEntry(logText);
    }

    public static void logDiscrete(Throwable t) {
        createLogEntry(formatStackTraceToLogEntry(t));
    }
}