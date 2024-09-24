package com.shaft.tools.io;

import org.apache.logging.log4j.Level;

import static com.shaft.tools.io.internal.ReportManagerHelper.*;

public class ReportManager {

    private ReportManager() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Creates a custom log entry that will also be added as a step in the execution report
     *
     * @param logText the text that will be logged by action
     */
    public static void log(String logText) {
        if (logText != null && !logText.isBlank()) {
            if (getDiscreteLogging() && !logText.toLowerCase().contains("failed") && isInternalStep()) {
                createLogEntry(logText, Level.INFO);
            } else {
                writeStepToReport(logText);
            }
        }
    }

    public static void log(String logText, Level logLevel) {
        if (logText != null && !logText.isBlank()) {
            if (getDiscreteLogging() && !logText.toLowerCase().contains("failed") && isInternalStep()) {
                createLogEntry(logText, logLevel);
            } else {
                writeStepToReport(logText);
            }
        }
    }

    /**
     * Creates a custom log entry that will not be added as a step in the execution report, but you can see it in the attached execution log txt file
     *
     * @param logText the text that will be logged by action
     */
    public static void logDiscrete(String logText) {
        logDiscrete(logText, Level.INFO);
    }

    public static void logDiscrete(String logText, Level logLevel) {
        createLogEntry(logText, logLevel);
    }

}